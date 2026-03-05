#ifndef _SPIO_ASYNC_MTQ_HPP_
#define _SPIO_ASYNC_MTQ_HPP_

#include <iostream>
#include <queue>
#include <thread>
#include <chrono>
#include <mutex>
#include <condition_variable>
#include <stdexcept>
#include "pio_internal.h"

namespace PIO_Util{

template<typename T>
class PIO_mtq{
  public:
    /* Multi-threaded queue exceptions */
    class Mtq_exception{
      public:
        Mtq_exception(const std::string &msg) : msg_(msg){}
        std::string what(void ) const { return msg_; }
      private:
        std::string msg_;
    };
    /* Exception : Signal received on MT queue */
    class Qsignal_exception : public Mtq_exception{
      public:
        Qsignal_exception(const std::string &msg) : Mtq_exception(msg){}
    };
    /* Exception : Queue is empty
     * e.g. After a signal was received, queue is empty
     */
    class Qempty_exception : public Mtq_exception{
      public:
        Qempty_exception(const std::string &msg) : Mtq_exception(msg){}
    };

    typedef enum SigTypes{
      PIO_MTQ_SIG_INVALID=1,
      /* Stop processing elements in the queue */
      PIO_MTQ_SIG_STOP,
      /* Complete processing elements in queue and quit */
      PIO_MTQ_SIG_COMPLETE
    }SigTypes_t;
    PIO_mtq();
    void enqueue(const T& val);
    int dequeue(T &val);
    T dequeue(void );
    void signal(SigTypes_t sig);
    int size(void );

    template<typename U>
    friend std::ostream & operator<<(std::ostream &ostr, PIO_mtq<U> &mtq);
  private:
    void thread_yield(std::unique_lock<std::mutex> &lk) const;
    std::mutex mtx_;
    std::queue<T> queue_;
    std::condition_variable cv_;
    SigTypes_t sig_;
    std::condition_variable cv_sig_;
};

template<typename T>
PIO_mtq<T>::PIO_mtq():sig_(PIO_mtq<T>::PIO_MTQ_SIG_INVALID)
{
  LOG((2, "PIO_mtq:PIO_mtq: Creating MT queue"));
}

template<typename T>
void PIO_mtq<T>::enqueue(const T& val)
{
  std::unique_lock<std::mutex> lk(mtx_);
  LOG((2, "PIO_mtq:enqueue: Enqueing val to mtq"));
  queue_.push(val);
  lk.unlock();
  cv_.notify_all();
}

template<typename T>
int PIO_mtq<T>::dequeue(T &val)
{
  std::unique_lock<std::mutex> lk(mtx_);
  do{
    LOG((2, "PIO_mtq:dequeue: Waiting for dequeueing val from mtq..."));
    while(queue_.empty() && (sig_ == PIO_MTQ_SIG_INVALID)){
      cv_.wait(lk, [this]{ return (queue_.empty() && (sig_ == PIO_MTQ_SIG_INVALID));}); 
      /*
      cv_.wait_for(lk, DEFAULT_TIMEOUT,
              [this]{ return (queue_.empty() && (sig_ == PIO_MTQ_SIG_INVALID));}); 
      */
      /* At least on Linux (Ubuntu 4.5.0-040500-generic) just waiting on the condition
       * variable (with/without timeouts) does not allow other threads to acquire
       * the lock. So we explicitly unlock, sleep for 0 seconds and reacquire the 
       * lock to allow other threads to be able to get scheduled and acquire the lock
       */
      thread_yield(lk);
    }
    if(sig_ == PIO_MTQ_SIG_STOP){
      LOG((2, "PIO_mtq:dequeue: Received STOP signal on mtq (exiting...)"));
      return 1;
    }
    else if(sig_ == PIO_MTQ_SIG_COMPLETE){
      if(queue_.empty()){
        /* Reset signal */
        sig_ = PIO_MTQ_SIG_INVALID;
        lk.unlock();
        /* Notify threads that all async ops are complete */
        cv_sig_.notify_all();
        lk.lock();
      }
      else{
        break;
      }
    }
    else{
      break;
    }
  }while(true);
  val = queue_.front();
  queue_.pop();
  LOG((2, "PIO_mtq:dequeue: Successfully dequeued val from mtq"));
  lk.unlock();
  return 0;
}

template<typename T>
T PIO_mtq<T>::dequeue(void )
{
  std::unique_lock<std::mutex> lk(mtx_);
  do{
    LOG((2, "PIO_mtq:dequeue: Waiting for dequeueing val from mtq..."));
    while(queue_.empty() && (sig_ == PIO_MTQ_SIG_INVALID)){
      cv_.wait(lk, [this]{ return (queue_.empty() && (sig_ == PIO_MTQ_SIG_INVALID));});
      /*
      cv_.wait_for(lk, DEFAULT_TIMEOUT,
              [this]{ return (queue_.empty() && (sig_ == PIO_MTQ_SIG_INVALID));});
      */
      /* At least on Linux (Ubuntu 4.5.0-040500-generic) just waiting on the condition
       * variable (with/without timeouts) does not allow other threads to acquire
       * the lock. So we explicitly unlock, sleep for 0 seconds and reacquire the
       * lock to allow other threads to be able to get scheduled and acquire the lock
       */
      thread_yield(lk);
    }
    if(sig_ == PIO_MTQ_SIG_STOP){
      LOG((2, "PIO_mtq:dequeue: Received STOP signal on mtq (exiting...)"));
      throw Qsignal_exception("Received STOP signal on multi-threaded queue");
    }
    else if(sig_ == PIO_MTQ_SIG_COMPLETE){
      if(queue_.empty()){
        /* Reset signal */
        sig_ = PIO_MTQ_SIG_INVALID;
        lk.unlock();
        /* Notify threads that all async ops are complete */
        cv_sig_.notify_all();
        lk.lock();
      }
      else{
        break;
      }
    }
    else{
      break;
    }
  }while(true);

  if(!queue_.empty()){
    T val = queue_.front();
    queue_.pop();
    LOG((2, "PIO_mtq:dequeue: Successfully dequeued val from mtq"));
    lk.unlock();
    return val;
  }
  else{
    lk.unlock();
    throw Qempty_exception("Multi-threaded queue is empty");
  }
}

template<typename T>
void PIO_mtq<T>::signal(PIO_mtq<T>::SigTypes_t sig)
{
  std::unique_lock<std::mutex> lk(mtx_);
  sig_ = sig;
  lk.unlock();
  cv_.notify_all();

  lk.lock();
  if(sig == PIO_MTQ_SIG_COMPLETE){
    LOG((2, "PIO_mtq:signal: Received PIO_MTQ_SIG_COMPLETE, Waiting for async ops to complete"));
    /* Wait for all async ops in the queue to complete */
    while(sig_ == PIO_MTQ_SIG_COMPLETE){
      cv_sig_.wait(lk, [this]{ return (sig_ == PIO_MTQ_SIG_COMPLETE);}); 
      thread_yield(lk);
    }
  }
  lk.unlock();
}

template<typename T>
int PIO_mtq<T>::size(void )
{
  int sz = 0;
  std::unique_lock<std::mutex> lk(mtx_);
  sz = queue_.size();
  lk.unlock();
  return sz;
}

template<typename T>
void PIO_mtq<T>::thread_yield(std::unique_lock<std::mutex> &lk) const
{
  /* At least on Linux (Ubuntu 4.5.0-040500-generic) just waiting on the condition
   * variable (with/without timeouts) does not allow other threads to acquire
   * the lock. So we explicitly unlock, sleep for 0 seconds and reacquire the 
   * lock to allow other threads to be able to get scheduled and acquire the lock
   */
  const std::chrono::milliseconds ZERO_TIMEOUT = std::chrono::milliseconds(0);
  lk.unlock();
  //LOG((2, "PIO_mtq:thread_yield: Yielding for 0 secs"));
  std::this_thread::sleep_for(ZERO_TIMEOUT);
  lk.lock();
}

template<typename T>
std::ostream &operator<<(std::ostream &ostr, PIO_Util::PIO_mtq<T> &q)
{
  std::unique_lock<std::mutex> lk(q.mtx_);
  /* We need to copy the queue since it is not iterable */
  std::queue<T> tq = q.queue_;
  while(!tq.empty()){
    ostr << tq.front() << ", ";
    tq.pop();
  }
  lk.unlock();
  return ostr;
}

} // namespace PIO_Util

#endif // _SPIO_ASYNC_MTQ_HPP_
