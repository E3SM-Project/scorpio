#ifndef _PIO_ASYNC_MTQ_HPP_
#define _PIO_ASYNC_MTQ_HPP_

#include <iostream>
#include <queue>
#include <thread>
#include <chrono>
#include <mutex>
#include <condition_variable>

namespace PIO_Util{

template<typename T>
class PIO_mtq{
  public:
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
    void signal(SigTypes_t sig);
    int size(void ) const;

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
{}

template<typename T>
void PIO_mtq<T>::enqueue(const T& val)
{
  std::unique_lock<std::mutex> lk(mtx_);
  queue_.push(val);
  lk.unlock();
  cv_.notify_all();
}

template<typename T>
int PIO_mtq<T>::dequeue(T &val)
{
  std::unique_lock<std::mutex> lk(mtx_);
  do{
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
  lk.unlock();
  return 0;
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
    /* Wait for all async ops in the queue to complete */
    while(sig_ == PIO_MTQ_SIG_COMPLETE){
      cv_sig_.wait(lk, [this]{ return (sig_ == PIO_MTQ_SIG_COMPLETE);}); 
      thread_yield(lk);
    }
  }
  lk.unlock();
}

template<typename T>
int PIO_mtq<T>::size(void ) const
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

#endif // _PIO_ASYNC_MTQ_HPP_
