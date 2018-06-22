#include <vector>
#include <thread>
#include <chrono>
#include <algorithm>
#include <cassert>
#include "pio_async_mtq.hpp"
extern "C"{
#include <pio.h>
#include <pio_internal.h>
#include <pio_tests.h>
}

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

static const int FAIL = -1;

class UType{
  public:
    int i;
    float f;
};

/* Util function to enqueue data in elems to q */
template<typename T>
void thread_enq(PIO_Util::PIO_mtq<T> &q, const std::vector<T> &elems,
  const std::chrono::seconds &delay)
{
  for(typename std::vector<T>::const_iterator citer = elems.cbegin();
      citer != elems.cend(); ++citer){
    if(delay.count() != 0){
      std::this_thread::sleep_for(delay);
    }
    q.enqueue(*citer);
  }
}

/* Util function to dequeue nelems from q and store in elems */
template<typename T>
void thread_deq(PIO_Util::PIO_mtq<T> &q, std::vector<T> &elems,
  const std::chrono::seconds &delay)
{
  int nelems = elems.size();
  for(int i=0; i<nelems; i++){
    if(delay.count() != 0){
      std::this_thread::sleep_for(delay);
    }
    int ret = q.dequeue(elems[i]);
    if(ret != 0){
      /* Break on signal */
      break;
    }
  }
}

/* Util functoin to signal the threads waiting in a multi-threaded
 * queue
 */
template<typename T>
void thread_signal(PIO_Util::PIO_mtq<T> &q,
                    typename PIO_Util::PIO_mtq<T>::SigTypes_t sig,
                    const std::chrono::seconds &delay)
{
  if(delay.count() != 0){
    std::this_thread::sleep_for(delay);
  }
  q.signal(sig);
}

std::ostream &operator<<(std::ostream &ostr, const UType &utype)
{
  ostr << "(" << utype.i << "," << utype.f << "), ";
  return ostr;
}

std::ostream &operator<<(std::ostream &ostr, const UType *putype)
{
  ostr << "(" << putype->i << "," << putype->f << "), ";
  return ostr;
}

/* Test sending signals to waiting dequeue threads in a multi-threaded queue containing
 * ints */
int test_signal_mtq(int wrank, const int max_elems_in_q, const int nthreads,
    PIO_Util::PIO_mtq<int>::SigTypes_t sig,
    const std::chrono::seconds &enqueue_delay,
    const std::chrono::seconds &dequeue_delay,
    bool use_signal_thread)
{
    assert((max_elems_in_q > 0) && (nthreads > 0));
    int nelems_per_thread = max_elems_in_q/nthreads;
    int nelems_last_thread = max_elems_in_q - nelems_per_thread * (nthreads - 1);

    /* Enqueue multiple elems from different threads and dequeue to make
     * sure that the elems are in the queue
     */

    /* Generate input vals */
    PIO_Util::PIO_mtq<int> qi;
    std::vector<std::vector<int> > ivals;
    std::vector<std::vector<int> > ovals;
    for(int i=0; i<nthreads; i++){
      bool is_last_thread = (i == (nthreads - 1));
      std::vector<int> ivals_ithread;
      std::vector<int> ovals_ithread;
      if(!is_last_thread){
        ivals_ithread.resize(nelems_per_thread);
        /* The size is +1 so that we wait for more elements than in the queue */
        ovals_ithread.resize(nelems_per_thread + 1);
      }
      else{
        ivals_ithread.resize(nelems_last_thread);
        /* The size is +1 so that we wait for more elements than in the queue */
        ovals_ithread.resize(nelems_last_thread + 1);
      }
      int sval = i * nelems_per_thread;
      std::generate(ivals_ithread.begin(), ivals_ithread.end(), [&sval]{return sval++; });
      ivals.push_back(ivals_ithread);
      std::fill(ovals_ithread.begin(), ovals_ithread.end(), -1);
      ovals.push_back(ovals_ithread);
    }

    /* Enqueue */
    std::vector<std::thread> tpool;
    for(int i=0; i<nthreads; i++){
      tpool.push_back(std::thread(thread_enq<int>, std::ref(qi), std::ref(ivals[i]), enqueue_delay));
    }

    /* Wait for enqueue operations to complete */
    for(int i=0; i<nthreads; i++){
      tpool[i].join();
    }
    tpool.clear();
    //std::cout << qi << std::endl;

    /* Dequeue */
    for(int i=0; i<nthreads; i++){
      tpool.push_back(std::thread(thread_deq<int>, std::ref(qi), std::ref(ovals[i]), dequeue_delay));
    }

    /* Wait for enqueue operations to complete */
    std::thread *sig_thread = NULL;
    if(!use_signal_thread){
      /* Signal all threads so that they exit once the queue is empty */
      qi.signal(sig);
    }
    else{
      /* If there is delay specified during dequeueing data, set the delay so
       * that approx half of the data is dequeued by each thread before sending
       * the signal
       */
      std::chrono::seconds sig_thread_delay = dequeue_delay * ovals[0].size() / 2;
      sig_thread = new std::thread(thread_signal<int>, std::ref(qi), sig, sig_thread_delay); 
    }
    for(int i=0; i<nthreads; i++){
      tpool[i].join();
    }
    if(use_signal_thread){
      sig_thread->join();
      delete sig_thread;
    }
    tpool.clear();

    /* Verify that we dequeued all elements in the queue */
    std::vector<bool> elem_in_q(max_elems_in_q, false);
    for(int i=0; i<nthreads; i++){
      for(int j=0; j<ovals[i].size(); j++){
        int idx = (ovals[i])[j];
        if((idx < -1) || (idx >= max_elems_in_q)){
          /* Invalid element in queue */
          LOG_RANK0(wrank, "Invalid element (%d) in queue\n", idx);
          return PIO_EINTERNAL;
        }
        /* Note that some threads may not be able to get ovals[i].size() 
         * elements from the queue. These entries will be -1 on that
         * thread. Ignore these entries
         */
        if(idx != -1){
          /* Mark that the element was in the queue */
          elem_in_q[idx] = true;
        }
      }
    }

    /* A SIG_STOP abnormally terminates the threads, the queue is not
     * completely used up, so we should not be verifying the contents
     * here.
     */
    if(sig != PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_STOP){
      for(int i=0; i<max_elems_in_q; i++){
        if(elem_in_q[i] == false){
          LOG_RANK0(wrank, "Element (%d) not found in queue\n", i);
          return PIO_EINTERNAL;
        }
      }
    }


    return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
    int nerrs = 0, ret = PIO_NOERR;
    assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
    
    /* Test signalling waiting threads (the threads wait for 11 elements but we
     * only queue 10) - both threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_COMPLETE
     */
    try{
      ret = test_signal_mtq(wrank, 10, 2,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_COMPLETE,
              std::chrono::seconds(0),
              std::chrono::seconds(0),
              false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(10 elems, 2 threads, SIG_COMPLETE, no delays, no signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(10 elems, 2 threads, SIG_COMPLETE, no delays, no signalthread) PASSED\n");
    }

    /* Test signalling waiting threads (the threads wait for 101 elements but we
     * only queue 100) - all 4 threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_COMPLETE
     */
    try{
      ret = test_signal_mtq(wrank, 100, 4,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_COMPLETE,
              std::chrono::seconds(0),
              std::chrono::seconds(0),
              false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(100 elems, 4 threads, SIG_COMPLETE, no delays, no signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(100 elems, 4 threads, SIG_COMPLETE, no delays, no signalthread) PASSED\n");
    }

    /* Test signalling waiting threads (the threads wait for 11 elements but we
     * only queue 10) - both threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_STOP
     */
    try{
      ret = test_signal_mtq(wrank, 10, 2,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_STOP,
              std::chrono::seconds(0),
              std::chrono::seconds(0),
              false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(10 elems, 2 threads, SIG_STOP, no delays, no signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(10 elems, 2 threads, SIG_STOP, no delays, no signalthread) PASSED\n");
    }

    /* Test signalling waiting threads (the threads wait for 101 elements but we
     * only queue 100) - all 4 threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_STOP
     */
    try{
      ret = test_signal_mtq(wrank, 100, 4,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_STOP,
              std::chrono::seconds(0),
              std::chrono::seconds(0),
              false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(100 elems, 4 threads, SIG_STOP, no delays, no signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(100 elems, 4 threads, SIG_STOP, no delays, no signalthread) PASSED\n");
    }

    /* Test signalling waiting threads (the threads wait for 21 elements but we
     * only queue 20) - all 4 threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_COMPLETE, with a delay of 1 sec when dequeueing data
     */
    try{
      ret = test_signal_mtq(wrank, 20, 4,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_COMPLETE,
              std::chrono::seconds(0),
              std::chrono::seconds(1),
              false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_COMPLETE, 1s delay deq, no signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_COMPLETE, 1s delay deq, no signalthread) PASSED\n");
    }

    /* Test signalling waiting threads (the threads wait for 21 elements but we
     * only queue 20) - all 4 threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_STOP, with a delay of 1 sec when dequeueing data
     */
    try{
      ret = test_signal_mtq(wrank, 20, 4,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_STOP,
              std::chrono::seconds(0),
              std::chrono::seconds(1),
              false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_STOP, 1s delay deq, no signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_STOP, 1s delay deq, no signalthread) PASSED\n");
    }

    /* Test signalling waiting threads (the threads wait for 21 elements but we
     * only queue 20) - all 4 threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_COMPLETE, with a delay of 1 sec when dequeueing data
     * The signal is sent from a separate thread
     */
    try{
      ret = test_signal_mtq(wrank, 20, 4,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_COMPLETE,
              std::chrono::seconds(0),
              std::chrono::seconds(1),
              true);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_COMPLETE, 1s delay deq, using signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_COMPLETE, 1s delay deq, using signalthread) PASSED\n");
    }

    /* Test signalling waiting threads (the threads wait for 21 elements but we
     * only queue 20) - all 4 threads are waiting and the main thread signals
     * all the threads to quit
     * Test SIG_STOP, with a delay of 1 sec when dequeueing data
     * The signal is sent from a separate thread
     */
    try{
      ret = test_signal_mtq(wrank, 20, 4,
              PIO_Util::PIO_mtq<int>::PIO_MTQ_SIG_STOP,
              std::chrono::seconds(0),
              std::chrono::seconds(1),
              true);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_STOP, 1s delay deq, using signalthread) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_signal_mtq(20 elems, 4 threads, SIG_STOP, 1s delay deq, using signalthread) PASSED\n");
    }

    *num_errors += nerrs;
    return nerrs;
}

int main(int argc, char *argv[])
{
    int ret;
    int wrank, wsz;
    int num_errors;
#ifdef TIMING
#ifndef TIMING_INTERNAL
    ret = GPTLinitialize();
    if(ret != 0)
    {
        LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
        return ret;
    }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

    ret = MPI_Init(&argc, &argv);
    if(ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Init() FAILED, ret = %d\n", ret);
        return ret;
    }

    ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
    if(ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
        return ret;
    }
    ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
    if(ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
        return ret;
    }

    num_errors = 0;
    ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
    if(ret != 0)
    {
        LOG_RANK0(wrank, "Test driver FAILED\n");
        return FAIL;
    }
    else{
        LOG_RANK0(wrank, "All tests PASSED\n");
    }

    MPI_Finalize();

#ifdef TIMING
#ifndef TIMING_INTERNAL
    ret = GPTLfinalize();
    if(ret != 0)
    {
        LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
        return ret;
    }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

    if(num_errors != 0)
    {
        LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
        return FAIL;
    }
    return 0;
}
