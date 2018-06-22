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

/* Test creating a multi-threaded queue */
int test_create_mtq(void )
{
    PIO_Util::PIO_mtq<int> qi;
    PIO_Util::PIO_mtq<float> qf;

    class TestClass{
      int i;
      float f;
    };

    PIO_Util::PIO_mtq<TestClass> qt;

    return PIO_NOERR;
}

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
    //std::cout << "Enqueued item : " << *citer << "\n";
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
    //std::cout << "Dequeued item : " << elems[i] << "\n";
  }
}

/* Test enqueue and dequeue operations in a multi-threaded queue containing
 * ints */
int test_enq_deq_int(int wrank, const int max_elems_in_q, const int nthreads,
                      const std::chrono::seconds &enqueue_delay,
                      const std::chrono::seconds &dequeue_delay,
                      bool concurrent_enq_deq)
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
        ovals_ithread.resize(nelems_per_thread);
      }
      else{
        ivals_ithread.resize(nelems_last_thread);
        ovals_ithread.resize(nelems_last_thread);
      }
      int sval = i * nelems_per_thread;
      std::generate(ivals_ithread.begin(), ivals_ithread.end(), [&sval]{return sval++; });
      ivals.push_back(ivals_ithread);
      std::fill(ovals_ithread.begin(), ovals_ithread.end(), -1);
      ovals.push_back(ovals_ithread);
    }

    /* Enqueue */
    std::vector<std::thread> tpool_enq, tpool_deq;
    for(int i=0; i<nthreads; i++){
      tpool_enq.push_back(std::thread(thread_enq<int>, std::ref(qi), std::ref(ivals[i]), enqueue_delay));
    }

    /* Wait for enqueue operations to complete */
    if(!concurrent_enq_deq){
      for(int i=0; i<nthreads; i++){
        tpool_enq[i].join();
      }
      tpool_enq.clear();
      //std::cout << qi << std::endl;
    }
    else{
      for(int i=0; i<nthreads; i++){
        tpool_enq[i].detach();
      }
    }

    /* Dequeue */
    for(int i=0; i<nthreads; i++){
      tpool_deq.push_back(std::thread(thread_deq<int>, std::ref(qi), std::ref(ovals[i]), dequeue_delay));
    }

    /* Wait for enqueue operations to complete */
    for(int i=0; i<nthreads; i++){
      tpool_deq[i].join();
    }
    tpool_deq.clear();

    /* Verify that we dequeued all elements in the queue */
    std::vector<bool> elem_in_q(max_elems_in_q, false);
    for(int i=0; i<nthreads; i++){
      for(int j=0; j<ovals[i].size(); j++){
        int idx = (ovals[i])[j];
        if((idx < 0) || (idx >= max_elems_in_q)){
          /* Invalid element in queue */
          LOG_RANK0(wrank, "Invalid element (%d) in queue\n", idx);
          return PIO_EINTERNAL;
        }
        /* Mark that the element was in the queue */
        elem_in_q[idx] = true;
      }
    }

    for(int i=0; i<max_elems_in_q; i++){
      if(elem_in_q[i] == false){
        /* At least one element was missing in the queue */
        LOG_RANK0(wrank, "Element (%d) was not found in the queue\n", i);
        return PIO_EINTERNAL;
      }
    }


    return PIO_NOERR;
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

/* Test enqueue and dequeue operations in a multi-threaded queue containing
 * a user defined type */
int test_enq_deq_utype(int wrank, const int max_elems_in_q, const int nthreads)
{
    assert((max_elems_in_q > 0) && (nthreads > 0));
    int nelems_per_thread = max_elems_in_q/nthreads;
    int nelems_last_thread = max_elems_in_q - nelems_per_thread * (nthreads - 1);

    /* Enqueue multiple elems from different threads and dequeue to make
     * sure that the elems are in the queue
     */

    /* Generate input vals */
    PIO_Util::PIO_mtq<UType> qu;
    std::vector<std::vector<UType> > ivals;
    std::vector<std::vector<UType> > ovals;
    for(int i=0; i<nthreads; i++){
      bool is_last_thread = (i == (nthreads - 1));
      std::vector<UType> ivals_ithread;
      std::vector<UType> ovals_ithread;
      if(!is_last_thread){
        ivals_ithread.resize(nelems_per_thread);
        ovals_ithread.resize(nelems_per_thread);
      }
      else{
        ivals_ithread.resize(nelems_last_thread);
        ovals_ithread.resize(nelems_last_thread);
      }
      int sval = i * nelems_per_thread;
      std::generate(ivals_ithread.begin(), ivals_ithread.end(),
                      [&sval]{  UType t;
                                t.f = static_cast<float>(sval);
                                t.i = sval++;
                                return t; });
      ivals.push_back(ivals_ithread);
      UType invalid_val;
      invalid_val.i = -1;
      invalid_val.f = -1.0;
      std::fill(ovals_ithread.begin(), ovals_ithread.end(), invalid_val);
      ovals.push_back(ovals_ithread);
    }

    /* Enqueue */
    std::vector<std::thread> tpool;
    for(int i=0; i<nthreads; i++){
      tpool.push_back(std::thread(thread_enq<UType>, std::ref(qu), std::ref(ivals[i]), std::chrono::seconds(0)));
    }

    /* Wait for enqueue operations to complete */
    for(int i=0; i<nthreads; i++){
      tpool[i].join();
    }
    tpool.clear();
    //std::cout << qu << std::endl;

    /* Dequeue */
    for(int i=0; i<nthreads; i++){
      tpool.push_back(std::thread(thread_deq<UType>, std::ref(qu), std::ref(ovals[i]), std::chrono::seconds(0)));
    }

    /* Wait for enqueue operations to complete */
    for(int i=0; i<nthreads; i++){
      tpool[i].join();
    }
    tpool.clear();

    /* Verify that we dequeued all elements in the queue */
    std::vector<bool> elem_in_q(max_elems_in_q, false);
    for(int i=0; i<nthreads; i++){
      for(int j=0; j<ovals[i].size(); j++){
        int ioval = (ovals[i])[j].i;
        int idx = ioval;
        float foval = (ovals[i])[j].f;
        if((idx < 0) || (idx >= max_elems_in_q)){
          /* Invalid element in queue */
          LOG_RANK0(wrank, "Invalid element (%d) in queue\n", idx);
          return PIO_EINTERNAL;
        }
        if(static_cast<int>(foval) != ioval){
          /* Invalid element in queue */
          LOG_RANK0(wrank, "Invalid element (%d) in queue\n", idx);
          return PIO_EINTERNAL;
        }
        /* Mark that the element was in the queue */
        elem_in_q[idx] = true;
      }
    }

    for(int i=0; i<max_elems_in_q; i++){
      if(elem_in_q[i] == false){
        /* At least one element was missing in the queue */
        LOG_RANK0(wrank, "Element (%d) was not found in the queue\n", i);
        return PIO_EINTERNAL;
      }
    }


    return PIO_NOERR;
}

/* Test enqueue and dequeue operations in a multi-threaded queue containing
 * pointers to user defined types */
int test_enq_deq_putype(int wrank, const int max_elems_in_q, const int nthreads)
{
    assert((max_elems_in_q > 0) && (nthreads > 0));
    int nelems_per_thread = max_elems_in_q/nthreads;
    int nelems_last_thread = max_elems_in_q - nelems_per_thread * (nthreads - 1);

    /* Enqueue multiple elems from different threads and dequeue to make
     * sure that the elems are in the queue
     */

    /* Generate input vals */
    PIO_Util::PIO_mtq<UType *> qu;
    std::vector<std::vector<UType *> > ivals;
    std::vector<std::vector<UType *> > ovals;
    for(int i=0; i<nthreads; i++){
      bool is_last_thread = (i == (nthreads - 1));
      std::vector<UType *> ivals_ithread;
      std::vector<UType *> ovals_ithread;
      if(!is_last_thread){
        ivals_ithread.resize(nelems_per_thread);
        ovals_ithread.resize(nelems_per_thread);
      }
      else{
        ivals_ithread.resize(nelems_last_thread);
        ovals_ithread.resize(nelems_last_thread);
      }
      int sval = i * nelems_per_thread;
      std::generate(ivals_ithread.begin(), ivals_ithread.end(),
                      [&sval]{  UType *pt = new UType();
                                pt->f = static_cast<float>(sval);
                                pt->i = sval++;
                                return pt; });
      ivals.push_back(ivals_ithread);
      UType invalid_val;
      invalid_val.i = -1;
      invalid_val.f = -1.0;
      std::fill(ovals_ithread.begin(), ovals_ithread.end(), &invalid_val);
      ovals.push_back(ovals_ithread);
    }

    /* Enqueue */
    std::vector<std::thread> tpool;
    for(int i=0; i<nthreads; i++){
      tpool.push_back(std::thread(thread_enq<UType *>, std::ref(qu), std::ref(ivals[i]),std::chrono::seconds(0)));
    }

    /* Wait for enqueue operations to complete */
    for(int i=0; i<nthreads; i++){
      tpool[i].join();
    }
    tpool.clear();
    //std::cout << qu << std::endl;

    /* Dequeue */
    for(int i=0; i<nthreads; i++){
      tpool.push_back(std::thread(thread_deq<UType *>, std::ref(qu), std::ref(ovals[i]), std::chrono::seconds(0)));
    }

    /* Wait for enqueue operations to complete */
    for(int i=0; i<nthreads; i++){
      tpool[i].join();
    }
    tpool.clear();

    /* Verify that we dequeued all elements in the queue */
    std::vector<bool> elem_in_q(max_elems_in_q, false);
    for(int i=0; i<nthreads; i++){
      for(int j=0; j<ovals[i].size(); j++){
        int ioval = ((ovals[i])[j])->i;
        int idx = ioval;
        float foval = ((ovals[i])[j])->f;
        if((idx < 0) || (idx >= max_elems_in_q)){
          /* Invalid element in queue */
          LOG_RANK0(wrank, "Invalid element (%d) in queue\n", idx);
          return PIO_EINTERNAL;
        }
        if(static_cast<int>(foval) != ioval){
          /* Invalid element in queue */
          LOG_RANK0(wrank, "Invalid element (%d) in queue\n", idx);
          return PIO_EINTERNAL;
        }
        delete((ovals[i])[j]);
        /* Mark that the element was in the queue */
        elem_in_q[idx] = true;
      }
    }

    for(int i=0; i<max_elems_in_q; i++){
      if(elem_in_q[i] == false){
        /* At least one element was missing in the queue */
        LOG_RANK0(wrank, "Element (%d) was not found in the queue\n", i);
        return PIO_EINTERNAL;
      }
    }


    return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
    int nerrs = 0, ret = PIO_NOERR;
    assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
    
    /* Test creating a multi threaded queue - the queue is empty */
    try{
      ret = test_create_mtq();
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_create_mtq() FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_create_mtq() PASSED\n");
    }

    /* Test enqueueing 10 ints into a queue using 2 threads, the ints are
     * removed from the queue and the results are verified
     */
    try{
      ret = test_enq_deq_int(wrank, 10, 2,
              std::chrono::seconds(0),
              std::chrono::seconds(0), false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_int(10 elems, 2 threads, no delay, deq after enq) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_int(10 elems, 2 threads, no delay, deq after enq) PASSED\n");
    }

    /* Test enqueueing 100 ints into a queue using 4 threads, the ints are
     * removed from the queue and the results are verified
     */
    try{
      ret = test_enq_deq_int(wrank, 100, 4,
              std::chrono::seconds(0),
              std::chrono::seconds(0), false);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_int(100 elems, 4 threads, no delay, deq after enq) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_int(100 elems, 4 threads, no delay, deq after enq) PASSED\n");
    }

    /* Test enqueueing 10 user defined elems into a queue using 2 threads,
     * the elems are removed from the queue and the results are verified
     */
    try{
      ret = test_enq_deq_utype(wrank, 10, 2);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_utype(10 elems, 2 threads) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_utype(10 elems, 2 threads) PASSED\n");
    }

    /* Test enqueueing 100 user defined elems into a queue using 4 threads,
     * the elems are removed from the queue and the results are verified
     */
    try{
      ret = test_enq_deq_utype(wrank, 100, 4);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_utype(100 elems, 4 threads) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_utype(100 elems, 4 threads) PASSED\n");
    }

    /* Test enqueueing 10 ptr to user defined elems into a queue using 2 threads,
     * the elems are removed from the queue and the results are verified
     */
    try{
      ret = test_enq_deq_putype(wrank, 10, 2);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_putype(10 elems, 2 threads) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_putype(10 elems, 2 threads) PASSED\n");
    }

    /* Test enqueueing 100 ptr to user defined elems into a queue using 4 threads,
     * the elems are removed from the queue and the results are verified
     */
    try{
      ret = test_enq_deq_putype(wrank, 100, 4);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_putype(100 elems, 4 threads) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_putype(100 elems, 4 threads) PASSED\n");
    }

    /* Test enqueueing 20 ints into a queue using 2 threads, the ints are
     * removed from the queue and the results are verified
     * The enqueue and dequeue operations happen on separate threads (2 threads
     * enqueue data - with a delay of 1s btw queuing elements and 2 threads
     * dequeue data - no delay)
     * Slow producer, fast consumer
     */
    try{
      ret = test_enq_deq_int(wrank, 20, 2,
              std::chrono::seconds(1),
              std::chrono::seconds(0), true);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_int(20 elems, 2 threads, 1s delay for queueing data, concurrent deq enq) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_int(20 elems, 2 threads, 1s delay for queueing data, concurrent deq enq) PASSED\n");
    }

    /* Test enqueueing 20 ints into a queue using 4 threads, the ints are
     * removed from the queue and the results are verified
     * The enqueue and dequeue operations happen on separate threads (4 threads
     * enqueue data - with a delay of 1s btw queuing elements and 4 threads
     * dequeue data - no delay)
     * Slow producer, fast consumer
     */
    try{
      ret = test_enq_deq_int(wrank, 20, 4,
              std::chrono::seconds(1),
              std::chrono::seconds(0), true);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_enq_deq_int(20 elems, 4 threads, 1s delay for queueing data, concurrent deq enq) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_enq_deq_int(20 elems, 4 threads, 1s delay for queueing data, concurrent deq enq) PASSED\n");
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
