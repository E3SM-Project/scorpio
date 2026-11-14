#include <cassert>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "spio_async_tcomm.hpp"

#if PIO_USE_ASYNC_WR_THREAD
#include <mutex>
#include <condition_variable>
#include "spio_async_op.hpp"
#include "spio_async_tpool.hpp"
#endif

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

static const int FAIL = -1;

#if PIO_USE_ASYNC_WR_THREAD
/* Data captured by an async op run on the (single) async I/O worker thread:
 * the thread's view of get_io_comm()/get_async_io_comm(), plus a condition
 * variable used to let the enqueuing (main) thread block until the op has
 * actually run on the worker thread. */
struct Captured_comms{
  SPIO_Util::TComm_info *tcomm_info;
  MPI_Comm io_comm;
  MPI_Comm async_io_comm;
  std::mutex mtx;
  std::condition_variable cv;
  bool done;
};

static int capture_comms_wait_fn(void *pdata)
{
  Captured_comms *cc = static_cast<Captured_comms *>(pdata);
  assert(cc && cc->tcomm_info);

  cc->io_comm = cc->tcomm_info->get_io_comm();
  cc->async_io_comm = cc->tcomm_info->get_async_io_comm();

  {
    std::lock_guard<std::mutex> lg(cc->mtx);
    cc->done = true;
  }
  cc->cv.notify_one();

  return PIO_NOERR;
}

static int capture_comms_poke_fn(void *pdata, bool &is_complete)
{
  is_complete = true;
  return PIO_NOERR;
}

static void capture_comms_free_fn(void *pdata)
{
}

/* Enqueue an op on the (real) async I/O thread pool and block until it has
 * run get_io_comm()/get_async_io_comm() on the worker thread, returning what
 * it saw. */
static void run_on_async_io_thread(SPIO_Util::TComm_info *tcomm_info, Captured_comms &cc)
{
  cc.tcomm_info = tcomm_info;
  cc.io_comm = MPI_COMM_NULL;
  cc.async_io_comm = MPI_COMM_NULL;
  cc.done = false;

  PIO_Util::PIO_async_tpool_manager tpool_mgr;
  PIO_Util::PIO_async_tpool *tpool = tpool_mgr.get_tpool_instance();

  SPIO_Util::Async_op op(SPIO_Util::Async_op::Op_type::SPIO_ASYNC_FILE_WRITE_OPS,
                          static_cast<void *>(&cc),
                          capture_comms_wait_fn, capture_comms_poke_fn, capture_comms_free_fn);
  tpool->enqueue(op);

  std::unique_lock<std::mutex> lk(cc.mtx);
  cc.cv.wait(lk, [&cc]{ return cc.done; });
}
#endif /* PIO_USE_ASYNC_WR_THREAD */

/* Build a TComm_info with distinct dups of MPI_COMM_WORLD for io_comm and
 * async_io_comm, and verify get_io_comm()/get_async_io_comm():
 *  (a) each return a valid, usable communicator (MPI_Comm_size() succeeds)
 *  (b) are stable across two calls from the same thread (the thread-local
 *      cache returns the exact same handle both times)
 *  (c) are distinct from each other (io vs async io are different comms)
 *
 * TComm_info's thread-local-dup pattern (mirrored here from the pre-existing
 * get_io_comm()) only actually dups the communicator for I/O-thread-pool
 * worker threads - the constructing/main thread's slot is the original
 * handle passed in (zero-copy for the common case). So, when
 * PIO_USE_ASYNC_WR_THREAD is on, this test also enqueues a real op on the
 * async I/O thread pool to verify, from inside the worker thread, that it
 * gets its own dup (not the original handle), and that the dup is stable
 * across two such calls.
 */
int test_io_and_async_io_comm(int wrank)
{
  int ret = PIO_NOERR;
  int mpierr = MPI_SUCCESS;

  MPI_Comm io_comm_in = MPI_COMM_NULL, async_io_comm_in = MPI_COMM_NULL;
  mpierr = MPI_Comm_dup(MPI_COMM_WORLD, &io_comm_in); assert(mpierr == MPI_SUCCESS);
  mpierr = MPI_Comm_dup(MPI_COMM_WORLD, &async_io_comm_in); assert(mpierr == MPI_SUCCESS);

  int io_comm_rank = 0, async_io_comm_rank = 0;
  MPI_Comm_rank(io_comm_in, &io_comm_rank);
  MPI_Comm_rank(async_io_comm_in, &async_io_comm_rank);

  SPIO_Util::TComm_info *tcomm_info = NULL;
  try{
    tcomm_info = new SPIO_Util::TComm_info(
        /* union_comm, union_comm_rank, union_comm_io_root, union_comm_comp_root */
        MPI_COMM_WORLD, wrank, 0, 0,
        /* io_comm, io_comm_rank, is_io_master */
        io_comm_in, io_comm_rank, (wrank == 0),
        /* async_io_comm, async_io_comm_rank, is_async_io_master */
        async_io_comm_in, async_io_comm_rank, (wrank == 0),
        /* comp_comm, comp_comm_rank, is_comp_master */
        MPI_COMM_WORLD, wrank, (wrank == 0),
        /* intercomm, my_comm, node_comm */
        MPI_COMM_NULL, MPI_COMM_WORLD, MPI_COMM_NULL);
  }
  catch(...){
    LOG_RANK0(wrank, "Unable to create TComm_info\n");
    MPI_Comm_free(&io_comm_in);
    MPI_Comm_free(&async_io_comm_in);
    return PIO_EINTERNAL;
  }

  /* Main thread: get_io_comm()/get_async_io_comm(), called twice */
  MPI_Comm got_io_comm_1 = tcomm_info->get_io_comm();
  MPI_Comm got_async_io_comm_1 = tcomm_info->get_async_io_comm();
  MPI_Comm got_io_comm_2 = tcomm_info->get_io_comm();
  MPI_Comm got_async_io_comm_2 = tcomm_info->get_async_io_comm();

  /* (a) valid, usable communicators */
  int sz = 0;
  if(MPI_Comm_size(got_io_comm_1, &sz) != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_size() on get_io_comm() result FAILED\n");
    ret = PIO_EINTERNAL;
  }
  if(MPI_Comm_size(got_async_io_comm_1, &sz) != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_size() on get_async_io_comm() result FAILED\n");
    ret = PIO_EINTERNAL;
  }

  /* (b) stable across two calls from the same (main) thread */
  if(got_io_comm_1 != got_io_comm_2){
    LOG_RANK0(wrank, "get_io_comm() returned different handles across two calls from the same thread\n");
    ret = PIO_EINTERNAL;
  }
  if(got_async_io_comm_1 != got_async_io_comm_2){
    LOG_RANK0(wrank, "get_async_io_comm() returned different handles across two calls from the same thread\n");
    ret = PIO_EINTERNAL;
  }

  /* (c) io and async io comms are distinct from each other */
  if(got_io_comm_1 == got_async_io_comm_1){
    LOG_RANK0(wrank, "get_io_comm() and get_async_io_comm() unexpectedly returned the same handle\n");
    ret = PIO_EINTERNAL;
  }

#if PIO_USE_ASYNC_WR_THREAD
  /* From the main/constructing thread, TComm_info hands back the original
   * handle directly (no dup) - this is the pre-existing get_io_comm()
   * behavior, unchanged by this task. */
  if(got_io_comm_1 != io_comm_in){
    LOG_RANK0(wrank, "get_io_comm() on the main thread unexpectedly did not return the original io_comm handle\n");
    ret = PIO_EINTERNAL;
  }
  if(got_async_io_comm_1 != async_io_comm_in){
    LOG_RANK0(wrank, "get_async_io_comm() on the main thread unexpectedly did not return the original async_io_comm handle\n");
    ret = PIO_EINTERNAL;
  }

  /* From an actual async I/O worker thread, both comms must be dups: valid,
   * distinct from the original handles, and stable across two calls from
   * that same worker thread. */
  Captured_comms cc1, cc2;
  run_on_async_io_thread(tcomm_info, cc1);
  run_on_async_io_thread(tcomm_info, cc2);

  if(MPI_Comm_size(cc1.io_comm, &sz) != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_size() on the worker thread's get_io_comm() result FAILED\n");
    ret = PIO_EINTERNAL;
  }
  if(MPI_Comm_size(cc1.async_io_comm, &sz) != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_size() on the worker thread's get_async_io_comm() result FAILED\n");
    ret = PIO_EINTERNAL;
  }
  if(cc1.io_comm == io_comm_in){
    LOG_RANK0(wrank, "Worker thread's get_io_comm() unexpectedly returned the original io_comm handle (not a dup)\n");
    ret = PIO_EINTERNAL;
  }
  if(cc1.async_io_comm == async_io_comm_in){
    LOG_RANK0(wrank, "Worker thread's get_async_io_comm() unexpectedly returned the original async_io_comm handle (not a dup)\n");
    ret = PIO_EINTERNAL;
  }
  if(cc1.io_comm == cc1.async_io_comm){
    LOG_RANK0(wrank, "Worker thread's get_io_comm() and get_async_io_comm() unexpectedly returned the same handle\n");
    ret = PIO_EINTERNAL;
  }
  if(cc1.io_comm != cc2.io_comm){
    LOG_RANK0(wrank, "Worker thread's get_io_comm() returned different handles across two calls from the same (worker) thread\n");
    ret = PIO_EINTERNAL;
  }
  if(cc1.async_io_comm != cc2.async_io_comm){
    LOG_RANK0(wrank, "Worker thread's get_async_io_comm() returned different handles across two calls from the same (worker) thread\n");
    ret = PIO_EINTERNAL;
  }
#endif /* PIO_USE_ASYNC_WR_THREAD */

  delete tcomm_info;
  MPI_Comm_free(&io_comm_in);
  MPI_Comm_free(&async_io_comm_in);

  return ret;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);

  try{
    ret = test_io_and_async_io_comm(wrank);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_io_and_async_io_comm() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_io_and_async_io_comm() PASSED\n");
  }

  *num_errors += nerrs;
  return nerrs;
}

int main(int argc, char *argv[])
{
  int ret;
  int wrank, wsz;
  int num_errors;
#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLinitialize();
  if(ret != 0){
    LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  ret = pio_test_mpi_init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Init() FAILED, ret = %d\n", ret);
    return ret;
  }

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
    return ret;
  }
  ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_size() FAILED, ret = %d\n", ret);
    return ret;
  }

  num_errors = 0;
  ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
  if(ret != 0){
    LOG_RANK0(wrank, "Test driver FAILED\n");
    return FAIL;
  }
  else{
    LOG_RANK0(wrank, "All tests PASSED\n");
  }

  MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLfinalize();
  if(ret != 0){
    LOG_RANK0(wrank, "GPTLfinalize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  if(num_errors != 0){
    LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
    return FAIL;
  }
  return 0;
}
