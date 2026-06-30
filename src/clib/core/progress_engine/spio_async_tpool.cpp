#include <iostream>
#include <thread>
#include <vector>
#include <utility>
#include <algorithm>
extern "C"{
#include "pio_config.h"
} // extern "C"
#include "spio_async_mtq.hpp"
#include "spio_async_tpool.hpp"
#include "pio_internal.h"

int pio_async_init_cnt = 0;
PIO_Util::PIO_async_tpool *PIO_Util::PIO_async_tpool_manager::tpool_ = NULL;
static PIO_Util::PIO_async_tpool_manager tpool_mgr;

void PIO_Util::PIO_async_tpool::enqueue(const SPIO_Util::Async_op &op)
{
  LOG((2, "PIO_async_tpool:enqueue: Enqueing async op, kind = %s", SPIO_Util::Async_op::op_type_to_string(op.type()).c_str()));
  mtq_.enqueue(op);
}

void PIO_Util::PIO_async_tpool::finalize(void )
{
  mtq_.signal(PIO_Util::PIO_mtq<SPIO_Util::Async_op>::PIO_MTQ_SIG_COMPLETE);
}

std::vector<std::size_t> PIO_Util::PIO_async_tpool::get_thread_ids(void ) const
{
  std::vector<std::size_t> htids;

  std::hash<std::thread::id> tid_hasher;
  std::for_each(pool_threads_.cbegin(), pool_threads_.cend(),
    [&tid_hasher, &htids](const std::thread &t) { htids.push_back(tid_hasher(t.get_id())); });

  return htids;
}

PIO_Util::PIO_async_tpool::PIO_async_tpool(int nthreads)
{
  LOG((2, "PIO_async_tpool:PIO_async_tpool: Creating %d threads", nthreads));
  for(int i=0; i<nthreads; i++){
    pool_threads_.push_back(std::thread(PIO_async_tpool::dequeue_and_process, this));
  }
}

PIO_Util::PIO_async_tpool::~PIO_async_tpool()
{
  LOG((2, "PIO_async_tpool:~PIO_async_tpool: Sending STOP signal"));
  mtq_.signal(PIO_Util::PIO_mtq<SPIO_Util::Async_op>::PIO_MTQ_SIG_STOP);
  for(std::vector<std::thread>::iterator iter = pool_threads_.begin();
      iter != pool_threads_.end(); ++iter){
    if(iter->joinable()){
      iter->join();
    }
  }
}

int PIO_Util::PIO_async_tpool::dequeue_and_process(
  PIO_Util::PIO_async_tpool *tpool)
{
  int ret = PIO_NOERR;
  assert(tpool);
  /* Wait in an infinite loop (until the threads receive a signal) for 
   * pending asynchronous operations queued in the thread pool
   */
  while(true){
    LOG((2, "PIO_async_tpool:dequeue_and_process: Waiting for async ops..."));

    try{
      SPIO_Util::Async_op op = tpool->mtq_.dequeue();
      ret = op.wait();
      if(ret != PIO_NOERR){
        return pio_err(NULL, NULL, PIO_EINTERNAL, __FILE__, __LINE__,
                        "Internal error dequeuing and processing asynchronous operation (%s) in the thread pool. Internal error waiting on asynchronous file write operation",
                        SPIO_Util::Async_op::op_type_to_string(op.type()).c_str());
      }
      op.free();
    }catch(const PIO_Util::PIO_mtq<SPIO_Util::Async_op>::Mtq_exception &e){
      /* MTQ exceptions are for MTQ signal handling, stop waiting on the queue */
      LOG((1, "MTQ exception : %s", e.what().c_str()));
      break;
    }
  }

  return PIO_NOERR;
}

PIO_Util::PIO_async_tpool *
          PIO_Util::PIO_async_tpool_manager::get_tpool_instance(void )
{
  if(tpool_ == NULL){
    LOG((2, "PIO_async_tpool_manager:get_tpool_instance: Creating new tpool instance"));
    tpool_ = new PIO_Util::PIO_async_tpool(get_num_threads());
  }
  else{
    LOG((2, "PIO_async_tpool_manager:get_tpool_instance: Retrieving tpool instance"));
  }
  return tpool_;
}

PIO_Util::PIO_async_tpool_manager::~PIO_async_tpool_manager()
{
  if(tpool_){
    LOG((2, "PIO_async_tpool_manager:~PIO_async_tpool_manager: Finalizing tpool"));
    tpool_->finalize();
    delete(tpool_);
    tpool_ = NULL;
  }
  else{
    LOG((2, "PIO_async_tpool_manager:~PIO_async_tpool_manager: tpool is already finalized/deleted"));
  }
}

int pio_async_tpool_create(void )
{
  /* Although creation and deletion of the thread pool is managed by the 
   * thread pool manager, we still need reference counting to decide on 
   * when to send signals to async threads to finish queue async ops
   * (Note: Multiple iosystems init/finalize but use the same thread pool)
   */
  LOG((2, "pio_async_tpool_create: Creating tpool (by ref)"));
  pio_async_init_cnt++;

  return PIO_NOERR;
}

int pio_async_tpool_op_add(const SPIO_Util::Async_op &op)
{
  PIO_Util::PIO_async_tpool *tpool = tpool_mgr.get_tpool_instance();
  assert(tpool);
  LOG((2, "pio_async_tpool_op_add(): Adding op"));
  tpool->enqueue(op);

  return PIO_NOERR;
}

int pio_async_tpool_ops_wait(void )
{
  /* Currently we don't support waiting on async ops, unless we are
   * terminating the async threads in *tpool_finalize()
   */ 
  assert(0);
}

int pio_async_tpool_finalize(void )
{
  pio_async_init_cnt--;
  if(pio_async_init_cnt == 0){
    LOG((2, "pio_async_tpool_finalize: Finalizing tpool instance"));
    PIO_Util::PIO_async_tpool *tpool = tpool_mgr.get_tpool_instance();
    assert(tpool);
    /* Signal threads waiting on queue to finish up/complete
     * the queued async ops and exit
     */
    tpool->finalize();
  }
  else{
    LOG((2, "pio_async_tpool_finalize: Decrement ref cnt"));
  }

  return PIO_NOERR;
}
