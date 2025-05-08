#include <iostream>
#include <thread>
#include <vector>
extern "C"{
#include "pio_config.h"
} // extern "C"
#include "spio_async_mtq.hpp"
#include "spio_async_tpool.hpp"
extern "C"{
#include "pio_internal.h"
#include "spio_async_tpool_cint.h"
} // extern "C"

int pio_async_init_cnt = 0;
PIO_Util::PIO_async_tpool *PIO_Util::PIO_async_tpool_manager::tpool_ = NULL;
static PIO_Util::PIO_async_tpool_manager tpool_mgr;

void PIO_Util::PIO_async_tpool::enqueue(pio_async_op_t *op)
{
  assert(op);
  LOG((2, "PIO_async_tpool:enqueue: Enqueing async op, kind = %s",
      (op->op_type == PIO_ASYNC_REARR_OP) ? "PIO_ASYNC_REARR_OP" :
      ((op->op_type == PIO_ASYNC_PNETCDF_WRITE_OP) ? "PIO_ASYNC_PNETCDF_WRITE_OP" :
      ((op->op_type == PIO_ASYNC_FILE_WRITE_OPS) ? "PIO_ASYNC_FILE_WRITE_OPS" :
      "UNKNOWN"))));
  mtq_.enqueue(op);
}

void PIO_Util::PIO_async_tpool::finalize(void )
{
  mtq_.signal(PIO_Util::PIO_mtq<pio_async_op_t *>::PIO_MTQ_SIG_COMPLETE);
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
  mtq_.signal(PIO_Util::PIO_mtq<pio_async_op_t *>::PIO_MTQ_SIG_STOP);
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
  int ret = 0;
  assert(tpool);
  /* Wait in an infinite loop (until the threads receive a signal) for 
   * pending asynchronous operations queued in the thread pool
   */
  do{
    LOG((2, "PIO_async_tpool:dequeue_and_process: Waiting for async ops..."));
    pio_async_op_t *op = NULL;
    ret = tpool->mtq_.dequeue(op);
    if(ret == 0){
      LOG((2, "Tpool processing async op, kind = %s",
          (op->op_type == PIO_ASYNC_REARR_OP) ? "PIO_ASYNC_REARR_OP" :
          ((op->op_type == PIO_ASYNC_PNETCDF_WRITE_OP) ? "PIO_ASYNC_PNETCDF_WRITE_OP" :
          ((op->op_type == PIO_ASYNC_FILE_WRITE_OPS) ? "PIO_ASYNC_FILE_WRITE_OPS" :
          "UNKNOWN"))));
      /* We currently support only file write ops here */
      assert(op->op_type == PIO_ASYNC_FILE_WRITE_OPS);
      ret = op->wait(op->pdata);
      if(ret != PIO_NOERR){
        return pio_err(NULL, NULL, PIO_EINTERNAL, __FILE__, __LINE__,
                        "Internal error dequeuing and processing asynchronous operations in the thread pool. Internal error waiting on asynchronous file write operation");
      }
      op->free(op->pdata);
      free(op);
    }
  } while(ret == 0);

  return PIO_NOERR;
}

PIO_Util::PIO_async_tpool *
          PIO_Util::PIO_async_tpool_manager::get_tpool_instance(void )
{
  /* We need to make NUM_THREADS configurable by the user (compile-time) */
  const int NUM_THREADS = 1;
  if(tpool_ == NULL){
    LOG((2, "PIO_async_tpool_manager:get_tpool_instance: Creating new tpool instance"));
    tpool_ = new PIO_Util::PIO_async_tpool(NUM_THREADS);
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

int pio_async_tpool_op_add(pio_async_op_t *op)
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
