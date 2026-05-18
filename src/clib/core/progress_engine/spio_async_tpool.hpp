#ifndef _SPIO_ASYNC_TPOOL_HPP_
#define _SPIO_ASYNC_TPOOL_HPP_

#include <iostream>
#include <thread>
#include <vector>
#include "spio_async_op.hpp"
#include "spio_async_mtq.hpp"
#include "spio_async_utils.hpp"
#include "pio_internal.h"

namespace PIO_Util{

class PIO_async_tpool{
  public:
    void enqueue(const SPIO_Util::Async_op &op);
    void finalize(void );
    std::vector<std::size_t> get_thread_ids(void ) const;
  private:
    friend class PIO_async_tpool_manager;
    PIO_async_tpool(int nthreads);
    ~PIO_async_tpool();
    static int dequeue_and_process(PIO_async_tpool *tpool);
    PIO_Util::PIO_mtq<SPIO_Util::Async_op> mtq_;
    std::vector<std::thread> pool_threads_;
};

class PIO_async_tpool_manager{
  public:
    static int get_num_threads(void) { return SPIO_ASYNC_NTHREADS; }
    static PIO_async_tpool *get_tpool_instance(void );
    ~PIO_async_tpool_manager();
  private:
    static PIO_async_tpool *tpool_;
};

} // namespace PIO_Util

/* FIXME: Move it inside the SPIO_Util namespace */
int pio_async_tpool_create(void );
int pio_async_tpool_op_add(const SPIO_Util::Async_op &op);
int pio_async_tpool_ops_wait(void );
int pio_async_tpool_finalize(void );

#endif // _SPIO_ASYNC_TPOOL_HPP_
