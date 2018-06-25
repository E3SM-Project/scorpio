#ifndef _PIO_ASYNC_TPOOL_HPP_
#define _PIO_ASYNC_TPOOL_HPP_

#include <iostream>
#include <thread>
#include <vector>
#include "pio_async_mtq.hpp"
extern "C"{
#include "pio_internal.h"
#include "pio_async_tpool_cint.h"
} // extern "C"

namespace PIO_Util{

class PIO_async_tpool{
  public:
    void enqueue(pio_async_op_t *op);
    void finalize(void );
  private:
    friend class PIO_async_tpool_manager;
    PIO_async_tpool(int nthreads);
    ~PIO_async_tpool();
    static int dequeue_and_process(PIO_async_tpool *tpool);
    PIO_Util::PIO_mtq<pio_async_op_t *> mtq_;
    std::vector<std::thread> pool_threads_;
};

class PIO_async_tpool_manager{
  public:
    static PIO_async_tpool *get_tpool_instance(void );
    ~PIO_async_tpool_manager();
  private:
    static PIO_async_tpool *tpool_;
};

} // namespace PIO_Util

#endif // _PIO_ASYNC_TPOOL_HPP_
