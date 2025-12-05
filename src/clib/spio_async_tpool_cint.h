#ifndef _PIO_ASYNC_THREAD_H_
#define _PIO_ASYNC_THREAD_H_

#include "pio.h"
#include "pio_internal.h"

extern "C"{
int pio_async_tpool_create(void );
int pio_async_tpool_op_add(pio_async_op_t *op);
int pio_async_tpool_ops_wait(void );
int pio_async_tpool_finalize(void );
} // extern "C"

#endif // _PIO_ASYNC_THREAD_H_
