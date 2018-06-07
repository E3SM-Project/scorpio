#ifndef _PIO_ASYNC_THREAD_H_
#define _PIO_ASYNC_THREAD_H_
extern "C"{

  int pio_async_tpool_create(int nthreads);
  int pio_async_tpool_finalize(void );

} // extern "C"
#endif // _PIO_ASYNC_THREAD_H_
