#ifndef _SPIO_ASYNC_UTILS_HPP_
#define _SPIO_ASYNC_UTILS_HPP_

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#include <string>

int pio_file_async_pend_ops_wait(file_desc_t *file);
int pio_file_async_pend_op_add(file_desc_t *file,
      pio_async_op_type_t op_type, void *pdata);
int pio_var_rearr_and_cache(file_desc_t *file, var_desc_t *vdesc,
      io_desc_t *iodesc, void *buf,
      size_t buflen, void *fillvalue, int rec_num);
int pio_var_rem_cache_data(var_desc_t *vdesc, int rec_num, viobuf_cache_t **pviobuf);
int pio_file_compact_and_copy_rearr_data(void *dest, size_t dest_sz,
      io_desc_t *iodesc, file_desc_t *file, const int *varids,
      const int *frames, int nvars);

int pio_iosys_async_pend_op_add(iosystem_desc_t *iosys,
      pio_async_op_type_t op_type, void *pdata);
#if PIO_USE_ASYNC_WR_THREAD
int pio_tpool_async_pend_op_add(iosystem_desc_t *iosys,
      pio_async_op_type_t op_type, void *pdata);
#endif // PIO_USE_ASYNC_WR_THREAD

std::string pio_async_op_type_to_string(pio_async_op_type_t op);

#endif // _SPIO_ASYNC_UTILS_HPP_

