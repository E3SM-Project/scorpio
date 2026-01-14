#ifndef __SPIO_ASYNC_HDF5_UTILS_HPP__
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"

namespace SPIO_Util{
  namespace GVars{
    extern std::atomic<int> npend_hdf5_async_ops;
  } // namespace GVars
} // namespace SPIO_Util

int spio_iosys_async_hdf5_create_op_add(file_desc_t *file, const char *filename);
int spio_iosys_async_hdf5_def_var_op_add(file_desc_t *file, const char *name,
      nc_type xtype, int ndims, const int *dimidsp, int varid);
int spio_iosys_async_hdf5_put_att_op_add(file_desc_t *file, int varid,
      const char *aname, nc_type atype, PIO_Offset alen, const void *abuf);
int spio_iosys_async_hdf5_enddef_op_add(file_desc_t *file);

int spio_wait_all_hdf5_async_ops(int iosysid);
int pio_iosys_async_op_hdf5_write(void *pdata);
void pio_iosys_async_op_hdf5_write_free(void *pdata);
int pio_iosys_async_hdf5_write_op_add(file_desc_t *file, int nvars, int fndims,
      const int *varids, io_desc_t *iodesc, int fill, const int *frame);

#define __SPIO_ASYNC_HDF5_UTILS_HPP__
#endif // __SPIO_ASYNC_HDF5_UTILS_HPP__
