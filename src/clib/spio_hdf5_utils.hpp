#ifndef __SPIO_HDF5_UTILS_HPP__
#define __SPIO_HDF5_UTILS_HPP__

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#if PIO_USE_HDF5
#include <hdf5.h>
#endif // PIO_USE_HDF5

#ifdef _HDF5
hid_t spio_nc_type_to_hdf5_type(nc_type xtype);
int spio_hdf5_type_to_pio_type(hid_t xtype);
PIO_Offset hdf5_get_nc_type_size(nc_type xtype);

int spio_hdf5_create(iosystem_desc_t *ios, file_desc_t *file, const char *filename);

int spio_hdf5_def_var(iosystem_desc_t *ios, file_desc_t *file, const char *name,
                      nc_type xtype, int ndims, const int *dimidsp, int varid);

int spio_hdf5_enddef(iosystem_desc_t *ios, file_desc_t *file);

int spio_hdf5_put_att(iosystem_desc_t *ios, file_desc_t *file, int varid, const char *name,
                      nc_type atttype, PIO_Offset len, const void *op);

int spio_hdf5_put_var(iosystem_desc_t *ios, file_desc_t *file, int varid,
                      const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, nc_type xtype, const void *buf);

int spio_hdf5_close(iosystem_desc_t *ios, file_desc_t *file);
#endif

#endif // __SPIO_HDF5_UTILS_HPP__
