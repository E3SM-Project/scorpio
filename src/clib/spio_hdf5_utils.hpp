#ifndef __SPIO_HDF5_UTILS_HPP__
#define __SPIO_HDF5_UTILS_HPP__

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#if PIO_USE_HDF5
#include <hdf5.h>
#endif // PIO_USE_HDF5

#ifdef _HDF5

/* Function declarations */
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

/* Inline functions */
inline hid_t spio_nc_type_to_hdf5_type(nc_type xtype)
{
  switch(xtype){
    case NC_BYTE:   return H5T_NATIVE_UINT8;
    case NC_UBYTE:  return H5T_NATIVE_UCHAR;
    case NC_CHAR:   return H5T_NATIVE_CHAR;
    case NC_SHORT:  return H5T_NATIVE_SHORT;
    case NC_USHORT: return H5T_NATIVE_USHORT;
    case NC_INT:    return H5T_NATIVE_INT;
    case NC_UINT:   return H5T_NATIVE_UINT;
    case NC_FLOAT : return H5T_NATIVE_FLOAT;
    case NC_DOUBLE: return H5T_NATIVE_DOUBLE;
    case NC_INT64:  return H5T_NATIVE_INT64;
    case NC_UINT64: return H5T_NATIVE_UINT64;
    default: return H5I_INVALID_HID;
  }

  return H5I_INVALID_HID;
}

inline nc_type spio_hdf5_type_to_pio_type(hid_t ntype)
{
  /* switch() does not work with HDF5 "types" since these types are macros
   * (which include library initialization call, H5Open(), if needed)
   */
  if(H5Tequal(ntype, H5T_NATIVE_UINT8)) { return PIO_BYTE; }
  else if(H5Tequal(ntype, H5T_NATIVE_UCHAR)) { return PIO_UBYTE; }
  else if(H5Tequal(ntype, H5T_NATIVE_CHAR)) { return PIO_CHAR; }
  else if(H5Tequal(ntype, H5T_NATIVE_SHORT)) { return PIO_SHORT; }
  else if(H5Tequal(ntype, H5T_NATIVE_USHORT)) { return PIO_USHORT; }
  else if(H5Tequal(ntype, H5T_NATIVE_INT)) { return PIO_INT; }
  else if(H5Tequal(ntype, H5T_NATIVE_UINT)) { return PIO_UINT; }
  else if(H5Tequal(ntype, H5T_NATIVE_FLOAT)) { return PIO_FLOAT; }
  else if(H5Tequal(ntype, H5T_NATIVE_DOUBLE)) { return PIO_DOUBLE; }
  else if(H5Tequal(ntype, H5T_NATIVE_INT64)) { return PIO_INT64; }
  else if(H5Tequal(ntype, H5T_NATIVE_UINT64)) { return PIO_UINT64; }
  else{
    assert(0);
  }

  return PIO_NAT;
}


#endif

#endif // __SPIO_HDF5_UTILS_HPP__
