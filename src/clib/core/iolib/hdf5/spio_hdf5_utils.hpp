#ifndef __SPIO_HDF5_UTILS_HPP__
#define __SPIO_HDF5_UTILS_HPP__

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#if PIO_USE_HDF5
#include <hdf5.h>
#endif // PIO_USE_HDF5

#ifdef _HDF5

/* Reserved HDF5 attribute names written by spio_hdf5_utils.cpp to encode
 * netCDF-4-compatible structure (dimension ids / coordinate-variable
 * dimension ids) in a raw HDF5 file. Shared so that readers (e.g.
 * spio_hdf5_reopen.cpp's introspection of a SCORPIO-written file) do not
 * need to duplicate these literals.
 */
extern const char *SPIO_HDF5_ATT_NETCDF4_DIMID;
extern const char *SPIO_HDF5_ATT_NETCDF4_COORDINATES;

/* Function declarations */
int spio_hdf5_create(iosystem_desc_t *ios, file_desc_t *file, const char *filename);

int spio_hdf5_def_var(iosystem_desc_t *ios, file_desc_t *file, const char *name,
                      nc_type xtype, int ndims, const int *dimidsp, int varid);

int spio_hdf5_enddef(iosystem_desc_t *ios, file_desc_t *file);

int spio_hdf5_copy_att(iosystem_desc_t *ios, file_desc_t *ifile, int ivarid, const char *name,
                        file_desc_t *ofile, int ovarid);

int spio_hdf5_put_att(iosystem_desc_t *ios, file_desc_t *file, int varid, const char *name,
                      nc_type atttype, PIO_Offset len, const void *op);

int spio_hdf5_get_att(iosystem_desc_t *ios, file_desc_t *file, int varid, const char *name,
                      nc_type memtype, void *ip);

int spio_hdf5_put_var(iosystem_desc_t *ios, file_desc_t *file, int varid,
                      const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, nc_type xtype, const void *buf);

int spio_hdf5_get_var(iosystem_desc_t *ios, file_desc_t *file, int varid,
                      const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, nc_type xtype, void *buf);

int spio_hdf5_close(iosystem_desc_t *ios, file_desc_t *file);
int spio_hdf5_set_frame(file_desc_t *file, int varid, int frame);

PIO_Offset spio_hdf5_get_dim_chunk_sz_from_chunk_info(const std::string &dim_name);

/* Get the current on-disk extent of a 1-D dataset (a dimension-scale dataset
 * or a variable's dataset). Unlike file->hdf5_dims[dimid].len (which stores
 * the PIO_UNLIMITED sentinel for a record dimension - a "this is the
 * unlimited dim" marker, not a length - see PIOc_inq_dim_impl()'s doc
 * comment in pio_nc.cpp), this always returns the dataset's real current
 * length, whether or not it is a record dimension.
 */
int spio_hdf5_get_cur_dim_len(iosystem_desc_t *ios, file_desc_t *file,
                              hid_t dim_dataset_id, const char *dim_name, PIO_Offset *lenp);

/* Get the current (live) length of a dimension, correctly handling a record
 * (unlimited) dimension. file->hdf5_dims[dimid]'s own dataset is only
 * actually extended by spio_hdf5_put_var()/spio_hdf5_set_frame() when the
 * dimension also has a coordinate variable (has_coord_var: the dims-array
 * entry and the coordinate variable's vars-array entry share one aliased
 * HDF5 dataset id, so extending the variable also extends this dataset -
 * see spio_hdf5_utils.cpp:891). A "pure" dimension (no coordinate variable)
 * has its own, separate phantom dimension-scale dataset that is never
 * extended by anything - only the datasets of the real variables that use
 * the dimension are, so for that case this looks up the current extent of
 * each such variable's dataset (the dimension must be a variable's first/
 * slowest-varying dimension, same convention real netCDF4 uses to compute
 * its own reported unlimited dimension length) and returns the max.
 */
int spio_hdf5_get_unlimited_dimlen(iosystem_desc_t *ios, file_desc_t *file,
                                   int dimid, PIO_Offset *lenp);

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
