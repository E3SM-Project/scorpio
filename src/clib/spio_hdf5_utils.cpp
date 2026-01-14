/** @file
 * Utility functions for using the HDF5 library
 */
#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <regex>
#include <vector>
#include <array>
#include <iostream>
#include <algorithm>
//#include "spio_io_summary.h"
#include "spio_file_mvcache.h"
#include "spio_dt_converter.hpp"
//#include "spio_hash.h"
//#include "pio_rearr_contig.hpp"
//#include "spio_decomp_logger.hpp"
#include "spio_async_utils.hpp"
#include <typeinfo>
#include <vector>
#include <string>
#include <algorithm>
#include <mutex>

#include "spio_hdf5_utils.hpp"
/* Include headers for HDF5 compression filters */
#if PIO_USE_HDF5
#include <hdf5.h>
#ifdef _SPIO_HAS_H5Z_ZFP
#include "H5Zzfp_lib.h"
#include "H5Zzfp_props.h"
#endif
#ifdef _SPIO_HAS_H5Z_BLOSC2
#include "blosc2_filter.h"
#endif
#endif

#ifdef _HDF5
int spio_hdf5_create(iosystem_desc_t *ios, file_desc_t *file, const char *filename)
{
  int mpierr = MPI_SUCCESS, ret = PIO_NOERR;

  assert(ios && file && filename);
  assert((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C));
  assert(ios->ioproc);

  if(file->mode & PIO_NOCLOBBER){
    /* No clobber : Check if file exists from all processes */
    struct stat sd;
    if(0 == stat(filename, &sd)){
      return pio_err(ios, NULL, PIO_EEXIST, __FILE__, __LINE__,
                     "Creating file (%s) using HDF5 iotype failed. "
                     "The HDF5 file already exists and PIO_NOCLOBBER mode is specified",
                     filename);
    }
  }
  else{
    /* Clobber mode : Delete HDF5 file (from root I/O proc) if it exists */
    if(ios->io_rank == 0){
      struct stat sd;
      if(0 == stat(filename, &sd)) { unlink(filename); }
    }

    /* Wait for root process (that might delete an existing file) */
    if((mpierr = MPI_Barrier(ios->io_comm))){
      return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
    }
  }

  if(ios->info == MPI_INFO_NULL){
    if((mpierr = MPI_Info_create(&ios->info))){
      return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
    }
  }

  hid_t fcpl_id = H5Pcreate(H5P_FILE_CREATE);
  if(fcpl_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new file creation property list",
                   filename);
  }

  if(H5Pset_link_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set tracking and indexing of link creation order",
                   filename);
  }

  /* H5DSattach_scale calls (even with MPI_Barrier) might fail or hang if attribute creation
   * order is tracked or indexed. Before we have a better workaround, temporarily disable
   * tracking and indexing of attribute creation order. */
#if 0
  if(H5Pset_attr_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set tracking and indexing of attribute creation order",
                   filename);
  }
#endif

  hid_t fapl_id = H5Pcreate(H5P_FILE_ACCESS);
  if(fapl_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new file access property list",
                   filename);
  }

  if(H5Pset_fapl_mpio(fapl_id, ios->io_comm, ios->info) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to store the user-supplied MPI IO parameters",
                   filename);
  }

  file->hdf5_file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl_id, fapl_id);
  if(file->hdf5_file_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new HDF5 file",
                   filename);
  }

#if SPIO_DISABLE_HDF5_MPI_FILE_SYNC
  /* This is essentially the same as setting env variable, HDF5_DO_MPI_FILE_SYNC=FALSE */
  if(H5Fset_mpi_atomicity(file->hdf5_file_id, false) < 0){
    std::string warn_msg = std::string("Unable to turn off MPI file syncing for HDF5 output") +
                            "( file = " + pio_get_fname_from_file(file) + ")";
    PIOc_warn(ios->iosysid, file->pio_ncid, __FILE__, __LINE__, warn_msg.c_str());
  }
#endif

  if(H5Pclose(fcpl_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close a file creation property list",
                   filename);
  }

  if(H5Pclose(fapl_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close a file access property list",
                   filename);
  }

  /* Set up collective dataset transfer property list */
  file->dxplid_coll = H5Pcreate(H5P_DATASET_XFER);
  if(file->dxplid_coll == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new dataset transfer property list (collective data transfer)",
                   filename);
  }

  if(H5Pset_dxpl_mpio(file->dxplid_coll, H5FD_MPIO_COLLECTIVE) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set data transfer mode to collective",
                   filename);
  }

  /* Set up independent dataset transfer property list */
  file->dxplid_indep = H5Pcreate(H5P_DATASET_XFER);
  if(file->dxplid_indep == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new dataset transfer property list (independent data transfer)",
                   filename);
  }

  if(H5Pset_dxpl_mpio(file->dxplid_indep, H5FD_MPIO_COLLECTIVE) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set data transfer mode to collective",
                   filename);
  }

  if(H5Pset_dxpl_mpio_collective_opt(file->dxplid_indep, H5FD_MPIO_INDIVIDUAL_IO) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set data transfer mode to individual I/O",
                   filename);
  }

  /* Writing _NCProperties attribute */
  const char* attr_name = "_NCProperties";
  char nc_properties[PIO_MAX_NAME];
  unsigned int major, minor, release;

  if(H5get_libversion(&major, &minor, &release) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to retrieve the the version of the HDF5 library",
                   filename);
  }

  snprintf(nc_properties, PIO_MAX_NAME,
           "version=2,scorpio=%d.%d.%d,hdf5=%1u.%1u.%1u",
           PIO_VERSION_MAJOR, PIO_VERSION_MINOR, PIO_VERSION_PATCH,
           major, minor, release);

  hid_t attr_id;
  hsize_t asize = strlen(nc_properties);
  hid_t loc_id = file->hdf5_file_id;

  hid_t space_id = H5Screate(H5S_SCALAR);
  if(space_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                   filename);
  }

  hid_t h5_string_type = H5Tcopy(H5T_C_S1);
  if(h5_string_type == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to make a copy of the predefined string datatype in C",
                   filename);
  }

  assert(asize > 0);
  if(H5Tset_size(h5_string_type, asize) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set the total size (%ld bytes) for a derived C-style string datatype",
                   filename, asize);
  }

  if(H5Tset_strpad(h5_string_type, H5T_STR_NULLTERM) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to define the type of padding (NULL-terminated) used for a derived C-style string datatype",
                   filename);
  }

  if(H5Tset_cset(h5_string_type, H5T_CSET_ASCII) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set the character set (US ASCII) to be used in a derived C-style string datatype",
                   filename);
  }

  /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
  htri_t att_exists = H5Aexists(loc_id, attr_name);
  if(att_exists > 0) { assert(0); }
  else if(att_exists == 0){
    attr_id = H5Acreate2(loc_id, attr_name, h5_string_type, space_id, H5P_DEFAULT, H5P_DEFAULT);
    if(attr_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Creating file (%s) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to the file",
                     filename, attr_name);
    }
  }
  else{
    /* Error determining whether an attribute with a given name exists on an object */
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to determine whether an attribute (%s) exists on the file",
                   filename, attr_name);
  }

  if(H5Awrite(attr_id, h5_string_type, nc_properties) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to the file",
                   filename, attr_name);
  }

  if(H5Sclose(space_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to release a scalar dataspace",
                   filename);
  }

  if(H5Aclose(attr_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to the file",
                   filename, attr_name);
  }

  if(H5Tclose(h5_string_type) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Creating file (%s) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to release a derived C-style string datatype "
                   "used by an attribute (%s) attached to the file",
                   filename, attr_name);
  }

  return PIO_NOERR;
}

/* Create HDF5 dataset property ID */
static hid_t spio_create_hdf5_dataset_pid(iosystem_desc_t *ios, file_desc_t *file, const char *var_name, const std::vector<hsize_t> &max_dim_sz, nc_type var_type)
{
  herr_t ret;
  hid_t dpid = H5I_INVALID_HID;

  std::size_t var_ndims = max_dim_sz.size();

  assert((ios != NULL) && (file != NULL) && (var_ndims >= 0));

  bool var_has_more_than_four_dims = (max_dim_sz.size() > 4) ? true : false;
  bool var_has_only_unlimited_dims = true;
  bool var_is_scalar = true;
  for(std::vector<hsize_t>::const_iterator citer = max_dim_sz.cbegin(); citer != max_dim_sz.cend(); ++citer){
    if(*citer != H5S_UNLIMITED){
      var_has_only_unlimited_dims = false;
      if(*citer != 1) { var_is_scalar = false; }
    }
  }

  /* Initialize the compression filter property list */
  dpid = H5Pcreate(H5P_DATASET_CREATE);
  assert(dpid != H5I_INVALID_HID);

  /* We currently support compression for non-scalar data */
  if(file->iotype != PIO_IOTYPE_HDF5C) return dpid;
  if((var_ndims < 1) || (var_type == NC_CHAR)){
    std::string msg("Disabling HDF5 compression for variable");
      msg += std::string("(name=") + std::string(var_name) + std::string(", file=") + std::string(pio_get_fname_from_file(file)) + std::string(")");
      msg += (var_ndims < 1) ? std::string(" since its a scalar variable") : std::string(" since its a string/char variable");
    PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, msg.c_str());
    return dpid;
  }

  /* Check if any variables have compression disabled by the user */
  /* FIXME: Variables written out in a chunk size different from the one defined can cause hangs
   * e.g. E3SM variables : decomp_type, numlev, hwrt_prec, avgflag, fillvalue,
   *  meridional_complement, zonal_complement
   */
#ifndef SPIO_NO_CXX_REGEX
  std::regex vname_override_rgx(SPIO_OVERRIDE_HDF5_COMPRESSION_VNAME_REGEX);
  if(var_name && std::regex_match(std::string(var_name), vname_override_rgx)){
    std::string msg("Disabling HDF5 compression for variable");
      msg += std::string("(name=") + std::string(var_name) + std::string(", file=") + std::string(pio_get_fname_from_file(file)) + std::string(")");
      msg += std::string(" since it matches the user specified regex");
      msg += std::string(" (SPIO_OVERRIDE_HDF5_COMPRESSION_VNAME_REGEX=\"") + std::string(SPIO_OVERRIDE_HDF5_COMPRESSION_VNAME_REGEX) + std::string("\")");
    PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, msg.c_str());
    return dpid;
  }
#endif

#ifdef _SPIO_HDF5_USE_COMPRESSION

#ifdef _SPIO_HDF5_USE_LOSSY_COMPRESSION

#ifdef _SPIO_HAS_H5Z_ZFP
  /* Avoid ZFP compression for vars with only unlimited dims */
  if(var_has_only_unlimited_dims || var_is_scalar || var_has_more_than_four_dims){
    std::string msg("Disabling HDF5 ZFP compression for variable");
      msg += std::string("(name=") + std::string(var_name) + std::string(", file=") + std::string(pio_get_fname_from_file(file)) + std::string(")");
      msg += (var_has_only_unlimited_dims) ? std::string(" since it only has unlimited dims") :
              ((var_is_scalar) ? std::string(" since it is essentially a scalar variable") : std::string(" since variable has more than 4 dimensions"));
    PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, msg.c_str());
    return dpid;
  }

  if(SPIO_HDF5_ZFP_COMPRESSION_MODE == "H5Z_ZFP_MODE_RATE"){
    /* Lossy compression : Fixed bit rate : Number of bits used for compressed values is fixed, e.g. 16 */
    ret = H5Pset_zfp_rate(dpid, SPIO_HDF5_ZFP_COMPRESSION_RATE);
    if(ret < 0){
      PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, "Setting HDF5 ZFP filter compression rate failed (ignoring specific compression bit rate)");
    }
  }
  else if(SPIO_HDF5_ZFP_COMPRESSION_MODE == "H5Z_ZFP_MODE_PRECISION"){
    /* Lossy compression : Fixed precision Variable bit rate : Number of bits used for original value is fixed. e.g. 16 */
    ret = H5Pset_zfp_precision(dpid, SPIO_HDF5_ZFP_PRECISION);
    if(ret < 0){
      PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, "Setting HDF5 ZFP filter relative error bound failed (continuing with the default error bounds)");
    }
  }
  else if(SPIO_HDF5_ZFP_COMPRESSION_MODE == "H5Z_ZFP_MODE_ACCURACY"){
    /* Lossy compression : Fixed accuracy Variable bit rate : Absolute error between original and compressed values is bound e.g. 0.001 */
    ret = H5Pset_zfp_accuracy(dpid, SPIO_HDF5_ZFP_ACCURACY);
    if(ret < 0){
      PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, "Setting HDF5 ZFP filter absolute error bound failed (continuing with the default error bounds)");
    }
  }
  else if(SPIO_HDF5_ZFP_COMPRESSION_MODE == "H5Z_ZFP_MODE_REVERSIBLE"){
    /* Lossless compression */
    ret = H5Pset_zfp_reversible(dpid);
    if(ret < 0){
      PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, "Setting HDF5 ZFP filter for lossless compression failed (ignoring compression option)");
    }
  }
#endif

#else /* lossless compression, ifdef _SPIO_HDF5_USE_LOSSY_COMPRESSION */

#ifdef _SPIO_HAS_H5Z_BLOSC2
  /* Lossless compression : Default Blosc2 + ZSTD */
  unsigned int cd_values[7];
  cd_values[4] = SPIO_HDF5_BLOSC2_COMPRESSION_LEVEL; // compression level
  cd_values[5] = SPIO_HDF5_BLOSC2_SHUFFLE_METHOD; // shuffle option
  cd_values[6] = SPIO_HDF5_BLOSC2_COMPRESSION_LIBRARY; // Use ZSTD for compression
  ret = H5Pset_filter(dpid, FILTER_BLOSC2, H5Z_FLAG_OPTIONAL, 7, cd_values);
  if(ret < 0){
    PIOc_warn(ios->iosysid, file->fh, __FILE__, __LINE__, "User requested lossless compression, but setting HDF5 Blosc2 filter failed. Writing data without compression");
  }
#endif

#endif /* ifdef _SPIO_HDF5_USE_LOSSY_COMPRESSION */

#endif /* ifdef _SPIO_HDF5_USE_COMPRESSION */

  /* By default HDF5 does not track the order of creation of the attributes. So the attributes
   * appear based on the alphanumeric order, of the attribute name, in the file. However,
   * H5DSattach_scale calls (even with MPI_Barrier) might fail or hang if attribute creation
   * order is tracked or indexed. Before we have a better workaround, temporarily disable
   * tracking and indexing of attribute creation order.
   */
#if 0
  if(H5Pset_attr_creation_order(dcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to set tracking and indexing of attribute creation order",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }
#endif

  return dpid;
}

/* Get default chunk size (no of elems) - across each dimension - for variable data. The
 * max chunk size (across all dimensions) is specified via PIO_CHUNK_SIZE (in bytes)
 */
static inline std::vector<hsize_t> spio_get_dim_chunk_sz(const std::vector<hsize_t> &dim_sz, nc_type xtype)
{
  std::size_t ndims = dim_sz.size();
  std::vector<hsize_t> dim_chunk_sz = dim_sz;

  /* Unlimited dimensions have a chunk size of 1 */
  std::transform(dim_chunk_sz.begin(), dim_chunk_sz.end(), dim_chunk_sz.begin(),
                  [](hsize_t i) { return (i != H5S_UNLIMITED) ? i : 1; });

  /* No chunking for scalars and 1D vars */
  if(ndims <= 1) { return dim_chunk_sz; }

  /* Number of elements corresponding to PIO_CHUNK_SIZE */
  double chunk_nelems = static_cast<double>(PIO_CHUNK_SIZE)/static_cast<double>(spio_get_nc_type_size(xtype));
  /* Assuming that elements are evenly distributed across all non-unlimited dimensions,
   * Total (across all dimensions) chunked elements = (d * d * ...(n -1) times), where d is the size of each
   * dimension
   */
  hsize_t chunk_per_dim_nelems = static_cast<hsize_t>(pow(chunk_nelems, 1.0/(ndims - 1) ));

  for(std::size_t i = 0; i < ndims; i++){
    /* Chunk size across UNLIMITED dimension is 1 */
    dim_chunk_sz[i] = (dim_sz[i] != H5S_UNLIMITED) ? (std::min(chunk_per_dim_nelems, dim_sz[i])) : 1;
  }

  return dim_chunk_sz;
}

/* Create an HDF5 string type - ASCII + null terminated */
static inline hid_t spio_create_hdf5_str_type(void )
{
  hid_t st = H5Tcopy(H5T_C_S1);
  if(st != H5I_INVALID_HID){
    if(H5Tset_strpad(st, H5T_STR_NULLTERM) < 0){
      H5Tclose(st);
      return H5I_INVALID_HID;
    }
    if(H5Tset_cset(st, H5T_CSET_ASCII) < 0){
      H5Tclose(st);
      return H5I_INVALID_HID;
    }
  }

  return st;
}

/* Write a hidden coordinates attribute (_Netcdf4Coordinates), which lists the dimids of the variable. */
static inline int spio_add_nc_hidden_coord(iosystem_desc_t *ios, file_desc_t *file, int varid,
                                            int ndims, const int *dimidsp)
{
  assert(ios && file && (varid >= 0) && (ndims >= 0));

  /* No coordinate atttribute for scalars */
  if(ndims == 0) { return PIO_NOERR; }

  assert(dimidsp);

  /* Writing "_Netcdf4Coordinates" hidden attribute. This attribute stores the dimension ids of the
   * variable dimensions in an integer array.
   * This attribute is required for NetCDF to read HDF5 output
   */
  const char* attr_name = "_Netcdf4Coordinates";

  /* Sanity check : Ensure that "_Netcdf4Coordinates" attribute does not exist */
  htri_t attr_exists = H5Aexists(file->hdf5_vars[varid].hdf5_dataset_id, attr_name);
  assert(attr_exists == 0);

  /* Create dataspace for the attribute i.e., space for integer array to store the dimension ids */
  std::array<hsize_t, 1> coords_len = {static_cast<hsize_t>(ndims)};
  hid_t coords_space_id = H5Screate_simple(1, coords_len.data(), NULL);
  if(coords_space_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  /* Create the hidden attribute */
  hid_t coords_att_id = H5Acreate2(file->hdf5_vars[varid].hdf5_dataset_id, attr_name,
                                    H5T_NATIVE_INT, coords_space_id, H5P_DEFAULT, H5P_DEFAULT);
  if(coords_att_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to the variable",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, attr_name);
  }

  /* Write the dimension ids */
  if(H5Awrite(coords_att_id, H5T_NATIVE_INT, dimidsp) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to the variable",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, attr_name);
  }

  if(H5Aclose(coords_att_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to the variable",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, attr_name);
  }

  if(H5Sclose(coords_space_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to release a simple dataspace",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  return PIO_NOERR;
}

int spio_hdf5_def_var(iosystem_desc_t *ios, file_desc_t *file, const char *name,
                      nc_type xtype, int ndims, const int *dimidsp, int varid)
{
  int ret = PIO_NOERR;

  assert(ios && file && name && ndims >= 0 && varid >= 0);
  assert((ndims == 0) || dimidsp);
  assert((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C));
  assert(ios->ioproc);

  /* Cache the dim sizes for HDF5 calls */
  std::vector<hsize_t> dim_sz(ndims), max_dim_sz(ndims);
  for(int i = 0; i < ndims; i++){
    if(file->hdf5_dims[dimidsp[i]].len != PIO_UNLIMITED){
      dim_sz[i] = max_dim_sz[i] = file->hdf5_dims[dimidsp[i]].len;
    }
    else{
      dim_sz[i] = 1;
      max_dim_sz[i] = H5S_UNLIMITED;
    }
  }

  /* Create HDF5 dataset (and optionally add filters as needed) */
  hid_t dcpl_id = spio_create_hdf5_dataset_pid(ios, file, name, max_dim_sz, xtype);
  if(dcpl_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new dataset creation property list",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  file->hdf5_vars[varid].hdf5_type = (xtype == NC_CHAR) ? spio_create_hdf5_str_type() : spio_nc_type_to_hdf5_type(xtype);
  assert(file->hdf5_vars[varid].hdf5_type != H5I_INVALID_HID);

  /* Set default chunk size for variable data */
  if(ndims > 0){
    if(H5Pset_chunk(dcpl_id, ndims, spio_get_dim_chunk_sz(max_dim_sz, xtype).data()) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to set the size of the chunks used to store a chunked layout dataset",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }

  /* Create a simple dataspace to define the global variable dimensions */
  hid_t sid = H5Screate_simple(ndims, dim_sz.data(), max_dim_sz.data());
  if(sid == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  /* Define the variable */
  const char* dataset_name = (file->hdf5_vars[varid].alt_name == NULL)? name : file->hdf5_vars[varid].alt_name;
  file->hdf5_vars[varid].hdf5_dataset_id = H5Dcreate2(file->hdf5_file_id, dataset_name, file->hdf5_vars[varid].hdf5_type,
                                                      sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
  if(file->hdf5_vars[varid].hdf5_dataset_id < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new dataset (%s) for the variable",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid, dataset_name);
  }

  if(H5Sclose(sid) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to release a simple dataspace",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(H5Pclose(dcpl_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close a dataset creation property list",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  /* Add a hidden attribute, "_Netcdf4Coordinates", to store var dimension ids so that NetCDF can read the var */
  if(spio_add_nc_hidden_coord(ios, file, varid, ndims, dimidsp) != PIO_NOERR){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                   "Adding NetCDF hidden coordinate attribute failed",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  return PIO_NOERR;
}

int spio_hdf5_enddef(iosystem_desc_t *ios, file_desc_t *file)
{
  int i = 0, ret = PIO_NOERR;

  assert(ios && file);
  assert((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C));
  assert(ios->ioproc);

  for(i = 0; i < file->hdf5_num_dims; i++){
    /* For dimensions without an associated coordinate var, define them here. However since the
     * the user can call redef() multiple times define it only its not already defined/valid
     */
    if(!file->hdf5_dims[i].has_coord_var && (file->hdf5_dims[i].hdf5_dataset_id == H5I_INVALID_HID)){
      hid_t space_id, dcpl_id = H5I_INVALID_HID, dimscale_id;
      hsize_t dims[1], max_dims[1], chunk_dims[1] = {1};

      dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
      if(dcpl_id == H5I_INVALID_HID){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new dataset creation property list",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }

      /* H5DSattach_scale calls (even with MPI_Barrier) might fail or hang if attribute creation
       * order is tracked or indexed. Before we have a better workaround, temporarily disable
       * tracking and indexing of attribute creation order. */
#if 0
      if(H5Pset_attr_creation_order(dcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set tracking and indexing of attribute creation order",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }
#endif

      /* Set size of dataset to size of dimension. */
      max_dims[0] = dims[0] = file->hdf5_dims[i].len;

      /* If this dimension scale is unlimited, set up chunking with a chunksize of 1. */
      if(max_dims[0] == PIO_UNLIMITED){
        max_dims[0] = H5S_UNLIMITED;

        if(H5Pset_chunk(dcpl_id, 1, chunk_dims) < 0){
          return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                         "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                         "The low level (HDF5) I/O library call failed to set the size of the chunks used to store a chunked layout dataset",
                         pio_get_fname_from_file(file), file->pio_ncid);
        }
      }

      space_id = H5Screate_simple(1, dims, max_dims);
      if(space_id == H5I_INVALID_HID){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }

      /* Create a dataset that will be converted to a dimension scale. */
      dimscale_id = H5Dcreate2(file->hdf5_file_id, file->hdf5_dims[i].name, H5T_IEEE_F32BE,
                               space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
      if(dimscale_id < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new dataset (%s) that will be converted to a dimension scale",
                       pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name);
      }

      char dimscale_name[PIO_MAX_NAME];
      snprintf(dimscale_name, PIO_MAX_NAME, "%s%10d", "This is a netCDF dimension but not a netCDF variable.", (int)dims[0]);

      if(H5DSset_scale(dimscale_id, dimscale_name) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to convert a dataset (for dimension %s) to a dimension scale",
                       pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name);
      }

      file->hdf5_dims[i].hdf5_dataset_id = dimscale_id;

      /* Write a special attribute (_Netcdf4Dimid) for the netCDF-4 dimension ID. */
      hid_t dimid_att_id;
      htri_t attr_exists;

      hid_t dimid_space_id = H5Screate(H5S_SCALAR);
      if(dimid_space_id == H5I_INVALID_HID){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }

      /* Writing _Netcdf4Dimid attribute */
      const char* attr_name = "_Netcdf4Dimid";

      /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
      attr_exists = H5Aexists(dimscale_id, attr_name);
      if(attr_exists > 0) { assert(0); }
      else if(attr_exists == 0){
        dimid_att_id = H5Acreate2(dimscale_id, attr_name,
                                  H5T_NATIVE_INT, dimid_space_id, H5P_DEFAULT, H5P_DEFAULT);
        if(dimid_att_id == H5I_INVALID_HID){
          return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                         "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                         "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to a dimension scale (%s)",
                         pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
        }
      }
      else{
        /* Error determining whether an attribute with a given name exists on an object */
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to determine whether an attribute (%s) exists on a dimension scale (%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
      }

      if(H5Awrite(dimid_att_id, H5T_NATIVE_INT, &i) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to a dimension scale (%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
      }

      if(H5Sclose(dimid_space_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a scalar dataspace",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }

      if(H5Aclose(dimid_att_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to a dimension scale (%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
      }

      if(H5Sclose(space_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a simple dataspace",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }

      if(H5Pclose(dcpl_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close a dataset creation property list",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }
    }
  }

  for(i = 0; i < file->hdf5_num_vars; i++){
    /* Upgrade the dataset of a coordinate variable to a dimension scale */
    if(file->hdf5_vars[i].is_coord_var){
      /* Write a special attribute (_Netcdf4Dimid) for the netCDF-4 dimension ID. */
      hid_t dimscale_id = file->hdf5_vars[i].hdf5_dataset_id;
      hid_t dimid_att_id;
      htri_t attr_exists;

      /* Writing _Netcdf4Dimid attribute */
      const char* attr_name = "_Netcdf4Dimid";

      attr_exists = H5Aexists(dimscale_id, attr_name);
      if(attr_exists < 0){
        /* Error determining whether an attribute with a given name exists on an object */
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to determine whether an attribute (%s) exists on a dimension scale (%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
      }

      if(attr_exists > 0){
        /* If redef/enddef is called, potentially multiple times,
         * this attribute might already have been created
         */
        continue;
      }

      if(H5DSset_scale(file->hdf5_vars[i].hdf5_dataset_id, file->hdf5_vars[i].name) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to convert a dataset (for coordinate variable %s) to a dimension scale",
                       pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_vars[i].name);
      }

      assert(file->hdf5_vars[i].ndims > 0);
      int dimid = file->hdf5_vars[i].hdf5_dimids[0];
      file->hdf5_dims[dimid].hdf5_dataset_id = file->hdf5_vars[i].hdf5_dataset_id;

      hid_t dimid_space_id = H5Screate(H5S_SCALAR);
      if(dimid_space_id == H5I_INVALID_HID){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }

      dimid_att_id = H5Acreate2(dimscale_id, attr_name,
                                H5T_NATIVE_INT, dimid_space_id, H5P_DEFAULT, H5P_DEFAULT);
      if(dimid_att_id == H5I_INVALID_HID){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to a dimension scale (%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
      }

      if(H5Awrite(dimid_att_id, H5T_NATIVE_INT, &dimid) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to a dimension scale (%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
      }

      if(H5Aclose(dimid_att_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to a dimension scale (%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
      }

      if(H5Sclose(dimid_space_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a scalar dataspace",
                       pio_get_fname_from_file(file), file->pio_ncid);
      }
    }
    else{
      /* Not a coordinate var */
      int ndims = file->hdf5_vars[i].ndims;
      if(ndims > 0){
        int* dimids = file->hdf5_vars[i].hdf5_dimids;
        for(int j = 0; j < ndims; j++){
          if(H5DSattach_scale(file->hdf5_vars[i].hdf5_dataset_id, file->hdf5_dims[dimids[j]].hdf5_dataset_id, j) < 0){
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to attach a dimension scale (for dimension %s) to %dth dimension of a dataset (for variable %s)",
                           pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[dimids[j]].name, j, file->hdf5_vars[i].name);
          }

          /* According to HDF5 developers, the H5DS routines are not parallel, so all the ranks are going to be
           * doing the same operations. At some point, with enough iterations of the loop, HDF5 might get out of
           * step between the ranks.
           * Workaround: place a barrier to sync H5DSattach_scale calls.
           */
          MPI_Barrier(ios->io_comm);
        }
      }
    }
  }

  return PIO_NOERR;
}

int spio_hdf5_put_att(iosystem_desc_t *ios, file_desc_t *file, int varid, const char *name,
                      nc_type atttype, PIO_Offset len, const void *op)
{
  int ret = PIO_NOERR;
  hid_t attr_id;
  hid_t space_id;
  hsize_t asize = len;
  htri_t att_exists;
  hid_t loc_id;
  hid_t h5_xtype;

  assert(ios && file && name && op);
  assert((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C));
  assert(ios->ioproc);

  if(varid == PIO_GLOBAL){
    loc_id = file->hdf5_file_id;
  }
  else{
    loc_id = file->hdf5_vars[varid].hdf5_dataset_id;
  }

  if(atttype == NC_CHAR){
    /* String type */
    if(asize == 0){
      space_id = H5Screate(H5S_NULL);
    }
    else{
      space_id = H5Screate(H5S_SCALAR);
    }

    if(space_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    h5_xtype = H5Tcopy(H5T_C_S1);
    if(h5_xtype == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to make a copy of the predefined string datatype in C",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    /* For empty strings, asize is 0, while H5Tset_size() requires that size must be positive */
    if(H5Tset_size(h5_xtype, (asize == 0 ? 1 : asize)) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to set the total size (%ld bytes) for a derived C-style string datatype",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid, (asize == 0 ? 1 : asize));
    }

    if(H5Tset_strpad(h5_xtype, H5T_STR_NULLTERM) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to define the type of padding (NULL-terminated) used for a derived C-style string datatype",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if(H5Tset_cset(h5_xtype, H5T_CSET_ASCII) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to set the character set (US ASCII) to be used in a derived C-style string datatype",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }
  else{
    space_id = H5Screate_simple(1, &asize, &asize);
    if(space_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    h5_xtype = spio_nc_type_to_hdf5_type(atttype);
    if(h5_xtype == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "Unsupported variable type (type=%x)",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid, atttype);
    }
  }

  /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
  att_exists = H5Aexists(loc_id, name);
  if(att_exists > 0){
    attr_id = H5Aopen(loc_id, name, H5P_DEFAULT);
    if(attr_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) with HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to open this existing attribute",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }
  else if(att_exists == 0){
    attr_id = H5Acreate2(loc_id, name, h5_xtype, space_id, H5P_DEFAULT, H5P_DEFAULT);
    if(attr_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to create a new attribute",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }
  else{
    /* Error determining whether an attribute with a given name exists on an object */
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to determine whether this attribute exists",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(H5Awrite(attr_id, h5_xtype, op) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to write this attribute",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(H5Sclose(space_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to release a simple dataspace",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(H5Aclose(attr_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close this attribute",
                   name, varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  /* String attribute */
  if(atttype == NC_CHAR){
    if(H5Tclose(h5_xtype) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to release a derived C-style string datatype used by this attribute",
                     name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }

  return PIO_NOERR;
}

int spio_hdf5_put_var(iosystem_desc_t *ios, file_desc_t *file, int varid,
                      const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, nc_type xtype, const void *buf)
{
  int ret = PIO_NOERR;
  hsize_t dims[H5S_MAX_RANK];
  hsize_t mdims[H5S_MAX_RANK];

  assert(ios && file && varid >= 0 && buf);
  assert((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C));
  assert(ios->ioproc);

  hid_t file_space_id = H5Dget_space(file->hdf5_vars[varid].hdf5_dataset_id);
  if(file_space_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to make a copy of the dataspace of the dataset associated with this variable",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(H5Sget_simple_extent_dims(file_space_id, dims, mdims) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to retrieve dimension size and maximum size of a dataspace copied from the dataset associated with this variable",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  int ndims = file->hdf5_vars[varid].ndims;

  /* Extend record dimension if needed */
  if(ndims > 0 && start != NULL && count != NULL && mdims[0] == H5S_UNLIMITED && dims[0] < (hsize_t)(start[0] + count[0])){
    dims[0] = (hsize_t) (start[0] + count[0]);

    if(H5Sclose(file_space_id) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to release a dataspace copied from the dataset associated with this variable",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if(H5Dextend(file->hdf5_vars[varid].hdf5_dataset_id, dims) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to extend the dataset associated with this variable",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    file_space_id = H5Dget_space(file->hdf5_vars[varid].hdf5_dataset_id);
    if(file_space_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to make a copy of the dataspace of the dataset (extended) associated with this variable",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }

  hsize_t hstart[(ndims > 0) ? ndims : 1];
  hsize_t hcount[(ndims > 0) ? ndims : 1];
  hsize_t hstride[(ndims > 0) ? ndims : 1];
  hsize_t nelems = 0;

  for(int i = 0; i < ndims; i++){
    if(start){
      hstart[i] = (hsize_t) start[i];
    }
    else{
      hstart[i] = 0;
    }
    hcount[i] = 0;
    hstride[i] = 1;
  }

  /* Only the IO master does the IO */
  if(ios->iomaster == MPI_ROOT){
    if(count){
      for(int i = 0; i < ndims; i++){
        hcount[i] = (hsize_t)count[i];
      }
    }
    else{
      for(int i = 0; i < ndims; i++){
        hcount[i] = dims[i];
      }
    }

    nelems = (ndims > 0) ? 1 : 0;
    for(int i = 0; i < ndims; i++){
      nelems *= hcount[i];
    }

    if(stride){
      for(int i = 0; i < ndims; i++){
        hstride[i] = (stride[i] > 0) ? ((hsize_t)stride[i]) : 1 ;
      }
    }
  }

  hid_t mem_space_id = H5Screate_simple(ndims, hcount, hcount);
  if(mem_space_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(ndims > 0){
    if(H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, hstart, hstride, hcount, NULL) < 0){
      H5Eprint2(H5E_DEFAULT, stderr);
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to select a hyperslab region for a dataspace copied from the dataset associated with this variable",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }

  hid_t mem_type_id;
  if(xtype == NC_CHAR){
    /* String type */
    mem_type_id = H5Tcopy(H5T_C_S1);
    if(mem_type_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to make a copy of the predefined string datatype in C",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if(H5Tset_strpad(mem_type_id, H5T_STR_NULLTERM) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to define the type of padding (NULL-terminated) used for a derived C-style string datatype",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if(H5Tset_cset(mem_type_id, H5T_CSET_ASCII) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to set the character set (US ASCII) to be used in a derived C-style string datatype",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }
  else{
    mem_type_id = spio_nc_type_to_hdf5_type(xtype);
    if(mem_type_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "Unsupported memory type (type=%x)",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, xtype);
    }
  }

  /* Independent write */
  if(H5Dwrite(file->hdf5_vars[varid].hdf5_dataset_id, mem_type_id, mem_space_id,
               file_space_id, file->dxplid_indep, buf) < 0){
    H5Eprint2(H5E_DEFAULT, stderr);
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to write the dataset associated with this variable",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(H5Sclose(mem_space_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to release a simple dataspace",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(xtype == NC_CHAR){
    if(H5Tclose(mem_type_id) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to release a derived C-style string datatype used by this variable",
                     pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
  }

  if(H5Sclose(file_space_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to release a dataspace copied from the dataset associated with this variable",
                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
  }

  return PIO_NOERR;
}

int spio_hdf5_close(iosystem_desc_t *ios, file_desc_t *file)
{
  int i = 0;

  assert(ios && file);
  assert((file->iotype == PIO_IOTYPE_HDF5) || ((file->iotype == PIO_IOTYPE_HDF5C)));
  assert(ios->ioproc);

  /* Since close is always an async op, when async thread is used, we do not need
   * to explicitly wait for async ops to complete
   * i.e., we don't need spio_wait_all_hdf5_async_ops() here
   */
  if(H5Pclose(file->dxplid_coll) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close a dataset transfer property list (collective data transfer)",
                   pio_get_fname_from_file(file), file->pio_ncid);
  }

  if(H5Pclose(file->dxplid_indep) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to close a dataset transfer property list (independent data transfer)",
                   pio_get_fname_from_file(file), file->pio_ncid);
  }

  for(i = 0; i < file->hdf5_num_dims; i++){
    if(!file->hdf5_dims[i].has_coord_var){
      if(H5Dclose(file->hdf5_dims[i].hdf5_dataset_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close a dataset created for a dimension (%s, dimid = %d)",
                       pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name, i);
      }
    }
  }

  for(i = 0; i < file->hdf5_num_vars; i++){
    if(H5Dclose(file->hdf5_vars[i].hdf5_dataset_id) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to close a dataset created for a variable (%s, varid = %d)",
                     pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_vars[i].name, i);
    }

    if(file->hdf5_vars[i].nc_type == NC_CHAR){
      if(H5Tclose(file->hdf5_vars[i].hdf5_type) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a derived C-style string datatype created for a variable (%s, varid = %d)",
                       pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name, i);
      }
    }
  }

  if(H5Fclose(file->hdf5_file_id) < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to terminate access to an HDF5 file",
                   pio_get_fname_from_file(file), file->pio_ncid);
  }

  return PIO_NOERR;
}
#endif
