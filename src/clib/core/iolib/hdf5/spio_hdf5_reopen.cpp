/** @file
 * Native HDF5 reopen support.
 *
 * PIOc_openfile_retry_impl() has historically handled every HDF5-iotype
 * reopen by switching file->iotype away to NETCDF4 (or, for a file that is
 * not even valid HDF5, PNETCDF/NETCDF) - see the H5Fis_hdf5() block in
 * pioc_support.cpp. That is a safe fallback, but it means a file SCORPIO
 * itself wrote with the HDF5 iotype can never be read back with the HDF5
 * iotype's own (eventual) read support.
 *
 * This file fixes that: spio_hdf5_is_scorpio_written() recognizes a file
 * written by spio_hdf5_create()/spio_hdf5_def_var()/spio_hdf5_enddef() (see
 * spio_hdf5_utils.cpp), and spio_hdf5_reopen() is the entry point
 * PIOc_openfile_retry_impl() calls before falling back to the historical
 * switch-away behavior. For a file it recognizes, spio_hdf5_reopen() walks
 * the file's root group and reconstructs file->hdf5_dims/hdf5_vars/
 * hdf5_attrs from what spio_hdf5_utils.cpp's writer actually put on disk -
 * see spio_hdf5_reopen_scan_cb()'s comment for the dimension/variable
 * classification and id-numbering scheme. For a file it does not recognize,
 * it reports PIO_EBADIOTYPE so the caller falls back to the existing
 * switch-to-NETCDF4 behavior instead.
 */
#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>

#include <cstring>
#include <cstdlib>

#include "spio_hdf5_reopen.hpp"
#include "spio_hdf5_utils.hpp"
/* For ios->tcomm_info's comm/MPI_Info accessors and
 * SPIO_Util::Iosys_Util::is_file_owning_io_proc(), both used by the
 * write-capable reopen's collective H5Pset_fapl_mpio() upgrade below. */
#include "spio_async_tcomm.hpp"
#include "spio_iosys_utils.hpp"

#ifdef _HDF5

namespace {

struct spio_hdf5_dimscale_scan_ctx_t
{
  bool found_dimscale;
};

/* Marker written (via H5DSset_scale()) on the dimension-scale dataset
 * created for a dimension that has no associated coordinate variable - see
 * spio_hdf5_utils.cpp's spio_hdf5_sync_defs(), Step 1:
 *   snprintf(dimscale_name, PIO_MAX_NAME, "%s%10d",
 *            "This is a netCDF dimension but not a netCDF variable.",
 *            (int) dims[0]);
 * i.e. the actual on-disk NAME is this literal followed by the dimension's
 * (10-char field width, printf("%10d", ...)) length - so recognizing a
 * "pure dimension" (as opposed to a coordinate variable, whose NAME is
 * simply its own name) requires a *prefix* match, not an exact match.
 */
const char *SPIO_HDF5_PURE_DIM_SCALE_NAME_PREFIX =
  "This is a netCDF dimension but not a netCDF variable.";

/* Reserved attribute names that are either HDF5 dimension-scale internals
 * (CLASS, NAME, DIMENSION_LIST, REFERENCE_LIST - see H5DSpublic.h) or
 * SCORPIO's own hidden netCDF-4-compatibility/marker attributes
 * (_Netcdf4Dimid, _Netcdf4Coordinates - see spio_hdf5_utils.hpp -  and
 * _NCProperties, a global attribute spio_hdf5_create() writes on every
 * SCORPIO-created HDF5 file recording the scorpio/HDF5 version that created
 * it, see spio_hdf5_utils.cpp). None of these are ordinary user-visible
 * attributes, so introspection must not surface them in file->hdf5_attrs[].
 */
const char *SPIO_HDF5_RESERVED_ATTR_NAMES[] =
{
  "CLASS", "NAME", "DIMENSION_LIST", "REFERENCE_LIST", "_NCProperties"
  /* _Netcdf4Dimid/_Netcdf4Coordinates are checked separately, via the
   * SPIO_HDF5_ATT_NETCDF4_DIMID/SPIO_HDF5_ATT_NETCDF4_COORDINATES extern
   * consts (spio_hdf5_utils.hpp), not duplicated here.
   */
};

bool spio_hdf5_is_reserved_attr_name(const char *name)
{
  if(strcmp(name, SPIO_HDF5_ATT_NETCDF4_DIMID) == 0){ return true; }
  if(strcmp(name, SPIO_HDF5_ATT_NETCDF4_COORDINATES) == 0){ return true; }

  for(size_t i = 0; i < sizeof(SPIO_HDF5_RESERVED_ATTR_NAMES) / sizeof(SPIO_HDF5_RESERVED_ATTR_NAMES[0]); i++){
    if(strcmp(name, SPIO_HDF5_RESERVED_ATTR_NAMES[i]) == 0){ return true; }
  }

  return false;
}

/* spio_hdf5_type_to_pio_type() (spio_hdf5_utils.hpp) only maps numeric
 * H5T_NATIVE_* types - it was written for variable/attribute *definition*,
 * where the on-disk type is always one of spio_nc_type_to_hdf5_type()'s
 * fixed set, never a string type (NC_CHAR is represented by a dynamically
 * created H5T_STRING-class type - see spio_create_hdf5_str_type() and
 * spio_hdf5_put_att()'s NC_CHAR branch - which is never H5Tequal() to
 * H5T_NATIVE_CHAR). Introspection has to recognize that on-disk case too, so
 * this thin wrapper checks the type *class* for H5T_STRING first and only
 * falls through to the shared helper (which remains the single place that
 * knows the numeric-type mapping) for everything else.
 */
nc_type spio_hdf5_reopen_type_to_pio_type(hid_t h5_type)
{
  if(H5Tget_class(h5_type) == H5T_STRING){
    return NC_CHAR;
  }

  return spio_hdf5_type_to_pio_type(h5_type);
}

/* Per-call state threaded through the root-group H5Literate2() walk below:
 * which file/file_desc_t we are populating, and a running tally of how many
 * dimensions/variables have been assigned ids so far (ids are assigned in
 * iteration order - see the long comment on the dimension/variable loop in
 * spio_hdf5_reopen() for why that reproduces the original numbering).
 */
struct spio_hdf5_reopen_ctx_t
{
  iosystem_desc_t *ios;
  file_desc_t *file;
  int next_varid;
  int max_dimid_seen;
  int ierr;
};

/* Per-attribute-scan state: which file/varid (PIO_GLOBAL for file-level
 * attributes) the attributes being iterated belong to.
 */
struct spio_hdf5_reopen_attr_ctx_t
{
  iosystem_desc_t *ios;
  file_desc_t *file;
  int varid;
  int ierr;
};

/* H5Aiterate2() callback, shared by both per-dataset and file-level
 * (global) attribute scans. Indexes by H5_INDEX_NAME, not H5_INDEX_CRT_ORDER
 * - attribute creation-order tracking is disabled everywhere in
 * spio_hdf5_utils.cpp (every H5Pset_attr_creation_order() call site is
 * `#if 0`'d out, due to a documented H5DSattach_scale() hang), so
 * H5_INDEX_CRT_ORDER is not available to iterate by here.
 *
 * KNOWN, ACCEPTED LIMITATION OF REOPEN (review finding I4): attribute
 * presence, type and length do not depend on iteration order, but the
 * eventual hdf5_attrs[] array index does - and that index IS meaningful.
 * PIOc_inq_attname_impl() (pio_nc.cpp, HDF5 branch) indexes
 * file->hdf5_attrs[attnum] directly by the caller's attnum, which netCDF
 * semantics define as the attribute's creation order. So a caller
 * enumerating attributes with PIOc_inq_natts() + PIOc_inq_attname() sees
 * creation order on a freshly-written file and alphabetical (name) order on
 * the same file after a reopen. Attribute lookup *by name*
 * (PIOc_inq_att/get_att) is unaffected; only enumeration order changes.
 * Fixing this properly means either re-enabling attribute creation-order
 * tracking in the writer (blocked by the H5DSattach_scale() hang noted
 * above) or persisting/sorting the order explicitly; neither is done here.
 */
extern "C" herr_t spio_hdf5_reopen_attr_cb(hid_t loc_id, const char *name,
                                            const H5A_info_t *ainfo, void *op_data)
{
  (void) ainfo;
  spio_hdf5_reopen_attr_ctx_t *ctx = static_cast<spio_hdf5_reopen_attr_ctx_t *>(op_data);

  if(spio_hdf5_is_reserved_attr_name(name)){
    return 0;
  }

  if(ctx->file->hdf5_num_attrs >= PIO_MAX_ATTRS){
    ctx->ierr = pio_err(ctx->ios, ctx->file, PIO_ENOMEM, __FILE__, __LINE__,
                        "Reopening file (%s) using the HDF5 iotype failed. "
                        "The file has more attributes than PIO_MAX_ATTRS (%d)",
                        pio_get_fname_from_file(ctx->file), PIO_MAX_ATTRS);
    return -1;
  }

  hid_t attr_id = H5Aopen(loc_id, name, H5P_DEFAULT);
  if(attr_id == H5I_INVALID_HID){
    ctx->ierr = pio_err(ctx->ios, ctx->file, PIO_EHDF5ERR, __FILE__, __LINE__,
                        "Reopening file (%s) using the HDF5 iotype failed. "
                        "The low level (HDF5) I/O library call failed to open an attribute (%s)",
                        pio_get_fname_from_file(ctx->file), name);
    return -1;
  }

  hid_t atype = H5Aget_type(attr_id);
  hid_t aspace = H5Aget_space(attr_id);
  if((atype == H5I_INVALID_HID) || (aspace == H5I_INVALID_HID)){
    if(atype != H5I_INVALID_HID) { H5Tclose(atype); }
    if(aspace != H5I_INVALID_HID) { H5Sclose(aspace); }
    H5Aclose(attr_id);
    ctx->ierr = pio_err(ctx->ios, ctx->file, PIO_EHDF5ERR, __FILE__, __LINE__,
                        "Reopening file (%s) using the HDF5 iotype failed. "
                        "The low level (HDF5) I/O library call failed to get the type/dataspace "
                        "of an attribute (%s)",
                        pio_get_fname_from_file(ctx->file), name);
    return -1;
  }

  nc_type pio_type = spio_hdf5_reopen_type_to_pio_type(atype);

  PIO_Offset att_len = 0;
  if(pio_type == NC_CHAR){
    /* A string attribute is written (see spio_hdf5_put_att()) as a scalar
     * dataspace with a fixed-size H5T_STRING type whose size is the string
     * length - except a zero-length string, which is written with an
     * H5S_NULL dataspace (and a type size of 1, since H5Tset_size(0) is not
     * meaningful). Detect that case via the dataspace class, not the type
     * size, so a true zero-length string round-trips as len == 0.
     */
    if(H5Sget_simple_extent_type(aspace) == H5S_NULL){
      att_len = 0;
    }
    else{
      att_len = static_cast<PIO_Offset>(H5Tget_size(atype));
    }
  }
  else{
    hssize_t npoints = H5Sget_simple_extent_npoints(aspace);
    att_len = (npoints > 0) ? static_cast<PIO_Offset>(npoints) : 0;
  }

  H5Sclose(aspace);
  H5Tclose(atype);
  H5Aclose(attr_id);

  int idx = ctx->file->hdf5_num_attrs;
  ctx->file->hdf5_attrs[idx].att_name = spio_strdup(name);
  ctx->file->hdf5_attrs[idx].att_type = pio_type;
  ctx->file->hdf5_attrs[idx].att_len = att_len;
  /* file->pio_ncid is not assigned until after spio_hdf5_reopen() returns
   * (PIOc_openfile_retry_impl() only calls pio_add_to_file_list() later) -
   * the caller is responsible for backfilling att_ncid once it is known,
   * the same way the ADIOS read path backfills adios_attrs[].att_ncid right
   * after pio_add_to_file_list() succeeds.
   */
  ctx->file->hdf5_attrs[idx].att_ncid = ctx->file->pio_ncid;
  ctx->file->hdf5_attrs[idx].att_varid = ctx->varid;

  ctx->file->hdf5_num_attrs++;
  if(ctx->varid == PIO_GLOBAL){
    ctx->file->hdf5_num_gattrs++;
  }

  return 0;
}

/* Scan every attribute attached to loc_id (a dataset or the file's root
 * group) into file->hdf5_attrs[], skipping reserved/internal names. Returns
 * PIO_NOERR, or the genuine PIO error code a callback already reported via
 * pio_err() (ctx.ierr) if iteration had to stop early.
 */
int spio_hdf5_reopen_scan_attrs(iosystem_desc_t *ios, file_desc_t *file, hid_t loc_id, int varid)
{
  spio_hdf5_reopen_attr_ctx_t actx;
  actx.ios = ios;
  actx.file = file;
  actx.varid = varid;
  actx.ierr = PIO_NOERR;

  herr_t iret = H5Aiterate2(loc_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            spio_hdf5_reopen_attr_cb, &actx);
  if(iret < 0){
    if(actx.ierr != PIO_NOERR){
      return actx.ierr;
    }
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to iterate over attributes",
                   pio_get_fname_from_file(file));
  }

  return PIO_NOERR;
}

/* Read the dimension length (and record-dimension status) of a 1-D
 * dimension-scale dataset's current on-disk extent - not necessarily the
 * length at the time the file was first written, since a record dimension
 * can have grown since then.
 */
int spio_hdf5_reopen_get_dim_len(iosystem_desc_t *ios, file_desc_t *file,
                                  hid_t did, const char *dim_name, PIO_Offset *lenp)
{
  hid_t sid = H5Dget_space(did);
  if(sid == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to get the dataspace "
                   "of a dimension-scale dataset (%s)",
                   pio_get_fname_from_file(file), dim_name);
  }

  hsize_t dims[1] = {0}, max_dims[1] = {0};
  if(H5Sget_simple_extent_dims(sid, dims, max_dims) < 0){
    H5Sclose(sid);
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to get the extent "
                   "of a dimension-scale dataset (%s)",
                   pio_get_fname_from_file(file), dim_name);
  }
  H5Sclose(sid);

  *lenp = (max_dims[0] == H5S_UNLIMITED) ? static_cast<PIO_Offset>(PIO_UNLIMITED) :
                                            static_cast<PIO_Offset>(dims[0]);
  return PIO_NOERR;
}

/* Read the "_Netcdf4Coordinates" hidden attribute (see
 * spio_add_nc_hidden_coord(), spio_hdf5_utils.cpp) off a variable's dataset,
 * allocating and filling *dimidsp with its *ndimsp dimension ids. A
 * variable with ndims == 0 (scalar) never has this attribute (see
 * spio_add_nc_hidden_coord()'s early return) - that is not an error, it just
 * means *ndimsp = 0 and *dimidsp = NULL.
 */
int spio_hdf5_reopen_get_var_dimids(iosystem_desc_t *ios, file_desc_t *file,
                                     hid_t did, const char *var_name,
                                     int *ndimsp, int **dimidsp)
{
  *ndimsp = 0;
  *dimidsp = NULL;

  htri_t attr_exists = H5Aexists(did, SPIO_HDF5_ATT_NETCDF4_COORDINATES);
  if(attr_exists < 0){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to determine whether "
                   "an attribute (%s) exists on a variable (%s)",
                   pio_get_fname_from_file(file), SPIO_HDF5_ATT_NETCDF4_COORDINATES, var_name);
  }

  if(attr_exists == 0){
    /* Scalar variable - no hidden coordinates attribute, by design */
    return PIO_NOERR;
  }

  hid_t attr_id = H5Aopen(did, SPIO_HDF5_ATT_NETCDF4_COORDINATES, H5P_DEFAULT);
  if(attr_id == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to open an attribute (%s) "
                   "on a variable (%s)",
                   pio_get_fname_from_file(file), SPIO_HDF5_ATT_NETCDF4_COORDINATES, var_name);
  }

  hid_t aspace = H5Aget_space(attr_id);
  if(aspace == H5I_INVALID_HID){
    H5Aclose(attr_id);
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to get the dataspace of "
                   "an attribute (%s) on a variable (%s)",
                   pio_get_fname_from_file(file), SPIO_HDF5_ATT_NETCDF4_COORDINATES, var_name);
  }

  hssize_t npoints = H5Sget_simple_extent_npoints(aspace);
  H5Sclose(aspace);
  if(npoints <= 0){
    H5Aclose(attr_id);
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "An attribute (%s) on a variable (%s) has an unexpected (non-positive) length",
                   pio_get_fname_from_file(file), SPIO_HDF5_ATT_NETCDF4_COORDINATES, var_name);
  }

  int ndims = static_cast<int>(npoints);
  int *dimids = static_cast<int *>(malloc(ndims * sizeof(int)));
  if(!dimids){
    H5Aclose(attr_id);
    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "Out of memory allocating space for the dimension ids of a variable (%s)",
                   pio_get_fname_from_file(file), var_name);
  }

  if(H5Aread(attr_id, H5T_NATIVE_INT, dimids) < 0){
    free(dimids);
    H5Aclose(attr_id);
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to read an attribute (%s) "
                   "on a variable (%s)",
                   pio_get_fname_from_file(file), SPIO_HDF5_ATT_NETCDF4_COORDINATES, var_name);
  }

  H5Aclose(attr_id);

  *ndimsp = ndims;
  *dimidsp = dimids;
  return PIO_NOERR;
}

/* Fill in nc_type/hdf5_type for a dataset's on-disk type, following the same
 * ownership convention spio_hdf5_def_var()/spio_hdf5_close() established for
 * the write path: for NC_CHAR, keep the (dynamically typed) H5Dget_type()
 * handle alive in hdf5_type (spio_hdf5_close() only H5Tclose()s it when
 * nc_type == NC_CHAR); for every other type, close the transient handle and
 * store the shared predefined constant instead.
 */
int spio_hdf5_reopen_get_var_type(iosystem_desc_t *ios, file_desc_t *file,
                                   hid_t did, const char *var_name,
                                   int *nc_typep, hid_t *hdf5_typep)
{
  hid_t h5_type = H5Dget_type(did);
  if(h5_type == H5I_INVALID_HID){
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to get the type of a "
                   "dataset (%s)",
                   pio_get_fname_from_file(file), var_name);
  }

  nc_type pio_type = spio_hdf5_reopen_type_to_pio_type(h5_type);

  if(pio_type == NC_CHAR){
    *nc_typep = pio_type;
    *hdf5_typep = h5_type;
  }
  else{
    H5Tclose(h5_type);
    *nc_typep = pio_type;
    *hdf5_typep = spio_nc_type_to_hdf5_type(pio_type);
  }

  return PIO_NOERR;
}

/* The "_nc4_non_coord_" prefix spio_hdf5_def_var() (spio_hdf5_utils.cpp)
 * mangles an on-disk dataset name with, when an ordinary (non-coordinate)
 * variable happens to share its name with some other dimension (see
 * pio_nc.cpp's def_var() for where alt_name gets set). Recovering it lets a
 * reopened variable's name and alt_name match exactly what a fresh def_var()
 * of the same schema would have produced.
 */
const char *SPIO_HDF5_NON_COORD_VAR_ALT_NAME_PREFIX = "_nc4_non_coord_";

/* H5Literate2() callback: the real introspection work. Iterates the root
 * group's immediate children in link-creation-order (H5_INDEX_CRT_ORDER -
 * tracked/indexed on the root group's GCPL, unlike attribute creation order;
 * see spio_hdf5_is_scorpio_written()'s check (a)), classifying each dataset
 * as one of:
 *   - a "pure" dimension (an HDF5 dimension-scale dataset with no associated
 *     coordinate variable - see spio_hdf5_utils.cpp's spio_hdf5_sync_defs()
 *     Step 1): recorded only into file->hdf5_dims[].
 *   - a coordinate variable (an HDF5 dimension-scale dataset that *is* also
 *     a variable - Step 2 of the same function): recorded into both
 *     file->hdf5_dims[] and file->hdf5_vars[].
 *   - an ordinary (non-scale) variable: recorded only into file->hdf5_vars[].
 *
 * Variable ids are assigned sequentially (0, 1, 2, ...) in the same order
 * the original def_var() calls created them: spio_hdf5_def_var() always
 * creates a variable's dataset immediately, at def_var() time, while
 * spio_hdf5_sync_defs() Step 1 only creates "pure" dimension datasets later,
 * at enddef() time - so the root group's link-creation-order is exactly
 * [vars in original def_var() call order] followed by [pure dimensions in
 * dimension-index order]. Filtering out "pure" dimensions and assigning
 * sequential ids to everything else therefore reproduces the exact original
 * varid numbering, without needing to persist it anywhere on disk.
 */
extern "C" herr_t spio_hdf5_reopen_scan_cb(hid_t loc_id, const char *name,
                                            const H5L_info2_t *info, void *op_data)
{
  (void) info;
  spio_hdf5_reopen_ctx_t *ctx = static_cast<spio_hdf5_reopen_ctx_t *>(op_data);
  iosystem_desc_t *ios = ctx->ios;
  file_desc_t *file = ctx->file;

  H5O_info2_t oinfo;
  if(H5Oget_info_by_name3(loc_id, name, &oinfo, H5O_INFO_BASIC, H5P_DEFAULT) < 0){
    ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                        "Reopening file (%s) using the HDF5 iotype failed. "
                        "The low level (HDF5) I/O library call failed to stat a link (%s) "
                        "in the root group",
                        pio_get_fname_from_file(file), name);
    return -1;
  }

  if(oinfo.type != H5O_TYPE_DATASET){
    /* Not a dataset (e.g. a group) - nothing SCORPIO's HDF5 writer creates
     * at the root group is anything but a dataset, so skip it rather than
     * fail the whole reopen over something unrelated sharing the file.
     */
    return 0;
  }

  hid_t did = H5Dopen2(loc_id, name, H5P_DEFAULT);
  if(did == H5I_INVALID_HID){
    ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                        "Reopening file (%s) using the HDF5 iotype failed. "
                        "The low level (HDF5) I/O library call failed to open a dataset (%s)",
                        pio_get_fname_from_file(file), name);
    return -1;
  }

  htri_t is_ds = H5DSis_scale(did);
  if(is_ds < 0){
    H5Dclose(did);
    ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                        "Reopening file (%s) using the HDF5 iotype failed. "
                        "The low level (HDF5) I/O library call failed to determine whether "
                        "a dataset (%s) is an HDF5 dimension scale",
                        pio_get_fname_from_file(file), name);
    return -1;
  }

  if(is_ds > 0){
    /* Dimension-scale dataset: either a "pure" dimension or a coordinate
     * variable - distinguish them via the dimension-scale NAME attribute
     * (see H5DSset_scale()/H5DSget_scale_name()): a pure dimension's NAME is
     * the magic prefix (plus a formatted length suffix); a coordinate
     * variable's NAME is simply its own (and its dimension's) name.
     */
    char scale_name[PIO_MAX_NAME + 64];
    /* H5DSget_scale_name() returns 0 - not a negative value - and leaves the
     * buffer untouched for a dimension scale with no NAME attribute, so the
     * "name_len < 0" check below does not cover that case and the strncmp()
     * further down would read uninitialized stack memory. A third-party file
     * can reach here: spio_hdf5_is_scorpio_written() only requires creation
     * -order flags plus *any* dimension scale. */
    scale_name[0] = '\0';
    ssize_t name_len = H5DSget_scale_name(did, scale_name, sizeof(scale_name));
    if(name_len < 0){
      H5Dclose(did);
      ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                          "Reopening file (%s) using the HDF5 iotype failed. "
                          "The low level (HDF5) I/O library call failed to get the "
                          "dimension-scale name of a dataset (%s)",
                          pio_get_fname_from_file(file), name);
      return -1;
    }

    bool is_pure_dim = (strncmp(scale_name, SPIO_HDF5_PURE_DIM_SCALE_NAME_PREFIX,
                                strlen(SPIO_HDF5_PURE_DIM_SCALE_NAME_PREFIX)) == 0);

    /* Every dimension-scale dataset (pure or coordinate-variable) carries a
     * "_Netcdf4Dimid" hidden attribute recording its dimension index - see
     * spio_hdf5_sync_defs() Steps 1 and 2.
     */
    htri_t dimid_attr_exists = H5Aexists(did, SPIO_HDF5_ATT_NETCDF4_DIMID);
    if(dimid_attr_exists <= 0){
      H5Dclose(did);
      ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                          "Reopening file (%s) using the HDF5 iotype failed. "
                          "A dimension-scale dataset (%s) is missing its (%s) hidden attribute",
                          pio_get_fname_from_file(file), name, SPIO_HDF5_ATT_NETCDF4_DIMID);
      return -1;
    }

    hid_t dimid_attr_id = H5Aopen(did, SPIO_HDF5_ATT_NETCDF4_DIMID, H5P_DEFAULT);
    if(dimid_attr_id == H5I_INVALID_HID){
      H5Dclose(did);
      ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                          "Reopening file (%s) using the HDF5 iotype failed. "
                          "The low level (HDF5) I/O library call failed to open an attribute (%s) "
                          "on a dimension-scale dataset (%s)",
                          pio_get_fname_from_file(file), SPIO_HDF5_ATT_NETCDF4_DIMID, name);
      return -1;
    }

    int dimid = -1;
    herr_t aret = H5Aread(dimid_attr_id, H5T_NATIVE_INT, &dimid);
    H5Aclose(dimid_attr_id);
    if((aret < 0) || (dimid < 0)){
      H5Dclose(did);
      ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                          "Reopening file (%s) using the HDF5 iotype failed. "
                          "The low level (HDF5) I/O library call failed to read a valid dimension id "
                          "from a dimension-scale dataset (%s)",
                          pio_get_fname_from_file(file), name);
      return -1;
    }

    if(dimid >= PIO_MAX_DIMS){
      H5Dclose(did);
      ctx->ierr = pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                          "Reopening file (%s) using the HDF5 iotype failed. "
                          "The file has more dimensions than PIO_MAX_DIMS (%d)",
                          pio_get_fname_from_file(file), PIO_MAX_DIMS);
      return -1;
    }

    PIO_Offset dim_len = 0;
    int ret = spio_hdf5_reopen_get_dim_len(ios, file, did, name, &dim_len);
    if(ret != PIO_NOERR){
      H5Dclose(did);
      ctx->ierr = ret;
      return -1;
    }

    file->hdf5_dims[dimid].name = spio_strdup(name);
    file->hdf5_dims[dimid].len = dim_len;
    file->hdf5_dims[dimid].chunk_sz = 0;
    file->hdf5_dims[dimid].has_coord_var = !is_pure_dim;
    file->hdf5_dims[dimid].hdf5_dataset_id = did;

    if(dimid > ctx->max_dimid_seen){
      ctx->max_dimid_seen = dimid;
    }

    if(!is_pure_dim){
      /* Coordinate variable: also record it as a variable. A coordinate
       * variable's dataset name on disk is always its own (unmangled) name -
       * spio_hdf5_def_var() only ever sets alt_name for a *non*-coordinate
       * variable that collides with a dimension name (see pio_nc.cpp's
       * def_var()) - so no alt_name recovery is needed here.
       */
      /* Every failure return from here on must close `did` itself: the
       * dimension-scale section above already stored it in
       * file->hdf5_dims[dimid].hdf5_dataset_id, but on a failed scan the
       * caller (spio_hdf5_reopen()) never sets file->hdf5_num_dims /
       * hdf5_num_vars, so nothing downstream will ever iterate over - and
       * release - that entry. The stored copy is invalidated alongside the
       * close so no stale id is left behind either. */
      if(ctx->next_varid >= PIO_MAX_VARS){
        H5Dclose(did);
        file->hdf5_dims[dimid].hdf5_dataset_id = H5I_INVALID_HID;
        ctx->ierr = pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                            "Reopening file (%s) using the HDF5 iotype failed. "
                            "The file has more variables than PIO_MAX_VARS (%d)",
                            pio_get_fname_from_file(file), PIO_MAX_VARS);
        return -1;
      }

      int varid = ctx->next_varid;

      int nc_type = PIO_NAT;
      hid_t hdf5_type = H5I_INVALID_HID;
      ret = spio_hdf5_reopen_get_var_type(ios, file, did, name, &nc_type, &hdf5_type);
      if(ret != PIO_NOERR){
        H5Dclose(did);
        file->hdf5_dims[dimid].hdf5_dataset_id = H5I_INVALID_HID;
        ctx->ierr = ret;
        return -1;
      }

      int ndims = 0;
      int *dimids = NULL;
      ret = spio_hdf5_reopen_get_var_dimids(ios, file, did, name, &ndims, &dimids);
      if(ret != PIO_NOERR){
        if(nc_type == NC_CHAR) { H5Tclose(hdf5_type); }
        H5Dclose(did);
        file->hdf5_dims[dimid].hdf5_dataset_id = H5I_INVALID_HID;
        ctx->ierr = ret;
        return -1;
      }

      if((ndims <= 0) || !dimids){
        /* A coordinate variable always has exactly one dimension (itself) -
         * spio_add_nc_hidden_coord() only skips writing the attribute for
         * ndims == 0 (scalars), which a coordinate variable can never be.
         */
        if(nc_type == NC_CHAR) { H5Tclose(hdf5_type); }
        if(dimids) { free(dimids); }
        H5Dclose(did);
        file->hdf5_dims[dimid].hdf5_dataset_id = H5I_INVALID_HID;
        ctx->ierr = pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                            "Reopening file (%s) using the HDF5 iotype failed. "
                            "A coordinate variable (%s) is missing its dimension ids",
                            pio_get_fname_from_file(file), name);
        return -1;
      }

      file->hdf5_vars[varid].name = spio_strdup(name);
      file->hdf5_vars[varid].alt_name = NULL;
      file->hdf5_vars[varid].nc_type = nc_type;
      file->hdf5_vars[varid].hdf5_type = hdf5_type;
      file->hdf5_vars[varid].ndims = ndims;
      file->hdf5_vars[varid].hdf5_dimids = dimids;
      file->hdf5_vars[varid].is_coord_var = true;
      file->hdf5_vars[varid].hdf5_dataset_id = did;

      ret = spio_hdf5_reopen_scan_attrs(ios, file, did, varid);
      if(ret != PIO_NOERR){
        H5Dclose(did);
        file->hdf5_dims[dimid].hdf5_dataset_id = H5I_INVALID_HID;
        file->hdf5_vars[varid].hdf5_dataset_id = H5I_INVALID_HID;
        ctx->ierr = ret;
        return -1;
      }

      ctx->next_varid++;
    }

    return 0;
  }

  /* Ordinary (non-scale) variable dataset */
  if(ctx->next_varid >= PIO_MAX_VARS){
    H5Dclose(did);
    ctx->ierr = pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                        "Reopening file (%s) using the HDF5 iotype failed. "
                        "The file has more variables than PIO_MAX_VARS (%d)",
                        pio_get_fname_from_file(file), PIO_MAX_VARS);
    return -1;
  }

  int varid = ctx->next_varid;

  const char *alt_name_prefix = SPIO_HDF5_NON_COORD_VAR_ALT_NAME_PREFIX;
  size_t alt_name_prefix_len = strlen(alt_name_prefix);
  bool has_alt_name = (strncmp(name, alt_name_prefix, alt_name_prefix_len) == 0);
  const char *real_name = has_alt_name ? (name + alt_name_prefix_len) : name;

  int nc_type = PIO_NAT;
  hid_t hdf5_type = H5I_INVALID_HID;
  int ret = spio_hdf5_reopen_get_var_type(ios, file, did, name, &nc_type, &hdf5_type);
  if(ret != PIO_NOERR){
    H5Dclose(did);
    ctx->ierr = ret;
    return -1;
  }

  int ndims = 0;
  int *dimids = NULL;
  ret = spio_hdf5_reopen_get_var_dimids(ios, file, did, name, &ndims, &dimids);
  if(ret != PIO_NOERR){
    if(nc_type == NC_CHAR) { H5Tclose(hdf5_type); }
    H5Dclose(did);
    ctx->ierr = ret;
    return -1;
  }

  file->hdf5_vars[varid].name = spio_strdup(real_name);
  file->hdf5_vars[varid].alt_name = has_alt_name ? spio_strdup(name) : NULL;
  file->hdf5_vars[varid].nc_type = nc_type;
  file->hdf5_vars[varid].hdf5_type = hdf5_type;
  file->hdf5_vars[varid].ndims = ndims;
  file->hdf5_vars[varid].hdf5_dimids = dimids;
  file->hdf5_vars[varid].is_coord_var = false;
  file->hdf5_vars[varid].hdf5_dataset_id = did;

  ret = spio_hdf5_reopen_scan_attrs(ios, file, did, varid);
  if(ret != PIO_NOERR){
    /* As in the coordinate-variable section above: a failed scan means
     * file->hdf5_num_vars is never set, so nothing downstream releases the
     * id already stored in file->hdf5_vars[varid].hdf5_dataset_id. */
    H5Dclose(did);
    file->hdf5_vars[varid].hdf5_dataset_id = H5I_INVALID_HID;
    ctx->ierr = ret;
    return -1;
  }

  ctx->next_varid++;

  return 0;
}

/* H5Literate2() callback: stop at the first immediate child of the
 * iterated group that H5DSis_scale() recognizes as an HDF5 dimension-scale
 * dataset - the marker spio_hdf5_utils.cpp's spio_hdf5_enddef() writes, via
 * H5DSset_scale(), on every SCORPIO dimension dataset (coordinate or
 * pure-dimension).
 */
extern "C" herr_t spio_hdf5_find_dimscale_cb(hid_t loc_id, const char *name,
                                              const H5L_info2_t *info, void *op_data)
{
  (void) info;
  spio_hdf5_dimscale_scan_ctx_t *ctx = static_cast<spio_hdf5_dimscale_scan_ctx_t *>(op_data);

  /* Check the link target's type before trying to open it as a dataset -
   * H5Dopen2() on a non-dataset (e.g. a group) would push an avoidable
   * error onto the HDF5 error stack (and, with the default auto-printing
   * error handler, print it) on every ordinary group link we encounter.
   */
  H5O_info2_t oinfo;
  if(H5Oget_info_by_name3(loc_id, name, &oinfo, H5O_INFO_BASIC, H5P_DEFAULT) < 0){
    /* Could not stat this link's target - skip it, do not fail the scan */
    return 0;
  }

  if(oinfo.type != H5O_TYPE_DATASET){
    return 0;
  }

  hid_t did = H5Dopen2(loc_id, name, H5P_DEFAULT);
  if(did == H5I_INVALID_HID){
    return 0;
  }

  htri_t is_ds = H5DSis_scale(did);
  H5Dclose(did);

  if(is_ds > 0){
    ctx->found_dimscale = true;
    return 1; /* Found one - stop iterating */
  }

  return 0;
}

/* Release every dataset id currently held in file->hdf5_dims[]/hdf5_vars[],
 * exactly once each, and invalidate the entries.
 *
 * Used only by spio_hdf5_reopen()'s write-capable upgrade block, on an
 * error path where some - but not necessarily all - of the datasets have
 * already been reopened against the new, collective file handle. The caller
 * invalidates every entry before that block starts reopening them, so an
 * entry that is still H5I_INVALID_HID here is simply one that was never
 * reached.
 *
 * The has_coord_var skip is load-bearing, not an optimization: for a
 * dimension that is also a coordinate variable the dims-array entry and the
 * vars-array entry hold the SAME hid_t (the writer establishes this at
 * spio_hdf5_utils.cpp:891 and the reopen path preserves it), so closing
 * both would be a double close. This mirrors spio_hdf5_close()'s own loops.
 */
void spio_hdf5_reopen_release_dset_ids(file_desc_t *file)
{
  assert(file);

  for(int i = 0; i < file->hdf5_num_dims; i++){
    if(!file->hdf5_dims[i].has_coord_var &&
       (file->hdf5_dims[i].hdf5_dataset_id != H5I_INVALID_HID)){
      H5Dclose(file->hdf5_dims[i].hdf5_dataset_id);
    }
    file->hdf5_dims[i].hdf5_dataset_id = H5I_INVALID_HID;
  }

  for(int i = 0; i < file->hdf5_num_vars; i++){
    if(file->hdf5_vars[i].hdf5_dataset_id != H5I_INVALID_HID){
      H5Dclose(file->hdf5_vars[i].hdf5_dataset_id);
    }
    file->hdf5_vars[i].hdf5_dataset_id = H5I_INVALID_HID;
  }
}

} // anonymous namespace

bool spio_hdf5_is_scorpio_written(hid_t file_id)
{
  if(file_id == H5I_INVALID_HID){
    return false;
  }

  /* (a) The root group's creation property list must have link creation
   * order both tracked and indexed - see spio_hdf5_create()'s
   * H5Pset_link_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED |
   * H5P_CRT_ORDER_INDEXED) call, made on the file creation property list at
   * file-creation time. H5Pget_link_creation_order() itself, however, only
   * accepts a *group* creation property list (it calls
   * H5P_object_verify(plist_id, H5P_GROUP_CREATE) internally) - so reading
   * the flags back requires going through the root group's GCPL
   * (H5Gget_create_plist()), not H5Fget_create_plist()'s FCPL, even though
   * the property was originally set on the FCPL. This alone is not
   * sufficient (nothing stops some other HDF5 writer from setting the same
   * property), so it is always combined with the dimension-scale marker
   * check below.
   */
  hid_t root_id = H5Gopen2(file_id, "/", H5P_DEFAULT);
  if(root_id == H5I_INVALID_HID){
    return false;
  }

  hid_t gcpl_id = H5Gget_create_plist(root_id);
  if(gcpl_id == H5I_INVALID_HID){
    H5Gclose(root_id);
    return false;
  }

  unsigned crt_order_flags = 0;
  herr_t pret = H5Pget_link_creation_order(gcpl_id, &crt_order_flags);
  H5Pclose(gcpl_id);
  if(pret < 0){
    H5Gclose(root_id);
    return false;
  }

  const unsigned expected_flags =
    static_cast<unsigned>(H5P_CRT_ORDER_TRACKED) | static_cast<unsigned>(H5P_CRT_ORDER_INDEXED);
  if((crt_order_flags & expected_flags) != expected_flags){
    H5Gclose(root_id);
    return false;
  }

  /* (b) The root group must contain at least one HDF5 dimension-scale
   * dataset (H5DSis_scale()) - every dimension SCORPIO defines gets one
   * (see spio_hdf5_utils.cpp's spio_hdf5_enddef(), which calls
   * H5DSset_scale() on every hdf5_dims[]/coordinate-variable dataset). An
   * empty file, or a file with no datasets at all, correctly falls through
   * to "false" here (H5Literate2() simply finds nothing to stop on,
   * leaving found_dimscale at its initial value).
   */
  spio_hdf5_dimscale_scan_ctx_t ctx;
  ctx.found_dimscale = false;

  herr_t iret = H5Literate2(root_id, H5_INDEX_NAME, H5_ITER_NATIVE, NULL,
                             spio_hdf5_find_dimscale_cb, &ctx);
  H5Gclose(root_id);

  if(iret < 0){
    return false;
  }

  return ctx.found_dimscale;
}

int spio_hdf5_reopen(iosystem_desc_t *ios, file_desc_t *file, const char *filename, int mode)
{
  assert(ios && file && filename);

  /* Cheap pre-check, using the exact same local/non-collective call
   * PIOc_openfile_retry_impl() itself uses right next to this function to
   * decide whether a file is HDF5 at all: if H5Fis_hdf5() does not confirm
   * this file is valid HDF5 *right now*, do not even attempt the heavier
   * H5Fopen() probe below - just report "not recognized" immediately. Aside
   * from avoiding needless work on a clearly-non-HDF5 file, this also keeps
   * this function's own view of the file consistent with the caller's: the
   * caller only reaches its own switch-iotype-away H5Fis_hdf5() check after
   * this function returns, so checking the identical thing first, here,
   * means both checks are evaluated back-to-back rather than one being
   * separated from the other by this function's (comparatively heavier)
   * H5Fopen()/H5Literate2() work - which matters because this probe is not
   * an MPI collective call, and a freshly-closed file on a shared
   * filesystem is exactly the kind of thing whose visibility can otherwise
   * flip between two closely-spaced, independent local opens.
   */
  if(H5Fis_hdf5(filename) <= 0){
    return PIO_EBADIOTYPE;
  }

  /* A plain, local (not MPI-IO collective) probe open, purely to read the
   * file-level/dataset-level metadata spio_hdf5_is_scorpio_written() needs,
   * and then (further below) to walk the root group and reconstruct
   * file->hdf5_dims/hdf5_vars/hdf5_attrs.
   *
   * For a write-capable reopen (mode & PIO_WRITE) this probe handle is NOT
   * the handle the file ends up using: once the metadata scan completes,
   * the block near the end of this function closes it (and every dataset id
   * opened against it) and reopens the file collectively, H5F_ACC_RDWR,
   * through an H5Pset_fapl_mpio() fapl built on the role-correct comm - the
   * same setup spio_hdf5_create() uses for a fresh create. A read-only
   * reopen keeps this serial probe handle for the rest of the session,
   * which is sufficient (and is what the HDF5 read paths already assume).
   *
   * A failure here is treated the same as "not recognized" (PIO_EBADIOTYPE),
   * not as a genuine, propagated error: since this probe is not an MPI
   * collective call, different ranks can observe different outcomes for the
   * exact same file at the exact same point in time. Treating a transient
   * H5Fopen() failure as a hard error here, instead, would let one rank take
   * the "genuine error, return immediately" path in
   * PIOc_openfile_retry_impl() while other ranks fall through normally - a
   * divergence that can deadlock any collective call later in that
   * function. Falling back uniformly avoids that regardless of why the open
   * failed.
   */
  hid_t fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
  if(fid == H5I_INVALID_HID){
    return PIO_EBADIOTYPE;
  }

  if(!spio_hdf5_is_scorpio_written(fid)){
    /* Not a SCORPIO-written HDF5 file (or indeterminate) - close the probe
     * handle and let the caller fall back to the existing (safe, if lossy)
     * switch-iotype-away behavior. This is an expected outcome, not an
     * error: every file that predates this feature, or that was written by
     * some other HDF5 writer, takes this path.
     */
    H5Fclose(fid);
    return PIO_EBADIOTYPE;
  }

  /* Recognized as a SCORPIO-written HDF5 file: walk the root group (in
   * link-creation order - tracked/indexed per spio_hdf5_is_scorpio_written()'s
   * check (a), unlike attribute creation order, which is disabled - see the
   * H5Aiterate2() calls below) and populate file->hdf5_dims/hdf5_vars/
   * hdf5_attrs from what is actually on disk.
   *
   * On genuine-error handling, and why an immediate per-rank pio_err()
   * return here is safe despite spio_hdf5_reopen() running on every calling
   * rank unconditionally (not just is_sync_io_proc() ranks - see
   * PIOc_openfile_retry_impl()'s call site comment in pioc_support.cpp):
   * unlike the H5Fopen() probe above (whose failure is deliberately folded
   * into PIO_EBADIOTYPE, not a hard error, precisely because visibility of a
   * freshly-closed file on a shared filesystem can differ across ranks at
   * the same instant), everything from here on only runs *after* every rank
   * has already independently confirmed, locally, that this exact file is
   * both valid HDF5 and SCORPIO-written. Architecture section 3's async-write lock
   * file (the create/remove helpers added to pio_file.cpp alongside
   * iosystem_desc_t::spio_lock_id) is what guarantees the file is quiescent
   * (no concurrent writer) by the time a read-only reopen gets
   * this far, so every rank is reading byte-identical, static content
   * through the same HDF5 library build. The only genuine-error conditions
   * introduced below - a hidden attribute missing where the write path
   * always creates one, a dimension/variable count that exceeds
   * PIO_MAX_DIMS/PIO_MAX_VARS/PIO_MAX_ATTRS, or H5Literate2()/H5Aiterate2()
   * itself failing - are therefore properties of the file's on-disk content
   * and this build's compile-time limits, not of anything rank-local or
   * transient, so every rank hits the same outcome. A truly transient,
   * rank-specific I/O failure at this point (e.g. a local disk hiccup) is a
   * known, narrow residual risk shared with any other non-collective
   * metadata read in this codebase; it is not something this function can
   * resolve without making the reopen itself collective, which is out of
   * scope here (file->mode/file->uses_async_io - and so the right
   * sub-communicator to collect over - are not yet established at this
   * point in PIOc_openfile_retry_impl(), see the fields' own comments in
   * pio_types.hpp).
   */
  spio_hdf5_reopen_ctx_t ctx;
  ctx.ios = ios;
  ctx.file = file;
  ctx.next_varid = 0;
  ctx.max_dimid_seen = -1;
  ctx.ierr = PIO_NOERR;

  hid_t root_id = H5Gopen2(fid, "/", H5P_DEFAULT);
  if(root_id == H5I_INVALID_HID){
    H5Fclose(fid);
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to open the root group",
                   filename);
  }

  herr_t iret = H5Literate2(root_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL,
                            spio_hdf5_reopen_scan_cb, &ctx);
  H5Gclose(root_id);

  if(iret < 0){
    H5Fclose(fid);
    if(ctx.ierr != PIO_NOERR){
      return ctx.ierr;
    }
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to iterate over the root group",
                   filename);
  }

  /* File-level (global) attributes */
  int ret = spio_hdf5_reopen_scan_attrs(ios, file, fid, PIO_GLOBAL);
  if(ret != PIO_NOERR){
    H5Fclose(fid);
    return ret;
  }

  file->hdf5_num_dims = ctx.max_dimid_seen + 1;
  file->hdf5_num_vars = ctx.next_varid;

  if(mode & PIO_WRITE){
    /* Upgrade from the serial recognition-probe handle to a real collective
     * MPI-IO handle, mirroring spio_hdf5_create()'s own fapl/comm setup
     * (spio_hdf5_utils.cpp) - by this point file->uses_async_io is already
     * established (PIOc_openfile_retry_impl() now computes it before calling
     * this function - see that function's comment at the computation site),
     * so the same role-aware comm selection that function uses for a fresh
     * create applies equally to a write-capable reopen.
     *
     * The probe's dataset ids (file->hdf5_dims[].hdf5_dataset_id /
     * file->hdf5_vars[].hdf5_dataset_id) and the probe's fid are not valid
     * against a different, closed-and-reopened file id - every one of them
     * must be closed here and reopened by name against the new fid.
     *
     * The coordinate-variable aliasing invariant is preserved throughout:
     * for a dimension that is also a coordinate variable, the dims-array
     * entry and the vars-array entry hold the SAME hid_t. That is how the
     * writer builds them (spio_hdf5_utils.cpp:891,
     * file->hdf5_dims[dimid].hdf5_dataset_id =
     * file->hdf5_vars[i].hdf5_dataset_id;) and how this file's own read-only
     * scan callback builds them, and it is precisely WHY spio_hdf5_close()
     * skips the dims-array id for a has_coord_var dim: there is only one id
     * to close, reachable through the vars array. So below, the probe
     * teardown closes a coordinate variable's dataset once (through the
     * vars-array loop, with the dims-array loop skipping it exactly as
     * spio_hdf5_close() does), and the reopen H5Dopen2()s it once (through
     * the vars-array loop) and copies the resulting id into the dims-array
     * entry. Opening it independently from each loop instead would yield two
     * distinct, independently-reference-counted ids, of which
     * spio_hdf5_close() would then close only one - leaving an object open
     * on a handle whose MPI-IO VFD fclose degree is H5F_CLOSE_SEMI (not
     * H5F_CLOSE_WEAK), so H5Fclose() - and with it PIOc_closefile() - would
     * fail for any file containing a coordinate variable.
     *
     * The probe close runs on EVERY rank (every rank ran the probe open and
     * the scan above - this function is deliberately not gated by
     * is_sync_io_proc(), see its call site in PIOc_openfile_retry_impl()),
     * but the collective reopen that follows it can only run on the ranks
     * that are actually members of the file's I/O communicator: for every
     * other rank ios->tcomm_info->get_io_comm()/get_async_io_comm() is
     * MPI_COMM_NULL, which H5Pset_fapl_mpio() rejects outright. Those ranks
     * keep no HDF5 file/dataset handle at all after a write-capable reopen -
     * they only need the reconstructed metadata (names/types/lengths) that
     * the inq paths read, and every HDF5 I/O entry point asserts
     * is_file_owning_io_proc() before touching a handle.
     *
     * On cross-thread safety (this reopen runs on the calling/main thread
     * while, in a PIO_USE_ASYNC_WR_THREAD build, HDF5 writes for other files
     * can be issued from async-op worker threads): the load-bearing
     * invariant is that PIOc_openfile_retry_impl() calls
     * spio_wait_all_hdf5_async_ops(ios->iosysid) UNCONDITIONALLY at
     * pioc_support.cpp:4698, strictly before it reaches the native-reopen
     * block at pioc_support.cpp:4900 that calls this function. Every rank
     * therefore drains its async-op thread pool before any rank can reach
     * the collective H5Fopen() below, which makes this reopen a true join
     * point: there is no concurrent worker-thread HDF5 or MPI activity
     * anywhere in the comm while it runs, hence no racing MPI_Comm_dup and
     * no concurrent access to HDF5's global state. That HDF5/MPI internally
     * dup the supplied comm into a private handle is also true, and is
     * necessary, but it is not by itself sufficient - it says nothing about
     * concurrent HDF5 global-state access. NOTE: if that drain is ever made
     * conditional - as its immediate neighbour, the lock-file wait at
     * pioc_support.cpp:4687-4692, already is (read-only reopens only) - this
     * safety argument must be revisited.
     */
    for(int i = 0; i < file->hdf5_num_dims; i++){
      /* Skip a has_coord_var dim: its id is the vars-array entry's id and is
       * closed by the loop below. Mirrors spio_hdf5_close(). */
      if(!file->hdf5_dims[i].has_coord_var){
        H5Dclose(file->hdf5_dims[i].hdf5_dataset_id);
      }
      file->hdf5_dims[i].hdf5_dataset_id = H5I_INVALID_HID;
    }
    for(int i = 0; i < file->hdf5_num_vars; i++){
      H5Dclose(file->hdf5_vars[i].hdf5_dataset_id);
      file->hdf5_vars[i].hdf5_dataset_id = H5I_INVALID_HID;
    }
    H5Fclose(fid);
    fid = H5I_INVALID_HID;

    if(!SPIO_Util::Iosys_Util::is_file_owning_io_proc(ios, file)){
      /* Not a member of this file's I/O comm - nothing to reopen here. */
      file->hdf5_file_id = H5I_INVALID_HID;
      file->dxplid_coll = H5Pcreate(H5P_DATASET_XFER);
      file->dxplid_indep = H5Pcreate(H5P_DATASET_XFER);
      if((file->dxplid_coll == H5I_INVALID_HID) || (file->dxplid_indep == H5I_INVALID_HID)){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Reopening file (%s) using the HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a dataset "
                       "transfer property list",
                       filename);
      }
      return PIO_NOERR;
    }

    MPI_Info *pinfo = ios->tcomm_info->create_mpi_info();
    if(!pinfo){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Reopening file (%s) using the HDF5 iotype failed. "
                     "Failed to create MPI Info object for the file", filename);
    }

    hid_t fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if(fapl_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Reopening file (%s) using the HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to create a new file access property list",
                     filename);
    }

    if(H5Pset_fapl_mpio(fapl_id, file->uses_async_io ? ios->tcomm_info->get_async_io_comm()
                                                      : ios->tcomm_info->get_io_comm(), *pinfo) < 0){
      H5Pclose(fapl_id);
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Reopening file (%s) using the HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to store the user-supplied MPI IO parameters",
                     filename);
    }

    fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);
    H5Pclose(fapl_id);
    if(fid == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Reopening file (%s) using the HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to collectively reopen the file for writing",
                     filename);
    }

    /* Dimensions that are not also coordinate variables are reopened here,
     * in dimension-id order, exactly as before. A has_coord_var dimension is
     * deliberately skipped: its dataset IS its coordinate variable's
     * dataset, which the variables loop below opens once and then shares
     * back into this array - the same aliasing the writer sets up at
     * spio_hdf5_utils.cpp:891. Opening it here as well would yield two
     * distinct ids for one dataset, of which spio_hdf5_close() closes only
     * one (see the discussion above).
     *
     * Note on loop order: the dimensions loop deliberately stays FIRST, so
     * that for a file with no coordinate variables the sequence of
     * H5Dopen2() calls is byte-for-byte the one this code has always made. */
    for(int i = 0; i < file->hdf5_num_dims; i++){
      if(file->hdf5_dims[i].has_coord_var){
        continue;
      }

      file->hdf5_dims[i].hdf5_dataset_id = H5Dopen2(fid, file->hdf5_dims[i].name, H5P_DEFAULT);
      if(file->hdf5_dims[i].hdf5_dataset_id == H5I_INVALID_HID){
        /* Release everything reopened so far - file->hdf5_file_id is never
         * set on this path, so no downstream cleanup can reach these ids. */
        spio_hdf5_reopen_release_dset_ids(file);
        H5Fclose(fid);
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Reopening file (%s) using the HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to reopen a dimension dataset (%s) "
                       "against the collectively-reopened file handle",
                       filename, file->hdf5_dims[i].name);
      }
    }

    for(int i = 0; i < file->hdf5_num_vars; i++){
      const char *on_disk_name = file->hdf5_vars[i].alt_name ? file->hdf5_vars[i].alt_name
                                                              : file->hdf5_vars[i].name;
      file->hdf5_vars[i].hdf5_dataset_id = H5Dopen2(fid, on_disk_name, H5P_DEFAULT);
      if(file->hdf5_vars[i].hdf5_dataset_id == H5I_INVALID_HID){
        /* Release everything reopened so far - file->hdf5_file_id is never
         * set on this path, so no downstream cleanup can reach these ids. */
        spio_hdf5_reopen_release_dset_ids(file);
        H5Fclose(fid);
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Reopening file (%s) using the HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to reopen a variable dataset (%s) "
                       "against the collectively-reopened file handle",
                       filename, on_disk_name);
      }

      if(file->hdf5_vars[i].is_coord_var){
        /* A coordinate variable always has exactly one dimension (itself),
         * and that dimension's dataset IS this dataset - alias the id.
         * The dimension id comes from the file's own (on-disk)
         * _Netcdf4Coordinates attribute, so bounds-check it for real rather
         * than with an assert() that a non-debug build would compile out. */
        if((file->hdf5_vars[i].ndims <= 0) || !file->hdf5_vars[i].hdf5_dimids ||
           (file->hdf5_vars[i].hdf5_dimids[0] < 0) ||
           (file->hdf5_vars[i].hdf5_dimids[0] >= file->hdf5_num_dims)){
          spio_hdf5_reopen_release_dset_ids(file);
          H5Fclose(fid);
          return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                         "Reopening file (%s) using the HDF5 iotype failed. "
                         "A coordinate variable (%s) has an out of range or missing dimension id",
                         filename, on_disk_name);
        }

        int coord_dimid = file->hdf5_vars[i].hdf5_dimids[0];
        file->hdf5_dims[coord_dimid].hdf5_dataset_id = file->hdf5_vars[i].hdf5_dataset_id;
      }
    }
    /* Every has_coord_var dimension must have been aliased by the variables
     * loop above. If one was not, the reconstructed metadata is internally
     * inconsistent (a dimension marked as having a coordinate variable, with
     * no coordinate variable referring back to it) - fail rather than leave
     * an invalid id behind for a later read/write to use. Cheap: no HDF5
     * calls, just a PIO_MAX_DIMS-bounded scan. */
    for(int i = 0; i < file->hdf5_num_dims; i++){
      if(file->hdf5_dims[i].has_coord_var &&
         (file->hdf5_dims[i].hdf5_dataset_id == H5I_INVALID_HID)){
        spio_hdf5_reopen_release_dset_ids(file);
        H5Fclose(fid);
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Reopening file (%s) using the HDF5 iotype failed. "
                       "A dimension (%s) is marked as having a coordinate variable, but no "
                       "coordinate variable refers back to it",
                       filename, file->hdf5_dims[i].name);
      }
    }
  }

  file->hdf5_file_id = fid;

  /* Dataset transfer property lists, matching spio_hdf5_create()'s. For a
   * read-only reopen both stay local (not MPI-IO collective), since that
   * path keeps the serial recognition-probe handle - see the long comment
   * above the H5Fopen() probe. For a write-capable reopen the handle was
   * just upgraded to a collective MPI-IO one above, so dxplid_coll is given
   * H5Pset_dxpl_mpio(H5FD_MPIO_COLLECTIVE) exactly as a fresh create does.
   */
  file->dxplid_coll = H5Pcreate(H5P_DATASET_XFER);
  file->dxplid_indep = H5Pcreate(H5P_DATASET_XFER);
  if((file->dxplid_coll == H5I_INVALID_HID) || (file->dxplid_indep == H5I_INVALID_HID)){
    H5Fclose(fid);
    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                   "Reopening file (%s) using the HDF5 iotype failed. "
                   "The low level (HDF5) I/O library call failed to create a dataset "
                   "transfer property list",
                   filename);
  }

  if(mode & PIO_WRITE){
    if(H5Pset_dxpl_mpio(file->dxplid_coll, H5FD_MPIO_COLLECTIVE) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Reopening file (%s) using the HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to set data transfer mode to collective",
                     filename);
    }
  }

  return PIO_NOERR;
}

#endif /* _HDF5 */
