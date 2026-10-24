/*
 * Test the detection/fallback skeleton for native HDF5 reopen:
 *  - spio_hdf5_is_scorpio_written() correctly distinguishes a file written
 *    by SCORPIO's own HDF5 writer (spio_hdf5_create()/spio_hdf5_def_var()/
 *    spio_hdf5_enddef()) from a plain HDF5 file with none of SCORPIO's
 *    markers.
 *  - spio_hdf5_reopen() reports the "not recognized, fall back" sentinel
 *    (PIO_EBADIOTYPE) for a file it does not recognize, without crashing
 *    and without leaking the probe H5Fopen() handle it used to check.
 *
 * This is a single-task test - none of what is being tested here depends
 * on a distributed-array decomposition or a multi-rank I/O topology.
 */
#include "pio_config.h"

#ifdef _HDF5

#include <cstdio>
#include <string>

#include <hdf5.h>
#include <hdf5_hl.h>

#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"
#include "spio_hdf5_reopen.hpp"

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

static const int FAIL = -1;

/* Reserved/internal HDF5 attribute names that spio_hdf5_reopen.cpp's
 * introspection must never surface in file->hdf5_attrs[] - mirrors the
 * (internal, anonymous-namespace) list in spio_hdf5_reopen.cpp plus the two
 * shared externs in spio_hdf5_utils.hpp. Kept here, independently, so this
 * test verifies the *visible* behavior (no reserved name ever appears in
 * hdf5_attrs[]) rather than reaching into spio_hdf5_reopen.cpp's internals.
 */
static bool test_is_reserved_attr_name(const char *name)
{
  static const char *reserved[] = {
    "_Netcdf4Dimid", "_Netcdf4Coordinates",
    "CLASS", "NAME", "DIMENSION_LIST", "REFERENCE_LIST", "_NCProperties"
  };

  if(!name){
    return false;
  }

  for(size_t i = 0; i < sizeof(reserved) / sizeof(reserved[0]); i++){
    if(strcmp(name, reserved[i]) == 0){
      return true;
    }
  }

  return false;
}

/* A successful spio_hdf5_reopen() heap-allocates file->hdf5_dims[].name,
 * file->hdf5_vars[].name/alt_name/hdf5_dimids and file->hdf5_attrs[].att_name
 * (via spio_strdup()/malloc()), exactly like spio_hdf5_def_var()/
 * spio_hdf5_sync_defs()/spio_hdf5_put_att() do for a freshly-defined file.
 * In production these are released by the generic file_desc_t cleanup in
 * pio_lists.cpp's pio_free_file() (reached via PIOc_closefile() ->
 * pio_delete_file_from_list()). These tests call spio_hdf5_reopen()
 * directly on a bare file_desc_t, bypassing that normal
 * pio_add_to_file_list()/pio_delete_file_from_list() lifecycle (there is no
 * ncid to register under), so this mirrors just the HDF5-specific part of
 * that cleanup to avoid leaking the allocations spio_hdf5_reopen() made.
 */
static void test_free_hdf5_reopen_allocs(file_desc_t *file)
{
  for(int i = 0; i < file->hdf5_num_dims; i++){
    if(file->hdf5_dims[i].name) { free(file->hdf5_dims[i].name); }
  }

  for(int i = 0; i < file->hdf5_num_vars; i++){
    if(file->hdf5_vars[i].name) { free(file->hdf5_vars[i].name); }
    if(file->hdf5_vars[i].alt_name) { free(file->hdf5_vars[i].alt_name); }
    if(file->hdf5_vars[i].hdf5_dimids) { free(file->hdf5_vars[i].hdf5_dimids); }
  }

  for(int i = 0; i < file->hdf5_num_attrs; i++){
    if(file->hdf5_attrs[i].att_name) { free(file->hdf5_attrs[i].att_name); }
  }
}

/* Create a SCORPIO-written HDF5 file (via the real PIOc API, so it goes
 * through spio_hdf5_create()/spio_hdf5_def_var()/spio_hdf5_enddef() exactly
 * like production code) with a single dimension and no coordinate variable,
 * so spio_hdf5_utils.cpp's spio_hdf5_enddef() creates a pure-dimension
 * dimension-scale dataset for it - the marker spio_hdf5_is_scorpio_written()
 * looks for.
 */
static int make_scorpio_written_hdf5_file(MPI_Comm comm, const char *fname)
{
  int ret;
  int iosysid = -1;
  int ncid = -1;
  int dimid = -1;
  int format = PIO_IOTYPE_HDF5;

  if((ret = PIOc_Init_Intracomm(comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid))){
    return ret;
  }

  if((ret = PIOc_createfile(iosysid, &ncid, &format, fname, PIO_CLOBBER))){
    PIOc_finalize(iosysid);
    return ret;
  }

  if((ret = PIOc_def_dim(ncid, "x", (PIO_Offset) 4, &dimid))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  if((ret = PIOc_enddef(ncid))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  if((ret = PIOc_closefile(ncid))){
    PIOc_finalize(iosysid);
    return ret;
  }

  PIOc_finalize(iosysid);
  return PIO_NOERR;
}

/* Create a plain HDF5 file with no SCORPIO-specific markers at all: no
 * H5Pset_link_creation_order() on its creation property list, no
 * H5DSset_scale()'d dataset - just a single ordinary dataset, created
 * directly through the HDF5 C API (not through any SCORPIO code path).
 */
static int make_plain_hdf5_file(const char *fname)
{
  hid_t fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if(fid == H5I_INVALID_HID){
    return FAIL;
  }

  hsize_t dims[1] = { 4 };
  hid_t sid = H5Screate_simple(1, dims, NULL);
  if(sid == H5I_INVALID_HID){
    H5Fclose(fid);
    return FAIL;
  }

  hid_t did = H5Dcreate2(fid, "plain_var", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  if(did == H5I_INVALID_HID){
    H5Sclose(sid);
    H5Fclose(fid);
    return FAIL;
  }

  H5Dclose(did);
  H5Sclose(sid);
  H5Fclose(fid);

  return PIO_NOERR;
}

/* Step 1(a)/1(b): spio_hdf5_is_scorpio_written() returns true for a
 * SCORPIO-written file and false for a plain HDF5 file with none of
 * SCORPIO's markers.
 */
static int test_is_scorpio_written(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  const char *scorpio_fname = "test_hdf5_reopen_scorpio_written.nc";
  const char *plain_fname = "test_hdf5_reopen_plain.nc";

  if((ret = make_scorpio_written_hdf5_file(comm, scorpio_fname))){
    LOG_RANK0(wrank, "test_is_scorpio_written() FAILED: could not create the SCORPIO-written "
      "fixture file (ret = %d)\n", ret);
    return PIO_EINTERNAL;
  }

  if((ret = make_plain_hdf5_file(plain_fname))){
    LOG_RANK0(wrank, "test_is_scorpio_written() FAILED: could not create the plain HDF5 "
      "fixture file (ret = %d)\n", ret);
    return PIO_EINTERNAL;
  }

  int nerrs = 0;

  hid_t scorpio_fid = H5Fopen(scorpio_fname, H5F_ACC_RDONLY, H5P_DEFAULT);
  if(scorpio_fid == H5I_INVALID_HID){
    LOG_RANK0(wrank, "test_is_scorpio_written() FAILED: H5Fopen(\"%s\") failed\n", scorpio_fname);
    nerrs++;
  }
  else{
    bool is_scorpio = spio_hdf5_is_scorpio_written(scorpio_fid);
    if(!is_scorpio){
      LOG_RANK0(wrank, "test_is_scorpio_written() FAILED: spio_hdf5_is_scorpio_written() "
        "returned false for a file actually written via spio_hdf5_create()/spio_hdf5_def_var()/"
        "spio_hdf5_enddef() (\"%s\")\n", scorpio_fname);
      nerrs++;
    }
    H5Fclose(scorpio_fid);
  }

  hid_t plain_fid = H5Fopen(plain_fname, H5F_ACC_RDONLY, H5P_DEFAULT);
  if(plain_fid == H5I_INVALID_HID){
    LOG_RANK0(wrank, "test_is_scorpio_written() FAILED: H5Fopen(\"%s\") failed\n", plain_fname);
    nerrs++;
  }
  else{
    bool is_scorpio = spio_hdf5_is_scorpio_written(plain_fid);
    if(is_scorpio){
      LOG_RANK0(wrank, "test_is_scorpio_written() FAILED: spio_hdf5_is_scorpio_written() "
        "returned true for a plain HDF5 file with none of SCORPIO's markers (\"%s\")\n",
        plain_fname);
      nerrs++;
    }
    H5Fclose(plain_fid);
  }

  remove(scorpio_fname);
  remove(plain_fname);

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Step 5-8: spio_hdf5_reopen() on a plain (non-SCORPIO-written) HDF5 file
 * returns the "not recognized, fall back" sentinel (PIO_EBADIOTYPE),
 * without crashing, and without leaking the probe H5Fopen() handle it used
 * internally to check recognition (verified via the global open-file-object
 * count, H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_FILE), which must be
 * unchanged by the call).
 */
static int test_reopen_fallback_sentinel(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  const char *plain_fname = "test_hdf5_reopen_fallback.nc";

  if((ret = make_plain_hdf5_file(plain_fname))){
    LOG_RANK0(wrank, "test_reopen_fallback_sentinel() FAILED: could not create the plain HDF5 "
      "fixture file (ret = %d)\n", ret);
    return PIO_EINTERNAL;
  }

  int iosysid = -1;
  if((ret = PIOc_Init_Intracomm(comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid))){
    LOG_RANK0(wrank, "test_reopen_fallback_sentinel() FAILED: PIOc_Init_Intracomm() returned %d\n", ret);
    remove(plain_fname);
    return PIO_EINTERNAL;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios){
    LOG_RANK0(wrank, "test_reopen_fallback_sentinel() FAILED: pio_get_iosystem_from_id() returned NULL\n");
    PIOc_finalize(iosysid);
    remove(plain_fname);
    return PIO_EINTERNAL;
  }

  file_desc_t fake_file{};
  fake_file.iotype = PIO_IOTYPE_HDF5;
  strncpy(fake_file.fname, plain_fname, PIO_MAX_NAME);
  fake_file.fname[PIO_MAX_NAME] = '\0';

  ssize_t nopen_before = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_FILE);

  int reopen_ret = spio_hdf5_reopen(ios, &fake_file, plain_fname, PIO_NOWRITE);

  ssize_t nopen_after = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_FILE);

  int nerrs = 0;
  if(reopen_ret != PIO_EBADIOTYPE){
    LOG_RANK0(wrank, "test_reopen_fallback_sentinel() FAILED: spio_hdf5_reopen() returned %d, "
      "expected PIO_EBADIOTYPE (%d), for a file it should not recognize\n", reopen_ret, PIO_EBADIOTYPE);
    nerrs++;
  }

  if(nopen_after != nopen_before){
    LOG_RANK0(wrank, "test_reopen_fallback_sentinel() FAILED: the number of globally open HDF5 "
      "file objects changed from %lld to %lld across the spio_hdf5_reopen() call - its internal "
      "probe H5Fopen() handle was not closed before returning\n",
      (long long) nopen_before, (long long) nopen_after);
    nerrs++;
  }

  PIOc_finalize(iosysid);
  remove(plain_fname);

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Create a SCORPIO-written HDF5 file with a schema rich enough to exercise
 * every case spio_hdf5_reopen()'s introspection has to handle:
 *  - dim "x" (dimid 0): a "pure" dimension, no coordinate variable.
 *  - dim "y" (dimid 1): has a coordinate variable (var "y", varid 0).
 *  - var "v1" (varid 1): uses both dims (x, y).
 *  - var "v2" (varid 2): uses only the coordinate-variable dim (y).
 *  - global attributes of mixed nc_type (NC_INT, NC_FLOAT, NC_CHAR).
 *  - a variable attribute (on v1) of nc_type NC_INT.
 * Variable ids are assigned in def_var() call order: y, then v1, then v2 -
 * this is relied on below to assert the "same index order" requirement.
 */
static int make_scorpio_written_hdf5_file_full(MPI_Comm comm, const char *fname)
{
  int ret;
  int iosysid = -1;
  int ncid = -1;
  int x_dimid = -1, y_dimid = -1;
  int y_varid = -1, v1_varid = -1, v2_varid = -1;
  int format = PIO_IOTYPE_HDF5;

  if((ret = PIOc_Init_Intracomm(comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid))){
    return ret;
  }

  if((ret = PIOc_createfile(iosysid, &ncid, &format, fname, PIO_CLOBBER))){
    PIOc_finalize(iosysid);
    return ret;
  }

  if((ret = PIOc_def_dim(ncid, "x", (PIO_Offset) 4, &x_dimid)) ||
     (ret = PIOc_def_dim(ncid, "y", (PIO_Offset) 3, &y_dimid))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  int y_dimids[1] = { y_dimid };
  if((ret = PIOc_def_var(ncid, "y", PIO_INT, 1, y_dimids, &y_varid))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  int v1_dimids[2] = { x_dimid, y_dimid };
  if((ret = PIOc_def_var(ncid, "v1", PIO_FLOAT, 2, v1_dimids, &v1_varid))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  int v2_dimids[1] = { y_dimid };
  if((ret = PIOc_def_var(ncid, "v2", PIO_INT, 1, v2_dimids, &v2_varid))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  int g_int_val = 42;
  float g_float_val = 1.5f;
  const char *g_text_val = "hello";
  if((ret = PIOc_put_att(ncid, PIO_GLOBAL, "g_int", PIO_INT, 1, &g_int_val)) ||
     (ret = PIOc_put_att(ncid, PIO_GLOBAL, "g_float", PIO_FLOAT, 1, &g_float_val)) ||
     (ret = PIOc_put_att_text(ncid, PIO_GLOBAL, "g_text", strlen(g_text_val), g_text_val))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  int v1_att_val = 7;
  if((ret = PIOc_put_att(ncid, v1_varid, "v1_int", PIO_INT, 1, &v1_att_val))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  if((ret = PIOc_enddef(ncid))){
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return ret;
  }

  if((ret = PIOc_closefile(ncid))){
    PIOc_finalize(iosysid);
    return ret;
  }

  PIOc_finalize(iosysid);
  return PIO_NOERR;
}

/* Step 1/4: the full round-trip - spio_hdf5_reopen() on a file written via
 * make_scorpio_written_hdf5_file_full() must reconstruct file->hdf5_dims/
 * hdf5_vars/hdf5_attrs (and their counts) to match exactly what the original
 * def_dim()/def_var()/put_att() calls produced, in the same index order, with
 * no reserved/internal attribute names leaking into hdf5_attrs[].
 */
static int test_reopen_introspection_round_trip(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  const char *fname = "test_hdf5_reopen_round_trip.nc";

  if((ret = make_scorpio_written_hdf5_file_full(comm, fname))){
    LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: could not create the "
      "fixture file (ret = %d)\n", ret);
    return PIO_EINTERNAL;
  }

  int iosysid = -1;
  if((ret = PIOc_Init_Intracomm(comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid))){
    LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: PIOc_Init_Intracomm() "
      "returned %d\n", ret);
    remove(fname);
    return PIO_EINTERNAL;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios){
    LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    PIOc_finalize(iosysid);
    remove(fname);
    return PIO_EINTERNAL;
  }

  int nerrs = 0;

  file_desc_t fake_file{};
  fake_file.iotype = PIO_IOTYPE_HDF5;
  strncpy(fake_file.fname, fname, PIO_MAX_NAME);
  fake_file.fname[PIO_MAX_NAME] = '\0';

  int reopen_ret = spio_hdf5_reopen(ios, &fake_file, fname, PIO_WRITE);
  if(reopen_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: spio_hdf5_reopen() "
      "returned %d, expected PIO_NOERR\n", reopen_ret);
    nerrs++;
  }
  else{
    /* Dimensions: dimid 0 = "x" (pure), dimid 1 = "y" (has coord var) */
    if(fake_file.hdf5_num_dims != 2){
      LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: hdf5_num_dims = %d, "
        "expected 2\n", fake_file.hdf5_num_dims);
      nerrs++;
    }
    else{
      if(!fake_file.hdf5_dims[0].name || strcmp(fake_file.hdf5_dims[0].name, "x") != 0 ||
         (fake_file.hdf5_dims[0].len != 4) || fake_file.hdf5_dims[0].has_coord_var){
        LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: hdf5_dims[0] "
          "(name=%s, len=%lld, has_coord_var=%d) does not match expected (x, 4, false)\n",
          (fake_file.hdf5_dims[0].name) ? fake_file.hdf5_dims[0].name : "(null)",
          (long long) fake_file.hdf5_dims[0].len, (int) fake_file.hdf5_dims[0].has_coord_var);
        nerrs++;
      }

      if(!fake_file.hdf5_dims[1].name || strcmp(fake_file.hdf5_dims[1].name, "y") != 0 ||
         (fake_file.hdf5_dims[1].len != 3) || !fake_file.hdf5_dims[1].has_coord_var){
        LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: hdf5_dims[1] "
          "(name=%s, len=%lld, has_coord_var=%d) does not match expected (y, 3, true)\n",
          (fake_file.hdf5_dims[1].name) ? fake_file.hdf5_dims[1].name : "(null)",
          (long long) fake_file.hdf5_dims[1].len, (int) fake_file.hdf5_dims[1].has_coord_var);
        nerrs++;
      }
    }

    /* Variables: varid 0 = "y" (coord var, ndims=1, dimids={1}),
     * varid 1 = "v1" (ndims=2, dimids={0,1}), varid 2 = "v2" (ndims=1, dimids={1})
     */
    if(fake_file.hdf5_num_vars != 3){
      LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: hdf5_num_vars = %d, "
        "expected 3\n", fake_file.hdf5_num_vars);
      nerrs++;
    }
    else{
      struct{
        const char *name;
        int nc_type;
        int ndims;
        int dimids[2];
      } expected_vars[3] = {
        { "y",  PIO_INT,   1, { 1, -1 } },
        { "v1", PIO_FLOAT, 2, { 0, 1 } },
        { "v2", PIO_INT,   1, { 1, -1 } }
      };

      for(int i = 0; i < 3; i++){
        hdf5_var_desc_t *v = &(fake_file.hdf5_vars[i]);
        bool var_ok = v->name && (strcmp(v->name, expected_vars[i].name) == 0) &&
                      (v->nc_type == expected_vars[i].nc_type) &&
                      (v->ndims == expected_vars[i].ndims) && v->hdf5_dimids;
        if(var_ok){
          for(int d = 0; d < expected_vars[i].ndims; d++){
            if(v->hdf5_dimids[d] != expected_vars[i].dimids[d]){
              var_ok = false;
            }
          }
        }

        if(!var_ok){
          LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: hdf5_vars[%d] "
            "(name=%s, nc_type=%d, ndims=%d) does not match expected (%s, %d, %d)\n",
            i, (v->name) ? v->name : "(null)", v->nc_type, v->ndims,
            expected_vars[i].name, expected_vars[i].nc_type, expected_vars[i].ndims);
          nerrs++;
        }
      }
    }

    /* Attributes: 3 global (g_int/g_float/g_text) + 1 on var "v1" (v1_int) */
    if((fake_file.hdf5_num_attrs != 4) || (fake_file.hdf5_num_gattrs != 3)){
      LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: hdf5_num_attrs = %d, "
        "hdf5_num_gattrs = %d, expected 4 and 3\n",
        fake_file.hdf5_num_attrs, fake_file.hdf5_num_gattrs);
      nerrs++;
    }
    else{
      struct{
        const char *name;
        int nc_type;
        PIO_Offset len;
        int varid;
        bool found;
      } expected_attrs[4] = {
        { "g_int",   PIO_INT,   1, PIO_GLOBAL, false },
        { "g_float", PIO_FLOAT, 1, PIO_GLOBAL, false },
        { "g_text",  PIO_CHAR,  5, PIO_GLOBAL, false },
        { "v1_int",  PIO_INT,   1, 1,          false }
      };

      for(int i = 0; i < fake_file.hdf5_num_attrs; i++){
        hdf5_att_desc_t *a = &(fake_file.hdf5_attrs[i]);
        if(test_is_reserved_attr_name(a->att_name)){
          LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: reserved "
            "attribute name (%s) leaked into hdf5_attrs[]\n",
            (a->att_name) ? a->att_name : "(null)");
          nerrs++;
          continue;
        }

        bool matched_one = false;
        for(int j = 0; j < 4; j++){
          if(a->att_name && (strcmp(a->att_name, expected_attrs[j].name) == 0)){
            matched_one = true;
            if(expected_attrs[j].found){
              LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: "
                "attribute (%s) appears more than once in hdf5_attrs[]\n", a->att_name);
              nerrs++;
            }
            expected_attrs[j].found = true;

            if((a->att_type != expected_attrs[j].nc_type) ||
               (a->att_len != expected_attrs[j].len) ||
               (a->att_varid != expected_attrs[j].varid)){
              LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: "
                "attribute (%s) (type=%d, len=%lld, varid=%d) does not match "
                "expected (%d, %lld, %d)\n",
                a->att_name, a->att_type, (long long) a->att_len, a->att_varid,
                expected_attrs[j].nc_type, (long long) expected_attrs[j].len,
                expected_attrs[j].varid);
              nerrs++;
            }
            break;
          }
        }

        if(!matched_one){
          LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: "
            "unexpected attribute (%s) in hdf5_attrs[]\n",
            (a->att_name) ? a->att_name : "(null)");
          nerrs++;
        }
      }

      for(int j = 0; j < 4; j++){
        if(!expected_attrs[j].found){
          LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED: "
            "expected attribute (%s) not found in hdf5_attrs[]\n", expected_attrs[j].name);
          nerrs++;
        }
      }
    }
  }

  test_free_hdf5_reopen_allocs(&fake_file);
  PIOc_finalize(iosysid);
  remove(fname);

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Step 5/6: reopen a write-capable file natively, then immediately reopen the
 * *same* file a second time with PIO_NOWRITE on a second, independent
 * file_desc_t - the second open must succeed and must not see any state
 * left over from the first (no accidental shared/static state in the new
 * reopen code).
 */
static int test_reopen_then_readonly_reopen(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  const char *fname = "test_hdf5_reopen_then_readonly.nc";

  if((ret = make_scorpio_written_hdf5_file_full(comm, fname))){
    LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED: could not create the "
      "fixture file (ret = %d)\n", ret);
    return PIO_EINTERNAL;
  }

  int iosysid = -1;
  if((ret = PIOc_Init_Intracomm(comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid))){
    LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED: PIOc_Init_Intracomm() "
      "returned %d\n", ret);
    remove(fname);
    return PIO_EINTERNAL;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios){
    LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    PIOc_finalize(iosysid);
    remove(fname);
    return PIO_EINTERNAL;
  }

  int nerrs = 0;

  /* file_desc_t embeds fixed-size hdf5_dims[PIO_MAX_DIMS]/hdf5_vars[PIO_MAX_VARS]/
   * hdf5_attrs[PIO_MAX_ATTRS] arrays directly (not as dynamically-allocated
   * arrays), so a single instance is already large; two of them as plain
   * stack locals in the same frame overflow the (ASan-instrumented) default
   * stack. Heap-allocate both instead.
   */
  file_desc_t *write_file = new file_desc_t();
  write_file->iotype = PIO_IOTYPE_HDF5;
  strncpy(write_file->fname, fname, PIO_MAX_NAME);
  write_file->fname[PIO_MAX_NAME] = '\0';

  int write_reopen_ret = spio_hdf5_reopen(ios, write_file, fname, PIO_WRITE);
  if(write_reopen_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED: the first (write-capable) "
      "spio_hdf5_reopen() returned %d, expected PIO_NOERR\n", write_reopen_ret);
    nerrs++;
  }

  file_desc_t *readonly_file = new file_desc_t();
  readonly_file->iotype = PIO_IOTYPE_HDF5;
  strncpy(readonly_file->fname, fname, PIO_MAX_NAME);
  readonly_file->fname[PIO_MAX_NAME] = '\0';

  int readonly_reopen_ret = spio_hdf5_reopen(ios, readonly_file, fname, PIO_NOWRITE);
  if(readonly_reopen_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED: the second (read-only) "
      "spio_hdf5_reopen() returned %d, expected PIO_NOERR\n", readonly_reopen_ret);
    nerrs++;
  }
  else{
    if((readonly_file->hdf5_num_dims != write_file->hdf5_num_dims) ||
       (readonly_file->hdf5_num_vars != write_file->hdf5_num_vars) ||
       (readonly_file->hdf5_num_attrs != write_file->hdf5_num_attrs) ||
       (readonly_file->hdf5_num_gattrs != write_file->hdf5_num_gattrs)){
      LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED: the second file_desc_t's "
        "counts (dims=%d, vars=%d, attrs=%d, gattrs=%d) do not match the first's "
        "(dims=%d, vars=%d, attrs=%d, gattrs=%d) - independent opens of the same file "
        "should see the same schema exactly once each, not a doubled/corrupted one\n",
        readonly_file->hdf5_num_dims, readonly_file->hdf5_num_vars,
        readonly_file->hdf5_num_attrs, readonly_file->hdf5_num_gattrs,
        write_file->hdf5_num_dims, write_file->hdf5_num_vars,
        write_file->hdf5_num_attrs, write_file->hdf5_num_gattrs);
      nerrs++;
    }

    if(readonly_file->hdf5_file_id == write_file->hdf5_file_id){
      LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED: the two independent "
        "spio_hdf5_reopen() calls ended up with the same hdf5_file_id - each open should "
        "have its own H5Fopen() handle\n");
      nerrs++;
    }
  }

  test_free_hdf5_reopen_allocs(write_file);
  test_free_hdf5_reopen_allocs(readonly_file);
  delete write_file;
  delete readonly_file;

  PIOc_finalize(iosysid);
  remove(fname);

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Step 7/8: a non-SCORPIO HDF5 file opened read-write (PIO_WRITE, not just
 * PIO_NOWRITE as in test_reopen_fallback_sentinel()) still falls back
 * correctly (PIO_EBADIOTYPE) and does not populate/corrupt file->hdf5_*
 * fields with partial introspection state.
 */
static int test_reopen_fallback_write_mode(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  const char *plain_fname = "test_hdf5_reopen_fallback_write_mode.nc";

  if((ret = make_plain_hdf5_file(plain_fname))){
    LOG_RANK0(wrank, "test_reopen_fallback_write_mode() FAILED: could not create the plain "
      "HDF5 fixture file (ret = %d)\n", ret);
    return PIO_EINTERNAL;
  }

  int iosysid = -1;
  if((ret = PIOc_Init_Intracomm(comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid))){
    LOG_RANK0(wrank, "test_reopen_fallback_write_mode() FAILED: PIOc_Init_Intracomm() "
      "returned %d\n", ret);
    remove(plain_fname);
    return PIO_EINTERNAL;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios){
    LOG_RANK0(wrank, "test_reopen_fallback_write_mode() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    PIOc_finalize(iosysid);
    remove(plain_fname);
    return PIO_EINTERNAL;
  }

  file_desc_t fake_file{};
  fake_file.iotype = PIO_IOTYPE_HDF5;
  strncpy(fake_file.fname, plain_fname, PIO_MAX_NAME);
  fake_file.fname[PIO_MAX_NAME] = '\0';

  /* fake_file.hdf5_file_id is whatever file_desc_t{}'s value-initialization
   * zeroed it to (not necessarily H5I_INVALID_HID, which is -1) - capture
   * that as the "untouched" baseline rather than assuming a specific sentinel.
   */
  hid_t hdf5_file_id_before = fake_file.hdf5_file_id;

  int nerrs = 0;

  int reopen_ret = spio_hdf5_reopen(ios, &fake_file, plain_fname, PIO_WRITE);
  if(reopen_ret != PIO_EBADIOTYPE){
    LOG_RANK0(wrank, "test_reopen_fallback_write_mode() FAILED: spio_hdf5_reopen() "
      "returned %d, expected PIO_EBADIOTYPE (%d), for a file it should not recognize, "
      "opened with PIO_WRITE\n", reopen_ret, PIO_EBADIOTYPE);
    nerrs++;
  }

  if((fake_file.iotype != PIO_IOTYPE_HDF5) || (fake_file.hdf5_num_dims != 0) ||
     (fake_file.hdf5_num_vars != 0) || (fake_file.hdf5_num_attrs != 0) ||
     (fake_file.hdf5_num_gattrs != 0) || (fake_file.hdf5_file_id != hdf5_file_id_before)){
    LOG_RANK0(wrank, "test_reopen_fallback_write_mode() FAILED: the fallback path left "
      "file->iotype/hdf5_* fields in an unexpected, possibly-corrupted state "
      "(iotype=%d, hdf5_num_dims=%d, hdf5_num_vars=%d, hdf5_num_attrs=%d, "
      "hdf5_num_gattrs=%d, hdf5_file_id=%lld, expected hdf5_file_id=%lld)\n",
      fake_file.iotype, fake_file.hdf5_num_dims, fake_file.hdf5_num_vars,
      fake_file.hdf5_num_attrs, fake_file.hdf5_num_gattrs,
      (long long) fake_file.hdf5_file_id, (long long) hdf5_file_id_before);
    nerrs++;
  }

  test_free_hdf5_reopen_allocs(&fake_file);

  PIOc_finalize(iosysid);
  remove(plain_fname);

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);

  ret = test_is_scorpio_written(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_is_scorpio_written() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_is_scorpio_written() PASSED\n");
  }

  ret = test_reopen_fallback_sentinel(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_reopen_fallback_sentinel() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_reopen_fallback_sentinel() PASSED\n");
  }

  ret = test_reopen_introspection_round_trip(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_reopen_introspection_round_trip() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_reopen_introspection_round_trip() PASSED\n");
  }

  ret = test_reopen_then_readonly_reopen(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_reopen_then_readonly_reopen() PASSED\n");
  }

  ret = test_reopen_fallback_write_mode(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_reopen_fallback_write_mode() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_reopen_fallback_write_mode() PASSED\n");
  }

  *num_errors += nerrs;
  return nerrs;
}

int main(int argc, char *argv[])
{
  int ret;
  int wrank, wsz;
  int num_errors;
#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLinitialize();
  if(ret != 0){
    fprintf(stderr, "GPTLinitialize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  ret = MPI_Init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Init() FAILED, ret = %d\n", ret);
    return ret;
  }

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
    return ret;
  }
  ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Comm_size() FAILED, ret = %d\n", ret);
    return ret;
  }

  num_errors = 0;
  ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
  if(ret != 0){
    LOG_RANK0(wrank, "Test driver FAILED\n");
  }
  else{
    LOG_RANK0(wrank, "All tests PASSED\n");
  }

  MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLfinalize();
  if(ret != 0){
    fprintf(stderr, "GPTLfinalize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  if(num_errors != 0){
    LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
    return FAIL;
  }
  return 0;
}

#endif /* _HDF5 */
