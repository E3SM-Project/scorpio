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
