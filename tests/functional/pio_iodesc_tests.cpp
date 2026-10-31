#include <iostream>
#include <string>
#include <vector>
#include <functional>
#include <utility>

#include "pio.h"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#include "spio_test_framework.hpp"
#include "spio_test_check.hpp"
#include "spio_test_common.hpp"

/* Decomp shifted left "shift" positions, each rank contains VEC_LOCAL_SZ
 * elements. The shift is a global "cyclic left shift"
 */
static void get_1d_bc_shift_decomp_info(int rank, int sz, int dims[1],
    std::vector<int> &start, std::vector<int> &count, int nlshift)
{
  const int VEC_LOCAL_SZ = 7;
  dims[0] = VEC_LOCAL_SZ * sz;
  int start_idx = ((VEC_LOCAL_SZ * rank + 1) + nlshift) % dims[0];
  int last_idx = start_idx + VEC_LOCAL_SZ - 1;
  if(last_idx <= dims[0]){
    start = {start_idx};
    count = {VEC_LOCAL_SZ};
  }
  else{
    start = {start_idx, 1};
    count = {dims[0] - start_idx + 1, last_idx - dims[0]};
  }
}

/* Get a block cyclic decomposition with multiple regions to write
 * from a single proc
 */
static void get_1d_bc_mureg_info(int rank, int sz, int dims[1],
    std::vector<int> &start, std::vector<int> &count)
{
  const int VEC_LOCAL_SZ = 7;
  const int NUM_REGIONS = 3;
  dims[0] = sz * VEC_LOCAL_SZ * NUM_REGIONS;
  start.resize(NUM_REGIONS);
  count.resize(NUM_REGIONS);
  for(int i = 0; i < NUM_REGIONS; i++){
    count[i] = VEC_LOCAL_SZ;
    start[i] = rank * VEC_LOCAL_SZ + i * (sz * VEC_LOCAL_SZ) + 1;
  }
}

/* Get a block cyclic decomposition with multiple regions to write
 * from a single proc, the number of regions written out by odd
 * and even procs are different (not uniform).
 */
static void get_1d_bc_mnureg_info(int rank, int sz, int dims[1],
    std::vector<int> &start, std::vector<int> &count)
{
  const int VEC_LOCAL_SZ = 7;
  const int NUM_REGIONS_ODD = 3;
  const int NUM_REGIONS_EVEN = 2;
  bool is_even = ((rank % 2) == 0);
  int num_odd_procs = sz / 2;
  int num_even_procs = sz - num_odd_procs;
  int num_odd_procs_bfr_rank = rank / 2;
  int num_even_procs_bfr_rank = (rank + 1) / 2;
  int num_regions = is_even ? NUM_REGIONS_EVEN : NUM_REGIONS_ODD;
  dims[0] = num_odd_procs * VEC_LOCAL_SZ * NUM_REGIONS_ODD +
            num_even_procs * VEC_LOCAL_SZ * NUM_REGIONS_EVEN;
  start.resize(num_regions);
  count.resize(num_regions);
  /* The number of values already written out globally */
  int gcount = 0;
  for(int i = 0; i < num_regions; i++){
    count[i] = VEC_LOCAL_SZ;
    start[i] = gcount + 1;
    if(i < NUM_REGIONS_ODD){
      start[i] += num_odd_procs_bfr_rank * VEC_LOCAL_SZ;
      gcount += num_odd_procs * VEC_LOCAL_SZ;
    }
    if(i < NUM_REGIONS_EVEN){
      start[i] += num_even_procs_bfr_rank * VEC_LOCAL_SZ;
      gcount += num_even_procs * VEC_LOCAL_SZ;
    }
  }
}

/* Initialize a 1D decomposition and check the local array size */
template<typename T>
int init_decomp_1d_get_loc_sz(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;

  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  int dims[1] = { VEC_LOCAL_SZ * tf.get_comm_size() };

  int ioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int lsz = PIOc_get_local_array_size(ioid);
  if(!passert(tf, lsz == VEC_LOCAL_SZ, "Checking the local array size")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write multiple frames of data, even frames with no shift - evenly
 * divided across all procs, and odd frames with data evenly divided
 * across all procs but with a global cyclic left shift
 */
template<typename T>
int nc_wr_2d_two_iodescs(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int NFRAMES = 6;
  const int NLSHIFT = 3;
  const int NOSHIFT = 0;
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  const std::string filename("test_pio_decomp_simple_tests.testfile");

  /* No left shift - data evenly distributed across all procs,
   * even frames written out with no left shift
   */
  int dims[1];
  std::vector<int> start, count;
  get_1d_bc_shift_decomp_info(tf.get_comm_rank(), tf.get_comm_size(), dims, start, count, NOSHIFT);

  int nrows = 0;
  for(std::size_t i = 0; i < count.size(); i++){
    nrows += count[i];
  }

  std::vector<PIO_Offset> compdof(nrows);
  std::size_t k = 0;
  for(std::size_t i = 0; i < start.size(); i++){
    for(int j = 0; j < count[i]; j++){
      compdof[k++] = start[i] + j;
    }
  }

  std::vector<std::vector<T> > wbuf(NFRAMES, std::vector<T>(nrows));
  std::vector<std::vector<T> > rbuf(NFRAMES, std::vector<T>(nrows, static_cast<T>(0)));
  std::vector<std::vector<T> > exp_val(NFRAMES, std::vector<T>(nrows));
  for(int cf = 0; cf < NFRAMES; cf++){
    for(int i = 0; i < nrows; i++){
      wbuf[cf][i] = static_cast<T>(static_cast<int>(compdof[i]) + cf * dims[0]);
      exp_val[cf][i] = wbuf[cf][i];
    }
  }

  int wioid = -1, rioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          nrows, compdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          nrows, compdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  /* Decomp with left shift of NLSHIFT - used to write odd frames.
   * Note: dims is the same irrespective of the shift
   */
  std::vector<int> start2, count2;
  get_1d_bc_shift_decomp_info(tf.get_comm_rank(), tf.get_comm_size(), dims, start2, count2, NLSHIFT);

  int nrows2 = 0;
  for(std::size_t i = 0; i < count2.size(); i++){
    nrows2 += count2[i];
  }

  std::vector<PIO_Offset> compdof_shifted(nrows2);
  k = 0;
  for(std::size_t i = 0; i < start2.size(); i++){
    for(int j = 0; j < count2[i]; j++){
      compdof_shifted[k++] = start2[i] + j;
    }
  }

  std::vector<std::vector<T> > wbuf_shifted(NFRAMES, std::vector<T>(nrows2));
  for(int cf = 0; cf < NFRAMES; cf++){
    for(int i = 0; i < nrows2; i++){
      wbuf_shifted[cf][i] = static_cast<T>(static_cast<int>(compdof_shifted[i]) + cf * dims[0]);
    }
  }

  int wioid_shifted = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          nrows2, compdof_shifted.data(), &wioid_shifted, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, row_dimid = -1, time_dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_2d_two_iodescs : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_row", dims[0], &row_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_time", PIO_UNLIMITED, &time_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    int dimids[2] = { time_dimid, row_dimid };
    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 2, dimids, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    for(int cf = 0; cf < NFRAMES; cf++){
      /* Even frames (1-based) have no shift, odd frames are left
       * shifted by NLSHIFT elements. cf is 0-based so the 1-based
       * Fortran frame number is (cf + 1).
       */
      bool is_even_frame = (((cf + 1) % 2) == 0);
      ret = PIOc_setframe(ncid, varid, cf);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      if(is_even_frame){
        ret = PIOc_write_darray(ncid, varid, wioid, nrows, wbuf[cf].data(), NULL);
      }
      else{
        ret = PIOc_write_darray(ncid, varid, wioid_shifted, nrows2, wbuf_shifted[cf].data(), NULL);
      }
      if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
        return PIO_EINTERNAL;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    /* Read data - with no shift */
    for(int cf = 0; cf < NFRAMES; cf++){
      for(int j = 0; j < nrows; j++){
        rbuf[cf][j] = static_cast<T>(0);
      }
    }

    for(int cf = 0; cf < NFRAMES; cf++){
      ret = PIOc_setframe(ncid, varid, cf);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_read_darray(ncid, varid, rioid, nrows, rbuf[cf].data());
      if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
        return PIO_EINTERNAL;
      }
    }

    for(int cf = 0; cf < NFRAMES; cf++){
      if(!check_val(tf, rbuf[cf], exp_val[cf], "Got wrong val, frame=" + std::to_string(cf))){
        return PIO_EINTERNAL;
      }
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), rioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wioid_shifted);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write two variables with multiple contiguous regions of data.
 * The regions are uniform (same number of regions) across all procs
 */
template<typename T>
int nc_mureg_wr_rd_1d_bc(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const std::string PIO_VAR1_NAME("PIO_TF_test_var1_mureg");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2_mureg");
  const std::string filename("test_pio_decomp_simple_tests.testfile");

  int dims_mureg[1];
  std::vector<int> start, count;
  get_1d_bc_mureg_info(tf.get_comm_rank(), tf.get_comm_size(), dims_mureg, start, count);

  int total_count = 0;
  for(std::size_t i = 0; i < count.size(); i++){
    total_count += count[i];
  }

  std::vector<T> wbuf_mureg(total_count), rbuf_mureg(total_count);
  std::vector<PIO_Offset> compdof(total_count);
  for(std::size_t i = 0; i < count.size(); i++){
    for(int j = 0; j < count[i]; j++){
      /* Valid only because every region has the same count (VEC_LOCAL_SZ) */
      std::size_t idx = i * count[i] + j;
      compdof[idx] = start[i] + j;
      wbuf_mureg[idx] = static_cast<T>(compdof[idx]);
      rbuf_mureg[idx] = static_cast<T>(compdof[idx]);
    }
  }

  int iodesc_mureg = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims_mureg,
          total_count, compdof.data(), &iodesc_mureg, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, pio_dim_mureg = -1, varid1 = -1, varid2 = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_mureg_wr_rd_1d_bc : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_mureg", dims_mureg[0], &pio_dim_mureg);
    if(!check_ret(tf, ret, "Failed to define a dim (mureg) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR1_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim_mureg, &varid1);
    if(!check_ret(tf, ret, "Failed to define a var (var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR2_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim_mureg, &varid2);
    if(!check_ret(tf, ret, "Failed to define a var (var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variables out */
    ret = PIOc_write_darray(ncid, varid1, iodesc_mureg, total_count, wbuf_mureg.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray (var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, iodesc_mureg, total_count, wbuf_mureg.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray (var2) : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR1_NAME.c_str(), &varid1);
    if(!check_ret(tf, ret, "Could not inq var1 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR2_NAME.c_str(), &varid2);
    if(!check_ret(tf, ret, "Could not inq var2 : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf_mureg.size(); j++){
      rbuf_mureg[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid1, iodesc_mureg, total_count, rbuf_mureg.data());
    if(!check_ret(tf, ret, "Failed to read darray (var1): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf_mureg, wbuf_mureg, "Got wrong val (var1)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf_mureg.size(); j++){
      rbuf_mureg[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid2, iodesc_mureg, total_count, rbuf_mureg.data());
    if(!check_ret(tf, ret, "Failed to read darray (var2): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf_mureg, wbuf_mureg, "Got wrong val (var2)")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), iodesc_mureg);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write two variables with multiple contiguous regions of data.
 * The regions are non-uniform (odd and even procs write out a
 * different number of regions)
 */
template<typename T>
int nc_mnureg_wr_rd_1d_bc(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const std::string PIO_VAR1_NAME("PIO_TF_test_var1_mnureg");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2_mnureg");
  const std::string filename("test_pio_decomp_simple_tests.testfile");

  int dims_mnureg[1];
  std::vector<int> start, count;
  get_1d_bc_mnureg_info(tf.get_comm_rank(), tf.get_comm_size(), dims_mnureg, start, count);

  int total_count = 0;
  for(std::size_t i = 0; i < count.size(); i++){
    total_count += count[i];
  }

  std::vector<T> wbuf_mnureg(total_count), rbuf_mnureg(total_count);
  std::vector<PIO_Offset> compdof(total_count);
  for(std::size_t i = 0; i < count.size(); i++){
    for(int j = 0; j < count[i]; j++){
      /* Valid only because every region has the same count (VEC_LOCAL_SZ) */
      std::size_t idx = i * count[i] + j;
      compdof[idx] = start[i] + j;
      wbuf_mnureg[idx] = static_cast<T>(compdof[idx]);
      rbuf_mnureg[idx] = static_cast<T>(compdof[idx]);
    }
  }

  int iodesc_mnureg = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims_mnureg,
          total_count, compdof.data(), &iodesc_mnureg, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, pio_dim_mnureg = -1, varid1 = -1, varid2 = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_mnureg_wr_rd_1d_bc : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_mnureg", dims_mnureg[0], &pio_dim_mnureg);
    if(!check_ret(tf, ret, "Failed to define a dim (mnureg) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR1_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim_mnureg, &varid1);
    if(!check_ret(tf, ret, "Failed to define a var (var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR2_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim_mnureg, &varid2);
    if(!check_ret(tf, ret, "Failed to define a var (var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variables out */
    ret = PIOc_write_darray(ncid, varid1, iodesc_mnureg, total_count, wbuf_mnureg.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray (var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, iodesc_mnureg, total_count, wbuf_mnureg.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray (var2) : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR1_NAME.c_str(), &varid1);
    if(!check_ret(tf, ret, "Could not inq var1 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR2_NAME.c_str(), &varid2);
    if(!check_ret(tf, ret, "Could not inq var2 : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf_mnureg.size(); j++){
      rbuf_mnureg[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid1, iodesc_mnureg, total_count, rbuf_mnureg.data());
    if(!check_ret(tf, ret, "Failed to read darray (var1): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf_mnureg, wbuf_mnureg, "Got wrong val (var1)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf_mnureg.size(); j++){
      rbuf_mnureg[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid2, iodesc_mnureg, total_count, rbuf_mnureg.data());
    if(!check_ret(tf, ret, "Failed to read darray (var2): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf_mnureg, wbuf_mnureg, "Got wrong val (var2)")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), iodesc_mnureg);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

int main(int argc, char *argv[])
{
  int ret = MPI_SUCCESS, nerrs = 0;
  ret = MPI_Init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    std::cerr << "MPI_Init() FAILED, ret = " << ret << "\n";
    return ret;
  }

  std::vector<std::pair<std::string, std::function<int(SPIO_TF::Test_framework &)> > > rfuncs = {
    {"init_decomp_1d_get_loc_sz_int", init_decomp_1d_get_loc_sz<int>},
    {"init_decomp_1d_get_loc_sz_float", init_decomp_1d_get_loc_sz<float>},
    {"init_decomp_1d_get_loc_sz_double", init_decomp_1d_get_loc_sz<double>},
    {"nc_wr_2d_two_iodescs_int", nc_wr_2d_two_iodescs<int>},
    {"nc_wr_2d_two_iodescs_float", nc_wr_2d_two_iodescs<float>},
    {"nc_wr_2d_two_iodescs_double", nc_wr_2d_two_iodescs<double>},
    {"nc_mureg_wr_rd_1d_bc_int", nc_mureg_wr_rd_1d_bc<int>},
    {"nc_mureg_wr_rd_1d_bc_float", nc_mureg_wr_rd_1d_bc<float>},
    {"nc_mureg_wr_rd_1d_bc_double", nc_mureg_wr_rd_1d_bc<double>},
    {"nc_mnureg_wr_rd_1d_bc_int", nc_mnureg_wr_rd_1d_bc<int>},
    {"nc_mnureg_wr_rd_1d_bc_float", nc_mnureg_wr_rd_1d_bc<float>},
    {"nc_mnureg_wr_rd_1d_bc_double", nc_mnureg_wr_rd_1d_bc<double>}
  };

  SPIO_TF::Test_framework tf("pio_iodesc_tests_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  int rearrs[] = {PIO_REARR_BOX, PIO_REARR_SUBSET, PIO_REARR_CONTIG};
  for(int rearr : rearrs){
    if(tf.init_iosystem(rearr) != PIO_NOERR){
      nerrs++;
      continue;
    }
    nerrs += tf.run();
    tf.finalize_iosystem();
  }

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
