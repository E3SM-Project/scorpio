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

/* Write/read multiple frames of a 1d variable with an unlimited dimension.
 * This test does not use a decomposition (no PIOc_InitDecomp) - each frame
 * is a single, non-distributed value written/read with PIOc_put_vara/
 * PIOc_get_vara (start/count based hyperslab I/O).
 */
template<typename T>
int nc_wr_rd_1d_unlim_dim(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int NFRAMES = 6;
  const std::string PIO_VAR_NAME("PIO_TF_test_var_1d_unlim");
  const std::string filename("test_pio_decomp_frame_tests.testfile");

  std::vector<T> exp_val(NFRAMES);
  for(int f = 0; f < NFRAMES; f++){
    exp_val[f] = static_cast<T>(f + 1);
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int fl = 0; fl < num_flavors; fl++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[fl], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_rd_1d_unlim_dim : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_time", PIO_UNLIMITED, &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    for(int f = 0; f < NFRAMES; f++){
      PIO_Offset start[1] = { static_cast<PIO_Offset>(f) };
      PIO_Offset count[1] = { 1 };
      T wbuf[1] = { static_cast<T>(f + 1) };

      /* Write the current frame */
      ret = PIOc_put_vara(ncid, varid, start, count, wbuf);
      if(!check_ret(tf, ret, "Failed to write var : " + filename)){
        return PIO_EINTERNAL;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_NOWRITE);
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

    for(int f = 0; f < NFRAMES; f++){
      PIO_Offset start[1] = { static_cast<PIO_Offset>(f) };
      PIO_Offset count[1] = { 1 };
      std::vector<T> rbuf(1, static_cast<T>(0));

      ret = PIOc_get_vara(ncid, varid, start, count, rbuf.data());
      if(!check_ret(tf, ret, "Failed to read var : " + filename)){
        return PIO_EINTERNAL;
      }

      std::vector<T> frame_exp_val(1, exp_val[f]);
      if(!check_val(tf, rbuf, frame_exp_val, "Got wrong val, frame = " + std::to_string(f))){
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

  return PIO_NOERR;
}

/* Write/read multiple frames of a 2d variable (row dim + unlimited/time dim)
 * using two decompositions (wr_iodesc, rd_iodesc), both built from the
 * identical compdof/dims, over the row dimension, with PIOc_setframe
 * selecting the frame before each PIOc_write_darray/PIOc_read_darray call.
 */
template<typename T>
int nc_wr_rd_2d_unlim_dim(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int NFRAMES = 6;
  const int VEC_LOCAL_SZ = 7;
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  const std::string filename("test_pio_decomp_simple_tests.testfile");

  /* Data evenly distributed across all procs - each proc has VEC_LOCAL_SZ
   * elements on each frame
   */
  const std::size_t vec_local_sz = VEC_LOCAL_SZ;
  int dims = VEC_LOCAL_SZ * tf.get_comm_size();
  PIO_Offset start = static_cast<PIO_Offset>(VEC_LOCAL_SZ) * tf.get_comm_rank() + 1;

  std::vector<PIO_Offset> compdof(vec_local_sz);
  for(std::size_t i = 0; i < vec_local_sz; i++){
    compdof[i] = start - 1 + static_cast<PIO_Offset>(i + 1);
  }

  int gdims[1] = { dims };
  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, gdims,
          static_cast<int>(vec_local_sz), compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (wr_iodesc)")){
    return PIO_EINTERNAL;
  }

  int rd_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, gdims,
          static_cast<int>(vec_local_sz), compdof.data(), &rd_iodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (rd_iodesc)")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int fl = 0; fl < num_flavors; fl++){
    int ncid = -1, row_dimid = -1, time_dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[fl], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_rd_2d_unlim_dim : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_row", dims, &row_dimid);
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

    for(int f = 0; f < NFRAMES; f++){
      std::vector<T> wbuf(vec_local_sz);
      for(std::size_t i = 0; i < vec_local_sz; i++){
        wbuf[i] = static_cast<T>(compdof[i] + f * dims);
      }

      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      /* Write the current frame */
      ret = PIOc_write_darray(ncid, varid, wr_iodesc, static_cast<int>(wbuf.size()), wbuf.data(), NULL);
      if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
        return PIO_EINTERNAL;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_NOWRITE);
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

    for(int f = 0; f < NFRAMES; f++){
      std::vector<T> exp_val(vec_local_sz);
      for(std::size_t i = 0; i < vec_local_sz; i++){
        exp_val[i] = static_cast<T>(compdof[i] + f * dims);
      }

      std::vector<T> rbuf(vec_local_sz, static_cast<T>(0));
      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_read_darray(ncid, varid, rd_iodesc, static_cast<int>(rbuf.size()), rbuf.data());
      if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
        return PIO_EINTERNAL;
      }

      if(!check_val(tf, rbuf, exp_val, "Got wrong val, frame = " + std::to_string(f))){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), rd_iodesc);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED (rd_iodesc)")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED (wr_iodesc)")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Get a 3D column decomposition.
 * If force_rearrange is false, the decomposition is such that:
 * - All even ranks have VEC_HGT_SZ blocks of (VEC_COL_SZ rows x VEC_ROW_SZ columns) elements
 * - All odd ranks have VEC_HGT_SZ blocks of (VEC_COL_SZ rows x VEC_ROW_SZ + 1 columns) elements
 * If force_rearrange is true, the even rank "exchanges" elements with the next
 * higher ranked odd rank (if one exists), forcing a different global layout -
 * used to force the library's internal rearranger to do real work when writing.
 * dims/start/count use 1-based offsets/counts, matching the Fortran reference
 * and the 1-based PIO_Offset map convention used throughout SCORPIO (confirmed
 * via pio_rearrange.cpp's idx_to_dim_list: map[0] - 1).
 * dims[0] = row count, dims[1] = col count, dims[2] = height count - this is
 * the "Fortran order" (leftmost fastest varying); callers passing this to
 * PIOc_InitDecomp/PIOc_def_var must reverse it to C order (rightmost fastest
 * varying), exactly as the Fortran PIO_initdecomp wrapper itself does.
 */
static void get_3d_col_decomp_info(int rank, int sz, int dims[3], int start[3], int count[3],
    bool force_rearrange)
{
  const int VEC_ROW_SZ = 2;
  const int VEC_COL_SZ = 2;
  const int VEC_HGT_SZ = 2;

  bool is_even_rank = ((rank % 2) == 0);
  int num_odd_procs = sz / 2;
  int num_even_procs = sz - num_odd_procs;

  dims[0] = VEC_COL_SZ;
  dims[1] = num_even_procs * VEC_ROW_SZ + num_odd_procs * (VEC_ROW_SZ + 1);
  dims[2] = VEC_HGT_SZ;

  /* Number of odd and even ranks before this rank */
  int iodd = rank / 2;
  int ieven = (rank + 1) / 2;

  /* Rows */
  start[0] = 1;
  count[0] = VEC_COL_SZ;

  /* Columns */
  if(force_rearrange){
    if(is_even_rank){
      if(rank + 1 < sz){
        count[1] = VEC_ROW_SZ + 1;
        start[1] = ieven * VEC_ROW_SZ + iodd * (VEC_ROW_SZ + 1) + VEC_ROW_SZ + 1;
      }
      else{
        count[1] = VEC_ROW_SZ;
        start[1] = ieven * VEC_ROW_SZ + iodd * (VEC_ROW_SZ + 1) + 1;
      }
    }
    else{
      /* For all odd ranks there is a lower ranked, rank-1, even rank, so this
       * always forces rearrangement */
      count[1] = VEC_ROW_SZ;
      start[1] = ieven * VEC_ROW_SZ + iodd * (VEC_ROW_SZ + 1) - VEC_ROW_SZ + 1;
    }
  }
  else{
    if(is_even_rank){
      count[1] = VEC_ROW_SZ;
    }
    else{
      count[1] = VEC_ROW_SZ + 1;
    }
    start[1] = ieven * VEC_ROW_SZ + iodd * (VEC_ROW_SZ + 1) + 1;
  }

  /* Height */
  start[2] = 1;
  count[2] = VEC_HGT_SZ;
}

/* Write with one decomp (to force rearrangement) and read with another (no
 * rearrangement) - same 3D column decomposition as pio_decomp_tests_3d.cpp's
 * nc_write_read_3d_col_decomp, extended with a frame/time dimension (selected
 * via PIOc_setframe before each write_darray/read_darray call) - the
 * decomposition itself has no time dimension.
 */
template<typename T>
int nc_write_read_4d_col_decomp(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int NFRAMES = 6;
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  const std::string filename("test_pio_decomp_simple_tests.testfile");

  int dims[3], start[3], count[3];
  int rank = tf.get_comm_rank();
  int sz = tf.get_comm_size();

  /* Set the decomposition for writing data - forcing rearrangement */
  get_3d_col_decomp_info(rank, sz, dims, start, count, true);
  int nrows = count[0];
  int ncols = count[1];
  int nhgts = count[2];
  std::size_t local_sz = static_cast<std::size_t>(nrows) * ncols * nhgts;

  std::vector<std::vector<T> > wbuf(NFRAMES, std::vector<T>(local_sz));
  std::vector<PIO_Offset> compdof(local_sz);
  for(int k = 1; k <= nhgts; k++){
    for(int j = 1; j <= ncols; j++){
      for(int i = 1; i <= nrows; i++){
        PIO_Offset val = static_cast<PIO_Offset>(start[2] - 1 + k - 1) * (dims[0] * dims[1]) +
            static_cast<PIO_Offset>(start[1] - 1 + j - 1) * dims[0] + i;
        std::size_t tmp_idx = static_cast<std::size_t>(k - 1) * (ncols * nrows) +
            static_cast<std::size_t>(j - 1) * nrows + (i - 1);
        compdof[tmp_idx] = val;
        for(int f = 0; f < NFRAMES; f++){
          wbuf[f][tmp_idx] = static_cast<T>(val + f * (dims[0] * dims[1] * dims[2]));
        }
      }
    }
  }

  int wr_gdims[3] = {dims[2], dims[1], dims[0]};
  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 3, wr_gdims,
          static_cast<int>(local_sz), compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (wr_iodesc)")){
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data - different from the write decomp */
  get_3d_col_decomp_info(rank, sz, dims, start, count, false);
  nrows = count[0];
  ncols = count[1];
  nhgts = count[2];
  local_sz = static_cast<std::size_t>(nrows) * ncols * nhgts;

  std::vector<std::vector<T> > rbuf(NFRAMES, std::vector<T>(local_sz, static_cast<T>(0)));
  std::vector<std::vector<T> > exp_val(NFRAMES, std::vector<T>(local_sz));
  compdof.assign(local_sz, 0);
  for(int k = 1; k <= nhgts; k++){
    for(int j = 1; j <= ncols; j++){
      for(int i = 1; i <= nrows; i++){
        PIO_Offset val = static_cast<PIO_Offset>(start[2] - 1 + k - 1) * (dims[0] * dims[1]) +
            static_cast<PIO_Offset>(start[1] - 1 + j - 1) * dims[0] + i;
        std::size_t tmp_idx = static_cast<std::size_t>(k - 1) * (ncols * nrows) +
            static_cast<std::size_t>(j - 1) * nrows + (i - 1);
        compdof[tmp_idx] = val;
        for(int f = 0; f < NFRAMES; f++){
          exp_val[f][tmp_idx] = static_cast<T>(val + f * (dims[0] * dims[1] * dims[2]));
        }
      }
    }
  }

  int rd_gdims[3] = {dims[2], dims[1], dims[0]};
  int rd_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 3, rd_gdims,
          static_cast<int>(local_sz), compdof.data(), &rd_iodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (rd_iodesc)")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int fl = 0; fl < num_flavors; fl++){
    int ncid = -1, row_dimid = -1, col_dimid = -1, hgt_dimid = -1, time_dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[fl], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_write_read_4d_col_decomp : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_row", dims[0], &row_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_col", dims[1], &col_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_hgt", dims[2], &hgt_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_time", PIO_UNLIMITED, &time_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    int dimids[4] = {time_dimid, hgt_dimid, col_dimid, row_dimid};
    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 4, dimids, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    for(int f = 0; f < NFRAMES; f++){
      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      /* Write the current frame */
      ret = PIOc_write_darray(ncid, varid, wr_iodesc, static_cast<int>(wbuf[f].size()), wbuf[f].data(), NULL);
      if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
        return PIO_EINTERNAL;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_NOWRITE);
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

    /* Read the variable back in, using the natural (non-rearranged) decomp */
    for(int f = 0; f < NFRAMES; f++){
      for(std::size_t j = 0; j < rbuf[f].size(); j++){
        rbuf[f][j] = 0;
      }
    }

    for(int f = 0; f < NFRAMES; f++){
      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_read_darray(ncid, varid, rd_iodesc, static_cast<int>(rbuf[f].size()), rbuf[f].data());
      if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
        return PIO_EINTERNAL;
      }

      if(!check_val(tf, rbuf[f], exp_val[f], "Got wrong val, frame = " + std::to_string(f))){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), rd_iodesc);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED (rd_iodesc)")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED (wr_iodesc)")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Using a single 3D column decomposition (two iodescs: one to force
 * rearrangement on write, one for the natural layout on read) to write out
 * both a 3D var (no time dimension) and a 4D var (with a time dimension,
 * selected via PIOc_setframe) - the 3D var and each frame of the 4D var
 * both reuse the same wr_iodesc/rd_iodesc pair.
 */
template<typename T>
int nc_reuse_3d_decomp(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int NFRAMES = 3;
  const std::string PIO_3DVAR_NAME("PIO_TF_test_3d_var");
  const std::string PIO_4DVAR_NAME("PIO_TF_test_4d_var");
  const std::string filename("test_pio_decomp_simple_tests.testfile");

  int dims[3], start[3], count[3];
  int rank = tf.get_comm_rank();
  int sz = tf.get_comm_size();

  /* Set the decomposition for writing data - forcing rearrangement */
  get_3d_col_decomp_info(rank, sz, dims, start, count, true);
  int nrows = count[0];
  int ncols = count[1];
  int nhgts = count[2];
  std::size_t local_sz = static_cast<std::size_t>(nrows) * ncols * nhgts;

  std::vector<std::vector<T> > wbuf4d(NFRAMES, std::vector<T>(local_sz));
  std::vector<T> wbuf3d(local_sz);
  std::vector<PIO_Offset> compdof(local_sz);
  for(int k = 1; k <= nhgts; k++){
    for(int j = 1; j <= ncols; j++){
      for(int i = 1; i <= nrows; i++){
        PIO_Offset val = static_cast<PIO_Offset>(start[2] - 1 + k - 1) * (dims[0] * dims[1]) +
            static_cast<PIO_Offset>(start[1] - 1 + j - 1) * dims[0] + i;
        std::size_t tmp_idx = static_cast<std::size_t>(k - 1) * (ncols * nrows) +
            static_cast<std::size_t>(j - 1) * nrows + (i - 1);
        compdof[tmp_idx] = val;
        wbuf3d[tmp_idx] = static_cast<T>(val);
        for(int f = 0; f < NFRAMES; f++){
          wbuf4d[f][tmp_idx] = static_cast<T>(val + f * (dims[0] * dims[1] * dims[2]));
        }
      }
    }
  }

  int wr_gdims[3] = {dims[2], dims[1], dims[0]};
  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 3, wr_gdims,
          static_cast<int>(local_sz), compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (wr_iodesc)")){
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data - different from the write decomp */
  get_3d_col_decomp_info(rank, sz, dims, start, count, false);
  nrows = count[0];
  ncols = count[1];
  nhgts = count[2];
  local_sz = static_cast<std::size_t>(nrows) * ncols * nhgts;

  std::vector<std::vector<T> > rbuf4d(NFRAMES, std::vector<T>(local_sz, static_cast<T>(0)));
  std::vector<std::vector<T> > exp_val4d(NFRAMES, std::vector<T>(local_sz));
  std::vector<T> rbuf3d(local_sz, static_cast<T>(0));
  std::vector<T> exp_val3d(local_sz);
  compdof.assign(local_sz, 0);
  for(int k = 1; k <= nhgts; k++){
    for(int j = 1; j <= ncols; j++){
      for(int i = 1; i <= nrows; i++){
        PIO_Offset val = static_cast<PIO_Offset>(start[2] - 1 + k - 1) * (dims[0] * dims[1]) +
            static_cast<PIO_Offset>(start[1] - 1 + j - 1) * dims[0] + i;
        std::size_t tmp_idx = static_cast<std::size_t>(k - 1) * (ncols * nrows) +
            static_cast<std::size_t>(j - 1) * nrows + (i - 1);
        compdof[tmp_idx] = val;
        exp_val3d[tmp_idx] = static_cast<T>(val);
        for(int f = 0; f < NFRAMES; f++){
          exp_val4d[f][tmp_idx] = static_cast<T>(val + f * (dims[0] * dims[1] * dims[2]));
        }
      }
    }
  }

  int rd_gdims[3] = {dims[2], dims[1], dims[0]};
  int rd_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 3, rd_gdims,
          static_cast<int>(local_sz), compdof.data(), &rd_iodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (rd_iodesc)")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int fl = 0; fl < num_flavors; fl++){
    int ncid = -1, row_dimid = -1, col_dimid = -1, hgt_dimid = -1, time_dimid = -1;
    int varid3d = -1, varid4d = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[fl], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_reuse_3d_decomp : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_row", dims[0], &row_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_col", dims[1], &col_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_hgt", dims[2], &hgt_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_time", PIO_UNLIMITED, &time_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    int dimids3d[3] = {hgt_dimid, col_dimid, row_dimid};
    ret = PIOc_def_var(ncid, PIO_3DVAR_NAME.c_str(), Pio_type_map<T>::value, 3, dimids3d, &varid3d);
    if(!check_ret(tf, ret, "Failed to define a 3d var : " + filename)){
      return PIO_EINTERNAL;
    }

    int dimids4d[4] = {time_dimid, hgt_dimid, col_dimid, row_dimid};
    ret = PIOc_def_var(ncid, PIO_4DVAR_NAME.c_str(), Pio_type_map<T>::value, 4, dimids4d, &varid4d);
    if(!check_ret(tf, ret, "Failed to define a 4d var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid3d, wr_iodesc, static_cast<int>(wbuf3d.size()), wbuf3d.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write 3d darray : " + filename)){
      return PIO_EINTERNAL;
    }

    for(int f = 0; f < NFRAMES; f++){
      ret = PIOc_setframe(ncid, varid4d, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      /* Write the current frame */
      ret = PIOc_write_darray(ncid, varid4d, wr_iodesc, static_cast<int>(wbuf4d[f].size()), wbuf4d[f].data(), NULL);
      if(!check_ret(tf, ret, "Failed to write 4d darray : " + filename)){
        return PIO_EINTERNAL;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_3DVAR_NAME.c_str(), &varid3d);
    if(!check_ret(tf, ret, "Could not inq 3d var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_4DVAR_NAME.c_str(), &varid4d);
    if(!check_ret(tf, ret, "Could not inq 4d var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    /* Read the variables back in, using the natural (non-rearranged) decomp */
    for(std::size_t j = 0; j < rbuf3d.size(); j++){
      rbuf3d[j] = 0;
    }
    for(int f = 0; f < NFRAMES; f++){
      for(std::size_t j = 0; j < rbuf4d[f].size(); j++){
        rbuf4d[f][j] = 0;
      }
    }

    ret = PIOc_read_darray(ncid, varid3d, rd_iodesc, static_cast<int>(rbuf3d.size()), rbuf3d.data());
    if(!check_ret(tf, ret, "Failed to read 3d darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf3d, exp_val3d, "Got wrong 3d val")){
      return PIO_EINTERNAL;
    }

    for(int f = 0; f < NFRAMES; f++){
      ret = PIOc_setframe(ncid, varid4d, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_read_darray(ncid, varid4d, rd_iodesc, static_cast<int>(rbuf4d[f].size()), rbuf4d[f].data());
      if(!check_ret(tf, ret, "Failed to read 4d darray : " + filename)){
        return PIO_EINTERNAL;
      }

      if(!check_val(tf, rbuf4d[f], exp_val4d[f], "Got wrong 4d val, frame = " + std::to_string(f))){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), rd_iodesc);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED (rd_iodesc)")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED (wr_iodesc)")){
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
    {"nc_wr_rd_1d_unlim_dim_int", nc_wr_rd_1d_unlim_dim<int>},
    {"nc_wr_rd_1d_unlim_dim_float", nc_wr_rd_1d_unlim_dim<float>},
    {"nc_wr_rd_1d_unlim_dim_double", nc_wr_rd_1d_unlim_dim<double>},
    {"nc_wr_rd_2d_unlim_dim_int", nc_wr_rd_2d_unlim_dim<int>},
    {"nc_wr_rd_2d_unlim_dim_float", nc_wr_rd_2d_unlim_dim<float>},
    {"nc_wr_rd_2d_unlim_dim_double", nc_wr_rd_2d_unlim_dim<double>},
    {"nc_write_read_4d_col_decomp_int", nc_write_read_4d_col_decomp<int>},
    {"nc_write_read_4d_col_decomp_float", nc_write_read_4d_col_decomp<float>},
    {"nc_write_read_4d_col_decomp_double", nc_write_read_4d_col_decomp<double>},
    {"nc_reuse_3d_decomp_int", nc_reuse_3d_decomp<int>},
    {"nc_reuse_3d_decomp_float", nc_reuse_3d_decomp<float>},
    {"nc_reuse_3d_decomp_double", nc_reuse_3d_decomp<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_frame_tests_cxx", MPI_COMM_WORLD, rfuncs);
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
