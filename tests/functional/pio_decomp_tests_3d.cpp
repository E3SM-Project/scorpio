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
 * rearrangement) */
template<typename T>
int nc_write_read_3d_col_decomp(SPIO_TF::Test_framework &tf)
{
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int dims[3], start[3], count[3];
  int rank = tf.get_comm_rank();
  int sz = tf.get_comm_size();

  /* Set the decomposition for writing data - forcing rearrangement */
  get_3d_col_decomp_info(rank, sz, dims, start, count, true);
  int nrows = count[0];
  int ncols = count[1];
  int nhgts = count[2];
  std::size_t local_sz = static_cast<std::size_t>(nrows) * ncols * nhgts;

  std::vector<T> wbuf(local_sz);
  std::vector<PIO_Offset> compdof(local_sz);
  for(int k = 1; k <= nhgts; k++){
    for(int j = 1; j <= ncols; j++){
      for(int i = 1; i <= nrows; i++){
        PIO_Offset val = static_cast<PIO_Offset>(start[2] - 1 + k - 1) * (dims[0] * dims[1]) +
            static_cast<PIO_Offset>(start[1] - 1 + j - 1) * dims[0] + i;
        std::size_t tmp_idx = static_cast<std::size_t>(k - 1) * (ncols * nrows) +
            static_cast<std::size_t>(j - 1) * nrows + (i - 1);
        wbuf[tmp_idx] = static_cast<T>(val);
        compdof[tmp_idx] = val;
      }
    }
  }

  int wr_gdims[3] = {dims[2], dims[1], dims[0]};
  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 3, wr_gdims,
          static_cast<int>(local_sz), compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (wr_iodesc), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data - different from the write decomp */
  get_3d_col_decomp_info(rank, sz, dims, start, count, false);
  nrows = count[0];
  ncols = count[1];
  nhgts = count[2];
  local_sz = static_cast<std::size_t>(nrows) * ncols * nhgts;

  std::vector<T> rbuf(local_sz), exp_val(local_sz);
  compdof.assign(local_sz, 0);
  for(int k = 1; k <= nhgts; k++){
    for(int j = 1; j <= ncols; j++){
      for(int i = 1; i <= nrows; i++){
        PIO_Offset val = static_cast<PIO_Offset>(start[2] - 1 + k - 1) * (dims[0] * dims[1]) +
            static_cast<PIO_Offset>(start[1] - 1 + j - 1) * dims[0] + i;
        std::size_t tmp_idx = static_cast<std::size_t>(k - 1) * (ncols * nrows) +
            static_cast<std::size_t>(j - 1) * nrows + (i - 1);
        compdof[tmp_idx] = val;
        exp_val[tmp_idx] = static_cast<T>(val);
      }
    }
  }

  int rd_gdims[3] = {dims[2], dims[1], dims[0]};
  int rd_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 3, rd_gdims,
          static_cast<int>(local_sz), compdof.data(), &rd_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (rd_iodesc), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  for(int f = 0; f < num_flavors; f++){
    int ncid = -1, row_dimid = -1, col_dimid = -1, hgt_dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[f], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_write_read_3d_col_decomp : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[f]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_row", dims[0], &row_dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define row dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_col", dims[1], &col_dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define col dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_hgt", dims[2], &hgt_dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define hgt dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    int dimids[3] = {hgt_dimid, col_dimid, row_dimid};
    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 3, dimids, &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end redef mode : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* Write the variable out, using the forced-rearrangement decomp */
    ret = PIOc_write_darray(ncid, varid, wr_iodesc, static_cast<int>(wbuf.size()), wbuf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[f]), filename.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var : " + filename + "\n");
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file : " + filename + "\n");
      return PIO_EINTERNAL;
    }
#endif

    /* Read the variable back in, using the natural (non-rearranged) decomp */
    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid, rd_iodesc, static_cast<int>(rbuf.size()), rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file : " + filename + "\n");
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), rd_iodesc);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (rd_iodesc), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (wr_iodesc), ret = " + std::to_string(ret) + "\n");
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
    {"nc_write_read_3d_col_decomp_int", nc_write_read_3d_col_decomp<int>},
    {"nc_write_read_3d_col_decomp_float", nc_write_read_3d_col_decomp<float>},
    {"nc_write_read_3d_col_decomp_double", nc_write_read_3d_col_decomp<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_tests_3d_cxx", MPI_COMM_WORLD, rfuncs);
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
