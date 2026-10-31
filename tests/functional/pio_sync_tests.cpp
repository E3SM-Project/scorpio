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

/* Get a block cyclic decomposition
 * If force_rearrange is FALSE, the decomposition is such that
 * # All even procs have VEC_LOCAL_SZ elements
 * # All odd procs have VEC_LOCAL_SZ + 1 elements
 * e.g. For VEC_LOCAL_SZ = 2,
 * e.g. 1)    [1,2] [3,4,5] [6,7]
 * e.g. 2)    [1,2] [3,4,5] [6,7] [8,9,10]
 * e.g. 3)    [1,2] [3,4,5] [6,7] [8,9,10] [11,12]
 * If force_rearrange is TRUE, the decomposition is such that,
 * If possible, the even rank "exchanges" elements with the next
 * higher ranked odd proc.
 * This for example can be used to force rearrangement when reading
 * or writing data.
 * e.g. For VEC_LOCAL_SZ = 2,
 * e.g. 1)    [3,4,5] [1,2] [6,7]
 * e.g. 2)    [3,4,5] [1,2] [8,9,10] [6,7]
 * e.g. 3)    [3,4,5] [1,2] [8,9,10] [6,7] [11,12]
 */
void get_1d_bc_info(int rank, int sz, int &dim, PIO_Offset &start, int &count, bool force_rearrange)
{
  const int VEC_LOCAL_SZ = 7;
  bool is_even_rank = (rank % 2 == 0);
  int num_odd_procs = sz / 2;
  int num_even_procs = sz - num_odd_procs;
  dim = num_even_procs * VEC_LOCAL_SZ + num_odd_procs * (VEC_LOCAL_SZ + 1);
  int iodd = rank / 2;
  int ieven = (rank + 1) / 2;
  if(force_rearrange){
    if(is_even_rank){
      if(rank + 1 < sz){
        count = VEC_LOCAL_SZ + 1;
        start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) + VEC_LOCAL_SZ + 1;
      }
      else{
        count = VEC_LOCAL_SZ;
        start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) + 1;
      }
    }
    else{
      count = VEC_LOCAL_SZ;
      start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) - VEC_LOCAL_SZ + 1;
    }
  }
  else{
    count = is_even_rank ? VEC_LOCAL_SZ : (VEC_LOCAL_SZ + 1);
    start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) + 1;
  }
}

/* Get a block cyclic decomposition with multiple regions to write
 * from a single proc
 * # All procs have VEC_LOCAL_SZ elements starting at offset rank+1
 *   and also starting at offset (size * VEC_LOCAL_SZ)
 * e.g. For VEC_LOCAL_SZ = 2, NUM_REGIONS=2
 * e.g. 1)    [1,2,7,8] [3,4,9,10] [5,6,11,12]
 */
void get_1d_bc_mreg_info(int rank, int sz, int &dim, std::vector<PIO_Offset> &start, std::vector<int> &count)
{
  const int VEC_LOCAL_SZ = 7;
  const int NUM_REGIONS = 3;
  dim = sz * VEC_LOCAL_SZ * NUM_REGIONS;
  start.resize(NUM_REGIONS);
  count.resize(NUM_REGIONS);
  for(int i = 0; i < NUM_REGIONS; i++){
    count[i] = VEC_LOCAL_SZ;
    start[i] = rank * VEC_LOCAL_SZ + i * (sz * VEC_LOCAL_SZ) + 1;
  }
}

/* Test writes multiple variables before syncing and reading them back */
template<typename T>
int nc_mvar_wr_rd_1d_bc(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;

  /* Set the decomposition for writing data - forcing rearrangement */
  PIO_Offset wr_start = 0;
  int wr_count = 0, dim = 0;
  get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, wr_start, wr_count, true);

  std::vector<T> wbuf(wr_count);
  std::vector<PIO_Offset> compdof(wr_count);
  for(int i = 0; i < wr_count; i++){
    wbuf[i] = static_cast<T>(wr_start + i);
    compdof[i] = static_cast<PIO_Offset>(wr_start + i);
  }

  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          wr_count, compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data - different from the write decomp */
  PIO_Offset rd_start = 0;
  int rd_count = 0;
  get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, rd_start, rd_count, false);

  std::vector<T> rbuf(rd_count, 0);
  std::vector<PIO_Offset> rd_compdof(rd_count);
  std::vector<T> exp_val(rd_count);
  for(int i = 0; i < rd_count; i++){
    rd_compdof[i] = static_cast<PIO_Offset>(rd_start + i);
    /* Expected value, after reading, is the same as the compdof */
    exp_val[i] = static_cast<T>(rd_start + i);
  }

  int rd_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          rd_count, rd_compdof.data(), &rd_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Define/write/read data */
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  const std::string PIO_VAR_NAME1("PIO_TF_test_var1");
  const std::string PIO_VAR_NAME2("PIO_TF_test_var2");
  const std::string PIO_VAR_NAME3("PIO_TF_test_var3");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid1 = -1, varid2 = -1, varid3 = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_mvar_wr_rd_1d_bc : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dim, &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME1.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME2.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME3.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid3);
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

    /* Write the variables out */
    ret = PIOc_write_darray(ncid, varid1, wr_iodesc, wr_count, wbuf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, wr_iodesc, wr_count, wbuf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid3, wr_iodesc, wr_count, wbuf.data(), NULL);
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

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME1.c_str(), &varid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var1 : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME2.c_str(), &varid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var2 : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME3.c_str(), &varid3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var3 : " + filename + "\n");
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

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid1, rd_iodesc, rd_count, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var1) : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val (var1)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid2, rd_iodesc, rd_count, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var2) : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val (var2)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid3, rd_iodesc, rd_count, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var3) : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val (var3)")){
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
        "ERROR: PIOc_freedecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Test writes multiple variables before syncing and reading them back
 * This test adds multiple syncs between writes
 */
template<typename T>
int nc_mvar_msync_1d_bc(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  const int NVARS = 8;
  const int SYNC_STRIDE_START = 1;
  /* The strides get incremented by SYNC_STRIDE_INC after each sync call
   * With NVARS = 10, SYNC_STRIDE_START = 1, SYNC_STRIDE_INC = 1, the sync
   * call is invoked after
   * 1st (stride = 1), 3rd (stride = 2), 6th (stride = 3), 10th (stride = 4)
   * PIOc_write_darray() calls
   */
  const int SYNC_STRIDE_INC = 1;

  /* Set the decomposition for writing data - forcing rearrangement */
  PIO_Offset wr_start = 0;
  int wr_count = 0, dim = 0;
  get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, wr_start, wr_count, true);

  std::vector<T> wbuf(wr_count);
  std::vector<PIO_Offset> compdof(wr_count);
  for(int i = 0; i < wr_count; i++){
    wbuf[i] = static_cast<T>(wr_start + i);
    compdof[i] = static_cast<PIO_Offset>(wr_start + i);
  }

  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          wr_count, compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data - different from the write decomp */
  PIO_Offset rd_start = 0;
  int rd_count = 0;
  get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, rd_start, rd_count, false);

  std::vector<T> rbuf(rd_count, 0);
  std::vector<PIO_Offset> rd_compdof(rd_count);
  std::vector<T> exp_val(rd_count);
  for(int i = 0; i < rd_count; i++){
    rd_compdof[i] = static_cast<PIO_Offset>(rd_start + i);
    /* Expected value, after reading, is the same as the compdof */
    exp_val[i] = static_cast<T>(rd_start + i);
  }

  int rd_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          rd_count, rd_compdof.data(), &rd_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Define/write/read data */
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1;
    char iotype_name[PIO_MAX_NAME + 1];
    std::vector<int> varids(NVARS);
    std::vector<std::string> vnames(NVARS);

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_mvar_msync_1d_bc : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dim, &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    for(int j = 1; j <= NVARS; j++){
      vnames[j - 1] = "PIO_TF_test_var" + std::to_string(j);
      ret = PIOc_def_var(ncid, vnames[j - 1].c_str(), Pio_type_map<T>::value, 1, &dimid, &(varids[j - 1]));
      if(ret != PIO_NOERR){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: Failed to define a var : " + filename + "\n");
        return PIO_EINTERNAL;
      }
    }

    ret = PIOc_enddef(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end redef mode : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* Write the variables out - all the variables write out the same
     * data.
     * The stride determines the sync calls, stride starts
     * at SYNC_STRIDE_START and increases by SYNC_STRIDE_INC
     */
    int isync = SYNC_STRIDE_START;
    int prev_isync = 0;
    int cur_sync_stride = SYNC_STRIDE_START;
    for(int j = 1; j <= NVARS; j++){
      ret = PIOc_write_darray(ncid, varids[j - 1], wr_iodesc, wr_count, wbuf.data(), NULL);
      if(ret != PIO_NOERR){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: Failed to write darray : " + filename + "\n");
        return PIO_EINTERNAL;
      }

      if(j == isync){
#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
        ret = PIOc_closefile(ncid);
        if(ret != PIO_NOERR){
          logger.log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: Failed to close file : " + filename + "\n");
          return PIO_EINTERNAL;
        }

        ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_WRITE);
        if(ret != PIO_NOERR){
          logger.log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: Could not reopen file : " + filename + "\n");
          return PIO_EINTERNAL;
        }

        for(int k = 1; k <= NVARS; k++){
          ret = PIOc_inq_varid(ncid, vnames[k - 1].c_str(), &(varids[k - 1]));
          if(ret != PIO_NOERR){
            logger.log(SPIO_Util::Logger::Log_level::INFO,
                "ERROR: Could not inq var : " + filename + "\n");
            return PIO_EINTERNAL;
          }
        }
#else
        ret = PIOc_sync(ncid);
        if(ret != PIO_NOERR){
          logger.log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: Failed to sync file : " + filename + "\n");
          return PIO_EINTERNAL;
        }
#endif

        for(int k = prev_isync + 1; k <= isync; k++){
          for(std::size_t m = 0; m < rbuf.size(); m++){
            rbuf[m] = 0;
          }
          ret = PIOc_read_darray(ncid, varids[k - 1], rd_iodesc, rd_count, rbuf.data());
          if(ret != PIO_NOERR){
            logger.log(SPIO_Util::Logger::Log_level::INFO,
                "ERROR: Failed to read darray : " + filename + "\n");
            return PIO_EINTERNAL;
          }

          if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
            return PIO_EINTERNAL;
          }
        }
        cur_sync_stride += SYNC_STRIDE_INC;
        prev_isync = isync;
        isync += cur_sync_stride;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    for(int k = 1; k <= NVARS; k++){
      ret = PIOc_inq_varid(ncid, vnames[k - 1].c_str(), &(varids[k - 1]));
      if(ret != PIO_NOERR){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: Could not inq var : " + filename + "\n");
        return PIO_EINTERNAL;
      }
    }
#else
    ret = PIOc_sync(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file : " + filename + "\n");
      return PIO_EINTERNAL;
    }
#endif

    if(prev_isync < NVARS){
      for(int k = prev_isync + 1; k <= NVARS; k++){
        for(std::size_t m = 0; m < rbuf.size(); m++){
          rbuf[m] = 0;
        }
        ret = PIOc_read_darray(ncid, varids[k - 1], rd_iodesc, rd_count, rbuf.data());
        if(ret != PIO_NOERR){
          logger.log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: Failed to read darray : " + filename + "\n");
          return PIO_EINTERNAL;
        }

        if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
          return PIO_EINTERNAL;
        }
      }
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
        "ERROR: PIOc_freedecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
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
    {"nc_mvar_wr_rd_1d_bc_int", nc_mvar_wr_rd_1d_bc<int>},
    {"nc_mvar_wr_rd_1d_bc_float", nc_mvar_wr_rd_1d_bc<float>},
    {"nc_mvar_wr_rd_1d_bc_double", nc_mvar_wr_rd_1d_bc<double>},
    {"nc_mvar_msync_1d_bc_int", nc_mvar_msync_1d_bc<int>},
    {"nc_mvar_msync_1d_bc_float", nc_mvar_msync_1d_bc<float>},
    {"nc_mvar_msync_1d_bc_double", nc_mvar_msync_1d_bc<double>}
  };

  SPIO_TF::Test_framework tf("pio_sync_tests_cxx", MPI_COMM_WORLD, rfuncs);
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
