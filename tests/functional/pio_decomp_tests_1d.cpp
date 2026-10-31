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

/* Get a 1d block decomposition with holes
 * If has_hole is TRUE, the decomposition is such that
 * # All even procs have VEC_LOCAL_SZ * 2 elements
 * # (the even procs take all elems from the odd procs)
 * # All odd procs have 0 elements
 * e.g. For VEC_LOCAL_SZ = 2,
 * e.g. 1)    [1,2,3,4] [] [5,6]
 * e.g. 2)    [1,2,3,4] [] [5,6,7,8] []
 * If has_hole is FALSE, the data is evenly divided among all procs
 * e.g. For VEC_LOCAL_SZ = 2,
 * e.g. 1)    [1,2] [3,4] [5,6]
 * e.g. 2)    [1,2] [3,4] [5,6] [7,8]
 */
void get_1d_bc_with_holes_info(int rank, int sz, int &dim, PIO_Offset &start, int &count, bool has_hole)
{
  const int VEC_LOCAL_SZ = 7;
  bool is_even_rank = (rank % 2 == 0);
  dim = VEC_LOCAL_SZ * sz;
  int iodd = rank / 2;
  int ieven = (rank + 1) / 2;
  if(has_hole){
    if(is_even_rank){
      count = (rank + 1 < sz) ? (VEC_LOCAL_SZ * 2) : VEC_LOCAL_SZ;
      start = ieven * VEC_LOCAL_SZ * 2 + 1;
    }
    else{
      count = 0;
      start = 0;
    }
  }
  else{
    count = VEC_LOCAL_SZ;
    start = ieven * VEC_LOCAL_SZ + iodd * VEC_LOCAL_SZ + 1;
  }
}

/* Get a decomp that represents data in global reverse order
 * i.e., rank i contains data corresponding to
 *       (total_procs - i - 1)
 * # All even procs have VEC_LOCAL_SZ elements
 * # All odd procs have VEC_LOCAL_SZ + 1 elements
 * e.g. For VEC_LOCAL_SZ = 2,
 * e.g. 1)    [6,7] [3,4,5] [1,2]
 * e.g. 2)    [8,9,10] [6,7] [3,4,5] [1,2]
 * e.g. 3)    [11,12] [8,9,10] [6,7] [3,4,5] [1,2]
 */
void get_1d_rev_info(int rank, int sz, int &dim, PIO_Offset &start, int &count)
{
  const int VEC_LOCAL_SZ = 7;
  bool is_even_rank = (rank % 2 == 0);
  int num_odd_procs = sz / 2;
  int num_even_procs = sz - num_odd_procs;
  dim = num_even_procs * VEC_LOCAL_SZ + num_odd_procs * (VEC_LOCAL_SZ + 1);
  int num_odd_procs_bfr_rank = rank / 2;
  int num_even_procs_bfr_rank = (rank + 1) / 2;
  int num_odd_procs_aftr_rank, num_even_procs_aftr_rank;
  if(is_even_rank){
    num_odd_procs_aftr_rank = num_odd_procs - num_odd_procs_bfr_rank;
    num_even_procs_aftr_rank = num_even_procs - num_even_procs_bfr_rank - 1;
  }
  else{
    num_odd_procs_aftr_rank = num_odd_procs - num_odd_procs_bfr_rank - 1;
    num_even_procs_aftr_rank = num_even_procs - num_even_procs_bfr_rank;
  }
  count = is_even_rank ? VEC_LOCAL_SZ : (VEC_LOCAL_SZ + 1);
  start = num_even_procs_aftr_rank * VEC_LOCAL_SZ + num_odd_procs_aftr_rank * (VEC_LOCAL_SZ + 1) + 1;
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

/* Test block cyclic interface
 * Write with one decomp and read with another
 * Test all combs
 * - no rearrage read + no rearrange write
 * - rearrage read + no rearrange write
 * - no rearrage read + rearrange write
 * - rearrage read + rearrange write
 */
template<typename T>
int nc_wr_rd_1d_bc(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  bool enable_rd_rearr[2] = {false, true};
  bool enable_wr_rearr[2] = {false, true};

  for(int rd_rearr_opt_idx = 0; rd_rearr_opt_idx < 2; rd_rearr_opt_idx++){
    for(int wr_rearr_opt_idx = 0; wr_rearr_opt_idx < 2; wr_rearr_opt_idx++){
      bool rd_rearr = enable_rd_rearr[rd_rearr_opt_idx];
      bool wr_rearr = enable_wr_rearr[wr_rearr_opt_idx];

      logger.log(SPIO_Util::Logger::Log_level::INFO,
          std::string("Testing Rd rearr = ") + (rd_rearr ? "true" : "false") +
          ", Write rearr = " + (wr_rearr ? "true" : "false") + "\n");

      /* Set the decomposition for writing data */
      PIO_Offset wr_start = 0;
      int wr_count = 0, dim = 0;
      get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, wr_start, wr_count, wr_rearr);

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
      get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, rd_start, rd_count, rd_rearr);

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
      const std::string PIO_VAR_NAME("PIO_TF_test_var");
      for(int i = 0; i < num_flavors; i++){
        int ncid = -1, dimid = -1, varid = -1;
        char iotype_name[PIO_MAX_NAME + 1];

        get_iotype_name(flavors[i], iotype_name);
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            std::string("Testing : nc_wr_rd_1d_bc : ") + iotype_name + "\n");

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

        ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
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

        /* Write the variable out */
        ret = PIOc_write_darray(ncid, varid, wr_iodesc, wr_count, wbuf.data(), NULL);
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

        for(std::size_t j = 0; j < rbuf.size(); j++){
          rbuf[j] = 0;
        }
        ret = PIOc_read_darray(ncid, varid, rd_iodesc, rd_count, rbuf.data());
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
            "ERROR: PIOc_freedecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
        return PIO_EINTERNAL;
      }

      ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
      if(ret != PIO_NOERR){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
        return PIO_EINTERNAL;
      }
    } /* wr_rearr_opt_idx */
  } /* rd_rearr_opt_idx */

  return PIO_NOERR;
}

/* Test 1d block decomposition with holes
 * Write and read back using the same decomposition
 */
template<typename T>
int nc_wr_1d_bc_with_holes(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  PIO_Offset start = 0;
  int count = 0, dim = 0;

  /* Set the decomposition for writing data - has holes */
  get_1d_bc_with_holes_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count, true);

  std::vector<T> wbuf(count);
  std::vector<PIO_Offset> compdof(count);
  std::vector<T> exp_val(count);
  for(int i = 0; i < count; i++){
    wbuf[i] = static_cast<T>(start + i);
    compdof[i] = static_cast<PIO_Offset>(start + i);
    exp_val[i] = static_cast<T>(start + i);
  }

  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          count, compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Define/write/read data */
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  std::vector<T> rbuf(count, 0);
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_1d_bc_with_holes : ") + iotype_name + "\n");

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

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
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

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, wr_iodesc, count, wbuf.data(), NULL);
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

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid, wr_iodesc, count, rbuf.data());
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

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write (and read back) data in global reverse order of
 * the file offsets.
 */
template<typename T>
int nc_wr_1d_rev(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  PIO_Offset start = 0;
  int count = 0, dim = 0;

  /* Set the decomposition for writing data */
  get_1d_rev_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count);

  std::vector<T> wbuf(count);
  std::vector<PIO_Offset> compdof(count);
  std::vector<T> exp_val(count);
  for(int i = 0; i < count; i++){
    wbuf[i] = static_cast<T>(start + i);
    compdof[i] = static_cast<PIO_Offset>(start + i);
    exp_val[i] = static_cast<T>(start + i);
  }

  int wr_iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          count, compdof.data(), &wr_iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Define/write/read data */
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  std::vector<T> rbuf(count, 0);
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_1d_rev : ") + iotype_name + "\n");

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

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
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

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, wr_iodesc, count, wbuf.data(), NULL);
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

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid, wr_iodesc, count, rbuf.data());
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

  ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Test PIOc_InitDecomp_bc() using start and count
 * Write with one decomp and read with another
 * Test all combs
 * - no rearrage read + no rearrange write
 * - rearrage read + no rearrange write
 * - no rearrage read + rearrange write
 * - rearrage read + rearrange write
 */
template<typename T>
int nc_wr_rd_1d_bc_start_count(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  bool enable_rd_rearr[2] = {false, true};
  bool enable_wr_rearr[2] = {false, true};

  for(int rd_rearr_opt_idx = 0; rd_rearr_opt_idx < 2; rd_rearr_opt_idx++){
    for(int wr_rearr_opt_idx = 0; wr_rearr_opt_idx < 2; wr_rearr_opt_idx++){
      bool rd_rearr = enable_rd_rearr[rd_rearr_opt_idx];
      bool wr_rearr = enable_wr_rearr[wr_rearr_opt_idx];

      logger.log(SPIO_Util::Logger::Log_level::INFO,
          std::string("Testing Rd rearr = ") + (rd_rearr ? "true" : "false") +
          ", Write rearr = " + (wr_rearr ? "true" : "false") + "\n");

      /* Set the decomposition for writing data */
      PIO_Offset wr_start = 0;
      int wr_count = 0, dim = 0;
      get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, wr_start, wr_count, wr_rearr);

      std::vector<T> wbuf(wr_count);
      for(int i = 0; i < wr_count; i++){
        wbuf[i] = static_cast<T>(wr_start + i);
      }

      /* PIOc_InitDecomp_bc() expects a 0-based start, unlike the 1-based
       * start returned by get_1d_bc_info() (used elsewhere to build 1-based
       * compdof arrays for PIOc_InitDecomp()) */
      long int wr_compstart = static_cast<long int>(wr_start) - 1;
      long int wr_compcount = static_cast<long int>(wr_count);

      int wr_iodesc = -1;
      ret = PIOc_InitDecomp_bc(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
              &wr_compstart, &wr_compcount, &wr_iodesc);
      if(ret != PIO_NOERR){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_InitDecomp_bc() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
        return PIO_EINTERNAL;
      }

      /* Set the decomposition for reading data - different from the write decomp */
      PIO_Offset rd_start = 0;
      int rd_count = 0;
      get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, rd_start, rd_count, rd_rearr);

      std::vector<T> rbuf(rd_count, 0);
      std::vector<T> exp_val(rd_count);
      for(int i = 0; i < rd_count; i++){
        /* Expected value, after reading, is the same as the compdof */
        exp_val[i] = static_cast<T>(rd_start + i);
      }

      long int rd_compstart = static_cast<long int>(rd_start) - 1;
      long int rd_compcount = static_cast<long int>(rd_count);

      int rd_iodesc = -1;
      ret = PIOc_InitDecomp_bc(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
              &rd_compstart, &rd_compcount, &rd_iodesc);
      if(ret != PIO_NOERR){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_InitDecomp_bc() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
        return PIO_EINTERNAL;
      }

      /* Define/write/read data */
      int num_flavors = 0;
      int flavors[SPIO_TEST_NUM_FLAVORS];
      get_iotypes(&num_flavors, flavors);

      const std::string filename("test_pio_decomp_simple_tests.testfile");
      const std::string PIO_VAR_NAME("PIO_TF_test_var");
      for(int i = 0; i < num_flavors; i++){
        int ncid = -1, dimid = -1, varid = -1;
        char iotype_name[PIO_MAX_NAME + 1];

        get_iotype_name(flavors[i], iotype_name);
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            std::string("Testing : nc_wr_rd_1d_bc_start_count : ") + iotype_name + "\n");

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

        ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
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

        /* Write the variable out */
        ret = PIOc_write_darray(ncid, varid, wr_iodesc, wr_count, wbuf.data(), NULL);
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

        for(std::size_t j = 0; j < rbuf.size(); j++){
          rbuf[j] = 0;
        }
        ret = PIOc_read_darray(ncid, varid, rd_iodesc, rd_count, rbuf.data());
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
            "ERROR: PIOc_freedecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
        return PIO_EINTERNAL;
      }

      ret = PIOc_freedecomp(tf.get_iosysid(), wr_iodesc);
      if(ret != PIO_NOERR){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
        return PIO_EINTERNAL;
      }
    } /* wr_rearr_opt_idx */
  } /* rd_rearr_opt_idx */

  return PIO_NOERR;
}

/* Test write/read of a variable where each proc writes/reads multiple
 * regions of the data
 */
template<typename T>
int nc_wr_rd_mreg_1d_bc(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int dim = 0;
  std::vector<PIO_Offset> start;
  std::vector<int> count;

  get_1d_bc_mreg_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count);

  int total_count = 0;
  for(std::size_t i = 0; i < count.size(); i++){
    total_count += count[i];
  }

  std::vector<T> wbuf(total_count);
  std::vector<T> rbuf(total_count);
  std::vector<PIO_Offset> compdof(total_count);
  int idx = 0;
  for(std::size_t i = 0; i < count.size(); i++){
    for(int j = 0; j < count[i]; j++){
      compdof[idx] = static_cast<PIO_Offset>(start[i] + j);
      wbuf[idx] = static_cast<T>(compdof[idx]);
      rbuf[idx] = static_cast<T>(compdof[idx]);
      idx++;
    }
  }

  int iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          total_count, compdof.data(), &iodesc, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Define/write/read data */
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_rd_mreg_1d_bc : ") + iotype_name + "\n");

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

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
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

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, iodesc, total_count, wbuf.data(), NULL);
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

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid, iodesc, total_count, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), iodesc);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(ret) + "\n");
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
    {"nc_wr_rd_1d_bc_int", nc_wr_rd_1d_bc<int>},
    {"nc_wr_rd_1d_bc_float", nc_wr_rd_1d_bc<float>},
    {"nc_wr_rd_1d_bc_double", nc_wr_rd_1d_bc<double>},
    {"nc_wr_1d_bc_with_holes_int", nc_wr_1d_bc_with_holes<int>},
    {"nc_wr_1d_bc_with_holes_float", nc_wr_1d_bc_with_holes<float>},
    {"nc_wr_1d_bc_with_holes_double", nc_wr_1d_bc_with_holes<double>},
    {"nc_wr_1d_rev_int", nc_wr_1d_rev<int>},
    {"nc_wr_1d_rev_float", nc_wr_1d_rev<float>},
    {"nc_wr_1d_rev_double", nc_wr_1d_rev<double>},
    {"nc_wr_rd_1d_bc_start_count_int", nc_wr_rd_1d_bc_start_count<int>},
    {"nc_wr_rd_1d_bc_start_count_float", nc_wr_rd_1d_bc_start_count<float>},
    {"nc_wr_rd_1d_bc_start_count_double", nc_wr_rd_1d_bc_start_count<double>},
    {"nc_wr_rd_mreg_1d_bc_int", nc_wr_rd_mreg_1d_bc<int>},
    {"nc_wr_rd_mreg_1d_bc_float", nc_wr_rd_mreg_1d_bc<float>},
    {"nc_wr_rd_mreg_1d_bc_double", nc_wr_rd_mreg_1d_bc<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_tests_1d_cxx", MPI_COMM_WORLD, rfuncs);
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
