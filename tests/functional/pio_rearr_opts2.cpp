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

const std::string FNAME = "pio_rearr_opts2_test.nc";
const std::string VAR_NAME = "dummy_var";
const std::string DIM_NAME = "dummy_dim";
const int VEC_LOCAL_SZ = 3;

/* If force_rearrange is FALSE, the decomposition is such that
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
 *
 * Mirrors tests/general/pio_rearr_opts2.F90.in's get_1d_bc_info() subroutine.
 */
void get_1d_bc_info(int rank, int sz, int &dim, PIO_Offset &start, int &count, bool force_rearrange)
{
  bool is_even_rank = (rank % 2 == 0);
  int num_odd_procs = sz / 2;
  int num_even_procs = sz - num_odd_procs;
  dim = num_even_procs * VEC_LOCAL_SZ + num_odd_procs * (VEC_LOCAL_SZ + 1);

  /* Number of odd and even procs before this rank */
  int iodd = rank / 2;
  int ieven = (rank + 1) / 2;

  if(force_rearrange){
    /* Make sure that we force rearrangement */
    if(is_even_rank){
      if(rank + 1 < sz){
        /* Force rearrangement */
        count = VEC_LOCAL_SZ + 1;
        start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) + VEC_LOCAL_SZ + 1;
      }
      else{
        count = VEC_LOCAL_SZ;
        start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) + 1;
      }
    }
    else{
      /* For all odd procs there is an even lower ranked, rank-1, proc
       * So force rearrangement
       */
      count = VEC_LOCAL_SZ;
      start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) - VEC_LOCAL_SZ + 1;
    }
  }
  else{
    if(is_even_rank){
      count = VEC_LOCAL_SZ;
    }
    else{
      count = VEC_LOCAL_SZ + 1;
    }
    start = ieven * VEC_LOCAL_SZ + iodd * (VEC_LOCAL_SZ + 1) + 1;
  }
}

/* Log the rearranger options being tested - mirrors
 * tests/general/pio_rearr_opts2.F90.in's print_rearr_opts() subroutine.
 * The return value from the Fortran version is always PIO_NOERR and
 * ignored by both call sites, so it is not ported here.
 */
void print_rearr_opts(SPIO_TF::Test_framework &tf, int comm_type, int fcd,
    bool enable_hs_c2i, bool enable_isend_c2i, int max_pend_req_c2i,
    bool enable_hs_i2c, bool enable_isend_i2c, int max_pend_req_i2c)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  std::string msg = "PIO rearranger options :\n";

  if(comm_type == PIO_REARR_COMM_P2P){
    msg += " comm_type = PIO_REARR_COMM_P2P\n";
  }
  else if(comm_type == PIO_REARR_COMM_COLL){
    msg += " comm_type = PIO_REARR_COMM_COLL\n";
  }
  else{
    msg += " comm_type = INVALID\n";
  }

  if(fcd == PIO_REARR_COMM_FC_2D_ENABLE){
    msg += " fcd = PIO_REARR_COMM_FC_2D_ENABLE\n";
  }
  else if(fcd == PIO_REARR_COMM_FC_1D_COMP2IO){
    msg += " fcd = PIO_REARR_COMM_FC_1D_COMP2IO\n";
  }
  else if(fcd == PIO_REARR_COMM_FC_1D_IO2COMP){
    msg += " fcd = PIO_REARR_COMM_FC_1D_IO2COMP\n";
  }
  else if(fcd == PIO_REARR_COMM_FC_2D_DISABLE){
    msg += " fcd = PIO_REARR_COMM_FC_2D_DISABLE\n";
  }
  else{
    msg += " fcd = INVALID\n";
  }

  msg += " comp2io max_pend_req = " + std::to_string(max_pend_req_c2i) + "\n";
  msg += std::string(" comp2io enable_hs = ") + (enable_hs_c2i ? "TRUE" : "FALSE") + "\n";
  msg += std::string(" comp2io enable_isend = ") + (enable_isend_c2i ? "TRUE" : "FALSE") + "\n";

  msg += " io2comp max_pend_req = " + std::to_string(max_pend_req_i2c) + "\n";
  msg += std::string(" io2comp enable_hs = ") + (enable_hs_i2c ? "TRUE" : "FALSE") + "\n";
  msg += std::string(" io2comp enable_isend = ") + (enable_isend_i2c ? "TRUE" : "FALSE") + "\n";

  logger.log(SPIO_Util::Logger::Log_level::INFO, msg);
}

/* Create a decomp that will be used by the tests below.
 * The ioid needs to be freed by the caller (PIOc_freedecomp()).
 * Mirrors tests/general/pio_rearr_opts2.F90.in's create_decomp_and_init_buf()
 * subroutine.
 */
int create_decomp_and_init_buf(SPIO_TF::Test_framework &tf, int &ioid, std::vector<float> &wbuf, int &dim)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  PIO_Offset start = 0;
  int count = 0;

  get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count, true);

  wbuf.resize(count);
  std::vector<PIO_Offset> compdof(count);
  for(int i = 0; i < count; i++){
    wbuf[i] = static_cast<float>(start + i);
    compdof[i] = static_cast<PIO_Offset>(wbuf[i]);
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), PIO_REAL, 1, &dim, count, compdof.data(), &ioid, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Create file and var used in the tests below. All details are picked
 * from the file-scope constants above. Mirrors
 * tests/general/pio_rearr_opts2.F90.in's create_file_and_var() subroutine.
 */
int create_file_and_var(SPIO_TF::Test_framework &tf, int iotype, const std::string &iotype_name)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int ncid = -1, dimid = -1, varid = -1;
  int dim = 0;
  PIO_Offset start = 0;
  int count = 0;

  logger.log(SPIO_Util::Logger::Log_level::INFO, "Testing : " + iotype_name + "\n");

  get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count, true);

  ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Could not create file " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), (PIO_Offset)dim, &dimid);
  if(!check_ret(tf, ret, "Failed to define dim file = " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), PIO_REAL, 1, &dimid, &varid);
  if(!check_ret(tf, ret, "Failed to define var file = " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef file = " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close file = " + FNAME)){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Open file and inq var. All details are picked from the file-scope
 * constants above.
 * Note: The file is kept open so the caller needs to close it.
 * Mirrors tests/general/pio_rearr_opts2.F90.in's open_file_and_get_var()
 * subroutine.
 */
int open_file_and_get_var(SPIO_TF::Test_framework &tf, int iotype, int &ncid, int &varid)
{
  int ret = PIO_NOERR;

  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Could not open file " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &varid);
  if(!check_ret(tf, ret, "Could not inq var " + FNAME)){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Test all possible combinations of rearranger options using the pio
 * interface to explicitly set the rearranger options after init.
 * Mirrors tests/general/pio_rearr_opts2.F90.in's
 * PIO_TF_AUTO_TEST_SUB_BEGIN set_rearr_opts_and_write.
 */
int set_rearr_opts_and_write(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;

  /* Different rearranger options that are tested here */
  const int NUM_COMM_TYPE_OPTS = 2;
  const int comm_types[NUM_COMM_TYPE_OPTS] = { PIO_REARR_COMM_P2P, PIO_REARR_COMM_COLL };

  const int NUM_FCD_OPTS = 4;
  const int fcds[NUM_FCD_OPTS] = { PIO_REARR_COMM_FC_2D_DISABLE, PIO_REARR_COMM_FC_1D_COMP2IO,
                                    PIO_REARR_COMM_FC_1D_IO2COMP, PIO_REARR_COMM_FC_2D_ENABLE };

  const int NUM_ENABLE_HS_OPTS = 2;
  const bool enable_hs_opts[NUM_ENABLE_HS_OPTS] = { true, false };

  const int NUM_ENABLE_ISEND_OPTS = 2;
  const bool enable_isend_opts[NUM_ENABLE_ISEND_OPTS] = { true, false };

  const int NUM_MAX_PEND_REQ_OPTS = 3;
  const int max_pend_req_opts[NUM_MAX_PEND_REQ_OPTS] = { PIO_REARR_COMM_UNLIMITED_PEND_REQ, 1, 2 };

  /* iotypes = valid io types */
  int num_iotypes = 0;
  int iotypes[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_iotypes, iotypes);

  for(int i = 0; i < num_iotypes; i++){
    char iotype_name_buf[PIO_MAX_NAME + 1];
    iotype_name_buf[0] = '\0';
    get_iotype_name(iotypes[i], iotype_name_buf);

    int ncid = -1, varid = -1;

    /* Create the file and decomp */
    ret = create_file_and_var(tf, iotypes[i], std::string(iotype_name_buf));
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    ret = open_file_and_get_var(tf, iotypes[i], ncid, varid);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    /* Test all combinations of these flow control parameters */
    for(int comm_type_idx = 0; comm_type_idx < NUM_COMM_TYPE_OPTS; comm_type_idx++){
      int comm_type = comm_types[comm_type_idx];

      /* For coll we only test PIO_REARR_COMM_FC_2D_DISABLE,
       * for p2p we test all possible combinations
       */
      int num_fcds = (comm_type == PIO_REARR_COMM_COLL) ? 1 : NUM_FCD_OPTS;

      for(int fcd_idx = 0; fcd_idx < num_fcds; fcd_idx++){
        int fcd = fcds[fcd_idx];

        int num_hs_c2i = 1, num_isend_c2i = 1, num_pend_c2i = 1;
        int num_hs_i2c = 1, num_isend_i2c = 1, num_pend_i2c = 1;

        if(fcd == PIO_REARR_COMM_FC_2D_ENABLE){
          num_hs_c2i = NUM_ENABLE_HS_OPTS;
          num_isend_c2i = NUM_ENABLE_ISEND_OPTS;
          num_pend_c2i = NUM_MAX_PEND_REQ_OPTS;
          num_hs_i2c = NUM_ENABLE_HS_OPTS;
          num_isend_i2c = NUM_ENABLE_ISEND_OPTS;
          num_pend_i2c = NUM_MAX_PEND_REQ_OPTS;
        }
        else if(fcd == PIO_REARR_COMM_FC_1D_COMP2IO){
          /* Only test different rearr opts in comp2io direction */
          num_hs_c2i = NUM_ENABLE_HS_OPTS;
          num_isend_c2i = NUM_ENABLE_ISEND_OPTS;
          num_pend_c2i = NUM_MAX_PEND_REQ_OPTS;
        }
        else if(fcd == PIO_REARR_COMM_FC_1D_IO2COMP){
          /* Only test different rearr opts in io2comp direction */
          num_hs_i2c = NUM_ENABLE_HS_OPTS;
          num_isend_i2c = NUM_ENABLE_ISEND_OPTS;
          num_pend_i2c = NUM_MAX_PEND_REQ_OPTS;
        }
        /* else fcd == PIO_REARR_COMM_FC_2D_DISABLE :
         * Just test one default combination (num_* already 1)
         */

        for(int hs_c2i_idx = 0; hs_c2i_idx < num_hs_c2i; hs_c2i_idx++){
          for(int isend_c2i_idx = 0; isend_c2i_idx < num_isend_c2i; isend_c2i_idx++){
            for(int pend_c2i_idx = 0; pend_c2i_idx < num_pend_c2i; pend_c2i_idx++){
              for(int hs_i2c_idx = 0; hs_i2c_idx < num_hs_i2c; hs_i2c_idx++){
                for(int isend_i2c_idx = 0; isend_i2c_idx < num_isend_i2c; isend_i2c_idx++){
                  for(int pend_i2c_idx = 0; pend_i2c_idx < num_pend_i2c; pend_i2c_idx++){

                    print_rearr_opts(tf, comm_type, fcd,
                        enable_hs_opts[hs_c2i_idx], enable_isend_opts[isend_c2i_idx], max_pend_req_opts[pend_c2i_idx],
                        enable_hs_opts[hs_i2c_idx], enable_isend_opts[isend_i2c_idx], max_pend_req_opts[pend_i2c_idx]);
                    /* Ignoring return value - just printing for info */

                    /* Set the rearranger options */
                    ret = PIOc_set_rearr_opts(tf.get_iosysid(), comm_type, fcd,
                        enable_hs_opts[hs_c2i_idx], enable_isend_opts[isend_c2i_idx], max_pend_req_opts[pend_c2i_idx],
                        enable_hs_opts[hs_i2c_idx], enable_isend_opts[isend_i2c_idx], max_pend_req_opts[pend_i2c_idx]);
                    if(!check_ret(tf, ret, "PIOc_set_rearr_opts failed")){
                      return PIO_EINTERNAL;
                    }

                    int ioid = -1;
                    std::vector<float> wbuf;
                    int dim = 0;
                    ret = create_decomp_and_init_buf(tf, ioid, wbuf, dim);
                    if(ret != PIO_NOERR){
                      return PIO_EINTERNAL;
                    }

                    std::vector<float> rbuf(wbuf.size(), 0.0f);

                    ret = PIOc_write_darray(ncid, varid, ioid, static_cast<PIO_Offset>(wbuf.size()), wbuf.data(), NULL);
                    if(!check_ret(tf, ret, "Writing var failed fname = " + FNAME)){
                      return PIO_EINTERNAL;
                    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
                    PIOc_closefile(ncid);

                    ret = open_file_and_get_var(tf, iotypes[i], ncid, varid);
                    if(ret != PIO_NOERR){
                      return PIO_EINTERNAL;
                    }
#else
                    ret = PIOc_sync(ncid);
                    if(!check_ret(tf, ret, "Could not sync file " + FNAME)){
                      return PIO_EINTERNAL;
                    }
#endif

                    ret = PIOc_read_darray(ncid, varid, ioid, static_cast<PIO_Offset>(rbuf.size()), rbuf.data());
                    if(!check_ret(tf, ret, "Reading var failed fname = " + FNAME)){
                      return PIO_EINTERNAL;
                    }

                    if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
                      return PIO_EINTERNAL;
                    }

                    PIOc_freedecomp(tf.get_iosysid(), ioid);

                  } // pend_i2c_idx
                } // isend_i2c_idx
              } // hs_i2c_idx
            } // pend_c2i_idx
          } // isend_c2i_idx
        } // hs_c2i_idx
      } // fcd_idx
    } // comm_type_idx

    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
  } // iotypes

  return PIO_NOERR;
}

int main(int argc, char *argv[])
{
  int ret = MPI_SUCCESS, nerrs = 0;
#if PIO_USE_ASYNC_WR_THREAD
  int mpi_thread_support_provided = MPI_THREAD_SINGLE;
  ret = MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &mpi_thread_support_provided);
#else
  ret = MPI_Init(&argc, &argv);
#endif
  if(ret != MPI_SUCCESS){
    std::cerr << "MPI_Init() FAILED, ret = " << ret << "\n";
    return ret;
  }

  std::vector<std::pair<std::string, std::function<int(SPIO_TF::Test_framework &)> > > rfuncs = {
    {"set_rearr_opts_and_write", set_rearr_opts_and_write}
  };

  SPIO_TF::Test_framework tf("pio_rearr_opts2_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  if(tf.init_iosystem(PIO_REARR_BOX) != PIO_NOERR){
    nerrs++;
  }
  else{
    nerrs += tf.run();
    tf.finalize_iosystem();
  }

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
