#include <algorithm>
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

/* Split tf.get_comm() (the test's world communicator) into ncomms disjoint
 * sub-communicators of as-equal-as-possible size. The last sub-comm
 * absorbs any remainder ranks. gcomm_idx is the 1-based index (1..ncomms)
 * of the sub-comm this rank lands in, mirroring
 * tests/general/pio_async_decomp_tests_1d.F90.in's split_world_ncomms(). */
static void split_world_ncomms(const SPIO_TF::Test_framework &tf, int ncomms,
    MPI_Comm &new_comm, int &new_rank, int &new_size, int &gcomm_idx)
{
  new_comm = MPI_COMM_NULL;
  new_rank = 0;
  new_size = 0;
  gcomm_idx = 0;

  int world_sz = tf.get_comm_size();
  int world_rank = tf.get_comm_rank();

  /* We need at least one proc in each disjoint comm */
  if(world_sz < ncomms){
    return;
  }

  int nprocs_per_comm = world_sz / ncomms;
  bool is_last_comm = (world_rank >= nprocs_per_comm * (ncomms - 1));

  int color = world_rank / nprocs_per_comm;
  /* Make sure that every proc in the last set (all remaining procs are
   * included in the last set/comm) gets the same color */
  if(is_last_comm){
    color = ncomms - 1;
  }
  gcomm_idx = color + 1;

  MPI_Comm_split(tf.get_comm(), color, 0, &new_comm);
  MPI_Comm_size(new_comm, &new_size);
  MPI_Comm_rank(new_comm, &new_rank);
}

/* Reduce (ierr == PIO_NOERR) over comm (NOT tf.get_comm()) and log on
 * comm-rank 0 on failure. Mirrors the Fortran reference's 3-argument
 * PIO_TF_CHECK_ERR(ret, new_comm, msg) form (confirmed via
 * tests/general/util/pio_tf_f90gen.pl:415-427 to reduce over the passed-in
 * comm). Used instead of check_ret()/passert()
 * (tests/util/spio_test_check.hpp), which always reduce over
 * tf.get_comm() and would deadlock once ranks have diverged into I/O vs.
 * compute roles, since the I/O rank cannot reach a tf.get_comm()
 * collective until every compute rank has called PIOc_finalize(). comm
 * here is always a compute component's own sub-comm, which never includes
 * the I/O rank, so this Allreduce is deadlock-safe. */
static bool check_ret_over_comm(SPIO_TF::Test_framework &tf, int ierr, MPI_Comm comm,
    const std::string &ctx_msg)
{
  int lpass_i = (ierr == PIO_NOERR) ? 1 : 0;
  int gpass_i = 0;
  MPI_Allreduce(&lpass_i, &gpass_i, 1, MPI_INT, MPI_LAND, comm);

  if(gpass_i == 0){
    int comm_rank = 0;
    MPI_Comm_rank(comm, &comm_rank);
    if(comm_rank == 0){
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
          ctx_msg + " : ERROR: ret = " + std::to_string(ierr) + "\n");
    }
  }

  return (gpass_i != 0);
}

/* Get a 1d block decomposition with holes.
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
 *
 * Mirrors tests/general/pio_async_decomp_tests_1d.F90.in:56-67's
 * get_1d_bc_with_holes_info() subroutine. rank/sz here are always the
 * compute sub-comm's own new_rank/new_size, never the world rank/size. */
static void get_1d_bc_with_holes_info(int rank, int sz, int &dim, PIO_Offset &start,
    int &count, bool has_hole)
{
  const int VEC_LOCAL_SZ = 7;

  bool is_even_rank = (rank % 2 == 0);
  dim = VEC_LOCAL_SZ * sz;

  int iodd = rank / 2;
  int ieven = (rank + 1) / 2;

  if(has_hole){
    if(is_even_rank){
      if(rank + 1 < sz){
        count = VEC_LOCAL_SZ * 2;
      }
      else{
        count = VEC_LOCAL_SZ;
      }
      start = static_cast<PIO_Offset>(ieven) * VEC_LOCAL_SZ * 2 + 1;
    }
    else{
      count = 0;
      start = 0;
    }
  }
  else{
    count = VEC_LOCAL_SZ;
    start = static_cast<PIO_Offset>(ieven) * VEC_LOCAL_SZ + static_cast<PIO_Offset>(iodd) * VEC_LOCAL_SZ + 1;
  }
}

/* Async I/O as a service test with NUM_COMPONENTS compute components and
 * 1 I/O comm. The test writes data into two files - all info/data related
 * to the first file is provided by the 1st compute comm and all info/data
 * related to the second file is provided by the 2nd compute comm. Both
 * compute comms share a single I/O comm.
 *
 * Mirrors tests/functional/pio_async_decomp_tests.cpp's async-I/O
 * architecture (split the world comm into 1 I/O sub-comm + NUM_COMPONENTS
 * compute sub-comms via split_world_ncomms(), then call
 * PIOc_init_intercomm() on every rank).
 *
 * This test manages its own asynchronous I/O systems directly via
 * PIOc_init_intercomm()/PIOc_finalize() rather than via
 * Test_framework::init_iosystem()/get_iosysid()/finalize_iosystem(), since
 * those only support the single synchronous iosystem case
 * (PIOc_Init_Intracomm()). main() below correspondingly never calls
 * init_iosystem()/finalize_iosystem() for this file.
 *
 * Once ranks diverge into I/O vs. compute roles below, no code in this
 * function may call check_ret()/passert()/check_val()
 * (tests/util/spio_test_check.hpp) - those perform an MPI_Allreduce() over
 * the full test communicator (tf.get_comm()), but the I/O rank is blocked
 * inside PIOc_init_intercomm() until every compute rank has called
 * PIOc_finalize(), so it cannot reach such a collective until after that
 * point. Errors in the diverged region are therefore accumulated locally
 * (nerrs) and reduced by Test_framework::run()'s own post-call
 * MPI_Reduce() instead, except for check_ret_over_comm() calls, which
 * reduce only over the calling compute sub-comm (never including the I/O
 * rank) and are therefore deadlock-safe even before PIOc_finalize().
 *
 * The final MPI_Comm_free()/check_ret() pair below runs after every
 * compute rank has called PIOc_finalize() for all components, at which
 * point the I/O rank has also returned from PIOc_init_intercomm() and
 * rejoined tf.get_comm() collectives, so it is again safe to use
 * check_ret() (which reduces over tf.get_comm()) for that single call.
 *
 * See tests/general/pio_async_decomp_tests_1d.F90.in lines 107-259
 * (async_2comp_wr_1d) for the source being ported. */
template<typename T>
int test_async_2comp_wr_1d(SPIO_TF::Test_framework &tf)
{
  const int NUM_COMPONENTS = 2;
  const int NCOMMS = NUM_COMPONENTS + 1;
  const int MIN_NUM_PROCS = NUM_COMPONENTS + 1;
  const int VEC_LOCAL_SZ = 7;
  const int NUM_DIMS = 1;
  const int NUM_REARRANGERS = 2;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};
  const std::string dimname("PIO_TF_test_dim");
  const std::string vname("PIO_TF_test_var");
  const std::string fnames[NUM_COMPONENTS] = {
    "pio_async_decomp_tests_file_comp1.nc",
    "pio_async_decomp_tests_file_comp2.nc"
  };

  int nerrs = 0;

  for(int i = 0; i < NUM_REARRANGERS; i++){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing rearr : ") + rearrs_info[i] + "\n");

    if(tf.get_comm_size() < MIN_NUM_PROCS){
      continue;
    }

    MPI_Comm new_comm = MPI_COMM_NULL;
    int new_rank = 0, new_size = 0, gcomm_idx = 0;
    split_world_ncomms(tf, NCOMMS, new_comm, new_rank, new_size, gcomm_idx);

    std::vector<MPI_Comm> comp_comms(NUM_COMPONENTS, MPI_COMM_NULL);
    std::vector<int> iosysids(NUM_COMPONENTS, -1);

    bool is_io = (gcomm_idx == 1);
    int comp_idx = gcomm_idx - 2;

    if(is_io){
      MPI_Comm io_comm = new_comm;
      int ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
              io_comm, rearrs[i], iosysids.data());
      if(ret != PIO_NOERR){
        tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_init_intercomm() FAILED on I/O process, ret = " + std::to_string(ret) + "\n");
        nerrs++;
      }
    }
    else{
      comp_comms[comp_idx] = new_comm;
      MPI_Comm io_comm = MPI_COMM_NULL;
      int ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
              io_comm, rearrs[i], iosysids.data());
      if(ret != PIO_NOERR){
        tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_init_intercomm() FAILED on compute process, ret = " + std::to_string(ret) + "\n");
        nerrs++;
      }
      else{
        int dims[NUM_DIMS];
        dims[0] = VEC_LOCAL_SZ * new_size;

        std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
        for(int k = 0; k < VEC_LOCAL_SZ; k++){
          compdof[k] = VEC_LOCAL_SZ * new_rank + (k + 1);
        }

        std::vector<T> wbuf(VEC_LOCAL_SZ, static_cast<T>(tf.get_comm_rank()));
        std::vector<T> rbuf(VEC_LOCAL_SZ);

        int ioid = -1;
        ret = PIOc_InitDecomp(iosysids[comp_idx], Pio_type_map<T>::value, NUM_DIMS, dims,
                VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
        if(ret != PIO_NOERR){
          tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
          nerrs++;
        }
        else{
          int num_flavors = 0;
          int flavors[SPIO_TEST_NUM_FLAVORS];
          get_iotypes(&num_flavors, flavors);

          for(int j = 0; j < num_flavors; j++){
            char iotype_name[PIO_MAX_NAME + 1];
            iotype_name[0] = '\0';
            get_iotype_name(flavors[j], iotype_name);
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                std::string("Testing : ") + iotype_name + "\n");

            int pio_file = -1, pio_dim = -1, pio_var = -1;

            ret = PIOc_createfile(iosysids[comp_idx], &pio_file, &flavors[j],
                    fnames[comp_idx].c_str(), PIO_CLOBBER);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not create file : " + fnames[comp_idx])){
              nerrs++;
              continue;
            }

            ret = PIOc_def_dim(pio_file, dimname.c_str(), dims[0], &pio_dim);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not define dim : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            ret = PIOc_def_var(pio_file, vname.c_str(), Pio_type_map<T>::value, NUM_DIMS, &pio_dim, &pio_var);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not define var : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            ret = PIOc_enddef(pio_file);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not enddef in file : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            ret = PIOc_write_darray(pio_file, pio_var, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
            if(!check_ret_over_comm(tf, ret, new_comm, "Write darray failed : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
            PIOc_closefile(pio_file);

            ret = PIOc_openfile(iosysids[comp_idx], &pio_file, &flavors[j],
                    fnames[comp_idx].c_str(), PIO_NOWRITE);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not reopen file : " + fnames[comp_idx])){
              nerrs++;
              continue;
            }

            ret = PIOc_inq_varid(pio_file, vname.c_str(), &pio_var);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not inq var : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }
#else
            ret = PIOc_sync(pio_file);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not sync file : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }
#endif

            std::fill(rbuf.begin(), rbuf.end(), static_cast<T>(-1));
            ret = PIOc_read_darray(pio_file, pio_var, ioid, VEC_LOCAL_SZ, rbuf.data());
            if(!check_ret_over_comm(tf, ret, new_comm, "Could read var : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            bool vals_match = true;
            for(int k = 0; k < VEC_LOCAL_SZ; k++){
              if(rbuf[k] != wbuf[k]){
                vals_match = false;
                break;
              }
            }
            if(!vals_match){
              tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                  "Got wrong val : " + fnames[comp_idx] + "\n");
              nerrs++;
            }

            PIOc_closefile(pio_file);
            PIOc_deletefile(iosysids[comp_idx], fnames[comp_idx].c_str());
          }

          int fret = PIOc_freedecomp(iosysids[comp_idx], ioid);
          if(fret != PIO_NOERR){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(fret) + "\n");
            nerrs++;
          }
        }

        for(int j = 0; j < NUM_COMPONENTS; j++){
          int fret = PIOc_finalize(iosysids[j]);
          if(fret != PIO_NOERR){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "ERROR: PIOc_finalize() FAILED for component " + std::to_string(j) + ", ret = " + std::to_string(fret) + "\n");
            nerrs++;
          }
        }
      }
    }

    int mpi_free_ret = MPI_SUCCESS;
    if(new_comm != MPI_COMM_NULL){
      mpi_free_ret = MPI_Comm_free(&new_comm);
    }
    if(!check_ret(tf, mpi_free_ret, "PIO_init()/Finalize() failed")){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Async I/O as a service test with NUM_COMPONENTS compute components and
 * 1 I/O comm, same as test_async_2comp_wr_1d() above but using a 1d block
 * decomposition with holes (get_1d_bc_with_holes_info()) instead of the
 * simple block decomposition, so count can legitimately be 0 on odd
 * new_rank compute procs.
 *
 * Shares the async architecture, the deadlock-avoidance rule for
 * check_ret_over_comm() vs. check_ret()/passert()/check_val(), and the
 * MPI_Comm_free()-then-check_ret() final-check nuance documented on
 * test_async_2comp_wr_1d() above.
 *
 * See tests/general/pio_async_decomp_tests_1d.F90.in lines 261-420
 * (async_2comp_wr_1d_with_holes) for the source being ported. */
template<typename T>
int test_async_2comp_wr_1d_with_holes(SPIO_TF::Test_framework &tf)
{
  const int NUM_COMPONENTS = 2;
  const int NCOMMS = NUM_COMPONENTS + 1;
  const int MIN_NUM_PROCS = NUM_COMPONENTS + 1;
  const int NUM_DIMS = 1;
  const int NUM_REARRANGERS = 2;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};
  const std::string dimname("PIO_TF_test_dim");
  const std::string vname("PIO_TF_test_var");
  const std::string fnames[NUM_COMPONENTS] = {
    "pio_async_decomp_tests_file_comp1.nc",
    "pio_async_decomp_tests_file_comp2.nc"
  };

  int nerrs = 0;

  for(int i = 0; i < NUM_REARRANGERS; i++){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing rearr : ") + rearrs_info[i] + "\n");

    if(tf.get_comm_size() < MIN_NUM_PROCS){
      continue;
    }

    MPI_Comm new_comm = MPI_COMM_NULL;
    int new_rank = 0, new_size = 0, gcomm_idx = 0;
    split_world_ncomms(tf, NCOMMS, new_comm, new_rank, new_size, gcomm_idx);

    std::vector<MPI_Comm> comp_comms(NUM_COMPONENTS, MPI_COMM_NULL);
    std::vector<int> iosysids(NUM_COMPONENTS, -1);

    bool is_io = (gcomm_idx == 1);
    int comp_idx = gcomm_idx - 2;

    if(is_io){
      MPI_Comm io_comm = new_comm;
      int ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
              io_comm, rearrs[i], iosysids.data());
      if(ret != PIO_NOERR){
        tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_init_intercomm() FAILED on I/O process, ret = " + std::to_string(ret) + "\n");
        nerrs++;
      }
    }
    else{
      comp_comms[comp_idx] = new_comm;
      MPI_Comm io_comm = MPI_COMM_NULL;
      int ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
              io_comm, rearrs[i], iosysids.data());
      if(ret != PIO_NOERR){
        tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_init_intercomm() FAILED on compute process, ret = " + std::to_string(ret) + "\n");
        nerrs++;
      }
      else{
        int dims[NUM_DIMS];
        PIO_Offset start = 0;
        int count = 0;
        get_1d_bc_with_holes_info(new_rank, new_size, dims[0], start, count, true);

        std::vector<PIO_Offset> compdof(count);
        std::vector<T> wbuf(count);
        std::vector<T> rbuf(count);
        for(int k = 0; k < count; k++){
          compdof[k] = static_cast<PIO_Offset>(start) + k;
          wbuf[k] = static_cast<T>(compdof[k]);
        }

        int ioid = -1;
        ret = PIOc_InitDecomp(iosysids[comp_idx], Pio_type_map<T>::value, NUM_DIMS, dims,
                count, compdof.data(), &ioid, NULL, NULL, NULL);
        if(ret != PIO_NOERR){
          tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
          nerrs++;
        }
        else{
          int num_flavors = 0;
          int flavors[SPIO_TEST_NUM_FLAVORS];
          get_iotypes(&num_flavors, flavors);

          for(int j = 0; j < num_flavors; j++){
            char iotype_name[PIO_MAX_NAME + 1];
            iotype_name[0] = '\0';
            get_iotype_name(flavors[j], iotype_name);
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                std::string("Testing : ") + iotype_name + "\n");

            int pio_file = -1, pio_dim = -1, pio_var = -1;

            ret = PIOc_createfile(iosysids[comp_idx], &pio_file, &flavors[j],
                    fnames[comp_idx].c_str(), PIO_CLOBBER);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not create file : " + fnames[comp_idx])){
              nerrs++;
              continue;
            }

            ret = PIOc_def_dim(pio_file, dimname.c_str(), dims[0], &pio_dim);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not define dim : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            ret = PIOc_def_var(pio_file, vname.c_str(), Pio_type_map<T>::value, NUM_DIMS, &pio_dim, &pio_var);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not define var : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            ret = PIOc_enddef(pio_file);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not enddef in file : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            ret = PIOc_write_darray(pio_file, pio_var, ioid, count, wbuf.data(), NULL);
            if(!check_ret_over_comm(tf, ret, new_comm, "Write darray failed : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
            PIOc_closefile(pio_file);

            ret = PIOc_openfile(iosysids[comp_idx], &pio_file, &flavors[j],
                    fnames[comp_idx].c_str(), PIO_NOWRITE);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not reopen file : " + fnames[comp_idx])){
              nerrs++;
              continue;
            }

            ret = PIOc_inq_varid(pio_file, vname.c_str(), &pio_var);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not inq var : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }
#else
            ret = PIOc_sync(pio_file);
            if(!check_ret_over_comm(tf, ret, new_comm, "Could not sync file : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }
#endif

            std::fill(rbuf.begin(), rbuf.end(), static_cast<T>(-1));
            ret = PIOc_read_darray(pio_file, pio_var, ioid, count, rbuf.data());
            if(!check_ret_over_comm(tf, ret, new_comm, "Could read var : " + fnames[comp_idx])){
              nerrs++;
              PIOc_closefile(pio_file);
              continue;
            }

            bool vals_match = true;
            for(int k = 0; k < count; k++){
              if(rbuf[k] != wbuf[k]){
                vals_match = false;
                break;
              }
            }
            if(!vals_match){
              tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                  "Got wrong val : " + fnames[comp_idx] + "\n");
              nerrs++;
            }

            PIOc_closefile(pio_file);
            PIOc_deletefile(iosysids[comp_idx], fnames[comp_idx].c_str());
          }

          int fret = PIOc_freedecomp(iosysids[comp_idx], ioid);
          if(fret != PIO_NOERR){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(fret) + "\n");
            nerrs++;
          }
        }

        for(int j = 0; j < NUM_COMPONENTS; j++){
          int fret = PIOc_finalize(iosysids[j]);
          if(fret != PIO_NOERR){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "ERROR: PIOc_finalize() FAILED for component " + std::to_string(j) + ", ret = " + std::to_string(fret) + "\n");
            nerrs++;
          }
        }
      }
    }

    int mpi_free_ret = MPI_SUCCESS;
    if(new_comm != MPI_COMM_NULL){
      mpi_free_ret = MPI_Comm_free(&new_comm);
    }
    if(!check_ret(tf, mpi_free_ret, "PIO_init()/Finalize() failed")){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
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
    {"test_async_2comp_wr_1d_int", test_async_2comp_wr_1d<int>},
    {"test_async_2comp_wr_1d_float", test_async_2comp_wr_1d<float>},
    {"test_async_2comp_wr_1d_double", test_async_2comp_wr_1d<double>},
    {"test_async_2comp_wr_1d_with_holes_int", test_async_2comp_wr_1d_with_holes<int>},
    {"test_async_2comp_wr_1d_with_holes_float", test_async_2comp_wr_1d_with_holes<float>},
    {"test_async_2comp_wr_1d_with_holes_double", test_async_2comp_wr_1d_with_holes<double>}
  };

  SPIO_TF::Test_framework tf("pio_async_decomp_tests_1d_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  /* This test manages its own async iosystems per registered function
   * (see test_async_2comp_wr_1d()/test_async_2comp_wr_1d_with_holes()
   * above), so init_iosystem()/finalize_iosystem() are intentionally not
   * used here. */
  nerrs += tf.run();

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
