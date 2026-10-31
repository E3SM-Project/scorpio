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
 * tests/general/pio_async_file_tests.F90.in's split_world_ncomms(). */
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

/* Async I/O as a service test with NUM_COMPONENTS compute components and
 * 1 I/O comm. Creates a file, writes out the header (dim + var, no data),
 * closes it, reopens it and reads back the header info.
 *
 * Mirrors tests/functional/pio_async_decomp_tests.cpp's async-I/O
 * architecture: manages its own iosystems via PIOc_init_intercomm()/
 * PIOc_finalize() rather than Test_framework::init_iosystem(), so main()
 * below never calls init_iosystem()/finalize_iosystem() for this file.
 *
 * Once ranks diverge into I/O vs. compute roles, check_ret()/passert()/
 * check_val() (tests/util/spio_test_check.hpp) remain off-limits for the
 * same deadlock reason as pio_async_decomp_tests.cpp. Return-code checks
 * on PIO calls instead use check_ret_over_comm() above, scoped to this
 * component's own new_comm - mirroring the Fortran reference's own
 * PIO_TF_CHECK_ERR(ret, new_comm, ...) three-argument form. The
 * inquiry-result checks (var name/type/ndims) mirror the Fortran's
 * PIO_TF_ASSERT, which performs no cross-rank communication at all
 * (confirmed in tests/general/util/pio_tf_f90gen.pl's PIO_TF_ASSERT macro
 * expansion, lines 404-413) - so those accumulate into nerrs locally. */
template<typename T>
int test_async_nc_inq_var(SPIO_TF::Test_framework &tf)
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
  /* Literal names copied verbatim from the Fortran reference, which
   * itself reuses pio_async_decomp_tests.F90.in's file names here -
   * preserved as-is rather than "fixed" to this file's own name. */
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

    /* Use the first sub-comm as the I/O comm */
    bool is_io = (gcomm_idx == 1);

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
      /* 0-based index of this compute component in comp_comms[]/iosysids[] */
      int comp_idx = gcomm_idx - 2;
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
        /* Only compute procs send data; dims[0] is the full (non-decomposed)
         * extent across every rank in this compute component */
        int dims[NUM_DIMS];
        dims[0] = VEC_LOCAL_SZ * new_size;

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

          char rd_vname[PIO_MAX_NAME + 1];
          rd_vname[0] = '\0';
          ret = PIOc_inq_varname(pio_file, pio_var, rd_vname, PIO_MAX_NAME + 1);
          if((ret != PIO_NOERR) || (vname != std::string(rd_vname))){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "Variable name is not the expected value\n");
            nerrs++;
          }

          int rd_vtype = -1;
          ret = PIOc_inq_vartype(pio_file, pio_var, &rd_vtype);
          if((ret != PIO_NOERR) || (rd_vtype != Pio_type_map<T>::value)){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "Variable type is not the expected type\n");
            nerrs++;
          }

          int rd_ndims = -1;
          ret = PIOc_inq_varndims(pio_file, pio_var, &rd_ndims);
          if((ret != PIO_NOERR) || (rd_ndims != NUM_DIMS)){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "Number of dims in the variable is not the expected value\n");
            nerrs++;
          }

          PIOc_closefile(pio_file);
          PIOc_deletefile(iosysids[comp_idx], fnames[comp_idx].c_str());
        }
      }

      /* Only compute procs call PIO function calls; the I/O proc waits
       * inside PIOc_init_intercomm() for commands from the compute
       * processes and only returns once every component has finalized */
      for(int j = 0; j < NUM_COMPONENTS; j++){
        int fret = PIOc_finalize(iosysids[j]);
        if(fret != PIO_NOERR){
          tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: PIOc_finalize() FAILED for component " + std::to_string(j) + ", ret = " + std::to_string(fret) + "\n");
          nerrs++;
        }
      }
    }

    if(new_comm != MPI_COMM_NULL){
      MPI_Comm_free(&new_comm);
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
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
    {"test_async_nc_inq_var_int", test_async_nc_inq_var<int>},
    {"test_async_nc_inq_var_float", test_async_nc_inq_var<float>},
    {"test_async_nc_inq_var_double", test_async_nc_inq_var<double>}
  };

  SPIO_TF::Test_framework tf("pio_async_file_tests_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  /* This test manages its own async iosystems per registered function
   * (see test_async_nc_inq_var() above), so init_iosystem()/
   * finalize_iosystem() are intentionally not used here. */
  nerrs += tf.run();

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
