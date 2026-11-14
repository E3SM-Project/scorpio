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

/* Split tf.get_comm() (the test's world communicator) into ncomms disjoint
 * sub-communicators of as-equal-as-possible size. The last sub-comm
 * absorbs any remainder ranks. gcomm_idx is the 1-based index (1..ncomms)
 * of the sub-comm this rank lands in, mirroring
 * tests/general/pio_async_decomp_tests.F90.in's split_world_ncomms(). */
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

/* Async I/O as a service test with NUM_COMPONENTS compute components and
 * 1 I/O comm. Creates a 1d decomposition on each compute component.
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
 * MPI_Reduce() instead. */
template<typename T>
int test_async_2comp_iodesc_1d(SPIO_TF::Test_framework &tf)
{
  const int NUM_COMPONENTS = 2;
  const int NCOMMS = NUM_COMPONENTS + 1;
  const int MIN_NUM_PROCS = NUM_COMPONENTS + 1;
  const int VEC_LOCAL_SZ = 7;
  const int NUM_DIMS = 1;
  const int NUM_REARRANGERS = 2;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};

  int nerrs = 0;

  for(int i = 0; i < NUM_REARRANGERS; i++){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing rearr : ") + rearrs_info[i] + "\n");

    if(tf.get_comm_size() < MIN_NUM_PROCS){
      continue;
    }

    /* Create NCOMMS disjoint sets of procs: 1 I/O comm + NUM_COMPONENTS
     * compute comms */
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
        /* Only compute procs send data */
        int dims[NUM_DIMS];
        dims[0] = VEC_LOCAL_SZ * new_size;

        std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
        for(int k = 0; k < VEC_LOCAL_SZ; k++){
          compdof[k] = VEC_LOCAL_SZ * new_rank + (k + 1);
        }

        int ioid = -1;
        ret = PIOc_InitDecomp(iosysids[comp_idx], Pio_type_map<T>::value, NUM_DIMS, dims,
                VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
        if(ret != PIO_NOERR){
          tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
              "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
          nerrs++;
        }
        else{
          ret = PIOc_freedecomp(iosysids[comp_idx], ioid);
          if(ret != PIO_NOERR){
            tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
                "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(ret) + "\n");
            nerrs++;
          }
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
    {"test_async_2comp_iodesc_1d_int", test_async_2comp_iodesc_1d<int>},
    {"test_async_2comp_iodesc_1d_float", test_async_2comp_iodesc_1d<float>},
    {"test_async_2comp_iodesc_1d_double", test_async_2comp_iodesc_1d<double>}
  };

  SPIO_TF::Test_framework tf("pio_async_decomp_tests_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  /* This test manages its own async iosystems per registered function
   * (see test_async_2comp_iodesc_1d() above), so init_iosystem()/
   * finalize_iosystem() are intentionally not used here. */
  nerrs += tf.run();

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
