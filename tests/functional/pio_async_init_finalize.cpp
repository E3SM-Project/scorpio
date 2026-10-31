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

/* Split tf.get_comm() into an odd-ranks sub-communicator and an even-ranks
 * sub-communicator - every rank ends up in exactly one of the two, both
 * non-null. Mirrors tests/general/pio_async_init_finalize.F90.in's
 * split_world_odd_even(). */
static void split_world_odd_even(const SPIO_TF::Test_framework &tf,
    MPI_Comm &new_comm, int &new_rank, int &new_size, bool &is_even)
{
  new_comm = MPI_COMM_NULL;
  new_rank = 0;
  new_size = 0;

  int color = 0;

  if((tf.get_comm_rank() % 2) == 0){
    is_even = true;
    color = 1;
  }
  else{
    is_even = false;
    color = 0;
  }

  MPI_Comm_split(tf.get_comm(), color, 0, &new_comm);

  MPI_Comm_rank(new_comm, &new_rank);
  MPI_Comm_size(new_comm, &new_size);
}

/* Split tf.get_comm() (the test's world communicator) into ncomms disjoint
 * sub-communicators of as-equal-as-possible size. The last sub-comm
 * absorbs any remainder ranks. gcomm_idx is the 1-based index (1..ncomms)
 * of the sub-comm this rank lands in, mirroring
 * tests/general/pio_async_init_finalize.F90.in's split_world_ncomms(). */
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

/* Async I/O as a service test with 1 compute comm and an I/O comm */
int async_1comp_init_finalize(SPIO_TF::Test_framework &tf)
{
  const int NUM_COMPONENTS = 1;
  const int NUM_REARRANGERS = 2;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};

  int nerrs = 0;

  for(int i = 0; i < NUM_REARRANGERS; i++){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing rearr : ") + rearrs_info[i] + "\n");

    if(tf.get_comm_size() > 1){
      /* Create two disjoint sets of IO procs and compute procs */
      MPI_Comm odd_even_comm = MPI_COMM_NULL;
      int rank = 0, sz = 0;
      bool is_even = false;
      split_world_odd_even(tf, odd_even_comm, rank, sz, is_even);

      MPI_Comm comp_comms[NUM_COMPONENTS];
      MPI_Comm io_comm = MPI_COMM_NULL;
      int iosysids[NUM_COMPONENTS];
      int ret = PIO_NOERR;

      /* All even procs are compute procs and all odd procs are io procs */
      if(is_even){
        /* Compute proc */
        comp_comms[0] = odd_even_comm;
        io_comm = MPI_COMM_NULL;
        ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms, io_comm,
                rearrs[i], iosysids);
      }
      else{
        /* I/O proc */
        comp_comms[0] = MPI_COMM_NULL;
        io_comm = odd_even_comm;
        ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms, io_comm,
                rearrs[i], iosysids);
      }

      /* Only compute procs call PIO function calls, I/O proc waits inside
       * PIOc_init_intercomm() waiting for commands from the compute
       * processes */
      if(is_even){
        ret = PIOc_finalize(iosysids[0]);
      }
      MPI_Comm_free(&odd_even_comm);
      if(!check_ret(tf, ret, "PIO_init()/Finalize() failed")){
        nerrs++;
      }
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Async I/O as a service test with 1 compute comm and an I/O comm.
 * This test uses a different interface (from the test above) to init the
 * iosystems. Note: the C API only exposes a single PIOc_init_intercomm()
 * entry point (there is no separate "v2" C entry point mirroring the
 * Fortran PIO_init overloads), so this function is byte-for-byte identical
 * to async_1comp_init_finalize() above. */
int async_1comp_init_finalize_v2(SPIO_TF::Test_framework &tf)
{
  const int NUM_COMPONENTS = 1;
  const int NUM_REARRANGERS = 2;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};

  int nerrs = 0;

  for(int i = 0; i < NUM_REARRANGERS; i++){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing rearr : ") + rearrs_info[i] + "\n");

    if(tf.get_comm_size() > 1){
      /* Create two disjoint sets of IO procs and compute procs */
      MPI_Comm odd_even_comm = MPI_COMM_NULL;
      int rank = 0, sz = 0;
      bool is_even = false;
      split_world_odd_even(tf, odd_even_comm, rank, sz, is_even);

      MPI_Comm comp_comms[NUM_COMPONENTS];
      MPI_Comm io_comm = MPI_COMM_NULL;
      int iosysids[NUM_COMPONENTS];
      int ret = PIO_NOERR;

      /* All even procs are compute procs and all odd procs are io procs */
      if(is_even){
        /* Compute proc */
        comp_comms[0] = odd_even_comm;
        io_comm = MPI_COMM_NULL;
        ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms, io_comm,
                rearrs[i], iosysids);
      }
      else{
        /* I/O proc */
        comp_comms[0] = MPI_COMM_NULL;
        io_comm = odd_even_comm;
        ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms, io_comm,
                rearrs[i], iosysids);
      }

      /* Only compute procs call PIO function calls, I/O proc waits inside
       * PIOc_init_intercomm() waiting for commands from the compute
       * processes */
      if(is_even){
        ret = PIOc_finalize(iosysids[0]);
      }
      MPI_Comm_free(&odd_even_comm);
      if(!check_ret(tf, ret, "PIO_init()/Finalize() failed")){
        nerrs++;
      }
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Async I/O as a service test with 1 compute comm and an I/O comm.
 * Multiple calls to PIOc_init_intercomm() with alternating compute and I/O
 * procs. */
int async_loop_init_finalize(SPIO_TF::Test_framework &tf)
{
  const int NUM_LOOPS = 5;
  const int NUM_COMPONENTS = 1;
  const int NUM_REARRANGERS = 2;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};

  int nerrs = 0;

  for(int k = 1; k <= NUM_LOOPS; k++){
    for(int i = 0; i < NUM_REARRANGERS; i++){
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
          std::string("Testing rearr : ") + rearrs_info[i] + "\n");

      if(tf.get_comm_size() > 1){
        /* Create two disjoint sets of IO procs and compute procs */
        MPI_Comm odd_even_comm = MPI_COMM_NULL;
        int rank = 0, sz = 0;
        bool is_even = false;
        split_world_odd_even(tf, odd_even_comm, rank, sz, is_even);

        /* On even counts : All even procs are compute procs and all odd
         *                  procs are io procs
         * On odd counts  : All odd procs are compute procs and all even
         *                  procs are io procs
         */
        bool is_compute = is_even;
        if((k % 2) != 0){
          is_compute = !is_even;
        }

        MPI_Comm comp_comms[NUM_COMPONENTS];
        MPI_Comm io_comm = MPI_COMM_NULL;
        int iosysids[NUM_COMPONENTS];
        int ret = PIO_NOERR;

        if(is_compute){
          /* Compute proc */
          comp_comms[0] = odd_even_comm;
          io_comm = MPI_COMM_NULL;
          ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms, io_comm,
                  rearrs[i], iosysids);
        }
        else{
          /* I/O proc */
          comp_comms[0] = MPI_COMM_NULL;
          io_comm = odd_even_comm;
          ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms, io_comm,
                  rearrs[i], iosysids);
        }

        /* Only compute procs call PIO function calls, I/O proc waits
         * inside PIOc_init_intercomm() waiting for commands from the
         * compute processes */
        if(is_compute){
          ret = PIOc_finalize(iosysids[0]);
        }
        MPI_Comm_free(&odd_even_comm);
        if(!check_ret(tf, ret, "PIO_init()/Finalize() failed")){
          nerrs++;
        }
      }
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Async I/O as a service test with 2 compute comms and 1 io comm */
int async_2comp_init_finalize(SPIO_TF::Test_framework &tf)
{
  const int NUM_COMPONENTS = 2;
  const int NCOMMS = NUM_COMPONENTS + 1;
  const int MIN_NUM_PROCS = NUM_COMPONENTS + 1;
  const int NUM_REARRANGERS = 2;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};

  int nerrs = 0;

  for(int i = 0; i < NUM_REARRANGERS; i++){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing rearr : ") + rearrs_info[i] + "\n");

    if(tf.get_comm_size() >= MIN_NUM_PROCS){
      /* Create three disjoint sets of IO procs and compute procs */
      MPI_Comm new_comm = MPI_COMM_NULL;
      int new_rank = 0, new_size = 0, gcomm_idx = 0;
      split_world_ncomms(tf, NCOMMS, new_comm, new_rank, new_size, gcomm_idx);

      std::vector<MPI_Comm> comp_comms(NUM_COMPONENTS, MPI_COMM_NULL);
      std::vector<int> iosysids(NUM_COMPONENTS, -1);

      /* Use first comm as the IO proc */
      bool is_io = (gcomm_idx == 1);

      MPI_Comm io_comm = MPI_COMM_NULL;
      int ret = PIO_NOERR;

      if(is_io){
        /* I/O proc */
        io_comm = new_comm;
        ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
                io_comm, rearrs[i], iosysids.data());
      }
      else{
        /* Compute proc */
        int comp_idx = gcomm_idx - 2;
        comp_comms[comp_idx] = new_comm;
        io_comm = MPI_COMM_NULL;
        ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
                io_comm, rearrs[i], iosysids.data());
      }

      /* Only compute procs call PIO function calls, I/O proc waits inside
       * PIOc_init_intercomm() waiting for commands from the compute
       * processes */
      if(!is_io){
        for(int j = 0; j < NUM_COMPONENTS; j++){
          ret = PIOc_finalize(iosysids[j]);
        }
      }
      if(new_comm != MPI_COMM_NULL){
        MPI_Comm_free(&new_comm);
      }
      if(!check_ret(tf, ret, "PIO_init()/Finalize() failed")){
        nerrs++;
      }
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
    {"async_1comp_init_finalize", async_1comp_init_finalize},
    {"async_1comp_init_finalize_v2", async_1comp_init_finalize_v2},
    {"async_loop_init_finalize", async_loop_init_finalize},
    {"async_2comp_init_finalize", async_2comp_init_finalize}
  };

  SPIO_TF::Test_framework tf("pio_async_init_finalize_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  /* This test manages its own async iosystems per registered function,
   * so init_iosystem()/finalize_iosystem() are intentionally not used
   * here. */
  nerrs += tf.run();

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
