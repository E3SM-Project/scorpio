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

int create_many_decomps(SPIO_TF::Test_framework &tf)
{
  const int VEC_LOCAL_SZ = 7;
  const int MAX_DECOMPS = 64 * 1024 + 1;
  int dims[1];
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  for(int i = 0; i < MAX_DECOMPS; i++){
    int ioid = -1;
    int ret = PIOc_InitDecomp(tf.get_iosysid(), PIO_INT, 1, dims,
            VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
    if(ret != PIO_NOERR){
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
    if(ret != PIO_NOERR){
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }
  }

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
    {"create_many_decomps", create_many_decomps}
  };

  SPIO_TF::Test_framework tf("pio_decomp_large_tests_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  if(tf.init_iosystem(PIO_REARR_BOX) == PIO_NOERR){
    nerrs += tf.run();
    tf.finalize_iosystem();
  }
  else{
    nerrs++;
  }

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
