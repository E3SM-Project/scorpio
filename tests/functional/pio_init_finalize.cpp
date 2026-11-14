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

int init_finalize(SPIO_TF::Test_framework &tf)
{
  int major = 0, minor = 0, patch = 0;
  char vstr[PIO_MAX_NAME + 1] = {'\0'};
  char hstr[PIO_MAX_NAME + 1] = {'\0'};
  int ret = PIO_NOERR;

  ret = PIOc_get_version(&major, &minor, &patch, vstr, PIO_MAX_NAME);
  if(!check_ret(tf, ret, "Unable to retrieve version info")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_get_version_hash(hstr, PIO_MAX_NAME);
  if(!check_ret(tf, ret, "Unable to retrieve version hash")){
    return PIO_EINTERNAL;
  }

  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      std::string("SCORPIO : MAJOR VERSION : ") + std::to_string(major) +
      ", MINOR VERSION : " + std::to_string(minor) +
      ", PATCH VERSION : " + std::to_string(patch) +
      ", VERSION STRING : '" + vstr + "'" +
      ", VERSION HASH : '" + hstr + "'\n");

  return PIO_NOERR;
}

int set_debug_level(SPIO_TF::Test_framework &tf)
{
  const int MIN_DBG_LVL = 0;
  const int MAX_DBG_LVL = 5;

  for(int lvl = MIN_DBG_LVL; lvl <= MAX_DBG_LVL; lvl++){
    int ret = PIOc_set_log_level(lvl);
    if(!check_ret(tf, ret, "Setting debug level failed")){
      return PIO_EINTERNAL;
    }
  }
  PIOc_set_log_level(MIN_DBG_LVL);

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
    {"init_finalize", init_finalize},
    {"set_debug_level", set_debug_level}
  };

  SPIO_TF::Test_framework tf("pio_init_finalize_cxx", MPI_COMM_WORLD, rfuncs);
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
