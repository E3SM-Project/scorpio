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

int fail_rank_even(SPIO_TF::Test_framework &tf)
{
  bool cond = (tf.get_comm_rank() % 2 != 0);

  if(!passert(tf, cond, "Hard assert")){
    return PIO_EINTERNAL;
  }
  return PIO_NOERR;
}

int always_fail_with_error(SPIO_TF::Test_framework &tf)
{
  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "Fatal Error\n");
  return PIO_EINTERNAL;
}

int always_success(SPIO_TF::Test_framework &tf)
{
  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      "This test will always succeed\n");
  return PIO_NOERR;
}

int always_fail_with_passert(SPIO_TF::Test_framework &tf)
{
  if(!passert(tf, false, "Fatal Assert")){
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
    {"fail_rank_even", fail_rank_even},
    {"always_fail_with_error", always_fail_with_error},
    {"always_success", always_success},
    {"always_fail_with_passert", always_fail_with_passert}
  };

  SPIO_TF::Test_framework tf("pio_fail", MPI_COMM_WORLD, rfuncs);
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
  /* This test deliberately exercises the framework's own fail paths:
   * returning a nonzero process exit status here (reflecting the expected
   * failures above) is correct, not a bug -- CTest below is marked
   * WILLFAIL to match.
   */
  return nerrs;
}
