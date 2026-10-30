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

int create_file_always_fail(SPIO_TF::Test_framework &tf, int iotype, const std::string &filename)
{
  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_CLOBBER);
  if(!passert(tf, ret != PIO_NOERR, "PIOc_createfile did not fail as expected")){
    return PIO_EINTERNAL;
  }
  return PIO_NOERR;
}

int open_file_always_fail(SPIO_TF::Test_framework &tf, int iotype, const std::string &filename)
{
  int ncid = -1;
  int ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_NOWRITE);
  if(!passert(tf, ret != PIO_NOERR, "PIOc_openfile did not fail as expected")){
    return PIO_EINTERNAL;
  }
  return PIO_NOERR;
}

int file_fail_driver(SPIO_TF::Test_framework &tf)
{
  int num_uiotypes = 0;
  int uiotypes[SPIO_TEST_NUM_ALL_IOTYPES];
  get_undef_iotypes(&num_uiotypes, uiotypes);

  const std::string filename("test_pio_file_fail.testfile");
  int nerrs = 0;

  for(int i = 0; i < num_uiotypes; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_undef_iotype_name(uiotypes[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : file_fail : ") + iotype_name + "\n");

    if(create_file_always_fail(tf, uiotypes[i], filename) != PIO_NOERR) { nerrs++; }
    if(open_file_always_fail(tf, uiotypes[i], filename) != PIO_NOERR) { nerrs++; }
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
    {"file_fail_driver", file_fail_driver}
  };

  SPIO_TF::Test_framework tf("pio_file_fail_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  if(tf.init_iosystem(PIO_REARR_BOX) == PIO_NOERR){
    // Set error handler to avoid MPI_Abort on invalid iotype
    int old_handler = 0;
    PIOc_set_iosystem_error_handling(tf.get_iosysid(), PIO_BCAST_ERROR, &old_handler);

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
