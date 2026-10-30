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

int create_file_no_opts(SPIO_TF::Test_framework &tf, int iotype, const std::string &filename)
{
  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Could not create " + filename)){
    return PIO_EINTERNAL;
  }

  if(iotype != PIO_IOTYPE_PNETCDF){
    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Could not end define mode: " + filename)){
      return PIO_EINTERNAL;
    }
  }

  PIOc_closefile(ncid);
  return PIO_NOERR;
}

int open_file_no_write(SPIO_TF::Test_framework &tf, int iotype, const std::string &filename)
{
  int ncid = -1;
  int ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Could not open " + filename)){
    return PIO_EINTERNAL;
  }

  PIOc_closefile(ncid);
  return PIO_NOERR;
}

int delete_file(SPIO_TF::Test_framework &tf, const std::string &filename)
{
  PIOc_deletefile(tf.get_iosysid(), filename.c_str());
  return PIO_NOERR;
}

int file_simple_tests_driver(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_file_simple_tests.testfile");
  int nerrs = 0;

  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : file_simple_tests : ") + iotype_name + "\n");

    if(create_file_no_opts(tf, flavors[i], filename) != PIO_NOERR) { nerrs++; continue; }
    if(open_file_no_write(tf, flavors[i], filename) != PIO_NOERR) { nerrs++; continue; }
    if(delete_file(tf, filename) != PIO_NOERR) { nerrs++; }
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
    {"file_simple_tests_driver", file_simple_tests_driver}
  };

  SPIO_TF::Test_framework tf("pio_file_simple_tests_cxx", MPI_COMM_WORLD, rfuncs);
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
