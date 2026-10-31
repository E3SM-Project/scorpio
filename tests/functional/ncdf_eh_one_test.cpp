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

int test_clob_then_no_clob(SPIO_TF::Test_framework &tf, int iotype, const std::string &clob_fname)
{
  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, clob_fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + clob_fname)){
    return PIO_EINTERNAL;
  }
  PIOc_closefile(ncid);

  int cur_err_handler = PIO_BCAST_ERROR;
  PIOc_set_iosystem_error_handling(tf.get_iosysid(), PIO_BCAST_ERROR, &cur_err_handler);
  PIOc_set_iosystem_error_handling(tf.get_iosysid(), cur_err_handler, NULL);

  ncid = -1;
  ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, clob_fname.c_str(), PIO_NOCLOBBER);
  if(cur_err_handler == PIO_RETURN_ERROR){
    /* Under PIO_RETURN_ERROR, only I/O-task ranks see the real error code
     * -- other ranks may see PIO_NOERR. Reduce to the most severe (most
     * negative) code across all ranks before asserting, exactly like the
     * Fortran source does. passert/check_ret (MPI_LAND over a bool) is
     * the wrong primitive here -- this needs the raw code reduced first.
     */
    int reduced_ret = ret;
    int mpierr = MPI_Allreduce(MPI_IN_PLACE, &reduced_ret, 1, MPI_INT, MPI_MIN, tf.get_comm());
    if(!check_ret(tf, (mpierr == MPI_SUCCESS) ? PIO_NOERR : PIO_EINTERNAL,
          "MPI Allreduce on return value failed (error handler = PIO_RETURN_ERROR)")){
      return PIO_EINTERNAL;
    }
    ret = reduced_ret;
  }
  bool failed_as_expected = passert(tf, ret != PIO_NOERR,
      "Create file with clobber then no clobber did not fail as expected");

  /* No close since createfile should fail */
  PIOc_deletefile(tf.get_iosysid(), clob_fname.c_str());

  return failed_as_expected ? PIO_NOERR : PIO_EINTERNAL;
}

int ncdf_eh_one_test_driver(SPIO_TF::Test_framework &tf)
{
  const int err_handlers[] = {PIO_BCAST_ERROR, PIO_REDUCE_ERROR, PIO_RETURN_ERROR};
  const char *err_handler_names[] = {"PIO_BCAST_ERROR", "PIO_REDUCE_ERROR", "PIO_RETURN_ERROR"};
  const std::string fname("pio_ncdf_test_file.nc");
  const std::string clob_fname("pio_clob_test_file.nc");

  int num_iotypes = 0;
  int iotypes[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_iotypes, iotypes);

  int nerrs = 0;

  for(std::size_t h = 0; h < sizeof(err_handlers) / sizeof(err_handlers[0]); h++){
    PIOc_set_iosystem_error_handling(tf.get_iosysid(), err_handlers[h], NULL);

    for(int i = 0; i < num_iotypes; i++){
      char iotype_name[PIO_MAX_NAME + 1];
      iotype_name[0] = '\0';
      get_iotype_name(iotypes[i], iotype_name);
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
          std::string("Testing : ncdf_eh_one_test : ") + err_handler_names[h] + " : " + iotype_name + "\n");

      int ncid = -1;
      int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(iotypes[i]), fname.c_str(), PIO_CLOBBER);
      if(!check_ret(tf, ret, "Failed to create:" + std::string(iotype_name) + ":" + fname)){
        nerrs++;
        continue;
      }
      PIOc_closefile(ncid);

      if(test_clob_then_no_clob(tf, iotypes[i], clob_fname) != PIO_NOERR) { nerrs++; }

      PIOc_deletefile(tf.get_iosysid(), fname.c_str());
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
    {"ncdf_eh_one_test_driver", ncdf_eh_one_test_driver}
  };

  SPIO_TF::Test_framework tf("ncdf_eh_one_test_cxx", MPI_COMM_WORLD, rfuncs);
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
