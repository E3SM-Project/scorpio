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

int test_file_write(SPIO_TF::Test_framework &tf, int iotype, const std::string &iotype_name)
{
  const int VEC_LOCAL_SZ = 7;
  const std::string VNAME("PIO_TF_test_var");
  const std::string filename("test_pio_hints.testfile");

  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  std::vector<int> wbuf(VEC_LOCAL_SZ), rbuf(VEC_LOCAL_SZ, -1);
  int dims[1];

  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
    wbuf[i] = tf.get_comm_rank();
  }
  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();

  int ioid = -1;
  int ret = PIOc_InitDecomp(tf.get_iosysid(), PIO_INT, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      std::string("Testing : pio_set_hint : ") + iotype_name + "\n");

  int ncid = -1, dimid = -1, varid = -1;
  ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Could not create " + filename)){
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
  if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
    PIOc_closefile(ncid);
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VNAME.c_str(), PIO_INT, 1, &dimid, &varid);
  if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
    PIOc_closefile(ncid);
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to end define mode : " + filename)){
    PIOc_closefile(ncid);
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_write_darray(ncid, varid, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
  if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
    PIOc_closefile(ncid);
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  PIOc_closefile(ncid);

  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Could not reopen file " + filename)){
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VNAME.c_str(), &varid);
  if(!check_ret(tf, ret, "Could not inq var : " + filename)){
    PIOc_closefile(ncid);
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }
#else
  ret = PIOc_sync(ncid);
  if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
    PIOc_closefile(ncid);
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }
#endif

  ret = PIOc_read_darray(ncid, varid, ioid, VEC_LOCAL_SZ, rbuf.data());
  if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
    PIOc_closefile(ncid);
    PIOc_freedecomp(tf.get_iosysid(), ioid);
    return PIO_EINTERNAL;
  }

  bool ok = check_val(tf, rbuf, wbuf, "Got wrong val : " + filename);

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), filename.c_str());

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int pio_set_hint_driver(SPIO_TF::Test_framework &tf)
{
  /* Set the Scorpio hints. If the hints are not processed by Scorpio
   * they are set as MPI hints, allowing users to set hints for low level
   * libraries like PnetCDF. Return values are intentionally not checked
   * (matching the Fortran reference) -- these are library/filesystem
   * specific and may be silently ignored. */

  /* Set striping factor for Lustre file systems to 8 (file striped across 8 OSTs) */
  PIOc_set_hint(tf.get_iosysid(), "striping_factor", "8");

  /* Set striping unit for Lustre file systems to 1KB (each file stripe is 1KB) */
  PIOc_set_hint(tf.get_iosysid(), "striping_unit", "1024");

  PIOc_set_hint(tf.get_iosysid(), "spio_test_hint1", "spio_test_hint1_val");

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);

    if(test_file_write(tf, flavors[i], iotype_name) != PIO_NOERR){
      nerrs++;
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
    {"pio_set_hint_driver", pio_set_hint_driver}
  };

  SPIO_TF::Test_framework tf("pio_set_hint_cxx", MPI_COMM_WORLD, rfuncs);
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
