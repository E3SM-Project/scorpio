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

/* Shared by test_redef_with_no_write and test_redef_twice: the Fortran
 * driver (PIO_TF_TEST_DRIVER_BEGIN in ncdf_fail.F90.in) pre-creates this
 * file once per iotype and runs every sub-test against it. The C++ port
 * has no equivalent cross-function driver state, so each of the two
 * functions that need it creates, uses, and deletes it themselves. */
const std::string TGV_FNAME("pio_ncdf_test_file.nc");

static bool is_nc4_iotype(int iotype)
{
  return (iotype == PIO_IOTYPE_NETCDF4C) || (iotype == PIO_IOTYPE_NETCDF4P) ||
      (iotype == PIO_IOTYPE_NETCDF4P_NCZARR);
}

static int clob_then_no_clob_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string clob_fname("pio_clob_test_file.nc");

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, clob_fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + clob_fname)){
    return PIO_EINTERNAL;
  }
  PIOc_closefile(ncid);

  ncid = -1;
  ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, clob_fname.c_str(), PIO_NOCLOBBER);
  bool ok = passert(tf, ret != PIO_NOERR,
      "Create file with clobber then no clobber did not fail as expected");

  char errmsg[PIO_MAX_NAME + 1];
  errmsg[0] = '\0';
  int sret = PIOc_strerror(ret, errmsg, sizeof(errmsg));
  if(!check_ret(tf, sret,
        "Failed to get error message (Trying to recreate file with noclobber):" + clob_fname)){
    /* No close since createfile should fail */
    PIOc_deletefile(tf.get_iosysid(), clob_fname.c_str());
    return PIO_EINTERNAL;
  }

  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      std::string("Error message successfully retrieved (Trying to recreate file with noclobber), errmsg = ")
      + errmsg + "\n");

  /* No close since createfile should fail */
  PIOc_deletefile(tf.get_iosysid(), clob_fname.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_clob_then_no_clob(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : test_clob_then_no_clob : ") + iotype_name + "\n");

    if(clob_then_no_clob_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int redef_with_no_write_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }
  PIOc_closefile(ncid);

  bool ok = true;
  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to open:" + TGV_FNAME)){
    ok = false;
  }
  else{
    ret = PIOc_redef(ncid);
    ok = passert(tf, ret != PIO_NOERR, "Redef with nowrite did not fail as expected");

    /* No enddef because redef is expected to fail */
    PIOc_closefile(ncid);
  }

  PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_redef_with_no_write(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : test_redef_with_no_write : ") + iotype_name + "\n");

    if(redef_with_no_write_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int redef_twice_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  /* NetCDF4 iotypes do not require an explicit enddef before redef or
   * other calls -- the enddef is implicitly done for the NetCDF4 iotypes
   * -- so this sub-test is skipped entirely for them, matching
   * tests/general/ncdf_fail.F90.in's test_redef_twice. */
  if(is_nc4_iotype(iotype)){
    return PIO_NOERR;
  }

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }
  PIOc_closefile(ncid);

  bool ok = true;
  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to open:" + TGV_FNAME)){
    ok = false;
  }
  else{
    ret = PIOc_redef(ncid);
    if(!check_ret(tf, ret, "Failed to enter redef mode" + TGV_FNAME)){
      ok = false;
    }
    else{
      ret = PIOc_redef(ncid);
      ok = passert(tf, ret != PIO_NOERR, "Entering redef twice did not fail as expected");

      ret = PIOc_enddef(ncid);
      ok = check_ret(tf, ret, "Failed to end redef mode" + TGV_FNAME) && ok;
    }
    PIOc_closefile(ncid);
  }

  PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_redef_twice(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : test_redef_twice : ") + iotype_name + "\n");

    if(redef_twice_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int inq_missing_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string fname("ncdf_fail_test_inq_missing.nc");
  const std::string missing_data("MISSING_VAL");
  const int INVALID_ID = 1000;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to end redef mode" + fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), fname.c_str());
    return PIO_EINTERNAL;
  }

  /* The file has no variables or dimensions */
  bool ok = true;
  int var_id = -1, dim_id = -1, att_id = -1, tmp_val = -1;
  PIO_Offset tmp_val2 = -1;
  char tmp_str[PIO_MAX_NAME + 1];

  ret = PIOc_inq_varid(ncid, missing_data.c_str(), &var_id);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing var id did not fail as expected") && ok;

  var_id = INVALID_ID;

  tmp_str[0] = '\0';
  ret = PIOc_inq_varname(ncid, var_id, tmp_str, PIO_MAX_NAME + 1);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing var name did not fail as expected") && ok;

  ret = PIOc_inq_vartype(ncid, var_id, &tmp_val);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing var type did not fail as expected") && ok;

  ret = PIOc_inq_varndims(ncid, var_id, &tmp_val);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing var ndims did not fail as expected") && ok;

  ret = PIOc_inq_varnatts(ncid, var_id, &tmp_val);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing var natts did not fail as expected") && ok;

  ret = PIOc_inq_dimid(ncid, missing_data.c_str(), &dim_id);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing dim id did not fail as expected") && ok;

  dim_id = INVALID_ID;

  tmp_str[0] = '\0';
  ret = PIOc_inq_dimname(ncid, dim_id, tmp_str);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing dim name did not fail as expected") && ok;

  ret = PIOc_inq_dimlen(ncid, dim_id, &tmp_val2);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing dim len did not fail as expected") && ok;

  att_id = INVALID_ID;

  tmp_str[0] = '\0';
  ret = PIOc_inq_attname(ncid, var_id, att_id, tmp_str);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing att name did not fail as expected") && ok;

  ret = PIOc_inq_attlen(ncid, var_id, missing_data.c_str(), &tmp_val2);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing att len did not fail as expected") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), fname.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_inq_missing(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : test_inq_missing : ") + iotype_name + "\n");

    if(inq_missing_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int get_missing_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string fname("ncdf_fail_test_get_missing.nc");
  const std::string missing_data("MISSING_VAL");
  const int INVALID_ID = 1000;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to end redef mode" + fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), fname.c_str());
    return PIO_EINTERNAL;
  }

  /* The file has no variables */
  bool ok = true;
  int var_id = -1;

  ret = PIOc_inq_varid(ncid, missing_data.c_str(), &var_id);
  ok = passert(tf, ret != PIO_NOERR, "Querying missing var id did not fail as expected") && ok;

  var_id = INVALID_ID;

  int tmp_val[1] = {0};
  ret = PIOc_get_var(ncid, var_id, tmp_val);
  ok = passert(tf, ret != PIO_NOERR, "Getting missing var did not fail as expected") && ok;

  char tmp_str[PIO_MAX_NAME + 1];
  tmp_str[0] = '\0';
  ret = PIOc_get_att(ncid, PIO_GLOBAL, missing_data.c_str(), tmp_str);
  ok = passert(tf, ret != PIO_NOERR, "Getting missing att did not fail as expected") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), fname.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_get_missing(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : test_get_missing : ") + iotype_name + "\n");

    if(get_missing_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
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
    {"test_clob_then_no_clob", test_clob_then_no_clob},
    {"test_redef_with_no_write", test_redef_with_no_write},
    {"test_redef_twice", test_redef_twice},
    {"test_inq_missing", test_inq_missing},
    {"test_get_missing", test_get_missing}
  };

  SPIO_TF::Test_framework tf("ncdf_fail_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  if(tf.init_iosystem(PIO_REARR_BOX) == PIO_NOERR){
    /* Fortran's PIO_TF test driver defaults to PIO_BCAST_ERROR (see
     * pio_tf_err_handler_ in tests/general/util/pio_tutil.F90), so that
     * the PIOc_* calls below that are expected to fail return an error
     * code instead of aborting via PIO_INTERNAL_ERROR (the iosystem's
     * default when no handler has been set). Match that here. */
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
