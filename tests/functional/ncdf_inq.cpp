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

const std::string FNAME("pio_ncdf_inq_test_file.nc");
const std::string VAR_NAME("dummy_var");
const int VAR_NDIMS = 1;
const std::string DIM_NAME("dummy_dim");
const int DIM_LEN = 100;
const int VAR_NATTS = 2;
const std::string IATT_NAME("dummy_iatt");
const int IATT_VAL = 3;
const std::string CATT_NAME("dummy_catt");
const std::string CATT_VAL("DUMMY_STR");

static int test_setup(SPIO_TF::Test_framework &tf, int iotype)
{
  int ncid = -1, dimid = -1, varid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open: " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), DIM_LEN, &dimid);
  if(!check_ret(tf, ret, "Failed to define dim: " + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), PIO_REAL, VAR_NDIMS, &dimid, &varid);
  if(!check_ret(tf, ret, "Failed to define var: " + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  std::vector<int> iatt(DIM_LEN, IATT_VAL);
  ret = PIOc_put_att(ncid, varid, IATT_NAME.c_str(), PIO_INT, iatt.size(), iatt.data());
  if(!check_ret(tf, ret, "Failed to put attribute: " + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, varid, CATT_NAME.c_str(), PIO_CHAR, CATT_VAL.size(), CATT_VAL.c_str());
  if(!check_ret(tf, ret, "Failed to put attribute: " + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef: " + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  std::vector<float> val(DIM_LEN, static_cast<float>(tf.get_comm_rank()));
  ret = PIOc_put_var(ncid, varid, val.data());
  if(!check_ret(tf, ret, "Failed to put var: " + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  PIOc_closefile(ncid);
  return PIO_NOERR;
}

static void test_teardown(SPIO_TF::Test_framework &tf)
{
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
}

static int test_inq_var(SPIO_TF::Test_framework &tf, int ncid)
{
  int varid = -1, var_type = -1, var_ndims = -1, var_natts = -1;
  int ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &varid);
  if(!check_ret(tf, ret, "Failed to inquire varid: " + FNAME)){
    return PIO_EINTERNAL;
  }

  char var_name[PIO_MAX_NAME + 1];
  var_name[0] = '\0';
  ret = PIOc_inq_varname(ncid, varid, var_name, PIO_MAX_NAME + 1);
  if(!check_ret(tf, ret, "Failed to inquire var name: " + FNAME)){
    return PIO_EINTERNAL;
  }
  bool ok = passert(tf, (VAR_NAME == var_name), "Variable name is not the expected value");

  ret = PIOc_inq_vartype(ncid, varid, &var_type);
  if(!check_ret(tf, ret, "Failed to inquire var type: " + FNAME)){
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (var_type == PIO_REAL), "Variable type is not the expected type") && ok;

  ret = PIOc_inq_varndims(ncid, varid, &var_ndims);
  if(!check_ret(tf, ret, "Failed to inquire var ndims: " + FNAME)){
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (var_ndims == VAR_NDIMS), "Num of dims for variable is not expected") && ok;

  std::vector<int> var_dims(var_ndims);
  ret = PIOc_inq_vardimid(ncid, varid, var_dims.data());
  if(!check_ret(tf, ret, "Failed to get dim ids: " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varnatts(ncid, varid, &var_natts);
  if(!check_ret(tf, ret, "Failed to inquire var natts: " + FNAME)){
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (var_natts == VAR_NATTS), "Num of atts for variable is not expected") && ok;

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_inq_dim(SPIO_TF::Test_framework &tf, int ncid)
{
  int dimid = -1;
  int ret = PIOc_inq_dimid(ncid, DIM_NAME.c_str(), &dimid);
  if(!check_ret(tf, ret, "Failed to inq dimid: " + FNAME)){
    return PIO_EINTERNAL;
  }

  char dim_name[PIO_MAX_NAME + 1];
  dim_name[0] = '\0';
  ret = PIOc_inq_dimname(ncid, dimid, dim_name);
  if(!check_ret(tf, ret, "Failed to inq dim name: " + FNAME)){
    return PIO_EINTERNAL;
  }
  bool ok = passert(tf, (DIM_NAME == dim_name), "Dim name is not the expected value");

  PIO_Offset dim_len = -1;
  ret = PIOc_inq_dimlen(ncid, dimid, &dim_len);
  if(!check_ret(tf, ret, "Failed to inq dim len: " + FNAME)){
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (dim_len == DIM_LEN), "Dim length is not the expected value") && ok;

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_inq_att(SPIO_TF::Test_framework &tf, int ncid)
{
  int varid = -1;
  int ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &varid);
  if(!check_ret(tf, ret, "Failed to inquire varid: " + FNAME)){
    return PIO_EINTERNAL;
  }

  PIO_Offset att_len = -1;
  ret = PIOc_inq_attlen(ncid, varid, IATT_NAME.c_str(), &att_len);
  if(!check_ret(tf, ret, "Failed to inquire att len: " + FNAME)){
    return PIO_EINTERNAL;
  }
  /* The int attribute's length is intentionally not checked here,
   * matching tests/general/ncdf_inq.F90.in's own commented-out assert. */

  ret = PIOc_inq_attlen(ncid, varid, CATT_NAME.c_str(), &att_len);
  if(!check_ret(tf, ret, "Failed to inquire att len: " + FNAME)){
    return PIO_EINTERNAL;
  }
  bool ok = passert(tf, (att_len == static_cast<PIO_Offset>(CATT_VAL.size())),
      "Attribute length is not expected value");

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_inq(SPIO_TF::Test_framework &tf, int iotype, const std::string &iotype_name)
{
  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      std::string("Testing type : ") + iotype_name + "\n");

  int ret = test_setup(tf, iotype);
  if(!check_ret(tf, ret, "Test setup failed")){
    return PIO_EINTERNAL;
  }

  int ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to open: " + FNAME)){
    return PIO_EINTERNAL;
  }

  bool ok = true;

  ret = test_inq_var(tf, ncid);
  ok = check_ret(tf, ret, "Failed to inq var: " + FNAME) && ok;

  ret = test_inq_dim(tf, ncid);
  ok = check_ret(tf, ret, "Failed to inq dim: " + FNAME) && ok;

  ret = test_inq_att(tf, ncid);
  ok = check_ret(tf, ret, "Failed to inq att: " + FNAME) && ok;

  PIOc_closefile(ncid);
  test_teardown(tf);

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_inq_driver(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);

    if(test_inq(tf, flavors[i], iotype_name) != PIO_NOERR){
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
    {"test_inq_driver", test_inq_driver}
  };

  SPIO_TF::Test_framework tf("ncdf_inq_cxx", MPI_COMM_WORLD, rfuncs);
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
