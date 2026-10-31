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

/* The Fortran driver (PIO_TF_TEST_DRIVER_BEGIN in ncdf_simple_tests.F90.in)
 * pre-creates this file once per iotype, and test_redef_enddef/test_def_dim/
 * test_def_var all run against it. The C++ port has no equivalent
 * cross-function driver state, so each of the three functions that need it
 * creates, uses, and deletes it themselves. */
const std::string TGV_FNAME("pio_ncdf_test_file.nc");

static int test_create_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string fname("pio_test_create.nc");
  const int flags[3] = {PIO_CLOBBER, PIO_CLOBBER | PIO_64BIT_OFFSET, PIO_CLOBBER | PIO_64BIT_DATA};

  for(int i = 0; i < 3; i++){
    int ncid = -1;
    int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, fname.c_str(), flags[i]);
    if(!check_ret(tf, ret, "Failed to create:" + fname)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close:" + fname)){
      return PIO_EINTERNAL;
    }

    PIOc_deletefile(tf.get_iosysid(), fname.c_str());
  }

  return PIO_NOERR;
}

int test_create(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_create : ") + iotype_name + "\n");

    if(test_create_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_clobber_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string clob_fname("pio_test_clobber.nc");

  int ncid = -1;
  /* Default is NOCLOBBER */
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, clob_fname.c_str(), PIO_NOCLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + clob_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + clob_fname)){
    return PIO_EINTERNAL;
  }

  /* Recreate with CLOBBER option - should erase existing file and create new one */
  ncid = -1;
  ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, clob_fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create (with CLOBBER):" + clob_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + clob_fname)){
    return PIO_EINTERNAL;
  }

  PIOc_deletefile(tf.get_iosysid(), clob_fname.c_str());

  return PIO_NOERR;
}

int test_clobber(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_clobber : ") + iotype_name + "\n");

    if(test_clobber_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_redef_enddef_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to open:" + TGV_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* A simple redef and then enddef */
  ret = PIOc_redef(ncid);
  if(!check_ret(tf, ret, "Failed to redef:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + TGV_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());

  return PIO_NOERR;
}

int test_redef_enddef(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_redef_enddef : ") + iotype_name + "\n");

    if(test_redef_enddef_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_redef_twice_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string tmp_fname("pio_test_create_redef_twice.nc");

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, tmp_fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  /* A simple redef and then enddef */
  ret = PIOc_redef(ncid);
  if(!check_ret(tf, ret, "Failed to redef:" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  int dimid1 = -1;
  ret = PIOc_def_dim(ncid, "dummy_dim_def_var_redef1", 100, &dimid1);
  if(!check_ret(tf, ret, "Failed to define dim (1st redef):" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  int varid1 = -1;
  ret = PIOc_def_var(ncid, "dummy_var_def_var_redef1", PIO_INT, 1, &dimid1, &varid1);
  if(!check_ret(tf, ret, "Failed to define var (1st redef):" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  /* A second redef and then enddef */
  ret = PIOc_redef(ncid);
  if(!check_ret(tf, ret, "Failed to redef:" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  int dimid2 = -1;
  ret = PIOc_def_dim(ncid, "dummy_dim_def_var_redef2", 100, &dimid2);
  if(!check_ret(tf, ret, "Failed to define dim (2nd redef):" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  int varid2 = -1;
  ret = PIOc_def_var(ncid, "dummy_var_def_var_redef2", PIO_INT, 1, &dimid2, &varid2);
  if(!check_ret(tf, ret, "Failed to define var (2nd redef):" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef (2nd redef):" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + tmp_fname)){
    return PIO_EINTERNAL;
  }

  /* Note: the Fortran never deletes tmp_fname */
  return PIO_NOERR;
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

    if(test_redef_twice_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_def_dim_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to open:" + TGV_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* A simple redef and then enddef */
  ret = PIOc_redef(ncid);
  if(!check_ret(tf, ret, "Failed to redef:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int dimid = -1;
  ret = PIOc_def_dim(ncid, "dummy_dim_def_dim", 100, &dimid);
  if(!check_ret(tf, ret, "Failed to define dim:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + TGV_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());

  return PIO_NOERR;
}

int test_def_dim(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_def_dim : ") + iotype_name + "\n");

    if(test_def_dim_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_def_var_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + TGV_FNAME)){
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, TGV_FNAME.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to open:" + TGV_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* A simple redef and then enddef */
  ret = PIOc_redef(ncid);
  if(!check_ret(tf, ret, "Failed to redef:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int dimid = -1;
  ret = PIOc_def_dim(ncid, "dummy_dim_def_var", 100, &dimid);
  if(!check_ret(tf, ret, "Failed to define dim:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int varid = -1;
  ret = PIOc_def_var(ncid, "dummy_var_def_var", PIO_INT, 1, &dimid, &varid);
  if(!check_ret(tf, ret, "Failed to define var:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + TGV_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + TGV_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  PIOc_deletefile(tf.get_iosysid(), TGV_FNAME.c_str());

  return PIO_NOERR;
}

int test_def_var(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_def_var : ") + iotype_name + "\n");

    if(test_def_var_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
static int test_data_conversion_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int VEC_LOCAL_SZ = 7;
  const std::string data_fname("pio_test_data_conversion.nc");
  const std::string PIO_VAR_INT_NAME("PIO_TF_test_var_int");
  const std::string PIO_VAR_REAL_NAME("PIO_TF_test_var_real");
  const std::string PIO_VAR_DBL_NAME("PIO_TF_test_var_dbl");

  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  int dims[1] = {VEC_LOCAL_SZ * tf.get_comm_size()};
  std::vector<T> wbuf(VEC_LOCAL_SZ, static_cast<T>(tf.get_comm_rank()));
  std::vector<T> rbuf(VEC_LOCAL_SZ, 0);

  int wiodesc = -1, riodesc = -1;
  int ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &wiodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "Failed to init (write) decomp")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &riodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "Failed to init (read) decomp")){
    PIOc_freedecomp(tf.get_iosysid(), wiodesc);
    return PIO_EINTERNAL;
  }

  int ncid = -1;
  ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, data_fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Could not create file " + data_fname)){
    PIOc_freedecomp(tf.get_iosysid(), wiodesc);
    PIOc_freedecomp(tf.get_iosysid(), riodesc);
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &pio_dim);
  if(!check_ret(tf, ret, "Failed to define a dim : " + data_fname)){
    return PIO_EINTERNAL;
  }

  int varid_int = -1, varid_real = -1, varid_dbl = -1;
  ret = PIOc_def_var(ncid, PIO_VAR_INT_NAME.c_str(), PIO_INT, 1, &pio_dim, &varid_int);
  if(!check_ret(tf, ret, "Failed to define a var of int type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_VAR_REAL_NAME.c_str(), PIO_REAL, 1, &pio_dim, &varid_real);
  if(!check_ret(tf, ret, "Failed to define a var of real type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_VAR_DBL_NAME.c_str(), PIO_DOUBLE, 1, &pio_dim, &varid_dbl);
  if(!check_ret(tf, ret, "Failed to define a var of double type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to end redef mode : " + data_fname)){
    return PIO_EINTERNAL;
  }

  /* Write the int type variable out (data conversion might occur: real => int, double => int) */
  ret = PIOc_write_darray(ncid, varid_int, wiodesc, VEC_LOCAL_SZ, wbuf.data(), NULL);
  if(!check_ret(tf, ret, "Failed to write darray on a var of int type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  /* Write the real type variable out (data conversion might occur: int => real, double => real) */
  ret = PIOc_write_darray(ncid, varid_real, wiodesc, VEC_LOCAL_SZ, wbuf.data(), NULL);
  if(!check_ret(tf, ret, "Failed to write darray on a var of real type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  /* Write the double type variable out (data conversion might occur: int => double, real => double) */
  ret = PIOc_write_darray(ncid, varid_dbl, wiodesc, VEC_LOCAL_SZ, wbuf.data(), NULL);
  if(!check_ret(tf, ret, "Failed to write darray on a var of double type : " + data_fname)){
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close file " + data_fname)){
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, data_fname.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Could not reopen file " + data_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_VAR_INT_NAME.c_str(), &varid_int);
  if(!check_ret(tf, ret, "Cannot inq int var " + data_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_VAR_REAL_NAME.c_str(), &varid_real);
  if(!check_ret(tf, ret, "Cannot inq real var " + data_fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_VAR_DBL_NAME.c_str(), &varid_dbl);
  if(!check_ret(tf, ret, "Cannot inq double var " + data_fname)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  /* Read the int type variable back (data conversion might occur: int => real, int => double) */
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    rbuf[i] = 0;
  }
  ret = PIOc_read_darray(ncid, varid_int, riodesc, VEC_LOCAL_SZ, rbuf.data());
  if(!check_ret(tf, ret, "Failed to read darray on a var of int type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
    return PIO_EINTERNAL;
  }

  /* Read the real type variable back (data conversion might occur: real => int, real => double) */
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    rbuf[i] = 0;
  }
  ret = PIOc_read_darray(ncid, varid_real, riodesc, VEC_LOCAL_SZ, rbuf.data());
  if(!check_ret(tf, ret, "Failed to read darray on a var of real type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
    return PIO_EINTERNAL;
  }

  /* Read the double type variable back (data conversion might occur: double => int, double => real) */
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    rbuf[i] = 0;
  }
  ret = PIOc_read_darray(ncid, varid_dbl, riodesc, VEC_LOCAL_SZ, rbuf.data());
  if(!check_ret(tf, ret, "Failed to read darray on a var of double type : " + data_fname)){
    return PIO_EINTERNAL;
  }

  if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + data_fname)){
    PIOc_freedecomp(tf.get_iosysid(), wiodesc);
    PIOc_freedecomp(tf.get_iosysid(), riodesc);
    return PIO_EINTERNAL;
  }

  PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());

  PIOc_freedecomp(tf.get_iosysid(), wiodesc);
  PIOc_freedecomp(tf.get_iosysid(), riodesc);

  return PIO_NOERR;
}

template<typename T>
int test_data_conversion(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_data_conversion : ") + iotype_name + "\n");

    if(test_data_conversion_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
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
    {"test_create", test_create},
    {"test_clobber", test_clobber},
    {"test_redef_enddef", test_redef_enddef},
    {"test_redef_twice", test_redef_twice},
    {"test_def_dim", test_def_dim},
    {"test_def_var", test_def_var},
    {"test_data_conversion_int", test_data_conversion<int>},
    {"test_data_conversion_float", test_data_conversion<float>},
    {"test_data_conversion_double", test_data_conversion<double>}
  };

  SPIO_TF::Test_framework tf("ncdf_simple_tests_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  /* This is a NC-focused-driver test file (like ncdf_fail.cpp) that only
   * needs a single rearranger, not the usual 3-rearranger loop. */
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
