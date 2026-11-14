#include <iostream>
#include <string>
#include <vector>
#include <functional>
#include <utility>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <cstdio>
#include <cstring>

#include "pio.h"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#include "spio_test_framework.hpp"
#include "spio_test_check.hpp"
#include "spio_test_common.hpp"

/* This port, like ncdf_simple_tests.cpp/ncdf_fail.cpp, deliberately only
 * exercises the 5 iotypes returned by get_iotypes()/get_iotype_name()
 * (PNETCDF/NETCDF/NETCDF4C/NETCDF4P/NETCDF4P_NCZARR) instead of the full
 * set that tests/general/ncdf_get_put.F90.in's PIO_TF_Get_nc_iotypes()
 * covers (which also includes ADIOS/ADIOSC/HDF5/HDF5C). Any Fortran
 * ADIOS-specific conditional logic (e.g. skipping unsupported operations
 * for PIO_IOTYPE_ADIOS/PIO_IOTYPE_ADIOSC) is therefore dead code for this
 * port and is not carried over. */

const std::string FNAME("test_pio_ncdf_get_put.testfile");
const std::string FNAME_SLICE("test_pio_ncdf_get_put_slice.testfile");

/* Wrapper around passert() for string/scalar value comparisons - on failure
 * this reports the actual and expected values (unlike a bare passert()),
 * which makes debugging failures in this large file easier */
static bool check_str(SPIO_TF::Test_framework &tf, const std::string &res, const std::string &exp, const std::string &ctx)
{
  return passert(tf, (res == exp), ctx + " : got \"" + res + "\", expected \"" + exp + "\"");
}

template<typename T>
static int test_put_1datt_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int DIM_LEN = 100;
  const std::string DIM_NAME("dummy_dim_put_val");
  const std::string VAR_NAME("dummy_var_put_val");
  const std::string CVAR_NAME("dummy_var_put_cval");

  std::vector<T> val(DIM_LEN, static_cast<T>(tf.get_comm_size()));
  const std::string cval("DUMMY_STRING");

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int pio_var = -1, pio_cvar = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, CVAR_NAME.c_str(), PIO_CHAR, 1, &pio_dim, &pio_cvar);
  if(!check_ret(tf, ret, "Failed to define char var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, pio_var, "dummy_att_put_val", Pio_type_map<T>::value, val.size(), val.data());
  if(!check_ret(tf, ret, "Failed to put attribute:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, pio_cvar, "dummy_att_put_cval", PIO_CHAR, cval.size(), cval.c_str());
  if(!check_ret(tf, ret, "Failed to put char attribute:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, pio_cvar, "empty_str_att", PIO_CHAR, 0, "");
  if(!check_ret(tf, ret, "Failed to put char attribute:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return PIO_NOERR;
}

template<typename T>
int test_put_1datt(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_1datt : ") + iotype_name + "\n");

    if(test_put_1datt_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
static int test_put_get_1datt_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int DIM_LEN = 100;
  const std::string DIM_NAME("dummy_dim");
  const std::string VAR_NAME("dummy_val");
  const std::string CVAR_NAME("dummy_cval");

  T init_val = static_cast<T>(tf.get_comm_size());
  std::vector<T> pval(DIM_LEN, init_val);
  std::vector<T> gval(DIM_LEN, static_cast<T>(0));
  const std::string pcval("DUMMY_STRING");

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int pio_var = -1, pio_cvar = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, CVAR_NAME.c_str(), PIO_CHAR, 1, &pio_dim, &pio_cvar);
  if(!check_ret(tf, ret, "Failed to define char var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, pio_var, "dummy_att_val", Pio_type_map<T>::value, pval.size(), pval.data());
  if(!check_ret(tf, ret, "Failed to put attribute:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, pio_cvar, "dummy_att_cval", PIO_CHAR, pcval.size(), pcval.c_str());
  if(!check_ret(tf, ret, "Failed to put char attribute:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Could not reopen file " + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Could not inq var :" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, CVAR_NAME.c_str(), &pio_cvar);
  if(!check_ret(tf, ret, "Could not inq var :" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  ret = PIOc_get_att(ncid, pio_var, "dummy_att_val", gval.data());
  if(!check_ret(tf, ret, "Failed to get attribute:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong value") && ok;

  PIO_Offset cattlen = 0;
  ret = PIOc_inq_attlen(ncid, pio_cvar, "dummy_att_cval", &cattlen);
  if(!check_ret(tf, ret, "Failed to inquire attribute length:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  /* Size the get buffer from the real on-disk attribute length (not from
   * pcval's length) - spio_get_att_tc() copies attlen bytes verbatim without
   * null-terminating or bounds-checking against the caller's buffer. */
  std::vector<char> gcval(static_cast<std::size_t>(cattlen), '\0');
  ret = PIOc_get_att(ncid, pio_cvar, "dummy_att_cval", gcval.data());
  if(!check_ret(tf, ret, "Failed to get attribute:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gcval.begin(), gcval.end()), pcval, "Got wrong value") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_1datt(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1datt : ") + iotype_name + "\n");

    if(test_put_get_1datt_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
static int test_put_get_0dvar_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string VAR_NAME("dummy_scalar_var_put_val");
  const std::string CVAR_NAME("dummy_scalar_var_put_cval");

  T pval = static_cast<T>(tf.get_comm_size());
  T gval = static_cast<T>(0);
  char pcval = 'D';
  char gcval = '\0';

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_var = -1, pio_cvar = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 0, NULL, &pio_var);
  if(!check_ret(tf, ret, "Failed to define scalar var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, CVAR_NAME.c_str(), PIO_CHAR, 0, NULL, &pio_cvar);
  if(!check_ret(tf, ret, "Failed to define scalar char var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, pio_var, &pval);
  if(!check_ret(tf, ret, "Failed to put scalar var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, pio_cvar, &pcval);
  if(!check_ret(tf, ret, "Failed to put scalar char var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to get scalar var:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, CVAR_NAME.c_str(), &pio_cvar);
  if(!check_ret(tf, ret, "Failed to get scalar char var:" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  ret = PIOc_get_var(ncid, pio_var, &gval);
  if(!check_ret(tf, ret, "Failed to get scalar var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::to_string(gval), std::to_string(pval), "Got wrong value") && ok;

  ret = PIOc_get_var(ncid, pio_cvar, &gcval);
  if(!check_ret(tf, ret, "Failed to get scalar char var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(1, gcval), std::string(1, pcval), "Got wrong value") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_0dvar(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_0dvar : ") + iotype_name + "\n");

    if(test_put_get_0dvar_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
static int test_put_get_1dvar_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int DIM_LEN = 100;
  const std::string DIM_NAME("dummy_dim_put_val");
  const std::string VAR_NAME("dummy_var_put_val");
  const std::string CVAR_NAME("dummy_var_put_cval");

  std::vector<T> pval(DIM_LEN, static_cast<T>(tf.get_comm_size()));
  std::vector<T> gval(DIM_LEN, static_cast<T>(0));
  /* A 1D char variable is a plain array of DIM_LEN characters (unlike a
   * text attribute, whose length is the trimmed string length), so the
   * full DIM_LEN-character buffer, padded with spaces, is written out -
   * matching pio_put_var_text()'s use of LEN(val) (not LEN_TRIM(val)). */
  std::string pcval("DUMMY_STRING");
  pcval.resize(DIM_LEN, ' ');

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int pio_var = -1, pio_cvar = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, CVAR_NAME.c_str(), PIO_CHAR, 1, &pio_dim, &pio_cvar);
  if(!check_ret(tf, ret, "Failed to define char var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, pio_var, pval.data());
  if(!check_ret(tf, ret, "Failed to put var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, pio_cvar, pcval.c_str());
  if(!check_ret(tf, ret, "Failed to put char var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to get scalar var:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, CVAR_NAME.c_str(), &pio_cvar);
  if(!check_ret(tf, ret, "Failed to get scalar char var:" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  ret = PIOc_get_var(ncid, pio_var, gval.data());
  if(!check_ret(tf, ret, "Failed to get var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong value") && ok;

  std::vector<char> gcval(DIM_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_cvar, gcval.data());
  if(!check_ret(tf, ret, "Failed to get char var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gcval.begin(), gcval.end()), pcval, "Got wrong value") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_1dvar(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1dvar : ") + iotype_name + "\n");

    if(test_put_get_1dvar_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Put and get a single coordinate variable (dimension and variable with the
 * same name, e.g. lon[lon]) */
template<typename T>
static int test_put_get_1d_coord_var_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int DIM_LEN = 100;
  const std::string VAR_NAME("dummy_coord_var");

  std::vector<T> pval(DIM_LEN, static_cast<T>(tf.get_comm_size()));
  std::vector<T> gval(DIM_LEN, static_cast<T>(0));

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1, pio_var = -1;
  ret = PIOc_def_dim(ncid, VAR_NAME.c_str(), DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define coord var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, pio_var, pval.data());
  if(!check_ret(tf, ret, "Failed to put coord var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to get coord var:" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  ret = PIOc_get_var(ncid, pio_var, gval.data());
  if(!check_ret(tf, ret, "Failed to get coord var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  bool ok = check_val(tf, gval, pval, "Got wrong value");

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_1d_coord_var(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1d_coord_var : ") + iotype_name + "\n");

    if(test_put_get_1d_coord_var_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Put and get multiple variables and coordinate variables */
template<typename T>
static int test_put_get_1d_coord_mvars_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int DIM_LEN = 100;
  const std::string CVAR_NAME("dummy_coord_var");
  const std::string VAR_NAME("dummy_var");

  std::vector<T> pval(DIM_LEN, static_cast<T>(tf.get_comm_size()));
  std::vector<T> gval(DIM_LEN, static_cast<T>(0));

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1, pio_var = -1, pio_var2 = -1;
  ret = PIOc_def_dim(ncid, CVAR_NAME.c_str(), DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var2);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, CVAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define coord var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, pio_var, pval.data());
  if(!check_ret(tf, ret, "Failed to put coord var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, pio_var2, pval.data());
  if(!check_ret(tf, ret, "Failed to put var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, CVAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to get coord var:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var2);
  if(!check_ret(tf, ret, "Failed to get var:" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  ret = PIOc_get_var(ncid, pio_var, gval.data());
  if(!check_ret(tf, ret, "Failed to get coord var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong value") && ok;

  for(std::size_t j = 0; j < gval.size(); j++){
    gval[j] = static_cast<T>(0);
  }
  ret = PIOc_get_var(ncid, pio_var2, gval.data());
  if(!check_ret(tf, ret, "Failed to get var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong value") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_1d_coord_mvars(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1d_coord_mvars : ") + iotype_name + "\n");

    if(test_put_get_1d_coord_mvars_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Put and get a 1d variable using a user buffer whose size does not match
 * the variable's declared size. "svar"/"scvar" are variables written using a
 * user buffer *larger* than the variable (so the write is truncated to the
 * variable's declared length, DIM_LEN, and the full variable is compared on
 * read-back). "lvar"/"lcvar" are variables written using a user buffer
 * *smaller* than the variable (so only a DIM_SMALL_LEN prefix of the
 * variable is written, and only that written prefix is compared on
 * read-back - the untouched tail of the variable is unspecified). */
template<typename T>
static int test_put_get_1dvar_ls_buf_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int DIM_LEN = 200;
  const int DIM_LARGE_LEN = DIM_LEN + 100;
  const int DIM_SMALL_LEN = DIM_LEN - 100;
  const std::string DIM_NAME("dummy_dim_put_val");
  const std::string SVAR_NAME("dummy_svar_put_val");
  const std::string LVAR_NAME("dummy_lvar_put_val");
  const std::string SCVAR_NAME("dummy_svar_put_cval");
  const std::string LCVAR_NAME("dummy_lvar_put_cval");

  /* Buffer used to write svar/scvar (larger than the DIM_LEN-sized variable -
   * only the first DIM_LEN elements are ever written out, so only those are
   * initialized meaningfully; the tail, indices DIM_LEN..DIM_LARGE_LEN-1, is
   * filled with a filler value that is never written out or compared, matching
   * tests/general/ncdf_get_put.F90.in's lpval/lpcval DIM_LARGE_LEN buffers) */
  std::vector<T> pval_large(DIM_LARGE_LEN);
  for(std::size_t i = 0; i < pval_large.size(); i++){
    pval_large[i] = (i < static_cast<std::size_t>(DIM_LEN)) ?
        static_cast<T>(i + 1) : static_cast<T>(tf.get_comm_size());
  }
  std::string pcval_large(DIM_LEN, 'l');
  pcval_large.append(static_cast<std::size_t>(DIM_LARGE_LEN - DIM_LEN), ' ');

  /* Buffer used to write lvar/lcvar (smaller than the DIM_LEN-sized variable -
   * only a DIM_SMALL_LEN prefix of the variable is written out) */
  std::vector<T> pval_small(DIM_SMALL_LEN);
  for(std::size_t i = 0; i < pval_small.size(); i++){
    pval_small[i] = static_cast<T>(i + 1);
  }
  const std::string pcval_small(DIM_SMALL_LEN, 's');

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int pio_svar = -1, pio_lvar = -1, pio_scvar = -1, pio_lcvar = -1;
  ret = PIOc_def_var(ncid, SVAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_svar);
  if(!check_ret(tf, ret, "Failed to define var (smaller than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, LVAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_lvar);
  if(!check_ret(tf, ret, "Failed to define var (larger than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, SCVAR_NAME.c_str(), PIO_CHAR, 1, &pio_dim, &pio_scvar);
  if(!check_ret(tf, ret, "Failed to define char var (smaller than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, LCVAR_NAME.c_str(), PIO_CHAR, 1, &pio_dim, &pio_lcvar);
  if(!check_ret(tf, ret, "Failed to define char var (larger than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  PIO_Offset start[1] = { 0 };
  PIO_Offset count_full[1] = { DIM_LEN };
  PIO_Offset count_small[1] = { DIM_SMALL_LEN };

  /* Put the values using a user buffer that is larger than the variable */
  ret = PIOc_put_vara(ncid, pio_svar, start, count_full, pval_large.data());
  if(!check_ret(tf, ret, "Failed to put var (smaller than user buffer)):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_vara(ncid, pio_scvar, start, count_full, pcval_large.c_str());
  if(!check_ret(tf, ret, "Failed to put char var (smaller than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* Put the values using a user buffer that is smaller than the variable */
  ret = PIOc_put_vara(ncid, pio_lvar, start, count_small, pval_small.data());
  if(!check_ret(tf, ret, "Failed to put var (larger than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_vara(ncid, pio_lcvar, start, count_small, pcval_small.c_str());
  if(!check_ret(tf, ret, "Failed to put char var (larger than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, SVAR_NAME.c_str(), &pio_svar);
  if(!check_ret(tf, ret, "Failed to get var (smaller than user buffer):" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, LVAR_NAME.c_str(), &pio_lvar);
  if(!check_ret(tf, ret, "Failed to get var (larger than user buffer):" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, SCVAR_NAME.c_str(), &pio_scvar);
  if(!check_ret(tf, ret, "Failed to get char var (smaller than user buffer):" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, LCVAR_NAME.c_str(), &pio_lcvar);
  if(!check_ret(tf, ret, "Failed to get char var (larger than user buffer):" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  /* svar/scvar were written with a buffer larger than the variable - the
   * full DIM_LEN-sized variable was written (truncated), so compare it in
   * full */
  std::vector<T> gval_full(DIM_LEN, static_cast<T>(0));
  ret = PIOc_get_var(ncid, pio_svar, gval_full.data());
  if(!check_ret(tf, ret, "Failed to get var (smaller than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  /* Only the first DIM_LEN elements of pval_large/pcval_large were ever
   * written out (the rest of the larger source buffer was never read by
   * PIOc_put_vara), so only compare that written prefix */
  std::vector<T> pval_large_written(pval_large.begin(), pval_large.begin() + DIM_LEN);
  ok = check_val(tf, gval_full, pval_large_written,
      "Get variable written with large user buffer : Got wrong value") && ok;

  std::vector<char> gcval_full(DIM_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_scvar, gcval_full.data());
  if(!check_ret(tf, ret, "Failed to get char var (smaller than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gcval_full.begin(), gcval_full.end()), pcval_large.substr(0, DIM_LEN),
      "Get variable written with large user buffer : Got wrong value") && ok;

  /* lvar/lcvar were written with a buffer smaller than the variable - only
   * the DIM_SMALL_LEN prefix of the variable was written, so only read back
   * and compare that prefix; the untouched tail is unspecified */
  std::vector<T> gval_small(DIM_SMALL_LEN, static_cast<T>(0));
  ret = PIOc_get_vara(ncid, pio_lvar, start, count_small, gval_small.data());
  if(!check_ret(tf, ret, "Failed to get var (larger than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval_small, pval_small,
      "Get variable written with small user buffer : Got wrong value") && ok;

  std::vector<char> gcval_small(DIM_SMALL_LEN, '\0');
  ret = PIOc_get_vara(ncid, pio_lcvar, start, count_small, gcval_small.data());
  if(!check_ret(tf, ret, "Failed to get char var (larger than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gcval_small.begin(), gcval_small.end()), pcval_small,
      "Get variable written with small user buffer : Got wrong value") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_1dvar_ls_buf(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1dvar_ls_buf : ") + iotype_name + "\n");

    if(test_put_get_1dvar_ls_buf_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Write out a 1d var slice from a conceptual 2d (MAX_ROW_DIM_LEN x
 * MAX_COL_DIM_LEN) write buffer, stored column-major (as in the Fortran
 * source) so that a single column is a contiguous run of MAX_ROW_DIM_LEN
 * elements that can be handed directly to PIOc_put_vara. */
template<typename T>
static int test_put_get_1dvar_slice_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string DIM_NAME("dummy_dim_put_val");
  const std::string VAR_NAME("dummy_var_put_val");
  const int MAX_ROW_DIM_LEN = 100;
  const int MAX_COL_DIM_LEN = 4;
  const int COL_WRITE_DIM = 2;

  std::vector<T> pval(static_cast<std::size_t>(MAX_ROW_DIM_LEN) * MAX_COL_DIM_LEN, static_cast<T>(-1));
  const T write_val = static_cast<T>(tf.get_comm_size());
  const std::size_t col_off = static_cast<std::size_t>(COL_WRITE_DIM - 1) * MAX_ROW_DIM_LEN;
  for(std::size_t i = 0; i < static_cast<std::size_t>(MAX_ROW_DIM_LEN); i++){
    pval[col_off + i] = write_val;
  }

  std::vector<T> exp_val(MAX_ROW_DIM_LEN, write_val);
  std::vector<T> gval(MAX_ROW_DIM_LEN, static_cast<T>(0));

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME_SLICE.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME_SLICE)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), MAX_ROW_DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME_SLICE)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_SLICE.c_str());
    return PIO_EINTERNAL;
  }

  int pio_var = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME_SLICE)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_SLICE.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME_SLICE)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_SLICE.c_str());
    return PIO_EINTERNAL;
  }

  PIO_Offset start[1] = { 0 };
  PIO_Offset count[1] = { MAX_ROW_DIM_LEN };
  ret = PIOc_put_vara(ncid, pio_var, start, count, &pval[col_off]);
  if(!check_ret(tf, ret, "Failed to put var:" + FNAME_SLICE)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_SLICE.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME_SLICE)){
    PIOc_deletefile(tf.get_iosysid(), FNAME_SLICE.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME_SLICE.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME_SLICE)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to get scalar var:" + FNAME_SLICE)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  ret = PIOc_get_var(ncid, pio_var, gval.data());
  if(!check_ret(tf, ret, "Failed to get var:" + FNAME_SLICE)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  bool ok = check_val(tf, gval, exp_val, "Got wrong value");

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME_SLICE.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_1dvar_slice(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1dvar_slice : ") + iotype_name + "\n");

    if(test_put_get_1dvar_slice_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Write a 1d var in 4 sequential hyperslab parts (forward order), then read
 * it back in 4 separate hyperslab parts in reverse order (4th part first),
 * checking each part as soon as it is read. */
template<typename T>
static int test_put_get_1dvar_4parts_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string DIM_NAME("dummy_dim_put_val");
  const std::string VAR_NAME("dummy_var_put_val");
  const int DIM_LEN = 16;
  const int PART_LEN = DIM_LEN / 4;
  static const char *part_ctx[4] = { "1st", "2nd", "3rd", "4th" };

  std::vector<T> pval(DIM_LEN);
  for(std::size_t i = 0; i < pval.size(); i++){
    pval[i] = static_cast<T>(i + 1);
  }

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int pio_var = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  PIO_Offset count[1] = { PART_LEN };

  for(int p = 0; p < 4; p++){
    PIO_Offset start[1] = { static_cast<PIO_Offset>(p * PART_LEN) };
    ret = PIOc_put_vara(ncid, pio_var, start, count,
        &pval[static_cast<std::size_t>(p) * PART_LEN]);
    if(!check_ret(tf, ret, std::string("Failed to put var (") + part_ctx[p] + " part):" + FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to get scalar var:" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  /* Read back in reverse order (4th part first), checking each part as soon
   * as it is read */
  for(int p = 3; p >= 0; p--){
    PIO_Offset start[1] = { static_cast<PIO_Offset>(p * PART_LEN) };
    std::vector<T> gpart(PART_LEN, static_cast<T>(0));
    ret = PIOc_get_vara(ncid, pio_var, start, count, gpart.data());
    if(!check_ret(tf, ret, std::string("Failed to get var (") + part_ctx[p] + " part):" + FNAME)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }

    std::vector<T> exp_part(pval.begin() + static_cast<std::size_t>(p) * PART_LEN,
        pval.begin() + static_cast<std::size_t>(p + 1) * PART_LEN);
    ok = check_val(tf, gpart, exp_part, "Got wrong value") && ok;
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_1dvar_4parts(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1dvar_4parts : ") + iotype_name + "\n");

    if(test_put_get_1dvar_4parts_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Put and get 2d/3d/4d vars, one (fixed-size) time slice at a time */
template<typename T>
static int test_put_get_md2mdplus1_var_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string MD_SLICE_FNAME("test_pio_ncdf_get_put_md_slice.testfile");
  const std::string PIO_2DVAR_NAME("2d_val");
  const std::string PIO_3DVAR_NAME("3d_val");
  const std::string PIO_4DVAR_NAME("4d_val");
  const int MAX_ROWS = 10;
  const int MAX_COLS = 10;
  const int MAX_LEVS = 3;
  const int MAX_TIMES = 3;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, MD_SLICE_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + MD_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }

  int row_dim = -1, col_dim = -1, lev_dim = -1, time_dim = -1;
  ret = PIOc_def_dim(ncid, "nrows", MAX_ROWS, &row_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "ncols", MAX_COLS, &col_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "nlevs", MAX_LEVS, &lev_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "timesteps", MAX_TIMES, &time_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int dimids_2d[2] = {time_dim, row_dim};
  int dimids_3d[3] = {time_dim, col_dim, row_dim};
  int dimids_4d[4] = {time_dim, lev_dim, col_dim, row_dim};

  int pio_2dvar = -1, pio_3dvar = -1, pio_4dvar = -1;
  ret = PIOc_def_var(ncid, PIO_2DVAR_NAME.c_str(), Pio_type_map<T>::value, 2, dimids_2d, &pio_2dvar);
  if(!check_ret(tf, ret, "Failed to define var:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_3DVAR_NAME.c_str(), Pio_type_map<T>::value, 3, dimids_3d, &pio_3dvar);
  if(!check_ret(tf, ret, "Failed to define var:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_4DVAR_NAME.c_str(), Pio_type_map<T>::value, 4, dimids_4d, &pio_4dvar);
  if(!check_ret(tf, ret, "Failed to define var:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* Values are a simple sequential count (1, 2, 3, ...) over the flattened
   * (time, ..., col, row) index space, with row the fastest-varying index -
   * i.e. the expected value at flat index i (0-based) is i + 1. This mirrors
   * the Fortran source's per-timestep offset values (pval_1d/pval_2d/pval_3d
   * plus a per-timestep pval_start) while keeping the C++ port simple. */
  const std::size_t slice_2d = static_cast<std::size_t>(MAX_ROWS);
  const std::size_t slice_3d = static_cast<std::size_t>(MAX_ROWS) * MAX_COLS;
  const std::size_t slice_4d = static_cast<std::size_t>(MAX_ROWS) * MAX_COLS * MAX_LEVS;

  /* Put 2d/3d/4d vals, one timestep at a time */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    std::vector<T> pval_2d(slice_2d);
    for(std::size_t i = 0; i < slice_2d; i++){
      pval_2d[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_2d + i + 1);
    }
    PIO_Offset start_2d[2] = {tstep, 0};
    PIO_Offset count_2d[2] = {1, MAX_ROWS};
    ret = PIOc_put_vara(ncid, pio_2dvar, start_2d, count_2d, pval_2d.data());
    if(!check_ret(tf, ret, "Failed to put 2d var:" + MD_SLICE_FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
      return PIO_EINTERNAL;
    }

    std::vector<T> pval_3d(slice_3d);
    for(std::size_t i = 0; i < slice_3d; i++){
      pval_3d[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_3d + i + 1);
    }
    PIO_Offset start_3d[3] = {tstep, 0, 0};
    PIO_Offset count_3d[3] = {1, MAX_COLS, MAX_ROWS};
    ret = PIOc_put_vara(ncid, pio_3dvar, start_3d, count_3d, pval_3d.data());
    if(!check_ret(tf, ret, "Failed to put 3d var:" + MD_SLICE_FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
      return PIO_EINTERNAL;
    }

    std::vector<T> pval_4d(slice_4d);
    for(std::size_t i = 0; i < slice_4d; i++){
      pval_4d[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_4d + i + 1);
    }
    PIO_Offset start_4d[4] = {tstep, 0, 0, 0};
    PIO_Offset count_4d[4] = {1, MAX_LEVS, MAX_COLS, MAX_ROWS};
    ret = PIOc_put_vara(ncid, pio_4dvar, start_4d, count_4d, pval_4d.data());
    if(!check_ret(tf, ret, "Failed to put 4d var:" + MD_SLICE_FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + MD_SLICE_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, MD_SLICE_FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + MD_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_2DVAR_NAME.c_str(), &pio_2dvar);
  if(!check_ret(tf, ret, "Failed to inq 2d var:" + MD_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_3DVAR_NAME.c_str(), &pio_3dvar);
  if(!check_ret(tf, ret, "Failed to inq 3d var:" + MD_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_4DVAR_NAME.c_str(), &pio_4dvar);
  if(!check_ret(tf, ret, "Failed to inq 4d var:" + MD_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  std::vector<T> gval_2d(slice_2d * MAX_TIMES, static_cast<T>(0));
  ret = PIOc_get_var(ncid, pio_2dvar, gval_2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d var:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  std::vector<T> exp_val_2d(slice_2d * MAX_TIMES);
  for(std::size_t i = 0; i < exp_val_2d.size(); i++){
    exp_val_2d[i] = static_cast<T>(i + 1);
  }
  ok = check_val(tf, gval_2d, exp_val_2d, "Got wrong value (2d var)") && ok;

  std::vector<T> gval_3d(slice_3d * MAX_TIMES, static_cast<T>(0));
  ret = PIOc_get_var(ncid, pio_3dvar, gval_3d.data());
  if(!check_ret(tf, ret, "Failed to get 3d var:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  std::vector<T> exp_val_3d(slice_3d * MAX_TIMES);
  for(std::size_t i = 0; i < exp_val_3d.size(); i++){
    exp_val_3d[i] = static_cast<T>(i + 1);
  }
  ok = check_val(tf, gval_3d, exp_val_3d, "Got wrong value (3d var)") && ok;

  std::vector<T> gval_4d(slice_4d * MAX_TIMES, static_cast<T>(0));
  ret = PIOc_get_var(ncid, pio_4dvar, gval_4d.data());
  if(!check_ret(tf, ret, "Failed to get 4d var:" + MD_SLICE_FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  /* check_val() compares flat vectors - compare the 4d var one timestep (one
   * flattened (lev, col, row) slice) at a time rather than building a single
   * rank-4-aware comparison, since a single flat comparison across all 4
   * dims is unnecessarily complex to build here; this mirrors how the
   * Fortran source itself must special-case the 4d comparison. */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    std::vector<T> gval_4d_slice(gval_4d.begin() + static_cast<std::size_t>(tstep) * slice_4d,
        gval_4d.begin() + static_cast<std::size_t>(tstep + 1) * slice_4d);
    std::vector<T> exp_val_4d_slice(slice_4d);
    for(std::size_t i = 0; i < slice_4d; i++){
      exp_val_4d_slice[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_4d + i + 1);
    }
    ok = check_val(tf, gval_4d_slice, exp_val_4d_slice, "Got wrong value (4d var)") && ok;
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), MD_SLICE_FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_md2mdplus1_var(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_md2mdplus1_var : ") + iotype_name + "\n");

    if(test_put_get_md2mdplus1_var_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Put and get a 6d var, one time slice at a time (ne4pg2 + EAMXX write out
 * similar 6d vars) */
template<typename T>
static int test_put_get_6d_var_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string SIX_D_SLICE_FNAME("test_pio_ncdf_get_put_6d_slice.testfile");
  const std::string PIO_6DVAR_NAME("6d_val");
  /* There are 6 dims in the variable, but only 5 unique dims - the gp dim
   * (gp_dim below) is reused, as the SAME dimension id, for both of the
   * variable's two "gp" axes (not two distinct dims that happen to share a
   * length). */
  const int MAX_LEVS = 9;
  const int MAX_GP = 4;
  const int MAX_DIM2 = 2;
  const int MAX_ELEMS = 6;
  const int MAX_TIMES = 3;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, SIX_D_SLICE_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + SIX_D_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }

  int lev_dim = -1, gp_dim = -1, dim2_dim = -1, elems_dim = -1, time_dim = -1;
  ret = PIOc_def_dim(ncid, "nlevs", MAX_LEVS, &lev_dim);
  if(!check_ret(tf, ret, "Failed to define nlevs dim:" + SIX_D_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "gp", MAX_GP, &gp_dim);
  if(!check_ret(tf, ret, "Failed to define gp dim:" + SIX_D_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "dim2", MAX_DIM2, &dim2_dim);
  if(!check_ret(tf, ret, "Failed to define dim2 dim:" + SIX_D_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "elems", MAX_ELEMS, &elems_dim);
  if(!check_ret(tf, ret, "Failed to define elems dim:" + SIX_D_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "time", PIO_UNLIMITED, &time_dim);
  if(!check_ret(tf, ret, "Failed to define time dim:" + SIX_D_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* gp_dim is reused for both "gp" axes of the 6d var - see comment above. */
  int dimids_6d[6] = {time_dim, elems_dim, dim2_dim, gp_dim, gp_dim, lev_dim};
  int pio_6dvar = -1;
  ret = PIOc_def_var(ncid, PIO_6DVAR_NAME.c_str(), Pio_type_map<T>::value, 6, dimids_6d, &pio_6dvar);
  if(!check_ret(tf, ret, "Failed to define 6d var:" + SIX_D_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + SIX_D_SLICE_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  const std::size_t slice_6d = static_cast<std::size_t>(MAX_LEVS) * MAX_GP * MAX_GP * MAX_DIM2 * MAX_ELEMS;

  /* The Fortran source computes exp_val_6d(:,:,:,:,:,tstep) inside a 5-level
   * nested loop over (i,j,k,l,m), but assigns the RHS - a scalar expression
   * in i,j,k,l,m,tstep - to the *entire* slice (colon subscripts) on every
   * iteration, so the slice ends up uniformly filled with the value from the
   * loops' final iteration (i=MAX_LEVS, j=MAX_GP, k=MAX_GP, l=MAX_DIM2,
   * m=MAX_ELEMS). This port reproduces that same per-timestep constant value
   * directly instead of re-running the (self-overwriting) nested loop. */
  const PIO_Offset base = MAX_LEVS + MAX_GP * MAX_LEVS + MAX_GP * (MAX_GP * MAX_LEVS)
      + MAX_DIM2 * (MAX_GP * MAX_GP * MAX_LEVS)
      + MAX_ELEMS * (MAX_DIM2 * MAX_GP * MAX_GP * MAX_LEVS);
  const PIO_Offset tstep_coeff = MAX_ELEMS * MAX_DIM2 * MAX_GP * MAX_GP * MAX_LEVS;

  /* Put the 6d val, one timestep at a time */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    T pval = static_cast<T>(base + static_cast<PIO_Offset>(tstep + 1) * tstep_coeff);
    std::vector<T> pval_6d(slice_6d, pval);

    PIO_Offset start[6] = {tstep, 0, 0, 0, 0, 0};
    PIO_Offset count[6] = {1, MAX_ELEMS, MAX_DIM2, MAX_GP, MAX_GP, MAX_LEVS};
    ret = PIOc_put_vara(ncid, pio_6dvar, start, count, pval_6d.data());
    if(!check_ret(tf, ret, "Failed to put 6d var:" + SIX_D_SLICE_FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + SIX_D_SLICE_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, SIX_D_SLICE_FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + SIX_D_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_6DVAR_NAME.c_str(), &pio_6dvar);
  if(!check_ret(tf, ret, "Failed to inq 6d var:" + SIX_D_SLICE_FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  /* Get the 6d val back, one timestep at a time (hyperslab), matching the
   * Fortran source's per-timestep PIO_get_var() calls for this sub. */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    PIO_Offset start[6] = {tstep, 0, 0, 0, 0, 0};
    PIO_Offset count[6] = {1, MAX_ELEMS, MAX_DIM2, MAX_GP, MAX_GP, MAX_LEVS};
    std::vector<T> gval_6d(slice_6d, static_cast<T>(0));
    ret = PIOc_get_vara(ncid, pio_6dvar, start, count, gval_6d.data());
    if(!check_ret(tf, ret, "Failed to get 6d var time slice:" + SIX_D_SLICE_FNAME)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }

    T pval = static_cast<T>(base + static_cast<PIO_Offset>(tstep + 1) * tstep_coeff);
    std::vector<T> exp_val_6d(slice_6d, pval);
    ok = check_val(tf, gval_6d, exp_val_6d, "Got wrong value (6d var)") && ok;
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), SIX_D_SLICE_FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_6d_var(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_6d_var : ") + iotype_name + "\n");

    if(test_put_get_6d_var_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Similar to test_put_get_md2mdplus1_var, but uses an unlimited time
 * dimension instead of a fixed-size one */
template<typename T>
static int test_put_get_md2mdplus1_rec_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string MD_SLICE_REC_FNAME("test_pio_ncdf_get_put_md_slice_rec.testfile");
  const std::string PIO_2DVAR_NAME("2d_val");
  const std::string PIO_3DVAR_NAME("3d_val");
  const std::string PIO_4DVAR_NAME("4d_val");
  const int MAX_ROWS = 10;
  const int MAX_COLS = 10;
  const int MAX_LEVS = 3;
  const int MAX_TIMES = 3;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, MD_SLICE_REC_FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + MD_SLICE_REC_FNAME)){
    return PIO_EINTERNAL;
  }

  int row_dim = -1, col_dim = -1, lev_dim = -1, time_dim = -1;
  ret = PIOc_def_dim(ncid, "nrows", MAX_ROWS, &row_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "ncols", MAX_COLS, &col_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "nlevs", MAX_LEVS, &lev_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "timesteps", PIO_UNLIMITED, &time_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  int dimids_2d[2] = {time_dim, row_dim};
  int dimids_3d[3] = {time_dim, col_dim, row_dim};
  int dimids_4d[4] = {time_dim, lev_dim, col_dim, row_dim};

  int pio_2dvar = -1, pio_3dvar = -1, pio_4dvar = -1;
  ret = PIOc_def_var(ncid, PIO_2DVAR_NAME.c_str(), Pio_type_map<T>::value, 2, dimids_2d, &pio_2dvar);
  if(!check_ret(tf, ret, "Failed to define var:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_3DVAR_NAME.c_str(), Pio_type_map<T>::value, 3, dimids_3d, &pio_3dvar);
  if(!check_ret(tf, ret, "Failed to define var:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_4DVAR_NAME.c_str(), Pio_type_map<T>::value, 4, dimids_4d, &pio_4dvar);
  if(!check_ret(tf, ret, "Failed to define var:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* See test_put_get_md2mdplus1_var_one_iotype() above - values are a simple
   * sequential count (1, 2, 3, ...) over the flattened (time, ..., col, row)
   * index space, with row the fastest-varying index. */
  const std::size_t slice_2d = static_cast<std::size_t>(MAX_ROWS);
  const std::size_t slice_3d = static_cast<std::size_t>(MAX_ROWS) * MAX_COLS;
  const std::size_t slice_4d = static_cast<std::size_t>(MAX_ROWS) * MAX_COLS * MAX_LEVS;

  /* Put 2d/3d/4d vals, one timestep at a time */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    std::vector<T> pval_2d(slice_2d);
    for(std::size_t i = 0; i < slice_2d; i++){
      pval_2d[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_2d + i + 1);
    }
    PIO_Offset start_2d[2] = {tstep, 0};
    PIO_Offset count_2d[2] = {1, MAX_ROWS};
    ret = PIOc_put_vara(ncid, pio_2dvar, start_2d, count_2d, pval_2d.data());
    if(!check_ret(tf, ret, "Failed to put 2d var:" + MD_SLICE_REC_FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
      return PIO_EINTERNAL;
    }

    std::vector<T> pval_3d(slice_3d);
    for(std::size_t i = 0; i < slice_3d; i++){
      pval_3d[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_3d + i + 1);
    }
    PIO_Offset start_3d[3] = {tstep, 0, 0};
    PIO_Offset count_3d[3] = {1, MAX_COLS, MAX_ROWS};
    ret = PIOc_put_vara(ncid, pio_3dvar, start_3d, count_3d, pval_3d.data());
    if(!check_ret(tf, ret, "Failed to put 3d var:" + MD_SLICE_REC_FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
      return PIO_EINTERNAL;
    }

    std::vector<T> pval_4d(slice_4d);
    for(std::size_t i = 0; i < slice_4d; i++){
      pval_4d[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_4d + i + 1);
    }
    PIO_Offset start_4d[4] = {tstep, 0, 0, 0};
    PIO_Offset count_4d[4] = {1, MAX_LEVS, MAX_COLS, MAX_ROWS};
    ret = PIOc_put_vara(ncid, pio_4dvar, start_4d, count_4d, pval_4d.data());
    if(!check_ret(tf, ret, "Failed to put 4d var:" + MD_SLICE_REC_FNAME)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + MD_SLICE_REC_FNAME)){
    PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, MD_SLICE_REC_FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + MD_SLICE_REC_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_2DVAR_NAME.c_str(), &pio_2dvar);
  if(!check_ret(tf, ret, "Failed to inq 2d var:" + MD_SLICE_REC_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_3DVAR_NAME.c_str(), &pio_3dvar);
  if(!check_ret(tf, ret, "Failed to inq 3d var:" + MD_SLICE_REC_FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, PIO_4DVAR_NAME.c_str(), &pio_4dvar);
  if(!check_ret(tf, ret, "Failed to inq 4d var:" + MD_SLICE_REC_FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  std::vector<T> gval_2d(slice_2d * MAX_TIMES, static_cast<T>(0));
  ret = PIOc_get_var(ncid, pio_2dvar, gval_2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d var:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  std::vector<T> exp_val_2d(slice_2d * MAX_TIMES);
  for(std::size_t i = 0; i < exp_val_2d.size(); i++){
    exp_val_2d[i] = static_cast<T>(i + 1);
  }
  ok = check_val(tf, gval_2d, exp_val_2d, "Got wrong value (2d var)") && ok;

  std::vector<T> gval_3d(slice_3d * MAX_TIMES, static_cast<T>(0));
  ret = PIOc_get_var(ncid, pio_3dvar, gval_3d.data());
  if(!check_ret(tf, ret, "Failed to get 3d var:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  std::vector<T> exp_val_3d(slice_3d * MAX_TIMES);
  for(std::size_t i = 0; i < exp_val_3d.size(); i++){
    exp_val_3d[i] = static_cast<T>(i + 1);
  }
  ok = check_val(tf, gval_3d, exp_val_3d, "Got wrong value (3d var)") && ok;

  std::vector<T> gval_4d(slice_4d * MAX_TIMES, static_cast<T>(0));
  ret = PIOc_get_var(ncid, pio_4dvar, gval_4d.data());
  if(!check_ret(tf, ret, "Failed to get 4d var:" + MD_SLICE_REC_FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  /* See test_put_get_md2mdplus1_var_one_iotype() above for why the 4d var
   * comparison is done one timestep (one flattened (lev, col, row) slice) at
   * a time. */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    std::vector<T> gval_4d_slice(gval_4d.begin() + static_cast<std::size_t>(tstep) * slice_4d,
        gval_4d.begin() + static_cast<std::size_t>(tstep + 1) * slice_4d);
    std::vector<T> exp_val_4d_slice(slice_4d);
    for(std::size_t i = 0; i < slice_4d; i++){
      exp_val_4d_slice[i] = static_cast<T>(static_cast<std::size_t>(tstep) * slice_4d + i + 1);
    }
    ok = check_val(tf, gval_4d_slice, exp_val_4d_slice, "Got wrong value (4d var)") && ok;
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), MD_SLICE_REC_FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_md2mdplus1_rec(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_md2mdplus1_rec : ") + iotype_name + "\n");

    if(test_put_get_md2mdplus1_rec_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
static int test_put_get_time_scalar_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string FNAME_TS("test_pio_ncdf_get_put_time_scalar.testfile");
  const std::string DIM_NAME("timesteps");
  const std::string VAR_NAME("dummy_scalar_var_over_time");
  const int MAX_TIMES = 3;

  std::vector<T> gval(MAX_TIMES, static_cast<T>(0));
  std::vector<T> exp_val(MAX_TIMES, static_cast<T>(0));

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME_TS.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME_TS)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), MAX_TIMES, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME_TS)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  int pio_var = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME_TS)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME_TS)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  /* Put scalar vals, one timestep at a time */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    const PIO_Offset start[1] = {static_cast<PIO_Offset>(tstep)};
    const PIO_Offset count[1] = {1};
    exp_val[tstep] = static_cast<T>(tstep);
    T pval = static_cast<T>(tstep);
    ret = PIOc_put_vara(ncid, pio_var, start, count, &pval);
    if(!check_ret(tf, ret, "Failed to put scalar var over time:" + FNAME_TS)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME_TS)){
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME_TS.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME_TS)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to inq scalar var over time" + FNAME_TS)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  ret = PIOc_get_var(ncid, pio_var, gval.data());
  if(!check_ret(tf, ret, "Failed to get scalar var over time:" + FNAME_TS)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  bool ok = check_val(tf, gval, exp_val, "Got wrong value (scalar var over time)");

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_time_scalar(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_time_scalar : ") + iotype_name + "\n");

    if(test_put_get_time_scalar_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Similar to test_put_get_time_scalar, but uses an unlimited time dimension
 * instead */
template<typename T>
static int test_put_get_time_scalar_rec_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string FNAME_TS("test_pio_ncdf_get_put_time_scalar_rec.testfile");
  const std::string DIM_NAME("timesteps");
  const std::string VAR_NAME("dummy_scalar_var_over_time");
  const int MAX_TIMES = 3;

  std::vector<T> gval(MAX_TIMES, static_cast<T>(0));
  std::vector<T> exp_val(MAX_TIMES, static_cast<T>(0));

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME_TS.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME_TS)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, DIM_NAME.c_str(), PIO_UNLIMITED, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME_TS)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  int pio_var = -1;
  ret = PIOc_def_var(ncid, VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
  if(!check_ret(tf, ret, "Failed to define var:" + FNAME_TS)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME_TS)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  /* Put scalar vals, one timestep at a time */
  for(int tstep = 0; tstep < MAX_TIMES; tstep++){
    const PIO_Offset start[1] = {static_cast<PIO_Offset>(tstep)};
    const PIO_Offset count[1] = {1};
    exp_val[tstep] = static_cast<T>(tstep);
    T pval = static_cast<T>(tstep);
    ret = PIOc_put_vara(ncid, pio_var, start, count, &pval);
    if(!check_ret(tf, ret, "Failed to put scalar var over time:" + FNAME_TS)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME_TS)){
    PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME_TS.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME_TS)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR_NAME.c_str(), &pio_var);
  if(!check_ret(tf, ret, "Failed to inq scalar var over time" + FNAME_TS)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  ret = PIOc_get_var(ncid, pio_var, gval.data());
  if(!check_ret(tf, ret, "Failed to get scalar var over time:" + FNAME_TS)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  bool ok = check_val(tf, gval, exp_val, "Got wrong value (scalar var over time)");

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME_TS.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_time_scalar_rec(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_time_scalar_rec : ") + iotype_name + "\n");

    if(test_put_get_time_scalar_rec_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Builds the fixed-length (str_len chars, blank padded) string
 * "DUMMY_STRING, index = NN" matching the Fortran test's
 * WRITE(exp_str, '(a, i2)') 'DUMMY_STRING, index = ', index
 * (a 2-char-wide, right-justified index field, the rest of the
 * str_len-character string padded with trailing blanks). */
static std::string make_dummy_str(int index, std::size_t str_len)
{
  std::ostringstream oss;
  oss << "DUMMY_STRING, index = ";
  if(index < 10){
    oss << ' ' << index;
  }
  else{
    oss << index;
  }
  std::string s = oss.str();
  s.resize(str_len, ' ');
  return s;
}

static int test_put_get_1d_str_arr_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int STR_LEN = 64;
  const int NUM_STRS = 32;
  const int StrKIND = 512;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int strlen_dim = -1, nstrs_dim = -1;
  ret = PIOc_def_dim(ncid, "StrLen", STR_LEN, &strlen_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "nstrs", NUM_STRS, &nstrs_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* The dims passed to PIOc_def_var() are in "C order" (slowest to
   * fastest varying) - the reverse of the Fortran order in which
   * pio_dims (StrLen, nstrs) is declared in the Fortran test. */
  const int dimids[2] = {nstrs_dim, strlen_dim};

  int pio_str_arr_var1 = -1, pio_str_arr_var2 = -1;
  ret = PIOc_def_var(ncid, "var1_1d_str_arr", PIO_CHAR, 2, dimids, &pio_str_arr_var1);
  if(!check_ret(tf, ret, "Failed to define string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, "var2_1d_str_arr", PIO_CHAR, 2, dimids, &pio_str_arr_var2);
  if(!check_ret(tf, ret, "Failed to define string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* A contiguous buffer of NUM_STRS strings, each STR_LEN chars (blank
   * padded, no NUL terminators) - this is exactly the layout
   * pio_put_var_1d_text()/pio_put_vara_1d_text() produce internally. */
  std::vector<char> pbuf(static_cast<std::size_t>(NUM_STRS) * STR_LEN);
  for(int index = 1; index <= NUM_STRS; index++){
    std::string s = make_dummy_str(index, STR_LEN);
    std::copy(s.begin(), s.end(), pbuf.begin() + static_cast<std::size_t>(index - 1) * STR_LEN);
  }

  /* This corresponds to the Fortran PIO_put_var(file, var, val), which
   * calls put_var_1d_text() */
  ret = PIOc_put_var(ncid, pio_str_arr_var1, pbuf.data());
  if(!check_ret(tf, ret, "Failed to put string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  const PIO_Offset full_start[2] = {0, 0};
  const PIO_Offset full_count[2] = {NUM_STRS, STR_LEN};

  /* This corresponds to the Fortran PIO_put_var(file, var, start, count, val),
   * which calls put_vara_1d_text() - same data as above, hyperslab API path */
  ret = PIOc_put_vara(ncid, pio_str_arr_var2, full_start, full_count, pbuf.data());
  if(!check_ret(tf, ret, "Failed to put string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, "var1_1d_str_arr", &pio_str_arr_var1);
  if(!check_ret(tf, ret, "Failed to inq string array var:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, "var2_1d_str_arr", &pio_str_arr_var2);
  if(!check_ret(tf, ret, "Failed to inq string array var:" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  /* This corresponds to the Fortran PIO_get_var(file, var, val), which
   * calls get_var_1d_text() */
  std::vector<char> gbuf1(static_cast<std::size_t>(NUM_STRS) * STR_LEN, ' ');
  ret = PIOc_get_var(ncid, pio_str_arr_var1, gbuf1.data());
  if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  for(int index = 1; index <= NUM_STRS; index++){
    std::string exp_str = make_dummy_str(index, STR_LEN);
    std::string res_str(gbuf1.begin() + static_cast<std::size_t>(index - 1) * STR_LEN,
        gbuf1.begin() + static_cast<std::size_t>(index) * STR_LEN);
    ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;
  }

  /* This corresponds to the Fortran PIO_get_var(file, var, start, count, val),
   * which calls get_vara_1d_text() */
  std::vector<char> gbuf2(static_cast<std::size_t>(NUM_STRS) * STR_LEN, ' ');
  ret = PIOc_get_vara(ncid, pio_str_arr_var2, full_start, full_count, gbuf2.data());
  if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  for(int index = 1; index <= NUM_STRS; index++){
    std::string exp_str = make_dummy_str(index, STR_LEN);
    std::string res_str(gbuf2.begin() + static_cast<std::size_t>(index - 1) * STR_LEN,
        gbuf2.begin() + static_cast<std::size_t>(index) * STR_LEN);
    ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;
  }

  /* Read one string at a time, since the sizes differ (i.e. STR_LEN !=
   * StrKIND) - this calls get_vara_1d_text() too, but with a user
   * buffer (StrKIND chars) much larger than the on-disk string length
   * (STR_LEN chars), to verify that only STR_LEN chars are copied and
   * the rest of the buffer is left untouched (no overrun/mis-copy). */
  const PIO_Offset single_count[2] = {1, STR_LEN};
  for(int index = 1; index <= NUM_STRS; index++){
    std::string exp_str = make_dummy_str(index, STR_LEN);
    const PIO_Offset single_start[2] = {static_cast<PIO_Offset>(index - 1), 0};

    std::vector<char> single_str_buf(static_cast<std::size_t>(StrKIND), ' ');
    ret = PIOc_get_vara(ncid, pio_str_arr_var1, single_start, single_count, single_str_buf.data());
    if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
    std::string res_str(single_str_buf.begin(), single_str_buf.begin() + STR_LEN);
    ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;

    single_str_buf.assign(static_cast<std::size_t>(StrKIND), ' ');
    ret = PIOc_get_vara(ncid, pio_str_arr_var2, single_start, single_count, single_str_buf.data());
    if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
    res_str.assign(single_str_buf.begin(), single_str_buf.begin() + STR_LEN);
    ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_put_get_1d_str_arr(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_1d_str_arr : ") + iotype_name + "\n");

    if(test_put_get_1d_str_arr_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_put_get_2d_str_arr_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int STR_LEN = 64;
  const int NUM_ROWS = 4;
  const int NUM_COLS = 8;
  const int StrKIND = 512;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME)){
    return PIO_EINTERNAL;
  }

  int strlen_dim = -1, col_dim = -1, row_dim = -1;
  ret = PIOc_def_dim(ncid, "StrLen", STR_LEN, &strlen_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "col", NUM_COLS, &col_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, "row", NUM_ROWS, &row_dim);
  if(!check_ret(tf, ret, "Failed to define dim:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* The dims passed to PIOc_def_var() are in "C order" (slowest to
   * fastest varying) - the reverse of the Fortran order in which
   * pio_dims (StrLen, col, row) is declared in the Fortran test. */
  const int dimids[3] = {row_dim, col_dim, strlen_dim};

  int pio_str_arr_var1 = -1, pio_str_arr_var2 = -1;
  ret = PIOc_def_var(ncid, "var1_2d_str_arr", PIO_CHAR, 3, dimids, &pio_str_arr_var1);
  if(!check_ret(tf, ret, "Failed to define string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, "var2_2d_str_arr", PIO_CHAR, 3, dimids, &pio_str_arr_var2);
  if(!check_ret(tf, ret, "Failed to define string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  /* Fill the write buffer using Fortran-native column-major indexing -
   * the Fortran test fills pstr_arr(row, col), a column-major (Fortran)
   * array, and pio_put_var_2d_text()/pio_put_vara_2d_text() flatten it
   * into a contiguous buffer in that same column-major (col outer, row
   * inner) order. */
  std::vector<char> pbuf(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * STR_LEN);
  for(int col = 1; col <= NUM_COLS; col++){
    for(int row = 1; row <= NUM_ROWS; row++){
      /* Array storage in Fortran is column-major */
      int index = (col - 1) * NUM_ROWS + row;
      std::string s = make_dummy_str(index, STR_LEN);
      std::size_t cstr_idx = static_cast<std::size_t>(col - 1) * NUM_ROWS + (row - 1);
      std::copy(s.begin(), s.end(), pbuf.begin() + cstr_idx * STR_LEN);
    }
  }

  /* This corresponds to the Fortran PIO_put_var(file, var, val), which
   * calls put_var_2d_text() */
  ret = PIOc_put_var(ncid, pio_str_arr_var1, pbuf.data());
  if(!check_ret(tf, ret, "Failed to put string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  const PIO_Offset full_start[3] = {0, 0, 0};
  const PIO_Offset full_count[3] = {NUM_ROWS, NUM_COLS, STR_LEN};

  /* This corresponds to the Fortran PIO_put_var(file, var, start, count, val),
   * which calls put_vara_2d_text() - same data as above, hyperslab API path */
  ret = PIOc_put_vara(ncid, pio_str_arr_var2, full_start, full_count, pbuf.data());
  if(!check_ret(tf, ret, "Failed to put string array var:" + FNAME)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME)){
    PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, "var1_2d_str_arr", &pio_str_arr_var1);
  if(!check_ret(tf, ret, "Failed to inq string array var:" + FNAME)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, "var2_2d_str_arr", &pio_str_arr_var2);
  if(!check_ret(tf, ret, "Failed to inq string array var:" + FNAME)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  /* This corresponds to the Fortran PIO_get_var(file, var, val), which
   * calls get_var_2d_text() */
  std::vector<char> gbuf1(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * STR_LEN, ' ');
  ret = PIOc_get_var(ncid, pio_str_arr_var1, gbuf1.data());
  if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  for(int col = 1; col <= NUM_COLS; col++){
    for(int row = 1; row <= NUM_ROWS; row++){
      int index = (col - 1) * NUM_ROWS + row;
      std::string exp_str = make_dummy_str(index, STR_LEN);
      std::size_t cstr_idx = static_cast<std::size_t>(col - 1) * NUM_ROWS + (row - 1);
      std::string res_str(gbuf1.begin() + cstr_idx * STR_LEN, gbuf1.begin() + (cstr_idx + 1) * STR_LEN);
      ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;
    }
  }

  /* This corresponds to the Fortran PIO_get_var(file, var, start, count, val),
   * which calls get_vara_2d_text() */
  std::vector<char> gbuf2(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * STR_LEN, ' ');
  ret = PIOc_get_vara(ncid, pio_str_arr_var2, full_start, full_count, gbuf2.data());
  if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }

  for(int col = 1; col <= NUM_COLS; col++){
    for(int row = 1; row <= NUM_ROWS; row++){
      int index = (col - 1) * NUM_ROWS + row;
      std::string exp_str = make_dummy_str(index, STR_LEN);
      std::size_t cstr_idx = static_cast<std::size_t>(col - 1) * NUM_ROWS + (row - 1);
      std::string res_str(gbuf2.begin() + cstr_idx * STR_LEN, gbuf2.begin() + (cstr_idx + 1) * STR_LEN);
      ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;
    }
  }

  /* Read one string at a time, since the sizes differ (i.e. STR_LEN !=
   * StrKIND) - this calls get_vara_1d_text() too (a single string is a
   * 1d hyperslab of the 2d string array), but with a user buffer
   * (StrKIND chars) much larger than the on-disk string length (STR_LEN
   * chars), to verify that only STR_LEN chars are copied and the rest
   * of the buffer is left untouched (no overrun/mis-copy). */
  const PIO_Offset single_count[3] = {1, 1, STR_LEN};
  for(int row = 1; row <= NUM_ROWS; row++){
    for(int col = 1; col <= NUM_COLS; col++){
      /* NetCDF uses C row-major convention */
      int index = (row - 1) * NUM_COLS + col;
      std::string exp_str = make_dummy_str(index, STR_LEN);
      const PIO_Offset single_start[3] = {static_cast<PIO_Offset>(row - 1),
          static_cast<PIO_Offset>(col - 1), 0};

      std::vector<char> single_str_buf(static_cast<std::size_t>(StrKIND), ' ');
      ret = PIOc_get_vara(ncid, pio_str_arr_var1, single_start, single_count, single_str_buf.data());
      if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
        PIOc_closefile(ncid);
        return PIO_EINTERNAL;
      }
      std::string res_str(single_str_buf.begin(), single_str_buf.begin() + STR_LEN);
      ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;

      single_str_buf.assign(static_cast<std::size_t>(StrKIND), ' ');
      ret = PIOc_get_vara(ncid, pio_str_arr_var2, single_start, single_count, single_str_buf.data());
      if(!check_ret(tf, ret, "Failed to get string array var:" + FNAME)){
        PIOc_closefile(ncid);
        return PIO_EINTERNAL;
      }
      res_str.assign(single_str_buf.begin(), single_str_buf.begin() + STR_LEN);
      ok = check_str(tf, res_str, exp_str, "Got wrong value") && ok;
    }
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_put_get_2d_str_arr(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_2d_str_arr : ") + iotype_name + "\n");

    if(test_put_get_2d_str_arr_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Builds "Index is: <vname>(<idx1>[,<idx2>[,<idx3>]])" (or with '[',']' when
 * requested), mirroring the Fortran VNAME_WITH_*_IDX_FMT1/FMT2 format strings
 * used in tests/general/ncdf_get_put.F90.in (vname is always exactly 5
 * chars, matching the A5 field; each index is right-justified in a width-5
 * field, matching I5), then blank-pads (or truncates) the result to exactly
 * len chars - matching a Fortran CHARACTER(LEN=len) assignment. */
static std::string make_idx_str(const std::string &vname, char lbracket, char rbracket,
    const std::vector<int> &idx, std::size_t len)
{
  std::ostringstream oss;
  oss << "Index is: " << vname << lbracket;
  for(std::size_t i = 0; i < idx.size(); i++){
    if(i > 0){
      oss << ",";
    }
    oss << std::setw(5) << idx[i];
  }
  oss << rbracket;

  std::string res = oss.str();
  res.resize(len, ' ');
  return res;
}

/* Packs an array of (already length-padded) strings into a single flat char
 * buffer, one chunk of chunk_sz bytes per string. Only the first fill_sz
 * bytes of each chunk are copied from the string - the rest of the chunk is
 * left as the buffer's initial (zero) content. This matches how the small-
 * buffer put cases (svar0d/1d/2d) write a SHORT_STR_LEN-sized string into a
 * STR_LEN-sized on-disk slot : only the written (SHORT_STR_LEN) prefix is
 * meaningful, the rest of the slot is unspecified. */
static std::vector<char> pack_text_buf(const std::vector<std::string> &strs,
    std::size_t chunk_sz, std::size_t fill_sz)
{
  std::vector<char> buf(strs.size() * chunk_sz, '\0');
  for(std::size_t i = 0; i < strs.size(); i++){
    std::size_t n = std::min(fill_sz, strs[i].size());
    for(std::size_t j = 0; j < n; j++){
      buf[i * chunk_sz + j] = strs[i][j];
    }
  }
  return buf;
}

/* Extracts nstrs strings, each extract_sz bytes, from a flat char buffer
 * packed at chunk_sz byte strides. Used to pull the per-string content back
 * out of a get buffer - extract_sz may be smaller than chunk_sz (e.g. when
 * only the SHORT_STR_LEN-sized written prefix of a STR_LEN-sized on-disk
 * slot is being checked). */
static std::vector<std::string> unpack_text_buf(const std::vector<char> &buf,
    std::size_t chunk_sz, std::size_t nstrs, std::size_t extract_sz)
{
  std::vector<std::string> res;
  res.reserve(nstrs);
  for(std::size_t i = 0; i < nstrs; i++){
    res.push_back(std::string(buf.begin() + i * chunk_sz, buf.begin() + i * chunk_sz + extract_sz));
  }
  return res;
}

/* Misc scenarios for 0D-3D text (PIO_CHAR) variables : plain full-variable
 * put/get, put with a buffer smaller than the on-disk variable (and get it
 * back with a larger buffer), and put/get using an explicit hyperslab
 * (start/count). Ported from tests/general/ncdf_get_put.F90.in
 * (test_put_get_misc_str, L1811-2190). The four ADIOS/ADIOSC skip branches
 * present in the Fortran source (around the 3D/4D-char var put and get) are
 * dead code for this port (see the file-top comment) and are dropped
 * entirely.
 *
 * Note: svar3d and hsvar0d are defined (to match the file layout in the
 * Fortran source) but, as in the Fortran source, are never actually
 * put/get tested. */
static int test_put_get_misc_str_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string FNAME_MISC_STR("test_pio_ncdf_get_put_misc_str.testfile");

  const std::string VAR0D_NAME("var0d"), VAR1D_NAME("var1d"), VAR2D_NAME("var2d"), VAR3D_NAME("var3d");
  const std::string SVAR0D_NAME("svr0d"), SVAR1D_NAME("svr1d"), SVAR2D_NAME("svr2d"), SVAR3D_NAME("svr3d");
  const std::string HSVAR0D_NAME("hvr0d"), HSVAR1D_NAME("hvr1d"), HSVAR2D_NAME("hvr2d"), HSVAR3D_NAME("hvr3d");

  const int STR_LEN = 64;
  const int SHORT_STR_LEN = 32;
  const int LONG_STR_LEN = 128;

  const int NUM_ROWS = 4;
  const int NUM_COLS = 8;
  const int NUM_STEPS = 2;

  const std::string DIM0_NAME("slen"), DIM1_NAME("row"), DIM2_NAME("col"), DIM3_NAME("step");

  /* ===== Expected content (mirrors exp_str, exp_sstr and exp_hstr in the
   * Fortran source) ===== */
  std::string str0d_val(VAR0D_NAME);
  str0d_val.resize(STR_LEN, ' ');
  std::vector<std::string> exp_str0d(1, str0d_val);

  std::vector<std::string> exp_str1d(NUM_ROWS);
  for(int row = 0; row < NUM_ROWS; row++){
    exp_str1d[row] = make_idx_str(VAR1D_NAME, '(', ')', {row + 1}, STR_LEN);
  }

  std::vector<std::string> exp_str2d(NUM_ROWS * NUM_COLS);
  for(int col = 0; col < NUM_COLS; col++){
    for(int row = 0; row < NUM_ROWS; row++){
      exp_str2d[col * NUM_ROWS + row] = make_idx_str(VAR2D_NAME, '(', ')', {row + 1, col + 1}, STR_LEN);
    }
  }

  /* Note: The Fortran source (L1895) uses var2d_name, not var3d_name, when
   * generating exp_str3d's content - replicated verbatim here. This has no
   * effect on the test's pass/fail outcome since the same (buggy) expected
   * content is used for both the put and the later get-comparison. */
  std::vector<std::string> exp_str3d(NUM_ROWS * NUM_COLS * NUM_STEPS);
  for(int step = 0; step < NUM_STEPS; step++){
    for(int col = 0; col < NUM_COLS; col++){
      for(int row = 0; row < NUM_ROWS; row++){
        std::size_t idx = (static_cast<std::size_t>(step) * NUM_COLS + col) * NUM_ROWS + row;
        exp_str3d[idx] = make_idx_str(VAR2D_NAME, '(', ')', {row + 1, col + 1, step + 1}, STR_LEN);
      }
    }
  }

  /* Fortran L1902: exp_sstr0d = svar0d_name // "_" - the "_" suffix is
   * part of the real expected content (unlike the documented var2d/var3d
   * mixup elsewhere in this sub, this is not a bug to replicate). */
  std::string sstr0d_val(SVAR0D_NAME);
  sstr0d_val.append("_");
  sstr0d_val.resize(SHORT_STR_LEN, ' ');
  std::vector<std::string> exp_sstr0d(1, sstr0d_val);

  std::vector<std::string> exp_sstr1d(NUM_ROWS);
  for(int row = 0; row < NUM_ROWS; row++){
    exp_sstr1d[row] = make_idx_str(SVAR1D_NAME, '[', ']', {row + 1}, SHORT_STR_LEN);
  }

  std::vector<std::string> exp_sstr2d(NUM_ROWS * NUM_COLS);
  for(int col = 0; col < NUM_COLS; col++){
    for(int row = 0; row < NUM_ROWS; row++){
      exp_sstr2d[col * NUM_ROWS + row] = make_idx_str(SVAR2D_NAME, '[', ']', {row + 1, col + 1}, SHORT_STR_LEN);
    }
  }

  std::vector<std::string> exp_hstr1d(NUM_ROWS);
  for(int row = 0; row < NUM_ROWS; row++){
    exp_hstr1d[row] = make_idx_str(HSVAR1D_NAME, '(', ')', {row + 1}, STR_LEN);
  }

  std::vector<std::string> exp_hstr2d(NUM_ROWS * NUM_COLS);
  for(int col = 0; col < NUM_COLS; col++){
    for(int row = 0; row < NUM_ROWS; row++){
      exp_hstr2d[col * NUM_ROWS + row] = make_idx_str(HSVAR2D_NAME, '(', ')', {row + 1, col + 1}, STR_LEN);
    }
  }

  std::vector<std::string> exp_hstr3d(NUM_ROWS * NUM_COLS * NUM_STEPS);
  for(int step = 0; step < NUM_STEPS; step++){
    for(int col = 0; col < NUM_COLS; col++){
      for(int row = 0; row < NUM_ROWS; row++){
        std::size_t idx = (static_cast<std::size_t>(step) * NUM_COLS + col) * NUM_ROWS + row;
        exp_hstr3d[idx] = make_idx_str(HSVAR3D_NAME, '(', ')', {row + 1, col + 1, step + 1}, STR_LEN);
      }
    }
  }

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME_MISC_STR.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  int dim_slen = -1, dim_row = -1, dim_col = -1, dim_step = -1;
  ret = PIOc_def_dim(ncid, DIM0_NAME.c_str(), STR_LEN, &dim_slen);
  if(!check_ret(tf, ret, "Failed to define dim 0:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM1_NAME.c_str(), NUM_ROWS, &dim_row);
  if(!check_ret(tf, ret, "Failed to define dim 1:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM2_NAME.c_str(), NUM_COLS, &dim_col);
  if(!check_ret(tf, ret, "Failed to define dim 2:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM3_NAME.c_str(), NUM_STEPS, &dim_step);
  if(!check_ret(tf, ret, "Failed to define dim 3:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  int dimids_0d[1] = {dim_slen};
  int dimids_1d[2] = {dim_row, dim_slen};
  int dimids_2d[3] = {dim_col, dim_row, dim_slen};
  int dimids_3d[4] = {dim_step, dim_col, dim_row, dim_slen};

  int pio_var0d = -1, pio_var1d = -1, pio_var2d = -1, pio_var3d = -1;
  int pio_svar0d = -1, pio_svar1d = -1, pio_svar2d = -1, pio_svar3d = -1;
  int pio_hsvar0d = -1, pio_hsvar1d = -1, pio_hsvar2d = -1, pio_hsvar3d = -1;

  ret = PIOc_def_var(ncid, VAR0D_NAME.c_str(), PIO_CHAR, 1, dimids_0d, &pio_var0d);
  if(!check_ret(tf, ret, "Failed to define 0d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR1D_NAME.c_str(), PIO_CHAR, 2, dimids_1d, &pio_var1d);
  if(!check_ret(tf, ret, "Failed to define 1d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR2D_NAME.c_str(), PIO_CHAR, 3, dimids_2d, &pio_var2d);
  if(!check_ret(tf, ret, "Failed to define 2d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR3D_NAME.c_str(), PIO_CHAR, 4, dimids_3d, &pio_var3d);
  if(!check_ret(tf, ret, "Failed to define 3d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, SVAR0D_NAME.c_str(), PIO_CHAR, 1, dimids_0d, &pio_svar0d);
  if(!check_ret(tf, ret, "Failed to define 0d char var (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, SVAR1D_NAME.c_str(), PIO_CHAR, 2, dimids_1d, &pio_svar1d);
  if(!check_ret(tf, ret, "Failed to define 1d char var (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, SVAR2D_NAME.c_str(), PIO_CHAR, 3, dimids_2d, &pio_svar2d);
  if(!check_ret(tf, ret, "Failed to define 2d char var (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  /* svar3d is defined (to match the Fortran source's file layout) but, as in
   * the Fortran source, is never actually put/get tested. */
  ret = PIOc_def_var(ncid, SVAR3D_NAME.c_str(), PIO_CHAR, 4, dimids_3d, &pio_svar3d);
  if(!check_ret(tf, ret, "Failed to define 3d char var (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  /* hsvar0d is defined (to match the Fortran source's file layout) but, as in
   * the Fortran source, is never actually put/get tested. */
  ret = PIOc_def_var(ncid, HSVAR0D_NAME.c_str(), PIO_CHAR, 1, dimids_0d, &pio_hsvar0d);
  if(!check_ret(tf, ret, "Failed to define 0d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, HSVAR1D_NAME.c_str(), PIO_CHAR, 2, dimids_1d, &pio_hsvar1d);
  if(!check_ret(tf, ret, "Failed to define 1d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, HSVAR2D_NAME.c_str(), PIO_CHAR, 3, dimids_2d, &pio_hsvar2d);
  if(!check_ret(tf, ret, "Failed to define 2d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, HSVAR3D_NAME.c_str(), PIO_CHAR, 4, dimids_3d, &pio_hsvar3d);
  if(!check_ret(tf, ret, "Failed to define 3d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  std::vector<char> pbuf0d = pack_text_buf(exp_str0d, STR_LEN, STR_LEN);
  ret = PIOc_put_var(ncid, pio_var0d, pbuf0d.data());
  if(!check_ret(tf, ret, "Failed to put 0d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  std::vector<char> pbuf1d = pack_text_buf(exp_str1d, STR_LEN, STR_LEN);
  ret = PIOc_put_var(ncid, pio_var1d, pbuf1d.data());
  if(!check_ret(tf, ret, "Failed to put 1d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  std::vector<char> pbuf2d = pack_text_buf(exp_str2d, STR_LEN, STR_LEN);
  ret = PIOc_put_var(ncid, pio_var2d, pbuf2d.data());
  if(!check_ret(tf, ret, "Failed to put 2d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  std::vector<char> pbuf3d = pack_text_buf(exp_str3d, STR_LEN, STR_LEN);
  ret = PIOc_put_var(ncid, pio_var3d, pbuf3d.data());
  if(!check_ret(tf, ret, "Failed to put 3d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  /* Write with content (SHORT_STR_LEN bytes per string) smaller than the
   * variable's on-disk declared size (STR_LEN bytes per string). The put
   * buffer below is still STR_LEN-sized per string since PIOc_put_var always
   * writes the variable's full on-disk extent, but only the first
   * SHORT_STR_LEN bytes of each string are meaningful; the rest of each
   * on-disk string is left unspecified (not checked later). */
  std::vector<char> spbuf0d = pack_text_buf(exp_sstr0d, STR_LEN, SHORT_STR_LEN);
  ret = PIOc_put_var(ncid, pio_svar0d, spbuf0d.data());
  if(!check_ret(tf, ret, "Failed to put 0d char var with small buf (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  std::vector<char> spbuf1d = pack_text_buf(exp_sstr1d, STR_LEN, SHORT_STR_LEN);
  ret = PIOc_put_var(ncid, pio_svar1d, spbuf1d.data());
  if(!check_ret(tf, ret, "Failed to put 1d char var with small buf (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  std::vector<char> spbuf2d = pack_text_buf(exp_sstr2d, STR_LEN, SHORT_STR_LEN);
  ret = PIOc_put_var(ncid, pio_svar2d, spbuf2d.data());
  if(!check_ret(tf, ret, "Failed to put 2d char var with small buf (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  /* Write using explicit hyperslabs (start/count covering the full extent of
   * each variable). */
  PIO_Offset hsstart_1d[2] = {0, 0};
  PIO_Offset hscount_1d[2] = {static_cast<PIO_Offset>(NUM_ROWS), static_cast<PIO_Offset>(STR_LEN)};
  std::vector<char> hspbuf1d = pack_text_buf(exp_hstr1d, STR_LEN, STR_LEN);
  ret = PIOc_put_vara(ncid, pio_hsvar1d, hsstart_1d, hscount_1d, hspbuf1d.data());
  if(!check_ret(tf, ret, "Failed to put hslab of 1d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  PIO_Offset hsstart_2d[3] = {0, 0, 0};
  PIO_Offset hscount_2d[3] = {static_cast<PIO_Offset>(NUM_COLS), static_cast<PIO_Offset>(NUM_ROWS),
    static_cast<PIO_Offset>(STR_LEN)};
  std::vector<char> hspbuf2d = pack_text_buf(exp_hstr2d, STR_LEN, STR_LEN);
  ret = PIOc_put_vara(ncid, pio_hsvar2d, hsstart_2d, hscount_2d, hspbuf2d.data());
  if(!check_ret(tf, ret, "Failed to put hslab of 2d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  PIO_Offset hsstart_3d[4] = {0, 0, 0, 0};
  PIO_Offset hscount_3d[4] = {static_cast<PIO_Offset>(NUM_STEPS), static_cast<PIO_Offset>(NUM_COLS),
    static_cast<PIO_Offset>(NUM_ROWS), static_cast<PIO_Offset>(STR_LEN)};
  std::vector<char> hspbuf3d = pack_text_buf(exp_hstr3d, STR_LEN, STR_LEN);
  ret = PIOc_put_vara(ncid, pio_hsvar3d, hsstart_3d, hscount_3d, hspbuf3d.data());
  if(!check_ret(tf, ret, "Failed to put hslab of 3d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME_MISC_STR)){
    PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME_MISC_STR.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR0D_NAME.c_str(), &pio_var0d);
  if(!check_ret(tf, ret, "Failed to inq 0d char var:" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_NAME.c_str(), &pio_var1d);
  if(!check_ret(tf, ret, "Failed to inq 1d char var:" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR2D_NAME.c_str(), &pio_var2d);
  if(!check_ret(tf, ret, "Failed to inq 2d char var:" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR3D_NAME.c_str(), &pio_var3d);
  if(!check_ret(tf, ret, "Failed to inq 3d char var:" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, SVAR0D_NAME.c_str(), &pio_svar0d);
  if(!check_ret(tf, ret, "Failed to inq 0d char var (small var):" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, SVAR1D_NAME.c_str(), &pio_svar1d);
  if(!check_ret(tf, ret, "Failed to inq 1d char var (small var):" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, SVAR2D_NAME.c_str(), &pio_svar2d);
  if(!check_ret(tf, ret, "Failed to inq 2d char var (small var):" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, HSVAR1D_NAME.c_str(), &pio_hsvar1d);
  if(!check_ret(tf, ret, "Failed to inq 1d char var (hyperslab var):" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, HSVAR2D_NAME.c_str(), &pio_hsvar2d);
  if(!check_ret(tf, ret, "Failed to inq 2d char var (hyperslab var):" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, HSVAR3D_NAME.c_str(), &pio_hsvar3d);
  if(!check_ret(tf, ret, "Failed to inq 3d char var (hyperslab var):" + FNAME_MISC_STR)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  std::vector<char> gbuf0d(static_cast<std::size_t>(STR_LEN), '\0');
  ret = PIOc_get_var(ncid, pio_var0d, gbuf0d.data());
  if(!check_ret(tf, ret, "Failed to get 0d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_str0d = unpack_text_buf(gbuf0d, STR_LEN, 1, STR_LEN);
    for(std::size_t i = 0; i < exp_str0d.size(); i++){
      ok = check_str(tf, res_str0d[i], exp_str0d[i], "Got wrong value for 0d char var") && ok;
    }
  }

  std::vector<char> gbuf1d(static_cast<std::size_t>(NUM_ROWS) * STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_var1d, gbuf1d.data());
  if(!check_ret(tf, ret, "Failed to get 1d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_str1d = unpack_text_buf(gbuf1d, STR_LEN, NUM_ROWS, STR_LEN);
    for(std::size_t i = 0; i < exp_str1d.size(); i++){
      ok = check_str(tf, res_str1d[i], exp_str1d[i], "Got wrong value for 1d char var") && ok;
    }
  }

  std::vector<char> gbuf2d(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_var2d, gbuf2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_str2d = unpack_text_buf(gbuf2d, STR_LEN, NUM_ROWS * NUM_COLS, STR_LEN);
    for(std::size_t i = 0; i < exp_str2d.size(); i++){
      ok = check_str(tf, res_str2d[i], exp_str2d[i], "Got wrong value for 2d char var") && ok;
    }
  }

  std::vector<char> gbuf3d(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * NUM_STEPS * STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_var3d, gbuf3d.data());
  if(!check_ret(tf, ret, "Failed to get 3d char var:" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_str3d = unpack_text_buf(gbuf3d, STR_LEN, NUM_ROWS * NUM_COLS * NUM_STEPS, STR_LEN);
    for(std::size_t i = 0; i < exp_str3d.size(); i++){
      ok = check_str(tf, res_str3d[i], exp_str3d[i], "Got wrong value for 3d char var") && ok;
    }
  }

  /* Read back with a buffer (LONG_STR_LEN bytes per string) larger than the
   * variable's on-disk declared size (STR_LEN bytes per string). PIOc_get_var
   * always reads (and overwrites) exactly the variable's on-disk extent -
   * densely packed at STR_LEN-byte strides, not LONG_STR_LEN - so only the
   * first (STR_LEN * nstrs) bytes of the buffers below are ever touched;
   * only the SHORT_STR_LEN-byte written prefix of each STR_LEN-byte string is
   * compared below (the rest of each string's on-disk content is
   * unspecified, per the small-buffer put above). */
  std::vector<char> glbuf0d(static_cast<std::size_t>(LONG_STR_LEN), '\0');
  ret = PIOc_get_var(ncid, pio_svar0d, glbuf0d.data());
  if(!check_ret(tf, ret, "Failed to get 0d char var with large buf (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_sstr0d = unpack_text_buf(glbuf0d, STR_LEN, 1, SHORT_STR_LEN);
    for(std::size_t i = 0; i < exp_sstr0d.size(); i++){
      ok = check_str(tf, res_sstr0d[i], exp_sstr0d[i], "Got wrong value for 0d char var with large buf") && ok;
    }
  }

  std::vector<char> glbuf1d(static_cast<std::size_t>(NUM_ROWS) * LONG_STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_svar1d, glbuf1d.data());
  if(!check_ret(tf, ret, "Failed to get 1d char var with large buf (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_sstr1d = unpack_text_buf(glbuf1d, STR_LEN, NUM_ROWS, SHORT_STR_LEN);
    for(std::size_t i = 0; i < exp_sstr1d.size(); i++){
      ok = check_str(tf, res_sstr1d[i], exp_sstr1d[i], "Got wrong value for 1d char var with large buf") && ok;
    }
  }

  std::vector<char> glbuf2d(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * LONG_STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_svar2d, glbuf2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d char var with large buf (small var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_sstr2d = unpack_text_buf(glbuf2d, STR_LEN, NUM_ROWS * NUM_COLS, SHORT_STR_LEN);
    for(std::size_t i = 0; i < exp_sstr2d.size(); i++){
      ok = check_str(tf, res_sstr2d[i], exp_sstr2d[i], "Got wrong value for 2d char var with large buf") && ok;
    }
  }

  /* Hyperslab-written variables : verify with a plain (no start/count)
   * full-var get. */
  std::vector<char> hsgbuf1d(static_cast<std::size_t>(NUM_ROWS) * STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_hsvar1d, hsgbuf1d.data());
  if(!check_ret(tf, ret, "Failed to get 1d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_hstr1d = unpack_text_buf(hsgbuf1d, STR_LEN, NUM_ROWS, STR_LEN);
    for(std::size_t i = 0; i < exp_hstr1d.size(); i++){
      ok = check_str(tf, res_hstr1d[i], exp_hstr1d[i],
          "Got wrong value for 1d char var written with hslab put") && ok;
    }
  }

  std::vector<char> hsgbuf2d(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_hsvar2d, hsgbuf2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_hstr2d = unpack_text_buf(hsgbuf2d, STR_LEN, NUM_ROWS * NUM_COLS, STR_LEN);
    for(std::size_t i = 0; i < exp_hstr2d.size(); i++){
      ok = check_str(tf, res_hstr2d[i], exp_hstr2d[i],
          "Got wrong value for 2d char var written with hslab put") && ok;
    }
  }

  std::vector<char> hsgbuf3d(static_cast<std::size_t>(NUM_ROWS) * NUM_COLS * NUM_STEPS * STR_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_hsvar3d, hsgbuf3d.data());
  if(!check_ret(tf, ret, "Failed to get 3d char var (hyperslab var):" + FNAME_MISC_STR)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  {
    std::vector<std::string> res_hstr3d = unpack_text_buf(hsgbuf3d, STR_LEN, NUM_ROWS * NUM_COLS * NUM_STEPS, STR_LEN);
    for(std::size_t i = 0; i < exp_hstr3d.size(); i++){
      ok = check_str(tf, res_hstr3d[i], exp_hstr3d[i],
          "Got wrong value for 3d char var written with hslab put") && ok;
    }
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME_MISC_STR.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_put_get_misc_str(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_misc_str : ") + iotype_name + "\n");

    if(test_put_get_misc_str_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Helpers for building/comparing the "Index is: <name>(<idx>...)" style
 * strings written out by test_put_get_str_frames/test_put_get_str_idx
 * below (these mirror the Fortran VNAME_WITH_*_IDX_FMT* edit descriptors
 * in tests/general/ncdf_get_put.F90.in: A<w> is a right-justified,
 * blank-padded (or leftmost-truncated) string field and I<w> is a
 * right-justified, space-padded integer field). */
static std::string spio_fmt_idx_str1(const std::string &vname, int idx)
{
  char buf[64];
  std::snprintf(buf, sizeof(buf), "Index is: %5.5s(%5d)", vname.c_str(), idx);
  return std::string(buf);
}

static std::string spio_fmt_idx_str2(const std::string &vname, int idx0, int idx1)
{
  char buf[64];
  std::snprintf(buf, sizeof(buf), "Index is: %5.5s(%5d,%5d)", vname.c_str(), idx0, idx1);
  return std::string(buf);
}

static std::string spio_fmt_idx_str3(const std::string &vname, int idx0, int idx1, int idx2)
{
  char buf[64];
  std::snprintf(buf, sizeof(buf), "Index is: %5.5s(%5d,%5d,%5d)", vname.c_str(), idx0, idx1, idx2);
  return std::string(buf);
}

static std::string spio_fmt_idx_str_w10(const std::string &vname, int idx)
{
  char buf[64];
  std::snprintf(buf, sizeof(buf), "Index is: %10.10s(%5d)", vname.c_str(), idx);
  return std::string(buf);
}

/* Blank-pad (or truncate) a string to exactly 'len' chars - matches the
 * Fortran semantics of assigning/writing a value to a fixed-length
 * character variable. */
static std::string spio_pad_str(const std::string &s, std::size_t len)
{
  std::string out = s;
  out.resize(len, ' ');
  return out;
}

static int test_put_get_str_frames_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string FNAME_FRAMES("test_pio_ncdf_get_str_frames.testfile");

  const std::string VAR1D_NAME("var1d");
  const std::string VAR2D_NAME("var2d");
  const std::string VAR3D_NAME("var3d");

  const int STR_LEN = 64;
  const int NUM_ROWS = 4;
  const int NUM_COLS = 8;
  const int NUM_STEPS = 6;

  const std::string DIM0_NAME("slen");
  const std::string DIM1_NAME("row");
  const std::string DIM2_NAME("col");
  const std::string DIM3_NAME("step");

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME_FRAMES.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME_FRAMES)){
    return PIO_EINTERNAL;
  }

  int dim_slen = -1, dim_row = -1, dim_col = -1, dim_step = -1;
  ret = PIOc_def_dim(ncid, DIM0_NAME.c_str(), STR_LEN, &dim_slen);
  if(!check_ret(tf, ret, "Failed to define dim 0:" + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM1_NAME.c_str(), NUM_ROWS, &dim_row);
  if(!check_ret(tf, ret, "Failed to define dim 1:" + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM2_NAME.c_str(), NUM_COLS, &dim_col);
  if(!check_ret(tf, ret, "Failed to define dim 2:" + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM3_NAME.c_str(), NUM_STEPS, &dim_step);
  if(!check_ret(tf, ret, "Failed to define dim 3:" + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  int dimids1[2] = {dim_row, dim_slen};
  int dimids2[3] = {dim_col, dim_row, dim_slen};
  int dimids3[4] = {dim_step, dim_col, dim_row, dim_slen};

  int var1d = -1, var2d = -1, var3d = -1;
  ret = PIOc_def_var(ncid, VAR1D_NAME.c_str(), PIO_CHAR, 2, dimids1, &var1d);
  if(!check_ret(tf, ret, "Failed to define 1 dim char array or 1d array of strings in file " + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR2D_NAME.c_str(), PIO_CHAR, 3, dimids2, &var2d);
  if(!check_ret(tf, ret, "Failed to define 2 dim char array or 2d array of strings in file " + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR3D_NAME.c_str(), PIO_CHAR, 4, dimids3, &var3d);
  if(!check_ret(tf, ret, "Failed to define 3 dim char array or 3d array of strings in file " + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  /* Write the 1d text variable, 1 string (row) at a time. Note: the
   * content written for var1d uses VAR2D_NAME (not VAR1D_NAME) - this
   * mirrors a quirk in the original Fortran source
   * (tests/general/ncdf_get_put.F90.in), not a port defect. */
  for(int row = 0; row < NUM_ROWS; row++){
    std::string frame = spio_pad_str(spio_fmt_idx_str1(VAR2D_NAME, row + 1), STR_LEN);
    PIO_Offset start[2] = {static_cast<PIO_Offset>(row), 0};
    PIO_Offset count[2] = {1, STR_LEN};
    ret = PIOc_put_vara(ncid, var1d, start, count, frame.data());
    if(!check_ret(tf, ret, "Failed to put 1d array of strings 1 frame at a time in file " + FNAME_FRAMES)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* Write the 2d text variable, 1 array/row of strings (a full column)
   * at a time. */
  for(int col = 0; col < NUM_COLS; col++){
    std::vector<char> frame(static_cast<std::size_t>(NUM_ROWS) * STR_LEN);
    for(int row = 0; row < NUM_ROWS; row++){
      std::string s = spio_pad_str(spio_fmt_idx_str2(VAR2D_NAME, row + 1, col + 1), STR_LEN);
      std::copy(s.begin(), s.end(), frame.begin() + static_cast<std::size_t>(row) * STR_LEN);
    }
    PIO_Offset start[3] = {static_cast<PIO_Offset>(col), 0, 0};
    PIO_Offset count[3] = {1, NUM_ROWS, STR_LEN};
    ret = PIOc_put_vara(ncid, var2d, start, count, frame.data());
    if(!check_ret(tf, ret, "Failed to put 2d array of strings 1 frame at a time in file " + FNAME_FRAMES)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* Write the 3d text variable, 1 2d array (row x col) of strings at a
   * time. */
  for(int step = 0; step < NUM_STEPS; step++){
    std::vector<char> frame(static_cast<std::size_t>(NUM_COLS) * NUM_ROWS * STR_LEN);
    for(int col = 0; col < NUM_COLS; col++){
      for(int row = 0; row < NUM_ROWS; row++){
        std::string s = spio_pad_str(spio_fmt_idx_str3(VAR3D_NAME, row + 1, col + 1, step + 1), STR_LEN);
        std::size_t off = (static_cast<std::size_t>(col) * NUM_ROWS + row) * STR_LEN;
        std::copy(s.begin(), s.end(), frame.begin() + off);
      }
    }
    PIO_Offset start[4] = {static_cast<PIO_Offset>(step), 0, 0, 0};
    PIO_Offset count[4] = {1, NUM_COLS, NUM_ROWS, STR_LEN};
    ret = PIOc_put_vara(ncid, var3d, start, count, frame.data());
    if(!check_ret(tf, ret, "Failed to put 3d array of strings 1 frame at a time in file " + FNAME_FRAMES)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME_FRAMES)){
    PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME_FRAMES.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME_FRAMES)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_NAME.c_str(), &var1d);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of strings var in file " + FNAME_FRAMES)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR2D_NAME.c_str(), &var2d);
  if(!check_ret(tf, ret, "Failed to inquire 2d array of strings var in file " + FNAME_FRAMES)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR3D_NAME.c_str(), &var3d);
  if(!check_ret(tf, ret, "Failed to inquire 3d array of strings var in file " + FNAME_FRAMES)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  std::vector<char> gstr1d(static_cast<std::size_t>(NUM_ROWS) * STR_LEN);
  ret = PIOc_get_var(ncid, var1d, gstr1d.data());
  if(!check_ret(tf, ret, "Failed to get 1d char var in file " + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  for(int row = 0; row < NUM_ROWS; row++){
    std::string exp = spio_pad_str(spio_fmt_idx_str1(VAR2D_NAME, row + 1), STR_LEN);
    std::string res(gstr1d.begin() + static_cast<std::size_t>(row) * STR_LEN,
        gstr1d.begin() + static_cast<std::size_t>(row + 1) * STR_LEN);
    ok = check_str(tf, res, exp, "Got wrong value for 1d char var") && ok;
  }

  std::vector<char> gstr2d(static_cast<std::size_t>(NUM_COLS) * NUM_ROWS * STR_LEN);
  ret = PIOc_get_var(ncid, var2d, gstr2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d char var in file " + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  for(int col = 0; col < NUM_COLS; col++){
    for(int row = 0; row < NUM_ROWS; row++){
      std::string exp = spio_pad_str(spio_fmt_idx_str2(VAR2D_NAME, row + 1, col + 1), STR_LEN);
      std::size_t off = (static_cast<std::size_t>(col) * NUM_ROWS + row) * STR_LEN;
      std::string res(gstr2d.begin() + off, gstr2d.begin() + off + STR_LEN);
      ok = check_str(tf, res, exp, "Got wrong value for 2d char var") && ok;
    }
  }

  std::vector<char> gstr3d(static_cast<std::size_t>(NUM_STEPS) * NUM_COLS * NUM_ROWS * STR_LEN);
  ret = PIOc_get_var(ncid, var3d, gstr3d.data());
  if(!check_ret(tf, ret, "Failed to get 3d char var in file " + FNAME_FRAMES)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  for(int step = 0; step < NUM_STEPS; step++){
    for(int col = 0; col < NUM_COLS; col++){
      for(int row = 0; row < NUM_ROWS; row++){
        std::string exp = spio_pad_str(spio_fmt_idx_str3(VAR3D_NAME, row + 1, col + 1, step + 1), STR_LEN);
        std::size_t off = ((static_cast<std::size_t>(step) * NUM_COLS + col) * NUM_ROWS + row) * STR_LEN;
        std::string res(gstr3d.begin() + off, gstr3d.begin() + off + STR_LEN);
        ok = check_str(tf, res, exp, "Got wrong value for 3d char var") && ok;
      }
    }
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME_FRAMES.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_put_get_str_frames(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_str_frames : ") + iotype_name + "\n");

    if(test_put_get_str_frames_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Write 'nchars' chars from 's' (or fewer, if 's' is shorter) into a 2D
 * (row, slen) text variable's row, 1 PIOc_put_var1_text call per
 * character - this mirrors the literal "index, no count" single-element
 * var1 write semantics while still writing a whole string, and silently
 * writes only the first 'nchars' chars of 's' if 's' is longer
 * (truncation-on-write, the API contract being verified for var1d_trnc
 * below, not a defect). */
static int spio_put_text_var1_row(int ncid, int varid, int row, const std::string &s, int nchars)
{
  std::size_t n = std::min(s.size(), static_cast<std::size_t>(nchars));
  for(std::size_t c = 0; c < n; c++){
    PIO_Offset index[2] = {static_cast<PIO_Offset>(row), static_cast<PIO_Offset>(c)};
    int ret = PIOc_put_var1_text(ncid, varid, index, &s[c]);
    if(ret != PIO_NOERR){
      return ret;
    }
  }
  return PIO_NOERR;
}

/* Read 'nchars' chars, starting at char-position 'start_c', from a 2D
 * (row, slen) text variable's row, 1 PIOc_get_var1_text call per
 * character. */
static int spio_get_text_var1_row(int ncid, int varid, int row, int start_c, int nchars, std::string &out)
{
  out.assign(static_cast<std::size_t>(nchars), '\0');
  for(int c = 0; c < nchars; c++){
    PIO_Offset index[2] = {static_cast<PIO_Offset>(row), static_cast<PIO_Offset>(start_c + c)};
    int ret = PIOc_get_var1_text(ncid, varid, index, &out[static_cast<std::size_t>(c)]);
    if(ret != PIO_NOERR){
      return ret;
    }
  }
  return PIO_NOERR;
}

static int test_put_get_str_idx_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string FNAME_IDX("test_pio_ncdf_get_str_idx.testfile");

  const std::string VAR1D_NAME("var1d");
  const std::string VAR1D_LONG_NAME("var1d_long");
  const std::string VAR1D_SHRT_NAME("var1d_shrt");
  const std::string VAR1D_TRNC_NAME("var1d_trnc");
  const std::string VAR1D_MIDX_NAME("var1d_midx");
  const std::string VAR1D_TMPL_NAME("var1d_____");

  const int SHORT_STR_LEN = 32;
  const int STR_LEN = 64;
  const int LONG_STR_LEN = 128;
  const int NUM_ROWS = 10;

  const std::string DIM0_NAME("slen");
  const std::string DIM1_NAME("row");

  /* Char position (0-based) within the "Index is: <name>(<idx>)" pattern
   * (built with a 10-char-wide name field) at which the <name> field
   * starts - used below to overwrite just the name in-place. */
  const int NAME_IDX_IN_FMT2 = 10;

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, FNAME_IDX.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + FNAME_IDX)){
    return PIO_EINTERNAL;
  }

  int dim_slen = -1, dim_row = -1;
  ret = PIOc_def_dim(ncid, DIM0_NAME.c_str(), STR_LEN, &dim_slen);
  if(!check_ret(tf, ret, "Failed to define dim 0:" + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, DIM1_NAME.c_str(), NUM_ROWS, &dim_row);
  if(!check_ret(tf, ret, "Failed to define dim 1:" + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  int dimids[2] = {dim_row, dim_slen};
  int var1d = -1, var1d_long = -1, var1d_shrt = -1, var1d_trnc = -1, var1d_midx = -1;

  ret = PIOc_def_var(ncid, VAR1D_NAME.c_str(), PIO_CHAR, 2, dimids, &var1d);
  if(!check_ret(tf, ret, "Failed to define 1d array of strings in file " + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR1D_SHRT_NAME.c_str(), PIO_CHAR, 2, dimids, &var1d_shrt);
  if(!check_ret(tf, ret, "Failed to define 1d array of short strings in file " + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR1D_LONG_NAME.c_str(), PIO_CHAR, 2, dimids, &var1d_long);
  if(!check_ret(tf, ret, "Failed to define 1d array of long strings in file " + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR1D_TRNC_NAME.c_str(), PIO_CHAR, 2, dimids, &var1d_trnc);
  if(!check_ret(tf, ret, "Failed to define 1d array of truncatable strings in file " + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, VAR1D_MIDX_NAME.c_str(), PIO_CHAR, 2, dimids, &var1d_midx);
  if(!check_ret(tf, ret, "Failed to define 1d array of mid-index-overwritten strings in file " + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + FNAME_IDX)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  /* Write each variable 1 string at a time, 1 PIOc_put_var1_text call
   * per character - this exercises the literal "index, no count" netCDF
   * var1 semantics (1 element per call) while still writing a whole
   * string, naturally exercising short-buffer writes and
   * truncation-on-write for variants shorter/longer than STR_LEN
   * respectively. */
  for(int row = 0; row < NUM_ROWS; row++){
    std::string s = spio_pad_str(spio_fmt_idx_str1(VAR1D_NAME, row + 1), STR_LEN);
    ret = spio_put_text_var1_row(ncid, var1d, row, s, STR_LEN);
    if(!check_ret(tf, ret, "Failed to put 1d array of strings 1 string at a time to file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* Note: Fortran's pio_put_var1_text() writes only MIN(count, LEN_TRIM(str)) chars and leaves the rest as C_NULL_CHAR (not blank-padded), while spio_put_text_var1_row() below writes real blank padding for the remainder - both are self-consistent since the C API has no LEN_TRIM equivalent, so don't "fix" this to match. */
  /* The string written out is shorter than the declared string-length
   * dimension of the variable - only SHORT_STR_LEN chars are written,
   * leaving the rest of the row's string-length dimension untouched. */
  for(int row = 0; row < NUM_ROWS; row++){
    std::string s = spio_pad_str(spio_fmt_idx_str_w10(VAR1D_SHRT_NAME, row + 1), SHORT_STR_LEN);
    ret = spio_put_text_var1_row(ncid, var1d_shrt, row, s, SHORT_STR_LEN);
    if(!check_ret(tf, ret, "Failed to put 1d array of short strings 1 string at a time to file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* The string written out (LONG_STR_LEN chars, blank padded) is longer
   * than the declared string-length dimension of the variable (STR_LEN)
   * - only the first STR_LEN chars (all content, no truncated garbage)
   * are ever written. */
  for(int row = 0; row < NUM_ROWS; row++){
    std::string s = spio_pad_str(spio_fmt_idx_str_w10(VAR1D_LONG_NAME, row + 1), LONG_STR_LEN);
    ret = spio_put_text_var1_row(ncid, var1d_long, row, s, STR_LEN);
    if(!check_ret(tf, ret, "Failed to put 1d array of long strings 1 string at a time to file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* The string written out is longer than STR_LEN and, unlike the "long"
   * case above, has non-blank content (digits) all the way to its end -
   * writing it is expected to silently truncate at STR_LEN chars, not
   * error out. This is the API contract being verified here, not a
   * defect. */
  for(int row = 0; row < NUM_ROWS; row++){
    std::string content = spio_fmt_idx_str_w10(VAR1D_TRNC_NAME, row + 1);
    std::string s = content;
    s.resize(LONG_STR_LEN);
    for(std::size_t j = content.size(); j < s.size(); j++){
      s[j] = static_cast<char>('0' + ((j + 1) % 10));
    }
    ret = spio_put_text_var1_row(ncid, var1d_trnc, row, s, STR_LEN);
    if(!check_ret(tf, ret, "Failed to put 1d array of truncatable strings 1 string at a time to file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* Write out the string with the template var name first - its name
   * field is overwritten, in-place, at a non-zero string-length index
   * below. */
  for(int row = 0; row < NUM_ROWS; row++){
    std::string s = spio_pad_str(spio_fmt_idx_str_w10(VAR1D_TMPL_NAME, row + 1), STR_LEN);
    ret = spio_put_text_var1_row(ncid, var1d_midx, row, s, STR_LEN);
    if(!check_ret(tf, ret, "Failed to put 1d array of long strings 1 string at a time to file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* Re-open the file in PIO_WRITE (not PIO_NOWRITE, unlike every other
   * read-back phase below) specifically to allow modifying var1d_midx
   * in-place. */
#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME_IDX)){
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME_IDX.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME_IDX)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_MIDX_NAME.c_str(), &var1d_midx);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of mid-index-overwritten strings var in file " + FNAME_IDX)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  /* Overwrite just the name field, in-place, at a non-zero string-length
   * index - changes "Index is: var1d_____(<idx>)" to
   * "Index is: var1d_midx(<idx>)". */
  for(int row = 0; row < NUM_ROWS; row++){
    PIO_Offset index[2] = {static_cast<PIO_Offset>(row), 0};
    for(std::size_t c = 0; c < VAR1D_MIDX_NAME.size(); c++){
      index[1] = NAME_IDX_IN_FMT2 + static_cast<PIO_Offset>(c);
      ret = PIOc_put_var1_text(ncid, var1d_midx, index, &VAR1D_MIDX_NAME[c]);
      if(!check_ret(tf, ret, "Failed to overwrite name in 1d array of strings in file " + FNAME_IDX)){
        PIOc_closefile(ncid);
        PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
        return PIO_EINTERNAL;
      }
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + FNAME_IDX)){
    PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, FNAME_IDX.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + FNAME_IDX)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_NAME.c_str(), &var1d);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of strings var in file " + FNAME_IDX)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_SHRT_NAME.c_str(), &var1d_shrt);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of short strings var in file " + FNAME_IDX)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_LONG_NAME.c_str(), &var1d_long);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of long strings var in file " + FNAME_IDX)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_TRNC_NAME.c_str(), &var1d_trnc);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of truncatable strings var in file " + FNAME_IDX)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, VAR1D_MIDX_NAME.c_str(), &var1d_midx);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of mid-index-overwritten strings var in file " + FNAME_IDX)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  /* Verify by reading back exactly the extent that was written for each
   * variant (rather than the whole STR_LEN-wide row) - bytes beyond what
   * was explicitly written are left untouched by SCORPIO's default
   * NC_NOFILL fill mode and so have no defined value to compare
   * against. */
  for(int row = 0; row < NUM_ROWS; row++){
    std::string exp = spio_pad_str(spio_fmt_idx_str1(VAR1D_NAME, row + 1), STR_LEN);
    std::string res;
    ret = spio_get_text_var1_row(ncid, var1d, row, 0, STR_LEN, res);
    if(!check_ret(tf, ret, "Failed to get 1d char var in file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
    ok = check_str(tf, res, exp, "Got wrong value for 1d char var") && ok;
  }

  for(int row = 0; row < NUM_ROWS; row++){
    std::string exp = spio_pad_str(spio_fmt_idx_str_w10(VAR1D_SHRT_NAME, row + 1), SHORT_STR_LEN);
    std::string res;
    ret = spio_get_text_var1_row(ncid, var1d_shrt, row, 0, SHORT_STR_LEN, res);
    if(!check_ret(tf, ret, "Failed to get short 1d char var in file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
    ok = check_str(tf, res, exp, "Got wrong value for short 1d char var") && ok;
  }

  for(int row = 0; row < NUM_ROWS; row++){
    std::string exp = spio_pad_str(spio_fmt_idx_str_w10(VAR1D_LONG_NAME, row + 1), STR_LEN);
    std::string res;
    ret = spio_get_text_var1_row(ncid, var1d_long, row, 0, STR_LEN, res);
    if(!check_ret(tf, ret, "Failed to get long 1d char var in file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
    ok = check_str(tf, res, exp, "Got wrong value for long 1d char var") && ok;
  }

  for(int row = 0; row < NUM_ROWS; row++){
    std::string content = spio_fmt_idx_str_w10(VAR1D_TRNC_NAME, row + 1);
    std::string exp = content;
    exp.resize(STR_LEN);
    for(std::size_t j = content.size(); j < exp.size(); j++){
      exp[j] = static_cast<char>('0' + ((j + 1) % 10));
    }
    std::string res;
    ret = spio_get_text_var1_row(ncid, var1d_trnc, row, 0, STR_LEN, res);
    if(!check_ret(tf, ret, "Failed to get truncatable 1d char var in file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
    ok = check_str(tf, res, exp, "Got wrong value for truncatable 1d char var") && ok;
  }

  for(int row = 0; row < NUM_ROWS; row++){
    std::string exp = spio_pad_str(spio_fmt_idx_str_w10(VAR1D_MIDX_NAME, row + 1), STR_LEN);
    std::string res;
    ret = spio_get_text_var1_row(ncid, var1d_midx, row, 0, STR_LEN, res);
    if(!check_ret(tf, ret, "Failed to get index write of 1d char var in file " + FNAME_IDX)){
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
    ok = check_str(tf, res, exp, "Got wrong value for index write of 1d char var") && ok;
  }

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), FNAME_IDX.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_put_get_str_idx(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_str_idx : ") + iotype_name + "\n");

    if(test_put_get_str_idx_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Put and get hyperslabs of 1D/2D/3D text variables - progressively
 * rewrites parts (first the prefix, then the suffix) of already-written
 * strings using fine-grained, single-character PIOc_put_vara() hyperslabs,
 * verifying the final string contents after each rewrite phase. */
static int test_put_get_str_hslabs_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const std::string filename("test_pio_ncdf_get_str_hslabs.testfile");

  const int STR_LEN = 64;
  const int NUM_ROWS = 4;
  const int NUM_COLS = 8;
  const int NUM_STEPS = 6;

  const std::string VAR_NAME_PREFIX1("var");
  const std::string VAR_NAME_PREFIX2("v__");
  const std::string VAR_NAME_SUFFIX1("d");
  const std::string VAR_NAME_SUFFIX2("_");
  const int VAR1D_ID = 1;
  const int VAR2D_ID = 2;
  const int VAR3D_ID = 3;
  /* VAR1D_ID/VAR2D_ID/VAR3D_ID are always rendered as a single digit */
  const int ID_LEN = 1;

  const std::string dim0_name("slen");
  const std::string dim1_name("row");
  const std::string dim2_name("col");
  const std::string dim3_name("step");

  /* var*_name = VAR_NAME_PREFIX1 + VAR*D_ID + VAR_NAME_SUFFIX1 : the
   * NetCDF variable names themselves never change for the lifetime of this
   * sub - only the *contents* of the string variables are rewritten below
   * (first the prefix, then the suffix of each stored string) */
  const std::string var1d_name = VAR_NAME_PREFIX1 + std::to_string(VAR1D_ID) + VAR_NAME_SUFFIX1;
  const std::string var2d_name = VAR_NAME_PREFIX1 + std::to_string(VAR2D_ID) + VAR_NAME_SUFFIX1;
  const std::string var3d_name = VAR_NAME_PREFIX1 + std::to_string(VAR3D_ID) + VAR_NAME_SUFFIX1;

  /* Builds "name5(idx1[,idx2[,idx3]])", space-padded out to STR_LEN chars,
   * e.g. make_elem_str("var1d", {1}) => "var1d(    1)" (then space-padded) */
  auto make_elem_str = [STR_LEN](const std::string &name5, const std::vector<int> &idxs) -> std::string {
    std::string s = name5 + "(";
    for(std::size_t k = 0; k < idxs.size(); k++){
      if(k > 0){
        s += ",";
      }
      char ibuf[16];
      std::snprintf(ibuf, sizeof(ibuf), "%5d", idxs[k]);
      s += ibuf;
    }
    s += ")";
    s.resize(static_cast<std::size_t>(STR_LEN), ' ');
    return s;
  };

  /* Builds the full flat char buffer for var1d, in C (row-major) dim order,
   * slowest to fastest : [row][slen] */
  auto build_str1d = [&](const std::string &name5) -> std::vector<char> {
    std::vector<char> buf(static_cast<std::size_t>(NUM_ROWS) * STR_LEN);
    for(int row = 1; row <= NUM_ROWS; row++){
      std::string s = make_elem_str(name5, std::vector<int>{row});
      std::memcpy(buf.data() + static_cast<std::size_t>(row - 1) * STR_LEN, s.data(),
          static_cast<std::size_t>(STR_LEN));
    }
    return buf;
  };

  /* dim order, slowest to fastest : [col][row][slen] */
  auto build_str2d = [&](const std::string &name5) -> std::vector<char> {
    std::vector<char> buf(static_cast<std::size_t>(NUM_COLS) * NUM_ROWS * STR_LEN);
    for(int col = 1; col <= NUM_COLS; col++){
      for(int row = 1; row <= NUM_ROWS; row++){
        std::string s = make_elem_str(name5, std::vector<int>{row, col});
        std::size_t off = (static_cast<std::size_t>(col - 1) * NUM_ROWS +
            static_cast<std::size_t>(row - 1)) * STR_LEN;
        std::memcpy(buf.data() + off, s.data(), static_cast<std::size_t>(STR_LEN));
      }
    }
    return buf;
  };

  /* dim order, slowest to fastest : [step][col][row][slen] */
  auto build_str3d = [&](const std::string &name5) -> std::vector<char> {
    std::vector<char> buf(static_cast<std::size_t>(NUM_STEPS) * NUM_COLS * NUM_ROWS * STR_LEN);
    for(int step = 1; step <= NUM_STEPS; step++){
      for(int col = 1; col <= NUM_COLS; col++){
        for(int row = 1; row <= NUM_ROWS; row++){
          std::string s = make_elem_str(name5, std::vector<int>{row, col, step});
          std::size_t off = ((static_cast<std::size_t>(step - 1) * NUM_COLS +
              static_cast<std::size_t>(col - 1)) * NUM_ROWS +
              static_cast<std::size_t>(row - 1)) * STR_LEN;
          std::memcpy(buf.data() + off, s.data(), static_cast<std::size_t>(STR_LEN));
        }
      }
    }
    return buf;
  };

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open:" + filename)){
    return PIO_EINTERNAL;
  }

  int dim_slen = -1, dim_row = -1, dim_col = -1, dim_step = -1;
  ret = PIOc_def_dim(ncid, dim0_name.c_str(), STR_LEN, &dim_slen);
  if(!check_ret(tf, ret, "Failed to define dim 0:" + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, dim1_name.c_str(), NUM_ROWS, &dim_row);
  if(!check_ret(tf, ret, "Failed to define dim 1:" + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, dim2_name.c_str(), NUM_COLS, &dim_col);
  if(!check_ret(tf, ret, "Failed to define dim 2:" + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, dim3_name.c_str(), NUM_STEPS, &dim_step);
  if(!check_ret(tf, ret, "Failed to define dim 3:" + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  int var1d_dimids[2] = {dim_row, dim_slen};
  int var2d_dimids[3] = {dim_col, dim_row, dim_slen};
  int var3d_dimids[4] = {dim_step, dim_col, dim_row, dim_slen};

  int var1d = -1, var2d = -1, var3d = -1;
  ret = PIOc_def_var(ncid, var1d_name.c_str(), PIO_CHAR, 2, var1d_dimids, &var1d);
  if(!check_ret(tf, ret, "Failed to define 1 dim char array or 1d array of strings in file " + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, var2d_name.c_str(), PIO_CHAR, 3, var2d_dimids, &var2d);
  if(!check_ret(tf, ret, "Failed to define 2 dim char array or 2d array of strings in file " + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, var3d_name.c_str(), PIO_CHAR, 4, var3d_dimids, &var3d);
  if(!check_ret(tf, ret, "Failed to define 3 dim char array or 3d array of strings in file " + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to enddef:" + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  /* Write the entire contents of the strings, e.g. var1d(row) = "var1d(row)" */
  std::string name1d = var1d_name, name2d = var2d_name, name3d = var3d_name;
  std::vector<char> exp_str1d = build_str1d(name1d);
  std::vector<char> exp_str2d = build_str2d(name2d);
  std::vector<char> exp_str3d = build_str3d(name3d);

  ret = PIOc_put_var(ncid, var1d, exp_str1d.data());
  if(!check_ret(tf, ret, "Failed to put 1d array of strings in file " + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, var2d, exp_str2d.data());
  if(!check_ret(tf, ret, "Failed to put 2d array of strings in file " + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_var(ncid, var3d, exp_str3d.data());
  if(!check_ret(tf, ret, "Failed to put 3d array of strings in file " + filename)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + filename)){
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var1d_name.c_str(), &var1d);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var2d_name.c_str(), &var2d);
  if(!check_ret(tf, ret, "Failed to inquire 2d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var3d_name.c_str(), &var3d);
  if(!check_ret(tf, ret, "Failed to inquire 3d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  /* --- Phase 1 : rewrite the prefix of each stored string
   * (VAR_NAME_PREFIX1 => VAR_NAME_PREFIX2), one character at a time, via
   * PIOc_put_vara() hyperslabs --- */
  name1d = VAR_NAME_PREFIX2 + std::to_string(VAR1D_ID) + VAR_NAME_SUFFIX1;
  name2d = VAR_NAME_PREFIX2 + std::to_string(VAR2D_ID) + VAR_NAME_SUFFIX1;
  name3d = VAR_NAME_PREFIX2 + std::to_string(VAR3D_ID) + VAR_NAME_SUFFIX1;
  exp_str1d = build_str1d(name1d);
  exp_str2d = build_str2d(name2d);
  exp_str3d = build_str3d(name3d);

  const int prefix_len = static_cast<int>(VAR_NAME_PREFIX2.size());

  /* 1d var : rewrite the prefix of 1 row (string) at a time */
  for(int row = 1; row <= NUM_ROWS; row++){
    PIO_Offset start[2] = {static_cast<PIO_Offset>(row - 1), 0};
    PIO_Offset count[2] = {1, static_cast<PIO_Offset>(prefix_len)};

    /* Pack the data written out into a buffer of single chars, one
     * character at a time, e.g. tmp_str = ['v', '_', '_'] */
    std::vector<char> tmp_str(static_cast<std::size_t>(prefix_len));
    for(int k = 0; k < prefix_len; k++){
      tmp_str[static_cast<std::size_t>(k)] = VAR_NAME_PREFIX2[static_cast<std::size_t>(k)];
    }

    ret = PIOc_put_vara(ncid, var1d, start, count, tmp_str.data());
    if(!check_ret(tf, ret, "Failed to put string prefixes in file " + filename)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), filename.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* 2d var : rewrite the prefixes of 1 column (1d array of strings) at a time */
  for(int col = 1; col <= NUM_COLS; col++){
    PIO_Offset start[3] = {static_cast<PIO_Offset>(col - 1), 0, 0};
    PIO_Offset count[3] = {1, static_cast<PIO_Offset>(NUM_ROWS), static_cast<PIO_Offset>(prefix_len)};

    std::vector<char> tmp_str(static_cast<std::size_t>(NUM_ROWS) * prefix_len);
    for(int j = 0; j < NUM_ROWS; j++){
      for(int k = 0; k < prefix_len; k++){
        tmp_str[static_cast<std::size_t>(j) * prefix_len + k] = VAR_NAME_PREFIX2[static_cast<std::size_t>(k)];
      }
    }

    ret = PIOc_put_vara(ncid, var2d, start, count, tmp_str.data());
    if(!check_ret(tf, ret, "Failed to put 1d array of string prefixes in file " + filename)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), filename.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* 3d var : rewrite the prefixes of 1 step (2d array of strings) at a time */
  for(int step = 1; step <= NUM_STEPS; step++){
    PIO_Offset start[4] = {static_cast<PIO_Offset>(step - 1), 0, 0, 0};
    PIO_Offset count[4] = {1, static_cast<PIO_Offset>(NUM_COLS), static_cast<PIO_Offset>(NUM_ROWS),
        static_cast<PIO_Offset>(prefix_len)};

    std::vector<char> tmp_str(static_cast<std::size_t>(NUM_COLS) * NUM_ROWS * prefix_len);
    for(int l = 0; l < NUM_COLS; l++){
      for(int j = 0; j < NUM_ROWS; j++){
        for(int k = 0; k < prefix_len; k++){
          tmp_str[(static_cast<std::size_t>(l) * NUM_ROWS + j) * prefix_len + k] =
              VAR_NAME_PREFIX2[static_cast<std::size_t>(k)];
        }
      }
    }

    ret = PIOc_put_vara(ncid, var3d, start, count, tmp_str.data());
    if(!check_ret(tf, ret, "Failed to put 2d array of string prefixes in file " + filename)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), filename.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + filename)){
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var1d_name.c_str(), &var1d);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var2d_name.c_str(), &var2d);
  if(!check_ret(tf, ret, "Failed to inquire 2d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var3d_name.c_str(), &var3d);
  if(!check_ret(tf, ret, "Failed to inquire 3d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  bool ok = true;

  std::vector<char> gstr1d(exp_str1d.size(), '\0');
  ret = PIOc_get_var(ncid, var1d, gstr1d.data());
  if(!check_ret(tf, ret, "Failed to get 1d char var in file " + filename)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gstr1d.begin(), gstr1d.end()),
      std::string(exp_str1d.begin(), exp_str1d.end()),
      "Got wrong value for 1d char var with modified prefix") && ok;

  std::vector<char> gstr2d(exp_str2d.size(), '\0');
  ret = PIOc_get_var(ncid, var2d, gstr2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d char var in file " + filename)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gstr2d.begin(), gstr2d.end()),
      std::string(exp_str2d.begin(), exp_str2d.end()),
      "Got wrong value for 2d char var with modified prefix") && ok;

  std::vector<char> gstr3d(exp_str3d.size(), '\0');
  ret = PIOc_get_var(ncid, var3d, gstr3d.data());
  if(!check_ret(tf, ret, "Failed to get 3d char var in file " + filename)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gstr3d.begin(), gstr3d.end()),
      std::string(exp_str3d.begin(), exp_str3d.end()),
      "Got wrong value for 3d char var with modified prefix") && ok;

  /* --- Phase 2 : rewrite the suffix of each stored string
   * (VAR_NAME_SUFFIX1 => VAR_NAME_SUFFIX2), one character at a time, via
   * PIOc_put_vara() hyperslabs --- */
  name1d = VAR_NAME_PREFIX2 + std::to_string(VAR1D_ID) + VAR_NAME_SUFFIX2;
  name2d = VAR_NAME_PREFIX2 + std::to_string(VAR2D_ID) + VAR_NAME_SUFFIX2;
  name3d = VAR_NAME_PREFIX2 + std::to_string(VAR3D_ID) + VAR_NAME_SUFFIX2;
  exp_str1d = build_str1d(name1d);
  exp_str2d = build_str2d(name2d);
  exp_str3d = build_str3d(name3d);

  const int suffix_len = static_cast<int>(VAR_NAME_SUFFIX2.size());
  /* Skip VAR_NAME_PREFIX2 and VAR*D_ID */
  const int suffix_start = prefix_len + ID_LEN;

  /* 1d var : rewrite the suffix of 1 row (string) at a time */
  for(int row = 1; row <= NUM_ROWS; row++){
    PIO_Offset start[2] = {static_cast<PIO_Offset>(row - 1), static_cast<PIO_Offset>(suffix_start)};
    PIO_Offset count[2] = {1, static_cast<PIO_Offset>(suffix_len)};

    std::vector<char> tmp_str(static_cast<std::size_t>(suffix_len));
    for(int k = 0; k < suffix_len; k++){
      tmp_str[static_cast<std::size_t>(k)] = VAR_NAME_SUFFIX2[static_cast<std::size_t>(k)];
    }

    ret = PIOc_put_vara(ncid, var1d, start, count, tmp_str.data());
    if(!check_ret(tf, ret, "Failed to put string suffixes in file " + filename)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), filename.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* 2d var : rewrite the suffixes of 1 column (1d array of strings) at a time */
  for(int col = 1; col <= NUM_COLS; col++){
    PIO_Offset start[3] = {static_cast<PIO_Offset>(col - 1), 0, static_cast<PIO_Offset>(suffix_start)};
    PIO_Offset count[3] = {1, static_cast<PIO_Offset>(NUM_ROWS), static_cast<PIO_Offset>(suffix_len)};

    std::vector<char> tmp_str(static_cast<std::size_t>(NUM_ROWS) * suffix_len);
    for(int j = 0; j < NUM_ROWS; j++){
      for(int k = 0; k < suffix_len; k++){
        tmp_str[static_cast<std::size_t>(j) * suffix_len + k] = VAR_NAME_SUFFIX2[static_cast<std::size_t>(k)];
      }
    }

    ret = PIOc_put_vara(ncid, var2d, start, count, tmp_str.data());
    if(!check_ret(tf, ret, "Failed to put 1d array of string suffixes in file " + filename)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), filename.c_str());
      return PIO_EINTERNAL;
    }
  }

  /* 3d var : rewrite the suffixes of 1 step (2d array of strings) at a time */
  for(int step = 1; step <= NUM_STEPS; step++){
    PIO_Offset start[4] = {static_cast<PIO_Offset>(step - 1), 0, 0, static_cast<PIO_Offset>(suffix_start)};
    PIO_Offset count[4] = {1, static_cast<PIO_Offset>(NUM_COLS), static_cast<PIO_Offset>(NUM_ROWS),
        static_cast<PIO_Offset>(suffix_len)};

    std::vector<char> tmp_str(static_cast<std::size_t>(NUM_COLS) * NUM_ROWS * suffix_len);
    for(int l = 0; l < NUM_COLS; l++){
      for(int j = 0; j < NUM_ROWS; j++){
        for(int k = 0; k < suffix_len; k++){
          tmp_str[(static_cast<std::size_t>(l) * NUM_ROWS + j) * suffix_len + k] =
              VAR_NAME_SUFFIX2[static_cast<std::size_t>(k)];
        }
      }
    }

    ret = PIOc_put_vara(ncid, var3d, start, count, tmp_str.data());
    if(!check_ret(tf, ret, "Failed to put 2d array of string suffixes in file " + filename)){
      PIOc_closefile(ncid);
      PIOc_deletefile(tf.get_iosysid(), filename.c_str());
      return PIO_EINTERNAL;
    }
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close:" + filename)){
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    return PIO_EINTERNAL;
  }

  ncid = -1;
  ret = PIOc_openfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Failed to reopen:" + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var1d_name.c_str(), &var1d);
  if(!check_ret(tf, ret, "Failed to inquire 1d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var2d_name.c_str(), &var2d);
  if(!check_ret(tf, ret, "Failed to inquire 2d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var3d_name.c_str(), &var3d);
  if(!check_ret(tf, ret, "Failed to inquire 3d array of strings var in file " + filename)){
    return PIO_EINTERNAL;
  }
#else
  PIOc_sync(ncid);
#endif

  ret = PIOc_get_var(ncid, var1d, gstr1d.data());
  if(!check_ret(tf, ret, "Failed to get 1d char var in file " + filename)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gstr1d.begin(), gstr1d.end()),
      std::string(exp_str1d.begin(), exp_str1d.end()),
      "Got wrong value for 1d char var with modified suffixes") && ok;

  ret = PIOc_get_var(ncid, var2d, gstr2d.data());
  if(!check_ret(tf, ret, "Failed to get 2d char var in file " + filename)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gstr2d.begin(), gstr2d.end()),
      std::string(exp_str2d.begin(), exp_str2d.end()),
      "Got wrong value for 2d char var with modified suffixes") && ok;

  ret = PIOc_get_var(ncid, var3d, gstr3d.data());
  if(!check_ret(tf, ret, "Failed to get 3d char var in file " + filename)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_str(tf, std::string(gstr3d.begin(), gstr3d.end()),
      std::string(exp_str3d.begin(), exp_str3d.end()),
      "Got wrong value for 3d char var with modified suffixes") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), filename.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

int test_put_get_str_hslabs(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_str_hslabs : ") + iotype_name + "\n");

    if(test_put_get_str_hslabs_one_iotype(tf, flavors[i]) != PIO_NOERR){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Typed put/get dispatch : PIOc_put_var()/PIOc_get_var() (the untyped, void *
 * buffer, variants) use PIO_NAT as the memory type, i.e. they assume that the
 * buffer's type matches the variable's on-disk type exactly - no conversion
 * is done, and using them with a buffer type that does not match the
 * variable's type (e.g. a buffer of ints for a PIO_DOUBLE variable) can
 * overrun the buffer. The typed variants (PIOc_put_var_int/_float/_double and
 * PIOc_get_var_int/_float/_double) pass the buffer's actual memory type
 * explicitly, so the library performs a real on-the-fly type conversion
 * to/from the variable's on-disk type - this is required to exercise the
 * implicit type conversion behavior below. */
template<typename T>
static int put_var_typed(int ncid, int varid, const T *buf);

template<>
int put_var_typed<int>(int ncid, int varid, const int *buf)
{
  return PIOc_put_var_int(ncid, varid, buf);
}

template<>
int put_var_typed<float>(int ncid, int varid, const float *buf)
{
  return PIOc_put_var_float(ncid, varid, buf);
}

template<>
int put_var_typed<double>(int ncid, int varid, const double *buf)
{
  return PIOc_put_var_double(ncid, varid, buf);
}

template<typename T>
static int get_var_typed(int ncid, int varid, T *buf);

template<>
int get_var_typed<int>(int ncid, int varid, int *buf)
{
  return PIOc_get_var_int(ncid, varid, buf);
}

template<>
int get_var_typed<float>(int ncid, int varid, float *buf)
{
  return PIOc_get_var_float(ncid, varid, buf);
}

template<>
int get_var_typed<double>(int ncid, int varid, double *buf)
{
  return PIOc_get_var_double(ncid, varid, buf);
}

/* Put and get variables that can involve implicit type conversion : int,
 * real and double variables are all written from, and read back into, the
 * same buffer (of the templated type T), exercising on-the-fly type
 * conversion in PIOc_put_var_<T>()/PIOc_get_var_<T>(). No decomposition is
 * involved (unlike test_data_conversion<T>() in ncdf_simple_tests.cpp) - the
 * Fortran source for this sub uses PIO_put_var()/PIO_get_var() without a
 * decomp. */
template<typename T>
static int test_put_get_type_conversion_one_iotype(SPIO_TF::Test_framework &tf, int iotype)
{
  const int DIM_LEN = 100;
  const std::string data_fname("pio_test_put_get_type_conversion.nc");
  const std::string PIO_VAR_INT_NAME("PIO_TF_test_var_int");
  const std::string PIO_VAR_REAL_NAME("PIO_TF_test_var_real");
  const std::string PIO_VAR_DBL_NAME("PIO_TF_test_var_dbl");

  std::vector<T> pval(DIM_LEN, static_cast<T>(tf.get_comm_size()));
  std::vector<T> gval(DIM_LEN, static_cast<T>(0));

  int ncid = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, data_fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Could not create file " + data_fname)){
    return PIO_EINTERNAL;
  }

  int pio_dim = -1;
  ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", DIM_LEN, &pio_dim);
  if(!check_ret(tf, ret, "Failed to define a dim : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

  int varid_int = -1, varid_real = -1, varid_dbl = -1;
  ret = PIOc_def_var(ncid, PIO_VAR_INT_NAME.c_str(), PIO_INT, 1, &pio_dim, &varid_int);
  if(!check_ret(tf, ret, "Failed to define a var of int type : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_VAR_REAL_NAME.c_str(), PIO_REAL, 1, &pio_dim, &varid_real);
  if(!check_ret(tf, ret, "Failed to define a var of real type : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, PIO_VAR_DBL_NAME.c_str(), PIO_DOUBLE, 1, &pio_dim, &varid_dbl);
  if(!check_ret(tf, ret, "Failed to define a var of double type : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(!check_ret(tf, ret, "Failed to end redef mode : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

  /* Put the int type variable out (data conversion might occur: real => int, double => int) */
  ret = put_var_typed<T>(ncid, varid_int, pval.data());
  if(!check_ret(tf, ret, "Failed to put a var of int type : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

  /* Put the real type variable out (data conversion might occur: int => real, double => real) */
  ret = put_var_typed<T>(ncid, varid_real, pval.data());
  if(!check_ret(tf, ret, "Failed to put a var of real type : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

  /* Put the double type variable out (data conversion might occur: int => double, real => double) */
  ret = put_var_typed<T>(ncid, varid_dbl, pval.data());
  if(!check_ret(tf, ret, "Failed to put a var of double type : " + data_fname)){
    PIOc_closefile(ncid);
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close file " + data_fname)){
    PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());
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

  bool ok = true;

  /* Get the int type variable back (data conversion might occur: int => real, int => double) */
  for(std::size_t j = 0; j < gval.size(); j++){
    gval[j] = static_cast<T>(0);
  }
  ret = get_var_typed<T>(ncid, varid_int, gval.data());
  if(!check_ret(tf, ret, "Failed to get a var of int type : " + data_fname)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong val") && ok;

  /* Get the real type variable back (data conversion might occur: real => int, real => double) */
  for(std::size_t j = 0; j < gval.size(); j++){
    gval[j] = static_cast<T>(0);
  }
  ret = get_var_typed<T>(ncid, varid_real, gval.data());
  if(!check_ret(tf, ret, "Failed to get a var of real type : " + data_fname)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong val") && ok;

  /* Get the double type variable back (data conversion might occur: double => int, double => real) */
  for(std::size_t j = 0; j < gval.size(); j++){
    gval[j] = static_cast<T>(0);
  }
  ret = get_var_typed<T>(ncid, varid_dbl, gval.data());
  if(!check_ret(tf, ret, "Failed to get a var of double type : " + data_fname)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong val") && ok;

  PIOc_closefile(ncid);
  PIOc_deletefile(tf.get_iosysid(), data_fname.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_put_get_type_conversion(SPIO_TF::Test_framework &tf)
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
        std::string("Testing : test_put_get_type_conversion : ") + iotype_name + "\n");

    if(test_put_get_type_conversion_one_iotype<T>(tf, flavors[i]) != PIO_NOERR){
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
    {"test_put_1datt_int", test_put_1datt<int>},
    {"test_put_1datt_float", test_put_1datt<float>},
    {"test_put_1datt_double", test_put_1datt<double>},
    {"test_put_get_1datt_int", test_put_get_1datt<int>},
    {"test_put_get_1datt_float", test_put_get_1datt<float>},
    {"test_put_get_1datt_double", test_put_get_1datt<double>},
    {"test_put_get_0dvar_int", test_put_get_0dvar<int>},
    {"test_put_get_0dvar_float", test_put_get_0dvar<float>},
    {"test_put_get_0dvar_double", test_put_get_0dvar<double>},
    {"test_put_get_1dvar_int", test_put_get_1dvar<int>},
    {"test_put_get_1dvar_float", test_put_get_1dvar<float>},
    {"test_put_get_1dvar_double", test_put_get_1dvar<double>},
    {"test_put_get_1d_coord_var_int", test_put_get_1d_coord_var<int>},
    {"test_put_get_1d_coord_var_float", test_put_get_1d_coord_var<float>},
    {"test_put_get_1d_coord_var_double", test_put_get_1d_coord_var<double>},
    {"test_put_get_1d_coord_mvars_int", test_put_get_1d_coord_mvars<int>},
    {"test_put_get_1d_coord_mvars_float", test_put_get_1d_coord_mvars<float>},
    {"test_put_get_1d_coord_mvars_double", test_put_get_1d_coord_mvars<double>},
    {"test_put_get_1dvar_ls_buf_int", test_put_get_1dvar_ls_buf<int>},
    {"test_put_get_1dvar_ls_buf_float", test_put_get_1dvar_ls_buf<float>},
    {"test_put_get_1dvar_ls_buf_double", test_put_get_1dvar_ls_buf<double>},
    {"test_put_get_1dvar_slice_int", test_put_get_1dvar_slice<int>},
    {"test_put_get_1dvar_slice_float", test_put_get_1dvar_slice<float>},
    {"test_put_get_1dvar_slice_double", test_put_get_1dvar_slice<double>},
    {"test_put_get_1dvar_4parts_int", test_put_get_1dvar_4parts<int>},
    {"test_put_get_1dvar_4parts_float", test_put_get_1dvar_4parts<float>},
    {"test_put_get_1dvar_4parts_double", test_put_get_1dvar_4parts<double>},
    {"test_put_get_md2mdplus1_var_int", test_put_get_md2mdplus1_var<int>},
    {"test_put_get_md2mdplus1_var_float", test_put_get_md2mdplus1_var<float>},
    {"test_put_get_md2mdplus1_var_double", test_put_get_md2mdplus1_var<double>},
    {"test_put_get_6d_var_int", test_put_get_6d_var<int>},
    {"test_put_get_6d_var_float", test_put_get_6d_var<float>},
    {"test_put_get_6d_var_double", test_put_get_6d_var<double>},
    {"test_put_get_md2mdplus1_rec_int", test_put_get_md2mdplus1_rec<int>},
    {"test_put_get_md2mdplus1_rec_float", test_put_get_md2mdplus1_rec<float>},
    {"test_put_get_md2mdplus1_rec_double", test_put_get_md2mdplus1_rec<double>},
    {"test_put_get_time_scalar_int", test_put_get_time_scalar<int>},
    {"test_put_get_time_scalar_float", test_put_get_time_scalar<float>},
    {"test_put_get_time_scalar_double", test_put_get_time_scalar<double>},
    {"test_put_get_time_scalar_rec_int", test_put_get_time_scalar_rec<int>},
    {"test_put_get_time_scalar_rec_float", test_put_get_time_scalar_rec<float>},
    {"test_put_get_time_scalar_rec_double", test_put_get_time_scalar_rec<double>},
    {"test_put_get_1d_str_arr", test_put_get_1d_str_arr},
    {"test_put_get_2d_str_arr", test_put_get_2d_str_arr},
    {"test_put_get_misc_str", test_put_get_misc_str},
    {"test_put_get_str_frames", test_put_get_str_frames},
    {"test_put_get_str_idx", test_put_get_str_idx},
    {"test_put_get_str_hslabs", test_put_get_str_hslabs},
    {"test_put_get_type_conversion_int", test_put_get_type_conversion<int>},
    {"test_put_get_type_conversion_float", test_put_get_type_conversion<float>},
    {"test_put_get_type_conversion_double", test_put_get_type_conversion<double>}
  };

  SPIO_TF::Test_framework tf("ncdf_get_put_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  /* This is a NC-focused-driver test file (like ncdf_simple_tests.cpp) that
   * only needs a single rearranger, not the usual 3-rearranger loop. */
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
