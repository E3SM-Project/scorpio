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
  ok = passert(tf, (std::string(gcval.begin(), gcval.end()) == pcval), "Got wrong value") && ok;

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
  ok = passert(tf, (gval == pval), "Got wrong value") && ok;

  ret = PIOc_get_var(ncid, pio_cvar, &gcval);
  if(!check_ret(tf, ret, "Failed to get scalar char var:" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (gcval == pcval), "Got wrong value") && ok;

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
  ok = passert(tf, (std::string(gcval.begin(), gcval.end()) == pcval), "Got wrong value") && ok;

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
  const int DIM_SMALL_LEN = DIM_LEN - 100;
  const std::string DIM_NAME("dummy_dim_put_val");
  const std::string SVAR_NAME("dummy_svar_put_val");
  const std::string LVAR_NAME("dummy_lvar_put_val");
  const std::string SCVAR_NAME("dummy_svar_put_cval");
  const std::string LCVAR_NAME("dummy_lvar_put_cval");

  /* Buffer used to write svar/scvar (larger than the DIM_LEN-sized variable -
   * only the first DIM_LEN elements are ever written out) */
  std::vector<T> pval_large(DIM_LEN);
  for(std::size_t i = 0; i < pval_large.size(); i++){
    pval_large[i] = static_cast<T>(i + 1);
  }
  const std::string pcval_large(DIM_LEN, 'l');

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
  ok = check_val(tf, gval_full, pval_large,
      "Get variable written with large user buffer : Got wrong value") && ok;

  std::vector<char> gcval_full(DIM_LEN, '\0');
  ret = PIOc_get_var(ncid, pio_scvar, gcval_full.data());
  if(!check_ret(tf, ret, "Failed to get char var (smaller than user buffer):" + FNAME)){
    PIOc_closefile(ncid);
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (std::string(gcval_full.begin(), gcval_full.end()) == pcval_large),
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
  ok = passert(tf, (std::string(gcval_small.begin(), gcval_small.end()) == pcval_small),
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

int main(int argc, char *argv[])
{
  int ret = MPI_SUCCESS, nerrs = 0;
  ret = MPI_Init(&argc, &argv);
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
    {"test_put_get_md2mdplus1_rec_double", test_put_get_md2mdplus1_rec<double>}
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
