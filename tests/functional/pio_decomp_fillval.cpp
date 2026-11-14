#include <iostream>
#include <string>
#include <vector>
#include <functional>
#include <utility>
#include <algorithm>

#include "pio.h"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#include "spio_test_framework.hpp"
#include "spio_test_check.hpp"
#include "spio_test_common.hpp"

/* nc write 1d array with fillvalues (the holes are explicitly specified) */
template<typename T>
int nc_write_1d_explicit_fval(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;
  /* Compdof value to suggest that data point is a hole, this hole
   * is usually filled with a fillvalue
   */
  const int PIO_COMPDOF_FILLVAL = 0;
  /* The buffer fillvalue to be used when writing data */
  const T BUF_FILLVAL = static_cast<T>(-2);
  int dims[1];

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();

  std::vector<PIO_Offset> wcompdof(VEC_LOCAL_SZ), rcompdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wcompdof[i] = rcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(VEC_LOCAL_SZ), exp_val(VEC_LOCAL_SZ), rbuf(VEC_LOCAL_SZ, static_cast<T>(0));
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wbuf[i] = exp_val[i] = static_cast<T>(wcompdof[i]);
  }

  /* Even indices (1-based, odd 0-based: 0,2,4,6) need to be filled with
   * (BUF_FILLVAL == -2)
   */
  for(int i = 0; i < VEC_LOCAL_SZ; i += 2){
    wcompdof[i] = PIO_COMPDOF_FILLVAL;
    exp_val[i] = BUF_FILLVAL;
  }

  int wioid = -1, rioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];
    std::string filename("test_pio_decomp_fillval_tests.testfile" + std::to_string(i + 1));

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_write_1d_explicit_fval : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, wioid, VEC_LOCAL_SZ, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid, rioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), rioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* nc write 1d array with fillvalues (the holes are implicit) */
template<typename T>
int nc_write_1d_implicit_fval(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;
  /* The buffer fillvalue to be used when writing data */
  const T BUF_FILLVAL = static_cast<T>(-2);
  int dims[1];

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();

  /* compdof is only specified for valid data values, the data holes are
   * implicitly stated (by not specifying them rather than filling it with 0s)
   */
  int wcompdof_sz = std::min(tf.get_comm_rank() + 1, VEC_LOCAL_SZ);
  std::vector<PIO_Offset> wcompdof(wcompdof_sz);
  /* rank 0 has 1 valid data value, rank 1 has 2 data values and so on... */
  for(int i = 0; i < wcompdof_sz; i++){
    wcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  /* Read everything - including fillvalues that should have been
   * written for locations unspecified in wcompdof[] i.e.,
   * wcompdof[wcompdof_sz:VEC_LOCAL_SZ)
   */
  std::vector<PIO_Offset> rcompdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    rcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(wcompdof_sz);
  for(int i = 0; i < wcompdof_sz; i++){
    wbuf[i] = static_cast<T>(wcompdof[i]);
  }

  std::vector<T> exp_val(VEC_LOCAL_SZ);
  for(int i = 0; i < wcompdof_sz; i++){
    exp_val[i] = wbuf[i];
  }
  /* We expect the values [wcompdof_sz, VEC_LOCAL_SZ) to be read as
   * user specified fill values
   */
  for(int i = wcompdof_sz; i < VEC_LOCAL_SZ; i++){
    exp_val[i] = BUF_FILLVAL;
  }

  std::vector<T> rbuf(VEC_LOCAL_SZ, static_cast<T>(0));

  int wioid = -1, rioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          wcompdof_sz, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_fillval_tests.testfile");
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_write_1d_implicit_fval : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variable out, user specified fillvalue = BUF_FILLVAL */
    ret = PIOc_write_darray(ncid, varid, wioid, wcompdof_sz, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid, rioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), rioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* nc read 1d array with fillvalues (the holes are explicitly specified) */
template<typename T>
int nc_read_1d_explicit_fval(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;
  /* Compdof value to suggest that data point is a hole */
  const int PIO_COMPDOF_FILLVAL = 0;
  /* The buffer fillvalue used to initialize data */
  const T BUF_FILLVAL = static_cast<T>(-2);
  int dims[1];

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();

  std::vector<PIO_Offset> wcompdof(VEC_LOCAL_SZ), rcompdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wcompdof[i] = rcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(VEC_LOCAL_SZ), exp_val(VEC_LOCAL_SZ), rbuf(VEC_LOCAL_SZ, BUF_FILLVAL);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wbuf[i] = exp_val[i] = static_cast<T>(wcompdof[i]);
  }

  /* We don't want to read even indices (1-based, 0-based: 0,2,4,6)
   * (BUF_FILLVAL == -2)
   */
  for(int i = 0; i < VEC_LOCAL_SZ; i += 2){
    rcompdof[i] = PIO_COMPDOF_FILLVAL;
    exp_val[i] = BUF_FILLVAL;
  }

  int wioid = -1, rioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_fillval_tests.testfile");
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_read_1d_explicit_fval : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, wioid, VEC_LOCAL_SZ, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = BUF_FILLVAL;
    }
    ret = PIOc_read_darray(ncid, varid, rioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), rioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* nc read 1d array with fillvalues (the holes are implicit) */
template<typename T>
int nc_read_1d_implicit_fval(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;
  /* Note: the Fortran source declares this as a plain (default kind)
   * integer, not the templated PIO_TF_FC_DATA_TYPE - kept as int here
   * for parity, and cast to T wherever it is stored into a T-typed buffer.
   */
  const int BUF_FILLVAL = -2;
  int dims[1];

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();

  /* compdof is only specified for valid data values, the data holes are
   * implicitly stated (by not specifying them rather than filling it with 0s)
   */
  int rcompdof_sz = std::min(tf.get_comm_rank() + 1, VEC_LOCAL_SZ);
  std::vector<PIO_Offset> rcompdof(rcompdof_sz);
  /* We only read 1 value on rank0, 2 values on rank1, ... */
  for(int i = 0; i < rcompdof_sz; i++){
    rcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  /* Write everything - we only read some of these values */
  std::vector<PIO_Offset> wcompdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wbuf[i] = static_cast<T>(wcompdof[i]);
  }

  std::vector<T> rbuf(rcompdof_sz, static_cast<T>(BUF_FILLVAL));

  std::vector<T> exp_val(rcompdof_sz);
  for(int i = 0; i < rcompdof_sz; i++){
    exp_val[i] = wbuf[i];
  }

  int wioid = -1, rioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          rcompdof_sz, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_fillval_tests.testfile");
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_read_1d_implicit_fval : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid, wioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    /* Read only part of the written data */
    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(BUF_FILLVAL);
    }
    ret = PIOc_read_darray(ncid, varid, rioid, rcompdof_sz, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), rioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), wioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Test PIOc_set_fill() : writing/reading vars with/without fillmode set */
template<typename T>
int test_set_fill(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int DIM_LEN = 100;

  std::vector<T> pval(DIM_LEN, static_cast<T>(tf.get_comm_size()));
  std::string pcval("DUMMY_STRING");
  pcval.resize(DIM_LEN, ' ');

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_ncdf_get_put.testfile");
  const std::string PIO_VAR_NOFILL_NAME("dummy_nofill_var_put_val");
  const std::string PIO_VAR_FILL_NAME("dummy_fill_var_put_val");
  const std::string PIO_CVAR_NOFILL_NAME("dummy_nofill_var_put_cval");
  const std::string PIO_CVAR_FILL_NAME("dummy_fill_var_put_cval");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1;
    int varid_nofill = -1, varid_fill = -1, varid_cnofill = -1, varid_cfill = -1;
    int prev_fillmode = 0;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : test_set_fill : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Failed to open: " + filename)){
      return PIO_EINTERNAL;
    }

    /* Since file is just created no need to enter redef */
    ret = PIOc_def_dim(ncid, "dummy_dim_put_val", DIM_LEN, &dimid);
    if(!check_ret(tf, ret, "Failed to define dim: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NOFILL_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid_nofill);
    if(!check_ret(tf, ret, "Failed to define var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_FILL_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid_fill);
    if(!check_ret(tf, ret, "Failed to define var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_CVAR_NOFILL_NAME.c_str(), PIO_CHAR, 1, &dimid, &varid_cnofill);
    if(!check_ret(tf, ret, "Failed to define char var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_CVAR_FILL_NAME.c_str(), PIO_CHAR, 1, &dimid, &varid_cfill);
    if(!check_ret(tf, ret, "Failed to define char var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_set_fill(ncid, PIO_NOFILL, &prev_fillmode);
    if(!check_ret(tf, ret, "Failed to set fillmode to NOFILL: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to enddef: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_put_var(ncid, varid_nofill, pval.data());
    if(!check_ret(tf, ret, "Failed to put var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_put_var(ncid, varid_cnofill, pcval.data());
    if(!check_ret(tf, ret, "Failed to put char var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_redef(ncid);
    if(!check_ret(tf, ret, "Failed to redef: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_set_fill(ncid, PIO_FILL, &prev_fillmode);
    if(!check_ret(tf, ret, "Failed to set fillmode to FILL: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to enddef: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_put_var(ncid, varid_fill, pval.data());
    if(!check_ret(tf, ret, "Failed to put var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_put_var(ncid, varid_cfill, pcval.data());
    if(!check_ret(tf, ret, "Failed to put char var: " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Failed to reopen: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NOFILL_NAME.c_str(), &varid_nofill);
    if(!check_ret(tf, ret, "Failed to get scalar var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_FILL_NAME.c_str(), &varid_fill);
    if(!check_ret(tf, ret, "Failed to get scalar var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_CVAR_NOFILL_NAME.c_str(), &varid_cnofill);
    if(!check_ret(tf, ret, "Failed to get scalar char var: " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_CVAR_FILL_NAME.c_str(), &varid_cfill);
    if(!check_ret(tf, ret, "Failed to get scalar char var: " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    std::vector<T> gval(DIM_LEN, static_cast<T>(0));
    ret = PIOc_get_var(ncid, varid_nofill, gval.data());
    if(!check_ret(tf, ret, "Failed to get var (fillmode = NOFILL): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, gval, pval, "Got wrong value (fillmode = NOFILL)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < gval.size(); j++){
      gval[j] = static_cast<T>(0);
    }
    ret = PIOc_get_var(ncid, varid_fill, gval.data());
    if(!check_ret(tf, ret, "Failed to get var (fillmode = FILL): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, gval, pval, "Got wrong value (fillmode = FILL)")){
      return PIO_EINTERNAL;
    }

    std::string gcval(DIM_LEN, '\0');
    ret = PIOc_get_var(ncid, varid_cnofill, &gcval[0]);
    if(!check_ret(tf, ret, "Failed to get char var (fillmode = NOFILL): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!passert(tf, gcval == pcval, "Got wrong value (fillmode = NOFILL)")){
      return PIO_EINTERNAL;
    }

    gcval.assign(DIM_LEN, '\0');
    ret = PIOc_get_var(ncid, varid_cfill, &gcval[0]);
    if(!check_ret(tf, ret, "Failed to get char var (fillmode = FILL): " + filename)){
      return PIO_EINTERNAL;
    }

    if(!passert(tf, gcval == pcval, "Got wrong value (fillmode = FILL)")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  return PIO_NOERR;
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
    {"nc_write_1d_explicit_fval_int", nc_write_1d_explicit_fval<int>},
    {"nc_write_1d_explicit_fval_float", nc_write_1d_explicit_fval<float>},
    {"nc_write_1d_explicit_fval_double", nc_write_1d_explicit_fval<double>},
    {"nc_write_1d_implicit_fval_int", nc_write_1d_implicit_fval<int>},
    {"nc_write_1d_implicit_fval_float", nc_write_1d_implicit_fval<float>},
    {"nc_write_1d_implicit_fval_double", nc_write_1d_implicit_fval<double>},
    {"nc_read_1d_explicit_fval_int", nc_read_1d_explicit_fval<int>},
    {"nc_read_1d_explicit_fval_float", nc_read_1d_explicit_fval<float>},
    {"nc_read_1d_explicit_fval_double", nc_read_1d_explicit_fval<double>},
    {"nc_read_1d_implicit_fval_int", nc_read_1d_implicit_fval<int>},
    {"nc_read_1d_implicit_fval_float", nc_read_1d_implicit_fval<float>},
    {"nc_read_1d_implicit_fval_double", nc_read_1d_implicit_fval<double>},
    {"test_set_fill_int", test_set_fill<int>},
    {"test_set_fill_float", test_set_fill<float>},
    {"test_set_fill_double", test_set_fill<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_fillval_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  int rearrs[] = {PIO_REARR_BOX, PIO_REARR_SUBSET, PIO_REARR_CONTIG};
  for(int rearr : rearrs){
    if(tf.init_iosystem(rearr) != PIO_NOERR){
      nerrs++;
      continue;
    }
    nerrs += tf.run();
    tf.finalize_iosystem();
  }

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
