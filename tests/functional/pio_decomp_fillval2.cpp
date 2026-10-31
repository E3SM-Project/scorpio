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

/* nc write 1d array with fillvalues (the holes are explicitly specified)
 * Each process writes out data with "rank + 1" holes, strided by 2
 */
template<typename T>
int nc_wr_1d_mreg_exp_fval(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  /* Stride for holes in data */
  const int HSTRIDE = 2;
  /* Compdof value to suggest that data point is a hole, this hole
   * is usually filled with a fillvalue
   */
  const int PIO_COMPDOF_FILLVAL = 0;
  /* The buffer fillvalue to be used when writing data */
  const T BUF_FILLVAL = static_cast<T>(-2);
  int dims[1];

  int num_local_holes = tf.get_comm_rank() + 1;
  int vec_local_sz = tf.get_comm_size() * HSTRIDE;

  dims[0] = vec_local_sz * tf.get_comm_size();

  std::vector<PIO_Offset> wcompdof(vec_local_sz), rcompdof(vec_local_sz);
  for(int i = 0; i < vec_local_sz; i++){
    wcompdof[i] = rcompdof[i] = vec_local_sz * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(vec_local_sz), exp_val(vec_local_sz), rbuf(vec_local_sz, static_cast<T>(0));
  for(int i = 0; i < vec_local_sz; i++){
    wbuf[i] = exp_val[i] = static_cast<T>(wcompdof[i]);
  }

  /* For hole indices we expect (BUF_FILLVAL == -2)
   * holes are strided by HSTRIDE
   */
  for(int i = 0; i < num_local_holes; i++){
    int j = i * HSTRIDE;
    wcompdof[j] = PIO_COMPDOF_FILLVAL;
    exp_val[j] = BUF_FILLVAL;
  }

  int wioid = -1, rioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          vec_local_sz, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          vec_local_sz, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string PIO_VAR1_NAME("PIO_TF_test_var1");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid1 = -1, varid2 = -1;
    char iotype_name[PIO_MAX_NAME + 1];
    std::string filename("test_pio_decomp_fillval_tests.testfile" + std::to_string(i + 1));

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_1d_mreg_exp_fval : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR1_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid1);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR2_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid2);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid1, wioid, vec_local_sz, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, wioid, vec_local_sz, wbuf.data(), &BUF_FILLVAL);
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

    ret = PIOc_inq_varid(ncid, PIO_VAR1_NAME.c_str(), &varid1);
    if(!check_ret(tf, ret, "Could not inq var1 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR2_NAME.c_str(), &varid2);
    if(!check_ret(tf, ret, "Could not inq var2 : " + filename)){
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
    ret = PIOc_read_darray(ncid, varid1, rioid, vec_local_sz, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid2, rioid, vec_local_sz, rbuf.data());
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

/* nc write 1d array with fillvalues (the holes are implicitly specified)
 * Each process writes out data with "rank + 1" holes, strided by 2
 */
template<typename T>
int nc_wr_1d_mreg_imp_fval(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  /* Stride for holes in data */
  const int HSTRIDE = 2;
  /* The buffer fillvalue to be used when writing data */
  const T BUF_FILLVAL = static_cast<T>(-2);
  int dims[1];

  /* Data written out by each process has "rank + 1" holes */
  int num_local_holes = tf.get_comm_rank() + 1;
  int vec_local_rsz = tf.get_comm_size() * HSTRIDE;
  int vec_local_wsz = vec_local_rsz - num_local_holes;

  dims[0] = vec_local_rsz * tf.get_comm_size();

  /* We read more than we write (for procs with holes) */
  std::vector<PIO_Offset> rcompdof(vec_local_rsz);
  for(int i = 0; i < vec_local_rsz; i++){
    rcompdof[i] = vec_local_rsz * tf.get_comm_rank() + (i + 1);
  }

  /* Since holes are implicit - only specifying valid write compdofs
   * holes are strided by HSTRIDE
   */
  std::vector<PIO_Offset> wcompdof(vec_local_wsz);
  int k = 0;
  for(int i = 1; i <= num_local_holes; i++){
    wcompdof[k++] = vec_local_rsz * tf.get_comm_rank() + i * HSTRIDE;
  }
  for(int i = num_local_holes * HSTRIDE + 1; i <= vec_local_rsz; i++){
    wcompdof[k++] = vec_local_rsz * tf.get_comm_rank() + i;
  }

  std::vector<T> wbuf(vec_local_wsz);
  for(int i = 0; i < vec_local_wsz; i++){
    wbuf[i] = static_cast<T>(wcompdof[i]);
  }

  std::vector<T> exp_val(vec_local_rsz);
  for(int i = 0; i < vec_local_rsz; i++){
    exp_val[i] = static_cast<T>(rcompdof[i]);
  }
  std::vector<T> rbuf(vec_local_rsz, static_cast<T>(0));

  /* For hole indices we expect (BUF_FILLVAL == -2)
   * holes are strided by HSTRIDE
   */
  for(int i = 0; i < num_local_holes; i++){
    exp_val[i * HSTRIDE] = BUF_FILLVAL;
  }

  int wioid = -1, rioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          vec_local_wsz, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          vec_local_rsz, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string PIO_VAR1_NAME("PIO_TF_test_var1");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid1 = -1, varid2 = -1;
    char iotype_name[PIO_MAX_NAME + 1];
    std::string filename("test_pio_decomp_fillval_tests.testfile" + std::to_string(i + 1));

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_1d_mreg_imp_fval : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR1_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid1);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR2_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid2);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid1, wioid, vec_local_wsz, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, wioid, vec_local_wsz, wbuf.data(), &BUF_FILLVAL);
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

    ret = PIOc_inq_varid(ncid, PIO_VAR1_NAME.c_str(), &varid1);
    if(!check_ret(tf, ret, "Could not inq var1 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR2_NAME.c_str(), &varid2);
    if(!check_ret(tf, ret, "Could not inq var2 : " + filename)){
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
    ret = PIOc_read_darray(ncid, varid1, rioid, vec_local_rsz, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid2, rioid, vec_local_rsz, rbuf.data());
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

/* A test that writes out val with only one data point/value
 * All procs, except the last proc, only writes out fillvalues
 * the last proc, writes out 1 value that is not a fillvalue and
 * the rest all fillvalues.
 */
template<typename T>
int nc_wr_1d_one_val(SPIO_TF::Test_framework &tf)
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

  bool is_last_proc = (tf.get_comm_rank() == tf.get_comm_size() - 1);

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();

  std::vector<PIO_Offset> wcompdof(VEC_LOCAL_SZ), rcompdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wcompdof[i] = rcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(VEC_LOCAL_SZ), exp_val(VEC_LOCAL_SZ), rbuf(VEC_LOCAL_SZ, static_cast<T>(0));
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wbuf[i] = exp_val[i] = static_cast<T>(wcompdof[i]);
  }

  /* All indices have fillvalues by default */
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wcompdof[i] = PIO_COMPDOF_FILLVAL;
    exp_val[i] = BUF_FILLVAL;
  }

  /* Only the last proc has a valid data value */
  if(is_last_proc){
    wcompdof[VEC_LOCAL_SZ - 1] = VEC_LOCAL_SZ * tf.get_comm_rank() + VEC_LOCAL_SZ;
    exp_val[VEC_LOCAL_SZ - 1] = static_cast<T>(wcompdof[VEC_LOCAL_SZ - 1]);
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
        std::string("Testing : nc_wr_1d_one_val : ") + iotype_name + "\n");

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

/* A test where all odd procs write only fillvalues
 * - even procs write non-fillvalues
 */
template<typename T>
int nc_wr_1d_odd_fval(SPIO_TF::Test_framework &tf)
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

  bool is_odd_proc = ((tf.get_comm_rank() % 2) != 0);

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();

  std::vector<PIO_Offset> wcompdof(VEC_LOCAL_SZ), rcompdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wcompdof[i] = rcompdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(VEC_LOCAL_SZ), exp_val(VEC_LOCAL_SZ), rbuf(VEC_LOCAL_SZ, static_cast<T>(0));
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    wbuf[i] = exp_val[i] = static_cast<T>(wcompdof[i]);
  }

  /* All odd procs write only fillvalues */
  if(is_odd_proc){
    for(int i = 0; i < VEC_LOCAL_SZ; i++){
      wcompdof[i] = PIO_COMPDOF_FILLVAL;
      exp_val[i] = BUF_FILLVAL;
    }
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
        std::string("Testing : nc_wr_1d_odd_fval : ") + iotype_name + "\n");

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

/* This test writes out data that can involve type conversion while writing
 * the data out to a file. The test writes out multiple variables with
 * explicit fillvalues (and possible type conversion)
 */
template<typename T>
int test_data_conv_with_fval(SPIO_TF::Test_framework &tf)
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

  /* Even indices (1-based, 0-based: 0,2,4,6) need to be filled with
   * (BUF_FILLVAL == -2)
   */
  for(int i = 0; i < VEC_LOCAL_SZ; i += 2){
    wcompdof[i] = PIO_COMPDOF_FILLVAL;
    exp_val[i] = BUF_FILLVAL;
  }

  int wioid = -1, rioid = -1;
  /* Set the decomposition for writing data */
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data */
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("pio_test_data_conv_with_fval.nc");
  const std::string PIO_VAR_INT_NAME("PIO_TF_test_var_int");
  const std::string PIO_VAR_REAL_NAME("PIO_TF_test_var_real");
  const std::string PIO_VAR_DBL_NAME("PIO_TF_test_var_dbl");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1;
    int varid_int = -1, varid_real = -1, varid_double = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : test_data_conv_with_fval : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_INT_NAME.c_str(), PIO_INT, 1, &dimid, &varid_int);
    if(!check_ret(tf, ret, "Failed to define a var of int type : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_REAL_NAME.c_str(), PIO_REAL, 1, &dimid, &varid_real);
    if(!check_ret(tf, ret, "Failed to define a var of real type : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_DBL_NAME.c_str(), PIO_DOUBLE, 1, &dimid, &varid_double);
    if(!check_ret(tf, ret, "Failed to define a var of double type : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the int type variable out (data conversion might occur: real => int, double => int) */
    ret = PIOc_write_darray(ncid, varid_int, wioid, VEC_LOCAL_SZ, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray on a var of int type : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the real type variable out (data conversion might occur: int => real, double => real) */
    ret = PIOc_write_darray(ncid, varid_real, wioid, VEC_LOCAL_SZ, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray on a var of real type : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the double type variable out (data conversion might occur: int => double, real => double) */
    ret = PIOc_write_darray(ncid, varid_double, wioid, VEC_LOCAL_SZ, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray on a var of double type : " + filename)){
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

    ret = PIOc_inq_varid(ncid, PIO_VAR_INT_NAME.c_str(), &varid_int);
    if(!check_ret(tf, ret, "Cannot inq int var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_REAL_NAME.c_str(), &varid_real);
    if(!check_ret(tf, ret, "Cannot inq real var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_DBL_NAME.c_str(), &varid_double);
    if(!check_ret(tf, ret, "Cannot inq double var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    /* Read the int type variable back (data conversion might occur: int => real, int => double) */
    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid_int, rioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray on a var of int type : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    /* Read the real type variable back (data conversion might occur: real => int, real => double) */
    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid_real, rioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray on a var of real type : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    /* Read the double type variable back (data conversion might occur: double => int, double => real) */
    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }
    ret = PIOc_read_darray(ncid, varid_double, rioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray on a var of double type : " + filename)){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), wioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), rioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
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
    {"nc_wr_1d_mreg_exp_fval_int", nc_wr_1d_mreg_exp_fval<int>},
    {"nc_wr_1d_mreg_exp_fval_float", nc_wr_1d_mreg_exp_fval<float>},
    {"nc_wr_1d_mreg_exp_fval_double", nc_wr_1d_mreg_exp_fval<double>},
    {"nc_wr_1d_mreg_imp_fval_int", nc_wr_1d_mreg_imp_fval<int>},
    {"nc_wr_1d_mreg_imp_fval_float", nc_wr_1d_mreg_imp_fval<float>},
    {"nc_wr_1d_mreg_imp_fval_double", nc_wr_1d_mreg_imp_fval<double>},
    {"nc_wr_1d_one_val_int", nc_wr_1d_one_val<int>},
    {"nc_wr_1d_one_val_float", nc_wr_1d_one_val<float>},
    {"nc_wr_1d_one_val_double", nc_wr_1d_one_val<double>},
    {"nc_wr_1d_odd_fval_int", nc_wr_1d_odd_fval<int>},
    {"nc_wr_1d_odd_fval_float", nc_wr_1d_odd_fval<float>},
    {"nc_wr_1d_odd_fval_double", nc_wr_1d_odd_fval<double>},
    {"test_data_conv_with_fval_int", test_data_conv_with_fval<int>},
    {"test_data_conv_with_fval_float", test_data_conv_with_fval<float>},
    {"test_data_conv_with_fval_double", test_data_conv_with_fval<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_fillval2_cxx", MPI_COMM_WORLD, rfuncs);
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
