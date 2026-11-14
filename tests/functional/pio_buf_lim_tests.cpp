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

/* Get the iobuf size.
 * lbuf_sz -> local buffer size
 * lbuf_type_sz -> local buffer type size
 * nbufs -> number of buffers before a flush
 * The size of iobuf is calculated so that a flush is
 * called every nbufs
 * (iobuf_sz < lbuf_sz * lbuf_type_sz * nbufs)
 */
PIO_Offset get_iobuf_sz(PIO_Offset lbuf_sz, PIO_Offset lbuf_type_sz, int nbufs)
{
  PIO_Offset iobuf_sz = lbuf_sz * lbuf_type_sz * nbufs - 1;
  if(iobuf_sz < 0){
    iobuf_sz = 0;
  }
  return iobuf_sz;
}

/* Test that writes out a variable with buffer limit set to 0 */
template<typename T>
int nc_wr_buf_limit_zero(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  int dims[1];

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  std::vector<T> wbuf(VEC_LOCAL_SZ, static_cast<T>(tf.get_comm_rank()));

  int ioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_buf_limit_zero : ") + iotype_name + "\n");

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

    const PIO_Offset PIO_BUFFER_SZ_ZERO = 0;
    PIOc_set_buffer_size_limit(PIO_BUFFER_SZ_ZERO);

    /* Query the current buffer size limit by passing a negative value for the
     * new limit, which will be ignored by PIOc_set_buffer_size_limit
     */
    const PIO_Offset PIO_BUFFER_SZ_NEG = -1;
    PIO_Offset cur_buf_sz_limit = PIOc_set_buffer_size_limit(PIO_BUFFER_SZ_NEG);
    if(!passert(tf, cur_buf_sz_limit == PIO_BUFFER_SZ_ZERO,
          "Current buffer size limit should equal the previously set zero limit")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
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

    std::vector<T> rbuf(VEC_LOCAL_SZ, 0);
    ret = PIOc_read_darray(ncid, varid, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Test that writes out multiple variables with implicit flush.
 * The flush is not explicitly invoked using PIOc_sync, instead
 * implicitly invoked due to the buffer size limit set for the io
 * procs.
 */
template<typename T>
int nc_wr_mvar_impl_flush(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  int dims[1];

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  std::vector<T> wbuf(VEC_LOCAL_SZ, static_cast<T>(tf.get_comm_rank()));

  int ioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  const std::string PIO_VAR1_NAME("PIO_TF_test_var1");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2");
  const std::string PIO_VAR3_NAME("PIO_TF_test_var3");
  const std::string PIO_VAR4_NAME("PIO_TF_test_var4");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1;
    int varid1 = -1, varid2 = -1, varid3 = -1, varid4 = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_mvar_impl_flush : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR1_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid1);
    if(!check_ret(tf, ret, "Failed to define a var1 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR2_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid2);
    if(!check_ret(tf, ret, "Failed to define a var2 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR3_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid3);
    if(!check_ret(tf, ret, "Failed to define a var3 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR4_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid4);
    if(!check_ret(tf, ret, "Failed to define a var4 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Set the buffer size limit such that we trigger an implicit flush
     * on the next write. To flush every var set the buffer size limit
     * to be slightly less than the size (local) required to cache a
     * single variable. When the second variable is written out the first
     * variable will be flushed out (since internal cache >= buffer size limit)
     */

    /* ============ Flush every 1 var ====================== */
    /* Before the 2nd variable is written out the data
     * corresponding to the first variable will be flushed out
     */
    PIO_Offset iobuf_sz = get_iobuf_sz(VEC_LOCAL_SZ, sizeof(T), 1);
    PIOc_set_buffer_size_limit(iobuf_sz);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "Testing : Flush every 1 var : buffer limit = " + std::to_string(iobuf_sz) + "\n");

    ret = PIOc_write_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Flush for the first variable should happen inside PIOc_write_darray(var2) */
    ret = PIOc_write_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_WRITE);
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

    ret = PIOc_inq_varid(ncid, PIO_VAR3_NAME.c_str(), &varid3);
    if(!check_ret(tf, ret, "Could not inq var3 : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    std::vector<T> rbuf(VEC_LOCAL_SZ, 0);
    ret = PIOc_read_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 1)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 2)")){
      return PIO_EINTERNAL;
    }

    /* ============ Flush every 2 vars ====================== */
    iobuf_sz = get_iobuf_sz(VEC_LOCAL_SZ, sizeof(T), 2);
    PIOc_set_buffer_size_limit(iobuf_sz);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "Testing : Flush every 2 vars : buffer limit = " + std::to_string(iobuf_sz) + "\n");

    ret = PIOc_write_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Flush for the previous 2 vars, var1 and var2 should happen
     * inside PIOc_write_darray(var3)
     */
    ret = PIOc_write_darray(ncid, varid3, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var3) : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_WRITE);
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

    ret = PIOc_inq_varid(ncid, PIO_VAR3_NAME.c_str(), &varid3);
    if(!check_ret(tf, ret, "Could not inq var3 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR4_NAME.c_str(), &varid4);
    if(!check_ret(tf, ret, "Could not inq var4 : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 1)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 2)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid3, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var3) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 3)")){
      return PIO_EINTERNAL;
    }

    /* ============ Flush every 3 vars ====================== */
    iobuf_sz = get_iobuf_sz(VEC_LOCAL_SZ, sizeof(T), 3);
    PIOc_set_buffer_size_limit(iobuf_sz);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "Testing : Flush every 3 vars : buffer limit = " + std::to_string(iobuf_sz) + "\n");

    ret = PIOc_write_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid3, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var3) : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Flush for the previous 3 vars, var1, var2, var3 should happen
     * inside PIOc_write_darray(var4)
     */
    ret = PIOc_write_darray(ncid, varid4, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var4) : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_WRITE);
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

    ret = PIOc_inq_varid(ncid, PIO_VAR3_NAME.c_str(), &varid3);
    if(!check_ret(tf, ret, "Could not inq var3 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR4_NAME.c_str(), &varid4);
    if(!check_ret(tf, ret, "Could not inq var4 : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 1)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 2)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid3, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var3) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 3)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid4, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var4) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 4)")){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Test that writes out two variables with implicit flush. Then
 * a third variable is written out (after the implicit flush).
 * The data corresponding to the third variable is only synced
 * when PIOc_sync() (or close/reopen) is called (explicit flush).
 */
template<typename T>
int nc_wr_after_impl_flush(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  const int VEC_LOCAL_SZ = 7;
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  int dims[1];

  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  std::vector<T> wbuf(VEC_LOCAL_SZ, static_cast<T>(tf.get_comm_rank()));

  int ioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_wr_aftr_impl_flush_tests.testfile");
  const std::string PIO_VAR1_NAME("PIO_TF_test_var1");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2");
  const std::string PIO_VAR3_NAME("PIO_TF_test_var3");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1;
    int varid1 = -1, varid2 = -1, varid3 = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_after_impl_flush : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR1_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid1);
    if(!check_ret(tf, ret, "Failed to define a var1 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR2_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid2);
    if(!check_ret(tf, ret, "Failed to define a var2 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR3_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid3);
    if(!check_ret(tf, ret, "Failed to define a var3 : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* ============ Flush every 2 vars ====================== */
    PIO_Offset iobuf_sz = get_iobuf_sz(VEC_LOCAL_SZ, sizeof(T), 2);
    PIOc_set_buffer_size_limit(iobuf_sz);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "Testing : Flush every 2 vars : buffer limit = " + std::to_string(iobuf_sz) + "\n");

    /* Write two variables */
    ret = PIOc_write_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    /* The implicit flush should happen for the previous
     * two variables inside the next PIOc_write_darray() call
     *
     * Write out another variable, the data for var3 is not
     * flushed out until we explicitly sync/close
     */
    ret = PIOc_write_darray(ncid, varid3, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray(var3) : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Sync data for all three variables (only results in explicit
     * flush of data corresponding to var3, the third variable)
     */
#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_WRITE);
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

    ret = PIOc_inq_varid(ncid, PIO_VAR3_NAME.c_str(), &varid3);
    if(!check_ret(tf, ret, "Could not inq var3 : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    std::vector<T> rbuf(VEC_LOCAL_SZ, 0);
    ret = PIOc_read_darray(ncid, varid1, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var1) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 1)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid2, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var2) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 2)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid3, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray(var3) : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (var 3)")){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
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
    {"nc_wr_buf_limit_zero_int", nc_wr_buf_limit_zero<int>},
    {"nc_wr_buf_limit_zero_float", nc_wr_buf_limit_zero<float>},
    {"nc_wr_buf_limit_zero_double", nc_wr_buf_limit_zero<double>},
    {"nc_wr_mvar_impl_flush_int", nc_wr_mvar_impl_flush<int>},
    {"nc_wr_mvar_impl_flush_float", nc_wr_mvar_impl_flush<float>},
    {"nc_wr_mvar_impl_flush_double", nc_wr_mvar_impl_flush<double>},
    {"nc_wr_after_impl_flush_int", nc_wr_after_impl_flush<int>},
    {"nc_wr_after_impl_flush_float", nc_wr_after_impl_flush<float>},
    {"nc_wr_after_impl_flush_double", nc_wr_after_impl_flush<double>}
  };

  SPIO_TF::Test_framework tf("pio_buf_lim_tests_cxx", MPI_COMM_WORLD, rfuncs);
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
