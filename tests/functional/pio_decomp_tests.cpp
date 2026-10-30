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

template<typename T>
int nc_write_1d_darray(SPIO_TF::Test_framework &tf)
{
  const int VEC_LOCAL_SZ = 7;
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int ioid = -1;
  int dims[1];
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  std::vector<T> buf(VEC_LOCAL_SZ);

  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    buf[i] = tf.get_comm_rank();
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_write_1d_darray : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, "PIO_TF_test_var", Pio_type_map<T>::value, 1, &dimid, &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end redef mode : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, ioid, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* FIXME: Verify the written output */

    ret = PIOc_closefile(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file : " + filename + "\n");
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Test freeing I/O decomposition before closing (writing data to) file */
int free_before_close(SPIO_TF::Test_framework &tf)
{
  const int VEC_LOCAL_SZ = 7;
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int dims[1];
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  std::vector<int> buf(VEC_LOCAL_SZ), rbuf(VEC_LOCAL_SZ);
  const std::string PIO_VAR1_NAME("PIO_TF_test_var1");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2");

  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    buf[i] = tf.get_comm_rank();
  }

  const std::string filename1("test_pio_decomp_simple_tests_free_before_close_file1.testfile");
  const std::string filename2("test_pio_decomp_simple_tests_free_before_close_file2.testfile");
  const std::string filename3("test_pio_decomp_simple_tests_free_before_close_file3.testfile");

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int i = 0; i < num_flavors; i++){
    /* pio_file1 uses iodesc1, iodesc2
     * pio_file2 uses iodesc1, iodesc3
     * pio_file3 uses iodesc2
     */
    int ncid1 = -1, ncid2 = -1, ncid3 = -1;
    int dimid1 = -1, dimid2 = -1, dimid3 = -1;
    int varid1_file1 = -1, varid2_file1 = -1, varid1_file2 = -1, varid2_file2 = -1, varid1_file3 = -1;
    int iodesc1 = -1, iodesc2 = -1, iodesc3 = -1, rd_iodesc = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : free_before_close : ") + iotype_name + "\n");

    /* Create the 3 files */
    ret = PIOc_createfile(tf.get_iosysid(), &ncid1, &(flavors[i]), filename1.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create (file 1) " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_createfile(tf.get_iosysid(), &ncid2, &(flavors[i]), filename2.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create (file 2) " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_createfile(tf.get_iosysid(), &ncid3, &(flavors[i]), filename3.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create (file 3) " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid1, "PIO_TF_test_dim", dims[0], &dimid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim (file 1) : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid2, "PIO_TF_test_dim", dims[0], &dimid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim (file 2) : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid3, "PIO_TF_test_dim", dims[0], &dimid3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim (file 3) : " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid1, PIO_VAR1_NAME.c_str(), PIO_INT, 1, &dimid1, &varid1_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var (var 1 + file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid1, PIO_VAR2_NAME.c_str(), PIO_INT, 1, &dimid1, &varid2_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var (var 2 + file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid2, PIO_VAR1_NAME.c_str(), PIO_INT, 1, &dimid2, &varid1_file2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var (var 1 + file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid2, PIO_VAR2_NAME.c_str(), PIO_INT, 1, &dimid2, &varid2_file2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var (var 2 + file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid3, PIO_VAR1_NAME.c_str(), PIO_INT, 1, &dimid3, &varid1_file3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var (var 1 + file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end define mode (file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end define mode (file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end define mode (file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    /* Create I/O decomps : Same decomposition for all variables in all files */
    ret = PIOc_InitDecomp(tf.get_iosysid(), PIO_INT, 1, dims,
            VEC_LOCAL_SZ, compdof.data(), &iodesc1, NULL, NULL, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_InitDecomp() FAILED (iodesc1), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_InitDecomp(tf.get_iosysid(), PIO_INT, 1, dims,
            VEC_LOCAL_SZ, compdof.data(), &iodesc2, NULL, NULL, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_InitDecomp() FAILED (iodesc2), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_InitDecomp(tf.get_iosysid(), PIO_INT, 1, dims,
            VEC_LOCAL_SZ, compdof.data(), &iodesc3, NULL, NULL, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_InitDecomp() FAILED (iodesc3), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

    /* Write the variable out into the three files
     * file 1 : writes using iodesc1 and iodesc2
     * file 2 : writes using iodesc1 and iodesc3
     * file 3 : writes using iodesc2
     */
    ret = PIOc_write_darray(ncid1, varid1_file1, iodesc1, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray (file 1 + var 1 + iodesc 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid1, varid2_file1, iodesc2, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray (file 1 + var 2 + iodesc 2): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid2, varid1_file2, iodesc1, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray (file 2 + var 1 + iodesc 1): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid2, varid2_file2, iodesc3, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray (file 2 + var 2 + iodesc 3): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid3, varid1_file3, iodesc2, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray (file 3 + var 1 + iodesc 2): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    /* Free I/O decompositions (write decomps) before sync (and before closing the files) */
    ret = PIOc_freedecomp(tf.get_iosysid(), iodesc1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_freedecomp() FAILED (iodesc1), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_freedecomp(tf.get_iosysid(), iodesc2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_freedecomp() FAILED (iodesc2), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_freedecomp(tf.get_iosysid(), iodesc3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_freedecomp() FAILED (iodesc3), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file (file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file (file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file (file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid1, &(flavors[i]), filename1.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file (file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid2, &(flavors[i]), filename2.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file (file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid3, &(flavors[i]), filename3.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file (file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid1, PIO_VAR1_NAME.c_str(), &varid1_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var (var 1 + file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid1, PIO_VAR2_NAME.c_str(), &varid2_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var (var 2 + file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid2, PIO_VAR1_NAME.c_str(), &varid1_file2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var (var 1 + file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid2, PIO_VAR2_NAME.c_str(), &varid2_file2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var (var 2 + file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid3, PIO_VAR1_NAME.c_str(), &varid1_file3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var (var 1 + file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file (file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_sync(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file (file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_sync(ncid3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file (file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }
#endif

    /* Create read I/O decomp */
    ret = PIOc_InitDecomp(tf.get_iosysid(), PIO_INT, 1, dims,
            VEC_LOCAL_SZ, compdof.data(), &rd_iodesc, NULL, NULL, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_InitDecomp() FAILED (rd_iodesc), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }

    /* Read and verify contents of the first file */
    for(int j = 0; j < VEC_LOCAL_SZ; j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid1, varid1_file1, rd_iodesc, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var 1 + file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val (var 1 + file 1)")){
      return PIO_EINTERNAL;
    }

    for(int j = 0; j < VEC_LOCAL_SZ; j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid1, varid2_file1, rd_iodesc, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var 2 + file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val (var 2 + file 1)")){
      return PIO_EINTERNAL;
    }

    /* Close/delete first file, the next two files will be closed/deleted together */
    ret = PIOc_closefile(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file (file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename1.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file (file 1): " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    /* Verify the contents of the remaining files (interleaved reads) */
    /* Read from second file */
    for(int j = 0; j < VEC_LOCAL_SZ; j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid2, varid1_file2, rd_iodesc, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var 1 + file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val (var 1 + file 2)")){
      return PIO_EINTERNAL;
    }

    /* Read from third file */
    for(int j = 0; j < VEC_LOCAL_SZ; j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid3, varid1_file3, rd_iodesc, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var 1 + file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val (var 1 + file 3)")){
      return PIO_EINTERNAL;
    }

    /* Now read back from second file */
    for(int j = 0; j < VEC_LOCAL_SZ; j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid2, varid2_file2, rd_iodesc, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray (var 2 + file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val (var 2 + file 2)")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file (file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid3);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file (file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename2.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file (file 2): " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename3.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file (file 3): " + filename3 + "\n");
      return PIO_EINTERNAL;
    }

    /* Test freeing (read) I/O decomp after the files are closed/deleted */
    ret = PIOc_freedecomp(tf.get_iosysid(), rd_iodesc);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: PIOc_freedecomp() FAILED (rd_iodesc), ret = " + std::to_string(ret) + "\n");
      return PIO_EINTERNAL;
    }
  }

  return PIO_NOERR;
}

/* Write 1d array, although diff procs have different
 * number of elements to write locally they all use
 * the same buffer size (compdof size is different for
 * each rank but buf size is the same)
 * Odd procs write out VEC_LOCAL_SZ_ODD elements &
 * even procs write out VEC_LOCAL_SZ_EVEN elements, but
 * all procs use buf[MAX_VEC_SZ]
 * eg:
 * Elements in buffer on each proc with MAX_VEC_SZ = 2,
 * [0 1] [2 X] [3 4] [5 X] ...
 * The 'X'es in the buffer are not written out
 */
template<typename T>
int nc_wr_1d_const_buf_sz(SPIO_TF::Test_framework &tf)
{
  const int MAX_VEC_SZ = 2;
  const int VEC_LOCAL_SZ_ODD = MAX_VEC_SZ - 1;
  const int VEC_LOCAL_SZ_EVEN = MAX_VEC_SZ;
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int ioid = -1;
  int dims[1];
  const std::string PIO_VAR1_NAME("PIO_TF_test_var1");
  const std::string PIO_VAR2_NAME("PIO_TF_test_var2");

  int comm_rank = tf.get_comm_rank();
  int comm_sz = tf.get_comm_size();
  int nodd_procs = comm_sz / 2;
  /* Number of odd procs before this rank */
  int nodd_procs_bfr = comm_rank / 2;
  int neven_procs = comm_sz - nodd_procs;
  /* Number of even procs before this rank */
  int neven_procs_bfr = comm_rank - nodd_procs_bfr;

  /* Odd procs write out VEC_LOCAL_SZ_ODD elements &
   * even procs write out VEC_LOCAL_SZ_EVEN elements
   */
  int cdof_sz = VEC_LOCAL_SZ_ODD;
  if(comm_rank % 2 == 0){
    cdof_sz = VEC_LOCAL_SZ_EVEN;
  }

  std::vector<PIO_Offset> compdof(cdof_sz);
  std::vector<T> wbuf(MAX_VEC_SZ, 0), rbuf(MAX_VEC_SZ, 0);

  /* Find out where compdof starts for this rank */
  int compdof_rel_start = nodd_procs_bfr * VEC_LOCAL_SZ_ODD +
      neven_procs_bfr * VEC_LOCAL_SZ_EVEN;
  for(int i = 0; i < cdof_sz; i++){
    compdof[i] = compdof_rel_start + (i + 1);
    wbuf[i] = i + 1;
  }
  dims[0] = nodd_procs * VEC_LOCAL_SZ_ODD + neven_procs * VEC_LOCAL_SZ_EVEN;

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          cdof_sz, compdof.data(), &ioid, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid1 = -1, varid2 = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_1d_const_buf_sz : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR1_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define first var : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR2_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define second var : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end redef mode : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* Write first variable out */
    ret = PIOc_write_darray(ncid, varid1, ioid, cdof_sz, wbuf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write first darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* Write second variable out */
    ret = PIOc_write_darray(ncid, varid2, ioid, cdof_sz, wbuf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write second darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR1_NAME.c_str(), &varid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var1 : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR2_NAME.c_str(), &varid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var2 : " + filename + "\n");
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file : " + filename + "\n");
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid1, ioid, cdof_sz, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read first darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (first var)")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid2, ioid, cdof_sz, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read second darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val (second var)")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file : " + filename + "\n");
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write variables to two different files, reusing a single I/O
 * decomposition across both files (and across all 3 variables written
 * - 2 vars in file1 and 1 var in file2)
 */
template<typename T>
int nc_write_1d_reuse_decomp(SPIO_TF::Test_framework &tf)
{
  const int VEC_LOCAL_SZ = 7;
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int ioid = -1;
  int dims[1];
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  std::vector<T> buf(VEC_LOCAL_SZ), rbuf(VEC_LOCAL_SZ);
  const std::string PIO_VAR1_FILE1_NAME("PIO_TF_test_var1");
  const std::string PIO_VAR2_FILE1_NAME("PIO_TF_test_var2");
  const std::string PIO_VAR1_FILE2_NAME("PIO_TF_test_var1");

  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    buf[i] = tf.get_comm_rank();
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename1("test_pio_decomp_simple_tests.testfile1");
  const std::string filename2("test_pio_decomp_simple_tests.testfile2");
  for(int i = 0; i < num_flavors; i++){
    int ncid1 = -1, ncid2 = -1;
    int dimid1 = -1, dimid2 = -1;
    int varid1_file1 = -1, varid2_file1 = -1, varid1_file2 = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_write_1d_reuse_decomp : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid1, &(flavors[i]), filename1.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_createfile(tf.get_iosysid(), &ncid2, &(flavors[i]), filename2.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid1, "PIO_TF_test_dim", dims[0], &dimid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid2, "PIO_TF_test_dim", dims[0], &dimid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid1, PIO_VAR1_FILE1_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid1, &varid1_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define first var : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid1, PIO_VAR2_FILE1_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid1, &varid2_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define second var : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid2, PIO_VAR1_FILE2_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid2, &varid1_file2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define first var : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end redef mode : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end redef mode : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    /* Write the variables - file1 */
    ret = PIOc_write_darray(ncid1, varid1_file1, ioid, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write first darray : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_write_darray(ncid1, varid2_file1, ioid, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write second darray : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    /* Write the variable - file2 */
    ret = PIOc_write_darray(ncid2, varid1_file2, ioid, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write first darray : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid1, &(flavors[i]), filename1.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid1, PIO_VAR1_FILE1_NAME.c_str(), &varid1_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var1 file1 : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid1, PIO_VAR2_FILE1_NAME.c_str(), &varid2_file1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var2 file1 : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid1, varid1_file1, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read first darray : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid1, varid2_file1, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read second darray : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val")){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid2, &(flavors[i]), filename2.c_str(), PIO_NOWRITE);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not reopen file : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid2, PIO_VAR1_FILE2_NAME.c_str(), &varid1_file2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var1 file2 : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to sync file : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid2, varid1_file2, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read first darray : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, buf, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid2);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename1.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file : " + filename1 + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename2.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file : " + filename2 + "\n");
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(ret) + "\n");
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
    {"nc_write_1d_darray_int", nc_write_1d_darray<int>},
    {"nc_write_1d_darray_float", nc_write_1d_darray<float>},
    {"nc_write_1d_darray_double", nc_write_1d_darray<double>},
    {"free_before_close", free_before_close},
    {"nc_wr_1d_const_buf_sz_int", nc_wr_1d_const_buf_sz<int>},
    {"nc_wr_1d_const_buf_sz_float", nc_wr_1d_const_buf_sz<float>},
    {"nc_wr_1d_const_buf_sz_double", nc_wr_1d_const_buf_sz<double>},
    {"nc_write_1d_reuse_decomp_int", nc_write_1d_reuse_decomp<int>},
    {"nc_write_1d_reuse_decomp_float", nc_write_1d_reuse_decomp<float>},
    {"nc_write_1d_reuse_decomp_double", nc_write_1d_reuse_decomp<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_tests", MPI_COMM_WORLD, rfuncs);
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
