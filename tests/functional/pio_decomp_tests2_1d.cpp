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

/* Get a decomposition such that each process has to perform
 * an alltoall (communicate with all processes including itself)
 * communication to write data out.
 * If force_all2all is FALSE, the decomposition is such that
 * NPROCS * (NPROCS * VEC_BLOCK_SZ) elements are evenly distributed
 * across all processes
 * # All procs have NPROCS * VEC_BLOCK_SZ elements
 * e.g. For NPROCS = 3, VEC_BLOCK_SZ = 1,
 *       [1,2,3] [4,5,6] [7,8,9]
 * e.g. For NPROCS = 3, VEC_BLOCK_SZ = 2,
 *       [1,2,3,4,5,6] [7,8,9,10,11,12] [13,14,15,16,17,18]
 * If force_all2all is TRUE, the decomposition is such that,
 * each process reads/writes elements in
 * [rank*VEC_BLOCK_SZ + 1, rank*VEC_BLOCK_SZ + VEC_BLOCK_SZ] from
 * each process in the decomposition with the force_all2all set to
 * FALSE
 * # All procs have NPROCS * VEC_BLOCK_SZ elements
 * e.g. For NPROCS = 3, VEC_BLOCK_SZ = 1,
 *       [1,4,7] [2,5,8] [3,6,9]
 * e.g. For NPROCS = 3, VEC_BLOCK_SZ = 2,
 *       [1,2,7,8,13,14] [3,4,9,10,15,16] [5,6,11,12,17,18]
 */
void get_1d_a2a_info(int rank, int sz, int &dim, std::vector<PIO_Offset> &start,
    std::vector<int> &count, bool force_all2all)
{
  const int VEC_BLOCK_SZ = 7;

  /* Number of elements in local process */
  int lsz = sz * VEC_BLOCK_SZ;

  /* Global dimension size */
  dim = lsz * sz;

  /* Compute start/count */
  if(force_all2all){
    /* Local elements belong to multiple contiguous regions
     * : sz regions, each region of size VEC_BLOCK_SZ
     */
    start.resize(sz);
    count.resize(sz);

    for(int i = 0; i < sz; i++){
      start[i] = (rank * VEC_BLOCK_SZ + 1) + i * lsz;
      count[i] = VEC_BLOCK_SZ;
    }
  }
  else{
    /* Evenly distribute elements across all procs
     * Local elements belong to a single contiguous region
     */
    start.resize(1);
    count.resize(1);

    start[0] = rank * lsz + 1;
    count[0] = lsz;
  }
}

/* Get 1d block cycle decomposition */
void get_1d_bc_info(int rank, int sz, int &dim, PIO_Offset &start, int &count)
{
  const int VEC_BLOCK_SZ = 7;

  /* Number of elements in local process */
  int lsz = sz * VEC_BLOCK_SZ;

  /* Global dimension size */
  dim = lsz * sz;

  /* Evenly distribute elements across all procs
   * Local elements belong to a single contiguous region
   */
  start = rank * lsz + 1;
  count = lsz;
}

/* Write (and read back) data with a decomposition that forces
 * alltoall communication between the MPI processes
 */
template<typename T>
int nc_wr_rd_a2a(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  std::vector<PIO_Offset> start;
  std::vector<int> count;
  int dim = 0;

  /* Set the decomposition for writing data */
  get_1d_a2a_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count, true);
  if(!passert(tf, start.size() == count.size(), "Invalid start/count calculated for wr decomposition")){
    return PIO_EINTERNAL;
  }
  if(!passert(tf, start.size() > 0, "Invalid start calculated for wr decomposition")){
    return PIO_EINTERNAL;
  }

  int tot_count = 0;
  for(std::size_t i = 0; i < count.size(); i++){
    tot_count += count[i];
  }

  std::vector<T> wbuf(tot_count);
  std::vector<PIO_Offset> wr_compdof(tot_count);
  int k = 0;
  for(std::size_t i = 0; i < start.size(); i++){
    for(int j = 0; j < count[i]; j++){
      wbuf[k] = static_cast<T>(start[i] + j);
      wr_compdof[k] = static_cast<PIO_Offset>(wbuf[k]);
      k++;
    }
  }

  int ioid_wr = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          tot_count, wr_compdof.data(), &ioid_wr, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data */
  std::vector<T> rbuf(tot_count, 0);
  std::vector<T> exp_val(tot_count);

  get_1d_a2a_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count, false);
  if(!passert(tf, start.size() == count.size(), "Invalid start/count calculated for rd decomposition")){
    return PIO_EINTERNAL;
  }
  if(!passert(tf, start.size() > 0, "Invalid start calculated for rd decomposition")){
    return PIO_EINTERNAL;
  }

  std::vector<PIO_Offset> rd_compdof(tot_count);
  k = 0;
  for(std::size_t i = 0; i < start.size(); i++){
    for(int j = 0; j < count[i]; j++){
      exp_val[k] = static_cast<T>(start[i] + j);
      rd_compdof[k] = static_cast<PIO_Offset>(exp_val[k]);
      k++;
    }
  }

  int ioid_rd = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          tot_count, rd_compdof.data(), &ioid_rd, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Define/write/read data */
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
        std::string("Testing : nc_wr_rd_a2a : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dim, &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
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

    /* Write the variable out - alltoall communication forced via ioid_wr */
    ret = PIOc_write_darray(ncid, varid, ioid_wr, tot_count, wbuf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray : " + filename + "\n");
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

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var : " + filename + "\n");
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
    ret = PIOc_read_darray(ncid, varid, ioid_rd, tot_count, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid_rd);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid_wr);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write (and read back) data with a decomposition that is not
 * monotonically increasing (is local reverse of block cyclic decomposition)
 */
template<typename T>
int nc_wr_rev_rd(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  PIO_Offset start = 0;
  int count = 0, dim = 0;

  /* Set the decomposition for writing data
   * The data is written out using reverse order of the block decomposition
   * returned by get_1d_bc_info()
   * e.g. start = 10, count = 3
   * The data is written out with [12, 11, 10]
   */
  get_1d_bc_info(tf.get_comm_rank(), tf.get_comm_size(), dim, start, count);
  if(!passert(tf, start > 0, "Invalid start calculated for bc decomp")){
    return PIO_EINTERNAL;
  }
  if(!passert(tf, count > 0, "Invalid count calculated for bc decomp")){
    return PIO_EINTERNAL;
  }

  int tot_count = count;
  std::vector<T> wbuf(tot_count);
  std::vector<PIO_Offset> wr_compdof(tot_count);

  /* Reverse the local decomposition */
  for(int i = 0; i < count; i++){
    wbuf[i] = static_cast<T>(start + count - 1 - i);
    wr_compdof[i] = static_cast<PIO_Offset>(wbuf[i]);
  }

  int ioid_wr = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          tot_count, wr_compdof.data(), &ioid_wr, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Set the decomposition for reading data
   * The data is read out using a cyclic left-shifted (by 1) order of the block
   * decomposition returned by get_1d_bc_info()
   * e.g. start = 10, count = 3
   * The data is read with [11, 12, 10]
   */
  std::vector<T> rbuf(tot_count, 0);
  std::vector<T> exp_val(tot_count);
  std::vector<PIO_Offset> rd_compdof(tot_count);

  for(int i = 0; i < count - 1; i++){
    exp_val[i] = static_cast<T>(start + i + 1);
    rd_compdof[i] = static_cast<PIO_Offset>(exp_val[i]);
  }
  exp_val[count - 1] = static_cast<T>(start);
  rd_compdof[count - 1] = static_cast<PIO_Offset>(exp_val[count - 1]);

  int ioid_rd = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          tot_count, rd_compdof.data(), &ioid_rd, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  /* Define/write/read data */
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
        std::string("Testing : nc_wr_rev_rd : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dim, &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
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

    /* Write the variable out - reverse ordered via ioid_wr */
    ret = PIOc_write_darray(ncid, varid, ioid_wr, tot_count, wbuf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray : " + filename + "\n");
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

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not inq var : " + filename + "\n");
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
    ret = PIOc_read_darray(ncid, varid, ioid_rd, tot_count, rbuf.data());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to read darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
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

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid_rd);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (rd decomp), ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid_wr);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED (wr decomp), ret = " + std::to_string(ret) + "\n");
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
    {"nc_wr_rd_a2a_int", nc_wr_rd_a2a<int>},
    {"nc_wr_rd_a2a_float", nc_wr_rd_a2a<float>},
    {"nc_wr_rd_a2a_double", nc_wr_rd_a2a<double>},
    {"nc_wr_rev_rd_int", nc_wr_rev_rd<int>},
    {"nc_wr_rev_rd_float", nc_wr_rev_rd<float>},
    {"nc_wr_rev_rd_double", nc_wr_rev_rd<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_tests2_1d_cxx", MPI_COMM_WORLD, rfuncs);
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
