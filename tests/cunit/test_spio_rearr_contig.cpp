#include <iostream>
#include <vector>
#include <string>
#include <cassert>
#include <cstring>
#include <numeric>
#include <algorithm>
#include <sstream>
#include <functional>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "pio_rearr_contig.hpp"
#include "spio_test_framework.hpp"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

static const int FAIL = -1;

template<typename T>
bool cmp_result(SPIO_TF::Test_framework &tf, int wrank, const std::vector<T> &res, const std::vector<T> &exp)
{
  if(res.size() != exp.size()){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "ERROR: The result and expected vectors are of different sizes\n");
    return false;
  }

  for(std::size_t i = 0; i < res.size(); i++){
    if(res[i] != exp[i]){
      std::ostringstream oss;
      oss << "ERROR: Invalid/Unexpected value, array[ " << i << "] = " << res[i]
          << " (Expected array[" << i << "] = " << exp[i] << ")\n";
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "ERROR:" + oss.str());
      return false;
    }
  }

  return true;
}

iosystem_desc_t *get_iosystem(SPIO_TF::Test_framework &tf, MPI_Comm comm, int wrank, int wsz, int nio_procs)
{
  int ret = PIO_NOERR;
  static int iosysid = 1;
  iosystem_desc_t *ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t));
  if(!ios){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "Unable to allocate memory for I/O system");
    return NULL;
  }

  ios->iosysid = iosysid++;
  ios->union_comm = comm;
  ios->num_uniontasks = wsz;
  ios->union_rank = wrank;
  /* Every proc is a compute proc */
  ios->comp_comm = comm;
  ios->num_comptasks = wsz;
  ios->comp_rank = wrank;
  ios->compproc = true;

  assert(nio_procs <= wsz);

  /* Assign first nio_procs procs as I/O processes */
  int color = (wrank/nio_procs == 0) ? 0 : 1;

  ret = MPI_Comm_split(comm, color, 0, &(ios->io_comm));
  if(ret != MPI_SUCCESS){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "Unable to split comm for creating I/O system");
    free(ios);
    return NULL;
  }

  ios->num_iotasks = nio_procs;
  ios->ioproc = (color == 0) ? true : false;
  if(ios->ioproc){
    ret = MPI_Comm_rank(ios->io_comm, &(ios->io_rank));
    if(ret != MPI_SUCCESS){
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "Unable to get rank of I/O process");
      free(ios);
      return NULL;
    }
  }
  else{
    ios->io_rank = -1;
  }

  return ios;
}

void free_iosystem(iosystem_desc_t *ios){
  if(!ios){
    return;
  }

  MPI_Comm_free(&(ios->io_comm));
  free(ios);
}

int test_create_block_rearr(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();

  int ret = PIO_NOERR;

  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(tf, comm, wrank, wsz, nio_procs);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::ERROR, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int LOCAL_COMPMAP_SZ = 4;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Creating contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_c2i_block_data_rearr(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;

  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(tf, comm, wrank, wsz, nio_procs);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int LOCAL_COMPMAP_SZ = 4;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::vector<double> sdata(compmap.size());

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      std::iota(exp_data.begin(), exp_data.end(), ios->io_rank * rearr_iochunk_sz);
    }

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);
    std::transform(compmap.cbegin(), compmap.cend(), sdata.begin(),
      [](const PIO_Offset i){ return static_cast<double>(i); });
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), sdata.size() * sizeof(double),
            rdata.data(), rdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, rdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_block_data_rearr(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;

  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(tf, comm, wrank, wsz, nio_procs);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int LOCAL_COMPMAP_SZ = 4;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::vector<double> sdata(compmap.size());

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      std::iota(exp_data.begin(), exp_data.end(), ios->io_rank * rearr_iochunk_sz);
    }

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);
    std::transform(compmap.cbegin(), compmap.cend(), sdata.begin(),
      [](const PIO_Offset i){ return static_cast<double>(i); });
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from compute processes to I/O processes */
    ret = rearr.rearrange_comp2io(sdata.data(), sdata.size() * sizeof(double),
            rdata.data(), rdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, rdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement (compute to I/O procs)\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    exp_data.resize(LOCAL_COMPMAP_SZ);
    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), rdata.size() * sizeof(double),
            sdata.data(), sdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, sdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_rev_block_data_rearr(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();

  int ret = PIO_NOERR;
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(tf, comm, wrank, wsz, nio_procs);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int LOCAL_COMPMAP_SZ = 4;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::vector<double> sdata(compmap.size());

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      std::iota(exp_data.begin(), exp_data.end(), ios->io_rank * rearr_iochunk_sz);
    }

    //std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);
    PIO_Offset compmap_val = wrank * LOCAL_COMPMAP_SZ;
    std::generate(compmap.begin(), compmap.end(),
      [&compmap_val, gdimlen]() mutable { return gdimlen - 1 - compmap_val++; });
    std::transform(compmap.cbegin(), compmap.cend(), sdata.begin(),
      [](const PIO_Offset i){ return static_cast<double>(i); });
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), sdata.size() * sizeof(double),
            rdata.data(), rdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, rdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    exp_data.resize(LOCAL_COMPMAP_SZ);
    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), rdata.size() * sizeof(double),
            sdata.data(), sdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed (I/O to compute procs)");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, sdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_mrange_block_data_rearr(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();

  int ret = PIO_NOERR;
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(tf, comm, wrank, wsz, nio_procs);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int FIRST_RANGE_SZ = 3;
    const int SECOND_RANGE_SZ = 1;
    const int THIRD_RANGE_SZ = 2;
    const int LOCAL_COMPMAP_SZ = FIRST_RANGE_SZ + SECOND_RANGE_SZ + THIRD_RANGE_SZ;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::vector<double> sdata(compmap.size());

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      std::iota(exp_data.begin(), exp_data.end(), ios->io_rank * rearr_iochunk_sz);
    }

    std::iota(compmap.begin(), compmap.begin() + FIRST_RANGE_SZ, wrank * FIRST_RANGE_SZ);
    std::iota(compmap.begin() + FIRST_RANGE_SZ, compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, wsz * FIRST_RANGE_SZ + wrank * SECOND_RANGE_SZ);
    std::iota(compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, compmap.end(), wsz * (FIRST_RANGE_SZ + SECOND_RANGE_SZ) + wrank * THIRD_RANGE_SZ);
    std::transform(compmap.cbegin(), compmap.cend(), sdata.begin(),
      [](const PIO_Offset i){ return static_cast<double>(i); });
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), sdata.size() * sizeof(double),
            rdata.data(), rdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, rdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    exp_data.resize(LOCAL_COMPMAP_SZ);
    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), rdata.size() * sizeof(double),
            sdata.data(), sdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed (I/O to compute procs)");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, sdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_mrange_oddz_data_rearr(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();

  bool is_odd_proc = ((wrank % 2) != 0) ? true : false;
  int nodd_procs = wsz/2;
  int neven_procs = wsz - nodd_procs;
  int ret = PIO_NOERR;

  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(tf, comm, wrank, wsz, nio_procs);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int FIRST_RANGE_SZ = 3;
    const int SECOND_RANGE_SZ = 1;
    const int THIRD_RANGE_SZ = 2;
    const int LOCAL_COMPMAP_SZ = FIRST_RANGE_SZ + SECOND_RANGE_SZ + THIRD_RANGE_SZ;
    /* Only even procs have data */
    int gdimlen = LOCAL_COMPMAP_SZ * neven_procs;
    std::vector<PIO_Offset> compmap;
    std::vector<double> sdata;

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      std::iota(exp_data.begin(), exp_data.end(), ios->io_rank * rearr_iochunk_sz);
    }

    if(!is_odd_proc){
      compmap.resize(LOCAL_COMPMAP_SZ);
      sdata.resize(compmap.size());

      int neven_procs_before_wrank = wrank - (wrank / 2);
      std::iota(compmap.begin(), compmap.begin() + FIRST_RANGE_SZ, neven_procs_before_wrank * FIRST_RANGE_SZ);
      std::iota(compmap.begin() + FIRST_RANGE_SZ, compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, neven_procs * FIRST_RANGE_SZ + neven_procs_before_wrank * SECOND_RANGE_SZ);
      std::iota(compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, compmap.end(), neven_procs * (FIRST_RANGE_SZ + SECOND_RANGE_SZ) + neven_procs_before_wrank * THIRD_RANGE_SZ);
      std::transform(compmap.cbegin(), compmap.cend(), sdata.begin(),
        [](const PIO_Offset i){ return static_cast<double>(i); });
    }

    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), sdata.size() * sizeof(double),
            rdata.data(), rdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, rdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    if(!is_odd_proc){
      exp_data.resize(LOCAL_COMPMAP_SZ);
    }
    else{
      exp_data.clear();
    }

    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), rdata.size() * sizeof(double),
            sdata.data(), sdata.size() * sizeof(double), 1);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed (I/O to compute procs)");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(tf, wrank, sdata, exp_data)){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int main(int argc, char *argv[])
{
  int ret;

  ret = MPI_Init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    std::cerr << "MPI_Init() FAILED, ret = " << ret << "\n";
    return ret;
  }

  std::vector<std::pair<std::string, std::function<int(SPIO_TF::Test_framework &)> > > rfuncs = {
    {"test_create_block_rearr", test_create_block_rearr}, 
    {"test_c2i_block_data_rearr", test_c2i_block_data_rearr},
    {"test_block_data_rearr", test_block_data_rearr},
    {"test_rev_block_data_rearr", test_rev_block_data_rearr},
    {"test_mrange_block_data_rearr", test_mrange_block_data_rearr},
    {"test_mrange_oddz_data_rearr", test_mrange_oddz_data_rearr}
  };

  SPIO_TF::Test_framework tf("test_spio_rearr_contig", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);
  ret = tf.run();
  tf.finalize();

  MPI_Finalize();

  return ret;
}
