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

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#define LOG_RANK0(rank, ...)                      \
            do{                                   \
              if(rank == 0){                      \
                fprintf(stderr, __VA_ARGS__);     \
              }                                   \
            }while(0);

static const int FAIL = -1;

template<typename T>
bool cmp_result(int wrank, const std::vector<T> &res, const std::vector<T> &exp)
{
  
  if(res.size() != exp.size()){
    LOG_RANK0(wrank, "ERROR: The result and expected vectors are of different sizes\n");
    return false;
  }

  for(std::size_t i = 0; i < res.size(); i++){
    if(res[i] != exp[i]){
      std::ostringstream oss;
      oss << "ERROR: Invalid/Unexpected value, array[ " << i << "] = " << res[i]
          << " (Expected array[" << i << "] = " << exp[i] << ")";
      LOG_RANK0(wrank, "ERROR: %s\n", oss.str().c_str());
      return false;
    }
  }

  return true;
}

iosystem_desc_t *get_iosystem(MPI_Comm comm, int wrank, int wsz, int nio_procs)
{
  int ret = PIO_NOERR;
  static int iosysid = 1;
  iosystem_desc_t *ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t));
  if(!ios){
    LOG_RANK0(wrank, "Unable to allocate memory for I/O system");
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
    LOG_RANK0(wrank, "Unable to split comm for creating I/O system");
    free(ios);
    return NULL;
  }

  ios->num_iotasks = nio_procs;
  ios->ioproc = (color == 0) ? true : false;
  if(ios->ioproc){
    ret = MPI_Comm_rank(ios->io_comm, &(ios->io_rank));
    if(ret != MPI_SUCCESS){
      LOG_RANK0(wrank, "Unable to get rank of I/O process");
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

int test_c2i_block_data_rearr_nvars(MPI_Comm comm, int wrank, int wsz, int nvars)
{
  int ret = PIO_NOERR;
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int LOCAL_COMPMAP_SZ = 4;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::vector<double> sdata(compmap.size() * nvars);

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t one_var_rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
      std::size_t rdata_sz = one_var_rdata_sz * nvars;
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      for(int i = 0; i < nvars; i++){
        std::iota(exp_data.begin() + i * one_var_rdata_sz, exp_data.begin() + (i + 1) * one_var_rdata_sz, ios->io_rank * rearr_iochunk_sz);
      }
    }

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);

    for(int i = 0; i < nvars; i++){
      std::transform(compmap.cbegin(), compmap.cend(), sdata.begin() + i * LOCAL_COMPMAP_SZ,
        [](const PIO_Offset i){ return static_cast<double>(i); });
    }
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), compmap.size(),
            rdata.data(), rdata.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_block_data_rearr_nvars(MPI_Comm comm, int wrank, int wsz, int nvars)
{
  int ret = PIO_NOERR;
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int LOCAL_COMPMAP_SZ = 4;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::vector<double> sdata(compmap.size() * nvars);

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t one_var_rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
      std::size_t rdata_sz = one_var_rdata_sz * nvars;
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      for(int i = 0; i < nvars; i++){
        std::iota(exp_data.begin() + i * one_var_rdata_sz, exp_data.begin() + (i + 1) * one_var_rdata_sz, ios->io_rank * rearr_iochunk_sz);
      }
    }

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);

    for(int i = 0; i < nvars; i++){
      std::transform(compmap.cbegin(), compmap.cend(), sdata.begin() + i * LOCAL_COMPMAP_SZ,
        [](const PIO_Offset i){ return static_cast<double>(i); });
    }
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), compmap.size(),
            rdata.data(), rdata.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    exp_data.resize(LOCAL_COMPMAP_SZ * nvars);
    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), compmap.size(),
            sdata.data(), compmap.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, sdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_rev_block_data_rearr_nvars(MPI_Comm comm, int wrank, int wsz, int nvars)
{
  int ret = PIO_NOERR;
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system");
    return PIO_EINTERNAL;
  }
    
  try{
    SPIO::DataRearr::Contig_rearr rearr(ios);
    const int LOCAL_COMPMAP_SZ = 4;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::vector<double> sdata(compmap.size() * nvars);

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t one_var_rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
      std::size_t rdata_sz = one_var_rdata_sz * nvars;
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      for(int i = 0; i < nvars; i++){
        std::iota(exp_data.begin() + i * one_var_rdata_sz, exp_data.begin() + (i + 1) * one_var_rdata_sz, ios->io_rank * rearr_iochunk_sz);
      }
    }

    //std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);
    PIO_Offset compmap_val = wrank * LOCAL_COMPMAP_SZ;
    std::generate(compmap.begin(), compmap.end(),
      [&compmap_val, gdimlen]() mutable { return gdimlen - 1 - compmap_val++; });

    for(int i = 0; i < nvars; i++){
      std::transform(compmap.cbegin(), compmap.cend(), sdata.begin() + i * LOCAL_COMPMAP_SZ,
        [](const PIO_Offset i){ return static_cast<double>(i); });
    }
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), compmap.size(),
            rdata.data(), rdata.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    exp_data.resize(LOCAL_COMPMAP_SZ * nvars);
    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), compmap.size(),
            sdata.data(), compmap.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed (I/O to compute procs)");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, sdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_mrange_block_data_rearr_nvars(MPI_Comm comm, int wrank, int wsz, int nvars)
{
  int ret = PIO_NOERR;
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system");
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
    std::vector<double> sdata(compmap.size() * nvars);

    std::vector<double> rdata, exp_data;

    if(ios->ioproc){
      std::size_t rearr_iochunk_sz = gdimlen/nio_procs;
      std::size_t one_var_rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
      std::size_t rdata_sz = one_var_rdata_sz * nvars;
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      for(int i = 0; i < nvars; i++){
        std::iota(exp_data.begin() + i * one_var_rdata_sz, exp_data.begin() + (i + 1) * one_var_rdata_sz, ios->io_rank * rearr_iochunk_sz);
      }
    }

    std::iota(compmap.begin(), compmap.begin() + FIRST_RANGE_SZ, wrank * FIRST_RANGE_SZ);
    std::iota(compmap.begin() + FIRST_RANGE_SZ, compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, wsz * FIRST_RANGE_SZ + wrank * SECOND_RANGE_SZ);
    std::iota(compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, compmap.end(), wsz * (FIRST_RANGE_SZ + SECOND_RANGE_SZ) + wrank * THIRD_RANGE_SZ);

    for(int i = 0; i < nvars; i++){
      std::transform(compmap.cbegin(), compmap.cend(), sdata.begin() + i * LOCAL_COMPMAP_SZ,
        [](const PIO_Offset i){ return static_cast<double>(i); });
    }
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), compmap.size(),
            rdata.data(), rdata.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    exp_data.resize(LOCAL_COMPMAP_SZ * nvars);
    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), compmap.size(),
            sdata.data(), compmap.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed (I/O to compute procs)");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, sdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_mrange_oddz_data_rearr_nvars(MPI_Comm comm, int wrank, int wsz, int nvars)
{
  bool is_odd_proc = ((wrank % 2) != 0) ? true : false;
  int nodd_procs = wsz/2;
  int neven_procs = wsz - nodd_procs;
  int ret = PIO_NOERR;

  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system");
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
      std::size_t one_var_rdata_sz = (ios->io_rank != (nio_procs - 1)) ? rearr_iochunk_sz : (gdimlen - (ios->io_rank * rearr_iochunk_sz));
      std::size_t rdata_sz = one_var_rdata_sz * nvars;
    
      rdata.resize(rdata_sz);
      std::fill(rdata.begin(), rdata.end(), PIO_FILL_DOUBLE);

      exp_data.resize(rdata_sz);
      for(int i = 0; i < nvars; i++){
        std::iota(exp_data.begin() + i * one_var_rdata_sz, exp_data.begin() + (i + 1) * one_var_rdata_sz, ios->io_rank * rearr_iochunk_sz);
      }
    }

    if(!is_odd_proc){
      compmap.resize(LOCAL_COMPMAP_SZ);
      sdata.resize(compmap.size() * nvars);

      int neven_procs_before_wrank = wrank - (wrank / 2);
      std::iota(compmap.begin(), compmap.begin() + FIRST_RANGE_SZ, neven_procs_before_wrank * FIRST_RANGE_SZ);
      std::iota(compmap.begin() + FIRST_RANGE_SZ, compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, neven_procs * FIRST_RANGE_SZ + neven_procs_before_wrank * SECOND_RANGE_SZ);
      std::iota(compmap.begin() + FIRST_RANGE_SZ + SECOND_RANGE_SZ, compmap.end(), neven_procs * (FIRST_RANGE_SZ + SECOND_RANGE_SZ) + neven_procs_before_wrank * THIRD_RANGE_SZ);

      for(int i = 0; i < nvars; i++){
        std::transform(compmap.cbegin(), compmap.cend(), sdata.begin() + i * LOCAL_COMPMAP_SZ,
          [](const PIO_Offset i){ return static_cast<double>(i); });
      }
    }

    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Initializing contig rearranger failed");
      return PIO_EINTERNAL;
    }
    ret = rearr.rearrange_comp2io(sdata.data(), compmap.size(),
            rdata.data(), rdata.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement\n");
      return PIO_EINTERNAL;
    }

    /* Rearrange data from I/O processes to compute processes */
    if(!is_odd_proc){
      exp_data.resize(LOCAL_COMPMAP_SZ * nvars);
    }
    else{
      exp_data.clear();
    }

    assert(exp_data.size() == sdata.size());
    std::copy(sdata.begin(), sdata.end(), exp_data.begin());
    std::fill(sdata.begin(), sdata.end(), PIO_FILL_DOUBLE);

    ret = rearr.rearrange_io2comp(rdata.data(), compmap.size(),
            sdata.data(), compmap.size() * sizeof(double), nvars);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Rearranging data using contig rearranger failed (I/O to compute procs)");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, sdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/invalid data received after data rearrangement (I/O to compute procs)\n");
      return PIO_EINTERNAL;
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Finalizing contig rearranger failed");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Rearranging data using contig rearranger failed");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR, mpierr = MPI_SUCCESS;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);

  std::vector<std::pair<std::string, std::function<int(MPI_Comm, int, int, int)> > > test_funcs = {
      {"test_c2i_block_data_rearr_nvars", test_c2i_block_data_rearr_nvars},
      {"test_block_data_rearr_nvars", test_block_data_rearr_nvars},
      {"test_rev_block_data_rearr_nvars", test_rev_block_data_rearr_nvars},
      {"test_mrange_block_data_rearr_nvars", test_mrange_block_data_rearr_nvars},
      {"test_mrange_oddz_data_rearr_nvars", test_mrange_oddz_data_rearr_nvars}
    };
  
  const int MAX_NVARS = 6;
  for(std::size_t tid = 0; tid < test_funcs.size(); tid++){
    for(int i = 1; i < MAX_NVARS; i++){
      try{
        ret = test_funcs[tid].second(comm, wrank, wsz, i);
      }
      catch(...){
        ret = PIO_EINTERNAL;
        nerrs++;
      }
      int lfail = (ret == PIO_NOERR) ? 0 : 1;
      mpierr = MPI_Reduce(&ret, &lfail, 1, MPI_INT, MPI_SUM, 0, comm);
      if(mpierr != MPI_SUCCESS){
        LOG_RANK0(wrank, "Test Driver failed: Unable to calculate total num errors\n");
      }
      if(ret != 0){
        std::string non_root_fail_msg = std::string(", failed on ") + std::to_string(ret) + std::string(" non-root processes");
        LOG_RANK0(wrank, "%s(nvars=%d) FAILED (ret = %d%s)\n", test_funcs[tid].first.c_str(), i, ret, (lfail) ? "" : non_root_fail_msg.c_str());
        nerrs++;
      }
      else{
        LOG_RANK0(wrank, "%s(nvars=%d) PASSED\n", test_funcs[tid].first.c_str(), i);
      }
    }
  }

  *num_errors += nerrs;
  return nerrs;
}

int main(int argc, char *argv[])
{
  int ret;
  int wrank, wsz;
  int num_errors;
#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLinitialize();
  if(ret != 0){
    LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  ret = MPI_Init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Init() FAILED, ret = %d\n", ret);
    return ret;
  }

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
    return ret;
  }
  ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
    return ret;
  }

  num_errors = 0;
  ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
  if(ret != 0){
    LOG_RANK0(wrank, "Test driver FAILED\n");
    return FAIL;
  }
  else{
    LOG_RANK0(wrank, "All tests PASSED\n");
  }

  MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLfinalize();
  if(ret != 0){
    LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  if(num_errors != 0){
    LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
    return FAIL;
  }
  return 0;
}
