#include <iostream>
#include <vector>
#include <string>
#include <cassert>
#include <cstring>
#include <numeric>
#include <algorithm>
#include <ostream>
#include <sstream>
#include <iterator>
#include <functional>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "pio_rearr_utils.hpp"

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

/* Test alltoall() with each process sending the same sized block of data */
int test_alltoall_eq_sized_type_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const int LOCAL_SZ = 4;
  const double FILLVAL = -1.0;
  std::vector<double> sdata(LOCAL_SZ * wsz);
  std::vector<double> rdata(LOCAL_SZ * wsz, FILLVAL);
  std::vector<double> exp_data(LOCAL_SZ * wsz, FILLVAL);
  rearr_comm_fc_opt_t ropts = {true, true, -1};

  const double ldata_start = wrank * LOCAL_SZ;
  double ldata = ldata_start;
  int ldata_idx = 0;
  std::generate(sdata.begin(), sdata.end(),
    [&ldata, &ldata_idx, ldata_start] () mutable {
      if(ldata_idx % LOCAL_SZ == 0){
        ldata = ldata_start;
      }
      ldata_idx++;
      return ldata++;
    }
  );

  std::iota(exp_data.begin(), exp_data.end(), 0);

  try{
    ret = SPIO_Util::Rearr_Util::alltoall(sdata.data(), LOCAL_SZ, MPI_DOUBLE,
            rdata.data(), LOCAL_SZ, MPI_DOUBLE, comm, &ropts);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Alltoall failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data received in alltoall()\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Alltoall failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

/* Test alltoall() with each process sending a contig block of data */
int test_alltoall_contig_to_type_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const int LOCAL_SZ = 4;
  const double FILLVAL = -1.0;
  std::vector<double> sdata(LOCAL_SZ * wsz);
  std::vector<double> rdata(LOCAL_SZ * wsz, FILLVAL);
  std::vector<double> exp_data(LOCAL_SZ * wsz, FILLVAL);

  MPI_Datatype stype = MPI_DATATYPE_NULL;
  rearr_comm_fc_opt_t ropts = {true, true, -1};

  const double ldata_start = wrank * LOCAL_SZ;
  double ldata = ldata_start;
  int ldata_idx = 0;
  std::generate(sdata.begin(), sdata.end(),
    [&ldata, &ldata_idx, ldata_start] () mutable {
      if(ldata_idx % LOCAL_SZ == 0){
        ldata = ldata_start;
      }
      ldata_idx++;
      return ldata++;
    }
  );

  ret = MPI_Type_contiguous(LOCAL_SZ, MPI_DOUBLE, &stype);
  if(ret == MPI_SUCCESS){
    ret = MPI_Type_commit(&stype);
  }
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "Creating contig MPI type to send data failed\n");
    return PIO_EINTERNAL;
  }

  std::iota(exp_data.begin(), exp_data.end(), 0);

  try{
    ret = SPIO_Util::Rearr_Util::alltoall(sdata.data(), 1, stype,
            rdata.data(), LOCAL_SZ, MPI_DOUBLE, comm, &ropts);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Alltoall failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data received in alltoall()\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Alltoall failed\n");
    return PIO_EINTERNAL;
  }

  MPI_Type_free(&stype);
    
  return PIO_NOERR;
}

/* Test alltoall() with each process receives a contig block of data */
int test_alltoall_type_to_contig_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const int LOCAL_SZ = 4;
  const double FILLVAL = -1.0;
  std::vector<double> sdata(LOCAL_SZ * wsz);
  std::vector<double> rdata(LOCAL_SZ * wsz, FILLVAL);
  std::vector<double> exp_data(LOCAL_SZ * wsz, FILLVAL);

  MPI_Datatype rtype = MPI_DATATYPE_NULL;
  rearr_comm_fc_opt_t ropts = {true, true, -1};

  const double ldata_start = wrank * LOCAL_SZ;
  double ldata = ldata_start;
  int ldata_idx = 0;
  std::generate(sdata.begin(), sdata.end(),
    [&ldata, &ldata_idx, ldata_start] () mutable {
      if(ldata_idx % LOCAL_SZ == 0){
        ldata = ldata_start;
      }
      ldata_idx++;
      return ldata++;
    }
  );

  ret = MPI_Type_contiguous(LOCAL_SZ, MPI_DOUBLE, &rtype);
  if(ret == MPI_SUCCESS){
    ret = MPI_Type_commit(&rtype);
  }
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "Creating contig MPI type to send data failed\n");
    return PIO_EINTERNAL;
  }

  std::iota(exp_data.begin(), exp_data.end(), 0);

  try{
    ret = SPIO_Util::Rearr_Util::alltoall(sdata.data(), LOCAL_SZ, MPI_DOUBLE,
            rdata.data(), 1, rtype, comm, &ropts);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Alltoall failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data received in alltoall()\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Alltoall failed\n");
    return PIO_EINTERNAL;
  }

  MPI_Type_free(&rtype);
    
  return PIO_NOERR;
}

/* Test alltoallw() with each process sending the same sized block of data */
int test_alltoallw_eq_sized_type_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const int LOCAL_SZ = 4;
  const double FILLVAL = -1.0;
  std::vector<double> sdata(LOCAL_SZ * wsz);
  std::vector<double> rdata(LOCAL_SZ * wsz, FILLVAL);
  std::vector<double> exp_data(LOCAL_SZ * wsz, FILLVAL);
  rearr_comm_fc_opt_t ropts = {true, true, -1};

  std::vector<int> srcount(wsz, LOCAL_SZ);
  std::vector<int> srdispls(wsz, 0);
  std::vector<MPI_Datatype> srtypes(wsz, MPI_DOUBLE);
  int type_sz = 0;

  ret = MPI_Type_size(MPI_DOUBLE, &type_sz);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "ERROR: Unable to query size of MPI_DOUBLE\n");
    return PIO_EINTERNAL;
  }

  const double ldata_start = wrank * LOCAL_SZ;
  double ldata = ldata_start;
  int ldata_idx = 0;
  std::generate(sdata.begin(), sdata.end(),
    [&ldata, &ldata_idx, ldata_start] () mutable {
      if(ldata_idx % LOCAL_SZ == 0){
        ldata = ldata_start;
      }
      ldata_idx++;
      return ldata++;
    }
  );

  int displ = -LOCAL_SZ;
  std::generate(srdispls.begin(), srdispls.end(),
    [&displ, type_sz] () mutable {
      displ += LOCAL_SZ;
      return displ * type_sz;  
    }
  );

  std::iota(exp_data.begin(), exp_data.end(), 0);

  try{
    ret = SPIO_Util::Rearr_Util::alltoallw(
            sdata.data(), srcount.data(), srdispls.data(), srtypes.data(),
            rdata.data(), srcount.data(), srdispls.data(), srtypes.data(),
            comm, &ropts);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Alltoallw failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data received in alltoallw()\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Alltoallw failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

/* Test alltoallw() with each process sending uneven sized block of data */
int test_alltoallw_uneven_block_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const double FILLVAL = -1.0;
  const int LOCAL_SZ = wrank + 1;
  std::vector<double> sdata(LOCAL_SZ);
  std::vector<double> rdata((wsz * (wsz + 1))/2, FILLVAL);
  std::vector<double> exp_data((wsz * (wsz + 1))/2, FILLVAL);

  std::vector<int> scounts(wsz), sdispls(wsz), rcounts(wsz), rdispls(wsz);
  std::vector<MPI_Datatype> types(wsz, MPI_DOUBLE);
  int type_sz = 0;
  rearr_comm_fc_opt_t ropts = {true, true, -1};

  ret = MPI_Type_size(MPI_DOUBLE, &type_sz);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "ERROR: Unable to query size of MPI_DOUBLE\n");
    return PIO_EINTERNAL;
  }

  /* sdata[] in proc i contains i * wsz consecutive numbers,
   * e.g.: wsz = 3
   * sdata[] at rank 0 is {0} => rank 0 sends {0} to all procs
   * sdata[] at rank 1 is {1,2} => rank 1 sends {1,2} to all procs
   * sdata[] at rank 2 is {3,4,5} => rank 2 sends {3,4,5} to all procs
   * Number of elements in all procs <= wrank, = (wrank + 1) * (wrank + 2)/2
   * Number of elements in proc wrank = (wrank + 1)
   * => Proc wrank starts at index (wrank * (wrank + 1)) / 2)
   * sdata[] at rank wrank is {(wrank * (wrank + 1)) / 2),
   *                                (wrank * (wrank + 1)) / 2) + 1, ...,
   *                                (wrank * (wrank + 1)) / 2) + wrank - 1}
   */
  std::iota(sdata.begin(), sdata.end(), (wrank * (wrank + 1)) / 2);
  std::iota(exp_data.begin(), exp_data.end(), 0);

  for(int i = 0; i < wsz; i++){
    scounts[i] = wrank + 1;
    sdispls[i] = 0;

    rcounts[i] = i + 1;
    rdispls[i] = (i * (i + 1))/2 * type_sz;
  }

  try{
    ret = SPIO_Util::Rearr_Util::alltoallw(sdata.data(), scounts.data(), sdispls.data(), types.data(),
            rdata.data(), rcounts.data(), rdispls.data(), types.data(), comm, &ropts);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Alltoallw failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rdata, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in alltoallw buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Alltoallw failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR, mpierr = MPI_SUCCESS;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
  
  std::vector<std::pair<std::string, std::function<int(MPI_Comm, int, int)> > > test_funcs = {
      {"test_alltoall_eq_sized_type_decomp", test_alltoall_eq_sized_type_decomp},
      {"test_alltoall_contig_to_type_decomp", test_alltoall_contig_to_type_decomp},
      {"test_alltoall_type_to_contig_decomp", test_alltoall_type_to_contig_decomp},
      {"test_alltoallw_eq_sized_type_decomp", test_alltoallw_eq_sized_type_decomp},
      {"test_alltoallw_uneven_block_decomp", test_alltoallw_uneven_block_decomp}
    };
  
  for(std::size_t tid = 0; tid < test_funcs.size(); tid++){
    try{
      ret = test_funcs[tid].second(comm, wrank, wsz);
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
      LOG_RANK0(wrank, "%s() FAILED (ret = %d%s)\n", test_funcs[tid].first.c_str(), ret, (lfail) ? "" : non_root_fail_msg.c_str());
      nerrs++;
    }
    else{
      LOG_RANK0(wrank, "%s() PASSED\n", test_funcs[tid].first.c_str());
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
