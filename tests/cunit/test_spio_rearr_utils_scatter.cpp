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

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
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
      if(wrank == 0){
        std::cerr << "ERROR: Invalid/Unexpected value, array[ " << i << "] = " << res[i]
                  << " (Expected array[" << i << "] = " << exp[i] << ")\n";
      }
      return false;
    }
  }

  return true;
}

/* Test scatterw() with each process receiving the same sized block of data */
int test_scatterw_double_block_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const int LOCAL_SZ = 4;
  const double FILLVAL = -1.0;
  const int ROOT_RANK = 0;
  std::vector<double> local_data(LOCAL_SZ);
  std::vector<double> scatter_data;
  std::vector<double> exp_data(LOCAL_SZ);

  int recvcount = LOCAL_SZ;
  MPI_Datatype recvtype = MPI_DOUBLE;

  std::vector<int> sendcounts, sdispls;
  std::vector<MPI_Datatype> sendtypes;

  std::fill(local_data.begin(), local_data.end(), FILLVAL);
  std::iota(exp_data.begin(), exp_data.end(), wrank * LOCAL_SZ);

  if(wrank == ROOT_RANK){
    scatter_data.resize(wsz * LOCAL_SZ);
    std::iota(scatter_data.begin(), scatter_data.end(), 0);

    sendcounts.resize(wsz);
    sdispls.resize(wsz);
    sendtypes.resize(wsz);

    for(int i = 0; i < wsz; i++){
      sendcounts[i] = LOCAL_SZ;
      sdispls[i] = i * LOCAL_SZ * sizeof(double);
      sendtypes[i] = MPI_DOUBLE;
    }
  }

  try{
    ret = SPIO_Util::Rearr_Util::scatterw(scatter_data.data(), sendcounts,
            sdispls, sendtypes,
            local_data.data(), recvcount, recvtype,
            ROOT_RANK, comm, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Scattering data failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, local_data, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in scatter buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Scattering data failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

/* Test scatterw() with each process sending uneven sized block of data */
int test_scatterw_double_uneven_block_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const double FILLVAL = -1.0;
  const int ROOT_RANK = 0;
  const int LOCAL_SZ = wrank;
  std::vector<double> local_data(LOCAL_SZ);
  std::vector<double> scatter_data;
  std::vector<double> exp_data(LOCAL_SZ);

  int recvcount = LOCAL_SZ;
  MPI_Datatype recvtype = MPI_DOUBLE;

  std::vector<int> sendcounts, sdispls;
  std::vector<MPI_Datatype> sendtypes;

  /* local_data[] in proc i contains i consecutive numbers,
   * e.g.: wsz = 5, after scatter,
   * local_data[] at rank 0 is empty
   * local_data[] at rank 1 is {0} 
   * local_data[] at rank 2 is {1,2} 
   * local_data[] at rank 3 is {3,4,5} 
   * local_data[] at rank 4 is {6,7,8,9} 
   * local_data[] at rank 5 is {10,11,12,13,14} 
   * Number of elements in all procs <= wrank, = wrank * (wrank + 1)/2
   * Number of elements in proc wrank = wrank
   * => Proc wrank starts at index (wrank * (wrank + 1)) / 2 - wrank)
   * local_data[] at rank wrank is {(wrank * (wrank + 1)) / 2 - wrank),
   *                                (wrank * (wrank + 1)) / 2 - wrank) + 1, ...,
   *                                (wrank * (wrank + 1)) / 2 - wrank) + wrank - 1}
   */
  std::fill(local_data.begin(), local_data.end(), FILLVAL);
  std::iota(exp_data.begin(), exp_data.end(), (wrank * (wrank + 1)) / 2 - wrank);

  if(wrank == ROOT_RANK){
    scatter_data.resize((wsz * (wsz - 1))/2);
    std::iota(scatter_data.begin(), scatter_data.end(), 0);

    sendcounts.resize(wsz);
    sdispls.resize(wsz);
    sendtypes.resize(wsz);

    for(int i = 0; i < wsz; i++){
      sendcounts[i] = i;
      /* Number of elements in all procs <= i, = i * (i + 1)/2
       * Number of elements in proc i = i
       * => Proc i starts at index (i * (i + 1)) / 2 - i)
       */
      sdispls[i] = ((i * (i + 1)) / 2 - i) * sizeof(double);
      sendtypes[i] = MPI_DOUBLE;
    }
  }

  try{
    ret = SPIO_Util::Rearr_Util::scatterw(scatter_data.data(), sendcounts,
            sdispls, sendtypes,
            local_data.data(), recvcount, recvtype,
            ROOT_RANK, comm, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Scattering data failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, local_data, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in scatter buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Scattering data failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

/* Test scatterw() with each process receiving the same sized block of data using contig type */
int test_scatterw_contig_block_decomp(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const int LOCAL_SZ = 4;
  const double FILLVAL = -1.0;
  const int ROOT_RANK = 0;
  std::vector<double> local_data(LOCAL_SZ);
  std::vector<double> scatter_data;
  std::vector<double> exp_data(LOCAL_SZ);

  int recvcount = 1;
  MPI_Datatype srtype = MPI_DATATYPE_NULL;

  std::vector<int> sendcounts, sdispls;
  std::vector<MPI_Datatype> sendtypes;

  std::fill(local_data.begin(), local_data.end(), FILLVAL);
  std::iota(exp_data.begin(), exp_data.end(), wrank * LOCAL_SZ);

  ret = MPI_Type_contiguous(LOCAL_SZ, MPI_DOUBLE, &srtype);
  if(ret == MPI_SUCCESS){
    ret = MPI_Type_commit(&srtype);
  }
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "ERROR: Unable to create MPI contig type to receive double data (sz = %d)\n", LOCAL_SZ);
    return PIO_EINTERNAL;
  }

  if(wrank == ROOT_RANK){
    scatter_data.resize(wsz * LOCAL_SZ);
    std::iota(scatter_data.begin(), scatter_data.end(), 0);

    sendcounts.resize(wsz);
    sdispls.resize(wsz);
    sendtypes.resize(wsz);

    for(int i = 0; i < wsz; i++){
      sendcounts[i] = 1;
      sdispls[i] = i * LOCAL_SZ * sizeof(double);
      sendtypes[i] = srtype;
    }
  }

  try{
    ret = SPIO_Util::Rearr_Util::scatterw(scatter_data.data(), sendcounts,
            sdispls, sendtypes,
            local_data.data(), recvcount, srtype,
            ROOT_RANK, comm, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Scattering data failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, local_data, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in scatter buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Scattering data failed\n");
    return PIO_EINTERNAL;
  }

  MPI_Type_free(&srtype);
    
  return PIO_NOERR;
}

/* Test scatterw() with each process sending contig block of data and scattered using an indexed type */
int test_scatterw_contig_indexed(MPI_Comm comm, int wrank, int wsz)
{
  int ret = PIO_NOERR;
  const int LOCAL_SZ = 4;
  const double FILLVAL = -1.0;
  const int ROOT_RANK = 0;
  std::vector<double> local_data(LOCAL_SZ);
  std::vector<double> scatter_data;
  std::vector<double> exp_data(LOCAL_SZ);

  int recvcount = 1;
  MPI_Datatype recvtype = MPI_DATATYPE_NULL;

  std::vector<int> sendcounts, sdispls;
  MPI_Datatype sendtype = MPI_DATATYPE_NULL;
  std::vector<MPI_Datatype> sendtypes;

  /* Each process contains LOCAL_SZ doubles. After scatter,
   * rank0 contains {0, 0+wsz, 0+2*wsz,...}
   * rank1 contains {1, 1+wsz, 1+2*wsz,...}
   * ...
   * ranki contains {i, i+wsz, i+2*wsz,...}
   */
  std::fill(local_data.begin(), local_data.end(), FILLVAL);
  int i = 0;
  std::generate(exp_data.begin(), exp_data.end(),
    [wrank, wsz, &i](){ return static_cast<double>(i++ * wsz + wrank); } );

  ret = MPI_Type_contiguous(LOCAL_SZ, MPI_DOUBLE, &recvtype);
  if(ret == MPI_SUCCESS){
    ret = MPI_Type_commit(&recvtype);
  }
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "ERROR: Unable to create MPI contig type to recv double data (sz = %d)\n", LOCAL_SZ);
    return PIO_EINTERNAL;
  }

  if(wrank == ROOT_RANK){
    scatter_data.resize(wsz * LOCAL_SZ);
    std::iota(scatter_data.begin(), scatter_data.end(), 0);

    sendcounts.resize(wsz);
    sdispls.resize(wsz);
    sendtypes.resize(wsz);

    /* Receive the data such that the elements are in a sorted order - ascending */
    /* Receive data in rank0 {0, 0+wsz, 0+2*wz,...} at indices 0, 0+wsz, 0+2*wsz,... */
    std::vector<int> elem_counts(LOCAL_SZ, 1);
    std::vector<int> elem_rdispls(LOCAL_SZ);

    int j = 0;
    std::generate(elem_rdispls.begin(), elem_rdispls.end(),
      [&j, wsz](){ return static_cast<double>(j++ * wsz); } );

    ret = MPI_Type_indexed(LOCAL_SZ, elem_counts.data(), elem_rdispls.data(),
            MPI_DOUBLE, &(sendtype));
    if(ret == MPI_SUCCESS){
      ret = MPI_Type_commit(&(sendtype));
    }
    if(ret != MPI_SUCCESS){
      LOG_RANK0(wrank, "ERROR: Unable to create MPI indexed type to send doubles\n");
      return PIO_EINTERNAL;
    }

    for(int i = 0; i < wsz; i++){
      sendcounts[i] = 1;
      sdispls[i] = i * sizeof(double);
      sendtypes[i] = sendtype;
    }
  }

  try{
    ret = SPIO_Util::Rearr_Util::scatterw(scatter_data.data(), sendcounts,
            sdispls, sendtypes,
            local_data.data(), recvcount, recvtype,
            ROOT_RANK, comm, NULL);
    if(ret != PIO_NOERR){
      LOG_RANK0(wrank, "Scattering data failed\n");
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, local_data, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in scatter buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Scattering data failed\n");
    return PIO_EINTERNAL;
  }

  MPI_Type_free(&recvtype);
  if(sendtype != MPI_DATATYPE_NULL){
    MPI_Type_free(&(sendtype));
  }
    
  return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR, mpierr = MPI_SUCCESS;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
  
  std::vector<std::pair<std::string, std::function<int(MPI_Comm, int, int)> > > test_funcs = {
      {"test_scatterw_double_block_decomp", test_scatterw_double_block_decomp},
      {"test_scatterw_double_uneven_block_decomp", test_scatterw_double_uneven_block_decomp},
      {"test_scatterw_contig_block_decomp", test_scatterw_contig_block_decomp},
      {"test_scatterw_contig_indexed", test_scatterw_contig_indexed}
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
