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
#include "spio_sort_utils.hpp"

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

int test_simple_kway_merge_sort(MPI_Comm comm, int wrank, int wsz)
{
  std::vector<double> data_range1 = {12, 28, 32, 36, 37, 68, 69, 70};
  std::vector<double> data_range2 = {2, 3, 4, 6, 9, 10, 13, 14, 30, 81};

  std::vector<double> data;
  data.insert(data.end(), data_range1.begin(), data_range1.end());
  data.insert(data.end(), data_range2.begin(), data_range2.end());

  std::vector<std::pair<std::size_t, std::size_t> > ranges =
    { {0, data_range1.size()}, {data_range1.size(), data_range1.size() + data_range2.size()} };

  std::vector<double> exp_data(data);
  std::sort(exp_data.begin(), exp_data.end());

  try{
    SPIO_Util::vec_kway_merge_sort(data, ranges);

    if(!cmp_result(wrank, data, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in sorted buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Kway merge sort of Vector failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

int test_odd_even_kway_merge_sort(MPI_Comm comm, int wrank, int wsz)
{
  const std::size_t NELEMS = 32;
  std::vector<double> data_range1;
  std::vector<double> data_range2;

  for(std::size_t i = 0; i < NELEMS; i+=2){
    data_range1.push_back(i);
    data_range2.push_back(i + 1);
  }

  std::vector<double> data;
  data.insert(data.end(), data_range1.begin(), data_range1.end());
  data.insert(data.end(), data_range2.begin(), data_range2.end());

  std::vector<std::pair<std::size_t, std::size_t> > ranges =
    { {0, data_range1.size()}, {data_range1.size(), data_range1.size() + data_range2.size()} };

  std::vector<double> exp_data(data);
  std::sort(exp_data.begin(), exp_data.end());

  try{
    SPIO_Util::vec_kway_merge_sort(data, ranges);

    if(!cmp_result(wrank, data, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in sorted buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Kway merge sort of Vector failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

int test_rev_kway_merge_sort(MPI_Comm comm, int wrank, int wsz)
{
  std::vector<double> data_range1 = {11, 12, 13, 14};
  std::vector<double> data_range2 = {5, 6, 7, 8, 9};
  std::vector<double> data_range3 = {2, 4};

  std::vector<double> data;
  data.insert(data.end(), data_range1.begin(), data_range1.end());
  data.insert(data.end(), data_range2.begin(), data_range2.end());
  data.insert(data.end(), data_range3.begin(), data_range3.end());

  std::vector<std::pair<std::size_t, std::size_t> > ranges =
    { {0, data_range1.size()},
      {data_range1.size(), data_range1.size() + data_range2.size()},
      {data_range1.size() + data_range2.size(),
        data_range1.size() + data_range2.size() + data_range3.size()}
    };

  std::vector<double> exp_data(data);
  std::sort(exp_data.begin(), exp_data.end());

  try{
    SPIO_Util::vec_kway_merge_sort(data, ranges);

    if(!cmp_result(wrank, data, exp_data)){
      LOG_RANK0(wrank, "ERROR: Unexpected/Invalid data in sorted buffer\n");
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    LOG_RANK0(wrank, "Kway merge sort of Vector failed\n");
    return PIO_EINTERNAL;
  }
    
  return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR, mpierr = MPI_SUCCESS;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
  
  std::vector<std::pair<std::string, std::function<int(MPI_Comm, int, int)> > > test_funcs = {
      {"test_simple_kway_merge_sort", test_simple_kway_merge_sort},
      {"test_odd_even_kway_merge_sort", test_odd_even_kway_merge_sort},
      {"test_rev_kway_merge_sort", test_rev_kway_merge_sort}
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
