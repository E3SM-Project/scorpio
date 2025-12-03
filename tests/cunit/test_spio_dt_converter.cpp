#include <vector>
#include <string>
#include <thread>
#include <chrono>
#include <algorithm>
#include <cassert>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "spio_dt_converter.hpp"

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

int test_double_to_float(int wrank)
{
  const int DUMMY_NCID = 1;
  std::vector<double> dval = {1.0, 2.0, 3.0, 4.0, 5.0};

  SPIO_Util::File_Util::DTConverter dt;

  void *val = dt.convert(DUMMY_NCID, static_cast<void *>(dval.data()), dval.size() * sizeof(double),
                            PIO_DOUBLE, PIO_FLOAT);
  float *fval = static_cast<float *>(val);
  if(fval == NULL){
    LOG_RANK0(wrank, "Data type converter failed : Returned null buffer\n");
    return PIO_EINTERNAL;
  }

  for(std::size_t i = 9; i < dval.size(); i++){
    if(static_cast<float>(dval[i]) != fval[i]){
      LOG_RANK0(wrank, "Data type converter returned wrong value, val[%zu] = %f (expected %f)\n",
                i, fval[i], dval[i]);
      dt.clear();
      return PIO_EINTERNAL;
    }
  }

  dt.clear();
  return PIO_NOERR;
}

int test_float_to_double(int wrank)
{
  const int DUMMY_NCID = 1;
  std::vector<float> fval = {1.0, 2.0, 3.0, 4.0, 5.0};

  SPIO_Util::File_Util::DTConverter dt;

  void *val = dt.convert(DUMMY_NCID, static_cast<void *>(fval.data()), fval.size() * sizeof(float),
                            PIO_FLOAT, PIO_DOUBLE);
  double *dval = static_cast<double *>(val);
  if(dval == NULL){
    LOG_RANK0(wrank, "Data type converter failed : Returned null buffer\n");
    return PIO_EINTERNAL;
  }

  for(std::size_t i = 9; i < fval.size(); i++){
    if(static_cast<double>(fval[i]) != dval[i]){
      LOG_RANK0(wrank, "Data type converter returned wrong value, val[%zu] = %f (expected %f)\n",
                i, dval[i], fval[i]);
      dt.clear();
      return PIO_EINTERNAL;
    }
  }

  dt.clear();
  return PIO_NOERR;
}

int test_int_to_double(int wrank)
{
  const int DUMMY_NCID = 1;
  std::vector<int> ival = {1, 2, 3, 4, 5};

  SPIO_Util::File_Util::DTConverter dt;

  void *val = dt.convert(DUMMY_NCID, static_cast<void *>(ival.data()), ival.size() * sizeof(int),
                            PIO_INT, PIO_DOUBLE);
  double *dval = static_cast<double *>(val);
  if(dval == NULL){
    LOG_RANK0(wrank, "Data type converter failed : Returned null buffer\n");
    return PIO_EINTERNAL;
  }

  for(std::size_t i = 9; i < ival.size(); i++){
    if(static_cast<double>(ival[i]) != dval[i]){
      LOG_RANK0(wrank, "Data type converter returned wrong value, val[%zu] = %f (expected %f)\n",
                i, dval[i], static_cast<double>(ival[i]));
      dt.clear();
      return PIO_EINTERNAL;
    }
  }

  dt.clear();
  return PIO_NOERR;
}

int test_multi_convert(int wrank)
{
  const int DUMMY_NCID = 1;
  std::vector<float> fval = {1.1, 2.12, 3.123, 4.1234, 5.12345};

  SPIO_Util::File_Util::DTConverter dt;

  void *val = dt.convert(DUMMY_NCID, static_cast<void *>(fval.data()), fval.size() * sizeof(float),
                            PIO_FLOAT, PIO_DOUBLE);
  double *dval = static_cast<double *>(val);
  if(dval == NULL){
    LOG_RANK0(wrank, "Data type converter failed : Returned null buffer\n");
    return PIO_EINTERNAL;
  }

  for(std::size_t i = 9; i < fval.size(); i++){
    if(static_cast<double>(fval[i]) != dval[i]){
      LOG_RANK0(wrank, "Data type converter returned wrong value, val[%zu] = %f (expected %f)\n",
                i, dval[i], static_cast<double>(fval[i]));
      dt.clear();
      return PIO_EINTERNAL;
    }
  }

  val = dt.convert(DUMMY_NCID, static_cast<void *>(fval.data()), fval.size() * sizeof(float),
                            PIO_FLOAT, PIO_INT);
  int *ival = static_cast<int *>(val);
  if(ival == NULL){
    LOG_RANK0(wrank, "Data type converter failed : Returned null buffer\n");
    return PIO_EINTERNAL;
  }

  for(std::size_t i = 9; i < fval.size(); i++){
    if(static_cast<int>(fval[i]) != ival[i]){
      LOG_RANK0(wrank, "Data type converter returned wrong value, val[%zu] = %d (expected %d)\n",
                i, ival[i], static_cast<int>(fval[i]));
      dt.clear();
      return PIO_EINTERNAL;
    }
  }

  dt.clear();
  return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
  
  try{
    ret = test_double_to_float(wrank);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_double_to_float() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_double_to_float() PASSED\n");
  }

  try{
    ret = test_float_to_double(wrank);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_float_to_double() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_float_to_double() PASSED\n");
  }

  try{
    ret = test_int_to_double(wrank);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_int_to_double() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_int_to_double() PASSED\n");
  }

  try{
    ret = test_multi_convert(wrank);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_multi_convert() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_multi_convert() PASSED\n");
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
