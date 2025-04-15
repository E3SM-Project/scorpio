#include <iostream>
#include <vector>
#include <string>
#include <cassert>
#include <cstring>
#include <numeric>
#include <algorithm>
#include <sstream>
#include <functional>
#include <stdexcept>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "spio_decomp_logger.hpp"

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
    LOG_RANK0(wrank, "Unable to allocate memory for I/O system\n");
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
    LOG_RANK0(wrank, "Unable to split comm for creating I/O system\n");
    free(ios);
    return NULL;
  }

  ios->num_iotasks = nio_procs;
  ios->ioproc = (color == 0) ? true : false;
  if(ios->ioproc){
    ret = MPI_Comm_rank(ios->io_comm, &(ios->io_rank));
    if(ret != MPI_SUCCESS){
      LOG_RANK0(wrank, "Unable to get rank of I/O process\n");
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

io_desc_t *get_iodesc(int wrank, iosystem_desc_t *ios, const std::vector<PIO_Offset> &compmap, const std::vector<int> &gdimlen)
{
  io_desc_t *iodesc = NULL;
  int ret = PIO_NOERR;

  ret = malloc_iodesc(ios, PIO_DOUBLE, static_cast<int>(gdimlen.size()), static_cast<int>(compmap.size()), &iodesc);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "Unable to alloc mem for I/O desc\n");
    return iodesc;
  }

  assert(iodesc->dimlen);
  std::copy(gdimlen.cbegin(), gdimlen.cend(), iodesc->dimlen);

  assert(iodesc->map);
  std::copy(compmap.cbegin(), compmap.cend(), iodesc->map);

  return iodesc;
}

void free_iodesc(io_desc_t *iodesc)
{
  if(!iodesc){
    return;
  }

  free(iodesc->firstregion->start);
  free(iodesc->firstregion->count);
  free(iodesc->firstregion);
  free(iodesc->map);
  free(iodesc->dimlen);
  free(iodesc);
}

int test_create_decomp_logger(MPI_Comm comm, int wrank, int wsz)
{
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system\n");
    return PIO_EINTERNAL;
  }
    
  try{
    std::string log_fname("piodecomplogger_test_01.nc");
    SPIO_Util::Decomp_Util::Decomp_logger *logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger failed\n");
      return PIO_EINTERNAL;
    }

    delete logger;
  }
  catch(...){
    LOG_RANK0(wrank, "Creating decomp logger failed\n");
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_simple_decomp_logger(MPI_Comm comm, int wrank, int wsz)
{
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system\n");
    return PIO_EINTERNAL;
  }
    
  try{
    std::string log_fname("piodecomplogger_test_simple_01.nc");
    SPIO_Util::Decomp_Util::Decomp_logger *logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    const int LOCAL_COMPMAP_SZ = 4;
    std::vector<int> gdimlen = {LOCAL_COMPMAP_SZ * wsz};
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);

    io_desc_t *iodesc = get_iodesc(wrank, ios, compmap, gdimlen);
    if(iodesc == NULL){
      LOG_RANK0(wrank, "Create I/O descriptor failed\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    (*logger).write_only().open().put(iodesc).close();
    free_iodesc(iodesc);
    delete logger;
  }
  catch(...){
    LOG_RANK0(wrank, "Creating decomp logger failed\n");
    free_iosystem(ios);
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_simple_decomp_logger_wr_cached_rd(MPI_Comm comm, int wrank, int wsz)
{
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system\n");
    return PIO_EINTERNAL;
  }
    
  try{
    std::string log_fname("piodecomplogger_test_simple_01.nc");
    SPIO_Util::Decomp_Util::Decomp_logger *logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    const int LOCAL_COMPMAP_SZ = 4;
    std::vector<int> gdimlen = {LOCAL_COMPMAP_SZ * wsz};
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);

    io_desc_t *iodesc = get_iodesc(wrank, ios, compmap, gdimlen);
    if(iodesc == NULL){
      LOG_RANK0(wrank, "Create I/O descriptor failed\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    (*logger).write_only().open().put(iodesc).close();
    free_iodesc(iodesc);

    std::string version;
    int rd_nprocs = -1;
    std::vector<int> rd_gdims;
    std::vector<PIO_Offset> rd_compmap;

    (*logger).get(version, rd_nprocs, rd_gdims, rd_compmap);
    if(rd_nprocs != wsz){
      LOG_RANK0(wrank, "Read invalid nprocs (%d) from log file, expected = %d\n", rd_nprocs, wsz);
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_gdims, gdimlen)){
      LOG_RANK0(wrank, "Read invalid gdims from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_compmap, compmap)){
      LOG_RANK0(wrank, "Read invalid map (local compmap) from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    delete logger;
  }
  catch(...){
    LOG_RANK0(wrank, "Creating decomp logger failed\n");
    free_iosystem(ios);
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_simple_decomp_logger_wr_rd(MPI_Comm comm, int wrank, int wsz)
{
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system\n");
    return PIO_EINTERNAL;
  }
    
  try{
    std::string log_fname("piodecomplogger_test_simple_01.nc");
    SPIO_Util::Decomp_Util::Decomp_logger *logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    const int LOCAL_COMPMAP_SZ = 4;
    std::vector<int> gdimlen = {LOCAL_COMPMAP_SZ * wsz};
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);

    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);

    io_desc_t *iodesc = get_iodesc(wrank, ios, compmap, gdimlen);
    if(iodesc == NULL){
      LOG_RANK0(wrank, "Create I/O descriptor failed\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    (*logger).write_only().open().put(iodesc).close();
    free_iodesc(iodesc);
    delete logger;

    logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger (for read) failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }
    std::string version;
    int rd_nprocs = -1;
    std::vector<int> rd_gdims;
    std::vector<PIO_Offset> rd_compmap;

    (*logger).read_only().open().get(version, rd_nprocs, rd_gdims, rd_compmap).close();
    if(rd_nprocs != wsz){
      LOG_RANK0(wrank, "Read invalid nprocs (%d) from log file, expected = %d\n", rd_nprocs, wsz);
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_gdims, gdimlen)){
      LOG_RANK0(wrank, "Read invalid gdims from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_compmap, compmap)){
      LOG_RANK0(wrank, "Read invalid map (local compmap) from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    delete logger;
  }
  catch(std::runtime_error &e){
    LOG_RANK0(wrank, "Creating decomp logger failedi (%s)\n", e.what());
    free_iosystem(ios);
    return PIO_EINTERNAL;
  }
  catch(...){
    LOG_RANK0(wrank, "Creating decomp logger failed\n");
    free_iosystem(ios);
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_vlen_decomp_logger(MPI_Comm comm, int wrank, int wsz)
{
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system\n");
    return PIO_EINTERNAL;
  }
    
  try{
    std::string log_fname("piodecomplogger_test_vlen_01.nc");
    SPIO_Util::Decomp_Util::Decomp_logger *logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    const int LOCAL_COMPMAP_SZ = wrank + 1;
    std::vector<int> gdimlen = {(wsz * (wsz + 1))/2};
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);

    std::iota(compmap.begin(), compmap.end(), (wrank * (wrank + 1))/2);

    io_desc_t *iodesc = get_iodesc(wrank, ios, compmap, gdimlen);
    if(iodesc == NULL){
      LOG_RANK0(wrank, "Create I/O descriptor failed\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    (*logger).write_only().open().put(iodesc).close();
    free_iodesc(iodesc);

    delete logger;

    logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger (for read) failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }
    std::string version;
    int rd_nprocs = -1;
    std::vector<int> rd_gdims;
    std::vector<PIO_Offset> rd_compmap;

    (*logger).read_only().open().get(version, rd_nprocs, rd_gdims, rd_compmap).close();
    if(rd_nprocs != wsz){
      LOG_RANK0(wrank, "Read invalid nprocs (%d) from log file, expected = %d\n", rd_nprocs, wsz);
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_gdims, gdimlen)){
      LOG_RANK0(wrank, "Read invalid gdims from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_compmap, compmap)){
      LOG_RANK0(wrank, "Read invalid map (local compmap) from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    delete logger;
  }
  catch(...){
    LOG_RANK0(wrank, "Creating decomp logger failed\n");
    free_iosystem(ios);
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_rvlen_decomp_logger(MPI_Comm comm, int wrank, int wsz)
{
  int nio_procs = (wsz/2 > 0) ? wsz/2 : wsz;
  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz, nio_procs);
  if(!ios){
    LOG_RANK0(wrank, "Unable to get I/O system\n");
    return PIO_EINTERNAL;
  }
    
  try{
    std::string log_fname("piodecomplogger_test_rvlen_01.nc");
    SPIO_Util::Decomp_Util::Decomp_logger *logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    const int LOCAL_COMPMAP_SZ = (wsz - 1 - wrank) + 1;
    std::vector<int> gdimlen = {(wsz * (wsz + 1))/2};
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);

    std::iota(compmap.rbegin(), compmap.rend(), ((wsz - 1 - wrank) * (wsz - wrank))/2);

    io_desc_t *iodesc = get_iodesc(wrank, ios, compmap, gdimlen);
    if(iodesc == NULL){
      LOG_RANK0(wrank, "Create I/O descriptor failed\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    (*logger).write_only().open().put(iodesc).close();
    free_iodesc(iodesc);

    delete logger;

    logger =
      SPIO_Util::Decomp_Util::create_decomp_logger(ios->comp_comm, log_fname);
    if(logger == NULL){ 
      LOG_RANK0(wrank, "Creating decomp logger (for read) failed\n");
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }
    std::string version;
    int rd_nprocs = -1;
    std::vector<int> rd_gdims;
    std::vector<PIO_Offset> rd_compmap;

    (*logger).read_only().open().get(version, rd_nprocs, rd_gdims, rd_compmap).close();
    if(rd_nprocs != wsz){
      LOG_RANK0(wrank, "Read invalid nprocs (%d) from log file, expected = %d\n", rd_nprocs, wsz);
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_gdims, gdimlen)){
      LOG_RANK0(wrank, "Read invalid gdims from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    if(!cmp_result(wrank, rd_compmap, compmap)){
      LOG_RANK0(wrank, "Read invalid map (local compmap) from log file\n");
      delete logger;
      free_iosystem(ios);
      return PIO_EINTERNAL;
    }

    delete logger;
  }
  catch(...){
    LOG_RANK0(wrank, "Creating decomp logger failed\n");
    free_iosystem(ios);
    return PIO_EINTERNAL;
  }

  free_iosystem(ios);
  return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR, mpierr = MPI_SUCCESS;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
  
  std::vector<std::pair<std::string, std::function<int(MPI_Comm, int, int)> > > test_funcs = {
      {"test_create_decomp_logger", test_create_decomp_logger},
      {"test_simple_decomp_logger", test_simple_decomp_logger},
      {"test_simple_decomp_logger_wr_cached_rd", test_simple_decomp_logger_wr_cached_rd},
      {"test_simple_decomp_logger_wr_rd", test_simple_decomp_logger_wr_rd},
      {"test_vlen_decomp_logger", test_vlen_decomp_logger},
      {"test_rvlen_decomp_logger", test_rvlen_decomp_logger}
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
    mpierr = MPI_Reduce(&lfail, &ret, 1, MPI_INT, MPI_SUM, 0, comm);
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
