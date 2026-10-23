#include "pio_config.h"

#ifdef _HDF5

#include <vector>
#include <cassert>
#include <cstdlib>

#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"
#include "spio_iosys_utils.hpp"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

/* Unlike LOG_RANK0 in other cunit tests, this logs on every rank: the whole
 * point of these tests is to compare behavior *across* ranks (a sync-only
 * rank vs an async-only rank), so a rank-1-only failure must not be
 * silenced.
 */
#define LOG_ERR(rank, ...)                          \
            do{                                      \
                fprintf(stderr, "[rank %d] ", rank);  \
                fprintf(stderr, __VA_ARGS__);         \
            }while(0);

static const int FAIL = -1;

/* Minimal mock iosystem_desc_t wrapping an already-constructed IO_comm_info.
 * Ownership of io_comm_info passes to the returned iosystem_desc_t (freed by
 * free_iosystem()).
 */
iosystem_desc_t *get_iosystem(SPIO_Util::Iosys_Util::IO_comm_info *io_comm_info)
{
  iosystem_desc_t *ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t));
  if(ios){
    ios->io_comm_info = io_comm_info;
    ios->intercomm = MPI_COMM_NULL;
  }
  return ios;
}

void free_iosystem(iosystem_desc_t *ios)
{
  if(ios){
    delete ios->io_comm_info;
    free(ios);
  }
}

file_desc_t *get_file(bool uses_async_io)
{
  file_desc_t *file = (file_desc_t *) calloc(1, sizeof(file_desc_t));
  if(file){
    file->uses_async_io = uses_async_io;
  }
  return file;
}

void free_file(file_desc_t *file)
{
  if(file){ free(file); }
}

/* Case (a): uses_async_io == false must return is_sync_io_proc()'s value,
 * ignoring is_async_io_proc() entirely. Exercised on a rank where the two
 * disagree (the disjoint sync-only-vs-async-only I/O comm info passed in by
 * the caller), so a buggy implementation that reads the wrong flag, or
 * always returns is_async_io_proc(), is caught.
 */
int test_uses_async_io_false_ignores_async_flag(int wrank, iosystem_desc_t *ios)
{
  file_desc_t *file = get_file(false);
  if(!file){
    LOG_ERR(wrank, "Unable to allocate mock file_desc_t\n");
    return PIO_EINTERNAL;
  }

  bool expected = ios->io_comm_info->is_sync_io_proc();
  bool got = SPIO_Util::Iosys_Util::is_file_owning_io_proc(ios, file);

  int ret = PIO_NOERR;
  if(got != expected){
    LOG_ERR(wrank, "is_file_owning_io_proc() with uses_async_io=false returned %d, expected is_sync_io_proc() = %d (is_async_io_proc() = %d)\n",
            got, expected, ios->io_comm_info->is_async_io_proc());
    ret = PIO_EINTERNAL;
  }

  free_file(file);
  return ret;
}

#if PIO_USE_ASYNC_WR_THREAD
/* Case (b): uses_async_io == true, with sync and async I/O roles performed
 * by the same set of ranks (shared procs) - every rank must own the file.
 */
int test_uses_async_io_true_shared_procs(int wrank, iosystem_desc_t *ios)
{
  file_desc_t *file = get_file(true);
  if(!file){
    LOG_ERR(wrank, "Unable to allocate mock file_desc_t\n");
    return PIO_EINTERNAL;
  }

  int ret = PIO_NOERR;
  if(!ios->io_comm_info->is_sync_io_proc() || !ios->io_comm_info->is_async_io_proc()){
    LOG_ERR(wrank, "Test setup error - expected shared-procs I/O comm info (is_sync_io_proc() = %d, is_async_io_proc() = %d), both should be true\n",
            ios->io_comm_info->is_sync_io_proc(), ios->io_comm_info->is_async_io_proc());
    ret = PIO_EINTERNAL;
  }

  bool got = SPIO_Util::Iosys_Util::is_file_owning_io_proc(ios, file);
  if(got != true){
    LOG_ERR(wrank, "is_file_owning_io_proc() with uses_async_io=true and shared sync/async procs returned %d, expected true\n", got);
    ret = PIO_EINTERNAL;
  }

  free_file(file);
  return ret;
}

/* Cases (c)/(d): uses_async_io == true, with disjoint sync/async I/O procs.
 * Only the async I/O ranks own the file - the sync-only rank (wrank == 0 in
 * the disjoint I/O comm info built by test_driver()) must NOT; the
 * async-only rank (wrank == 1) must.
 */
int test_uses_async_io_true_disjoint_procs(int wrank, iosystem_desc_t *ios)
{
  file_desc_t *file = get_file(true);
  if(!file){
    LOG_ERR(wrank, "Unable to allocate mock file_desc_t\n");
    return PIO_EINTERNAL;
  }

  int ret = PIO_NOERR;
  bool is_sync = ios->io_comm_info->is_sync_io_proc();
  bool is_async = ios->io_comm_info->is_async_io_proc();
  /* Confirm the I/O comm info used for this test is really disjoint for this
   * rank (sync-only on rank 0, async-only on rank 1) - otherwise the test
   * below would pass vacuously.
   */
  bool expect_async = (wrank == 1);
  if(is_sync == is_async || is_async != expect_async){
    LOG_ERR(wrank, "Test setup error - expected disjoint I/O comm info (is_sync_io_proc() = %d, is_async_io_proc() = %d)\n",
            is_sync, is_async);
    ret = PIO_EINTERNAL;
  }

  bool got = SPIO_Util::Iosys_Util::is_file_owning_io_proc(ios, file);
  if(got != expect_async){
    LOG_ERR(wrank, "is_file_owning_io_proc() with uses_async_io=true and disjoint sync/async procs returned %d, expected %d (is_async_io_proc())\n",
            got, expect_async);
    ret = PIO_EINTERNAL;
  }

  free_file(file);
  return ret;
}
#endif /* PIO_USE_ASYNC_WR_THREAD */

/* Mirrors the exact uses_async_io computation Task 1 places at both
 * production call sites (spio_createfile_int / PIOc_openfile_retry_impl),
 * right after "file->mode = mode;". Exercised directly here (rather than via
 * spio_createfile_int/PIOc_openfile_retry_impl) since those need a live
 * MPI/HDF5 file.
 */
static void compute_uses_async_io(iosystem_desc_t *ios, file_desc_t *file)
{
#if PIO_USE_ASYNC_WR_THREAD
  file->uses_async_io = (file->mode & PIO_WRITE) &&
                         !SPIO_Util::Iosys_Util::iosys_uses_async_io_service(ios);
#else
  file->uses_async_io = false;
#endif
}

/* Step 5 test(s): the uses_async_io computation itself (independent of
 * is_file_owning_io_proc).
 */
int test_uses_async_io_computation(int wrank)
{
  int ret = PIO_NOERR;

  /* iosystem_desc_t/file_desc_t contain non-trivial members (e.g.
   * std::deque, std::atomic) that a stack declaration would default
   * construct and a memset() would then corrupt. calloc(), matching the
   * existing mock pattern used elsewhere in this test suite (see
   * get_iosystem()/get_file() above and test_req_block_wait.cpp), never runs
   * those constructors/destructors at all, which is safe here because only
   * the plain-old-data fields below (intercomm, mode, uses_async_io) are
   * ever touched.
   */
  iosystem_desc_t *ios_no_async_service = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t));
  iosystem_desc_t *ios_with_async_service = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t));
  file_desc_t *file = (file_desc_t *) calloc(1, sizeof(file_desc_t));
  if(!ios_no_async_service || !ios_with_async_service || !file){
    LOG_ERR(wrank, "Unable to allocate mock iosystem_desc_t/file_desc_t\n");
    free(ios_no_async_service);
    free(ios_with_async_service);
    free(file);
    return PIO_EINTERNAL;
  }

  ios_no_async_service->intercomm = MPI_COMM_NULL;
  /* Sentinel non-null value - iosys_uses_async_io_service() only compares
   * this against MPI_COMM_NULL, it is never dereferenced/communicated on.
   */
  ios_with_async_service->intercomm = MPI_COMM_WORLD;

#if PIO_USE_ASYNC_WR_THREAD
  /* mode = PIO_NOWRITE -> always false, regardless of async-service state */
  file->mode = PIO_NOWRITE;
  compute_uses_async_io(ios_no_async_service, file);
  if(file->uses_async_io != false){
    LOG_ERR(wrank, "uses_async_io computation with mode=PIO_NOWRITE returned true, expected false\n");
    ret = PIO_EINTERNAL;
  }

  /* mode = PIO_WRITE, iosys uses the async I/O service (intercomm model)
   * -> always false
   */
  file->mode = PIO_WRITE;
  compute_uses_async_io(ios_with_async_service, file);
  if(file->uses_async_io != false){
    LOG_ERR(wrank, "uses_async_io computation with mode=PIO_WRITE and iosys_uses_async_io_service()=true returned true, expected false\n");
    ret = PIO_EINTERNAL;
  }

  /* mode = PIO_WRITE, iosys does not use the async I/O service -> true */
  file->mode = PIO_WRITE;
  compute_uses_async_io(ios_no_async_service, file);
  if(file->uses_async_io != true){
    LOG_ERR(wrank, "uses_async_io computation with mode=PIO_WRITE and iosys_uses_async_io_service()=false returned false, expected true\n");
    ret = PIO_EINTERNAL;
  }
#else
  /* PIO_USE_ASYNC_WR_THREAD compiled off -> always false, regardless of mode
   * or async-service state
   */
  file->mode = PIO_WRITE;
  compute_uses_async_io(ios_no_async_service, file);
  if(file->uses_async_io != false){
    LOG_ERR(wrank, "uses_async_io computation with PIO_USE_ASYNC_WR_THREAD off returned true, expected false\n");
    ret = PIO_EINTERNAL;
  }
#endif

  free(ios_no_async_service);
  free(ios_with_async_service);
  free(file);

  return ret;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz == 2) && num_errors);

  MPI_Group union_group = MPI_GROUP_NULL;
  if(MPI_Comm_group(comm, &union_group) != MPI_SUCCESS){
    LOG_ERR(wrank, "Unable to get MPI group for the test communicator\n");
    *num_errors += 1;
    return 1;
  }

  try{
    ret = test_uses_async_io_computation(wrank);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_ERR(wrank, "test_uses_async_io_computation() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_ERR(wrank, "test_uses_async_io_computation() PASSED\n");
  }

  /* Disjoint I/O comm info: sync_io_root = 0, sync_io_stride = 2,
   * sync_io_nprocs = 1 -> with wsz == 2, rank 0 is the (only) sync I/O proc
   * and rank 1 becomes the (only) async I/O proc (disjoint from the sync
   * set) when PIO_USE_ASYNC_WR_THREAD is on; with it off, rank 1 is simply
   * not an I/O proc at all (is_sync_io_proc() == is_async_io_proc() ==
   * false), which is still a fine (if less interesting) setup for case (a).
   */
  SPIO_Util::Iosys_Util::IO_comm_info *disjoint_io_comm_info = NULL;
  try{
    disjoint_io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(comm, union_group,
        /* sync_io_root = */ 0, /* sync_io_stride = */ 2, /* sync_io_nprocs = */ 1);
  }
  catch(...){
    LOG_ERR(wrank, "Unable to create disjoint I/O comm info\n");
    MPI_Group_free(&union_group);
    *num_errors += 1;
    return 1;
  }

  iosystem_desc_t *ios_disjoint = get_iosystem(disjoint_io_comm_info);
  if(!ios_disjoint){
    LOG_ERR(wrank, "Unable to create mock I/O system (disjoint)\n");
    delete disjoint_io_comm_info;
    MPI_Group_free(&union_group);
    *num_errors += 1;
    return 1;
  }

  try{
    ret = test_uses_async_io_false_ignores_async_flag(wrank, ios_disjoint);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_ERR(wrank, "test_uses_async_io_false_ignores_async_flag() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_ERR(wrank, "test_uses_async_io_false_ignores_async_flag() PASSED\n");
  }

#if PIO_USE_ASYNC_WR_THREAD
  try{
    ret = test_uses_async_io_true_disjoint_procs(wrank, ios_disjoint);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_ERR(wrank, "test_uses_async_io_true_disjoint_procs() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_ERR(wrank, "test_uses_async_io_true_disjoint_procs() PASSED\n");
  }
#endif

  free_iosystem(ios_disjoint);

#if PIO_USE_ASYNC_WR_THREAD
  /* Shared-procs I/O comm info: sync_io_nprocs == wsz -> every rank is a
   * sync I/O proc, so the constructor falls back to duplicating the sync
   * comm as the async comm (no dedicated async procs available) and every
   * rank is an async I/O proc too.
   */
  SPIO_Util::Iosys_Util::IO_comm_info *shared_io_comm_info = NULL;
  try{
    shared_io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(comm, union_group,
        /* sync_io_root = */ 0, /* sync_io_stride = */ 1, /* sync_io_nprocs = */ wsz);
  }
  catch(...){
    LOG_ERR(wrank, "Unable to create shared-procs I/O comm info\n");
    MPI_Group_free(&union_group);
    *num_errors += 1;
    return 1;
  }

  iosystem_desc_t *ios_shared = get_iosystem(shared_io_comm_info);
  if(!ios_shared){
    LOG_ERR(wrank, "Unable to create mock I/O system (shared procs)\n");
    delete shared_io_comm_info;
    MPI_Group_free(&union_group);
    *num_errors += 1;
    return 1;
  }

  try{
    ret = test_uses_async_io_true_shared_procs(wrank, ios_shared);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_ERR(wrank, "test_uses_async_io_true_shared_procs() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_ERR(wrank, "test_uses_async_io_true_shared_procs() PASSED\n");
  }

  free_iosystem(ios_shared);
#endif /* PIO_USE_ASYNC_WR_THREAD */

  MPI_Group_free(&union_group);

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
    fprintf(stderr, "GPTLinitialize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  ret = pio_test_mpi_init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Init() FAILED, ret = %d\n", ret);
    return ret;
  }

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
    return ret;
  }
  ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Comm_size() FAILED, ret = %d\n", ret);
    return ret;
  }

  if(wsz != 2){
    fprintf(stderr, "This test requires exactly 2 MPI processes, got %d\n", wsz);
    MPI_Finalize();
    return FAIL;
  }

  num_errors = 0;
  ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
  if(ret != 0){
    LOG_ERR(wrank, "Test driver FAILED\n");
  }
  else{
    LOG_ERR(wrank, "All tests PASSED\n");
  }

  MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLfinalize();
  if(ret != 0){
    fprintf(stderr, "GPTLfinalize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  if(num_errors != 0){
    LOG_ERR(wrank, "Total errors = %d\n", num_errors);
    return FAIL;
  }
  return 0;
}

#else /* _HDF5 */

/* is_file_owning_io_proc()/file_desc_t::uses_async_io only exist when _HDF5
 * is defined - nothing to test otherwise.
 */
int main(int argc, char *argv[])
{
  return 0;
}

#endif /* _HDF5 */
