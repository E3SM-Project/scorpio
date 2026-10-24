#include <cstdio>
#include <cstdint>
#include <cassert>
#include <cstring>
#include <string>
#include <sys/stat.h>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"
#include "spio_iosys_utils.hpp"

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

static bool spio_test_file_exists(const std::string &path)
{
  struct stat st;
  return (stat(path.c_str(), &st) == 0);
}

/* Test spio_get_async_lock_filename(): exact expected string, including the
 * fixed-width (16 hex digit), zero-padded, lowercase formatting of lock_id -
 * in particular the lock_id = 0 case, which is the case most likely to break
 * a naive hex-formatting implementation that does not force the field width.
 */
int test_lock_filename_exact(int wrank)
{
  int ret = PIO_NOERR;
  const char *fname = "test_lockfile_base.nc";

  uint64_t lock_id = 0x0102030405060708ULL;
  std::string expected = std::string(fname) + "_0102030405060708.spio_lock";
  std::string actual = spio_get_async_lock_filename(fname, lock_id);
  if(actual != expected){
    LOG_RANK0(wrank, "test_lock_filename_exact() FAILED: expected \"%s\", got \"%s\"\n",
      expected.c_str(), actual.c_str());
    ret = PIO_EINTERNAL;
  }

  uint64_t zero_lock_id = 0;
  std::string expected_zero = std::string(fname) + "_0000000000000000.spio_lock";
  std::string actual_zero = spio_get_async_lock_filename(fname, zero_lock_id);
  if(actual_zero != expected_zero){
    LOG_RANK0(wrank, "test_lock_filename_exact() FAILED (zero lock_id): expected \"%s\", got \"%s\"\n",
      expected_zero.c_str(), actual_zero.c_str());
    ret = PIO_EINTERNAL;
  }

  return ret;
}

/* Test that two different lock_ids on the same filename produce different
 * lock filenames (the whole point of keying the lock filename on the
 * per-iosystem lock id).
 */
int test_lock_filename_uniqueness(int wrank)
{
  int ret = PIO_NOERR;
  const char *fname = "test_lockfile_uniq.nc";

  std::string n1 = spio_get_async_lock_filename(fname, 1);
  std::string n2 = spio_get_async_lock_filename(fname, 2);
  if(n1 == n2){
    LOG_RANK0(wrank, "test_lock_filename_uniqueness() FAILED: two different lock ids "
      "(1, 2) for the same filename produced identical lock filenames (\"%s\")\n", n1.c_str());
    ret = PIO_EINTERNAL;
  }

  return ret;
}

/* Test the no-op case: spio_create_async_lock_file()/spio_remove_async_lock_file()
 * must not create/touch any file at all when file->uses_async_io is false -
 * regardless of whether this rank happens to be the async I/O master.
 */
int test_lock_create_remove_noop_when_not_using_async_io(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  int mpierr = MPI_SUCCESS;

  MPI_Group union_group = MPI_GROUP_NULL;
  mpierr = MPI_Comm_group(comm, &union_group);
  assert(mpierr == MPI_SUCCESS);

  /* A single sync I/O proc (this rank), union comm == sync comm. Whether or
   * not this ends up being the async I/O master (depends on
   * PIO_USE_ASYNC_WR_THREAD) is irrelevant for this test: uses_async_io =
   * false alone must make both calls a no-op.
   */
  SPIO_Util::Iosys_Util::IO_comm_info io_comm_info(comm, union_group, 0, 1, 1);

  /* Value-initialize in place (not memset(), and not "= T()" which would
   * need a move/copy ctor) since both structs contain non-POD, non-copyable
   * members (e.g. iosystem_desc_t::async_pend_ops is a std::deque, and
   * file_desc_t has std::atomic members under some configs) that memset()
   * would corrupt and that a copy/move-construct-then-assign would fail to
   * compile against. */
  iosystem_desc_t ios{};
  ios.iosysid = 0;
  ios.io_comm_info = &io_comm_info;
  ios.spio_lock_id = 0xdeadbeefcafebabeULL;

  file_desc_t file{};
  file.pio_ncid = 0;
  file.uses_async_io = false;
  strncpy(file.fname, "test_lock_noop_file.nc", PIO_MAX_NAME);
  file.fname[PIO_MAX_NAME] = '\0';

  std::string lock_filename = spio_get_async_lock_filename(file.fname, ios.spio_lock_id);
  /* Make sure we are not accidentally seeing a stale file from a previous
   * (failed) run of this test. */
  remove(lock_filename.c_str());

  int create_ret = spio_create_async_lock_file(&ios, &file);
  if(create_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() FAILED: "
      "spio_create_async_lock_file() returned %d (expected PIO_NOERR) for uses_async_io=false\n", create_ret);
    ret = PIO_EINTERNAL;
  }
  if(spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() FAILED: "
      "lock file \"%s\" was created even though uses_async_io=false\n", lock_filename.c_str());
    ret = PIO_EINTERNAL;
    remove(lock_filename.c_str());
  }

  int remove_ret = spio_remove_async_lock_file(&ios, &file);
  if(remove_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() FAILED: "
      "spio_remove_async_lock_file() returned %d (expected PIO_NOERR) for uses_async_io=false\n", remove_ret);
    ret = PIO_EINTERNAL;
  }

  return ret;
}

#if PIO_USE_ASYNC_WR_THREAD
/* Test the real create -> remove lifecycle on the async I/O root rank.
 *
 * With a single-process union comm and 1 sync I/O proc (this rank), the
 * production IO_comm_info ctor has no procs left over to dedicate to async
 * I/O, so it falls back to duplicating the sync comm for async I/O
 * (dup_sync_comm) - this rank ends up as both the sync and the async I/O
 * root, i.e. get_async_io_master() == MPI_ROOT, which is exactly the gating
 * condition spio_create_async_lock_file()/spio_remove_async_lock_file()
 * check for.
 */
int test_lock_create_remove_on_async_root(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  int mpierr = MPI_SUCCESS;

  MPI_Group union_group = MPI_GROUP_NULL;
  mpierr = MPI_Comm_group(comm, &union_group);
  assert(mpierr == MPI_SUCCESS);

  SPIO_Util::Iosys_Util::IO_comm_info io_comm_info(comm, union_group, 0, 1, 1);

  if(io_comm_info.get_async_io_master() != MPI_ROOT){
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() FAILED: test setup bug - "
      "expected this single process to be the async I/O master (get_async_io_master() == MPI_ROOT), got %d\n",
      io_comm_info.get_async_io_master());
    return PIO_EINTERNAL;
  }

  iosystem_desc_t ios{};
  ios.iosysid = 0;
  ios.io_comm_info = &io_comm_info;
  ios.spio_lock_id = 0x1122334455667788ULL;

  file_desc_t file{};
  file.pio_ncid = 0;
  file.uses_async_io = true;
  strncpy(file.fname, "test_lock_async_root_file.nc", PIO_MAX_NAME);
  file.fname[PIO_MAX_NAME] = '\0';

  std::string lock_filename = spio_get_async_lock_filename(file.fname, ios.spio_lock_id);
  remove(lock_filename.c_str());

  int create_ret = spio_create_async_lock_file(&ios, &file);
  if(create_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() FAILED: "
      "spio_create_async_lock_file() returned %d (expected PIO_NOERR)\n", create_ret);
    ret = PIO_EINTERNAL;
  }
  if(!spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() FAILED: "
      "lock file \"%s\" was not created\n", lock_filename.c_str());
    ret = PIO_EINTERNAL;
  }

  int remove_ret = spio_remove_async_lock_file(&ios, &file);
  if(remove_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() FAILED: "
      "spio_remove_async_lock_file() returned %d (expected PIO_NOERR)\n", remove_ret);
    ret = PIO_EINTERNAL;
  }
  if(spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() FAILED: "
      "lock file \"%s\" still exists after spio_remove_async_lock_file()\n", lock_filename.c_str());
    ret = PIO_EINTERNAL;
    remove(lock_filename.c_str());
  }

  /* Removing an already-removed (or never-created) lock file must not be
   * treated as an error - ENOENT is explicitly ignored. */
  int remove_again_ret = spio_remove_async_lock_file(&ios, &file);
  if(remove_again_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() FAILED: "
      "spio_remove_async_lock_file() on an already-removed lock file returned %d (expected PIO_NOERR)\n",
      remove_again_ret);
    ret = PIO_EINTERNAL;
  }

  return ret;
}
#endif /* PIO_USE_ASYNC_WR_THREAD */

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);

  ret = test_lock_filename_exact(wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_filename_exact() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_filename_exact() PASSED\n");
  }

  ret = test_lock_filename_uniqueness(wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_filename_uniqueness() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_filename_uniqueness() PASSED\n");
  }

  ret = test_lock_create_remove_noop_when_not_using_async_io(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() PASSED\n");
  }

#if PIO_USE_ASYNC_WR_THREAD
  ret = test_lock_create_remove_on_async_root(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() PASSED\n");
  }
#else
  LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() SKIPPED (PIO_USE_ASYNC_WR_THREAD is disabled - "
    "no rank can be the async I/O master in this build, and file->uses_async_io is always false in "
    "production for this build too, so the create/remove calls would always be no-ops here)\n");
#endif

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

  ret = MPI_Init(&argc, &argv);
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
    fprintf(stderr, "GPTLfinalize() FAILED, ret = %d\n", ret);
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
