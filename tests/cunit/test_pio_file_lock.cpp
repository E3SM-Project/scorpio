#include <cstdio>
#include <cstdint>
#include <cassert>
#include <cstring>
#include <string>
#include <sys/stat.h>
#include <thread>
#include <chrono>

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

/* Tests for spio_wait_on_async_lock_file() (Task 9): on the sync I/O root,
 * a read-only reopen must wait (bounded) for a pre-existing asynchronous
 * write lock file to be removed before proceeding, but must never block
 * indefinitely - a lock file that is never removed degrades to a warning,
 * not an error. All three tests below build a single-process sync I/O comm
 * the same way test_lock_create_remove_on_async_root() does above (this
 * rank ends up as both the sync I/O root - get_sync_io_master() == MPI_ROOT
 * - and a sync I/O proc, exactly the gating conditions
 * spio_wait_on_async_lock_file() checks for), with intercomm explicitly set
 * to MPI_COMM_NULL so iosys_uses_async_io_service(&ios) is false (this is
 * the in-process async-write-thread-pool iosystem shape that
 * spio_wait_on_async_lock_file() is meant for, as opposed to the disjoint
 * MPI-intercomm-based async I/O service).
 */

/* Case (a): no lock file present at all -> returns immediately with
 * PIO_NOERR, without ever sleeping/polling. */
int test_wait_on_async_lock_file_no_lock_present(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  int mpierr = MPI_SUCCESS;

  MPI_Group union_group = MPI_GROUP_NULL;
  mpierr = MPI_Comm_group(comm, &union_group);
  assert(mpierr == MPI_SUCCESS);

  SPIO_Util::Iosys_Util::IO_comm_info io_comm_info(comm, union_group, 0, 1, 1);

  iosystem_desc_t ios{};
  ios.iosysid = 0;
  ios.io_comm_info = &io_comm_info;
  ios.intercomm = MPI_COMM_NULL;
  ios.spio_lock_id = 0xaaaa000000000001ULL;

  const char *fname = "test_wait_lock_no_lock_file.nc";
  std::string lock_filename = spio_get_async_lock_filename(fname, ios.spio_lock_id);
  /* Make sure we are not accidentally seeing a stale lock file from a
   * previous (failed) run of this test. */
  remove(lock_filename.c_str());

  std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
  int wait_ret = spio_wait_on_async_lock_file(&ios, fname);
  long long elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start).count();

  if(wait_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_no_lock_present() FAILED: "
      "spio_wait_on_async_lock_file() returned %d (expected PIO_NOERR) when no lock file is present\n", wait_ret);
    ret = PIO_EINTERNAL;
  }
  /* Should return essentially immediately - well under one poll interval
   * (500 ms, see pio_file.cpp) - since the lock file never existed. */
  if(elapsed_ms >= 500){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_no_lock_present() FAILED: "
      "spio_wait_on_async_lock_file() took %lld ms to return when no lock file was present "
      "(expected it to return immediately)\n", elapsed_ms);
    ret = PIO_EINTERNAL;
  }

  return ret;
}

/* Case (b): lock file present at call time, removed by a concurrent helper
 * thread partway through the wait -> returns PIO_NOERR after the removal is
 * observed, well under the full SPIO_ASYNC_OP_WAIT_TIMEOUT bound. */
int test_wait_on_async_lock_file_removed_concurrently(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  int mpierr = MPI_SUCCESS;

  MPI_Group union_group = MPI_GROUP_NULL;
  mpierr = MPI_Comm_group(comm, &union_group);
  assert(mpierr == MPI_SUCCESS);

  SPIO_Util::Iosys_Util::IO_comm_info io_comm_info(comm, union_group, 0, 1, 1);

  iosystem_desc_t ios{};
  ios.iosysid = 0;
  ios.io_comm_info = &io_comm_info;
  ios.intercomm = MPI_COMM_NULL;
  ios.spio_lock_id = 0xbbbb000000000002ULL;

  const char *fname = "test_wait_lock_removed_concurrently.nc";
  std::string lock_filename = spio_get_async_lock_filename(fname, ios.spio_lock_id);
  remove(lock_filename.c_str());

  FILE *fp = fopen(lock_filename.c_str(), "w");
  if(!fp){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_removed_concurrently() FAILED: "
      "could not create the test lock file \"%s\"\n", lock_filename.c_str());
    return PIO_EINTERNAL;
  }
  fclose(fp);

  /* Remove the lock file from a background thread, well inside the
   * SPIO_ASYNC_OP_WAIT_TIMEOUT bound that spio_wait_on_async_lock_file()
   * polls against (500 ms per poll - see pio_file.cpp), simulating a
   * concurrent process finishing its async write/close and clearing the
   * lock while this rank is waiting on it. */
  std::thread remover([&lock_filename](){
    std::this_thread::sleep_for(std::chrono::milliseconds(750));
    remove(lock_filename.c_str());
  });

  std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
  int wait_ret = spio_wait_on_async_lock_file(&ios, fname);
  long long elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start).count();

  remover.join();

  if(wait_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_removed_concurrently() FAILED: "
      "spio_wait_on_async_lock_file() returned %d (expected PIO_NOERR) after the lock file "
      "was removed concurrently\n", wait_ret);
    ret = PIO_EINTERNAL;
  }
  /* Must return well under the full SPIO_ASYNC_OP_WAIT_TIMEOUT bound - it
   * should have observed the concurrent removal, not timed out. */
  if(elapsed_ms >= SPIO_ASYNC_OP_WAIT_TIMEOUT){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_removed_concurrently() FAILED: "
      "spio_wait_on_async_lock_file() took %lld ms (>= the %d ms timeout) - expected it to "
      "observe the concurrent removal well before timing out\n", elapsed_ms, SPIO_ASYNC_OP_WAIT_TIMEOUT);
    ret = PIO_EINTERNAL;
  }

  /* Safety net in case the test itself failed oddly and left the lock file behind. */
  remove(lock_filename.c_str());

  return ret;
}

/* Case (c): lock file present and never removed -> returns after
 * approximately SPIO_ASYNC_OP_WAIT_TIMEOUT with PIO_NOERR (timeout-then-warn,
 * never an error code - a stale lock degrades to a warning per the spec).
 *
 * This test deliberately lets spio_wait_on_async_lock_file() run to its full
 * SPIO_ASYNC_OP_WAIT_TIMEOUT bound (120000 ms / 2 minutes by this project's
 * default CMake configuration - see the top-level CMakeLists.txt) without
 * ever removing the lock file, so it is slow by design. Per the plan spec
 * this exact scenario (exercising the real configured timeout end to end) is
 * explicitly out of CI scope (Non-Goals); a second, shorter timeout constant
 * invented just to make this one test fast would not actually be testing
 * SPIO_ASYNC_OP_WAIT_TIMEOUT. This is the only slow test in this file.
 */
int test_wait_on_async_lock_file_timeout(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  int mpierr = MPI_SUCCESS;

  MPI_Group union_group = MPI_GROUP_NULL;
  mpierr = MPI_Comm_group(comm, &union_group);
  assert(mpierr == MPI_SUCCESS);

  SPIO_Util::Iosys_Util::IO_comm_info io_comm_info(comm, union_group, 0, 1, 1);

  iosystem_desc_t ios{};
  ios.iosysid = 0;
  ios.io_comm_info = &io_comm_info;
  ios.intercomm = MPI_COMM_NULL;
  ios.spio_lock_id = 0xcccc000000000003ULL;

  const char *fname = "test_wait_lock_timeout.nc";
  std::string lock_filename = spio_get_async_lock_filename(fname, ios.spio_lock_id);
  remove(lock_filename.c_str());

  FILE *fp = fopen(lock_filename.c_str(), "w");
  if(!fp){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_timeout() FAILED: "
      "could not create the test lock file \"%s\"\n", lock_filename.c_str());
    return PIO_EINTERNAL;
  }
  fclose(fp);

  LOG_RANK0(wrank, "test_wait_on_async_lock_file_timeout(): this test deliberately waits "
    "for the full SPIO_ASYNC_OP_WAIT_TIMEOUT (%d ms) to elapse - it is expected to be slow\n",
    SPIO_ASYNC_OP_WAIT_TIMEOUT);

  std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
  int wait_ret = spio_wait_on_async_lock_file(&ios, fname);
  long long elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start).count();

  if(wait_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_timeout() FAILED: "
      "spio_wait_on_async_lock_file() returned %d (expected PIO_NOERR even on timeout - "
      "a stale lock degrades to a warning, never a hard failure)\n", wait_ret);
    ret = PIO_EINTERNAL;
  }
  /* Allow some slack either side of the nominal timeout for poll-interval
   * granularity (500 ms) and scheduling jitter. */
  long long min_expected_ms = (long long)SPIO_ASYNC_OP_WAIT_TIMEOUT - 1000;
  if(elapsed_ms < min_expected_ms){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_timeout() FAILED: "
      "spio_wait_on_async_lock_file() returned after only %lld ms - expected it to wait "
      "approximately the full %d ms timeout since the lock file was never removed\n",
      elapsed_ms, SPIO_ASYNC_OP_WAIT_TIMEOUT);
    ret = PIO_EINTERNAL;
  }
  if(!spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_timeout() FAILED: test setup bug - "
      "the lock file \"%s\" disappeared on its own during the wait\n", lock_filename.c_str());
    ret = PIO_EINTERNAL;
  }

  /* This test's own lock file is never removed by spio_wait_on_async_lock_file()
   * (that is the whole point) - clean it up now that the wait has finished. */
  remove(lock_filename.c_str());

  return ret;
}

/* Step 6 integration test: exercise the full lock-file lifecycle end to end
 * using spio_create_async_lock_file()/spio_remove_async_lock_file() (Task 8)
 * together with spio_wait_on_async_lock_file() (this task), rather than each
 * function's own unit tests above manipulating the lock file directly with
 * fopen()/remove().
 *
 * A true two-disjoint-process-group MPI test (one group holding a
 * write-capable async-routed file open, a different, disjoint group racing
 * a read-only reopen of the same filename) is impractical within this
 * file's existing single-binary cunit harness: every other test here
 * constructs its IO_comm_info directly from a single MPI_COMM_WORLD rank
 * rather than standing up two disjoint iosystems via
 * PIOc_Init_Intracomm()/PIOc_Init_Async(), and this project's cunit
 * CMakeLists.txt runs test_pio_file_lock with a single process (see
 * add_test(NAME test_pio_file_lock COMMAND test_pio_file_lock) - no
 * add_mpi_test/NUMPROCS). Per the task brief's own fallback, this test
 * instead simulates the race sequentially within one process:
 *   - a background std::thread stands in for "group A" (the writer that
 *     created the lock file and is still holding the file open for an
 *     async write): it sleeps briefly, then calls
 *     spio_remove_async_lock_file() exactly as group A's real hard close
 *     would once the pending write completes;
 *   - the main thread stands in for "group B" (a reader attempting a
 *     read-only PIOc_openfile_retry_impl() of the same filename): it calls
 *     spio_wait_on_async_lock_file() while the lock file is still present,
 *     observes group A's (simulated) removal, and then - now that the lock
 *     is clear - a follow-up call proves "group B's open proceeds"
 *     immediately.
 */
int test_wait_on_async_lock_file_integration(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  int mpierr = MPI_SUCCESS;

  MPI_Group union_group = MPI_GROUP_NULL;
  mpierr = MPI_Comm_group(comm, &union_group);
  assert(mpierr == MPI_SUCCESS);

  SPIO_Util::Iosys_Util::IO_comm_info io_comm_info(comm, union_group, 0, 1, 1);

  iosystem_desc_t ios{};
  ios.iosysid = 0;
  ios.io_comm_info = &io_comm_info;
  ios.intercomm = MPI_COMM_NULL;
  ios.spio_lock_id = 0xdddd000000000004ULL;

  const char *fname = "test_wait_lock_integration_file.nc";
  std::string lock_filename = spio_get_async_lock_filename(fname, ios.spio_lock_id);
  remove(lock_filename.c_str());

  /* "Group A" (writer): create a file_desc_t that uses async I/O and create
   * its lock file - exactly what the HDF5 async write path does for a real
   * write-capable async-routed file (see spio_create_async_lock_file()'s own
   * doc comment above pio_file.cpp). */
  file_desc_t file{};
  file.pio_ncid = 0;
  file.uses_async_io = true;
  strncpy(file.fname, fname, PIO_MAX_NAME);
  file.fname[PIO_MAX_NAME] = '\0';

  int create_ret = spio_create_async_lock_file(&ios, &file);
  if(create_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED: "
      "spio_create_async_lock_file() returned %d (expected PIO_NOERR)\n", create_ret);
    return PIO_EINTERNAL;
  }
  if(!spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED: "
      "lock file \"%s\" was not created\n", lock_filename.c_str());
    remove(lock_filename.c_str());
    return PIO_EINTERNAL;
  }

  /* "Group A" finishing its pending async write/close, backgrounded so it
   * races the wait below: removes the lock file via the exact same helper
   * the real write-close path uses. */
  std::thread writer_finishes([&ios, &file](){
    std::this_thread::sleep_for(std::chrono::milliseconds(750));
    int remove_ret = spio_remove_async_lock_file(&ios, &file);
    assert(remove_ret == PIO_NOERR);
  });

  /* "Group B" (reader): a read-only reopen attempt waits on the still-present
   * lock file. */
  std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
  int wait_ret = spio_wait_on_async_lock_file(&ios, fname);
  long long elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start).count();

  writer_finishes.join();

  if(wait_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED: "
      "spio_wait_on_async_lock_file() returned %d (expected PIO_NOERR) after group A's "
      "(simulated) close removed the lock file\n", wait_ret);
    ret = PIO_EINTERNAL;
  }
  if(elapsed_ms >= SPIO_ASYNC_OP_WAIT_TIMEOUT){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED: "
      "spio_wait_on_async_lock_file() took %lld ms (>= the %d ms timeout) - expected it to "
      "observe group A's lock-file removal well before timing out\n", elapsed_ms, SPIO_ASYNC_OP_WAIT_TIMEOUT);
    ret = PIO_EINTERNAL;
  }
  if(spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED: "
      "lock file \"%s\" still exists after group A's close\n", lock_filename.c_str());
    ret = PIO_EINTERNAL;
    remove(lock_filename.c_str());
  }

  /* "Group B's open proceeds": now that the lock is clear, a follow-up
   * read-only reopen attempt must return immediately. */
  std::chrono::steady_clock::time_point start2 = std::chrono::steady_clock::now();
  int wait_ret2 = spio_wait_on_async_lock_file(&ios, fname);
  long long elapsed2_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start2).count();
  if(wait_ret2 != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED: "
      "spio_wait_on_async_lock_file() returned %d (expected PIO_NOERR) on the follow-up "
      "call after the lock was already cleared\n", wait_ret2);
    ret = PIO_EINTERNAL;
  }
  if(elapsed2_ms >= 500){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED: "
      "the follow-up spio_wait_on_async_lock_file() call took %lld ms - expected it to "
      "proceed immediately now that the lock file is gone\n", elapsed2_ms);
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

  ret = test_wait_on_async_lock_file_no_lock_present(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_no_lock_present() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_no_lock_present() PASSED\n");
  }

  ret = test_wait_on_async_lock_file_removed_concurrently(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_removed_concurrently() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_removed_concurrently() PASSED\n");
  }

  ret = test_wait_on_async_lock_file_integration(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_integration() PASSED\n");
  }

  /* Deliberately slow (waits for the full SPIO_ASYNC_OP_WAIT_TIMEOUT) - see
   * this test's own doc comment above. Run last so the fast tests above
   * always get a result even if this one is interrupted. */
  ret = test_wait_on_async_lock_file_timeout(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_timeout() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_timeout() PASSED\n");
  }
#else
  LOG_RANK0(wrank, "test_lock_create_remove_on_async_root() SKIPPED (PIO_USE_ASYNC_WR_THREAD is disabled - "
    "no rank can be the async I/O master in this build, and file->uses_async_io is always false in "
    "production for this build too, so the create/remove calls would always be no-ops here)\n");
  LOG_RANK0(wrank, "test_wait_on_async_lock_file_*() SKIPPED (PIO_USE_ASYNC_WR_THREAD is disabled - "
    "spio_wait_on_async_lock_file() is an unconditional no-op in this build, so there is no "
    "poll/timeout behavior to exercise)\n");
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
