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

#if PIO_USE_ASYNC_WR_THREAD && defined(_HDF5)
/* Only needed by test_wait_on_async_lock_file_real_open_wiring() below, to
 * drain any pending async ops left over from this test's own real
 * PIOc_createfile()/PIOc_closefile() calls before manufacturing a lock file
 * of our own. */
#include "spio_async_hdf5_utils.hpp"
#endif

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

#ifdef _HDF5
/* Test the no-op case: spio_create_async_lock_file()/spio_remove_async_lock_file()
 * must not create/touch any file at all when file->uses_async_io is false -
 * regardless of whether this rank happens to be the async I/O master.
 *
 * file_desc_t::uses_async_io and spio_create_async_lock_file()/
 * spio_remove_async_lock_file() only exist when _HDF5 is defined (see
 * pio_types.hpp/pio_internal.h), so this test has nothing to exercise
 * otherwise - see the SKIPPED message at this function's call site below.
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
#endif /* _HDF5 */

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

#ifdef _HDF5
/* Fix round 1 (post-review): all of the tests above call
 * spio_wait_on_async_lock_file() directly, which is exactly why the
 * original wiring bug at the real call site was not caught - that call
 * site (in PIOc_openfile_retry_impl(), pioc_support.cpp) originally read
 * "if(mode & PIO_NOWRITE)", but PIO_NOWRITE is NC_NOWRITE == 0x0000 (see
 * the same gotcha documented in tests/cunit/test_spio_iodesc_utils.cpp), so
 * "mode & PIO_NOWRITE" is always false for every possible mode value and
 * spio_wait_on_async_lock_file() was never actually invoked from a real
 * reopen. The fix is "if(!(mode & PIO_WRITE))". This test exercises the
 * real, public PIOc_openfile() -> PIOc_openfile_impl() -> openfile_int() ->
 * PIOc_openfile_retry_impl() entry point end to end, so a regression of
 * that mode-bit check would be caught here even if every direct-call test
 * above kept passing.
 *
 * As in test_wait_on_async_lock_file_integration() above, a true two-
 * disjoint-process-group MPI setup is impractical in this single-process
 * cunit binary, so "group A" (the async writer) is simulated by
 * manufacturing the lock file directly with Task 8's real
 * spio_create_async_lock_file()/spio_remove_async_lock_file() helpers on a
 * background thread, rather than racing the real async write thread pool's
 * own (non-deterministic) completion timing. What is NOT simulated is the
 * call site itself: "group B"'s reopen goes through the real, unmodified
 * PIOc_openfile() public API against a real, single-rank PIOc_Init_Intracomm()
 * iosystem and a real HDF5 file on disk, using that iosystem's own real
 * ios->spio_lock_id (not a hand-rolled one), so the exact code path
 * production code runs is what gets timed and checked below.
 */
int test_wait_on_async_lock_file_real_open_wiring(MPI_Comm comm, int wrank)
{
  int ret = PIO_NOERR;
  int iosysid = -1;
  const char *fname = "test_wait_lock_real_wiring.nc";

  if((ret = PIOc_set_iosystem_error_handling(PIO_DEFAULT, PIO_RETURN_ERROR, NULL))){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "PIOc_set_iosystem_error_handling() returned %d\n", ret);
    return PIO_EINTERNAL;
  }

  if((ret = PIOc_Init_Intracomm(comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid))){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return PIO_EINTERNAL;
  }

  int format = PIO_IOTYPE_HDF5;
  int ncid = -1;
  if((ret = PIOc_createfile(iosysid, &ncid, &format, fname, PIO_CLOBBER))){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "PIOc_createfile() returned %d\n", ret);
    PIOc_finalize(iosysid);
    return PIO_EINTERNAL;
  }

  int dimid = -1;
  if((ret = PIOc_def_dim(ncid, "x", (PIO_Offset)4, &dimid)) ||
     (ret = PIOc_enddef(ncid))){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "PIOc_def_dim()/PIOc_enddef() returned %d\n", ret);
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return PIO_EINTERNAL;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  file_desc_t *file = NULL;
  if(!ios || (ret = pio_get_file(ncid, &file)) || !file){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "pio_get_iosystem_from_id()/pio_get_file() lookup failed (ret = %d)\n", ret);
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return PIO_EINTERNAL;
  }

  if(!file->uses_async_io){
    /* Nothing to prove here - without uses_async_io there would never be a
     * lock file for this filename in production either. Skip rather than
     * fail: this can legitimately happen on topologies where
     * compute_uses_async_io() decides against async routing. */
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() SKIPPED: "
      "file->uses_async_io is false for this topology - nothing to exercise\n");
    PIOc_closefile(ncid);
    PIOc_finalize(iosysid);
    return PIO_NOERR;
  }

  if((ret = PIOc_closefile(ncid))){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "PIOc_closefile() returned %d\n", ret);
    PIOc_finalize(iosysid);
    return PIO_EINTERNAL;
  }
  /* Drain any pending async ops (including this real close's own lock-file
   * handling) before we manufacture our own lock file below, so the two
   * cannot race/collide - the lock filename is deterministic from
   * (fname, ios->spio_lock_id) alone, so the real close and our manufactured
   * one below would otherwise contend for the exact same path. */
  spio_wait_all_hdf5_async_ops(iosysid);

  std::string lock_filename = spio_get_async_lock_filename(fname, ios->spio_lock_id);
  remove(lock_filename.c_str());

  file_desc_t fake_async_file{};
  fake_async_file.pio_ncid = 0;
  fake_async_file.uses_async_io = true;
  strncpy(fake_async_file.fname, fname, PIO_MAX_NAME);
  fake_async_file.fname[PIO_MAX_NAME] = '\0';

  int create_ret = spio_create_async_lock_file(ios, &fake_async_file);
  if((create_ret != PIO_NOERR) || !spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "could not manufacture the lock file \"%s\" (create_ret = %d)\n",
      lock_filename.c_str(), create_ret);
    remove(lock_filename.c_str());
    PIOc_finalize(iosysid);
    return PIO_EINTERNAL;
  }

  /* "Group A" finishing its pending async write/close, backgrounded so it
   * races the real reopen attempt below. */
  std::thread writer_finishes([ios, &fake_async_file](){
    std::this_thread::sleep_for(std::chrono::milliseconds(750));
    int remove_ret = spio_remove_async_lock_file(ios, &fake_async_file);
    assert(remove_ret == PIO_NOERR);
  });

  /* "Group B" (reader): a real read-only reopen through the actual public
   * PIOc_openfile() entry point - this is the call that must wait on the
   * lock file. If the mode-bit check at the real call site is broken (e.g.
   * the "mode & PIO_NOWRITE" bug this test was added to catch), this
   * reopen races ahead immediately instead of waiting for the thread
   * above. */
  int reopen_ncid = -1;
  int reopen_format = PIO_IOTYPE_HDF5;
  std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
  int open_ret = PIOc_openfile(iosysid, &reopen_ncid, &reopen_format, fname, PIO_NOWRITE);
  long long elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start).count();

  writer_finishes.join();

  if(open_ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "the real read-only PIOc_openfile() reopen returned %d (expected PIO_NOERR)\n", open_ret);
    ret = PIO_EINTERNAL;
  }
  else{
    PIOc_closefile(reopen_ncid);
  }

  /* The key assertion: this real reopen must have actually waited for
   * group A's (simulated) lock-file removal, not raced ahead of it. A
   * reopen that returned well before the 750 ms removal delay above,
   * despite the lock file still being present when it started, means the
   * real call site never invoked spio_wait_on_async_lock_file() at all -
   * exactly the "mode & PIO_NOWRITE is always false" bug this test is for.
   * Leave headroom below the 750 ms delay for scheduling jitter. */
  if(elapsed_ms < 500){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "the real read-only reopen returned after only %lld ms, despite the "
      "asynchronous write lock file still being present when it started - "
      "the real PIOc_openfile_retry_impl() call site did not wait on it "
      "(this is exactly the \"mode & PIO_NOWRITE is always false\" bug)\n",
      elapsed_ms);
    ret = PIO_EINTERNAL;
  }
  if(elapsed_ms >= SPIO_ASYNC_OP_WAIT_TIMEOUT){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "the real read-only reopen took %lld ms (>= the %d ms timeout) - expected "
      "it to observe group A's lock-file removal well before timing out\n",
      elapsed_ms, SPIO_ASYNC_OP_WAIT_TIMEOUT);
    ret = PIO_EINTERNAL;
  }
  if(spio_test_file_exists(lock_filename)){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED: "
      "lock file \"%s\" still exists after group A's (simulated) close\n", lock_filename.c_str());
    ret = PIO_EINTERNAL;
    remove(lock_filename.c_str());
  }

  PIOc_deletefile(iosysid, fname);
  PIOc_finalize(iosysid);

  return ret;
}
#endif /* _HDF5 */
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

#ifdef _HDF5
  ret = test_lock_create_remove_noop_when_not_using_async_io(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() PASSED\n");
  }
#else
  LOG_RANK0(wrank, "test_lock_create_remove_noop_when_not_using_async_io() SKIPPED (_HDF5 is not "
    "enabled in this build - file_desc_t::uses_async_io and the lock-file create/remove helpers "
    "do not exist in this build)\n");
#endif /* _HDF5 */

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

#ifdef _HDF5
  /* Exercises the real PIOc_openfile() -> ... -> PIOc_openfile_retry_impl()
   * wiring end to end - see this test's own doc comment above for why it
   * exists in addition to the direct-call tests above. */
  ret = test_wait_on_async_lock_file_real_open_wiring(comm, wrank);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() PASSED\n");
  }
#else
  LOG_RANK0(wrank, "test_wait_on_async_lock_file_real_open_wiring() SKIPPED (_HDF5 is not "
    "enabled in this build - no HDF5 iotype to open a real file through)\n");
#endif

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
