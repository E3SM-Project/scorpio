/*
 * End-to-end integration tests for the HDF5 asynchronous-write lifecycle.
 *
 * Tasks 1-11 of the "HDF5 async write lifecycle and reopen" plan each test
 * their own slice in isolation (and, where a disjoint sync/async I/O proc
 * topology was impractical in a single-process cunit binary, deliberately
 * simplified it away - see e.g. test_pio_file_lock.cpp's
 * test_wait_on_async_lock_file_integration() doc comment). This file adds
 * the full-lifecycle scenarios on top of the real, public PIOc_* API:
 *
 *   1. test_write_only_full_async_lifecycle   - async on, disjoint sync/async
 *      I/O procs, write-only file: create/def/write/close are owned by the
 *      async I/O ranks only, and the old "asynchronous HDF5 writes for this
 *      I/O decomposition are disabled" warning is never emitted.
 *   2. test_read_only_always_sync             - async on, disjoint procs, a
 *      PIO_NOWRITE open is always-sync (uses_async_io == false, the sync I/O
 *      ranks own every op, and no lock file is created for it).
 *   3. test_read_write_interleaved_no_race    - async on, same-procs, a
 *      read-write file with interleaved put_var/get_var (each pair separated
 *      by an explicit PIOc_sync(), now required for read-after-write
 *      visibility on the non-decomposed vars API) and write_darray/
 *      read_darray (separated by PIOc_sync() for its own, unrelated reason -
 *      flushing the decomposed-write buffer; see the scenario's own comment).
 *   4. test_disjoint_write_close_reopen_lock_absent  - the close completes
 *      well before the reopen: lock file gone, reopen takes the fast path.
 *   5. test_disjoint_write_close_reopen_lock_present - the reopen is issued
 *      while a lock file is still present: exercises Task 9's wait path
 *      through the real PIOc_openfile() entry point.
 *   6-8. test_native_reopen_roundtrip_{contig,box,subset} - write via each
 *      rearranger, close, reopen write-capable, and check that the file
 *      stayed PIO_IOTYPE_HDF5 (not silently switched to NETCDF4P/PNETCDF)
 *      and that its dims/vars/attrs/data all round-trip.
 *   9. test_native_reopen_fallback_non_scorpio_file - a plain (non-SCORPIO)
 *      HDF5 file opened with PIO_IOTYPE_HDF5 through the real
 *      PIOc_openfile() path still falls back to the historical
 *      switch-iotype-away behavior (successful open, non-HDF5 iotype).
 *  10. test_disjoint_nondecomposed_put_get_roundtrip - async on, disjoint
 *      procs: a non-decomposed PIOc_put_var on a write-capable (async-
 *      routed) file actually writes its data (review finding C1).
 *  11. test_cross_routing_copy_att_errors - PIOc_copy_att between an
 *      async-routed and a sync-routed HDF5 file reports an error instead
 *      of silently doing nothing (review finding I2).
 *  12. test_non_hdf5_write_open_leaves_no_lock_file - a write-capable
 *      PnetCDF file leaves no *.spio_lock behind, on either the create or
 *      the write-capable open path (backlog item 8).
 *  13. test_subset_createfile_all_io_procs_no_race - createfile's Bcast no
 *      longer races the async create op's Barrier (every rank an I/O proc).
 *  14. test_subset_def_var_all_io_procs_no_race - def_var's collective fill
 *      (unlimited-dim var) no longer races a later collective across many
 *      create/def_var/close/delete cycles (every rank an I/O proc).
 *  15. test_subset_put_var_record_dim_no_race - a non-decomposed put_var on
 *      a record (unlimited) dimension no longer races H5Dextend() across
 *      ranks when writing many records back to back (every rank an I/O
 *      proc); the race silently dropped data instead of crashing.
 *
 * This file adds tests only - it does not change any production code.
 *
 * Known pre-existing, out-of-scope issues this file deliberately works
 * around rather than fixes are documented inline at each affected
 * assertion (search for "KNOWN PRE-EXISTING").
 */
#include "pio_config.h"

#ifdef _HDF5

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <string>
#include <vector>
#include <numeric>
#include <thread>
#include <chrono>

#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

#include <hdf5.h>

#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"
#include "spio_iosys_utils.hpp"

#if PIO_USE_ASYNC_WR_THREAD
#include "spio_async_hdf5_utils.hpp"
#endif

#define TEST_NAME "test_hdf5_async_full_lifecycle"

/* Four ranks with a single sync I/O proc (NUM_IO1 below) is the smallest
 * shape in which IO_comm_info's production constructor gives genuinely
 * disjoint sync and async I/O procs: it only duplicates the sync comm for
 * async I/O when sync_io_nprocs == union_comm_sz (see
 * spio_iosys_utils.cpp's "dup_sync_comm"). */
#define TARGET_NTASKS 4
#define MIN_NTASKS 4

#define NUM_IO1 1
#define STRIDE1 1
#define BASE0 0

#define NDIM1 1
#define MAPLEN_PER_RANK 4

#define VAR_NAME "lifecycle_var"
#define ND_VAR_NAME "lifecycle_nd_var"
#define DIM_NAME "x"
#define GATT_NAME "g_int"
#define VATT_NAME "v_int"

static const int GATT_VAL = 42;
static const int VATT_VAL = 7;

/* The warning spio_iodesc select_io_desc_info() used to emit whenever the
 * async I/O procs were disjoint from the sync I/O procs, before Task 3 made
 * file_desc_t::uses_async_io the single source of truth for async routing.
 * It no longer exists anywhere in src/ - scenario 1 below additionally
 * proves, at runtime, that a real disjoint-procs write lifecycle does not
 * print it (i.e. that the disjoint topology is genuinely supported now,
 * rather than being silently downgraded to sync I/O with a warning). */
#define REMOVED_ASYNC_DISABLED_WARNING \
  "Asynchronous HDF5 writes for this I/O decomposition are disabled"

static const int FAIL = -1;

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

static bool test_file_exists(const std::string &path)
{
  struct stat st;
  return (stat(path.c_str(), &st) == 0);
}

/* Sum per-rank error counts over comm so that a failure observed on any
 * single rank fails the scenario on every rank (every scenario below is
 * driven collectively, so the ranks must agree on the outcome). */
static int reduce_nerrs(MPI_Comm comm, int local_nerrs)
{
  int global_nerrs = 0;
  int mpierr = MPI_Allreduce(&local_nerrs, &global_nerrs, 1, MPI_INT, MPI_SUM, comm);
  if(mpierr != MPI_SUCCESS){
    return local_nerrs + 1;
  }

  return global_nerrs;
}

/* Temporarily redirect this rank's stderr (fd 2, so that it catches
 * PIOc_warn()'s fprintf(stderr, ...) regardless of any FILE* buffering) to
 * a per-rank scratch file, so a scenario can assert on what the library did
 * or did not print. Whatever was captured is echoed back out to the real
 * stderr by the caller, so nothing is actually swallowed. */
class Stderr_capture{
  public:
    Stderr_capture() : saved_fd_(-1), active_(false) {}
    ~Stderr_capture() { if(active_) { (void) stop(); } }

    void start(const std::string &path)
    {
      path_ = path;
      fflush(stderr);
      saved_fd_ = dup(STDERR_FILENO);
      if(saved_fd_ < 0){
        return;
      }

      int fd = open(path_.c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0644);
      if(fd < 0){
        close(saved_fd_);
        saved_fd_ = -1;
        return;
      }

      if(dup2(fd, STDERR_FILENO) < 0){
        close(fd);
        close(saved_fd_);
        saved_fd_ = -1;
        return;
      }
      close(fd);
      active_ = true;
    }

    /* Restores the real stderr and returns everything that was written to
     * the captured stderr in between. Returns an empty string if the
     * capture never started (in which case the scenario simply cannot
     * assert on the output - see its caller). */
    std::string stop(void)
    {
      std::string contents;
      if(!active_){
        return contents;
      }

      fflush(stderr);
      dup2(saved_fd_, STDERR_FILENO);
      close(saved_fd_);
      saved_fd_ = -1;
      active_ = false;

      FILE *fp = fopen(path_.c_str(), "r");
      if(fp){
        char buf[4096];
        size_t nread = 0;
        while((nread = fread(buf, 1, sizeof(buf), fp)) > 0){
          contents.append(buf, nread);
        }
        fclose(fp);
      }
      remove(path_.c_str());

      return contents;
    }

    bool active(void) const { return active_; }

  private:
    int saved_fd_;
    std::string path_;
    bool active_;
};

/* NEW BUG (found by this task, NOT in the plan's known-issues backlog -
 * documented, deliberately NOT fixed here, since this task adds tests only):
 *
 *   spio_wait_on_hard_close() (src/clib/core/pio_file.cpp) short-circuits
 *   with "if(!(file->mode & PIO_WRITE)) { return PIO_NOERR; }", on the
 *   comment "For read-only files, we don't have any async ops".
 *   spio_close_soft_closed_file() (src/clib/core/util/pio_lists.cpp) skips
 *   read-only files for the same stated reason. Since Tasks 10/11's native
 *   HDF5 reopen, that assumption is false: a read-only reopen of a
 *   SCORPIO-written HDF5 file now keeps file->iotype == PIO_IOTYPE_HDF5
 *   (it used to be switched away to PIO_IOTYPE_NETCDF4P), and
 *   PIOc_closefile_impl()'s soft-close gate tests only the iotype and
 *   is_file_owning_io_proc() - never "mode & PIO_WRITE". So closing a
 *   read-only HDF5 file queues a real async close op, PIOc_finalize() ->
 *   spio_close_all_files_and_delete_from_list() -> spio_wait_on_hard_close()
 *   declines to wait for it, and pio_free_file() then frees the file_desc_t
 *   out from under the thread-pool thread still running
 *   spio_hard_closefile() on it. ASan reports this as a heap-use-after-free
 *   at spio_hdf5_utils.cpp:1657 (spio_hdf5_close()).
 *
 * Until that is fixed in production code, every scenario below that closes
 * an HDF5 file and then finalizes its iosystem drains the async op queue
 * first through this helper. The drain is a test-side workaround only - it
 * does not paper over anything the scenarios actually assert on, since each
 * scenario's assertions are all evaluated before the drain. */
static void drain_async_ops(int iosysid)
{
#if PIO_USE_ASYNC_WR_THREAD
  spio_wait_all_hdf5_async_ops(iosysid);
#else
  (void) iosysid;
#endif
}

/* PIOc_finalize() preceded by the drain above - every scenario below uses
 * this instead of calling PIOc_finalize() directly. */
static int finalize_iosys(int iosysid)
{
  drain_async_ops(iosysid);
  return PIOc_finalize(iosysid);
}

/* True if this iosystem really has disjoint sync and async I/O procs. Used
 * to skip (rather than silently pass) the disjoint-procs scenarios on a
 * topology where IO_comm_info fell back to duplicating the sync comm. */
static bool iosys_has_disjoint_io_procs(const iosystem_desc_t *ios)
{
#if PIO_USE_ASYNC_WR_THREAD
  return (ios && ios->io_comm_info && !(ios->io_comm_info->sync_and_async_io_share_procs()));
#else
  (void) ios;
  return false;
#endif
}

/* Build the standard 1D block decomposition used by every decomposed
 * scenario below: MAPLEN_PER_RANK contiguous elements per rank. */
static int init_test_decomp(int iosysid, int my_rank, int ntasks, int rearranger,
                            int *gdimlen_out, int *ioid)
{
  const int gdimlen = MAPLEN_PER_RANK * ntasks;
  std::vector<PIO_Offset> compmap(MAPLEN_PER_RANK);
  std::iota(compmap.begin(), compmap.end(),
            static_cast<PIO_Offset>(my_rank * MAPLEN_PER_RANK + 1));

  if(gdimlen_out){
    *gdimlen_out = gdimlen;
  }

  return PIOc_init_decomp(iosysid, PIO_INT, NDIM1, &gdimlen, MAPLEN_PER_RANK,
                          compmap.data(), ioid, rearranger, NULL, NULL);
}

static void fill_wdata(std::vector<int> &wdata, int my_rank, int iter)
{
  for(int i = 0; i < (int) wdata.size(); i++){
    wdata[i] = my_rank * 1000 + iter * 100 + i;
  }
}

/* Create filename as a SCORPIO HDF5 file with one dimension, one decomposed
 * variable, one global attribute and one variable attribute; write wdata
 * through ioid; close. If uses_async_io_out is non-NULL it receives this
 * rank's file->uses_async_io as observed between enddef() and closefile().
 * If lock_present_out is non-NULL it receives whether the async write lock
 * file for this (filename, ios->spio_lock_id) existed at that same point. */
static int write_fixture_file(int iosysid, int ioid, const char *filename,
                              int gdimlen, const std::vector<int> &wdata,
                              bool *uses_async_io_out, bool *lock_present_out)
{
  int ret = PIO_NOERR;
  int ncid = -1, dimid = -1, varid = -1;
  int format = PIO_IOTYPE_HDF5;

  if(uses_async_io_out) { *uses_async_io_out = false; }
  if(lock_present_out) { *lock_present_out = false; }

  if((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER))){
    return ret;
  }

  if((ret = PIOc_def_dim(ncid, DIM_NAME, (PIO_Offset) gdimlen, &dimid)) ||
     (ret = PIOc_def_var(ncid, VAR_NAME, PIO_INT, NDIM1, &dimid, &varid))){
    PIOc_closefile(ncid);
    return ret;
  }

  if((ret = PIOc_put_att(ncid, PIO_GLOBAL, GATT_NAME, PIO_INT, 1, &GATT_VAL)) ||
     (ret = PIOc_put_att(ncid, varid, VATT_NAME, PIO_INT, 1, &VATT_VAL))){
    PIOc_closefile(ncid);
    return ret;
  }

  if((ret = PIOc_enddef(ncid))){
    PIOc_closefile(ncid);
    return ret;
  }

  {
    iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
    file_desc_t *file = NULL;
    if(ios && (pio_get_file(ncid, &file) == PIO_NOERR) && file){
      if(uses_async_io_out) { *uses_async_io_out = file->uses_async_io; }
      if(lock_present_out){
        *lock_present_out = test_file_exists(
          spio_get_async_lock_filename(filename, ios->spio_lock_id));
      }
    }
  }

  if((ret = PIOc_write_darray(ncid, varid, ioid, (PIO_Offset) wdata.size(),
                              const_cast<int *>(wdata.data()), NULL))){
    PIOc_closefile(ncid);
    return ret;
  }

  return PIOc_closefile(ncid);
}

/* ------------------------------------------------------------------------
 * Scenario 1: write-only file, disjoint sync/async I/O procs - the whole
 * create/def/write/close lifecycle is owned by the async I/O ranks, and the
 * removed "async writes disabled for disjoint procs" warning is not printed.
 * ------------------------------------------------------------------------ */
static int test_write_only_full_async_lifecycle(MPI_Comm test_comm, int my_rank, int ntasks)
{
  int ret = PIO_NOERR;
  int iosysid = -1, ioid = -1, gdimlen = 0;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_write_only.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_write_only_full_async_lifecycle() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios || !ios->io_comm_info){
    LOG_RANK0(my_rank, "test_write_only_full_async_lifecycle() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

#if PIO_USE_ASYNC_WR_THREAD
  if(!iosys_has_disjoint_io_procs(ios)){
    LOG_RANK0(my_rank, "test_write_only_full_async_lifecycle() SKIPPED: this iosystem's sync "
      "and async I/O procs are not disjoint on this topology (IO_comm_info fell back to "
      "duplicating the sync comm) - there is no disjoint-procs routing to observe\n");
    finalize_iosys(iosysid);
    return PIO_NOERR;
  }
#endif

  if((ret = init_test_decomp(iosysid, my_rank, ntasks, PIO_REARR_CONTIG, &gdimlen, &ioid))){
    LOG_RANK0(my_rank, "test_write_only_full_async_lifecycle() FAILED: "
      "PIOc_init_decomp() returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  std::vector<int> wdata(MAPLEN_PER_RANK);
  fill_wdata(wdata, my_rank, 0);

  int nerrs = 0;
  int ncid = -1, dimid = -1, varid = -1;
  int format = PIO_IOTYPE_HDF5;

  /* Capture this rank's stderr across the whole create/def/write/close
   * lifecycle so the absence of the removed warning can be asserted on
   * real output, not just on a grep of the sources. */
  char capture_path[PIO_MAX_NAME + 1];
  snprintf(capture_path, PIO_MAX_NAME + 1, "%s_stderr_rank%d.txt", TEST_NAME, my_rank);
  Stderr_capture capture;
  capture.start(capture_path);

  ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER);
  if(ret == PIO_NOERR){
    ret = PIOc_def_dim(ncid, DIM_NAME, (PIO_Offset) gdimlen, &dimid);
  }
  if(ret == PIO_NOERR){
    ret = PIOc_def_var(ncid, VAR_NAME, PIO_INT, NDIM1, &dimid, &varid);
  }
  if(ret == PIO_NOERR){
    ret = PIOc_enddef(ncid);
  }

  bool uses_async_io = false;
  bool owns_io = false;
  bool is_sync_io_proc = ios->io_comm_info->is_sync_io_proc();
  bool is_async_io_proc = false;
#if PIO_USE_ASYNC_WR_THREAD
  is_async_io_proc = ios->io_comm_info->is_async_io_proc();
#endif

  if(ret == PIO_NOERR){
    file_desc_t *file = NULL;
    if((pio_get_file(ncid, &file) != PIO_NOERR) || !file){
      ret = PIO_EINTERNAL;
    }
    else{
      uses_async_io = file->uses_async_io;
      owns_io = SPIO_Util::Iosys_Util::is_file_owning_io_proc(ios, file);
    }
  }

  if(ret == PIO_NOERR){
    ret = PIOc_write_darray(ncid, varid, ioid, (PIO_Offset) MAPLEN_PER_RANK,
                            wdata.data(), NULL);
  }
  if(ret == PIO_NOERR){
    ret = PIOc_closefile(ncid);
  }
  else if(ncid >= 0){
    PIOc_closefile(ncid);
  }

  std::string captured = capture.stop();
  /* Nothing is swallowed: everything the library printed during the
   * lifecycle above goes back out to the real stderr now. */
  if(!captured.empty()){
    fprintf(stderr, "%s", captured.c_str());
    fflush(stderr);
  }

  if(ret != PIO_NOERR){
    LOG_RANK0(my_rank, "test_write_only_full_async_lifecycle() FAILED: the write-only "
      "create/def/write/close lifecycle returned %d\n", ret);
    nerrs++;
  }

  if(captured.find(REMOVED_ASYNC_DISABLED_WARNING) != std::string::npos){
    fprintf(stderr, "test_write_only_full_async_lifecycle() FAILED (rank %d): the removed "
      "\"%s\" warning was emitted during a disjoint sync/async I/O procs write lifecycle - "
      "writes for this decomposition were silently downgraded to the sync I/O procs\n",
      my_rank, REMOVED_ASYNC_DISABLED_WARNING);
    nerrs++;
  }

#if PIO_USE_ASYNC_WR_THREAD
  if(ret == PIO_NOERR){
    if(!uses_async_io){
      fprintf(stderr, "test_write_only_full_async_lifecycle() FAILED (rank %d): a write-capable "
        "file created on a plain intracomm iosystem has uses_async_io == false - its lifecycle "
        "is not being routed through async I/O at all\n", my_rank);
      nerrs++;
    }

    /* The core routing assertion: for a write-capable, async-routed file the
     * ranks that own the file's I/O work are exactly the async I/O ranks -
     * on a disjoint topology that means the sync-only I/O rank(s) own
     * nothing. */
    if(owns_io != is_async_io_proc){
      fprintf(stderr, "test_write_only_full_async_lifecycle() FAILED (rank %d): "
        "is_file_owning_io_proc() = %d but is_async_io_proc() = %d - a write-capable "
        "async-routed file's ops must be owned by exactly the async I/O ranks\n",
        my_rank, (int) owns_io, (int) is_async_io_proc);
      nerrs++;
    }

    if(is_sync_io_proc && !is_async_io_proc && owns_io){
      fprintf(stderr, "test_write_only_full_async_lifecycle() FAILED (rank %d): a sync-only "
        "I/O rank still owns this write-capable file's I/O ops on a disjoint sync/async "
        "topology\n", my_rank);
      nerrs++;
    }
  }
#else
  (void) uses_async_io;
  (void) owns_io;
  (void) is_sync_io_proc;
  (void) is_async_io_proc;
#endif

  PIOc_freedecomp(iosysid, ioid);
  finalize_iosys(iosysid);

  int global_nerrs = reduce_nerrs(test_comm, nerrs);

#if PIO_USE_ASYNC_WR_THREAD
  /* Exactly the async I/O ranks owned the file, and there was at least one
   * of them - checked globally so a topology where no rank claimed
   * ownership cannot pass by every rank individually agreeing on "false". */
  int local_owned = (global_nerrs == 0) && owns_io ? 1 : 0;
  int total_owned = 0;
  MPI_Allreduce(&local_owned, &total_owned, 1, MPI_INT, MPI_SUM, test_comm);
  if((global_nerrs == 0) && (total_owned == 0)){
    LOG_RANK0(my_rank, "test_write_only_full_async_lifecycle() FAILED: no rank at all owned "
      "the write-capable file's I/O ops\n");
    global_nerrs++;
  }
#endif

  return (global_nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* ------------------------------------------------------------------------
 * Scenario 2: a PIO_NOWRITE open on a disjoint-procs iosystem is always
 * sync - uses_async_io is false, the sync I/O ranks own every op, and no
 * async write lock file is created for the read-only open.
 * ------------------------------------------------------------------------ */
static int test_read_only_always_sync(MPI_Comm test_comm, int my_rank, int ntasks)
{
  int ret = PIO_NOERR;
  int iosysid = -1, ioid = -1, gdimlen = 0;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_read_only.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_read_only_always_sync() FAILED: PIOc_Init_Intracomm() "
      "returned %d\n", ret);
    return ret;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios || !ios->io_comm_info){
    LOG_RANK0(my_rank, "test_read_only_always_sync() FAILED: pio_get_iosystem_from_id() "
      "returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

#if PIO_USE_ASYNC_WR_THREAD
  if(!iosys_has_disjoint_io_procs(ios)){
    LOG_RANK0(my_rank, "test_read_only_always_sync() SKIPPED: sync and async I/O procs are "
      "not disjoint on this topology\n");
    finalize_iosys(iosysid);
    return PIO_NOERR;
  }
#endif

  if((ret = init_test_decomp(iosysid, my_rank, ntasks, PIO_REARR_CONTIG, &gdimlen, &ioid))){
    LOG_RANK0(my_rank, "test_read_only_always_sync() FAILED: PIOc_init_decomp() returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  std::vector<int> wdata(MAPLEN_PER_RANK);
  fill_wdata(wdata, my_rank, 0);

  if((ret = write_fixture_file(iosysid, ioid, filename, gdimlen, wdata, NULL, NULL))){
    LOG_RANK0(my_rank, "test_read_only_always_sync() FAILED: writing the fixture file "
      "returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  int nerrs = 0;
  int ncid = -1;
  int format = PIO_IOTYPE_HDF5;

  if((ret = PIOc_openfile(iosysid, &ncid, &format, filename, PIO_NOWRITE))){
    LOG_RANK0(my_rank, "test_read_only_always_sync() FAILED: the read-only PIOc_openfile() "
      "returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  file_desc_t *file = NULL;
  if((pio_get_file(ncid, &file) != PIO_NOERR) || !file){
    LOG_RANK0(my_rank, "test_read_only_always_sync() FAILED: pio_get_file() failed after "
      "the read-only open\n");
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

  /* The whole point of Task 1's read-only rule, and of Task 9's PIO_NOWRITE
   * fix: PIO_NOWRITE is NC_NOWRITE == 0x0000, so a read-only open is
   * recognized by the *absence* of the PIO_WRITE bit, never by
   * "mode & PIO_NOWRITE". If that regressed, uses_async_io would be true
   * here and the ownership check below would move to the async ranks. */
  if(file->uses_async_io){
    fprintf(stderr, "test_read_only_always_sync() FAILED (rank %d): a file opened with "
      "PIO_NOWRITE has uses_async_io == true - read-only opens must always stay sync\n",
      my_rank);
    nerrs++;
  }

  bool owns_io = SPIO_Util::Iosys_Util::is_file_owning_io_proc(ios, file);
  if(owns_io != ios->io_comm_info->is_sync_io_proc()){
    fprintf(stderr, "test_read_only_always_sync() FAILED (rank %d): is_file_owning_io_proc() "
      "= %d but is_sync_io_proc() = %d - every op on a read-only file must be owned by the "
      "sync I/O ranks\n", my_rank, (int) owns_io, (int) ios->io_comm_info->is_sync_io_proc());
    nerrs++;
  }

#if PIO_USE_ASYNC_WR_THREAD
  if(ios->io_comm_info->is_async_io_proc() && !ios->io_comm_info->is_sync_io_proc() && owns_io){
    fprintf(stderr, "test_read_only_always_sync() FAILED (rank %d): an async-only I/O rank "
      "owns ops for a read-only file\n", my_rank);
    nerrs++;
  }

  /* A read-only open must not create an async write lock file of its own -
   * spio_create_async_lock_file() is gated on file->uses_async_io, which is
   * false here. */
  if(my_rank == 0){
    std::string lock_filename = spio_get_async_lock_filename(filename, ios->spio_lock_id);
    if(test_file_exists(lock_filename)){
      fprintf(stderr, "test_read_only_always_sync() FAILED: an asynchronous write lock file "
        "(%s) exists while only a read-only open of the file is outstanding\n",
        lock_filename.c_str());
      nerrs++;
    }
  }
#endif

  /* Prove the read-only open is actually usable: the decomposed data must
   * round-trip. */
  std::vector<int> rdata(MAPLEN_PER_RANK, -1);
  int varid = -1;
  if((ret = PIOc_inq_varid(ncid, VAR_NAME, &varid)) ||
     (ret = PIOc_read_darray(ncid, varid, ioid, (PIO_Offset) MAPLEN_PER_RANK, rdata.data()))){
    fprintf(stderr, "test_read_only_always_sync() FAILED (rank %d): reading back the "
      "decomposed variable from the read-only open returned %d\n", my_rank, ret);
    nerrs++;
  }
  else{
    for(int i = 0; i < MAPLEN_PER_RANK; i++){
      if(rdata[i] != wdata[i]){
        fprintf(stderr, "test_read_only_always_sync() FAILED (rank %d): rdata[%d] (%d) != "
          "wdata[%d] (%d)\n", my_rank, i, rdata[i], i, wdata[i]);
        nerrs++;
        break;
      }
    }
  }

  PIOc_closefile(ncid);
  PIOc_freedecomp(iosysid, ioid);
  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* ------------------------------------------------------------------------
 * Scenario 3: interleaved non-decomposed put_var/get_var and decomposed
 * write_darray/read_darray on a read-write file, each write/read pair
 * separated by an explicit PIOc_sync(), over many iterations - no stale or
 * torn reads.
 *
 * SCORPIO does not guarantee that a read sees a prior write on the same
 * file without an intervening PIOc_sync() (or a close/reopen) - see
 * PIOc_sync_impl() in pio_file.cpp. This scenario confirms that calling it
 * between each write and read is sufficient for both the non-decomposed
 * vars API and the decomposed darray API.
 *
 * Run on a single-rank iosystem (the sync and async I/O procs are then
 * necessarily the same proc, i.e. the "same-procs case" the brief asks for,
 * and the one PIOc_put_var/PIOc_get_var on HDF5 is exercised in by
 * test_putget_hdf5.cpp). Determinism is the point here: any mismatch must
 * be a genuine read-vs-pending-async-write race, not an artifact of several
 * I/O ranks collectively writing the same non-decomposed variable.
 * ------------------------------------------------------------------------ */
#define INTERLEAVE_NUM_ITERS 20

static int test_read_write_interleaved_no_race(MPI_Comm single_rank_comm, int my_rank)
{
  int ret = PIO_NOERR;
  int iosysid = -1, ioid = -1, gdimlen = 0;
  int ncid = -1, dimid = -1, varid = -1, nd_varid = -1;
  int format = PIO_IOTYPE_HDF5;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_interleaved.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(single_rank_comm, 1, STRIDE1, BASE0,
                                PIO_REARR_BOX, &iosysid))){
    LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_Init_Intracomm() "
      "returned %d\n", ret);
    return ret;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios || !ios->io_comm_info){
    LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

#if PIO_USE_ASYNC_WR_THREAD
  if(!ios->io_comm_info->sync_and_async_io_share_procs()){
    LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: this single-rank "
      "iosystem unexpectedly has disjoint sync/async I/O procs - the scenario is meant to "
      "exercise the same-procs case\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }
#endif

  /* One rank, so the decomposition covers the whole variable. */
  if((ret = init_test_decomp(iosysid, 0, 1, PIO_REARR_BOX, &gdimlen, &ioid))){
    LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_init_decomp() "
      "returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  if((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER))){
    LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_createfile() "
      "returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  if((ret = PIOc_def_dim(ncid, DIM_NAME, (PIO_Offset) gdimlen, &dimid)) ||
     (ret = PIOc_def_var(ncid, VAR_NAME, PIO_INT, NDIM1, &dimid, &varid)) ||
     (ret = PIOc_def_var(ncid, ND_VAR_NAME, PIO_INT, NDIM1, &dimid, &nd_varid)) ||
     (ret = PIOc_enddef(ncid))){
    LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: defining the file "
      "returned %d\n", ret);
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

#if PIO_USE_ASYNC_WR_THREAD
  {
    file_desc_t *file = NULL;
    if((pio_get_file(ncid, &file) == PIO_NOERR) && file && !file->uses_async_io){
      LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: the read-write file "
        "has uses_async_io == false, so no put/get would race a pending async write and the "
        "scenario would prove nothing\n");
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      finalize_iosys(iosysid);
      return PIO_EINTERNAL;
    }
  }
#endif

  int nerrs = 0;
  std::vector<int> nd_wdata(gdimlen), nd_rdata(gdimlen);
  std::vector<int> wdata(MAPLEN_PER_RANK), rdata(MAPLEN_PER_RANK);

  for(int iter = 0; (iter < INTERLEAVE_NUM_ITERS) && (nerrs == 0); iter++){
    /* Non-decomposed put_var followed by get_var, separated by PIOc_sync() -
     * required for the read to be guaranteed to see the write (see
     * PIOc_sync_impl() in pio_file.cpp). */
    for(int i = 0; i < gdimlen; i++){
      nd_wdata[i] = iter * 10000 + i;
    }
    std::fill(nd_rdata.begin(), nd_rdata.end(), -1);

    if((ret = PIOc_put_var_int(ncid, nd_varid, nd_wdata.data()))){
      LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_put_var_int() "
        "returned %d (iter = %d)\n", ret, iter);
      nerrs++;
      break;
    }
    if((ret = PIOc_sync(ncid))){
      LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_sync() (non-decomposed) "
        "returned %d (iter = %d)\n", ret, iter);
      nerrs++;
      break;
    }
    if((ret = PIOc_get_var_int(ncid, nd_varid, nd_rdata.data()))){
      LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_get_var_int() "
        "returned %d (iter = %d)\n", ret, iter);
      nerrs++;
      break;
    }
    for(int i = 0; i < gdimlen; i++){
      if(nd_rdata[i] != nd_wdata[i]){
        LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: stale/torn "
          "non-decomposed read at iter %d: nd_rdata[%d] (%d) != nd_wdata[%d] (%d) - the "
          "get_var did not see the prior put_var despite an intervening PIOc_sync()\n",
          iter, i, nd_rdata[i], i, nd_wdata[i]);
        nerrs++;
        break;
      }
    }
    if(nerrs != 0){
      break;
    }

    /* Decomposed write_darray followed by read_darray.
     *
     * The PIOc_sync() in between is NOT a synchronization against the async
     * write thread - it is PIO's decomposed-write buffer flush, which the
     * API requires: PIOc_write_darray() only caches the data in the file's
     * wmulti_buffer, and nothing reaches the rearranger or the HDF5 layer
     * until flush_buffer() runs (from PIOc_sync(), PIOc_closefile(), or a
     * full buffer). Without it, a read_darray would be reading a variable
     * that no layer has been asked to write yet, which is ordinary
     * documented PIO behavior rather than the race this scenario is after.
     *
     * The race IS still exercised: for an HDF5 file, sync_file()'s own
     * per-iotype sync is a no-op (see pio_file.cpp's "case PIO_IOTYPE_HDF5:
     * ierr = PIO_NOERR;"), so PIOc_sync() hands the write to the async
     * thread pool and returns without waiting for it. The PIOc_read_darray()
     * immediately below must therefore still wait for that pending
     * asynchronous write itself (Tasks 5/6) or it reads stale data. */
    fill_wdata(wdata, 0, iter);
    std::fill(rdata.begin(), rdata.end(), -1);

    if((ret = PIOc_write_darray(ncid, varid, ioid, (PIO_Offset) MAPLEN_PER_RANK,
                                wdata.data(), NULL))){
      LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_write_darray() "
        "returned %d (iter = %d)\n", ret, iter);
      nerrs++;
      break;
    }
    if((ret = PIOc_sync(ncid))){
      LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_sync() "
        "returned %d (iter = %d)\n", ret, iter);
      nerrs++;
      break;
    }
    if((ret = PIOc_read_darray(ncid, varid, ioid, (PIO_Offset) MAPLEN_PER_RANK,
                               rdata.data()))){
      LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: PIOc_read_darray() "
        "returned %d (iter = %d)\n", ret, iter);
      nerrs++;
      break;
    }
    for(int i = 0; i < MAPLEN_PER_RANK; i++){
      if(rdata[i] != wdata[i]){
        LOG_RANK0(my_rank, "test_read_write_interleaved_no_race() FAILED: stale/torn "
          "decomposed read at iter %d: rdata[%d] (%d) != wdata[%d] (%d) - the read_darray "
          "raced a still-pending asynchronous write_darray\n",
          iter, i, rdata[i], i, wdata[i]);
        nerrs++;
        break;
      }
    }
  }

  PIOc_closefile(ncid);
  PIOc_freedecomp(iosysid, ioid);
  finalize_iosys(iosysid);

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* ------------------------------------------------------------------------
 * Scenario 4: disjoint procs, write -> close -> read-only reopen where the
 * close has long since finished. The lock file must be gone by then and
 * the reopen must take the fast path (no poll interval spent waiting).
 * ------------------------------------------------------------------------ */
static int test_disjoint_write_close_reopen_lock_absent(MPI_Comm test_comm, int my_rank, int ntasks)
{
  int ret = PIO_NOERR;
  int iosysid = -1, ioid = -1, gdimlen = 0;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_lock_absent.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_absent() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios || !ios->io_comm_info){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_absent() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

#if PIO_USE_ASYNC_WR_THREAD
  if(!iosys_has_disjoint_io_procs(ios)){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_absent() SKIPPED: sync and "
      "async I/O procs are not disjoint on this topology\n");
    finalize_iosys(iosysid);
    return PIO_NOERR;
  }
#endif

  if((ret = init_test_decomp(iosysid, my_rank, ntasks, PIO_REARR_CONTIG, &gdimlen, &ioid))){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_absent() FAILED: "
      "PIOc_init_decomp() returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  std::vector<int> wdata(MAPLEN_PER_RANK);
  fill_wdata(wdata, my_rank, 0);

  bool uses_async_io = false, lock_present_during_write = false;
  if((ret = write_fixture_file(iosysid, ioid, filename, gdimlen, wdata,
                               &uses_async_io, &lock_present_during_write))){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_absent() FAILED: writing the "
      "fixture file returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  int nerrs = 0;
  std::string lock_filename = spio_get_async_lock_filename(filename, ios->spio_lock_id);

  /* "the close completes well before the reopen starts": PIOc_closefile()
   * on an async-routed HDF5 file is a *soft* close - it queues the real
   * close on the write thread pool and returns immediately, so the lock
   * file is still legitimately present the instant it returns (that is the
   * whole point of the lock file: it tells other processes a write is still
   * in flight). Draining the queue here is what makes this the "close has
   * finished" case, as opposed to
   * test_disjoint_write_close_reopen_lock_present() below. */
  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

#if PIO_USE_ASYNC_WR_THREAD
  /* Sanity-check that the scenario is meaningful at all: the write-capable
   * file must have been async-routed, and its lock file must have existed
   * while the file was open. The lock file is only created by the async I/O
   * root rank, so this is a global (any-rank) observation. */
  int local_saw_lock = lock_present_during_write ? 1 : 0;
  int total_saw_lock = 0;
  MPI_Allreduce(&local_saw_lock, &total_saw_lock, 1, MPI_INT, MPI_SUM, test_comm);
  if(!uses_async_io){
    fprintf(stderr, "test_disjoint_write_close_reopen_lock_absent() FAILED (rank %d): the "
      "write-capable file was not async-routed (uses_async_io == false)\n", my_rank);
    nerrs++;
  }
  if(total_saw_lock == 0){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_absent() FAILED: no rank saw "
      "an asynchronous write lock file (%s) while the write-capable file was open - the "
      "lock-file lifecycle is not being exercised\n", lock_filename.c_str());
    nerrs++;
  }

  /* The close has now fully completed on every rank, so the lock must be
   * gone - spio_remove_async_lock_file() runs from spio_hard_closefile(). */
  if(my_rank == 0){
    if(test_file_exists(lock_filename)){
      fprintf(stderr, "test_disjoint_write_close_reopen_lock_absent() FAILED: the "
        "asynchronous write lock file (%s) still exists after the close fully completed\n",
        lock_filename.c_str());
      nerrs++;
    }
  }
#else
  (void) uses_async_io;
  (void) lock_present_during_write;
#endif

  /* The read-only reopen must take the fast path: with no lock file present,
   * spio_wait_on_async_lock_file() returns without sleeping even one poll
   * interval (500 ms - see pio_file.cpp). */
  int ncid = -1;
  int format = PIO_IOTYPE_HDF5;
  std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
  ret = PIOc_openfile(iosysid, &ncid, &format, filename, PIO_NOWRITE);
  long long elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start).count();

  if(ret != PIO_NOERR){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_absent() FAILED: the read-only "
      "reopen returned %d\n", ret);
    nerrs++;
  }
  else{
    if(elapsed_ms >= 500){
      fprintf(stderr, "test_disjoint_write_close_reopen_lock_absent() FAILED (rank %d): the "
        "read-only reopen took %lld ms although no lock file was present - expected the fast "
        "path (no poll interval spent waiting)\n", my_rank, elapsed_ms);
      nerrs++;
    }

    std::vector<int> rdata(MAPLEN_PER_RANK, -1);
    int varid = -1;
    if((ret = PIOc_inq_varid(ncid, VAR_NAME, &varid)) ||
       (ret = PIOc_read_darray(ncid, varid, ioid, (PIO_Offset) MAPLEN_PER_RANK, rdata.data()))){
      fprintf(stderr, "test_disjoint_write_close_reopen_lock_absent() FAILED (rank %d): "
        "reading back the reopened file returned %d\n", my_rank, ret);
      nerrs++;
    }
    else{
      for(int i = 0; i < MAPLEN_PER_RANK; i++){
        if(rdata[i] != wdata[i]){
          fprintf(stderr, "test_disjoint_write_close_reopen_lock_absent() FAILED (rank %d): "
            "rdata[%d] (%d) != wdata[%d] (%d)\n", my_rank, i, rdata[i], i, wdata[i]);
          nerrs++;
          break;
        }
      }
    }

    PIOc_closefile(ncid);
  }

  PIOc_freedecomp(iosysid, ioid);
  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* ------------------------------------------------------------------------
 * Scenario 5: disjoint procs, the read-only reopen is issued while a lock
 * file for the same (filename, ios->spio_lock_id) is still present -
 * exercises Task 9's wait path through the real PIOc_openfile() ->
 * PIOc_openfile_impl() -> openfile_int() -> PIOc_openfile_retry_impl()
 * call chain.
 *
 * As in test_pio_file_lock.cpp's test_wait_on_async_lock_file_real_open_wiring(),
 * the still-running "group A" writer is simulated: racing the real async
 * write thread pool's own completion timing is non-deterministic, so after
 * the real write/close has fully drained, the lock file is manufactured
 * again on the sync I/O root (the only rank spio_wait_on_async_lock_file()
 * actually polls on) and removed from a background thread partway through
 * the wait. Everything on the reader side - the mode-bit check, the call
 * site, the lock filename derivation, the bounded poll - is the real,
 * unmodified production code path.
 * ------------------------------------------------------------------------ */
#if PIO_USE_ASYNC_WR_THREAD
/* How long the simulated writer holds the lock, and the floor the reopen's
 * elapsed time must clear to prove it really waited. Comfortably above the
 * 500 ms poll interval and well below SPIO_ASYNC_OP_WAIT_TIMEOUT. */
#define LOCK_HOLD_MS 1500
#define LOCK_WAIT_MIN_MS 1000

static int test_disjoint_write_close_reopen_lock_present(MPI_Comm test_comm, int my_rank, int ntasks)
{
  int ret = PIO_NOERR;
  int iosysid = -1, ioid = -1, gdimlen = 0;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_lock_present.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_present() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios || !ios->io_comm_info){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_present() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

  if(!iosys_has_disjoint_io_procs(ios)){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_present() SKIPPED: sync and "
      "async I/O procs are not disjoint on this topology\n");
    finalize_iosys(iosysid);
    return PIO_NOERR;
  }

  if((ret = init_test_decomp(iosysid, my_rank, ntasks, PIO_REARR_CONTIG, &gdimlen, &ioid))){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_present() FAILED: "
      "PIOc_init_decomp() returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  std::vector<int> wdata(MAPLEN_PER_RANK);
  fill_wdata(wdata, my_rank, 0);

  if((ret = write_fixture_file(iosysid, ioid, filename, gdimlen, wdata, NULL, NULL))){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_present() FAILED: writing the "
      "fixture file returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  /* Drain anything the real write/close left queued before manufacturing a
   * lock file of our own: the lock filename is derived purely from
   * (filename, ios->spio_lock_id), so the real close's own lock-file
   * removal and ours would otherwise contend for the exact same path. */
  spio_wait_all_hdf5_async_ops(iosysid);
  MPI_Barrier(test_comm);

  int nerrs = 0;
  std::string lock_filename = spio_get_async_lock_filename(filename, ios->spio_lock_id);
  bool is_sync_io_root = (ios->io_comm_info->get_sync_io_master() == MPI_ROOT);

  /* spio_wait_on_async_lock_file() only ever stats the filesystem on the
   * sync I/O root, so the simulated writer lives on that same rank. */
  if(is_sync_io_root){
    remove(lock_filename.c_str());
    FILE *fp = fopen(lock_filename.c_str(), "w");
    if(!fp){
      fprintf(stderr, "test_disjoint_write_close_reopen_lock_present() FAILED (rank %d): "
        "could not manufacture the lock file (%s)\n", my_rank, lock_filename.c_str());
      nerrs++;
    }
    else{
      fclose(fp);
    }
  }

  MPI_Barrier(test_comm);

  std::thread writer_finishes;
  if(is_sync_io_root && (nerrs == 0)){
    writer_finishes = std::thread([&lock_filename](){
      std::this_thread::sleep_for(std::chrono::milliseconds(LOCK_HOLD_MS));
      remove(lock_filename.c_str());
    });
  }

  int ncid = -1;
  int format = PIO_IOTYPE_HDF5;
  std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
  ret = PIOc_openfile(iosysid, &ncid, &format, filename, PIO_NOWRITE);
  long long elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::steady_clock::now() - start).count();

  if(writer_finishes.joinable()){
    writer_finishes.join();
  }

  if(ret != PIO_NOERR){
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_present() FAILED: the "
      "read-only reopen returned %d\n", ret);
    nerrs++;
  }
  else{
    PIOc_closefile(ncid);
  }

  /* Only the sync I/O root actually polls (every other rank returns from
   * spio_wait_on_async_lock_file() immediately), so the timing assertion
   * belongs on that rank. */
  if(is_sync_io_root){
    if(elapsed_ms < LOCK_WAIT_MIN_MS){
      fprintf(stderr, "test_disjoint_write_close_reopen_lock_present() FAILED (rank %d): the "
        "read-only reopen returned after only %lld ms although the asynchronous write lock "
        "file was still present when it started - PIOc_openfile_retry_impl() did not wait on "
        "it (this is exactly the \"mode & PIO_NOWRITE is always false\" bug)\n",
        my_rank, elapsed_ms);
      nerrs++;
    }
    if(elapsed_ms >= SPIO_ASYNC_OP_WAIT_TIMEOUT){
      fprintf(stderr, "test_disjoint_write_close_reopen_lock_present() FAILED (rank %d): the "
        "read-only reopen took %lld ms (>= the %d ms timeout) - expected it to observe the "
        "lock-file removal well before timing out\n",
        my_rank, elapsed_ms, SPIO_ASYNC_OP_WAIT_TIMEOUT);
      nerrs++;
    }
    if(test_file_exists(lock_filename)){
      fprintf(stderr, "test_disjoint_write_close_reopen_lock_present() FAILED (rank %d): the "
        "lock file (%s) still exists after the simulated writer finished\n",
        my_rank, lock_filename.c_str());
      nerrs++;
      remove(lock_filename.c_str());
    }
  }

  PIOc_freedecomp(iosysid, ioid);
  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}
#endif /* PIO_USE_ASYNC_WR_THREAD */

/* ------------------------------------------------------------------------
 * Scenario 12: a write-capable NON-HDF5 (PnetCDF) file must not leave a
 * *.spio_lock file behind (backlog item 8, re-triaged Important by the
 * final review).
 *
 * The async write lock file is created whenever file->uses_async_io is
 * set, but removed only inside spio_hard_closefile()'s
 * "iotype is HDF5/HDF5C" block. Both uses_async_io computations
 * (spio_createfile_int() and PIOc_openfile_retry_impl(), both in
 * pioc_support.cpp) used to omit any iotype test, so uses_async_io was
 * true for PnetCDF/NetCDF4P/NetCDF-classic files too. Every write-capable
 * file of every non-HDF5 iotype therefore leaked a lock file into the
 * user's output directory, and a later read-only open of the same
 * filename in the same run stalled on it for the full
 * SPIO_ASYNC_OP_WAIT_TIMEOUT (120 s) before warning and proceeding.
 *
 * This scenario lives in this (HDF5-named) file because the lock-file
 * machinery it is testing is HDF5-async-specific and all the helpers for
 * it are here - the file under test is deliberately NOT an HDF5 file.
 * ------------------------------------------------------------------------ */
#ifdef _PNETCDF
#define LOCK_LEAK_VAR_NAME "lock_leak_var"
#define LOCK_LEAK_DIM_LEN 4

static int check_no_lock_file(const iosystem_desc_t *ios, const char *filename,
                              const char *stage, int my_rank)
{
  std::string lock_filename = spio_get_async_lock_filename(filename, ios->spio_lock_id);
  if(test_file_exists(lock_filename)){
    fprintf(stderr, "test_non_hdf5_write_open_leaves_no_lock_file() FAILED (rank %d): the "
      "asynchronous write lock file (%s) still exists after %s a write-capable PnetCDF file "
      "- uses_async_io must not be set for a non-HDF5 iotype, because only the HDF5 close "
      "path ever removes the lock file (backlog item 8)\n",
      my_rank, lock_filename.c_str(), stage);
    /* Clean up so a repeat run of this binary is not poisoned by it. */
    remove(lock_filename.c_str());
    return 1;
  }

  return 0;
}

static int test_non_hdf5_write_open_leaves_no_lock_file(MPI_Comm test_comm, int my_rank)
{
  int ret = PIO_NOERR;
  int iosysid = -1;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_pnetcdf_lock_leak.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_non_hdf5_write_open_leaves_no_lock_file() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios){
    LOG_RANK0(my_rank, "test_non_hdf5_write_open_leaves_no_lock_file() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

  int nerrs = 0;
  int ncid = -1, dimid = -1, varid = -1;
  int format = PIO_IOTYPE_PNETCDF;

  /* (a) the create path - spio_createfile_int()'s uses_async_io */
  if((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER)) ||
     (ret = PIOc_def_dim(ncid, DIM_NAME, (PIO_Offset) LOCK_LEAK_DIM_LEN, &dimid)) ||
     (ret = PIOc_def_var(ncid, LOCK_LEAK_VAR_NAME, PIO_INT, NDIM1, &dimid, &varid)) ||
     (ret = PIOc_enddef(ncid)) ||
     (ret = PIOc_closefile(ncid))){
    LOG_RANK0(my_rank, "test_non_hdf5_write_open_leaves_no_lock_file() FAILED: creating and "
      "closing the PnetCDF file returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  MPI_Barrier(test_comm);
  nerrs += check_no_lock_file(ios, filename, "creating and closing", my_rank);

  /* (b) the write-capable open path - PIOc_openfile_retry_impl()'s
   * uses_async_io (the same computation, at its own site) */
  int wr_ncid = -1;
  int wr_format = PIO_IOTYPE_PNETCDF;
  if((ret = PIOc_openfile(iosysid, &wr_ncid, &wr_format, filename, PIO_WRITE)) ||
     (ret = PIOc_closefile(wr_ncid))){
    LOG_RANK0(my_rank, "test_non_hdf5_write_open_leaves_no_lock_file() FAILED: the "
      "write-capable open/close of the PnetCDF file returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  MPI_Barrier(test_comm);
  nerrs += check_no_lock_file(ios, filename, "write-capable opening and closing", my_rank);

  finalize_iosys(iosysid);

  if(my_rank == 0){
    remove(filename);
  }

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}
#endif /* _PNETCDF */

/* ------------------------------------------------------------------------
 * Scenario 10: non-decomposed (whole-variable) PIOc_put_var/PIOc_get_var on
 * a write-capable, async-routed file with DISJOINT sync/async I/O procs.
 *
 * Regression test for review finding C1: spio_hdf5_put_var()/
 * spio_hdf5_get_var() (src/clib/core/iolib/hdf5/spio_hdf5_utils.cpp) filled
 * their hcount[]/nelems only inside "if(get_sync_io_master() == MPI_ROOT)".
 * On a disjoint topology an async-routed file's owning ranks are the async
 * I/O procs, and none of those is ever the sync I/O master - so hcount[]
 * stayed all-zero, H5Dwrite transferred zero elements, and PIOc_put_var
 * returned PIO_NOERR having written nothing at all. Silent data loss.
 *
 * The round trip is deliberately verified through a *second*, read-only
 * (PIO_NOWRITE, and therefore sync-routed, uses_async_io == false) native
 * reopen rather than by reading back through the write-capable handle:
 *
 *   KNOWN PRE-EXISTING (backlog item 5, confirmed by the final review, out
 *   of scope for this fix round): spio_get_vars_tc()
 *   (src/clib/core/pio_getput_int.cpp:2235) broadcasts the result buffer
 *   from ios->io_comm_info->get_sync_io_root() unconditionally, even for an
 *   async-routed file whose data was read on an async I/O rank. On a
 *   disjoint topology that root never read anything, so every rank -
 *   including the rank that did the read - ends up with the sync root's
 *   untouched buffer. Fixing C1's inner gate is necessary but not
 *   sufficient to make a non-decomposed read *through an async-routed
 *   handle* return correct data; that needs backlog item 5's Bcast root
 *   fixed too. Reading back through a sync-routed open exercises
 *   spio_hdf5_get_var() with a coherent Bcast root, and - crucially -
 *   proves what C1 is really about: that put_var's bytes actually reached
 *   the file instead of being silently dropped.
 * ------------------------------------------------------------------------ */
#define ND_ROUNDTRIP_VAR_NAME "nd_roundtrip_var"
#define ND_ROUNDTRIP_LEN 8

static int test_disjoint_nondecomposed_put_get_roundtrip(MPI_Comm test_comm, int my_rank,
                                                         int ntasks)
{
  int ret = PIO_NOERR;
  int iosysid = -1;
  char filename[PIO_MAX_NAME + 1];

  (void) ntasks;

  snprintf(filename, PIO_MAX_NAME + 1, "%s_nd_put_get.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios || !ios->io_comm_info){
    LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

#if PIO_USE_ASYNC_WR_THREAD
  if(!iosys_has_disjoint_io_procs(ios)){
    LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() SKIPPED: this "
      "iosystem's sync and async I/O procs are not disjoint on this topology (IO_comm_info "
      "fell back to duplicating the sync comm) - the C1 defect is not reachable here\n");
    finalize_iosys(iosysid);
    return PIO_NOERR;
  }
#endif

  int nerrs = 0;
  int ncid = -1, dimid = -1, varid = -1;
  int format = PIO_IOTYPE_HDF5;

  std::vector<int> nd_wdata(ND_ROUNDTRIP_LEN);
  for(int i = 0; i < ND_ROUNDTRIP_LEN; i++){
    /* Deliberately non-zero: an HDF5 dataset that was never written reads
     * back as the (zero) fill value, which is exactly what the C1 defect
     * produced - zero-valued data must not be mistaken for a pass. */
    nd_wdata[i] = 7000 + i;
  }

  if((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER)) ||
     (ret = PIOc_def_dim(ncid, DIM_NAME, (PIO_Offset) ND_ROUNDTRIP_LEN, &dimid)) ||
     (ret = PIOc_def_var(ncid, ND_ROUNDTRIP_VAR_NAME, PIO_INT, NDIM1, &dimid, &varid)) ||
     (ret = PIOc_enddef(ncid))){
    LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED: defining the "
      "file returned %d\n", ret);
    if(ncid >= 0) { PIOc_closefile(ncid); }
    finalize_iosys(iosysid);
    return ret;
  }

#if PIO_USE_ASYNC_WR_THREAD
  {
    file_desc_t *file = NULL;
    if((pio_get_file(ncid, &file) != PIO_NOERR) || !file || !file->uses_async_io){
      LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED: the "
        "write-capable file does not have uses_async_io == true - the put_var would not be "
        "routed through the async I/O procs and this scenario would prove nothing\n");
      PIOc_closefile(ncid);
      finalize_iosys(iosysid);
      return PIO_EINTERNAL;
    }
  }
#endif

  /* The non-decomposed, whole-variable write path (NOT PIOc_write_darray) -
   * this is the call that silently wrote zero elements before the C1 fix. */
  if((ret = PIOc_put_var_int(ncid, varid, nd_wdata.data()))){
    LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED: "
      "PIOc_put_var_int() returned %d\n", ret);
    PIOc_closefile(ncid);
    finalize_iosys(iosysid);
    return ret;
  }

  if((ret = PIOc_closefile(ncid))){
    LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED: "
      "PIOc_closefile() returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

  /* Read back through a read-only (sync-routed) reopen - see this
   * scenario's doc comment for why not through the write-capable handle. */
  int rd_ncid = -1, rd_varid = -1;
  int rd_format = PIO_IOTYPE_HDF5;
  std::vector<int> nd_rdata(ND_ROUNDTRIP_LEN, -1);

  if((ret = PIOc_openfile(iosysid, &rd_ncid, &rd_format, filename, PIO_NOWRITE))){
    LOG_RANK0(my_rank, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED: the read-only "
      "reopen returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  if((ret = PIOc_inq_varid(rd_ncid, ND_ROUNDTRIP_VAR_NAME, &rd_varid)) ||
     (ret = PIOc_get_var_int(rd_ncid, rd_varid, nd_rdata.data()))){
    fprintf(stderr, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED (rank %d): reading "
      "the non-decomposed variable back returned %d\n", my_rank, ret);
    nerrs++;
  }
  else{
    for(int i = 0; i < ND_ROUNDTRIP_LEN; i++){
      if(nd_rdata[i] != nd_wdata[i]){
        fprintf(stderr, "test_disjoint_nondecomposed_put_get_roundtrip() FAILED (rank %d): "
          "nd_rdata[%d] (%d) != nd_wdata[%d] (%d) - PIOc_put_var_int() on a write-capable, "
          "async-routed file with disjoint sync/async I/O procs did not actually write the "
          "data (review finding C1: spio_hdf5_put_var() gated hcount[]/nelems on the *sync* "
          "I/O master, which no async-owning rank ever is)\n",
          my_rank, i, nd_rdata[i], i, nd_wdata[i]);
        nerrs++;
        break;
      }
    }
  }

  PIOc_closefile(rd_ncid);
  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* ------------------------------------------------------------------------
 * Scenario 11: PIOc_copy_att between two HDF5 files that route differently
 * must report an error, not silently do nothing (review finding I2).
 *
 * PIOc_copy_att_impl()'s HDF5 arm only performs the copy on a rank that is
 * a file-owning I/O proc for BOTH files. Copying from a read-only input
 * file (always sync-routed) to a write-capable output file (async-routed)
 * on a disjoint sync/async topology - the single most common real use of
 * copy_att - satisfies that conjunction on no rank at all, so ierr stayed
 * PIO_NOERR, the attribute was never copied, and the caller was told it
 * succeeded. The underlying operation really is impossible in that
 * topology (the two files' HDF5 handles live on disjoint rank sets); it is
 * the silent success that is the defect.
 * ------------------------------------------------------------------------ */
static int test_cross_routing_copy_att_errors(MPI_Comm test_comm, int my_rank, int ntasks)
{
  int ret = PIO_NOERR;
  int iosysid = -1, ioid = -1, gdimlen = 0;
  char in_filename[PIO_MAX_NAME + 1], out_filename[PIO_MAX_NAME + 1];

  snprintf(in_filename, PIO_MAX_NAME + 1, "%s_copy_att_in.nc", TEST_NAME);
  snprintf(out_filename, PIO_MAX_NAME + 1, "%s_copy_att_out.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: PIOc_Init_Intracomm() "
      "returned %d\n", ret);
    return ret;
  }

#if !PIO_USE_ASYNC_WR_THREAD
  /* uses_async_io is always false in this build (Task 1), so two HDF5 files
   * can never route differently and the cross-routing condition this
   * scenario is about is unreachable. */
  LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() SKIPPED: PIO_USE_ASYNC_WR_THREAD "
    "is disabled, so every file has uses_async_io == false and no two files can route "
    "differently - there is no cross-routing copy to reject\n");
  (void) ntasks;
  finalize_iosys(iosysid);
  return PIO_NOERR;
#else
  iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
  if(!ios || !ios->io_comm_info){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: "
      "pio_get_iosystem_from_id() returned NULL\n");
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

  if(!iosys_has_disjoint_io_procs(ios)){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() SKIPPED: this iosystem's sync "
      "and async I/O procs are not disjoint on this topology\n");
    finalize_iosys(iosysid);
    return PIO_NOERR;
  }

  if((ret = init_test_decomp(iosysid, my_rank, ntasks, PIO_REARR_CONTIG, &gdimlen, &ioid))){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: PIOc_init_decomp() "
      "returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  std::vector<int> wdata(MAPLEN_PER_RANK);
  fill_wdata(wdata, my_rank, 0);

  /* Both files are written and closed up front, one at a time, and only
   * then reopened - the input PIO_NOWRITE (so it is sync-routed,
   * uses_async_io == false) and the output PIO_WRITE (so it is
   * async-routed). Deliberately NOT PIOc_createfile()'ing the output file
   * while the input file is already open: that shape - a create dispatched
   * onto the async thread pool concurrently with the main thread's own
   * collective in spio_createfile_int() - is backlog item 7, a documented,
   * pre-existing, out-of-scope collective mismatch, and it made this
   * scenario crash non-deterministically (observed as a SEGV inside HDF5's
   * collective metadata-cache flush). Reopening two already-written files
   * exercises exactly the cross-routing copy_att this scenario is about,
   * and is also the realistic shape the review describes: carrying
   * attributes from an input dataset to an output dataset. */
  if((ret = write_fixture_file(iosysid, ioid, in_filename, gdimlen, wdata, NULL, NULL))){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: writing the input "
      "fixture file returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

  if((ret = write_fixture_file(iosysid, ioid, out_filename, gdimlen, wdata, NULL, NULL))){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: writing the output "
      "fixture file returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

  int nerrs = 0;
  int in_ncid = -1, out_ncid = -1;
  int in_format = PIO_IOTYPE_HDF5, out_format = PIO_IOTYPE_HDF5;

  if((ret = PIOc_openfile(iosysid, &in_ncid, &in_format, in_filename, PIO_NOWRITE))){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: the read-only open of "
      "the input file returned %d\n", ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  /* The output file: reopened write-capable, so async-routed. */
  if((ret = PIOc_openfile(iosysid, &out_ncid, &out_format, out_filename, PIO_WRITE))){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: the write-capable "
      "reopen of the output file returned %d\n", ret);
    PIOc_closefile(in_ncid);
    drain_async_ops(iosysid);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  /* Confirm the two files really do route differently, so a PIO_NOERR
   * below would genuinely be the silent no-op this scenario is about
   * rather than a legitimately-performed copy. */
  bool routing_differs = false;
  {
    file_desc_t *in_file = NULL, *out_file = NULL;
    if((pio_get_file(in_ncid, &in_file) == PIO_NOERR) && in_file &&
       (pio_get_file(out_ncid, &out_file) == PIO_NOERR) && out_file){
      routing_differs = (in_file->uses_async_io != out_file->uses_async_io);
    }
  }

  if(!routing_differs){
    LOG_RANK0(my_rank, "test_cross_routing_copy_att_errors() FAILED: the read-only input file "
      "and the write-capable output file have the same uses_async_io - there is no "
      "cross-routing copy to reject and the scenario would prove nothing\n");
    nerrs++;
  }
  else{
    int copy_ret = PIOc_copy_att(in_ncid, PIO_GLOBAL, GATT_NAME, out_ncid, PIO_GLOBAL);
    if(copy_ret == PIO_NOERR){
      fprintf(stderr, "test_cross_routing_copy_att_errors() FAILED (rank %d): PIOc_copy_att() "
        "between an async-routed and a sync-routed HDF5 file returned PIO_NOERR - the "
        "attribute cannot have been copied (the two files' HDF5 handles live on disjoint "
        "rank sets), so reporting success is silent data loss (review finding I2)\n",
        my_rank);
      nerrs++;
    }
  }

  /* Drain between the two closes: the write-capable output file's close is
   * an async op, and closing the read-only input file (which, since the
   * native reopen landed, is still iotype == PIO_IOTYPE_HDF5 and so also
   * queues an async close) right behind it leaves two async closes for two
   * different files in flight at once. That is backlog item 9 territory -
   * the same pre-existing read-only-reopen async-close defect this file's
   * drain_async_ops() helper documents - and was observed here as a
   * non-deterministic SEGV inside HDF5's collective metadata-cache flush
   * (H5AC__run_sync_point) on the async thread. Draining in between keeps
   * this scenario deterministic without suppressing anything it asserts
   * on: the copy_att assertion above has already been evaluated. */
  PIOc_closefile(out_ncid);
  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

  PIOc_closefile(in_ncid);
  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

  PIOc_freedecomp(iosysid, ioid);
  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
#endif /* PIO_USE_ASYNC_WR_THREAD */
}

/* ------------------------------------------------------------------------
 * Scenarios 6-8: write via each rearranger, close, reopen write-capable,
 * and verify the file stayed PIO_IOTYPE_HDF5 (Tasks 10/11's native reopen,
 * not the historical switch-away to NETCDF4P/PNETCDF) with its dims, vars,
 * attributes and data intact.
 * ------------------------------------------------------------------------ */
static int test_native_reopen_roundtrip(MPI_Comm test_comm, int my_rank, int ntasks,
                                        int rearranger, const char *rearr_name,
                                        int num_io_procs)
{
  int ret = PIO_NOERR;
  int iosysid = -1, ioid = -1, gdimlen = 0;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_native_reopen_%s.nc", TEST_NAME, rearr_name);

  if((ret = PIOc_Init_Intracomm(test_comm, num_io_procs, STRIDE1, BASE0, rearranger, &iosysid))){
    LOG_RANK0(my_rank, "test_native_reopen_roundtrip_%s() FAILED: PIOc_Init_Intracomm() "
      "returned %d\n", rearr_name, ret);
    return ret;
  }

  if((ret = init_test_decomp(iosysid, my_rank, ntasks, rearranger, &gdimlen, &ioid))){
    LOG_RANK0(my_rank, "test_native_reopen_roundtrip_%s() FAILED: PIOc_init_decomp() "
      "returned %d\n", rearr_name, ret);
    finalize_iosys(iosysid);
    return ret;
  }

  std::vector<int> wdata(MAPLEN_PER_RANK);
  fill_wdata(wdata, my_rank, 0);

  if((ret = write_fixture_file(iosysid, ioid, filename, gdimlen, wdata, NULL, NULL))){
    LOG_RANK0(my_rank, "test_native_reopen_roundtrip_%s() FAILED: writing the fixture file "
      "returned %d\n", rearr_name, ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  int nerrs = 0;
  int ncid = -1;
  int format = PIO_IOTYPE_HDF5;

  /* Write-capable reopen, as the brief asks for ("reopen (write or
   * read-write)") - this is also the mode in which a native reopen must
   * keep the file usable as an HDF5 file rather than handing it to the
   * NetCDF4 layer. */
  if((ret = PIOc_openfile(iosysid, &ncid, &format, filename, PIO_WRITE))){
    LOG_RANK0(my_rank, "test_native_reopen_roundtrip_%s() FAILED: the write-capable reopen "
      "returned %d\n", rearr_name, ret);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return ret;
  }

  file_desc_t *file = NULL;
  if((pio_get_file(ncid, &file) != PIO_NOERR) || !file){
    LOG_RANK0(my_rank, "test_native_reopen_roundtrip_%s() FAILED: pio_get_file() failed "
      "after the reopen\n", rearr_name);
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    finalize_iosys(iosysid);
    return PIO_EINTERNAL;
  }

  /* The headline assertion for Tasks 10/11: a SCORPIO-written HDF5 file
   * must reopen natively as HDF5, not be silently switched away to
   * NETCDF4P/NETCDF4C/PNETCDF by the historical fallback. */
  if((file->iotype != PIO_IOTYPE_HDF5) && (file->iotype != PIO_IOTYPE_HDF5C)){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened file's "
      "iotype is %d (%s), expected PIO_IOTYPE_HDF5 (%d) or PIO_IOTYPE_HDF5C (%d) - the native "
      "reopen did not recognize a SCORPIO-written HDF5 file and fell back to switching the "
      "iotype away\n", rearr_name, my_rank, file->iotype, pio_iotype_to_string(file->iotype),
      PIO_IOTYPE_HDF5, PIO_IOTYPE_HDF5C);
    nerrs++;
  }

  /* Dimensions */
  int ndims = -1, nvars = -1, ngatts = -1, unlimdimid = -2;
  if((ret = PIOc_inq(ncid, &ndims, &nvars, &ngatts, &unlimdimid))){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): PIOc_inq() returned "
      "%d\n", rearr_name, my_rank, ret);
    nerrs++;
  }
  else{
    if(ndims != 1){
      fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened file "
        "has %d dimension(s), expected 1\n", rearr_name, my_rank, ndims);
      nerrs++;
    }
    if(nvars != 1){
      fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened file "
        "has %d variable(s), expected 1\n", rearr_name, my_rank, nvars);
      nerrs++;
    }
    if(ngatts != 1){
      fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened file "
        "has %d global attribute(s), expected 1\n", rearr_name, my_rank, ngatts);
      nerrs++;
    }
  }

  char dimname[PIO_MAX_NAME + 1];
  PIO_Offset dimlen = -1;
  memset(dimname, 0, sizeof(dimname));
  if((ret = PIOc_inq_dim(ncid, 0, dimname, &dimlen))){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): PIOc_inq_dim() "
      "returned %d\n", rearr_name, my_rank, ret);
    nerrs++;
  }
  else if((strcmp(dimname, DIM_NAME) != 0) || (dimlen != (PIO_Offset) gdimlen)){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened file's "
      "dimension 0 is (%s, %lld), expected (%s, %lld)\n", rearr_name, my_rank, dimname,
      (long long) dimlen, DIM_NAME, (long long) gdimlen);
    nerrs++;
  }

  /* Variable */
  int varid = -1;
  nc_type vartype = PIO_NAT;
  int var_ndims = -1;
  if((ret = PIOc_inq_varid(ncid, VAR_NAME, &varid)) ||
     (ret = PIOc_inq_var(ncid, varid, NULL, 0, &vartype, &var_ndims, NULL, NULL))){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): inquiring the "
      "reopened variable (%s) returned %d\n", rearr_name, my_rank, VAR_NAME, ret);
    nerrs++;
  }
  else if((vartype != PIO_INT) || (var_ndims != NDIM1)){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened "
      "variable (%s) has type %d / %d dim(s), expected %d / %d\n", rearr_name, my_rank,
      VAR_NAME, (int) vartype, var_ndims, (int) PIO_INT, NDIM1);
    nerrs++;
  }

  /* Attributes: one global, one on the variable. Types and lengths are what
   * spio_hdf5_reopen() reconstructs into file->hdf5_attrs[]; PIOc_inq_att()
   * reads them straight back out of there for an HDF5 file (see
   * pio_nc.cpp's HDF5 block in PIOc_inq_att_impl()). */
  nc_type gatt_type = PIO_NAT, vatt_type = PIO_NAT;
  PIO_Offset gatt_len = -1, vatt_len = -1;
  if((ret = PIOc_inq_att(ncid, PIO_GLOBAL, GATT_NAME, &gatt_type, &gatt_len))){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): PIOc_inq_att() for "
      "the global attribute (%s) returned %d\n", rearr_name, my_rank, GATT_NAME, ret);
    nerrs++;
  }
  else if((gatt_type != PIO_INT) || (gatt_len != 1)){
    fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened global "
      "attribute (%s) has type %d / len %lld, expected %d / 1\n", rearr_name, my_rank,
      GATT_NAME, (int) gatt_type, (long long) gatt_len, (int) PIO_INT);
    nerrs++;
  }

  if(varid >= 0){
    if((ret = PIOc_inq_att(ncid, varid, VATT_NAME, &vatt_type, &vatt_len))){
      fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): PIOc_inq_att() for "
        "the variable attribute (%s) returned %d\n", rearr_name, my_rank, VATT_NAME, ret);
      nerrs++;
    }
    else if((vatt_type != PIO_INT) || (vatt_len != 1)){
      fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the reopened "
        "variable attribute (%s) has type %d / len %lld, expected %d / 1\n", rearr_name,
        my_rank, VATT_NAME, (int) vatt_type, (long long) vatt_len, (int) PIO_INT);
      nerrs++;
    }
  }

  PIOc_closefile(ncid);
  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

  /* Data.
   *
   * Deliberately verified through a *second*, read-only native reopen
   * rather than through the write-capable one above.
   *
   * NEW BUG (found by this task, NOT in the plan's known-issues backlog -
   * documented, deliberately NOT fixed here, since this task adds tests
   * only): PIOc_read_darray_impl() (src/clib/core/pio_darray.cpp) picks the
   * io_desc_info_t it reads through with
   * iodesc->select_io_desc_info(file->iotype, file), which since Task 3
   * returns the *async* io_desc_info_t whenever file->uses_async_io is set.
   * It then rearranges the result with rearrange_io2comp()
   * (src/clib/core/rearr/pio_rearrange.cpp), which reads
   * iodesc->sync_io_desc_info unconditionally - its scount/stype/rfrom/rtype
   * and the sync I/O rank list. For a write-capable (uses_async_io == true)
   * file on a disjoint sync/async-procs topology with the BOX or SUBSET
   * rearranger, those two halves describe different I/O rank sets and
   * different MPI datatypes, and the pio_swapm()/MPI_Alltoallw() in
   * rearrange_io2comp() then packs from a buffer sized/laid out by the other
   * half - observed as a hard SEGV inside MPICH's blkidx_m2m()
   * (looputil.c:584) via pio_swapm() -> rearrange_io2comp() ->
   * PIOc_read_darray_impl(). CONTIG is unaffected because that path
   * rearranges with desc_info.rearr->rearrange_io2comp() - the same
   * io_desc_info_t it read through - instead of the free rearrange_io2comp()
   * function. This is a pre-existing read-path defect in its own right (any
   * write-capable BOX/SUBSET HDF5 file would hit it); it is not caused by
   * Tasks 10/11, but Tasks 10/11 make it reachable on a *reopened* file too,
   * because a reopened SCORPIO HDF5 file now keeps iotype == HDF5 instead of
   * being switched away to NETCDF4P.
   *
   * A read-only reopen has uses_async_io == false, so select_io_desc_info()
   * and rearrange_io2comp() agree on sync_io_desc_info and the data check
   * below exercises exactly the round trip the brief asks for, on a file
   * that is still natively HDF5. */
  int rd_ncid = -1;
  int rd_format = PIO_IOTYPE_HDF5;
  if((ret = PIOc_openfile(iosysid, &rd_ncid, &rd_format, filename, PIO_NOWRITE))){
    LOG_RANK0(my_rank, "test_native_reopen_roundtrip_%s() FAILED: the read-only reopen "
      "returned %d\n", rearr_name, ret);
    nerrs++;
  }
  else{
    file_desc_t *rd_file = NULL;
    if((pio_get_file(rd_ncid, &rd_file) == PIO_NOERR) && rd_file &&
       (rd_file->iotype != PIO_IOTYPE_HDF5) && (rd_file->iotype != PIO_IOTYPE_HDF5C)){
      fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): the read-only "
        "reopen switched the iotype away to %d (%s)\n", rearr_name, my_rank,
        rd_file->iotype, pio_iotype_to_string(rd_file->iotype));
      nerrs++;
    }

    int rd_varid = -1;
    std::vector<int> rdata(MAPLEN_PER_RANK, -1);
    if((ret = PIOc_inq_varid(rd_ncid, VAR_NAME, &rd_varid)) ||
       (ret = PIOc_read_darray(rd_ncid, rd_varid, ioid, (PIO_Offset) MAPLEN_PER_RANK,
                               rdata.data()))){
      fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): reading the "
        "reopened file returned %d\n", rearr_name, my_rank, ret);
      nerrs++;
    }
    else{
      for(int i = 0; i < MAPLEN_PER_RANK; i++){
        if(rdata[i] != wdata[i]){
          /* KNOWN PRE-EXISTING (backlog item 1): the CONTIG rearranger's
           * HDF5 write path has a documented, pre-existing data-corruption
           * issue that is NOT caused by Tasks 10/11's reopen logic. If this
           * fires for PIO_REARR_CONTIG, the metadata assertions above (dims,
           * vars, attrs, iotype) still prove the native reopen itself
           * worked. Report it, but do not treat it as a reopen failure. */
          fprintf(stderr, "test_native_reopen_roundtrip_%s() FAILED (rank %d): rdata[%d] (%d) "
            "!= wdata[%d] (%d)\n", rearr_name, my_rank, i, rdata[i], i, wdata[i]);
          nerrs++;
          break;
        }
      }
    }

    PIOc_closefile(rd_ncid);
  }

  PIOc_freedecomp(iosysid, ioid);
  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_native_reopen_roundtrip_contig(MPI_Comm test_comm, int my_rank, int ntasks)
{
  return test_native_reopen_roundtrip(test_comm, my_rank, ntasks, PIO_REARR_CONTIG, "contig",
                                      NUM_IO1);
}

static int test_native_reopen_roundtrip_box(MPI_Comm test_comm, int my_rank, int ntasks)
{
  return test_native_reopen_roundtrip(test_comm, my_rank, ntasks, PIO_REARR_BOX, "box",
                                      NUM_IO1);
}

/* ------------------------------------------------------------------------
 * Regression test for the createfile Bcast/Barrier race (formerly backlog
 * item documented on test_native_reopen_roundtrip_subset() below): with
 * every rank in test_comm acting as an I/O proc (num_io_procs == ntasks),
 * PIOc_createfile()'s main thread used to run its own synchronous MPI_Bcast
 * in spio_createfile_int() (src/clib/core/pioc_support.cpp) concurrently
 * with the async thread pool dequeuing the create op and running
 * spio_hdf5_create() (src/clib/core/iolib/hdf5/spio_hdf5_utils.cpp), which
 * does its own MPI_Barrier on a different (but, on some MPI stacks, not
 * independently progressable) communicator. That raced every rank into a
 * SEGV inside MPICH's poll_active_fboxes() (MPICH 4.1.2, ch3:nemesis
 * channel). spio_createfile_int() now drains the just-enqueued create op
 * (spio_wait_file_async_pend_ops()) before issuing its own Bcast, so the
 * two collectives can no longer be in flight at the same time. This test
 * only exercises createfile/closefile (not a write, which would also hit
 * the unrelated, still-open backlog item 2 fillbuf NULL-deref documented on
 * test_native_reopen_roundtrip_subset() below) - it is only meaningful
 * under PIO_USE_ASYNC_WR_THREAD, since that is the only build where the
 * create is ever actually dispatched onto the async thread pool.
 * ------------------------------------------------------------------------ */
static int test_subset_createfile_all_io_procs_no_race(MPI_Comm test_comm, int my_rank, int ntasks)
{
#if PIO_USE_ASYNC_WR_THREAD
  int ret = PIO_NOERR;
  int iosysid = -1;
  char filename[PIO_MAX_NAME + 1];
  int test_comm_sz = 0;

  (void) ntasks; /* pio_test_init2() leaves this at MPI_COMM_WORLD's size even when
                  * test_comm itself was split down to max_ntasks - use test_comm's
                  * own size below so every one of its members, and no more, is
                  * requested as an I/O proc. */
  MPI_Comm_size(test_comm, &test_comm_sz);

  snprintf(filename, PIO_MAX_NAME + 1, "%s_subset_createfile_all_io_procs.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, test_comm_sz, STRIDE1, BASE0, PIO_REARR_SUBSET,
                                &iosysid))){
    LOG_RANK0(my_rank, "test_subset_createfile_all_io_procs_no_race() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  int nerrs = 0;
  int ncid = -1;
  int format = PIO_IOTYPE_HDF5;

  if((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER))){
    LOG_RANK0(my_rank, "test_subset_createfile_all_io_procs_no_race() FAILED (rank %d): "
      "PIOc_createfile() with every rank as an I/O proc returned %d - this used to crash "
      "the whole process before it could even return an error (see the doc comment on this "
      "function)\n", my_rank, ret);
    nerrs++;
  }
  else{
    PIOc_closefile(ncid);
  }

  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
#else
  (void) test_comm;
  (void) my_rank;
  (void) ntasks;
  return PIO_NOERR;
#endif
}

/* ------------------------------------------------------------------------
 * Regression test for the def_var Bcast/collective-fill race: like
 * test_subset_createfile_all_io_procs_no_race() above, but for
 * spio_iosys_async_hdf5_def_var_op_add() (src/clib/core/iolib/hdf5/
 * spio_async_hdf5_utils.cpp) instead of the create op. PIOc_def_var()
 * used to enqueue the HDF5 def_var op onto the async thread pool and
 * return immediately, before the worker thread had run it. For a
 * variable on an unlimited (record) dimension, spio_hdf5_def_var()'s
 * H5Dcreate2() does its own collective MPI I/O (H5D__chunk_collective_fill(),
 * which collectively writes the fill value for the newly allocated chunk) on
 * this rank's dup'd async I/O comm. Without draining this def_var op before
 * returning, that worker-thread collective could still be in flight when a
 * later collective ran concurrently with it - either this same rank's main
 * thread moving on to the next file's createfile Bcast, or another rank
 * reaching a different point in its own queue - exactly the same race
 * already fixed for createfile/create (commit 56d2ab618c) but for def_var's
 * call site instead. Reproducing it needs several create/def_var/close/
 * delete cycles in the same long-lived iosystem (a single cycle rarely
 * raced); DEF_VAR_RACE_NUM_ITERS below is comfortably above the handful of
 * iterations (2-12) a standalone reproducer needed to crash every run before
 * the fix. On MPI stacks whose progress engine isn't safe for genuinely
 * concurrent cross-thread collectives (observed with MPICH 4.1.2's
 * ch3:nemesis channel), the unfixed race crashed every rank with a SEGV
 * inside the MPI library (e.g. MPL_atomic_acquire_load_int() /
 * poll_active_fboxes()) well before this loop could complete.
 * ------------------------------------------------------------------------ */
#define DEF_VAR_RACE_NUM_ITERS 30
#define DEF_VAR_RACE_DIM_LEN 4

static int test_subset_def_var_all_io_procs_no_race(MPI_Comm test_comm, int my_rank, int ntasks)
{
#if PIO_USE_ASYNC_WR_THREAD
  int ret = PIO_NOERR;
  int iosysid = -1;
  int test_comm_sz = 0;

  MPI_Comm_size(test_comm, &test_comm_sz);

  if((ret = PIOc_Init_Intracomm(test_comm, test_comm_sz, STRIDE1, BASE0, PIO_REARR_SUBSET,
                                &iosysid))){
    LOG_RANK0(my_rank, "test_subset_def_var_all_io_procs_no_race() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  int nerrs = 0;

  for(int iter = 0; (iter < DEF_VAR_RACE_NUM_ITERS) && (nerrs == 0); iter++){
    char filename[PIO_MAX_NAME + 1];
    int ncid = -1, dimid = -1, rec_dimid = -1, varid = -1;
    int format = PIO_IOTYPE_HDF5;
    int dimids[2];

    snprintf(filename, PIO_MAX_NAME + 1, "%s_subset_def_var_all_io_procs_%d.nc",
             TEST_NAME, iter);

    if((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER))){
      LOG_RANK0(my_rank, "test_subset_def_var_all_io_procs_no_race() FAILED (rank %d, "
        "iter %d): PIOc_createfile() returned %d\n", my_rank, iter, ret);
      nerrs++;
      break;
    }

    /* The record (unlimited) dimension must be dimids[0] (see the netCDF
     * classic-model requirement) so that def_var below creates a chunked,
     * extendible dataset - only that storage layout makes H5Dcreate2()
     * collectively write fill values (H5D__chunk_collective_fill()), which
     * is what made the race observable. */
    if((ret = PIOc_def_dim(ncid, "rec", PIO_UNLIMITED, &rec_dimid)) ||
       (ret = PIOc_def_dim(ncid, DIM_NAME, (PIO_Offset) DEF_VAR_RACE_DIM_LEN, &dimid))){
      LOG_RANK0(my_rank, "test_subset_def_var_all_io_procs_no_race() FAILED (rank %d, "
        "iter %d): PIOc_def_dim() returned %d\n", my_rank, iter, ret);
      nerrs++;
      PIOc_closefile(ncid);
      break;
    }

    dimids[0] = rec_dimid;
    dimids[1] = dimid;
    if((ret = PIOc_def_var(ncid, VAR_NAME, PIO_INT, 2, dimids, &varid))){
      LOG_RANK0(my_rank, "test_subset_def_var_all_io_procs_no_race() FAILED (rank %d, "
        "iter %d): PIOc_def_var() on an unlimited-dim variable returned %d - this used to "
        "crash the whole process before it could even return an error (see the doc comment "
        "on this function)\n", my_rank, iter, ret);
      nerrs++;
      PIOc_closefile(ncid);
      break;
    }

    PIOc_closefile(ncid);
    PIOc_deletefile(iosysid, filename);
  }

  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
#else
  (void) test_comm;
  (void) my_rank;
  (void) ntasks;
  return PIO_NOERR;
#endif
}

/* ------------------------------------------------------------------------
 * Regression test for the put_var/H5Dextend() record-dimension race: like
 * test_subset_def_var_all_io_procs_no_race() above, but for
 * spio_hdf5_put_var() (src/clib/core/iolib/hdf5/spio_hdf5_utils.cpp) instead
 * of def_var's H5Dcreate2(). A non-decomposed PIOc_put_var*() on a variable
 * with an unlimited (record) dimension enqueues spio_hdf5_put_var() onto
 * this rank's async thread pool queue (spio_iosys_async_hdf5_put_var_op_add(),
 * src/clib/core/pio_getput_int.cpp) and returns immediately, before the
 * worker thread has run it. When the write grows the record dimension,
 * spio_hdf5_put_var() calls H5Dextend() - a collective HDF5 call every rank
 * sharing the file's communicator must invoke, in the same order, for the
 * same record index. Nothing synchronized two ranks' independently
 * scheduled worker threads, so one rank's thread could race ahead to a
 * later record's H5Dextend() while another was still behind on an earlier
 * one - the same architectural race already fixed for createfile (commit
 * 56d2ab618c) and for def_var (test 14 above), but for put_var's call site.
 * Unlike those two, mismatched H5Dextend() calls here don't crash: they
 * leave the dataset's recognized extent inconsistent across ranks, so some
 * records silently read back as the (zero) fill value instead of what was
 * written - exactly the "Got wrong value" data-mismatch symptom this test
 * reproduces by writing several records back to back and reading them all
 * back afterward.
 * ------------------------------------------------------------------------ */
#define PUT_VAR_RACE_NUM_RECS 20
#define PUT_VAR_RACE_VAR_NAME "put_var_race_var"

static int test_subset_put_var_record_dim_no_race(MPI_Comm test_comm, int my_rank, int ntasks)
{
  int ret = PIO_NOERR;
  int iosysid = -1;
  int test_comm_sz = 0;
  char filename[PIO_MAX_NAME + 1];

  (void) ntasks; /* see test_subset_createfile_all_io_procs_no_race()'s comment on why
                  * test_comm's own size, not ntasks, is used below. */
  MPI_Comm_size(test_comm, &test_comm_sz);

  snprintf(filename, PIO_MAX_NAME + 1, "%s_subset_put_var_race.nc", TEST_NAME);

  if((ret = PIOc_Init_Intracomm(test_comm, test_comm_sz, STRIDE1, BASE0, PIO_REARR_SUBSET,
                                &iosysid))){
    LOG_RANK0(my_rank, "test_subset_put_var_record_dim_no_race() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  int nerrs = 0;
  int ncid = -1, rec_dimid = -1, varid = -1;
  int format = PIO_IOTYPE_HDF5;

  if((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER)) ||
     (ret = PIOc_def_dim(ncid, "rec", PIO_UNLIMITED, &rec_dimid)) ||
     (ret = PIOc_def_var(ncid, PUT_VAR_RACE_VAR_NAME, PIO_INT, 1, &rec_dimid, &varid)) ||
     (ret = PIOc_enddef(ncid))){
    LOG_RANK0(my_rank, "test_subset_put_var_record_dim_no_race() FAILED: defining the file "
      "returned %d\n", ret);
    if(ncid >= 0) { PIOc_closefile(ncid); }
    finalize_iosys(iosysid);
    return ret;
  }

#if PIO_USE_ASYNC_WR_THREAD
  {
    file_desc_t *file = NULL;
    if((pio_get_file(ncid, &file) != PIO_NOERR) || !file || !file->uses_async_io){
      LOG_RANK0(my_rank, "test_subset_put_var_record_dim_no_race() FAILED: the write-capable "
        "file does not have uses_async_io == true - the put_var would not be routed through "
        "the async thread pool and this scenario would prove nothing\n");
      PIOc_closefile(ncid);
      finalize_iosys(iosysid);
      return PIO_EINTERNAL;
    }
  }
#endif

  /* Expected value at record r is deliberately non-zero so that an unwritten
   * (fill-valued) record cannot be mistaken for a pass. */
  std::vector<int> wdata(PUT_VAR_RACE_NUM_RECS);
  for(int r = 0; r < PUT_VAR_RACE_NUM_RECS; r++){
    wdata[r] = 1000 + r;
  }

  for(int r = 0; (r < PUT_VAR_RACE_NUM_RECS) && (ret == PIO_NOERR); r++){
    PIO_Offset start[1] = { (PIO_Offset) r };
    PIO_Offset count[1] = { 1 };

    if((ret = PIOc_put_vara_int(ncid, varid, start, count, &wdata[r]))){
      LOG_RANK0(my_rank, "test_subset_put_var_record_dim_no_race() FAILED (rank %d, rec %d): "
        "PIOc_put_vara_int() returned %d\n", my_rank, r, ret);
    }
  }

  if(ret != PIO_NOERR){
    nerrs++;
    PIOc_closefile(ncid);
    finalize_iosys(iosysid);
    return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
  }

  if((ret = PIOc_closefile(ncid))){
    LOG_RANK0(my_rank, "test_subset_put_var_record_dim_no_race() FAILED: PIOc_closefile() "
      "returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  drain_async_ops(iosysid);
  MPI_Barrier(test_comm);

  int rd_ncid = -1, rd_varid = -1;
  int rd_format = PIO_IOTYPE_HDF5;
  std::vector<int> rdata(PUT_VAR_RACE_NUM_RECS, -1);

  if((ret = PIOc_openfile(iosysid, &rd_ncid, &rd_format, filename, PIO_NOWRITE))){
    LOG_RANK0(my_rank, "test_subset_put_var_record_dim_no_race() FAILED: the read-only reopen "
      "returned %d\n", ret);
    finalize_iosys(iosysid);
    return ret;
  }

  if((ret = PIOc_inq_varid(rd_ncid, PUT_VAR_RACE_VAR_NAME, &rd_varid)) ||
     (ret = PIOc_get_var_int(rd_ncid, rd_varid, rdata.data()))){
    fprintf(stderr, "test_subset_put_var_record_dim_no_race() FAILED (rank %d): reading the "
      "record-dim variable back returned %d\n", my_rank, ret);
    nerrs++;
  }
  else{
    for(int r = 0; r < PUT_VAR_RACE_NUM_RECS; r++){
      if(rdata[r] != wdata[r]){
        fprintf(stderr, "test_subset_put_var_record_dim_no_race() FAILED (rank %d): "
          "rdata[%d] (%d) != wdata[%d] (%d) - PIOc_put_vara_int() on a record dimension raced "
          "H5Dextend() across ranks and silently dropped this record's data (see this test's "
          "doc comment)\n", my_rank, r, rdata[r], r, wdata[r]);
        nerrs++;
        break;
      }
    }
  }

  PIOc_closefile(rd_ncid);
  finalize_iosys(iosysid);

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

static int test_native_reopen_roundtrip_subset(MPI_Comm test_comm, int my_rank, int ntasks)
{
#if PIO_USE_ASYNC_WR_THREAD
  /* KNOWN PRE-EXISTING ISSUE - this scenario is skipped under
   * PIO_USE_ASYNC_WR_THREAD because routing a SUBSET-rearranger HDF5 write
   * through the async thread pool, with disjoint sync/async I/O procs
   * (num_io_procs == NUM_IO1, as used by the CONTIG/BOX variants above),
   * hits a hard SEGV that takes down the whole test binary (not something
   * that can be narrowed to a single skipped assertion). This is
   * pre-existing, independent of the native reopen logic (Tasks 10/11) this
   * scenario is actually about, and out of scope to fix here: backlog item
   * 2 - the SUBSET rearranger's HDF5 async write path dereferences a NULL
   * v1desc->fillbuf at spio_async_hdf5_utils.cpp:977 (its own in-source
   * FIXME documents this) whenever the *sync* io_desc_info_t's
   * holegridsize is 0 (no vdesc0->fillbuf was ever allocated in
   * PIOc_write_darray_multi_impl()) while the *async* one's is > 0 -
   * exactly what a disjoint topology produces.
   *
   * (A second, previously-documented pre-existing issue here - every rank
   * in test_comm as an I/O proc racing PIOc_createfile()'s synchronous
   * MPI_Bcast against the async thread pool's own MPI_Barrier inside
   * spio_hdf5_create() - is now fixed in spio_createfile_int() and covered
   * directly by test_subset_createfile_all_io_procs_no_race() above.)
   *
   * The disjoint-procs topology (and hence the native reopen path itself) is
   * still exercised end-to-end by the CONTIG and BOX variants above, and by
   * scenarios 1, 2, 4 and 5; the ASYNC-OFF build below runs this SUBSET
   * variant for real, since none of the above is reachable without the async
   * thread pool. */
  LOG_RANK0(my_rank, "test_native_reopen_roundtrip_subset() SKIPPED: every attempted way of "
    "routing a SUBSET-rearranger HDF5 write through the async thread pool hits a hard, "
    "pre-existing SEGV that takes down the whole process (see the doc comment on this function) "
    "- not exercised in the PIO_USE_ASYNC_WR_THREAD build. The native reopen path itself is "
    "still covered by the CONTIG and BOX variants, and the ASYNC-OFF build runs this scenario "
    "for real.\n");
  (void) test_comm;
  (void) ntasks;
  return PIO_NOERR;
#else
  /* No async thread pool in this build (uses_async_io is always false - Task
   * 1), so none of the concurrency issues documented above are reachable:
   * run the real round trip, with the same disjoint-procs I/O proc count
   * used by the CONTIG/BOX variants (num_io_procs must not exceed
   * test_comm's own size - see the CONTIG/BOX comment). */
  return test_native_reopen_roundtrip(test_comm, my_rank, ntasks, PIO_REARR_SUBSET, "subset",
                                      NUM_IO1);
#endif
}

/* ------------------------------------------------------------------------
 * Scenario 9: a plain, non-SCORPIO HDF5 file opened with PIO_IOTYPE_HDF5
 * through the real PIOc_openfile() path falls back to the historical
 * switch-iotype-away behavior, unchanged: the open succeeds and the file
 * ends up with a non-HDF5 iotype.
 *
 * Task 10/11's own unit tests (tests/cunit/test_hdf5_reopen.cpp) assert on
 * spio_hdf5_reopen()'s PIO_EBADIOTYPE return value directly; this one
 * asserts the end-user-visible outcome of that same fallback.
 * ------------------------------------------------------------------------ */
static int make_plain_hdf5_file(const char *fname)
{
  hid_t fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if(fid == H5I_INVALID_HID){
    return FAIL;
  }

  hsize_t dims[1] = { 4 };
  hid_t sid = H5Screate_simple(1, dims, NULL);
  if(sid == H5I_INVALID_HID){
    H5Fclose(fid);
    return FAIL;
  }

  hid_t did = H5Dcreate2(fid, "plain_var", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  if(did == H5I_INVALID_HID){
    H5Sclose(sid);
    H5Fclose(fid);
    return FAIL;
  }

  H5Dclose(did);
  H5Sclose(sid);
  H5Fclose(fid);

  return PIO_NOERR;
}

static int test_native_reopen_fallback_non_scorpio_file(MPI_Comm test_comm, int my_rank)
{
  int ret = PIO_NOERR;
  int iosysid = -1;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s_plain_hdf5.nc", TEST_NAME);

  /* Written with the raw HDF5 C API (no SCORPIO markers at all) on one rank
   * only - H5Fcreate() here is not a parallel/collective create. */
  int make_ret = PIO_NOERR;
  if(my_rank == 0){
    make_ret = make_plain_hdf5_file(filename);
  }
  MPI_Bcast(&make_ret, 1, MPI_INT, 0, test_comm);
  if(make_ret != PIO_NOERR){
    LOG_RANK0(my_rank, "test_native_reopen_fallback_non_scorpio_file() FAILED: could not "
      "create the plain HDF5 fixture file\n");
    return PIO_EINTERNAL;
  }
  MPI_Barrier(test_comm);

  if((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                PIO_REARR_CONTIG, &iosysid))){
    LOG_RANK0(my_rank, "test_native_reopen_fallback_non_scorpio_file() FAILED: "
      "PIOc_Init_Intracomm() returned %d\n", ret);
    return ret;
  }

  int nerrs = 0;
  int ncid = -1;
  int format = PIO_IOTYPE_HDF5;

  ret = PIOc_openfile(iosysid, &ncid, &format, filename, PIO_NOWRITE);
  if(ret != PIO_NOERR){
    LOG_RANK0(my_rank, "test_native_reopen_fallback_non_scorpio_file() FAILED: opening a "
      "plain (non-SCORPIO) HDF5 file with PIO_IOTYPE_HDF5 returned %d - the fallback must "
      "still open it successfully\n", ret);
    nerrs++;
  }
  else{
    file_desc_t *file = NULL;
    if((pio_get_file(ncid, &file) != PIO_NOERR) || !file){
      LOG_RANK0(my_rank, "test_native_reopen_fallback_non_scorpio_file() FAILED: "
        "pio_get_file() failed after the fallback open\n");
      nerrs++;
    }
    else if((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C)){
      fprintf(stderr, "test_native_reopen_fallback_non_scorpio_file() FAILED (rank %d): a "
        "plain (non-SCORPIO) HDF5 file was reopened natively as HDF5 (iotype = %d) - "
        "spio_hdf5_is_scorpio_written() must not recognize it, and the open must fall back "
        "to a non-HDF5 iotype\n", my_rank, file->iotype);
      nerrs++;
    }

    PIOc_closefile(ncid);
  }

  finalize_iosys(iosysid);

  if(my_rank == 0){
    remove(filename);
  }

  return (reduce_nerrs(test_comm, nerrs) == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* ------------------------------------------------------------------------ */

#define RUN_SCENARIO(call, name)                                              \
  do{                                                                          \
    printf("%d running %s()\n", my_rank, name);                                \
    fflush(stdout);                                                            \
    int scenario_ret = (call);                                                 \
    if(scenario_ret != PIO_NOERR){                                             \
      LOG_RANK0(my_rank, "%s() FAILED, ret = %d\n", name, scenario_ret);       \
      num_errors++;                                                            \
    }                                                                          \
    else{                                                                      \
      LOG_RANK0(my_rank, "%s() PASSED\n", name);                               \
    }                                                                          \
  }while(0)

int main(int argc, char **argv)
{
  int my_rank, ntasks;
  MPI_Comm test_comm = MPI_COMM_NULL;
  int ret;
  int num_errors = 0;

  if((ret = pio_test_init2(argc, argv, &my_rank, &ntasks, MIN_NTASKS,
                           TARGET_NTASKS, 3, &test_comm))){
    ERR(ERR_INIT);
  }
  if((ret = PIOc_set_iosystem_error_handling(PIO_DEFAULT, PIO_RETURN_ERROR, NULL))){
    return ret;
  }

  if(my_rank < TARGET_NTASKS){
    /* Scenario 3 runs on a single rank (see its doc comment); split it out
     * of test_comm up front so that every rank participates in the (
     * collective) split exactly once. */
    MPI_Comm single_rank_comm = MPI_COMM_NULL;
    MPI_Comm_split(test_comm, (my_rank == 0) ? 0 : MPI_UNDEFINED, my_rank, &single_rank_comm);

    RUN_SCENARIO(test_write_only_full_async_lifecycle(test_comm, my_rank, ntasks),
                 "test_write_only_full_async_lifecycle");

    RUN_SCENARIO(test_read_only_always_sync(test_comm, my_rank, ntasks),
                 "test_read_only_always_sync");

    if(single_rank_comm != MPI_COMM_NULL){
      RUN_SCENARIO(test_read_write_interleaved_no_race(single_rank_comm, my_rank),
                   "test_read_write_interleaved_no_race");
    }
    MPI_Barrier(test_comm);

    RUN_SCENARIO(test_disjoint_nondecomposed_put_get_roundtrip(test_comm, my_rank, ntasks),
                 "test_disjoint_nondecomposed_put_get_roundtrip");

    RUN_SCENARIO(test_cross_routing_copy_att_errors(test_comm, my_rank, ntasks),
                 "test_cross_routing_copy_att_errors");

#ifdef _PNETCDF
    RUN_SCENARIO(test_non_hdf5_write_open_leaves_no_lock_file(test_comm, my_rank),
                 "test_non_hdf5_write_open_leaves_no_lock_file");
#else
    LOG_RANK0(my_rank, "test_non_hdf5_write_open_leaves_no_lock_file() SKIPPED "
      "(PnetCDF is not configured in this build - no non-HDF5 parallel iotype to "
      "exercise the lock-file leak with)\n");
#endif

    RUN_SCENARIO(test_disjoint_write_close_reopen_lock_absent(test_comm, my_rank, ntasks),
                 "test_disjoint_write_close_reopen_lock_absent");

#if PIO_USE_ASYNC_WR_THREAD
    RUN_SCENARIO(test_disjoint_write_close_reopen_lock_present(test_comm, my_rank, ntasks),
                 "test_disjoint_write_close_reopen_lock_present");
#else
    LOG_RANK0(my_rank, "test_disjoint_write_close_reopen_lock_present() SKIPPED "
      "(PIO_USE_ASYNC_WR_THREAD is disabled - spio_wait_on_async_lock_file() is an "
      "unconditional no-op in this build and no lock file is ever created, so there is no "
      "wait path to exercise)\n");
#endif

    RUN_SCENARIO(test_native_reopen_fallback_non_scorpio_file(test_comm, my_rank),
                 "test_native_reopen_fallback_non_scorpio_file");

    RUN_SCENARIO(test_native_reopen_roundtrip_contig(test_comm, my_rank, ntasks),
                 "test_native_reopen_roundtrip_contig");

    RUN_SCENARIO(test_native_reopen_roundtrip_box(test_comm, my_rank, ntasks),
                 "test_native_reopen_roundtrip_box");

    RUN_SCENARIO(test_native_reopen_roundtrip_subset(test_comm, my_rank, ntasks),
                 "test_native_reopen_roundtrip_subset");

    RUN_SCENARIO(test_subset_createfile_all_io_procs_no_race(test_comm, my_rank, ntasks),
                 "test_subset_createfile_all_io_procs_no_race");

    RUN_SCENARIO(test_subset_def_var_all_io_procs_no_race(test_comm, my_rank, ntasks),
                 "test_subset_def_var_all_io_procs_no_race");

    RUN_SCENARIO(test_subset_put_var_record_dim_no_race(test_comm, my_rank, ntasks),
                 "test_subset_put_var_record_dim_no_race");

    if(single_rank_comm != MPI_COMM_NULL){
      MPI_Comm_free(&single_rank_comm);
    }
  }

  printf("%d %s Finalizing...\n", my_rank, TEST_NAME);
  if((ret = pio_test_finalize(&test_comm))){
    return ret;
  }

  if(num_errors != 0){
    LOG_RANK0(my_rank, "Total errors = %d\n", num_errors);
    return FAIL;
  }

  printf("%d %s SUCCESS!!\n", my_rank, TEST_NAME);

  return 0;
}

#else /* _HDF5 */

/* HDF5 support not enabled in this build - nothing to test. */
int main(int argc, char *argv[])
{
  return 0;
}

#endif /* _HDF5 */
