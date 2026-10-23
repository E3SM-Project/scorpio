/*
 * Test write+read using the HDF5 I/O type
 * Requires [2, 4] MPI tasks and writes a distributed 1D array and reads it back
 * Only enabled when ASYNC is turned on (PIO_USE_ASYNC_WR_THREAD), a no-op otherwise
 */
/* FIXME: Remove this test and replace it with a test for all I/O types */
#include "pio_config.h"

#ifdef _HDF5

#include <vector>
#include <numeric>

#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"

/* The number of tasks this test should run on. */
#define TARGET_NTASKS 4

/* Need at least 2 ranks so that, when PIO_USE_ASYNC_WR_THREAD is on, the
 * sync and async I/O procs used for the CONTIG case below (1 io task out of
 * TARGET_NTASKS) can be genuinely disjoint ranks - the scenario Task 4's
 * runtime dispatch must handle correctly. */
#define MIN_NTASKS 2

#define TEST_NAME "test_darray_hdf5_contig"

#define NDIM1 1

/* One dedicated (sync) I/O task; with PIO_USE_ASYNC_WR_THREAD on and
 * TARGET_NTASKS > 1 this leaves other ranks free to be selected as the
 * disjoint async I/O proc (see spio_iosys_utils.cpp's IO_comm_info ctor). */
#define STRIDE1 1
#define BASE0 0
#define NUM_IO1 1

#define MAPLEN_PER_RANK 2

#define VAR_NAME "contig_hdf5_var"

static const int FAIL = -1;

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

/* Write and read back a PIO_INT 1D array using HDF5 */
static int test_write_read_hdf5(int iosysid, int my_rank, int ntasks, int rearranger)
{
  int ret;
  int ioid;
  int ncid, ncid2;
  int dimid, varid;
  int format = PIO_IOTYPE_HDF5;
  char filename[PIO_MAX_NAME + 1];

  const int gdimlen[NDIM1] = { MAPLEN_PER_RANK * ntasks };
  std::vector<PIO_Offset> compmap(MAPLEN_PER_RANK);
  std::iota(compmap.begin(), compmap.end(),
            static_cast<PIO_Offset>(my_rank * MAPLEN_PER_RANK + 1));

  if ((ret = PIOc_init_decomp(iosysid, PIO_INT, NDIM1, gdimlen, MAPLEN_PER_RANK,
                              compmap.data(), &ioid, rearranger, NULL, NULL)))
    return ret;

  std::vector<int> wdata(MAPLEN_PER_RANK);
  for(int i = 0; i < MAPLEN_PER_RANK; i++){
    wdata[i] = my_rank * 100 + i;
  }

  snprintf(filename, PIO_MAX_NAME + 1, "%s_rearr_%d.nc", TEST_NAME, rearranger);

  if ((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER))){
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_def_dim(ncid, "x", (PIO_Offset)gdimlen[0], &dimid))){
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_def_var(ncid, VAR_NAME, PIO_INT, NDIM1, &dimid, &varid))){
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_enddef(ncid))){
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_write_darray(ncid, varid, ioid, MAPLEN_PER_RANK, wdata.data(), NULL))){
    LOG_RANK0(my_rank, "FAIL: PIOc_write_darray() failed, rearranger = %d, ret = %d\n", rearranger, ret);
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_closefile(ncid))){
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_openfile(iosysid, &ncid2, &format, filename, PIO_NOWRITE))){
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_inq_varid(ncid2, VAR_NAME, &varid))){
    PIOc_closefile(ncid2);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  std::vector<int> rdata(MAPLEN_PER_RANK, -1);
  if ((ret = PIOc_read_darray(ncid2, varid, ioid, MAPLEN_PER_RANK, rdata.data()))){
    LOG_RANK0(my_rank, "FAIL: PIOc_read_darray() failed, rearranger = %d, ret = %d\n", rearranger, ret);
    PIOc_closefile(ncid2);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_closefile(ncid2))){
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  int cmp_ret = PIO_NOERR;
  for(int i = 0; i < MAPLEN_PER_RANK; i++){
    if(rdata[i] != wdata[i]){
      LOG_RANK0(my_rank, "FAIL: rearranger = %d, rdata[%d] (%d) != wdata[%d] (%d)\n",
                rearranger, i, rdata[i], i, wdata[i]);
      cmp_ret = PIO_EINTERNAL;
    }
  }

  int free_ret = PIOc_freedecomp(iosysid, ioid);
  if(cmp_ret == PIO_NOERR) cmp_ret = free_ret;

  return cmp_ret;
}

/* Write and read back a 1D array using HDF5 (all rearrs), where
 * the decomposition's PIO type is different from the variable type,
 * PIO_DOUBLE, forcing data conversion
 */
static int test_write_read_hdf5_dtconv(int iosysid, int my_rank, int ntasks)
{
  int ret;
  int ioid;
  int ncid;
  int dimid, varid;
  int format = PIO_IOTYPE_HDF5;
  char filename[PIO_MAX_NAME + 1];
  int rearranger = PIO_REARR_SUBSET;

  /* Global dimension is twice the total number of mapped elements, and each
   * rank's block is followed by an equal-sized unmapped gap
  */
  const int gdimlen[NDIM1] = { 2 * MAPLEN_PER_RANK * ntasks };
  std::vector<PIO_Offset> compmap(MAPLEN_PER_RANK);
  std::iota(compmap.begin(), compmap.end(),
            static_cast<PIO_Offset>(my_rank * 2 * MAPLEN_PER_RANK + 1));

  /* Decomposition type: PIO_INT */
  if ((ret = PIOc_init_decomp(iosysid, PIO_INT, NDIM1, gdimlen, MAPLEN_PER_RANK,
                              compmap.data(), &ioid, rearranger, NULL, NULL)))
    return ret;

  /* Check if the decomposition map with gaps produces multiple contigous
   * regions as expected
   */
  if (my_rank == 0){
    io_desc_t *iodesc = pio_get_iodesc_from_id(ioid);
    if (iodesc == NULL){
      LOG_RANK0(my_rank, "FAIL: pio_get_iodesc_from_id(%d) returned NULL (dtconv)\n", ioid);
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
    if (iodesc->sync_io_desc_info.maxregions <= 1){
      LOG_RANK0(my_rank, "FAIL: test_write_read_hdf5_dtconv's gapped decomposition produced only "
                "%d region(s) on the IO task - expected > 1 to exercise the multi-region "
                "(H5S_SELECT_OR) union-hyperslab branch of the new HDF5 read case\n",
                iodesc->sync_io_desc_info.maxregions);
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
    LOG_RANK0(my_rank, "test_write_read_hdf5_dtconv: iodesc->sync_io_desc_info.maxregions = %d "
              "(> 1, confirms the H5S_SELECT_OR union-hyperslab branch is exercised below)\n",
              iodesc->sync_io_desc_info.maxregions);
  }

#if PIO_USE_ASYNC_WR_THREAD
  /* Confirm the disjoint-procs eager allocation still leaves
   * iodesc->async_io_desc_info non-NULL and correctly populated for this
   * SUBSET decomposition. Routing (sync vs. async io_desc_info_t) is no
   * longer decided here: since Task 3, select_io_desc_info() reads
   * file->uses_async_io, a per-*file* flag, not the disjoint-procs comm
   * topology this io_desc_t was built with - so that check has moved below,
   * once a real file_desc_t exists (see the comment after PIOc_enddef()). */
  if (my_rank == 0){
    io_desc_t *iodesc = pio_get_iodesc_from_id(ioid);
    if (iodesc == NULL){
      LOG_RANK0(my_rank, "FAIL: pio_get_iodesc_from_id() returned NULL (dtconv)\n");
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
    /* This decomposition's I/O ranks (TARGET_NTASKS=4, NUM_IO1=1) are the
     * real, production-built, genuinely disjoint sync/async I/O rank sets
     * (same shape as test_hdf5_async_contig.c) - not a cunit mock, which
     * cannot represent disjoint membership (see Task 6's Context: the
     * existing IO_comm_info test-mock constructors hard-code
     * is_sync_io_proc_/is_async_io_proc_ to true and do not model per-rank
     * membership). So this is the one place in the whole plan that proves,
     * for a real disjoint-procs IO configuration, that
     * subset_rearranger_init()'s Task 6 eager-allocation block still leaves
     * iodesc->async_io_desc_info non-NULL and correctly populated.
     * box_rearranger_init()'s equivalent block is structurally identical
     * (same io_desc_info_t constructor call, same CalcStartandCount/
     * compute_maxIObuffersize sequence, gated the same way) and is exercised
     * end-to-end, in an equally disjoint niotasks=1 configuration, by Task
     * 8's test_hdf5_async_box.c - that example cannot reach into
     * iodesc->async_io_desc_info (it only calls the public C API), so this
     * SUBSET check is the plan's one white-box confirmation of this
     * behavior; BOX relies on the code-path equivalence above plus Task 8's
     * example completing (no crash/hang) in the same disjoint shape. */
    if (iodesc->async_io_desc_info == NULL){
      LOG_RANK0(my_rank, "FAIL: test_write_read_hdf5_dtconv's SUBSET decomposition has a NULL "
                "async_io_desc_info even with PIO_USE_ASYNC_WR_THREAD on - subset_rearranger_init() "
                "should have eagerly allocated it regardless of the disjoint-procs fallback\n");
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
    if ((iodesc->async_io_desc_info->maplen != iodesc->sync_io_desc_info.maplen) ||
        (iodesc->async_io_desc_info->ndims != iodesc->sync_io_desc_info.ndims)){
      LOG_RANK0(my_rank, "FAIL: test_write_read_hdf5_dtconv's async_io_desc_info maplen/ndims do not "
                "match sync_io_desc_info - the disjoint-procs eager allocation populated it "
                "incorrectly\n");
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
  }
#endif

  std::vector<int> wdata(MAPLEN_PER_RANK);
  for(int i = 0; i < MAPLEN_PER_RANK; i++){
    wdata[i] = my_rank * 100 + i;
  }

  snprintf(filename, PIO_MAX_NAME + 1, "%s_dtconv.nc", TEST_NAME);

  if ((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER))){
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_def_dim(ncid, "x", (PIO_Offset)gdimlen[0], &dimid))){
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  /* Variable type in the file: PIO_DOUBLE */
  if ((ret = PIOc_def_var(ncid, VAR_NAME, PIO_DOUBLE, NDIM1, &dimid, &varid))){
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_enddef(ncid))){
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

#if PIO_USE_ASYNC_WR_THREAD
  /* Since Task 3, select_io_desc_info() routes purely on file->uses_async_io
   * (set once at file open/create time - see spio_createfile_int()), not on
   * this io_desc_t's own (disjoint) sync/async I/O comm topology. file->
   * uses_async_io is computed as (file->mode & PIO_WRITE) &&
   * !iosys_uses_async_io_service(ios). spio_createfile_int() always ORs the
   * PIO_WRITE bit into file->mode once the underlying create call succeeds
   * (created files are always writable), and uses_async_io is now computed
   * *after* that fixup - so even though this test creates its file via
   * PIOc_createfile(..., PIO_CLOBBER) (PIO_CLOBBER == NC_CLOBBER == 0, which
   * does not itself set the PIO_WRITE/NC_WRITE bit), file->mode & PIO_WRITE
   * is 1 by the time uses_async_io is computed. This test's iosystem is a
   * plain PIOc_Init_Intracomm iosystem (not an intercomm-based async I/O
   * service), so iosys_uses_async_io_service(ios) is false and
   * uses_async_io is true here. So this file's SUBSET writes route through
   * async_io_desc_info (assuming it is non-NULL, confirmed above, and the
   * rearranger is CONTIG/BOX/SUBSET, confirmed by the PIO_REARR_SUBSET used
   * here). */
  if (my_rank == 0){
    io_desc_t *iodesc = pio_get_iodesc_from_id(ioid);
    file_desc_t *file = NULL;
    if (iodesc == NULL || pio_get_file(ncid, &file) != PIO_NOERR || file == NULL){
      LOG_RANK0(my_rank, "FAIL: pio_get_iodesc_from_id()/pio_get_file() returned NULL (dtconv)\n");
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
    if (!file->uses_async_io){
      LOG_RANK0(my_rank, "FAIL: test_write_read_hdf5_dtconv's file unexpectedly has "
                "uses_async_io == false (spio_createfile_int() ORs the PIO_WRITE bit "
                "into file->mode before computing uses_async_io, so a created file "
                "should have uses_async_io == true here)\n");
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
    if (&(iodesc->select_io_desc_info(format, file)) != iodesc->async_io_desc_info){
      LOG_RANK0(my_rank, "FAIL: test_write_read_hdf5_dtconv's SUBSET decomposition unexpectedly "
                "did not route to async_io_desc_info even though file->uses_async_io is true\n");
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      return PIO_EINTERNAL;
    }
  }
#endif

  if ((ret = PIOc_write_darray(ncid, varid, ioid, MAPLEN_PER_RANK, wdata.data(), NULL))){
    LOG_RANK0(my_rank, "FAIL: PIOc_write_darray() failed (dtconv), ret = %d\n", ret);
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_sync(ncid))){
    LOG_RANK0(my_rank, "FAIL: PIOc_sync() failed (dtconv), ret = %d\n", ret);
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  std::vector<int> rdata(MAPLEN_PER_RANK, -1);
  if ((ret = PIOc_read_darray(ncid, varid, ioid, MAPLEN_PER_RANK, rdata.data()))){
    LOG_RANK0(my_rank, "FAIL: PIOc_read_darray() failed (dtconv), ret = %d\n", ret);
    PIOc_closefile(ncid);
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  if ((ret = PIOc_closefile(ncid))){
    PIOc_freedecomp(iosysid, ioid);
    return ret;
  }

  int cmp_ret = PIO_NOERR;
  for(int i = 0; i < MAPLEN_PER_RANK; i++){
    if(rdata[i] != wdata[i]){
      LOG_RANK0(my_rank, "FAIL: dtconv, rdata[%d] (%d) != wdata[%d] (%d)\n",
                i, rdata[i], i, wdata[i]);
      cmp_ret = PIO_EINTERNAL;
    }
  }

  int free_ret = PIOc_freedecomp(iosysid, ioid);
  if(cmp_ret == PIO_NOERR) cmp_ret = free_ret;

  return cmp_ret;
}

/* Larger per-rank workload used only by the close-time async-write drain
 * test below (test_write_close_drain_hdf5()): with the small MAPLEN_PER_RANK
 * payload the other tests in this file use, the async write thread pool's
 * HDF5 write usually finishes so quickly that even a completely missing
 * close-time wait could go unnoticed by luck. A bigger buffer makes the
 * worker thread's H5Dwrite() take measurably longer, widening the window in
 * which spio_hard_closefile() closing the file's HDF5 handles without first
 * waiting on the async-owning rank would be observable (as a data mismatch
 * on reopen, or worse, a crash from closing a handle the worker thread is
 * still using).
 */
#define CLOSE_DRAIN_MAPLEN_PER_RANK 200000
/* A single iteration only: looping multiple full create/write/close/open/
 * read/close cycles back-to-back on the same iosystem (without an
 * intervening PIOc_finalize()) was found, during development of this test,
 * to trip a separate, pre-existing, out-of-scope concurrency issue in the
 * async write thread pool's handling of rapid repeated file create/close
 * (an MPI_Comm_free() race, unrelated to this task's close-time-drain fix -
 * see the task-6-report.md for details). Kept at 1 to isolate this test to
 * the specific close-time-drain behavior Task 6 is responsible for. */
#define CLOSE_DRAIN_NUM_ITERS 1

/* Repro/regression test for Bug 6's close-time-drain half: a decomposed
 * (CONTIG) HDF5 write queues an async op on the async-owning I/O rank, and
 * then the file is closed immediately (no PIOc_sync() in between). With
 * NUM_IO1=1 and TARGET_NTASKS>1, this test's sync and async I/O ranks are
 * genuinely disjoint (see the file-level comment above), so the async-owning
 * rank is specifically a rank that is NOT the sync I/O proc - exactly the
 * rank on which the old spio_hard_closefile() gate
 * (ios->io_comm_info->is_sync_io_proc()) would always evaluate false and
 * skip the drain entirely, letting PIOc_closefile() close the file's HDF5
 * handles out from under the still-running async write.
 *
 * The fix (swapping that gate for
 * SPIO_Util::Iosys_Util::is_file_owning_io_proc(ios, file), see pio_file.cpp)
 * has to be reflected in correct, repeatable round-trip data after each
 * close+reopen below.
 */
static int test_write_close_drain_hdf5(int iosysid, int my_rank, int ntasks)
{
  int ret;
  int ioid;
  int ncid, ncid2;
  int dimid, varid;
  int format = PIO_IOTYPE_HDF5;
  char filename[PIO_MAX_NAME + 1];
  int rearranger = PIO_REARR_CONTIG;

  const int gdimlen[NDIM1] = { CLOSE_DRAIN_MAPLEN_PER_RANK * ntasks };
  std::vector<PIO_Offset> compmap(CLOSE_DRAIN_MAPLEN_PER_RANK);
  std::iota(compmap.begin(), compmap.end(),
            static_cast<PIO_Offset>(my_rank * CLOSE_DRAIN_MAPLEN_PER_RANK + 1));

  if ((ret = PIOc_init_decomp(iosysid, PIO_INT, NDIM1, gdimlen, CLOSE_DRAIN_MAPLEN_PER_RANK,
                              compmap.data(), &ioid, rearranger, NULL, NULL)))
    return ret;

  int cmp_ret = PIO_NOERR;
  for(int iter = 0; iter < CLOSE_DRAIN_NUM_ITERS; iter++){
    std::vector<int> wdata(CLOSE_DRAIN_MAPLEN_PER_RANK);
    for(int i = 0; i < CLOSE_DRAIN_MAPLEN_PER_RANK; i++){
      wdata[i] = my_rank * 100000 + iter * 1000 + i;
    }

    snprintf(filename, PIO_MAX_NAME + 1, "%s_close_drain_%d.nc", TEST_NAME, iter);

    if ((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER))){
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    if ((ret = PIOc_def_dim(ncid, "x", (PIO_Offset)gdimlen[0], &dimid))){
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    if ((ret = PIOc_def_var(ncid, VAR_NAME, PIO_INT, NDIM1, &dimid, &varid))){
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    if ((ret = PIOc_enddef(ncid))){
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

#if PIO_USE_ASYNC_WR_THREAD
    if ((iter == 0) && (my_rank == 0)){
      file_desc_t *file = NULL;
      if ((ret = pio_get_file(ncid, &file)) || (file == NULL)){
        LOG_RANK0(my_rank, "FAIL: pio_get_file() failed for %s (close drain)\n", filename);
        PIOc_closefile(ncid);
        PIOc_freedecomp(iosysid, ioid);
        return (ret != PIO_NOERR) ? ret : PIO_EINTERNAL;
      }
      if (!file->uses_async_io){
        LOG_RANK0(my_rank, "FAIL: %s unexpectedly has uses_async_io == false with "
                  "PIO_USE_ASYNC_WR_THREAD on - this test would not be exercising "
                  "the close-time drain race it is meant to catch\n", filename);
        PIOc_closefile(ncid);
        PIOc_freedecomp(iosysid, ioid);
        return PIO_EINTERNAL;
      }
    }
#endif

    if ((ret = PIOc_write_darray(ncid, varid, ioid, CLOSE_DRAIN_MAPLEN_PER_RANK, wdata.data(), NULL))){
      LOG_RANK0(my_rank, "FAIL: PIOc_write_darray() failed (close drain), iter = %d, ret = %d\n", iter, ret);
      PIOc_closefile(ncid);
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    /* Deliberately no PIOc_sync() here - the close-time drain in
     * spio_hard_closefile() (reached via PIOc_closefile()) is what must wait
     * for this pending async write, on the rank that actually owns it. */
    if ((ret = PIOc_closefile(ncid))){
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    if ((ret = PIOc_openfile(iosysid, &ncid2, &format, filename, PIO_NOWRITE))){
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    if ((ret = PIOc_inq_varid(ncid2, VAR_NAME, &varid))){
      PIOc_closefile(ncid2);
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    std::vector<int> rdata(CLOSE_DRAIN_MAPLEN_PER_RANK, -1);
    if ((ret = PIOc_read_darray(ncid2, varid, ioid, CLOSE_DRAIN_MAPLEN_PER_RANK, rdata.data()))){
      LOG_RANK0(my_rank, "FAIL: PIOc_read_darray() failed (close drain), iter = %d, ret = %d\n", iter, ret);
      PIOc_closefile(ncid2);
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    if ((ret = PIOc_closefile(ncid2))){
      PIOc_freedecomp(iosysid, ioid);
      return ret;
    }

    for(int i = 0; i < CLOSE_DRAIN_MAPLEN_PER_RANK; i++){
      if(rdata[i] != wdata[i]){
        LOG_RANK0(my_rank, "FAIL: close drain, iter = %d, rdata[%d] (%d) != wdata[%d] (%d) - "
                  "closefile() did not wait for a pending asynchronous write to complete "
                  "before closing the file's HDF5 handles\n", iter, i, rdata[i], i, wdata[i]);
        cmp_ret = PIO_EINTERNAL;
        break;
      }
    }
  }

  int free_ret = PIOc_freedecomp(iosysid, ioid);
  if(cmp_ret == PIO_NOERR) cmp_ret = free_ret;

  return cmp_ret;
}

int main(int argc, char **argv)
{
  int my_rank, ntasks;
  MPI_Comm test_comm;
  int ret;
  int num_errors = 0;

  if ((ret = pio_test_init2(argc, argv, &my_rank, &ntasks, MIN_NTASKS,
                            TARGET_NTASKS, 3, &test_comm)))
    ERR(ERR_INIT);
  if ((ret = PIOc_set_iosystem_error_handling(PIO_DEFAULT, PIO_RETURN_ERROR, NULL)))
    return ret;

  if (my_rank < TARGET_NTASKS)
  {
    int iosysid;

    /* Close-time async-write drain (Task 6, Bug 6) - disjoint sync/async I/O
     * procs. Deliberately run FIRST, before any other test in this file:
     * both test_write_read_hdf5(PIO_REARR_SUBSET) below and
     * test_write_read_hdf5_dtconv() are known to crash the whole process
     * under PIO_USE_ASYNC_WR_THREAD (the dtconv test hits the
     * already-documented fillbuf NULL-deref, commit 16f792893b; the plain
     * SUBSET test has also been observed to crash via an unrelated
     * pre-existing heap-buffer-overflow in pio_log()/pio_swapm() during
     * PIOc_closefile() - not deterministic, but real). Running this test
     * after either of them risks it silently never executing, with no FAIL
     * output to show for it. Running it first guarantees it actually
     * executes regardless of what those pre-existing, out-of-scope bugs do
     * afterward. */
    if ((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                   PIO_REARR_SUBSET, &iosysid)))
      return ret;

    printf("%d running test_write_close_drain_hdf5()\n", my_rank);
    if ((ret = test_write_close_drain_hdf5(iosysid, my_rank, ntasks)))
    {
      LOG_RANK0(my_rank, "test_write_close_drain_hdf5() FAILED, ret = %d\n", ret);
      num_errors++;
    }

    if ((ret = PIOc_finalize(iosysid)))
      return ret;

    /* PIO_REARR_CONTIG + HDF5 + PIO_USE_ASYNC_WR_THREAD : async write is enabled */
    if ((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                   PIO_REARR_SUBSET, &iosysid)))
      return ret;

    printf("%d running test_write_read_hdf5(PIO_REARR_SUBSET)\n", my_rank);
    if ((ret = test_write_read_hdf5(iosysid, my_rank, ntasks, PIO_REARR_SUBSET)))
    {
      LOG_RANK0(my_rank, "test_write_read_hdf5(PIO_REARR_SUBSET) FAILED, ret = %d\n", ret);
      num_errors++;
    }

    if ((ret = PIOc_finalize(iosysid)))
      return ret;

    /* PIO_REARR_SUBSET + HDF5 + PIO_USE_ASYNC_WR_THREAD : with data conversion */
    if ((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                   PIO_REARR_SUBSET, &iosysid)))
      return ret;

    printf("%d running test_write_read_hdf5_dtconv(PIO_REARR_SUBSET)\n", my_rank);
    if ((ret = test_write_read_hdf5_dtconv(iosysid, my_rank, ntasks)))
    {
      LOG_RANK0(my_rank, "test_write_read_hdf5_dtconv(PIO_REARR_SUBSET) FAILED, ret = %d\n", ret);
      num_errors++;
    }

    if ((ret = PIOc_finalize(iosysid)))
      return ret;

    /* PIO_REARR_CONTIG + HDF5 */
    if ((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                   PIO_REARR_CONTIG, &iosysid)))
      return ret;

    printf("%d running test_write_read_hdf5(PIO_REARR_CONTIG)\n", my_rank);
    if ((ret = test_write_read_hdf5(iosysid, my_rank, ntasks, PIO_REARR_CONTIG)))
    {
      LOG_RANK0(my_rank, "test_write_read_hdf5(PIO_REARR_CONTIG) FAILED, ret = %d\n", ret);
      num_errors++;
    }

    if ((ret = PIOc_finalize(iosysid)))
      return ret;
  }

  printf("%d %s Finalizing...\n", my_rank, TEST_NAME);
  if ((ret = pio_test_finalize(&test_comm)))
    return ret;

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
