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
  /* Explicitly confirm the fallback described in the comment above: this
   * decomposition's disjoint sync/async I/O ranks must still route this
   * SUBSET write through sync_io_desc_info, not async_io_desc_info, so this
   * test keeps avoiding the unrelated CONTIG-async deadlock/crash now that
   * select_io_desc_info() no longer excludes SUBSET from the async path. */
  if (my_rank == 0){
    io_desc_t *iodesc = pio_get_iodesc_from_id(ioid);
    iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
    if (iodesc == NULL || ios == NULL){
      LOG_RANK0(my_rank, "FAIL: pio_get_iodesc_from_id/pio_get_iosystem_from_id returned NULL (dtconv)\n");
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
     * iodesc->async_io_desc_info non-NULL and correctly populated even
     * though (per the check below) it ends up unused for this test's writes.
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
    if (&(iodesc->select_io_desc_info(format, ios)) != &(iodesc->sync_io_desc_info)){
      LOG_RANK0(my_rank, "FAIL: test_write_read_hdf5_dtconv's SUBSET decomposition unexpectedly "
                "routed to the async io_desc_info_t - this would resurface the unrelated "
                "CONTIG-async deadlock/crash this test was written to avoid\n");
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
