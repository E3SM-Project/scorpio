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

/* Write and read back a 1D array using HDF5+PIO_REARR_SUBSET, where
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
