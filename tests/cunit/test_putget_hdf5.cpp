/*
 * Test non-decomposed PIOc_put_var/PIOc_get_var round-tripping on a single
 * rank using the HDF5 I/O type.
 *
 * This is a single-task test (sync I/O proc and, when PIO_USE_ASYNC_WR_THREAD
 * is on, the async I/O proc that owns a file's queued HDF5 writes - see
 * SPIO_Util::Iosys_Util::is_file_owning_io_proc() - are necessarily the same
 * proc here), deliberately avoiding the harder disjoint sync/async I/O procs
 * topology (covered separately, e.g. by test_darray_hdf5_contig.cpp for the
 * decomposed write path).
 *
 * Goal: with PIO_USE_ASYNC_WR_THREAD on, a non-decomposed PIOc_put_var*()
 * call on an HDF5 file with file->uses_async_io == true dispatches the
 * write to the async write thread pool (see
 * spio_iosys_async_hdf5_put_var_op_add() in spio_async_hdf5_utils.cpp) and
 * returns before that write necessarily completes. SCORPIO does not
 * guarantee that a following PIOc_get_var*() on the same file sees that
 * write unless the caller calls PIOc_sync() (or closes/reopens the file) in
 * between - PIOc_sync_impl() (pio_file.cpp) is what actually drains this
 * file's pending async writes via spio_wait_file_async_pend_ops(). This test
 * confirms PIOc_sync() is sufficient to make the round trip reliable.
 * Looping the put/sync/get round trip several times increases the odds of
 * observing a stale read if that drain is ever missing or broken - a single
 * iteration is not reliably flaky.
 */
#include "pio_config.h"

#ifdef _HDF5

#include <vector>

#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"

/* Single rank test - keeps the sync and (if async write is enabled) async
 * I/O procs identical, which is this test's whole point (see file header). */
#define TARGET_NTASKS 1
#define MIN_NTASKS 1

#define TEST_NAME "test_putget_hdf5"

#define NDIM1 1
#define DIMLEN1 4

#define VAR_NAME "putget_hdf5_var"

/* Number of put_var/sync/get_var round trips to try, to surface a stale
 * read if PIOc_sync()'s drain of this file's pending async writes
 * (spio_wait_file_async_pend_ops(), see PIOc_sync_impl() in pio_file.cpp)
 * is ever missing or broken. */
#define NUM_ITERS 20

static const int FAIL = -1;

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

/* Write, sync, then read back a non-decomposed PIO_INT 1D variable,
 * NUM_ITERS times, using the HDF5 I/O type. */
static int test_put_get_var_hdf5(int iosysid, int my_rank)
{
  int ret;
  int ncid;
  int dimid, varid;
  int format = PIO_IOTYPE_HDF5;
  char filename[PIO_MAX_NAME + 1];

  snprintf(filename, PIO_MAX_NAME + 1, "%s.nc", TEST_NAME);

  if ((ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER)))
    return ret;

  if ((ret = PIOc_def_dim(ncid, "x", (PIO_Offset)DIMLEN1, &dimid))){
    PIOc_closefile(ncid);
    return ret;
  }

  if ((ret = PIOc_def_var(ncid, VAR_NAME, PIO_INT, NDIM1, &dimid, &varid))){
    PIOc_closefile(ncid);
    return ret;
  }

  if ((ret = PIOc_enddef(ncid))){
    PIOc_closefile(ncid);
    return ret;
  }

#if PIO_USE_ASYNC_WR_THREAD
  /* Confirm this file is actually routed through the async write thread
   * pool - otherwise this test would pass trivially without exercising the
   * race/fix this test is for. See spio_createfile_int(): a created file is
   * always write-capable, and this is a plain PIOc_Init_Intracomm iosystem
   * (not an intercomm async I/O service), so uses_async_io should be true. */
  {
    file_desc_t *file = NULL;
    if ((ret = pio_get_file(ncid, &file)) || (file == NULL)){
      LOG_RANK0(my_rank, "FAIL: pio_get_file() failed for %s\n", filename);
      PIOc_closefile(ncid);
      return (ret != PIO_NOERR) ? ret : PIO_EINTERNAL;
    }
    if (!file->uses_async_io){
      LOG_RANK0(my_rank, "FAIL: %s unexpectedly has uses_async_io == false with "
                "PIO_USE_ASYNC_WR_THREAD on - this test would not be exercising "
                "PIOc_sync()'s drain of a pending asynchronous write\n",
                filename);
      PIOc_closefile(ncid);
      return PIO_EINTERNAL;
    }
  }
#endif

  int cmp_ret = PIO_NOERR;
  for(int iter = 0; iter < NUM_ITERS; iter++){
    std::vector<int> wdata(DIMLEN1);
    for(int i = 0; i < DIMLEN1; i++){
      wdata[i] = iter * 1000 + i;
    }

    if ((ret = PIOc_put_var_int(ncid, varid, wdata.data()))){
      LOG_RANK0(my_rank, "FAIL: PIOc_put_var_int() failed, iter = %d, ret = %d\n", iter, ret);
      PIOc_closefile(ncid);
      return ret;
    }

    /* Required for a following read on the same file to be guaranteed to
     * see this write - see PIOc_sync_impl() in pio_file.cpp. */
    if ((ret = PIOc_sync(ncid))){
      LOG_RANK0(my_rank, "FAIL: PIOc_sync() failed, iter = %d, ret = %d\n", iter, ret);
      PIOc_closefile(ncid);
      return ret;
    }

    std::vector<int> rdata(DIMLEN1, -1);
    if ((ret = PIOc_get_var_int(ncid, varid, rdata.data()))){
      LOG_RANK0(my_rank, "FAIL: PIOc_get_var_int() failed, iter = %d, ret = %d\n", iter, ret);
      PIOc_closefile(ncid);
      return ret;
    }

    for(int i = 0; i < DIMLEN1; i++){
      if(rdata[i] != wdata[i]){
        LOG_RANK0(my_rank, "FAIL: iter = %d, rdata[%d] (%d) != wdata[%d] (%d) - read did not "
                  "see the prior write despite an intervening PIOc_sync()\n", iter, i, rdata[i], i, wdata[i]);
        cmp_ret = PIO_EINTERNAL;
      }
    }
  }

  if ((ret = PIOc_closefile(ncid))){
    if(cmp_ret == PIO_NOERR) cmp_ret = ret;
  }

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

    if ((ret = PIOc_Init_Intracomm(test_comm, 1, 1, 0, PIO_REARR_SUBSET, &iosysid)))
      return ret;

    printf("%d running test_put_get_var_hdf5()\n", my_rank);
    if ((ret = test_put_get_var_hdf5(iosysid, my_rank)))
    {
      LOG_RANK0(my_rank, "test_put_get_var_hdf5() FAILED, ret = %d\n", ret);
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
