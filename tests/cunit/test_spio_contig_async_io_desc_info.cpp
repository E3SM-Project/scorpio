/*
 * Tests for async_io_desc_info allocation for PIO_REARR_CONTIG decomps
 */
#include "pio_config.h"

#if PIO_USE_ASYNC_WR_THREAD

#include <vector>
#include <numeric>
#include <cassert>
#include <cstdlib>

#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"

/* FIXME: Make sure that the test runs on any number of MPI tasks */
/* The maximum number of tasks this test should run on. */
#define MAX_NTASKS 4

/* The minimum number of tasks this test should run on - need at least 2 so
 * the zero-maplen edge case test can give one rank an empty compmap while
 * other ranks still have data. */
#define MIN_NTASKS 2

/* The name of this test. */
#define TEST_NAME "test_spio_contig_async_io_desc_info"

/* For 1-D use. */
#define NDIM1 1

/* Used when initializing PIO. */
#define STRIDE1 1
#define BASE0 0
#define NUM_IO1 1

static const int FAIL = -1;

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

/* Check if sync and async I/O desc infos are allocated and initialized */
static int check_async_io_desc_info(int my_rank, io_desc_t *iodesc)
{
  assert(iodesc);

  if(iodesc->async_io_desc_info == NULL){
    LOG_RANK0(my_rank, "FAIL: async_io_desc_info is NULL for a CONTIG decomp with PIO_USE_ASYNC_WR_THREAD enabled\n");
    return PIO_EINTERNAL;
  }

  io_desc_info_t &sync_info = iodesc->sync_io_desc_info;
  io_desc_info_t &async_info = *(iodesc->async_io_desc_info);

  int ret = PIO_NOERR;
  if(async_info.maplen != sync_info.maplen){
    LOG_RANK0(my_rank, "FAIL: async_io_desc_info.maplen (%d) != sync_io_desc_info.maplen (%d)\n",
              async_info.maplen, sync_info.maplen);
    ret = PIO_EINTERNAL;
  }
  if(async_info.ndims != sync_info.ndims){
    LOG_RANK0(my_rank, "FAIL: async_io_desc_info.ndims (%d) != sync_io_desc_info.ndims (%d)\n",
              async_info.ndims, sync_info.ndims);
    ret = PIO_EINTERNAL;
  }
  else{
    for(int i = 0; i < sync_info.ndims; i++){
      if(async_info.dimlen[i] != sync_info.dimlen[i]){
        LOG_RANK0(my_rank, "FAIL: async_io_desc_info.dimlen[%d] (%d) != sync_io_desc_info.dimlen[%d] (%d)\n",
                  i, async_info.dimlen[i], i, sync_info.dimlen[i]);
        ret = PIO_EINTERNAL;
      }
    }
  }

  return ret;
}

/* Basic case: every rank has a non-empty local compmap. */
static int test_contig_async_io_desc_info_basic(int iosysid, int my_rank, int ntasks)
{
  int ioid;
  int ret;
  const int MAPLEN = 2;
  /* compmap is 1-based */
  PIO_Offset compmap[MAPLEN] = { static_cast<PIO_Offset>(my_rank * MAPLEN + 1),
                                  static_cast<PIO_Offset>(my_rank * MAPLEN + 2) };
  const int gdimlen[NDIM1] = { MAPLEN * ntasks };

  if ((ret = PIOc_init_decomp(iosysid, PIO_INT, NDIM1, gdimlen, MAPLEN,
                              compmap, &ioid, PIO_REARR_CONTIG, NULL, NULL)))
    return ret;

  io_desc_t *iodesc = pio_get_iodesc_from_id(ioid);
  if(!iodesc){
    LOG_RANK0(my_rank, "FAIL: pio_get_iodesc_from_id() returned NULL for ioid = %d\n", ioid);
    PIOc_freedecomp(iosysid, ioid);
    return PIO_EINTERNAL;
  }

  ret = check_async_io_desc_info(my_rank, iodesc);

  int free_ret = PIOc_freedecomp(iosysid, ioid);
  if(ret == PIO_NOERR) ret = free_ret;

  return ret;
}

/* Zero maplen case: rank 0 gets an empty local compmap (maplen == 0), every other
 * rank gets a non-empty one (maplen = MAPLEN_PER_NONZERO_RANK)
 */
static int test_contig_async_io_desc_info_zero_maplen(int iosysid, int my_rank, int ntasks)
{
  int ioid;
  int ret;
  const int MAPLEN_PER_NONZERO_RANK = 2;

  int maplen = (my_rank == 0) ? 0 : MAPLEN_PER_NONZERO_RANK;
  std::vector<PIO_Offset> compmap(maplen);
  if(maplen > 0){
    PIO_Offset start = static_cast<PIO_Offset>((my_rank - 1) * MAPLEN_PER_NONZERO_RANK + 1);
    std::iota(compmap.begin(), compmap.end(), start);
  }

  const int gdimlen[NDIM1] = { MAPLEN_PER_NONZERO_RANK * (ntasks - 1) };

  if ((ret = PIOc_init_decomp(iosysid, PIO_INT, NDIM1, gdimlen, maplen,
                              (maplen > 0) ? compmap.data() : NULL, &ioid,
                              PIO_REARR_CONTIG, NULL, NULL)))
    return ret;

  io_desc_t *iodesc = pio_get_iodesc_from_id(ioid);
  if(!iodesc){
    LOG_RANK0(my_rank, "FAIL: pio_get_iodesc_from_id() returned NULL for ioid = %d\n", ioid);
    PIOc_freedecomp(iosysid, ioid);
    return PIO_EINTERNAL;
  }

  ret = check_async_io_desc_info(my_rank, iodesc);
  if((ret == PIO_NOERR) && (my_rank == 0) && (iodesc->async_io_desc_info->maplen != 0)){
    LOG_RANK0(my_rank, "FAIL: rank 0's async_io_desc_info.maplen (%d) is not 0\n",
              iodesc->async_io_desc_info->maplen);
    ret = PIO_EINTERNAL;
  }

  int free_ret = PIOc_freedecomp(iosysid, ioid);
  if(ret == PIO_NOERR) ret = free_ret;

  return ret;
}

int main(int argc, char **argv)
{
  int my_rank, ntasks;
  MPI_Comm test_comm;
  int ret;
  int num_errors = 0;

  if ((ret = pio_test_init2(argc, argv, &my_rank, &ntasks, MIN_NTASKS,
                            MAX_NTASKS, 3, &test_comm)))
    ERR(ERR_INIT);
  if ((ret = PIOc_set_iosystem_error_handling(PIO_DEFAULT, PIO_RETURN_ERROR, NULL)))
    return ret;

  /* Test code runs on MAX_NTASKS tasks. The left over tasks do nothing. */
  if (my_rank < MAX_NTASKS)
  {
    int iosysid;
    if ((ret = PIOc_Init_Intracomm(test_comm, NUM_IO1, STRIDE1, BASE0,
                                   PIO_REARR_CONTIG, &iosysid)))
      return ret;

    printf("%d running test_contig_async_io_desc_info_basic\n", my_rank);
    if ((ret = test_contig_async_io_desc_info_basic(iosysid, my_rank, ntasks)))
    {
      LOG_RANK0(my_rank, "test_contig_async_io_desc_info_basic() FAILED, ret = %d\n", ret);
      num_errors++;
    }

    printf("%d running test_contig_async_io_desc_info_zero_maplen\n", my_rank);
    if ((ret = test_contig_async_io_desc_info_zero_maplen(iosysid, my_rank, ntasks)))
    {
      LOG_RANK0(my_rank, "test_contig_async_io_desc_info_zero_maplen() FAILED, ret = %d\n", ret);
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

#else /* PIO_USE_ASYNC_WR_THREAD */

/* Eager async_io_desc_info allocation only happens when the async I/O
 * service (PIO_USE_ASYNC_WR_THREAD) is enabled - nothing to test otherwise.
 * (The flag-off behavior - async_io_desc_info staying NULL - is covered as a
 * regression check by test_spio_select_io_desc_info.cpp's
 * test_select_contig_no_async(), which runs unconditionally.) */
int main(int argc, char *argv[])
{
  return 0;
}

#endif /* PIO_USE_ASYNC_WR_THREAD */
