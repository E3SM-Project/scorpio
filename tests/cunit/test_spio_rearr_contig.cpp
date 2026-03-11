#include <cassert>

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_tests.h"

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

/* Test the find_region() function with a simple 1-D contiguous map. */
int test_find_region_1d(int wrank)
{
    const int ndims = 1;
    int gdimlen[1] = {4};
    int maplen = 1;
    PIO_Offset map[1] = {1};
    PIO_Offset start[1];
    PIO_Offset count[1];
    PIO_Offset regionlen;

    /* Call the function under test. */
    regionlen = find_region(ndims, gdimlen, maplen, map, start, count);

    /* Check results: single element at index 0. */
    if (regionlen != 1 || start[0] != 0 || count[0] != 1)
    {
        LOG_RANK0(wrank, "test_find_region_1d() failed: "
            "expected regionlen=1, start[0]=0, count[0]=1, "
            "got regionlen=%lld, start[0]=%lld, count[0]=%lld\n",
            regionlen, start[0], count[0]);
        return PIO_EINTERNAL;
    }

    return PIO_NOERR;
}

/* Test find_region() with a contiguous 1-D map of length > 1. */
int test_find_region_1d_contiguous(int wrank)
{
    const int ndims = 1;
    int gdimlen[1] = {8};
    int maplen = 4;
    /* 1-based map: elements 1,2,3,4 are contiguous */
    PIO_Offset map[4] = {1, 2, 3, 4};
    PIO_Offset start[1];
    PIO_Offset count[1];
    PIO_Offset regionlen;

    /* Call the function under test. */
    regionlen = find_region(ndims, gdimlen, maplen, map, start, count);

    /* Check results: 4 contiguous elements starting at index 0. */
    if (regionlen != 4 || start[0] != 0 || count[0] != 4)
    {
        LOG_RANK0(wrank, "test_find_region_1d_contiguous() failed: "
            "expected regionlen=4, start[0]=0, count[0]=4, "
            "got regionlen=%lld, start[0]=%lld, count[0]=%lld\n",
            regionlen, start[0], count[0]);
        return PIO_EINTERNAL;
    }

    return PIO_NOERR;
}

/* Test the expand_region() function with a trivial 1-D case. */
int test_expand_region_1d(int wrank)
{
    const int dim = 0;
    int gdims[1] = {1};
    int maplen = 1;
    PIO_Offset map[1] = {5};
    int region_size = 1;
    int region_stride = 1;
    int max_size[1] = {10};
    PIO_Offset count[1] = {0};

    /* Call the function under test. */
    expand_region(dim, gdims, maplen, map, region_size, region_stride,
                  max_size, count);

    /* With gdims[0]=1, only one element fits; count[0] should be 1. */
    if (count[0] != 1)
    {
        LOG_RANK0(wrank,
            "test_expand_region_1d() failed: expected count[0]=1, got count[0]=%lld\n",
            count[0]);
        return PIO_EINTERNAL;
    }

    return PIO_NOERR;
}

/* Test expand_region() with room to expand along a 1-D dimension. */
int test_expand_region_1d_multi(int wrank)
{
    const int dim = 0;
    int gdims[1] = {8};
    int maplen = 4;
    /* 1-based contiguous map: 1,2,3,4 */
    PIO_Offset map[4] = {1, 2, 3, 4};
    int region_size = 1;
    int region_stride = 1;
    int max_size[1] = {8};
    PIO_Offset count[1] = {0};

    /* Call the function under test. */
    expand_region(dim, gdims, maplen, map, region_size, region_stride,
                  max_size, count);

    /* All 4 elements are contiguous, so count[0] should be 4. */
    if (count[0] != 4)
    {
        LOG_RANK0(wrank,
            "test_expand_region_1d_multi() failed: expected count[0]=4, got count[0]=%lld\n",
            count[0]);
        return PIO_EINTERNAL;
    }

    return PIO_NOERR;
}

/* Test the get_regions() function with a simple 1-D map. */
int test_get_regions_1d(int wrank)
{
    const int ndims = 1;
    const int gdimlen[1] = {8};
    /* Two non-contiguous elements */
    const int maplen = 2;
    /* 1-based: element 1 and element 3 are not contiguous */
    PIO_Offset map[2] = {1, 3};
    int maxregions;
    io_region *ior = NULL;
    int ret;

    /* Allocate a region. */
    if ((ret = alloc_region2(NULL, ndims, &ior)))
    {
        LOG_RANK0(wrank,
            "test_get_regions_1d() failed: alloc_region2() returned %d\n", ret);
        return ret;
    }
    ior->next = NULL;
    ior->count[0] = 1;

    /* Call the function under test. */
    if ((ret = get_regions(ndims, gdimlen, maplen, map, &maxregions, ior)))
    {
        free_region_list(ior);
        LOG_RANK0(wrank,
            "test_get_regions_1d() failed: get_regions() returned %d\n", ret);
        return ret;
    }

    /* Two non-contiguous elements should produce two regions. */
    if (maxregions != 2)
    {
        free_region_list(ior);
        LOG_RANK0(wrank,
            "test_get_regions_1d() failed: expected maxregions=2, got %d\n",
            maxregions);
        return PIO_EINTERNAL;
    }

    /* Free the region list. */
    free_region_list(ior);

    return PIO_NOERR;
}

/* Test get_regions() with a contiguous 1-D map (should yield one region). */
int test_get_regions_1d_contiguous(int wrank)
{
    const int ndims = 1;
    const int gdimlen[1] = {8};
    const int maplen = 4;
    /* 1-based contiguous map: elements 1,2,3,4 */
    PIO_Offset map[4] = {1, 2, 3, 4};
    int maxregions;
    io_region *ior = NULL;
    int ret;

    /* Allocate a region. */
    if ((ret = alloc_region2(NULL, ndims, &ior)))
    {
        LOG_RANK0(wrank,
            "test_get_regions_1d_contiguous() failed: alloc_region2() returned %d\n", ret);
        return ret;
    }
    ior->next = NULL;
    ior->count[0] = 1;

    /* Call the function under test. */
    if ((ret = get_regions(ndims, gdimlen, maplen, map, &maxregions, ior)))
    {
        free_region_list(ior);
        LOG_RANK0(wrank,
            "test_get_regions_1d_contiguous() failed: get_regions() returned %d\n", ret);
        return ret;
    }

    /* Four contiguous elements should produce one region. */
    if (maxregions != 1)
    {
        free_region_list(ior);
        LOG_RANK0(wrank,
            "test_get_regions_1d_contiguous() failed: expected maxregions=1, got %d\n",
            maxregions);
        return PIO_EINTERNAL;
    }

    /* Free the region list. */
    free_region_list(ior);

    return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
    int nerrs = 0, ret = PIO_NOERR;
    assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);

    /* Test find_region() with a single-element 1-D map. */
    try
    {
        ret = test_find_region_1d(wrank);
    }
    catch(...)
    {
        ret = PIO_EINTERNAL;
    }
    if (ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_find_region_1d() FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else
    {
        LOG_RANK0(wrank, "test_find_region_1d() PASSED\n");
    }

    /* Test find_region() with a contiguous multi-element 1-D map. */
    try
    {
        ret = test_find_region_1d_contiguous(wrank);
    }
    catch(...)
    {
        ret = PIO_EINTERNAL;
    }
    if (ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_find_region_1d_contiguous() FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else
    {
        LOG_RANK0(wrank, "test_find_region_1d_contiguous() PASSED\n");
    }

    /* Test expand_region() with a trivial 1-D case. */
    try
    {
        ret = test_expand_region_1d(wrank);
    }
    catch(...)
    {
        ret = PIO_EINTERNAL;
    }
    if (ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_expand_region_1d() FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else
    {
        LOG_RANK0(wrank, "test_expand_region_1d() PASSED\n");
    }

    /* Test expand_region() with room to expand along a 1-D dimension. */
    try
    {
        ret = test_expand_region_1d_multi(wrank);
    }
    catch(...)
    {
        ret = PIO_EINTERNAL;
    }
    if (ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_expand_region_1d_multi() FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else
    {
        LOG_RANK0(wrank, "test_expand_region_1d_multi() PASSED\n");
    }

    /* Test get_regions() with two non-contiguous elements. */
    try
    {
        ret = test_get_regions_1d(wrank);
    }
    catch(...)
    {
        ret = PIO_EINTERNAL;
    }
    if (ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_get_regions_1d() FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else
    {
        LOG_RANK0(wrank, "test_get_regions_1d() PASSED\n");
    }

    /* Test get_regions() with four contiguous elements. */
    try
    {
        ret = test_get_regions_1d_contiguous(wrank);
    }
    catch(...)
    {
        ret = PIO_EINTERNAL;
    }
    if (ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_get_regions_1d_contiguous() FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else
    {
        LOG_RANK0(wrank, "test_get_regions_1d_contiguous() PASSED\n");
    }

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
    if (ret != 0)
    {
        LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
        return ret;
    }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

    ret = MPI_Init(&argc, &argv);
    if (ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Init() FAILED, ret = %d\n", ret);
        return ret;
    }

    ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
    if (ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
        return ret;
    }
    ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
    if (ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Comm_size() FAILED, ret = %d\n", ret);
        return ret;
    }

    num_errors = 0;
    ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
    if (ret != 0)
    {
        LOG_RANK0(wrank, "Test driver FAILED\n");
        return FAIL;
    }
    else
    {
        LOG_RANK0(wrank, "All tests PASSED\n");
    }

    MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
    ret = GPTLfinalize();
    if (ret != 0)
    {
        LOG_RANK0(wrank, "GPTLfinalize() FAILED, ret = %d\n", ret);
        return ret;
    }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

    if (num_errors != 0)
    {
        LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
        return FAIL;
    }
    return 0;
}
