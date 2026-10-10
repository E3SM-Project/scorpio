/*
 * This program tests some internal functions in the library related
 * to the box and subset rearranger, and the transfer of data betweeen
 * IO and computation tasks.
 *
 * Ed Hartnett, 3/9/17
 */
#include <pio.h>
#include <pio_tests.h>
#include <pio_internal.h>
#include "spio_iosys_utils.hpp"
#if SIZEOF_PIO_OFFSET >= 8
#include <stdint.h>
#endif

/* The number of tasks this test should run on. */
#define TARGET_NTASKS 4

/* The minimum number of tasks this test should run on. */
#define MIN_NTASKS 1

/* The name of this test. */
#define TEST_NAME "test_rearr"

/* For 1-D use. */
#define NDIM1 1

/* For maplens of 2. */
#define MAPLEN2 2

/* Name of test var. (Name of a Welsh town.)*/
#define VAR_NAME "Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch"

iosystem_desc_t *create_dummy_iosystem(MPI_Comm test_comm)
{
  iosystem_desc_t *ios = static_cast<iosystem_desc_t *>(calloc(1, sizeof(iosystem_desc_t)));
  assert(ios);

  ios->union_comm = test_comm;

  return ios;
}

/* Test some of the rearranger utility functions. */
int test_rearranger_opts1(int iosysid)
{
    iosystem_desc_t *ios;
    int ret;

    /* This should not work. */
    if (PIOc_set_rearr_opts(TEST_VAL_42, 0, 0, false, false, 0, false,
                            false, 0) != PIO_EBADID)
        return ERR_WRONG;
    if (PIOc_set_rearr_opts(iosysid, TEST_VAL_42, 0, false, false, 0, false,
                            false, 0) != PIO_EINVAL)
        return ERR_WRONG;
    if (PIOc_set_rearr_opts(iosysid, 0, TEST_VAL_42, false, false, 0, false,
                            false, 0) != PIO_EINVAL)
        return ERR_WRONG;
    if (PIOc_set_rearr_opts(iosysid, 0, 0, false, false,
                            PIO_REARR_COMM_UNLIMITED_PEND_REQ - 1, false,
                            false, 0) != PIO_EINVAL)
        return ERR_WRONG;
    if (PIOc_set_rearr_opts(iosysid, 0, 0, false, false, 0, false,
                            false, PIO_REARR_COMM_UNLIMITED_PEND_REQ - 1) !=
        PIO_EINVAL)
        return ERR_WRONG;

    /* This should work. */
    if ((ret = PIOc_set_rearr_opts(iosysid, PIO_REARR_COMM_P2P,
                                   PIO_REARR_COMM_FC_1D_COMP2IO, true,
                                   true, TEST_VAL_42, true, true, TEST_VAL_42 + 1)))
        return ret;

    /* Get the IO system info from the id. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__, "Getting I/O system from id failed");

    /* Check the rearranger comp2io settings. */
    if (ios->rearr_opts.comm_type != PIO_REARR_COMM_P2P ||
        ios->rearr_opts.fcd != PIO_REARR_COMM_FC_1D_COMP2IO ||
        !ios->rearr_opts.comp2io.hs || !ios->rearr_opts.comp2io.isend ||
        ios->rearr_opts.comp2io.max_pend_req != TEST_VAL_42)
        return ERR_WRONG;

    return 0;
}

/* Test the compare_offsets() function. */
int test_compare_offsets()
{
    mapsort m1, m2, m3;
#if SIZEOF_PIO_OFFSET >= 8
    mapsort m_max, m_min;
#endif

    m1.rfrom = 0;
    m1.soffset = 0;
    m1.iomap = 0;
    m2.rfrom = 0;
    m2.soffset = 0;
    m2.iomap = 0;
    m3.rfrom = 0;
    m3.soffset = 0;
    m3.iomap = 1;
#if SIZEOF_PIO_OFFSET >= 8
    m_max.rfrom = 0;
    m_max.soffset = 0;
    m_max.iomap = INT64_MAX;
    m_min.rfrom = 0;
    m_min.soffset = 0;
    m_min.iomap = INT64_MIN;
#endif

    /* Return 0 if either or both parameters are null. */
    if (compare_offsets(NULL, &m2))
        return ERR_WRONG;
    if (compare_offsets(&m1, NULL))
        return ERR_WRONG;
    if (compare_offsets(NULL, NULL))
        return ERR_WRONG;

    /* m1 and m2 are the same. */
    if (compare_offsets(&m1, &m2))
        return ERR_WRONG;

    /* m1 and m3 are different. */
    if (compare_offsets(&m1, &m3) != -1)
        return ERR_WRONG;

#if SIZEOF_PIO_OFFSET >= 8
    /* Some representative test cases for 64-bit offset comparison with extreme values */

    /* Test: INT64_MAX - INT64_MIN, expected to be positive */
    if (compare_offsets(&m_max, &m_min) <= 0)
        return ERR_WRONG;

    /* Test: INT64_MIN - INT64_MAX, expected to be negative */
    if (compare_offsets(&m_min, &m_max) >= 0)
        return ERR_WRONG;

    /* Test: INT64_MAX - 0, expected to be positive */
    if (compare_offsets(&m_max, &m1) <= 0)
        return ERR_WRONG;

    /* Test: 0 - INT64_MAX, expected to be negative */
    if (compare_offsets(&m1, &m_max) >= 0)
        return ERR_WRONG;

    /* Test: INT64_MIN - 0, expected to be negative */
    if (compare_offsets(&m_min, &m1) >= 0)
        return ERR_WRONG;

    /* Test: 0 - INT64_MIN, expected to be positive */
    if (compare_offsets(&m1, &m_min) <= 0)
        return ERR_WRONG;
#endif

    return 0;
}

/* Test the ceil2() and pair() functions. */
int test_ceil2_pair()
{
    /* Test the ceil2() function. */
    if (ceil2(1) != 1)
        return ERR_WRONG;
    if (ceil2(-100) != 1)
        return ERR_WRONG;
    if (ceil2(2) != 2)
        return ERR_WRONG;
    if (ceil2(3) != 4)
        return ERR_WRONG;
    if (ceil2(16) != 16)
        return ERR_WRONG;
    if (ceil2(17) != 32)
        return ERR_WRONG;

    /* Test the pair() function. */
    if (pair(4, 0, 0) != 1)
        return ERR_WRONG;
    if (pair(4, 2, 2) != 1)
        return ERR_WRONG;

    return 0;
}

/* Test the create_mpi_datatypes() function.
 * @returns 0 for success, error code otherwise.*/
int test_create_mpi_datatypes()
{
    MPI_Datatype basetype = MPI_INT;
    int *mfrom = NULL;
    int mpierr;
    int ret;

    {
        int msgcnt = 1;
        PIO_Offset mindex[1] = {0};
        int mcount[1] = {1};
        MPI_Datatype mtype;

        /* Create an MPI data type. */
        if ((ret = create_mpi_datatypes(basetype, msgcnt, mindex, mcount, mfrom, &mtype)))
            return ret;

        /* Free the type. */
        if ((mpierr = MPI_Type_free(&mtype)))
            MPIERR(mpierr);
    }

    {
        int msgcnt = 4;
        PIO_Offset mindex[4] = {0, 0, 0, 0};
        int mcount[4] = {1, 1, 1, 1};
        MPI_Datatype mtype2[4];

        /* Create 4 MPI data types. */
        if ((ret = create_mpi_datatypes(basetype, msgcnt, mindex, mcount, mfrom, mtype2)))
            return ret;

        /* Check the size of the data types. It should be 4. */
        MPI_Aint lb, extent;
        for (int t = 0; t < 4; t++)
        {
            if ((mpierr = MPI_Type_get_extent(mtype2[t], &lb, &extent)))
                MPIERR(mpierr);
            printf("t = %d lb = %ld extent = %ld\n", t, lb, extent);
            if (lb != 0 || extent != 4)
                return ERR_WRONG;
        }

        /* Free them. */
        for (int t = 0; t < 4; t++)
            if ((mpierr = MPI_Type_free(&mtype2[t])))
                return ERR_WRONG;
    }

    return 0;
}

/* Test the idx_to_dim_list() function. */
/*
int test_idx_to_dim_list()
{
    int ndims = 1;
    int gdims[1] = {1};
    PIO_Offset idx = 0;
    PIO_Offset dim_list[1];

    idx_to_dim_list(ndims, gdims, idx, dim_list);

    if (dim_list[0] != 0)
        return ERR_WRONG;

    int ndims2 = 2;
    int gdims2[2] = {3, 2};
    PIO_Offset idx2 = 4;
    PIO_Offset dim_list2[2];

    idx_to_dim_list(ndims2, gdims2, idx2, dim_list2);
    printf("dim_list2[0] = %lld\n", dim_list2[0]);
    printf("dim_list2[1] = %lld\n", dim_list2[1]);

    if (dim_list2[0] != 2 || dim_list2[1] != 0)
        return ERR_WRONG;

    return 0;
}
*/

/* Test the coord_to_lindex() function. */
/*
int test_coord_to_lindex()
{
    int ndims = 1;
    PIO_Offset lcoord[1] = {0};
    PIO_Offset count[1] = {1};
    PIO_Offset lindex;

    lindex = coord_to_lindex(ndims, lcoord, count);
    if (lindex != 0)
        return ERR_WRONG;

    int ndims2 = 2;
    PIO_Offset lcoord2[2] = {0, 0};
    PIO_Offset count2[2] = {1, 1};
    PIO_Offset lindex2;

    lindex2 = coord_to_lindex(ndims2, lcoord2, count2);
    if (lindex2 != 0)
        return ERR_WRONG;

    int ndims3 = 2;
    PIO_Offset lcoord3[2] = {1, 2};
    PIO_Offset count3[2] = {1, 1};
    PIO_Offset lindex3;

    lindex3 = coord_to_lindex(ndims3, lcoord3, count3);
    printf("lindex = %lld\n", lindex3);
    if (lindex3 != 3)
        return ERR_WRONG;

    return 0;
}
*/

/* Test compute_maxIObuffersize() function. */
int test_compute_maxIObuffersize(MPI_Comm test_comm, int my_rank)
{
    int ret;

    iosystem_desc_t *ios = create_dummy_iosystem(test_comm);
    {
        /* This is a simple test with one region containing 1 data
         * element. */
        io_desc_t iodesc(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
        io_region *ior1;
        int ndims = 1;

        iodesc.sync_io_desc_info.free_all_regions();
        /* This is how we allocate a region. */
        if ((ret = alloc_region2(NULL, ndims, &ior1)))
            return ret;
        ior1->next = NULL;
        ior1->count[0] = 1;

        iodesc.sync_io_desc_info.firstregion = ior1;
        iodesc.sync_io_desc_info.ndims = 1;

        /* Run the function. Simplest possible case. */
        if ((ret = compute_maxIObuffersize(test_comm, iodesc.sync_io_desc_info)))
            return ret;
        if (iodesc.sync_io_desc_info.maxiobuflen != 1)
            return ERR_WRONG;

        /* Free resources for the region. */
        free(ior1->start);
        free(ior1->count);
        free(ior1);
        iodesc.sync_io_desc_info.firstregion = NULL; 
    }

    {
        /* This also has a single region, but with 2 dims and count
         * values > 1. */
        io_desc_t iodesc(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
        io_region *ior2;
        int ndims = 2;

        iodesc.sync_io_desc_info.free_all_regions();
        /* This is how we allocate a region. */
        if ((ret = alloc_region2(NULL, ndims, &ior2)))
            return ret;

        /* These should be 0. */
        for (int i = 0; i < ndims; i++)
            if (ior2->start[i] != 0 || ior2->count[i] != 0)
                return ERR_WRONG;

        ior2->next = NULL;
        ior2->count[0] = 10;
        ior2->count[1] = 2;

        iodesc.sync_io_desc_info.firstregion = ior2;
        iodesc.sync_io_desc_info.ndims = 2;

        /* Run the function. */
        if ((ret = compute_maxIObuffersize(test_comm, iodesc.sync_io_desc_info)))
            return ret;
        if (iodesc.sync_io_desc_info.maxiobuflen != 20)
            return ERR_WRONG;

        /* Free resources for the region. */
        free(ior2->start);
        free(ior2->count);
        free(ior2);
        iodesc.sync_io_desc_info.firstregion = NULL;
    }

    {
        /* This test has two regions of different sizes. */
        io_desc_t iodesc(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
        io_region *ior3;
        io_region *ior4;
        int ndims = 2;

        iodesc.sync_io_desc_info.free_all_regions();
        /* This is how we allocate a region. */
        if ((ret = alloc_region2(NULL, ndims, &ior4)))
            return ret;
        ior4->next = NULL;
        ior4->count[0] = 10;
        ior4->count[1] = 2;

        if ((ret = alloc_region2(NULL, ndims, &ior3)))
            return ret;
        ior3->next = ior4;
        ior3->count[0] = 100;
        ior3->count[1] = 5;

        iodesc.sync_io_desc_info.firstregion = ior3;
        iodesc.sync_io_desc_info.ndims = 2;

        /* Run the function. */
        if ((ret = compute_maxIObuffersize(test_comm, iodesc.sync_io_desc_info)))
            return ret;
        printf("iodesc.sync_io_desc_info.maxiobuflen = %lld\n", (unsigned long long) (iodesc.sync_io_desc_info.maxiobuflen));
        if (iodesc.sync_io_desc_info.maxiobuflen != 520)
            return ERR_WRONG;

        /* Free resources for the region. */
        free(ior4->start);
        free(ior4->count);
        free(ior4);
        free(ior3->start);
        free(ior3->count);
        free(ior3);
        iodesc.sync_io_desc_info.firstregion = NULL;
    }

    free(ios);

    return 0;
}

/* Tests for determine_fill() function. */
int test_determine_fill(MPI_Comm test_comm)
{
    iosystem_desc_t *ios;
    int gsize[1] = {4};
    PIO_Offset compmap[1] = {1};
    int ret;

    /* Initialize ios. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;
    ios->union_comm = test_comm;

    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    /* Set up iodesc for test. */
    iodesc->sync_io_desc_info.ndims = 1;
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_SUBSET;
    iodesc->sync_io_desc_info.llen = 1;

    /* We don't need fill. */
    if ((ret = determine_fill(ios, iodesc->sync_io_desc_info, gsize, compmap)))
        return ret;
    if (iodesc->sync_io_desc_info.needsfill)
        return ERR_WRONG;

    /* Change settings, so now we do need fill. */
    iodesc->sync_io_desc_info.llen = 0;
    if ((ret = determine_fill(ios, iodesc->sync_io_desc_info, gsize, compmap)))
        return ret;
    if (!iodesc->sync_io_desc_info.needsfill)
        return ERR_WRONG;

    /* Free test resources. */
    delete(iodesc);
    free(ios);

    return 0;
}

/* Run tests for get_start_and_count_regions() funciton. */
int test_get_regions(int my_rank)
{
#define MAPLEN 2
    int ndims = NDIM1;
    const int gdimlen[NDIM1] = {8};
    /* Don't forget map is 1-based!! */
    PIO_Offset map[MAPLEN] = {(my_rank * 2) + 1, ((my_rank + 1) * 2) + 1};
    int maxregions;
    io_region *ior1;
    int ret;

    /* This is how we allocate a region. */
    if ((ret = alloc_region2(NULL, NDIM1, &ior1)))
        return ret;
    ior1->next = NULL;
    ior1->count[0] = 1;

    /* Call the function we are testing. */
    if ((ret = get_regions(ndims, gdimlen, MAPLEN, map, &maxregions, ior1)))
        return ret;
    if (maxregions != 2)
        return ERR_WRONG;

    /* Free resources for the region. */
    free(ior1->next->start);
    free(ior1->next->count);
    free(ior1->next);
    free(ior1->start);
    free(ior1->count);
    free(ior1);

    return 0;
}

/* Run tests for find_region() function. */
int test_find_region()
{
    int ndims = NDIM1;
    int gdimlen[NDIM1] = {4};
    int maplen = 1;
    PIO_Offset map[1] = {1};
    PIO_Offset start[NDIM1];
    PIO_Offset count[NDIM1];
    PIO_Offset regionlen;

    /* Call the function we are testing. */
    regionlen = find_region(ndims, gdimlen, maplen, map, start, count);

    /* Check results. */
    printf("regionlen = %lld start[0] = %lld count[0] = %lld\n", regionlen, start[0], count[0]);
    if (regionlen != 1 || start[0] != 0 || count[0] != 1)
        return ERR_WRONG;

    return 0;
}

/* Run tests for expand_region() function. */
int test_expand_region()
{
    int dim = 0;
    int gdims[NDIM1] = {1};
    int maplen = 1;
    PIO_Offset map[1] = {5};
    int region_size = 1;
    int region_stride = 1;
    int max_size[NDIM1] = {10};
    PIO_Offset count[NDIM1];

    expand_region(dim, gdims, maplen, map, region_size, region_stride, max_size, count);
    if (count[0] != 1)
        return ERR_WRONG;
    printf("max_size[0] = %d count[0] = %lld\n", max_size[0], count[0]);

    return 0;
}

/* Test define_iodesc_datatypes() function. */
int test_define_iodesc_datatypes()
{
#define NUM_REARRANGERS 2
    int rearranger[NUM_REARRANGERS] = {PIO_REARR_BOX, PIO_REARR_SUBSET};
    int mpierr;
    int ret;

    /* Run the functon. */
    for (int r = 0; r < NUM_REARRANGERS; r++)
    {
        iosystem_desc_t ios;
        ios.io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(MPI_COMM_NULL, 0, 4,
            MPI_GROUP_NULL, std::vector<int>(), true, 0, 0, false);
        io_desc_t iodesc(&ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);

        /* Set up test for IO task with BOX rearranger to create one type. */
        ios.compproc = 1; /* this is also compute proc. */
        iodesc.sync_io_desc_info.rtype = NULL; /* Array of MPI types will be created here. */
        iodesc.sync_io_desc_info.nrecvs = 1; /* Number of types created. */
        iodesc.sync_io_desc_info.mpitype = MPI_INT;
        iodesc.sync_io_desc_info.stype = NULL; /* Array of MPI types will be created here. */

        /* Allocate space for arrays in iodesc that will be filled in
         * define_iodesc_datatypes(). */
        if (!(iodesc.sync_io_desc_info.rcount = (int *) malloc(iodesc.sync_io_desc_info.nrecvs * sizeof(int))))
            return PIO_ENOMEM;
        if (!(iodesc.sync_io_desc_info.rfrom = (int *) malloc(iodesc.sync_io_desc_info.nrecvs * sizeof(int))))
            return PIO_ENOMEM;
        if (!(iodesc.sync_io_desc_info.rindex = (PIO_Offset *) malloc(1 * sizeof(PIO_Offset))))
            return PIO_ENOMEM;
        iodesc.sync_io_desc_info.rindex[0] = 0;
        iodesc.sync_io_desc_info.rcount[0] = 1;

        iodesc.sync_io_desc_info.rearranger = rearranger[r];

        /* The two rearrangers create a different number of send types. */
        int num_send_types = iodesc.sync_io_desc_info.rearranger == PIO_REARR_BOX ? ios.io_comm_info->get_sync_io_comm_sz() : 1;

        if (!(iodesc.sync_io_desc_info.sindex = (PIO_Offset *) malloc(num_send_types * sizeof(PIO_Offset))))
            return PIO_ENOMEM;
        if (!(iodesc.sync_io_desc_info.scount = (int *) malloc(num_send_types * sizeof(int))))
            return PIO_ENOMEM;
        for (int st = 0; st < num_send_types; st++)
        {
            iodesc.sync_io_desc_info.sindex[st] = 0;
            iodesc.sync_io_desc_info.scount[st] = 1;
        }

        /* Run the test function. */
        if ((ret = define_iodesc_datatypes(&ios, &iodesc, iodesc.sync_io_desc_info,
                                           SPIO_Util::Iosys_Util::make_sync_io_comm_target(&ios))))
            return ret;

        /* We created send types, so free them. */
        for (int st = 0; st < num_send_types; st++)
            if ((mpierr = MPI_Type_free(&iodesc.sync_io_desc_info.stype[st])))
                MPIERR(mpierr);

        /* We created one receive type, so free it. */
        if ((mpierr = MPI_Type_free(&iodesc.sync_io_desc_info.rtype[0])))
            MPIERR(mpierr);

        /* Free resources. */
        free(iodesc.sync_io_desc_info.rtype); iodesc.sync_io_desc_info.rtype = NULL;
        free(iodesc.sync_io_desc_info.sindex); iodesc.sync_io_desc_info.sindex = NULL;
        free(iodesc.sync_io_desc_info.scount); iodesc.sync_io_desc_info.scount = NULL;
        free(iodesc.sync_io_desc_info.stype); iodesc.sync_io_desc_info.stype = NULL;
        free(iodesc.sync_io_desc_info.rcount); iodesc.sync_io_desc_info.rcount = NULL;
        free(iodesc.sync_io_desc_info.rfrom); iodesc.sync_io_desc_info.rfrom = NULL;
        free(iodesc.sync_io_desc_info.rindex); iodesc.sync_io_desc_info.rindex = NULL;
        delete(ios.io_comm_info);
    }

    return 0;
}

/* Test the compute_counts() function with the box rearranger. */
int test_compute_counts(MPI_Comm test_comm, int my_rank)
{
    iosystem_desc_t *ios;
    int dest_ioproc[TARGET_NTASKS] = {0, 1, 2, 3};
    PIO_Offset dest_ioindex[TARGET_NTASKS] = {0, 1, 2, 3};
    int ret;

    /* Initialize ios. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;

    ios->num_comptasks = TARGET_NTASKS;
    ios->num_uniontasks = TARGET_NTASKS;
    ios->compproc = 1;
    ios->union_comm = test_comm;
    {
        std::vector<int> sync_io_ranks(TARGET_NTASKS);
        for (int t = 0; t < TARGET_NTASKS; t++)
            sync_io_ranks[t] = t;
        ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(MPI_COMM_NULL, 0,
            TARGET_NTASKS, MPI_GROUP_NULL, sync_io_ranks, true, 0, 0, false);
    }
    if (!(ios->compranks = (int *) calloc(ios->num_comptasks, sizeof(int))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__, "Error allocating memory for comp ranks");
    for (int i = 0; i < TARGET_NTASKS; i++)
        ios->compranks[i] = i;

    /* Initialize iodesc. */
    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    assert(iodesc);
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;
    iodesc->sync_io_desc_info.ndof = TARGET_NTASKS;
    iodesc->sync_io_desc_info.llen = TARGET_NTASKS;
    iodesc->sync_io_desc_info.rearr_opts.comm_type = PIO_REARR_COMM_COLL;
    iodesc->sync_io_desc_info.rearr_opts.fcd = PIO_REARR_COMM_FC_2D_DISABLE;

    /* Test the function. */
    if ((ret = compute_counts(ios, iodesc, iodesc->sync_io_desc_info,
                              SPIO_Util::Iosys_Util::make_sync_io_comm_target(ios),
                              dest_ioproc, dest_ioindex)))
        return ret;

    /* Check results. */
    for (int i = 0; i < ios->io_comm_info->get_sync_io_comm_sz(); i++)
        if (iodesc->sync_io_desc_info.scount[i] != 1 || iodesc->sync_io_desc_info.sindex[i] != i)
            return ERR_WRONG;

    for (int i = 0; i < iodesc->sync_io_desc_info.ndof; i++)
        if (iodesc->sync_io_desc_info.rcount[i] != 1 || iodesc->sync_io_desc_info.rfrom[i] != i ||
            iodesc->sync_io_desc_info.rindex[i] != my_rank)
            return ERR_WRONG;

    /* Free resources allocated in compute_counts(). */
    free(iodesc->sync_io_desc_info.scount); iodesc->sync_io_desc_info.scount = NULL;
    free(iodesc->sync_io_desc_info.sindex); iodesc->sync_io_desc_info.sindex = NULL;
    free(iodesc->sync_io_desc_info.rcount); iodesc->sync_io_desc_info.rcount = NULL;
    free(iodesc->sync_io_desc_info.rfrom); iodesc->sync_io_desc_info.rfrom = NULL;
    free(iodesc->sync_io_desc_info.rindex); iodesc->sync_io_desc_info.rindex = NULL;

    /* Free test resources. */
    delete(ios->io_comm_info);
    free(ios->compranks);
    delete(iodesc);
    free(ios);

    return 0;
}

/* Call PIOc_InitDecomp() with parameters such that it calls
 * box_rearrange_create() just like test_box_rearrange_create() will
 * (see below). */
int test_init_decomp(int iosysid, MPI_Comm test_comm, int my_rank)
{
    int ioid;
    PIO_Offset compmap[MAPLEN2] = {my_rank * 2, (my_rank + 1) * 2};
    const int gdimlen[NDIM1] = {8};
    int ret;

    /* Initialize a decomposition. */
    if ((ret = PIOc_init_decomp(iosysid, PIO_INT, NDIM1, gdimlen, MAPLEN2,
                                compmap, &ioid, PIO_REARR_BOX, NULL, NULL)))
        return ret;

    /* Free it. */
    if ((ret = PIOc_freedecomp(iosysid, ioid)))
        return ret;

    return 0;
}

/* Test for the box_rearrange_create() function. */
int test_box_rearrange_create(MPI_Comm test_comm, int my_rank)
{
    iosystem_desc_t *ios;
    io_region *ior1;
    int maplen = MAPLEN2;
    PIO_Offset compmap[MAPLEN2] = {(my_rank * 2) + 1, ((my_rank + 1) * 2) + 1};
    const int gdimlen[NDIM1] = {8};
    int ndims = NDIM1;
    int ret;

    /* Allocate IO system info struct for this test. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;

    /* Allocate IO desc struct for this test. */
    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    assert(iodesc);
    iodesc->sync_io_desc_info.free_all_regions();

    /* Default rearranger options. */
    iodesc->sync_io_desc_info.rearr_opts.comm_type = PIO_REARR_COMM_COLL;
    iodesc->sync_io_desc_info.rearr_opts.fcd = PIO_REARR_COMM_FC_2D_DISABLE;

    /* Set up for determine_fill(). */
    ios->union_comm = test_comm;
    iodesc->sync_io_desc_info.ndims = NDIM1;
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;

    /* Set up the IO task info for the test. */
    ios->compproc = 1;
    ios->union_rank = my_rank;
    ios->num_comptasks = 4;
    ios->num_uniontasks = 4;
    {
        const int nio = 4;
        std::vector<int> sync_io_ranks(nio);
        for (int i = 0; i < nio; i++)
            sync_io_ranks[i] = i;
        /* io_comm aliases test_comm (owned/freed by the caller) - owns_resources = false */
        ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(test_comm, 0, nio,
            MPI_GROUP_NULL, sync_io_ranks, true, 0, 0, false);
    }
    if (!(ios->compranks = (int *) calloc(ios->num_comptasks, sizeof(int))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__, "Error allocating memory for compranks");
    for (int i = 0; i < TARGET_NTASKS; i++)
        ios->compranks[i] = i;

    /* This is how we allocate a region. */
    if ((ret = alloc_region2(NULL, NDIM1, &ior1)))
        return ret;
    if (my_rank == 0)
        ior1->count[0] = 8;

    iodesc->sync_io_desc_info.firstregion = ior1;

    /* We are finally ready to run the code under test. */
    if ((ret = box_rearrange_create(ios, maplen, compmap, gdimlen, ndims, iodesc,
                                    iodesc->sync_io_desc_info, SPIO_Util::Iosys_Util::make_sync_io_comm_target(ios))))
        return ret;

    /* Check some results. */
    if (iodesc->sync_io_desc_info.rearranger != PIO_REARR_BOX || iodesc->sync_io_desc_info.ndof != maplen ||
        iodesc->sync_io_desc_info.llen != my_rank ? 0 : 8 || !iodesc->sync_io_desc_info.needsfill)
        return ERR_WRONG;

    /* for (int i = 0; i < ios->num_iotasks; i++) */
    /* { */
    /*     /\* sindex is only allocated if scount[i] > 0. *\/ */
    /*     if (iodesc->sync_io_desc_info.scount[i] != i ? 0 : 1 || */
    /*         (iodesc->sync_io_desc_info.scount[i] && iodesc->sync_io_desc_info.sindex[i] != 0)) */
    /*         return ERR_WRONG; */
    /* } */

    /* for (int i = 0; i < iodesc->sync_io_desc_info.ndof; i++) */
    /* { */
    /*     /\* rcount is 1 for rank 0, 0 on other tasks. *\/ */
    /*     if (iodesc->sync_io_desc_info.rcount[i] != my_rank ? 0 : 1) */
    /*         return ERR_WRONG; */

    /*     /\* rfrom is 0 everywhere, except task 0, array elemnt 1. *\/ */
    /*     if (my_rank == 0 && i == 1) */
    /*     { */
    /*         if (iodesc->sync_io_desc_info.rfrom[i] != 1) */
    /*             return ERR_WRONG; */
    /*     } */
    /*     else */
    /*     { */
    /*         if (iodesc->sync_io_desc_info.rfrom[i] != 0) */
    /*             return ERR_WRONG; */
    /*     } */

    /*     /\* rindex is only allocated where there is a non-zero count. *\/ */
    /*     if (iodesc->sync_io_desc_info.rcount[i]) */
    /*         if (iodesc->sync_io_desc_info.rindex[i] != 0) */
    /*             return ERR_WRONG; */
    /* } */

    /* Free resources allocated in compute_counts(). */
    free(iodesc->sync_io_desc_info.scount); iodesc->sync_io_desc_info.scount = NULL;
    free(iodesc->sync_io_desc_info.sindex); iodesc->sync_io_desc_info.sindex = NULL;
    free(iodesc->sync_io_desc_info.rcount); iodesc->sync_io_desc_info.rcount = NULL;
    free(iodesc->sync_io_desc_info.rfrom); iodesc->sync_io_desc_info.rfrom = NULL;
    free(iodesc->sync_io_desc_info.rindex); iodesc->sync_io_desc_info.rindex = NULL;

    /* Free resources from test. */
    free(ior1->start);
    free(ior1->count);
    free(ior1); iodesc->sync_io_desc_info.firstregion = NULL;
    delete(ios->io_comm_info);
    free(ios->compranks);
    delete(iodesc);
    free(ios);

    return 0;
}

/* Test for the box_rearrange_create() function. */
int test_box_rearrange_create_2(MPI_Comm test_comm, int my_rank)
{
#define MAPLEN2 2
    iosystem_desc_t *ios;
    io_region *ior1;
    int maplen = MAPLEN2;
    PIO_Offset compmap[MAPLEN2] = {1, 0};
    const int gdimlen[NDIM1] = {8};
    int ndims = NDIM1;
    int ret;

    /* Allocate IO system info struct for this test. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;

    /* Allocate IO desc struct for this test. */
    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    assert(iodesc);
    iodesc->sync_io_desc_info.free_all_regions();

    /* Default rearranger options. */
    iodesc->sync_io_desc_info.rearr_opts.comm_type = PIO_REARR_COMM_COLL;
    iodesc->sync_io_desc_info.rearr_opts.fcd = PIO_REARR_COMM_FC_2D_DISABLE;

    /* Set up for determine_fill(). */
    ios->union_comm = test_comm;
    iodesc->sync_io_desc_info.ndims = NDIM1;
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;

    /* This is the size of the map in computation tasks. */
    iodesc->sync_io_desc_info.ndof = 2;

    /* Set up the IO task info for the test. */
    ios->compproc = 1;
    ios->union_rank = my_rank;
    ios->num_comptasks = 4;
    ios->num_uniontasks = 4;
    {
        const int nio = 4;
        std::vector<int> sync_io_ranks(nio);
        for (int i = 0; i < nio; i++)
            sync_io_ranks[i] = i;
        /* io_comm aliases test_comm (owned/freed by the caller) - owns_resources = false */
        ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(test_comm, 0, nio,
            MPI_GROUP_NULL, sync_io_ranks, true, 0, 0, false);
    }
    if (!(ios->compranks = (int *) calloc(ios->num_comptasks, sizeof(int))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__, "Error allocating memory for compranks");
    for (int i = 0; i < TARGET_NTASKS; i++)
        ios->compranks[i] = i;

    /* This is how we allocate a region. */
    if ((ret = alloc_region2(NULL, NDIM1, &ior1)))
        return ret;
    ior1->next = NULL;
    if (my_rank == 0)
        ior1->count[0] = 8;

    iodesc->sync_io_desc_info.firstregion = ior1;

    /* We are finally ready to run the code under test. */
    if ((ret = box_rearrange_create(ios, maplen, compmap, gdimlen, ndims, iodesc,
                                    iodesc->sync_io_desc_info, SPIO_Util::Iosys_Util::make_sync_io_comm_target(ios))))
        return ret;

    /* Check some results. */
    if (iodesc->sync_io_desc_info.rearranger != PIO_REARR_BOX || iodesc->sync_io_desc_info.ndof != maplen ||
        iodesc->sync_io_desc_info.llen != my_rank ? 0 : 8 || !iodesc->sync_io_desc_info.needsfill)
        return ERR_WRONG;

    for (int i = 0; i < ios->io_comm_info->get_sync_io_comm_sz(); i++)
    {
        /* sindex is only allocated if scount[i] > 0. */
        if (iodesc->sync_io_desc_info.scount[i] != i ? 0 : 1 ||
            (iodesc->sync_io_desc_info.scount[i] && iodesc->sync_io_desc_info.sindex[i] != 0))
            return ERR_WRONG;
    }

    if (my_rank == 0)
    {
        for (int i = 0; i < iodesc->sync_io_desc_info.ndof; i++)
        {
            /* rcount is 1 for rank 0, 0 on other tasks. */
            if (iodesc->sync_io_desc_info.rcount[i] != 1)
                return ERR_WRONG;

            /* rfrom only matters if there is a non-zero count. */
            if (iodesc->sync_io_desc_info.rcount[i])
                if (iodesc->sync_io_desc_info.rfrom[i] != i ? 1 : 0)
                    return ERR_WRONG;

            /* rindex is only allocated where there is a non-zero count. */
            if (iodesc->sync_io_desc_info.rcount[i])
                if (iodesc->sync_io_desc_info.rindex[i] != 0)
                    return ERR_WRONG;
        }
    }

    /* Free resources allocated in compute_counts(). */
    free(iodesc->sync_io_desc_info.scount); iodesc->sync_io_desc_info.scount = NULL;
    free(iodesc->sync_io_desc_info.sindex); iodesc->sync_io_desc_info.sindex = NULL;
    free(iodesc->sync_io_desc_info.rcount); iodesc->sync_io_desc_info.rcount = NULL;
    free(iodesc->sync_io_desc_info.rfrom); iodesc->sync_io_desc_info.rfrom = NULL;
    free(iodesc->sync_io_desc_info.rindex); iodesc->sync_io_desc_info.rindex = NULL;

    /* Free resources from test. */
    free(ior1->start);
    free(ior1->count);
    free(ior1); iodesc->sync_io_desc_info.firstregion = NULL;
    delete(ios->io_comm_info);
    free(ios->compranks);
    delete(iodesc);
    free(ios);

    return 0;
}

/* Test for the box_rearrange_create() function.
 * Test totalrecv > iodesc->sync_io_desc_info.llen case in compute_counts(). */
int test_box_rearrange_create_3(MPI_Comm test_comm, int my_rank)
{
#define MAPLEN1 1
    iosystem_desc_t *ios;
    io_region *ior1;
    int maplen = MAPLEN1;
    PIO_Offset compmap[MAPLEN1] = {0};
    const int gdimlen[NDIM1] = {1};
    int ndims = NDIM1;
    int ret;

    /* Allocate IO system info struct for this test. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;

    /* Allocate IO desc struct for this test. */
    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    assert(iodesc);
    iodesc->sync_io_desc_info.free_all_regions();

    /* Default rearranger options. */
    iodesc->sync_io_desc_info.rearr_opts.comm_type = PIO_REARR_COMM_COLL;
    iodesc->sync_io_desc_info.rearr_opts.fcd = PIO_REARR_COMM_FC_2D_DISABLE;

    /* Set up for determine_fill(). */
    ios->union_comm = test_comm;
    iodesc->sync_io_desc_info.ndims = NDIM1;
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;

    /* This is the size of the map in computation tasks. */
    iodesc->sync_io_desc_info.ndof = 1;

    /* Set up the IO task info for the test. */
    ios->compproc = 1;
    ios->union_rank = my_rank;
    ios->num_comptasks = TARGET_NTASKS;
    ios->num_uniontasks = TARGET_NTASKS;
    {
        const int nio = TARGET_NTASKS;
        std::vector<int> sync_io_ranks(nio);
        for (int i = 0; i < nio; i++)
            sync_io_ranks[i] = i;
        /* io_comm aliases test_comm (owned/freed by the caller) - owns_resources = false */
        ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(test_comm, 0, nio,
            MPI_GROUP_NULL, sync_io_ranks, true, 0, 0, false);
    }
    if (!(ios->compranks = (int *) calloc(ios->num_comptasks, sizeof(int))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__, "Error allocating memory for compranks");
    for (int i = 0; i < ios->num_comptasks; i++)
        ios->compranks[i] = i;

    /* This is how we allocate a region. */
    if ((ret = alloc_region2(NULL, NDIM1, &ior1)))
        return ret;
    ior1->next = NULL;
    if (my_rank == 0)
        ior1->count[0] = 1;

    iodesc->sync_io_desc_info.firstregion = ior1;

    /* Two computation tasks contain duplicate data element to be transfered */
    if (my_rank == 0 || my_rank == 2)
        compmap[0] = 1;

    /* We are finally ready to run the code under test. */
    if ((ret = box_rearrange_create(ios, maplen, compmap, gdimlen, ndims, iodesc,
                                    iodesc->sync_io_desc_info, SPIO_Util::Iosys_Util::make_sync_io_comm_target(ios))))
        return ret;

    /* Check some results. */
    if (iodesc->sync_io_desc_info.rearranger != PIO_REARR_BOX || iodesc->sync_io_desc_info.ndof != maplen ||
        (iodesc->sync_io_desc_info.llen != my_rank ? 0 : 1) || iodesc->sync_io_desc_info.needsfill)
        return ERR_WRONG;

    /* Free resources allocated in compute_counts(). */
    free(iodesc->sync_io_desc_info.scount); iodesc->sync_io_desc_info.scount = NULL;
    free(iodesc->sync_io_desc_info.sindex); iodesc->sync_io_desc_info.sindex = NULL;
    free(iodesc->sync_io_desc_info.rcount); iodesc->sync_io_desc_info.rcount = NULL;
    free(iodesc->sync_io_desc_info.rfrom); iodesc->sync_io_desc_info.rfrom = NULL;
    free(iodesc->sync_io_desc_info.rindex); iodesc->sync_io_desc_info.rindex = NULL;

    /* Free resources from test. */
    free(ior1->start);
    free(ior1->count);
    free(ior1); iodesc->sync_io_desc_info.firstregion = NULL;
    delete(ios->io_comm_info);
    free(ios->compranks);
    delete(iodesc);
    free(ios);

    return 0;
}

/* Test function default_subset_partition. */
int test_default_subset_partition(MPI_Comm test_comm, int my_rank)
{
    iosystem_desc_t *ios;
    int mpierr;
    int ret;

    /* Allocate IO system info struct for this test. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;

    /* Allocate IO desc struct for this test. */
    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    assert(iodesc);

    ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(MPI_COMM_NULL, my_rank,
        TARGET_NTASKS, MPI_GROUP_NULL, std::vector<int>(), true, 0, 0, false);
    ios->comp_rank = my_rank;
    ios->num_comptasks = TARGET_NTASKS;
    ios->comp_comm = test_comm;
    ios->union_comm = test_comm;

    /* Run the function to test. */
    if ((ret = default_subset_partition(ios, iodesc, iodesc->sync_io_desc_info,
                                        SPIO_Util::Iosys_Util::make_sync_io_comm_target(ios))))
        return ret;

    /* Free the created communicator. */
    if ((mpierr = MPI_Comm_free(&iodesc->sync_io_desc_info.subset_comm)))
        MPIERR(mpierr);

    /* Free resources from test. */
    delete(ios->io_comm_info);
    delete(iodesc);
    free(ios);

    return 0;
}

/* Test function rearrange_comp2io. */
int test_rearrange_comp2io(MPI_Comm test_comm, int my_rank)
{
    iosystem_desc_t *ios;
    file_desc_t *file;
    void *sbuf = NULL;
    void *rbuf = NULL;
    int nvars = 1;
    io_region *ior1;
    int maplen = 2;
    PIO_Offset compmap[2] = {1, 0};
    const int gdimlen[NDIM1] = {8};
    int ndims = NDIM1;
    int mpierr;
    int ret;

    /* Allocate some space for data. */
    if (!(sbuf = (int *) calloc(4, sizeof(int))))
        return PIO_ENOMEM;
    if (!(rbuf = (int *) calloc(4, sizeof(int))))
        return PIO_ENOMEM;

    /* Allocate IO system info struct for this test. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;

    /* Allocate IO desc struct for this test. */
    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    assert(iodesc);
    iodesc->sync_io_desc_info.free_all_regions();

    if (!(file = (file_desc_t *) calloc(1, sizeof(file_desc_t))))
        return PIO_ENOMEM;

    ios->compproc = 1;
    ios->union_comm = test_comm;
    ios->num_uniontasks = TARGET_NTASKS;
    {
        std::vector<int> sync_io_ranks(TARGET_NTASKS);
        for (int i = 0; i < TARGET_NTASKS; i++)
            sync_io_ranks[i] = i;
        /* io_comm aliases test_comm (owned/freed by the caller) - owns_resources = false */
        ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(test_comm, my_rank,
            TARGET_NTASKS, MPI_GROUP_NULL, sync_io_ranks, true, 0, 0, false);
    }
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;
    iodesc->sync_io_desc_info.mpitype = MPI_INT;

    /* Set up test for IO task with BOX rearranger to create one type. */
    iodesc->sync_io_desc_info.rtype = NULL; /* Array of MPI types will be created here. */
    iodesc->sync_io_desc_info.nrecvs = 1; /* Number of types created. */
    iodesc->sync_io_desc_info.mpitype = MPI_INT;
    iodesc->sync_io_desc_info.stype = NULL; /* Array of MPI types will be created here. */

    file->iosystem = ios;
    file->fh = -1;

    /* The two rearrangers create a different number of send types. */
    int num_send_types = iodesc->sync_io_desc_info.rearranger == PIO_REARR_BOX ? ios->io_comm_info->get_sync_io_comm_sz() : 1;

    /* Default rearranger options. */
    iodesc->sync_io_desc_info.rearr_opts.comm_type = PIO_REARR_COMM_COLL;
    iodesc->sync_io_desc_info.rearr_opts.fcd = PIO_REARR_COMM_FC_2D_DISABLE;

    /* Set up for determine_fill(). */
    ios->union_comm = test_comm;
    iodesc->sync_io_desc_info.ndims = NDIM1;
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;

    iodesc->sync_io_desc_info.ndof = 4;

    /* Set up the IO task info for the test. */
    ios->union_rank = my_rank;
    ios->num_comptasks = 4;
    if (!(ios->compranks = (int *) calloc(ios->num_comptasks, sizeof(int))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__, "Error allocating memory for compranks");
    for (int i = 0; i < TARGET_NTASKS; i++)
        ios->compranks[i] = i;

    /* This is how we allocate a region. */
    if ((ret = alloc_region2(NULL, NDIM1, &ior1)))
        return ret;
    ior1->next = NULL;
    if (my_rank == 0)
        ior1->count[0] = 8;

    iodesc->sync_io_desc_info.firstregion = ior1;

    /* Create the box rearranger. */
    if ((ret = box_rearrange_create(ios, maplen, compmap, gdimlen, ndims, iodesc,
                                    iodesc->sync_io_desc_info, SPIO_Util::Iosys_Util::make_sync_io_comm_target(ios))))
        return ret;

    /* Run the function to test. */
    if ((ret = rearrange_comp2io(ios, iodesc, file, sbuf, rbuf, nvars)))
        return ret;
    printf("returned from rearrange_comp2io\n");

    /* We created send types, so free them. */
    for (int st = 0; st < num_send_types; st++)
        if (iodesc->sync_io_desc_info.stype[st] != MPI_DATATYPE_NULL)
            if ((mpierr = MPI_Type_free(&iodesc->sync_io_desc_info.stype[st])))
                MPIERR(mpierr);

    /* We created one receive type, so free it. */
    if (iodesc->sync_io_desc_info.rtype)
        for (int r = 0; r < iodesc->sync_io_desc_info.nrecvs; r++)
            if (iodesc->sync_io_desc_info.rtype[r] != MPI_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(&iodesc->sync_io_desc_info.rtype[r])))
                    MPIERR(mpierr);

    /* Free resources allocated in library code. */
    free(iodesc->sync_io_desc_info.rtype); iodesc->sync_io_desc_info.rtype = NULL;
    free(iodesc->sync_io_desc_info.sindex); iodesc->sync_io_desc_info.sindex = NULL;
    free(iodesc->sync_io_desc_info.scount); iodesc->sync_io_desc_info.scount = NULL;
    free(iodesc->sync_io_desc_info.stype); iodesc->sync_io_desc_info.stype = NULL;
    free(iodesc->sync_io_desc_info.rcount); iodesc->sync_io_desc_info.rcount = NULL;
    free(iodesc->sync_io_desc_info.rfrom); iodesc->sync_io_desc_info.rfrom = NULL;
    free(iodesc->sync_io_desc_info.rindex); iodesc->sync_io_desc_info.rindex = NULL;

    /* Free resources from test. */
    free(ior1->start);
    free(ior1->count);
    free(ior1); iodesc->sync_io_desc_info.firstregion = NULL;
    delete(ios->io_comm_info);
    free(ios->compranks);
    free(file);
    delete(iodesc);
    free(ios);
    free(sbuf);
    free(rbuf);

    return 0;
}

/* Test function rearrange_io2comp. */
int test_rearrange_io2comp(MPI_Comm test_comm, int my_rank)
{
    iosystem_desc_t *ios;
    void *sbuf = NULL;
    void *rbuf = NULL;
    io_region *ior1;
    int maplen = 2;
    PIO_Offset compmap[2] = {1, 0};
    const int gdimlen[NDIM1] = {8};
    int ndims = NDIM1;
    int mpierr;
    int ret;

    /* Allocate some space for data. */
    if (!(sbuf = (int *) calloc(4, sizeof(int))))
        return PIO_ENOMEM;
    if (!(rbuf = (int *) calloc(4, sizeof(int))))
        return PIO_ENOMEM;

    /* Allocate IO system info struct for this test. */
    if (!(ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t))))
        return PIO_ENOMEM;

    /* Allocate IO desc struct for this test. */
    io_desc_t *iodesc = new io_desc_t(ios, PIO_INT, 0, NULL, 0, NULL, PIO_REARR_BOX, false);
    assert(iodesc);
    iodesc->sync_io_desc_info.free_all_regions();

    ios->union_comm = test_comm;
    {
        std::vector<int> sync_io_ranks(TARGET_NTASKS);
        for (int i = 0; i < TARGET_NTASKS; i++)
            sync_io_ranks[i] = i;
        /* io_comm aliases test_comm (owned/freed by the caller) - owns_resources = false */
        ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(test_comm, my_rank,
            TARGET_NTASKS, MPI_GROUP_NULL, sync_io_ranks, true, 0, 0, false);
    }
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;
    iodesc->sync_io_desc_info.mpitype = MPI_INT;

    /* Set up test for IO task with BOX rearranger to create one type. */
    iodesc->sync_io_desc_info.rtype = NULL; /* Array of MPI types will be created here. */
    iodesc->sync_io_desc_info.nrecvs = 1; /* Number of types created. */
    iodesc->sync_io_desc_info.mpitype = MPI_INT;
    iodesc->sync_io_desc_info.stype = NULL; /* Array of MPI types will be created here. */

    /* The two rearrangers create a different number of send types. */
    int num_send_types = iodesc->sync_io_desc_info.rearranger == PIO_REARR_BOX ? ios->io_comm_info->get_sync_io_comm_sz() : 1;

    /* Default rearranger options. */
    iodesc->sync_io_desc_info.rearr_opts.comm_type = PIO_REARR_COMM_COLL;
    iodesc->sync_io_desc_info.rearr_opts.fcd = PIO_REARR_COMM_FC_2D_DISABLE;

    /* Set up for determine_fill(). */
    ios->union_comm = test_comm;
    iodesc->sync_io_desc_info.ndims = NDIM1;
    iodesc->sync_io_desc_info.rearranger = PIO_REARR_BOX;

    iodesc->sync_io_desc_info.ndof = 4;

    /* Set up the IO task info for the test. */
    ios->compproc = 1;
    ios->union_rank = my_rank;
    ios->num_comptasks = 4;
    ios->num_uniontasks = 4;
    if (!(ios->compranks = (int *) calloc(ios->num_comptasks, sizeof(int))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__, "Error allocating memory for compranks");
    for (int i = 0; i < TARGET_NTASKS; i++)
        ios->compranks[i] = i;

    /* This is how we allocate a region. */
    if ((ret = alloc_region2(NULL, NDIM1, &ior1)))
        return ret;
    ior1->next = NULL;
    if (my_rank == 0)
        ior1->count[0] = 8;

    iodesc->sync_io_desc_info.firstregion = ior1;

    /* Create the box rearranger. */
    if ((ret = box_rearrange_create(ios, maplen, compmap, gdimlen, ndims, iodesc,
                                    iodesc->sync_io_desc_info, SPIO_Util::Iosys_Util::make_sync_io_comm_target(ios))))
        return ret;

    /* Run the function to test. */
    if ((ret = rearrange_io2comp(ios, iodesc, sbuf, rbuf)))
        return ret;
    printf("returned from rearrange_comp2io\n");

    /* We created send types, so free them. */
    for (int st = 0; st < num_send_types; st++)
        if (iodesc->sync_io_desc_info.stype[st] != MPI_DATATYPE_NULL)
            if ((mpierr = MPI_Type_free(&iodesc->sync_io_desc_info.stype[st])))
                MPIERR(mpierr);

    /* We created one receive type, so free it. */
    if (iodesc->sync_io_desc_info.rtype)
        for (int r = 0; r < iodesc->sync_io_desc_info.nrecvs; r++)
            if (iodesc->sync_io_desc_info.rtype[r] != MPI_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(&iodesc->sync_io_desc_info.rtype[r])))
                    MPIERR(mpierr);

    /* Free resources allocated in library code. */
    free(iodesc->sync_io_desc_info.rtype); iodesc->sync_io_desc_info.rtype = NULL;
    free(iodesc->sync_io_desc_info.sindex); iodesc->sync_io_desc_info.sindex = NULL;
    free(iodesc->sync_io_desc_info.scount); iodesc->sync_io_desc_info.scount = NULL;
    free(iodesc->sync_io_desc_info.stype); iodesc->sync_io_desc_info.stype = NULL;
    free(iodesc->sync_io_desc_info.rcount); iodesc->sync_io_desc_info.rcount = NULL;
    free(iodesc->sync_io_desc_info.rfrom); iodesc->sync_io_desc_info.rfrom = NULL;
    free(iodesc->sync_io_desc_info.rindex); iodesc->sync_io_desc_info.rindex = NULL;

    /* Free resources from test. */
    free(ior1->start);
    free(ior1->count);
    free(ior1); iodesc->sync_io_desc_info.firstregion = NULL;
    delete(ios->io_comm_info);
    free(ios->compranks);
    delete(iodesc);
    free(ios);
    free(sbuf);
    free(rbuf);

    return 0;
}

/* These tests do not need an iosysid. */
int run_no_iosys_tests(int my_rank, MPI_Comm test_comm)
{
    int ret;

    printf("%d running compute_maxIObuffersize tests\n", my_rank);
    if ((ret = test_compute_maxIObuffersize(test_comm, my_rank)))
        return ret;

    printf("%d running determine_fill\n", my_rank);
    if ((ret = test_determine_fill(test_comm)))
        return ret;

    printf("%d running tests for expand_region()\n", my_rank);
    if ((ret = test_expand_region()))
        return ret;

    printf("%d running tests for find_region()\n", my_rank);
    if ((ret = test_find_region()))
        return ret;

    printf("%d running tests for get_regions()\n", my_rank);
    if ((ret = test_get_regions(my_rank)))
        return ret;

    printf("%d running create_mpi_datatypes tests\n", my_rank);
    if ((ret = test_create_mpi_datatypes()))
        return ret;

    printf("%d running define_iodesc_datatypes tests\n", my_rank);
    if ((ret = test_define_iodesc_datatypes()))
        return ret;

    printf("%d running compare_offsets tests\n", my_rank);
    if ((ret = test_compare_offsets()))
        return ret;

    printf("%d running compute_counts tests for box rearranger\n", my_rank);
    if ((ret = test_compute_counts(test_comm, my_rank)))
        return ret;

    printf("%d running tests for box_rearrange_create\n", my_rank);
    if ((ret = test_box_rearrange_create(test_comm, my_rank)))
        return ret;

    printf("%d running more tests for box_rearrange_create\n", my_rank);
    if ((ret = test_box_rearrange_create_2(test_comm, my_rank)))
        return ret;

    if ((ret = test_box_rearrange_create_3(test_comm, my_rank)))
        return ret;

    printf("%d running tests for default_subset_partition\n", my_rank);
    if ((ret = test_default_subset_partition(test_comm, my_rank)))
        return ret;

    printf("%d running tests for rearrange_comp2io\n", my_rank);
    if ((ret = test_rearrange_comp2io(test_comm, my_rank)))
        return ret;

    printf("%d running tests for rearrange_io2comp\n", my_rank);
    if ((ret = test_rearrange_io2comp(test_comm, my_rank)))
        return ret;

     return 0;
}

/* Test scalar vars. */
int test_scalar(int numio, int iosysid, MPI_Comm test_comm, int my_rank,
                int num_flavors, int *flavor)
{

    int var_type[NUM_NETCDF4_TYPES - 1] = {PIO_BYTE, PIO_CHAR, PIO_SHORT, PIO_INT, PIO_FLOAT,
                                           PIO_DOUBLE, PIO_UBYTE, PIO_USHORT, PIO_UINT, PIO_INT64,
                                           PIO_UINT64};
    char char_data = 2;
    signed char byte_data = -42;
    short short_data = -300;
    int int_data = -10000;
    float float_data = -42.42;
    double double_data = -420000000000.5;
    unsigned char ubyte_data = 43;
    unsigned short ushort_data = 666;
    unsigned int uint_data = 666666;
    long long int64_data = -99999999999;
    unsigned long long uint64_data = 99999999999;
    char char_data_in;
    signed char byte_data_in;
    short short_data_in;
    unsigned char ubyte_data_in;
    int int_data_in;
    float float_data_in;
    double double_data_in;
    unsigned short ushort_data_in;
    unsigned int uint_data_in;
    long long int64_data_in;
    unsigned long long uint64_data_in;

    int ret;

    /* Run tests with all available iotypes. */
    for (int fmt = 0; fmt < num_flavors; fmt++)
    {
        /* For netcdf-4, there are extra types. */
        int num_types = (flavor[fmt] == PIO_IOTYPE_NETCDF4C || flavor[fmt] == PIO_IOTYPE_NETCDF4P) ?
            NUM_NETCDF4_TYPES - 1 : NUM_CLASSIC_TYPES;

        /* For each available type, create a file with a scalar var of
         * that type. */
        for (int t = 0; t < num_types; t++)
        {
            int ncid;
            int varid;
            char filename[PIO_MAX_NAME + 1];

            printf("test with t = %d\n", t);

            /* These iotypes only handle netCDF classic types. */
            if (t >= NUM_CLASSIC_TYPES &&
                (flavor[fmt] == PIO_IOTYPE_PNETCDF || flavor[fmt] == PIO_IOTYPE_NETCDF))
                continue;

            /* Create filename. */
            sprintf(filename, "%s_scalar_numio_%d_iotype_%d_var_type_%d.nc", TEST_NAME,
                    numio, flavor[fmt], var_type[t]);

            /* Create the file. */
            if ((ret = PIOc_createfile(iosysid, &ncid, &flavor[fmt], filename, NC_CLOBBER)))
                return ret;

            /* Define scalar var. */
            if ((ret = PIOc_def_var(ncid, VAR_NAME, var_type[t], 0, NULL, &varid)))
                return ret;

            /* End define mode. */
            if ((ret = PIOc_enddef(ncid)))
                return ret;

            /* Write a value. */
            switch (var_type[t])
            {
            case PIO_BYTE:
                if ((ret = PIOc_put_var_schar(ncid, varid, &byte_data)))
                    return ret;
                break;
            case PIO_CHAR:
                if ((ret = PIOc_put_var_text(ncid, varid, &char_data)))
                    return ret;
                break;
            case PIO_SHORT:
                if ((ret = PIOc_put_var_short(ncid, varid, &short_data)))
                    return ret;
                break;
            case PIO_INT:
                if ((ret = PIOc_put_var_int(ncid, varid, &int_data)))
                    return ret;
                break;
            case PIO_FLOAT:
                if ((ret = PIOc_put_var_float(ncid, varid, &float_data)))
                    return ret;
                break;
            case PIO_DOUBLE:
                if ((ret = PIOc_put_var_double(ncid, varid, &double_data)))
                    return ret;
                break;
#ifdef _NETCDF4                
            case PIO_UBYTE:
                if ((ret = PIOc_put_var_uchar(ncid, varid, &ubyte_data)))
                    return ret;
                break;
            case PIO_USHORT:
                if ((ret = PIOc_put_var_ushort(ncid, varid, &ushort_data)))
                    return ret;
                break;
            case PIO_UINT:
                if ((ret = PIOc_put_var_uint(ncid, varid, &uint_data)))
                    return ret;
                break;
            case PIO_INT64:
                if ((ret = PIOc_put_var_longlong(ncid, varid, &int64_data)))
                    return ret;
                break;
            case PIO_UINT64:
                if ((ret = PIOc_put_var_ulonglong(ncid, varid, &uint64_data)))
                    return ret;
                break;
#endif /* _NETCDF4 */                
            default:
                return ERR_WRONG;
            }

            /* Close the file. */
            if ((ret = PIOc_closefile(ncid)))
                return ret;

            /* Reopen the file. */
            if ((ret = PIOc_openfile(iosysid, &ncid, &flavor[fmt], filename, NC_NOWRITE)))
                return ret;

            /* Read the value. Is the value correct? */
            switch (var_type[t])
            {
            case PIO_BYTE:
                if ((ret = PIOc_get_var_schar(ncid, varid, &byte_data_in)))
                    return ret;
                if (byte_data_in != byte_data)
                    return ERR_WRONG;
                break;
            case PIO_CHAR:
                if ((ret = PIOc_get_var_text(ncid, varid, &char_data_in)))
                    return ret;
                if (char_data_in != char_data)
                    return ERR_WRONG;
                break;
            case PIO_SHORT:
                if ((ret = PIOc_get_var_short(ncid, varid, &short_data_in)))
                    return ret;
                if (short_data_in != short_data)
                    return ERR_WRONG;
                break;
            case PIO_INT:
                if ((ret = PIOc_get_var_int(ncid, varid, &int_data_in)))
                    return ret;
                if (int_data_in != int_data)
                    return ERR_WRONG;
                break;
            case PIO_FLOAT:
                if ((ret = PIOc_get_var_float(ncid, varid, &float_data_in)))
                    return ret;
                if (float_data_in != float_data)
                    return ERR_WRONG;
                break;
            case PIO_DOUBLE:
                if ((ret = PIOc_get_var_double(ncid, varid, &double_data_in)))
                    return ret;
                if (double_data_in != double_data)
                    return ERR_WRONG;
                break;
#ifdef _NETCDF4                
            case PIO_UBYTE:
                if ((ret = PIOc_get_var_uchar(ncid, varid, &ubyte_data_in)))
                    return ret;
                if (ubyte_data_in != ubyte_data)
                    return ERR_WRONG;
                break;
            case PIO_USHORT:
                if ((ret = PIOc_get_var_ushort(ncid, varid, &ushort_data_in)))
                    return ret;
                if (ushort_data_in != ushort_data)
                    return ERR_WRONG;
                break;
            case PIO_UINT:
                if ((ret = PIOc_get_var_uint(ncid, varid, &uint_data_in)))
                    return ret;
                if (uint_data_in != uint_data)
                    return ERR_WRONG;
                break;
            case PIO_INT64:
                if ((ret = PIOc_get_var_longlong(ncid, varid, &int64_data_in)))
                    return ret;
                if (int64_data_in != int64_data)
                    return ERR_WRONG;
                break;
            case PIO_UINT64:
                if ((ret = PIOc_get_var_ulonglong(ncid, varid, &uint64_data_in)))
                    return ret;
                if (uint64_data_in != uint64_data)
                    return ERR_WRONG;
                break;
#endif /* _NETCDF4 */                
            default:
                return ERR_WRONG;
            }

            /* Close the file. */
            if ((ret = PIOc_closefile(ncid)))
                return ret;

        } /* next iotype */
    } /* next type */

    return 0;
}

/* These tests are run with different rearrangers and numbers of IO
 * tasks. */
int run_iosys_tests(int numio, int iosysid, int my_rank, MPI_Comm test_comm,
                    int num_flavors, int *flavor)
{
    int ret;

    printf("%d running rearranger opts tests 1\n", my_rank);
    if ((ret = test_rearranger_opts1(iosysid)))
        return ret;

    printf("%d running test for init_decomp\n", my_rank);
    if ((ret = test_init_decomp(iosysid, test_comm, my_rank)))
        return ret;

    printf("%d running test for init_decomp\n", my_rank);
    if ((ret = test_scalar(numio, iosysid, test_comm, my_rank, num_flavors, flavor)))
        return ret;

    return 0;
}

/* Run Tests for pio_spmd.c functions. */
int main(int argc, char **argv)
{
    int my_rank; /* Zero-based rank of processor. */
    int ntasks;  /* Number of processors involved in current execution. */
    int num_flavors; /* Number of PIO netCDF flavors in this build. */
    int flavor[NUM_FLAVORS]; /* iotypes for the supported netCDF IO flavors. */
    MPI_Comm test_comm; /* A communicator for this test. */
    int ret;     /* Return code. */

    /* Initialize test. */
    if ((ret = pio_test_init2(argc, argv, &my_rank, &ntasks, MIN_NTASKS,
                              TARGET_NTASKS, 3, &test_comm)))
        ERR(ERR_INIT);
    if ((ret = PIOc_set_iosystem_error_handling(PIO_DEFAULT, PIO_RETURN_ERROR, NULL)))
        return ret;

    /* Figure out iotypes. */
    if ((ret = get_iotypes(&num_flavors, flavor)))
        ERR(ret);
    printf("Runnings tests for %d flavors\n", num_flavors);

    /* Test code runs on TARGET_NTASKS tasks. The left over tasks do
     * nothing. */
    if (my_rank < TARGET_NTASKS)
    {
        /* Run the tests that don't need an iosysid. */
        if ((ret = run_no_iosys_tests(my_rank, test_comm)))
            return ret;

        /* Test code with both rearrangers. */
        for (int r = 0; r < NUM_REARRANGERS; r++)
        {
            /* Test code with 1, 2, 3, and 4 io tasks. */
            for (int numio = 1; numio <= TARGET_NTASKS; numio++)
            {
                int iosysid;
                int rearranger = r ? PIO_REARR_SUBSET : PIO_REARR_BOX;

                if ((ret = PIOc_Init_Intracomm(test_comm, numio, 1, 0, rearranger,
                                               &iosysid)))
                    return ret;

                /* Run the tests that need an iosysid. */
                if ((ret = run_iosys_tests(numio, iosysid, my_rank, test_comm,
                                           num_flavors, flavor)))
                    return ret;

                /* Finalize PIO system. */
                if ((ret = PIOc_finalize(iosysid)))
                    return ret;
            } /* next numio */
        } /* next rearranger */
    } /* endif my_rank < TARGET_NTASKS */

    /* Finalize the MPI library. */
    printf("%d %s Finalizing...\n", my_rank, TEST_NAME);
    if ((ret = pio_test_finalize(&test_comm)))
        return ret;

    printf("%d %s SUCCESS!!\n", my_rank, TEST_NAME);

    return 0;
}
