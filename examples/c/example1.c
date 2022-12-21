#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#include <pio_internal.h>
#ifdef TIMING
#include <gptl.h>
#endif

#define ERR { if (ret != PIO_NOERR) printf("Error at line = %d\n", __LINE__); }

/* Similar to rearrange_comp2io in src/clib/pio_rearrange.c */
int my_rearrange_io2comp(iosystem_desc_t *ios, io_desc_t *iodesc, const void *sbuf, void *rbuf, bool debug)
{
    MPI_Comm mycomm;
    int ntasks;
    int niotasks;
    int ret;

    if (iodesc->rearranger == PIO_REARR_BOX)
    {
        mycomm = ios->union_comm;
        niotasks = ios->num_iotasks;
    }
    else
    {
        mycomm = iodesc->subset_comm;
        niotasks = 1;
    }

    MPI_Comm_size(mycomm, &ntasks);

    ret = define_iodesc_datatypes(ios, iodesc); ERR

    int sendcounts[ntasks];
    int recvcounts[ntasks];
    int sdispls[ntasks];
    int rdispls[ntasks];
    MPI_Datatype sendtypes[ntasks];
    MPI_Datatype recvtypes[ntasks];

    for (int i = 0; i < ntasks; i++)
    {
        sendcounts[i] = 0;
        recvcounts[i] = 0;
        sdispls[i] = 0;
        rdispls[i] = 0;
        sendtypes[i] = MPI_DATATYPE_NULL;
        recvtypes[i] = MPI_DATATYPE_NULL;
    }

    if (ios->ioproc)
    {
        for (int i = 0; i < iodesc->nrecvs; i++)
        {
            if (iodesc->rtype[i] != MPI_DATATYPE_NULL)
            {
                if (iodesc->rearranger == PIO_REARR_SUBSET)
                {
                    if (sbuf)
                    {
                        sendcounts[i] = 1;
                        sendtypes[i] = iodesc->rtype[i];
                    }
                }
                else
                {
                    sendcounts[iodesc->rfrom[i]] = 1;
                    sendtypes[iodesc->rfrom[i]] = iodesc->rtype[i];
                }
            }
        }
    }

    for (int i = 0; i < niotasks; i++)
    {
        int io_comprank = ios->ioranks[i];

        if (iodesc->rearranger == PIO_REARR_SUBSET)
            io_comprank = 0;

        if (iodesc->scount[i] > 0 && iodesc->stype[i] != MPI_DATATYPE_NULL)
        {
            recvcounts[io_comprank] = 1;
            recvtypes[io_comprank] = iodesc->stype[i];
        }
    }

    if (debug)
    {
        if (ios->union_rank == 0)
        {
            printf("Before calling MPI_Alltoallw\n");
            fflush(stdout);
        }
    }

    /* Some MPI implementations (some vers of OpenMPI, MPICH 4.0 etc) do not
     * allow passing MPI_DATATYPE_NULL to comm functions (MPI_Alltoallw) even
     * though the send or recv length is 0, so using a dummy MPI type instead
     * of MPI_DATATYPE_NULL
     */
    MPI_Datatype dummy_dt = MPI_CHAR;
    MPI_Datatype sndtypes[ntasks], rcvtypes[ntasks];
    for (int i = 0; i < ntasks; i++)
    {
        sndtypes[i] = sendtypes[i];
        if (sndtypes[i] == MPI_DATATYPE_NULL)
        {
            sndtypes[i] = dummy_dt;
        }
        rcvtypes[i] = recvtypes[i];
        if (rcvtypes[i] == MPI_DATATYPE_NULL)
        {
            rcvtypes[i] = dummy_dt;
        }
    }

    ret = MPI_Alltoallw(sbuf, sendcounts, sdispls, sndtypes, rbuf, recvcounts, rdispls, rcvtypes, mycomm); ERR

    if (debug)
    {
        if (ios->union_rank == 0)
        {
            printf("After calling MPI_Alltoallw\n");
            fflush(stdout);
        }
    }

    return PIO_NOERR;
}

int main(int argc, char* argv[])
{
#ifdef TIMING
    GPTLinitialize();
#endif

    MPI_Init(&argc, &argv);

    int my_rank;
    int ntasks;

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    if (ntasks == 48)
    {
        const int ioproc_start = 0;
        const int ioproc_stride = 1;
        const int niotasks = 48;

        double io_buffer[433];
        double comp_buffer[220];

        int iosysid;

        int ioid_515;
        int ndims_515 = 0;
        int *gdimlen_515 = NULL;
        PIO_Offset fmaplen_515 = 0;
        PIO_Offset *compmap_515 = NULL;

        int ioid_516;
        int ndims_516 = 0;
        int *gdimlen_516 = NULL;
        PIO_Offset fmaplen_516 = 0;
        PIO_Offset *compmap_516 = NULL;

        int ioid_517;
        int ndims_517 = 0;
        int *gdimlen_517 = NULL;
        PIO_Offset fmaplen_517 = 0;
        PIO_Offset *compmap_517 = NULL;

        int ioid_518;
        int ndims_518 = 0;
        int *gdimlen_518 = NULL;
        PIO_Offset fmaplen_518 = 0;
        PIO_Offset *compmap_518 = NULL;

        int ioid_519;
        int ndims_519 = 0;
        int *gdimlen_519 = NULL;
        PIO_Offset fmaplen_519 = 0;
        PIO_Offset *compmap_519 = NULL;

        int ioid_520;
        int ndims_520 = 0;
        int *gdimlen_520 = NULL;
        PIO_Offset fmaplen_520 = 0;
        PIO_Offset *compmap_520 = NULL;

        int ioid_521;
        int ndims_521 = 0;
        int *gdimlen_521 = NULL;
        PIO_Offset fmaplen_521 = 0;
        PIO_Offset *compmap_521 = NULL;

        int ioid_522;
        int ndims_522 = 0;
        int *gdimlen_522 = NULL;
        PIO_Offset fmaplen_522 = 0;
        PIO_Offset *compmap_522 = NULL;

        int ioid_523;
        int ndims_523 = 0;
        int *gdimlen_523 = NULL;
        PIO_Offset fmaplen_523 = 0;
        PIO_Offset *compmap_523 = NULL;

        int ioid_524;
        int ndims_524 = 0;
        int *gdimlen_524 = NULL;
        PIO_Offset fmaplen_524 = 0;
        PIO_Offset *compmap_524 = NULL;

        int ioid_525;
        int ndims_525 = 0;
        int *gdimlen_525 = NULL;
        PIO_Offset fmaplen_525 = 0;
        PIO_Offset *compmap_525 = NULL;

        int ioid_526;
        int ndims_526 = 0;
        int *gdimlen_526 = NULL;
        PIO_Offset fmaplen_526 = 0;
        PIO_Offset *compmap_526 = NULL;

        PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_BOX, &iosysid);
        iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);

        /* 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_515.dat", &ndims_515, &gdimlen_515, &fmaplen_515, &compmap_515, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen_515, fmaplen_515, compmap_515, &ioid_515, NULL, NULL, NULL);

        /* 11 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_516.dat", &ndims_516, &gdimlen_516, &fmaplen_516, &compmap_516, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_516, fmaplen_516, compmap_516, &ioid_516, NULL, NULL, NULL);

        /* 1 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_517.dat", &ndims_517, &gdimlen_517, &fmaplen_517, &compmap_517, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_517, fmaplen_517, compmap_517, &ioid_517, NULL, NULL, NULL);

        /* 1 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_518.dat", &ndims_518, &gdimlen_518, &fmaplen_518, &compmap_518, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_518, fmaplen_518, compmap_518, &ioid_518, NULL, NULL, NULL);

        /* 3 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_519.dat", &ndims_519, &gdimlen_519, &fmaplen_519, &compmap_519, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_519, fmaplen_519, compmap_519, &ioid_519, NULL, NULL, NULL);

        /* 3 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_520.dat", &ndims_520, &gdimlen_520, &fmaplen_520, &compmap_520, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_520, fmaplen_520, compmap_520, &ioid_520, NULL, NULL, NULL);

        /* 2 x 3 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_521.dat", &ndims_521, &gdimlen_521, &fmaplen_521, &compmap_521, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 3, gdimlen_521, fmaplen_521, compmap_521, &ioid_521, NULL, NULL, NULL);

        /* 5 x 3 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_522.dat", &ndims_522, &gdimlen_522, &fmaplen_522, &compmap_522, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 3, gdimlen_522, fmaplen_522, compmap_522, &ioid_522, NULL, NULL, NULL);

        /* 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_523.dat", &ndims_523, &gdimlen_523, &fmaplen_523, &compmap_523, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen_523, fmaplen_523, compmap_523, &ioid_523, NULL, NULL, NULL);

        /* 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_524.dat", &ndims_524, &gdimlen_524, &fmaplen_524, &compmap_524, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen_524, fmaplen_524, compmap_524, &ioid_524, NULL, NULL, NULL);

        /* 17 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_525.dat", &ndims_525, &gdimlen_525, &fmaplen_525, &compmap_525, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_525, fmaplen_525, compmap_525, &ioid_525, NULL, NULL, NULL);

        /* 10 x 866 */
        PIOc_readmap("/home/wuda/datasets/piodecomp_526.dat", &ndims_526, &gdimlen_526, &fmaplen_526, &compmap_526, MPI_COMM_WORLD);
        PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_526, fmaplen_526, compmap_526, &ioid_526, NULL, NULL, NULL);

        io_desc_t *iodesc_515 = pio_get_iodesc_from_id(ioid_515);
        my_rearrange_io2comp(ios, iodesc_515, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_516 = pio_get_iodesc_from_id(ioid_516);
        my_rearrange_io2comp(ios, iodesc_516, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_517 = pio_get_iodesc_from_id(ioid_517);
        my_rearrange_io2comp(ios, iodesc_517, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_518 = pio_get_iodesc_from_id(ioid_518);
        my_rearrange_io2comp(ios, iodesc_518, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_519 = pio_get_iodesc_from_id(ioid_519);
        my_rearrange_io2comp(ios, iodesc_519, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_520 = pio_get_iodesc_from_id(ioid_520);
        my_rearrange_io2comp(ios, iodesc_520, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_521 = pio_get_iodesc_from_id(ioid_521);
        my_rearrange_io2comp(ios, iodesc_521, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_522 = pio_get_iodesc_from_id(ioid_522);
        my_rearrange_io2comp(ios, iodesc_522, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_523 = pio_get_iodesc_from_id(ioid_523);
        my_rearrange_io2comp(ios, iodesc_523, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_524 = pio_get_iodesc_from_id(ioid_524);
        my_rearrange_io2comp(ios, iodesc_524, io_buffer, comp_buffer, false);

        io_desc_t *iodesc_525 = pio_get_iodesc_from_id(ioid_525);
        my_rearrange_io2comp(ios, iodesc_525, io_buffer, comp_buffer, false);

        if (my_rank == 0)
        {
            printf("Before calling rearrange_io2comp with ioid_526\n");
            fflush(stdout);
        }

        io_desc_t *iodesc_526 = pio_get_iodesc_from_id(ioid_526);
        my_rearrange_io2comp(ios, iodesc_526, io_buffer, comp_buffer, true);

        if (my_rank == 0)
        {
            printf("After calling rearrange_io2comp with ioid_526\n");
            fflush(stdout);
        }

        free(compmap_515);
        free(gdimlen_515);
        PIOc_freedecomp(iosysid, ioid_515);

        free(compmap_516);
        free(gdimlen_516);
        PIOc_freedecomp(iosysid, ioid_516);

        free(compmap_517);
        free(gdimlen_517);
        PIOc_freedecomp(iosysid, ioid_517);

        free(compmap_518);
        free(gdimlen_518);
        PIOc_freedecomp(iosysid, ioid_518);

        free(compmap_519);
        free(gdimlen_519);
        PIOc_freedecomp(iosysid, ioid_519);

        free(compmap_520);
        free(gdimlen_520);
        PIOc_freedecomp(iosysid, ioid_520);

        free(compmap_521);
        free(gdimlen_521);
        PIOc_freedecomp(iosysid, ioid_521);

        free(compmap_522);
        free(gdimlen_522);
        PIOc_freedecomp(iosysid, ioid_522);

        free(compmap_523);
        free(gdimlen_523);
        PIOc_freedecomp(iosysid, ioid_523);

        free(compmap_524);
        free(gdimlen_524);
        PIOc_freedecomp(iosysid, ioid_524);

        free(compmap_525);
        free(gdimlen_525);
        PIOc_freedecomp(iosysid, ioid_525);

        free(compmap_526);
        free(gdimlen_526);
        PIOc_freedecomp(iosysid, ioid_526);

        PIOc_finalize(iosysid);
    }

    MPI_Finalize();

#ifdef TIMING
    GPTLfinalize();
#endif

    return 0;
}
