#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <pio.h>
#include <assert.h>
#include <math.h>
#ifdef SPIO_ENABLE_GPTL_TIMING
#include <gptl.h>
#endif

/* FIXME: Either remove this test or make it generic for all I/O types */
/* Test HDF5 + PIO_USE_ASYNC_WR_THREAD + PIO_REARR_CONTIG */

#define ERR_CHECK(msg) \
  do { \
    if (ret != PIO_NOERR) { \
      fprintf(stderr, "rank = %d, FAIL: %s, ret = %d, at %s:%d\n", my_rank, msg, ret, __FILE__, __LINE__); \
      num_errors++; \
    } \
  } while (0)

#define ELEM_PER_PE 4
#define DOUBLE_EPS 1E-9

int main(int argc, char *argv[])
{
#if defined(_HDF5)
    int my_rank;
    int ntasks;
    int format = PIO_IOTYPE_HDF5;
    int niotasks;
    const int ioproc_start = 0;
    const int ioproc_stride = 1;
    const int rearranger = PIO_REARR_CONTIG;
    const char filename[] = "test_hdf5_async_contig.nc";
    int iosysid;
    int ncid;
    int dimid_x;
    int varid_double;   /* decomp PIO type == file var type (no dt conversion) */
    int varid_dtconv;   /* decomp PIO type != file var type (forces dt_conv_reqd) */
    int ioid_double;
    int ioid_dtconv;
    int gdimlen[1];
    int ret = PIO_NOERR;
    int num_errors = 0;
    int i;

#ifdef SPIO_ENABLE_GPTL_TIMING
    GPTLinitialize();
#endif

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    /* A single dedicated IO task */
    niotasks = 1;

    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start,
                               rearranger, &iosysid); ERR_CHECK("PIOc_Init_Intracomm failed");

    gdimlen[0] = ntasks * ELEM_PER_PE;

    PIO_Offset compmap[ELEM_PER_PE];
    for (i = 0; i < ELEM_PER_PE; i++)
        compmap[i] = (PIO_Offset)(my_rank * ELEM_PER_PE + i + 1);

    /* Decomposition where the decomp type and variable type match - no data conversion reqd */
    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen, ELEM_PER_PE, compmap,
                           &ioid_double, NULL, NULL, NULL); ERR_CHECK("PIOc_InitDecomp (double) failed");

    /* Decomposition where the decomp type, PIO_INT, and variable type, PIO_DOUBLE, do NOT match - data conversion reqd */
    ret = PIOc_InitDecomp(iosysid, PIO_INT, 1, gdimlen, ELEM_PER_PE, compmap,
                           &ioid_dtconv, NULL, NULL, NULL); ERR_CHECK("PIOc_InitDecomp (dtconv) failed");

    ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER); ERR_CHECK("PIOc_createfile failed");

    ret = PIOc_def_dim(ncid, "x", (PIO_Offset)gdimlen[0], &dimid_x); ERR_CHECK("PIOc_def_dim failed");

    ret = PIOc_def_var(ncid, "var_double_contig", PIO_DOUBLE, 1, &dimid_x, &varid_double);
    ERR_CHECK("PIOc_def_var (double) failed");

    /* Variable type PIO_DOUBLE */
    ret = PIOc_def_var(ncid, "var_dtconv_contig", PIO_DOUBLE, 1, &dimid_x, &varid_dtconv);
    ERR_CHECK("PIOc_def_var (dtconv) failed");

    ret = PIOc_enddef(ncid); ERR_CHECK("PIOc_enddef failed");

    double wdata_double[ELEM_PER_PE];
    int wdata_dtconv[ELEM_PER_PE];
    for (i = 0; i < ELEM_PER_PE; i++)
    {
        wdata_double[i] = (double)my_rank + 0.1 * (double)i;
        wdata_dtconv[i] = my_rank * 100 + i;
    }

    ret = PIOc_write_darray(ncid, varid_double, ioid_double, ELEM_PER_PE, wdata_double, NULL);
    ERR_CHECK("PIOc_write_darray (double) failed");

    ret = PIOc_write_darray(ncid, varid_dtconv, ioid_dtconv, ELEM_PER_PE, wdata_dtconv, NULL);
    ERR_CHECK("PIOc_write_darray (dtconv) failed");

    ret = PIOc_sync(ncid); ERR_CHECK("PIOc_sync failed");

    double rdata_double[ELEM_PER_PE];
    int rdata_dtconv[ELEM_PER_PE];
    for (i = 0; i < ELEM_PER_PE; i++)
    {
        rdata_double[i] = -1.0;
        rdata_dtconv[i] = -1;
    }

    ret = PIOc_read_darray(ncid, varid_double, ioid_double, ELEM_PER_PE, rdata_double);
    ERR_CHECK("PIOc_read_darray (double) failed");

    ret = PIOc_read_darray(ncid, varid_dtconv, ioid_dtconv, ELEM_PER_PE, rdata_dtconv);
    ERR_CHECK("PIOc_read_darray (dtconv) failed");

    /* Compare the read-back data against what was written, element-by-element. */
    for (i = 0; i < ELEM_PER_PE; i++)
    {
        double diff = fabs(rdata_double[i] - wdata_double[i]);
        if (diff >= DOUBLE_EPS)
        {
            fprintf(stderr, "rank = %d, FAIL: var_double_contig[%d]: expected %f, got %f (diff = %g)\n",
                    my_rank, i, wdata_double[i], rdata_double[i], diff);
            num_errors++;
        }
    }

    for (i = 0; i < ELEM_PER_PE; i++)
    {
        if (rdata_dtconv[i] != wdata_dtconv[i])
        {
            fprintf(stderr, "rank = %d, FAIL: var_dtconv_contig[%d]: expected %d, got %d\n",
                    my_rank, i, wdata_dtconv[i], rdata_dtconv[i]);
            num_errors++;
        }
    }

    ret = PIOc_closefile(ncid); ERR_CHECK("PIOc_closefile failed");

    ret = PIOc_freedecomp(iosysid, ioid_double); ERR_CHECK("PIOc_freedecomp (double) failed");
    ret = PIOc_freedecomp(iosysid, ioid_dtconv); ERR_CHECK("PIOc_freedecomp (dtconv) failed");

    ret = PIOc_finalize(iosysid); ERR_CHECK("PIOc_finalize failed");

    MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
    GPTLfinalize();
#endif

    if (num_errors != 0)
    {
        fprintf(stderr, "rank = %d, %s: FAILED with %d error(s)\n", my_rank, argv[0], num_errors);
        return -1;
    }

    if (my_rank == 0)
        printf("%s: SUCCESS!!\n", argv[0]);

    return 0;

#else /* !defined(_HDF5) */
    printf("SCORPIO is not configured with HDF5 support.\n");

    /* Make the test fail if HDF5 support is not enabled/available */
    return -1;
#endif
}
