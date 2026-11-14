#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <pio.h>
#include <assert.h>
#include <math.h>
#ifdef SPIO_ENABLE_GPTL_TIMING
#include <gptl.h>
#endif

/*
 * This example is built only when both WITH_HDF5 and PIO_USE_ASYNC_WR_THREAD
 * are enabled (see examples/c/CMakeLists.txt), so it specifically targets the
 * HDF5/HDF5C async-BOX write dispatch added by this plan:
 * io_desc_t::select_io_desc_info() now also returns the async io_desc_info_t
 * for PIO_REARR_BOX decompositions (previously PIO_REARR_CONTIG-only), so
 * this example deliberately uses PIO_REARR_BOX with the HDF5 IO type to
 * route the darray writes below through pio_iosys_async_hdf5_write_op_add()
 * / pio_iosys_async_op_hdf5_write() whenever PIO_USE_ASYNC_WR_THREAD is on
 * (always true in this build, since the whole translation unit is gated on
 * it). It is otherwise a line-for-line sibling of test_hdf5_async_contig.c -
 * see that file's header comment for the read-back-on-the-same-open-file
 * rationale and the disjoint-sync/async-procs fallback behavior, both of
 * which apply identically to BOX.
 */

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
    const int rearranger = PIO_REARR_BOX;
    const char filename[] = "test_hdf5_async_box.nc";
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

    /* A single dedicated IO task, like test_hdf5_async_contig.c, so that with
     * PIO_USE_ASYNC_WR_THREAD on the sync and async IO comms can genuinely be
     * distinct from each other and from the remaining compute-only ranks. */
    niotasks = 1;

    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start,
                               rearranger, &iosysid); ERR_CHECK("PIOc_Init_Intracomm failed");

    gdimlen[0] = ntasks * ELEM_PER_PE;

    PIO_Offset compmap[ELEM_PER_PE];
    for (i = 0; i < ELEM_PER_PE; i++)
        compmap[i] = (PIO_Offset)(my_rank * ELEM_PER_PE + i + 1);

    /* Decomposition used for the no-conversion variable: PIO type matches
     * the file variable's declared type (PIO_DOUBLE both sides). */
    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen, ELEM_PER_PE, compmap,
                           &ioid_double, NULL, NULL, NULL); ERR_CHECK("PIOc_InitDecomp (double) failed");

    /* Decomposition used for the dt-conversion variable: PIO type (PIO_INT,
     * native HDF5 type H5T_NATIVE_INT) deliberately differs from the file
     * variable's declared type below (PIO_DOUBLE, native HDF5 type
     * H5T_NATIVE_DOUBLE) - H5Tequal() of those two is always false, so this
     * forces dt_conv_reqd == true on both the write and read paths. Small
     * integer values round-trip exactly through a double, so an exact
     * comparison on read-back is still valid. */
    ret = PIOc_InitDecomp(iosysid, PIO_INT, 1, gdimlen, ELEM_PER_PE, compmap,
                           &ioid_dtconv, NULL, NULL, NULL); ERR_CHECK("PIOc_InitDecomp (dtconv) failed");

    ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER); ERR_CHECK("PIOc_createfile failed");

    ret = PIOc_def_dim(ncid, "x", (PIO_Offset)gdimlen[0], &dimid_x); ERR_CHECK("PIOc_def_dim failed");

    ret = PIOc_def_var(ncid, "var_double_box", PIO_DOUBLE, 1, &dimid_x, &varid_double);
    ERR_CHECK("PIOc_def_var (double) failed");

    /* File type PIO_DOUBLE, deliberately different from ioid_dtconv's
     * PIO_INT decomposition type above - see the comment near
     * PIOc_InitDecomp(ioid_dtconv) for why this forces dt_conv_reqd. */
    ret = PIOc_def_var(ncid, "var_dtconv_box", PIO_DOUBLE, 1, &dimid_x, &varid_dtconv);
    ERR_CHECK("PIOc_def_var (dtconv) failed");

    ret = PIOc_enddef(ncid); ERR_CHECK("PIOc_enddef failed");

    double wdata_double[ELEM_PER_PE];
    int wdata_dtconv[ELEM_PER_PE];
    for (i = 0; i < ELEM_PER_PE; i++)
    {
        wdata_double[i] = (double)my_rank + 0.1 * (double)i;
        wdata_dtconv[i] = my_rank * 100 + i;
    }

    /* Write both darray variables. With PIO_USE_ASYNC_WR_THREAD=ON every
     * HDF5/HDF5C darray write is queued on the async write thread pool; since
     * Task 7, whether the queued op targets the async or the sync I/O ranks
     * is decided by select_io_desc_info() for BOX exactly as it already was
     * for CONTIG. With niotasks=1 on more than one MPI rank the sync and
     * async I/O rank sets are disjoint, so these BOX writes are routed (with
     * a one-time warning) to the sync I/O ranks. */
    ret = PIOc_write_darray(ncid, varid_double, ioid_double, ELEM_PER_PE, wdata_double, NULL);
    ERR_CHECK("PIOc_write_darray (double) failed");

    ret = PIOc_write_darray(ncid, varid_dtconv, ioid_dtconv, ELEM_PER_PE, wdata_dtconv, NULL);
    ERR_CHECK("PIOc_write_darray (dtconv) failed");

    /* Flush the queued (possibly still-async) writes without closing the
     * file, so file->iotype stays PIO_IOTYPE_HDF5 for the PIOc_read_darray()
     * calls below - see test_hdf5_async_contig.c's header comment for why a
     * close+reopen would silently reroute reads to the NETCDF4P path. */
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
            fprintf(stderr, "rank = %d, FAIL: var_double_box[%d]: expected %f, got %f (diff = %g)\n",
                    my_rank, i, wdata_double[i], rdata_double[i], diff);
            num_errors++;
        }
    }

    for (i = 0; i < ELEM_PER_PE; i++)
    {
        if (rdata_dtconv[i] != wdata_dtconv[i])
        {
            fprintf(stderr, "rank = %d, FAIL: var_dtconv_box[%d]: expected %d, got %d\n",
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

    /* Make the test fail if HDF5 support is not enabled/available - this
     * example is only ever built when WITH_HDF5 is on (see
     * examples/c/CMakeLists.txt), so reaching here would indicate a CMake
     * wiring bug. */
    return -1;
#endif
}
