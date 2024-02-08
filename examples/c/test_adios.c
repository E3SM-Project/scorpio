#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#include <assert.h>
#ifdef TIMING
#include <gptl.h>
#endif

#define ERR { if (ret != PIO_NOERR) printf("rank = %d, error at line = %d\n", my_rank, __LINE__); }

int main(int argc, char* argv[])
{
    const int dim10_len = 10;
    const int dim2_len = 2;
    const int elem_len = 393216;
    const int gp_len = 4;
    const int ilev_len = 129;
    const int lev_len = 128;
    const int ncol_len = 1572864;
    const int element_per_pe = 10485760;
    const int niotasks = 96;
    const int ioproc_stride = 8;
    const int ioproc_start = 0;

    int my_rank;
    int ntasks;

    int format = PIO_IOTYPE_ADIOS; /* Change to PIO_IOTYPE_PNETCDF for comparison */

    int iosysid;

    int ncid;
    int ncid_read;

    int dimids[6];
    int dimid_time;
    int dimid_dim10;
    int dimid_dim2;
    int dimid_elem;
    int dimid_gp;
    int dimid_ilev;
    int dimid_lev;
    int dimid_ncol;

    int varid_write;
    int varid_time;
    int varid_Qdp_dyn;

    int ioid_double_5D;
    int gdimlen[5] = {elem_len, dim10_len, gp_len, gp_len, lev_len};

    PIO_Offset* compmap = calloc(element_per_pe, sizeof(PIO_Offset));
    double* write_buffer = calloc(element_per_pe, sizeof(double));
    double* read_buffer = calloc(element_per_pe, sizeof(double));

    double read_time;
    double max_read_time;
    double min_read_time;
    double sum_read_time;

    int ret = PIO_NOERR;

#ifdef TIMING
    GPTLinitialize();
#endif

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    if (ntasks != 768)
    {
        if (my_rank == 0)
            printf("This test must be run with exactly 768 MPI tasks!\n");

        MPI_Finalize();

#ifdef TIMING
        GPTLfinalize();
#endif

        return -1;
    }

    for (int i = 0; i < element_per_pe; i++)
    {
        compmap[i] = my_rank * (PIO_Offset)element_per_pe + i + 1;
        write_buffer[i] = my_rank;
    }

    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_BOX, &iosysid); ERR

    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 5, gdimlen, element_per_pe, compmap, &ioid_double_5D, NULL, NULL, NULL); ERR

    ret = PIOc_createfile(iosysid, &ncid, &format, "test_read.nc", PIO_CLOBBER); ERR

    ret = PIOc_def_dim(ncid, "time", PIO_UNLIMITED, &dimid_time); ERR
    ret = PIOc_def_dim(ncid, "dim10", dim10_len, &dimid_dim10); ERR
    ret = PIOc_def_dim(ncid, "dim2", dim2_len, &dimid_dim2); ERR
    ret = PIOc_def_dim(ncid, "elem", elem_len, &dimid_elem); ERR
    ret = PIOc_def_dim(ncid, "gp", gp_len, &dimid_gp); ERR
    ret = PIOc_def_dim(ncid, "ilev", ilev_len, &dimid_ilev); ERR
    ret = PIOc_def_dim(ncid, "lev", lev_len, &dimid_lev); ERR
    ret = PIOc_def_dim(ncid, "ncol", ncol_len, &dimid_ncol); ERR

    ret = PIOc_def_var(ncid, "time", PIO_DOUBLE, 1, &dimid_time, &varid_time); ERR

    dimids[0] = dimid_time;
    dimids[1] = dimid_elem;
    dimids[2] = dimid_dim10;
    dimids[3] = dimid_gp;
    dimids[4] = dimid_gp;
    dimids[5] = dimid_lev;
    ret = PIOc_def_var(ncid, "Qdp_dyn", PIO_DOUBLE, 6, dimids, &varid_Qdp_dyn); ERR

    ret = PIOc_enddef(ncid); ERR

    ret = PIOc_setframe(ncid, varid_Qdp_dyn, 0); ERR
    ret = PIOc_write_darray(ncid, varid_Qdp_dyn, ioid_double_5D, element_per_pe, write_buffer, NULL); ERR

    ret = PIOc_closefile(ncid); ERR

    ret = PIOc_openfile(iosysid, &ncid_read, &format, "test_read.nc", PIO_NOWRITE); ERR

    varid_Qdp_dyn = -1;
    ret = PIOc_inq_varid(ncid_read, "Qdp_dyn", &varid_Qdp_dyn); ERR

    MPI_Barrier(MPI_COMM_WORLD);

    read_time = MPI_Wtime();
    ret = PIOc_read_darray(ncid_read, varid_Qdp_dyn, ioid_double_5D, element_per_pe, read_buffer); ERR
    read_time = MPI_Wtime() - read_time;

    MPI_Reduce(&read_time, &max_read_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
    MPI_Reduce(&read_time, &min_read_time, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
    MPI_Reduce(&read_time, &sum_read_time, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

    if (my_rank == 0)
    {
        printf("max read time = %lf, min read time = %lf, average read time = %lf\n",
               max_read_time, min_read_time, sum_read_time / ntasks);
    }

    ret = PIOc_closefile(ncid_read); ERR

    ret = PIOc_freedecomp(iosysid, ioid_double_5D); ERR

    free(compmap);
    free(write_buffer);
    free(read_buffer);

    ret = PIOc_finalize(iosysid); ERR

    MPI_Finalize();

#ifdef TIMING
    GPTLfinalize();
#endif

    return 0;
}
