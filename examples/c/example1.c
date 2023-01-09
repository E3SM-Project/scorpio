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
    int my_rank;
    int ntasks;
    int format = PIO_IOTYPE_PNETCDF;
    int niotasks;
    const int ioproc_start = 0;
    const int ioproc_stride = 1;
    int iosysid;
    int ncid;
    int dimid;
    int varid;
    int ioid;
    int gdimlen[1] = {32};
    PIO_Offset* compmap = NULL;
    const int element_per_pe = 8;
    int write_buffer[element_per_pe];

    int ret = PIO_NOERR;

#ifdef TIMING
    GPTLinitialize();
#endif

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    if (ntasks != 4)
    {
        if (my_rank == 0)
            printf("Error: this program is intended to run on 4 processes only\n");

        MPI_Finalize();
        return 1;
    }

    niotasks = ntasks / ioproc_stride;
    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_SUBSET, &iosysid); ERR

    compmap = malloc(element_per_pe * sizeof(PIO_Offset));

    if (my_rank == 0)
    {
        compmap[0] = 4;
        compmap[1] = 2;
        compmap[2] = 1;
        compmap[3] = 3;

        compmap[4] = 32;
        compmap[5] = 31;
        compmap[6] = 30;
        compmap[7] = 29;
    }

    if (my_rank == 1)
    {
        compmap[0] = 5;
        compmap[1] = 7;
        compmap[2] = 6;
        compmap[3] = 8;

        compmap[4] = 28;
        compmap[5] = 27;
        compmap[6] = 25;
        compmap[7] = 26;
    }

    if (my_rank == 2)
    {
        compmap[0] = 9;
        compmap[1] = 10;
        compmap[2] = 12;
        compmap[3] = 11;

        compmap[4] = 21;
        compmap[5] = 24;
        compmap[6] = 22;
        compmap[7] = 23;
    }

    if (my_rank == 3)
    {
        compmap[0] = 19;
        compmap[1] = 17;
        compmap[2] = 15;
        compmap[3] = 13;
        compmap[4] = 20;
        compmap[5] = 18;
        compmap[6] = 14;
        compmap[7] = 16;
    }

    for (int i = 0; i < element_per_pe; i++)
        write_buffer[i] = 100 * my_rank + compmap[i];

    ret = PIOc_InitDecomp(iosysid, PIO_INT, 1, gdimlen, element_per_pe, compmap, &ioid, NULL, NULL, NULL); ERR

    ret = PIOc_createfile(iosysid, &ncid, &format, "test.nc", PIO_CLOBBER); ERR

    ret = PIOc_def_dim(ncid, "ncol", gdimlen[0], &dimid); ERR
    ret = PIOc_def_var(ncid, "data", PIO_INT, 1, &dimid, &varid); ERR

    ret = PIOc_enddef(ncid); ERR

    ret = PIOc_write_darray(ncid, varid, ioid, element_per_pe, write_buffer, NULL); ERR

    ret = PIOc_closefile(ncid); ERR

    free(compmap);

    ret = PIOc_freedecomp(iosysid, ioid); ERR

    ret = PIOc_finalize(iosysid); ERR

    MPI_Finalize();

#ifdef TIMING
    GPTLfinalize();
#endif

    return 0;
}
