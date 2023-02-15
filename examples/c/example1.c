#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#ifdef TIMING
#include <gptl.h>
#endif

#define MAX_VARS 63

#define ERR { if (ret != PIO_NOERR) printf("rank = %d, error at line = %d\n", my_rank, __LINE__); }

int main(int argc, char* argv[])
{
    int my_rank;
    int ntasks;
    int format = PIO_IOTYPE_ADIOS; /* Change format to PIO_IOTYPE_PNETCDF to compare write performance. */
    int niotasks;
    const int ioproc_start = 0;
    const int ioproc_stride = 84;
    char varname[PIO_MAX_NAME];
    int iosysid;
    int ncid;

    int dimids[3];
    int dimid_time;
    int dimid_ncol;
    int dimid_lev;

    int varids[MAX_VARS];

    const int ncol_len = 346752;
    const int lev_len = 72;

    int element_per_pe;

    int ioid;
    int gdimlen[2];
    PIO_Offset compmap[1152]; /* 346752 * 72 / 21672 = 1152 */
    double write_buffer[1152]; /* 346752 * 72 / 21672 = 1152 */

    int ret = PIO_NOERR;

#ifdef TIMING
    GPTLinitialize();
#endif

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    if (ntasks != 21672)
    {
        if (my_rank == 0)
            printf("Error: this program is intended to run on 21672 processes only\n");

        MPI_Finalize();
        return 1;
    }

    element_per_pe = lev_len * ncol_len / ntasks;

    gdimlen[0] = lev_len;
    gdimlen[1] = ncol_len;

    for (int i = 0; i < element_per_pe; i++)
    {
        compmap[i] = my_rank * element_per_pe + i + 1;
        write_buffer[i] = my_rank;
    }

    niotasks = ntasks / ioproc_stride;

    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_BOX, &iosysid); ERR

    ncid = -1;
    ret = PIOc_createfile(iosysid, &ncid, &format, "test.nc", PIO_CLOBBER | PIO_64BIT_DATA); ERR

    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen, element_per_pe, compmap, &ioid, NULL, NULL, NULL); ERR

    ret = PIOc_def_dim(ncid, "time", PIO_UNLIMITED, &dimid_time); ERR
    ret = PIOc_def_dim(ncid, "lev", lev_len, &dimid_lev); ERR
    ret = PIOc_def_dim(ncid, "ncol", ncol_len, &dimid_ncol); ERR

    dimids[0] = dimid_time;
    dimids[1] = dimid_lev;
    dimids[2] = dimid_ncol;
    for (int i = 0; i < MAX_VARS; i++)
    {
        snprintf(varname, PIO_MAX_NAME, "var_time_lev_ncol_%03d", i);
        ret = PIOc_def_var(ncid, varname, PIO_FLOAT, 3, dimids, &varids[i]); ERR
    }

    ret = PIOc_enddef(ncid); ERR

    for (int i = 0; i < MAX_VARS; i++)
    {
        ret = PIOc_setframe(ncid, varids[i], 0); ERR
        ret = PIOc_write_darray(ncid, varids[i], ioid, element_per_pe, write_buffer, NULL); ERR
    }

    ret = PIOc_closefile(ncid); ERR

    ret = PIOc_freedecomp(iosysid, ioid); ERR

    ret = PIOc_finalize(iosysid); ERR

    MPI_Finalize();

#ifdef TIMING
    GPTLfinalize();
#endif

    return 0;
}
