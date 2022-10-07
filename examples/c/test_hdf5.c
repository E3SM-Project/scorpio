#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#ifdef TIMING
#include <gptl.h>
#endif

#define ERR { if (ret != PIO_NOERR) printf("rank = %d, error at line = %d\n", my_rank, __LINE__); }

#define DIM_LEV_LEN 3
#define MAX_STR_LEN 8

int main(int argc, char* argv[])
{
#ifdef _HDF5
    int my_rank;
    int ntasks;
    int format = PIO_IOTYPE_HDF5;
    int niotasks;
    const int ioproc_start = 0;
    const int ioproc_stride = 1;
    int iosysid;
    int ncid;

    int dimids[3];
    int dimid_time;
    int dimid_nbnd;
    int dimid_chars;
    int dimid_lev;
    int dimid_ncol;
    int dimid_sample;
    int dimid_ship;

    int varid_lat;
    int varid_lev;
    int varid_P0;
    int varid_time;
    int varid_date;
    int varid_time_bnds;
    int varid_date_written;
    int varid_CLDHGH;
    int varid_U;
    int varid_sample;
    int varid_ship;

    const int element_per_pe_ncol = 4;
    const int element_per_pe_lev_ncol = DIM_LEV_LEN * element_per_pe_ncol;

    int ioid_double_1D;
    int gdimlen_ncol[1];
    PIO_Offset* compmap_ncol = NULL;
    double double_buffer_ncol[element_per_pe_ncol];

    int ioid_double_2D;
    int gdimlen_lev_ncol[2];
    PIO_Offset* compmap_lev_ncol = NULL;
    double double_buffer_lev_ncol[element_per_pe_lev_ncol];

    char put_string_data[MAX_STR_LEN + 1];

    PIO_Offset start[3];
    PIO_Offset count[3];

    int ret = PIO_NOERR;

#ifdef TIMING
    GPTLinitialize();
#endif

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    niotasks = ntasks / ioproc_stride;
    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_SUBSET, &iosysid); ERR

    gdimlen_ncol[0] = ntasks * element_per_pe_ncol;

    compmap_ncol = malloc(element_per_pe_ncol * sizeof(PIO_Offset));
    for (int i = 0; i < element_per_pe_ncol; i++)
    {
        compmap_ncol[i] = my_rank * element_per_pe_ncol + i + 1;
        double_buffer_ncol[i] = my_rank;
    }

    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen_ncol, element_per_pe_ncol, compmap_ncol, &ioid_double_1D, NULL, NULL, NULL); ERR

    gdimlen_lev_ncol[0] = DIM_LEV_LEN;
    gdimlen_lev_ncol[1] = gdimlen_ncol[0];

    compmap_lev_ncol = malloc(element_per_pe_lev_ncol * sizeof(PIO_Offset));
    for (int i = 0; i < element_per_pe_lev_ncol; i++)
    {
        compmap_lev_ncol[i] = my_rank * element_per_pe_lev_ncol + i + 1;
        double_buffer_lev_ncol[i] = my_rank;
    }

    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_lev_ncol, element_per_pe_lev_ncol, compmap_lev_ncol, &ioid_double_2D, NULL, NULL, NULL); ERR

    ret = PIOc_createfile(iosysid, &ncid, &format, "test.nc", PIO_CLOBBER); ERR

    /* Put some global attributes */
    int ne_val = 120;
    ret = PIOc_put_att(ncid, PIO_GLOBAL, "ne", PIO_INT, 1, &ne_val); ERR
    const char* title_val = "EAM History file information";
    ret = PIOc_put_att_text(ncid, PIO_GLOBAL, "title", strlen(title_val), title_val); ERR

    ret = PIOc_def_dim(ncid, "time", PIO_UNLIMITED, &dimid_time); ERR
    ret = PIOc_def_dim(ncid, "nbnd", 2, &dimid_nbnd); ERR
    ret = PIOc_def_dim(ncid, "chars", MAX_STR_LEN, &dimid_chars); ERR
    ret = PIOc_def_dim(ncid, "lev", DIM_LEV_LEN, &dimid_lev); ERR
    ret = PIOc_def_dim(ncid, "ncol", gdimlen_ncol[0], &dimid_ncol); ERR

    ret = PIOc_def_var(ncid, "lat", PIO_DOUBLE, 1, &dimid_ncol, &varid_lat); ERR
    ret = PIOc_def_var(ncid, "lev", PIO_DOUBLE, 1, &dimid_lev, &varid_lev); ERR
    ret = PIOc_def_var(ncid, "P0", PIO_DOUBLE, 0, NULL, &varid_P0); ERR
    ret = PIOc_def_var(ncid, "time", PIO_DOUBLE, 1, &dimid_time, &varid_time); ERR
    ret = PIOc_def_var(ncid, "date", PIO_INT, 1, &dimid_time, &varid_date); ERR

    dimids[0] = dimid_time;
    dimids[1] = dimid_nbnd;
    ret = PIOc_def_var(ncid, "time_bnds", PIO_DOUBLE, 2, dimids, &varid_time_bnds); ERR

    dimids[0] = dimid_time;
    dimids[1] = dimid_chars;
    ret = PIOc_def_var(ncid, "date_written", PIO_CHAR, 2, dimids, &varid_date_written); ERR

    dimids[0] = dimid_time;
    dimids[1] = dimid_ncol;
    ret = PIOc_def_var(ncid, "CLDHGH", PIO_FLOAT, 2, dimids, &varid_CLDHGH); ERR

    dimids[0] = dimid_time;
    dimids[1] = dimid_lev;
    dimids[2] = dimid_ncol;
    ret = PIOc_def_var(ncid, "U", PIO_FLOAT, 3, dimids, &varid_U); ERR

    /* Put some local attributes */
    ret = PIOc_put_att(ncid, varid_lat, "long_name", PIO_CHAR, strlen("latitude"), "latitude"); ERR
    ret = PIOc_put_att(ncid, varid_lat, "units", PIO_CHAR, strlen("degrees_north"), "degrees_north"); ERR
    ret = PIOc_put_att(ncid, varid_P0, "long_name", PIO_CHAR, strlen("reference pressure"), "reference pressure"); ERR
    ret = PIOc_put_att(ncid, varid_P0, "units", PIO_CHAR, strlen("Pa"), "Pa"); ERR
    int mdims_val = 1;
    ret = PIOc_put_att(ncid, varid_U, "mdims", PIO_INT, 1, &mdims_val); ERR
    ret = PIOc_put_att(ncid, varid_U, "units", PIO_CHAR, strlen("m/s"), "m/s"); ERR

    ret = PIOc_def_dim(ncid, "sample", 3, &dimid_sample); ERR
    ret = PIOc_def_dim(ncid, "ship", 4, &dimid_ship); ERR

    dimids[0] = dimid_time;
    dimids[1] = dimid_sample;
    ret = PIOc_def_var(ncid, "sample", PIO_INT, 2, dimids, &varid_sample); ERR

    dimids[0] = dimid_ship;
    dimids[1] = dimid_chars;
    ret = PIOc_def_var(ncid, "ship", PIO_CHAR, 2, dimids, &varid_ship); ERR

    ret = PIOc_enddef(ncid); ERR

    /* Put variable P0 */
    double P0_val = 99999.9999;
    ret = PIOc_put_var_double(ncid, varid_P0, &P0_val); ERR

    /* Put variable lev */
    double lev_vals[DIM_LEV_LEN] = {0.2, 0.4, 0.6};
    ret = PIOc_put_var_double(ncid, varid_lev, lev_vals); ERR

    /* Put variable time */
    double time_val;

    start[0] = 0;
    count[0] = 1;
    time_val = 0.1;
    ret = PIOc_put_vars_double(ncid, varid_time, start, count, NULL, &time_val); ERR

    start[0] = 1;
    count[0] = 1;
    time_val = 0.3;
    ret = PIOc_put_vars_double(ncid, varid_time, start, count, NULL, &time_val); ERR

    start[0] = 2;
    count[0] = 1;
    time_val = 0.5;
    ret = PIOc_put_vars_double(ncid, varid_time, start, count, NULL, &time_val); ERR

    /* Put variable date */
    int date_val;

    start[0] = 0;
    count[0] = 1;
    date_val = 10101;
    ret = PIOc_put_vars_int(ncid, varid_date, start, count, NULL, &date_val); ERR

    start[0] = 1;
    count[0] = 1;
    date_val = 10102;
    ret = PIOc_put_vars_int(ncid, varid_date, start, count, NULL, &date_val); ERR

    start[0] = 2;
    count[0] = 1;
    date_val = 10103;
    ret = PIOc_put_vars_int(ncid, varid_date, start, count, NULL, &date_val); ERR

    /* Put variable time_bnds */
    double put_time_bnds_buffer[2];

    start[1] = 0;
    count[1] = 2;

    start[0] = 0;
    count[0] = 1;
    put_time_bnds_buffer[0] = 0.0;
    put_time_bnds_buffer[1] = 0.0;
    ret = PIOc_put_vars_double(ncid, varid_time_bnds, start, count, NULL, put_time_bnds_buffer); ERR

    start[0] = 1;
    count[0] = 1;
    put_time_bnds_buffer[0] = 0.0;
    put_time_bnds_buffer[1] = 0.08;
    ret = PIOc_put_vars_double(ncid, varid_time_bnds, start, count, NULL, put_time_bnds_buffer); ERR

    start[0] = 2;
    count[0] = 1;
    put_time_bnds_buffer[0] = 0.08;
    put_time_bnds_buffer[1] = 0.16;
    ret = PIOc_put_vars_double(ncid, varid_time_bnds, start, count, NULL, put_time_bnds_buffer); ERR

    /* Put variable date_written */
    start[1] = 0;
    count[1] = MAX_STR_LEN;

    start[0] = 0;
    count[0] = 1;
    memset(put_string_data, 0, sizeof(put_string_data));
    strcpy(put_string_data, "06/15/21");
    ret = PIOc_put_vara_text(ncid, varid_date_written, start, count, put_string_data); ERR

    start[0] = 1;
    count[0] = 1;
    memset(put_string_data, 0, sizeof(put_string_data));
    strcpy(put_string_data, "06/16/21");
    ret = PIOc_put_vara_text(ncid, varid_date_written, start, count, put_string_data); ERR

    start[0] = 2;
    count[0] = 1;
    memset(put_string_data, 0, sizeof(put_string_data));
    strcpy(put_string_data, "06/17/21");
    ret = PIOc_put_vara_text(ncid, varid_date_written, start, count, put_string_data); ERR

    /* Write variable lat */
    ret = PIOc_write_darray(ncid, varid_lat, ioid_double_1D, element_per_pe_ncol, double_buffer_ncol, NULL); ERR

    /* Write variable U */
    ret = PIOc_setframe(ncid, varid_U, 0);
    ret = PIOc_write_darray(ncid, varid_U, ioid_double_2D, element_per_pe_lev_ncol, double_buffer_lev_ncol, NULL); ERR

    ret = PIOc_setframe(ncid, varid_U, 1);
    ret = PIOc_write_darray(ncid, varid_U, ioid_double_2D, element_per_pe_lev_ncol, double_buffer_lev_ncol, NULL); ERR

    /* Write variable CLDHGH */
    ret = PIOc_setframe(ncid, varid_CLDHGH, 0);
    ret = PIOc_write_darray(ncid, varid_CLDHGH, ioid_double_1D, element_per_pe_ncol, double_buffer_ncol, NULL); ERR

    ret = PIOc_setframe(ncid, varid_CLDHGH, 1);
    ret = PIOc_write_darray(ncid, varid_CLDHGH, ioid_double_1D, element_per_pe_ncol, double_buffer_ncol, NULL); ERR

    ret = PIOc_closefile(ncid); ERR

    free(compmap_ncol);
    free(compmap_lev_ncol);

    ret = PIOc_freedecomp(iosysid, ioid_double_1D); ERR
    ret = PIOc_freedecomp(iosysid, ioid_double_2D); ERR

    ret = PIOc_finalize(iosysid); ERR

    MPI_Finalize();

#ifdef TIMING
    GPTLfinalize();
#endif

#endif

    return 0;
}

