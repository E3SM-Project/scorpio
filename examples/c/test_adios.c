#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#include <assert.h>
#include <math.h>
#ifdef TIMING
#include <gptl.h>
#endif

#define ERR { if (ret != PIO_NOERR) { printf("rank = %d, error at line = %d\n", my_rank, __LINE__); return -1; } }

#define DIM_LEV_LEN 3
#define MAX_STR_LEN 8
#define MAX_LAT_LON_VARS 400

#ifdef _ADIOS2
int test_adios_read_case_1();

int test_adios_read_case_2();

int test_adios_read_case_3();

int test_adios_cdf5_new_data_types();
#endif

int main(int argc, char* argv[])
{
#ifdef _ADIOS2
    int my_rank;
    int ntasks;
    int format = PIO_IOTYPE_ADIOS;
    int niotasks;
    const int ioproc_start = 0;
    const int ioproc_stride = 1;
    const int rearranger[2] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
    char filename[PIO_MAX_NAME + 1];
    char varname[PIO_MAX_NAME + 1];
    int iosysid;
    int ncid;

    int dimids[3];
    int dimid;
    int dimid_time;
    int dimid_lon;
    int dimid_lat;
    int dimid_nbnd;
    int dimid_chars;
    int dimid_lev;
    int dimid_ncol;
    int dimid_sample;
    int dimid_ship;

    int varid;
    int varid_time;
    int varid_lon;
    int varid_lat;
    int varid_lev;
    int varid_P0;
    int varid_date;
    int varid_time_bnds;
    int varid_date_written;
    int varid_CLDHGH;
    int varid_U;
    int varid_sample;
    int varid_ship;

    int att_id;
    int att_type;
    PIO_Offset att_len;
    char att_name[PIO_MAX_NAME + 1];

    int ndims;
    int nvars;
    int ngatts;
    int unlimdimid;

    const int element_per_pe_ncol = 4;
    const int element_per_pe_lev_ncol = DIM_LEV_LEN * element_per_pe_ncol;

    int ioid_double_1D;
    int gdimlen_ncol[1];
    PIO_Offset compmap_ncol[element_per_pe_ncol];
    double double_buffer_ncol[element_per_pe_ncol];

    int ioid_double_2D;
    int gdimlen_lev_ncol[2];
    PIO_Offset compmap_lev_ncol[element_per_pe_lev_ncol];
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

    gdimlen_ncol[0] = ntasks * element_per_pe_ncol;
    gdimlen_lev_ncol[0] = DIM_LEV_LEN;
    gdimlen_lev_ncol[1] = gdimlen_ncol[0];

    for (int i = 0; i < element_per_pe_ncol; i++)
    {
        compmap_ncol[i] = my_rank * element_per_pe_ncol + i + 1;
        double_buffer_ncol[i] = my_rank;
    }

    for (int i = 0; i < element_per_pe_lev_ncol; i++)
    {
        compmap_lev_ncol[i] = my_rank * element_per_pe_lev_ncol + i + 1;
        double_buffer_lev_ncol[i] = my_rank;
    }

    niotasks = ntasks / ioproc_stride;

    for (int r = 0; r < 2; r++)
    {
        ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, rearranger[r], &iosysid); ERR

        /* Simulate history/restart files of E3SM F case (compset F2010, res ne4_oQU240) */
        snprintf(filename, PIO_MAX_NAME, "test_adios_f_case_rearr_%d.nc", rearranger[r]);

        if (my_rank == 0)
        {
            printf("Test writing %s with ADIOS IO type start\n", filename);
            fflush(stdout);
        }

        ncid = -1;
        ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER); ERR

        ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen_ncol, element_per_pe_ncol, compmap_ncol, &ioid_double_1D, NULL, NULL, NULL); ERR
        ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_lev_ncol, element_per_pe_lev_ncol, compmap_lev_ncol, &ioid_double_2D, NULL, NULL, NULL); ERR

        /* Put some global attributes */
        const int ne_val = 120;
        ret = PIOc_put_att(ncid, PIO_GLOBAL, "ne", PIO_INT, 1, &ne_val); ERR
        ret = PIOc_put_att_text(ncid, PIO_GLOBAL, "title", strlen("EAM History file information"), "EAM History file information"); ERR
        ret = PIOc_put_att_text(ncid, PIO_GLOBAL, "source", strlen("E3SM Atmosphere Model"), "E3SM Atmosphere Model"); ERR
        ret = PIOc_put_att_text(ncid, PIO_GLOBAL, "empty_string", strlen(""), ""); ERR

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
        ret = PIOc_put_att(ncid, varid_P0, "units", PIO_CHAR, strlen("pA"), "pA"); ERR /* Wrong attribute value */
        ret = PIOc_put_att(ncid, varid_P0, "units", PIO_CHAR, strlen("Pa"), "Pa"); ERR /* Correction: overwrite an existing attribute */
        const int mdims_val = 1;
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

        dimid = -1;
        ret = PIOc_inq_dimid(ncid, "lev", &dimid); ERR
        assert(dimid == dimid_lev);

        varid = -1;
        ret = PIOc_inq_varid(ncid, "P0", &varid); ERR
        assert(varid == varid_P0);

        att_type = PIO_NAT;
        att_len = 0;
        ret = PIOc_inq_att(ncid, PIO_GLOBAL, "ne", &att_type, &att_len); ERR
        assert(att_type == PIO_INT && att_len == 1);

        att_type = PIO_NAT;
        att_len = 0;
        ret = PIOc_inq_att(ncid, PIO_GLOBAL, "title", &att_type, &att_len); ERR
        assert(att_type == PIO_CHAR && att_len == strlen("EAM History file information"));

        att_type = PIO_NAT;
        att_len = 0;
        ret = PIOc_inq_att(ncid, varid_U, "mdims", &att_type, &att_len); ERR
        assert(att_type == PIO_INT && att_len == 1);

        att_id = -1;
        ret = PIOc_inq_attid(ncid, varid_U, "mdims", &att_id); ERR
        assert(att_id >= 0);

        memset(att_name, 0, sizeof(att_name));
        ret = PIOc_inq_attname(ncid, varid_U, att_id, att_name); ERR
        assert(strncmp(att_name, "mdims", 5) == 0);

        att_type = PIO_NAT;
        ret = PIOc_inq_atttype(ncid, varid_U, "mdims", &att_type); ERR
        assert(att_type == PIO_INT);

        att_len = 0;
        ret = PIOc_inq_attlen(ncid, varid_U, "mdims", &att_len); ERR
        assert(att_len == 1);

        att_type = PIO_NAT;
        att_len = 0;
        ret = PIOc_inq_att(ncid, varid_U, "units", &att_type, &att_len); ERR
        assert(att_type == PIO_CHAR && att_len == strlen("m/s"));

        ndims = -1;
        nvars = -1;
        ngatts = -1;
        unlimdimid = -1;
        ret = PIOc_inq(ncid, &ndims, &nvars, &ngatts, &unlimdimid); ERR
        assert(ndims == 7);
        assert(nvars == 11);
        assert(ngatts == 4);
        assert(unlimdimid == dimid_time);

        ndims = -1;
        ret = PIOc_inq_ndims(ncid, &ndims); ERR
        assert(ndims == 7);

        nvars = -1;
        ret = PIOc_inq_nvars(ncid, &nvars); ERR
        assert(nvars == 11);

        ngatts = -1;
        ret = PIOc_inq_natts(ncid, &ngatts); ERR
        assert(ngatts == 4);

        unlimdimid = -1;
        ret = PIOc_inq_unlimdim(ncid, &unlimdimid); ERR
        assert(unlimdimid == dimid_time);

        /* Put variable P0 */
        const double P0_val = 99999.9999;
        ret = PIOc_put_var_double(ncid, varid_P0, &P0_val); ERR

        /* Put variable lev */
        const double lev_vals[DIM_LEV_LEN] = {0.2, 0.4, 0.6};
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
        ret = PIOc_setframe(ncid, varid_U, 0); ERR
        ret = PIOc_write_darray(ncid, varid_U, ioid_double_2D, element_per_pe_lev_ncol, double_buffer_lev_ncol, NULL); ERR

        ret = PIOc_setframe(ncid, varid_U, 1); ERR
        ret = PIOc_write_darray(ncid, varid_U, ioid_double_2D, element_per_pe_lev_ncol, double_buffer_lev_ncol, NULL); ERR

        /* Write variable CLDHGH */
        ret = PIOc_setframe(ncid, varid_CLDHGH, 0); ERR
        ret = PIOc_write_darray(ncid, varid_CLDHGH, ioid_double_1D, element_per_pe_ncol, double_buffer_ncol, NULL); ERR

        ret = PIOc_setframe(ncid, varid_CLDHGH, 1); ERR
        ret = PIOc_write_darray(ncid, varid_CLDHGH, ioid_double_1D, element_per_pe_ncol, double_buffer_ncol, NULL); ERR

        ret = PIOc_closefile(ncid); ERR

        if (my_rank == 0)
        {
            printf("Test writing %s with ADIOS IO type end\n", filename);
            fflush(stdout);
        }

        if (my_rank == 0)
        {
            printf("Test reading %s with ADIOS IO type start\n", filename);
            fflush(stdout);
        }

        ncid = -1;
        ret = PIOc_openfile(iosysid, &ncid, &format, filename, PIO_NOWRITE); ERR

        ndims = -1;
        ret = PIOc_inq_ndims(ncid, &ndims); ERR
        assert(ndims == 7);

        nvars = -1;
        ret = PIOc_inq_nvars(ncid, &nvars); ERR
        assert(nvars == 11);

        int get_att_int = -1;
        ret = PIOc_get_att(ncid, PIO_GLOBAL, "ne", &get_att_int); ERR
        assert(get_att_int == ne_val);

        dimid_time = -1;
        ret = PIOc_inq_dimid(ncid, "time", &dimid_time); ERR
        assert(dimid_time >= 0 && dimid_time < ndims);

        varid_time = -1;
        ret = PIOc_inq_varid(ncid, "time", &varid_time); ERR
        assert(varid_time >= 0 && varid_time < nvars);

        varid_P0 = -1;
        ret = PIOc_inq_varid(ncid, "P0", &varid_P0); ERR
        assert(varid_P0 >= 0 && varid_P0 < nvars);

#if 0
        /* FIXME: ADIOS IO type does not overwrite an existing attribute so far */
        char get_att_text[PIO_MAX_NAME + 1] = "\0";
        ret = PIOc_get_att_text(ncid, varid_P0, "units", get_att_text); ERR
        assert(strncmp(get_att_text, "Pa", 2) == 0);
#endif

        varid_U = -1;
        ret = PIOc_inq_varid(ncid, "U", &varid_U); ERR
        assert(varid_U >= 0 && varid_U < nvars);

        att_id = -1;
        ret = PIOc_inq_attid(ncid, varid_U, "mdims", &att_id); ERR
        assert(att_id >= 0);

        memset(att_name, 0, sizeof(att_name));
        ret = PIOc_inq_attname(ncid, varid_U, att_id, att_name); ERR
        assert(strncmp(att_name, "mdims", 5) == 0);

        att_type = PIO_NAT;
        ret = PIOc_inq_atttype(ncid, varid_U, "mdims", &att_type); ERR
        assert(att_type == PIO_INT);

        att_len = 0;
        ret = PIOc_inq_attlen(ncid, varid_U, "mdims", &att_len); ERR
        assert(att_len == 1);

        dimid_lev = -1;
        ret = PIOc_inq_dimid(ncid, "lev", &dimid_lev); ERR
        assert(dimid_lev >= 0 && dimid_lev < ndims);

        PIO_Offset dimlen_lev = -1;
        ret = PIOc_inq_dimlen(ncid, dimid_lev, &dimlen_lev); ERR
        assert(dimlen_lev == DIM_LEV_LEN);

        varid_lev = -1;
        ret = PIOc_inq_varid(ncid, "lev", &varid_lev); ERR
        assert(varid_lev >= 0 && varid_lev < nvars);

        double get_var_double_array[DIM_LEV_LEN] = {0.0, 0.0, 0.0};
        ret = PIOc_get_var_double(ncid, varid_lev, get_var_double_array); ERR
        for (int i = 0; i < DIM_LEV_LEN; i++)
        {
            double diff = get_var_double_array[i] - lev_vals[i];
            assert(fabs(diff) < 1E-5);
        }

        double get_var_double = 0.0;
        ret = PIOc_get_var_double(ncid, varid_P0, &get_var_double); ERR
        assert(fabs(get_var_double - P0_val) < 1E-5);

        ret = PIOc_inq_varid(ncid, "CLDHGH", &varid_CLDHGH); ERR
        assert(varid_CLDHGH >= 0 && varid_CLDHGH < nvars);

        double read_darray_CLDHGH[element_per_pe_ncol];
        for (int i = 0; i < element_per_pe_ncol; i++)
            read_darray_CLDHGH[i] = 0.0;

        ret = PIOc_read_darray(ncid, varid_CLDHGH, ioid_double_1D, element_per_pe_ncol, read_darray_CLDHGH); ERR
        for (int i = 0; i < element_per_pe_ncol; i++)
        {
            double diff = read_darray_CLDHGH[i] - double_buffer_ncol[i];
            assert(fabs(diff) < 1E-5);
        }

        ret = PIOc_inq_varid(ncid, "U", &varid_U); ERR
        assert(varid_U >= 0 && varid_U < nvars);

        double read_darray_U[element_per_pe_lev_ncol];
        for (int i = 0; i < element_per_pe_lev_ncol; i++)
            read_darray_U[i] = 0.0;

        ret = PIOc_read_darray(ncid, varid_U, ioid_double_2D, element_per_pe_lev_ncol, read_darray_U); ERR
        for (int i = 0; i < element_per_pe_lev_ncol; i++)
        {
            double diff = read_darray_U[i] - double_buffer_lev_ncol[i];
            assert(fabs(diff) < 1E-5);
        }

        ret = PIOc_closefile(ncid); ERR

        if (my_rank == 0)
        {
            printf("Test reading %s with ADIOS IO type end\n", filename);
            fflush(stdout);
        }

        ret = PIOc_freedecomp(iosysid, ioid_double_1D); ERR
        ret = PIOc_freedecomp(iosysid, ioid_double_2D); ERR

        /* Simulate history/restart files of E3SM I case (compset I1850GSWCNPRDCTCBC, res f19_g16) */
        snprintf(filename, PIO_MAX_NAME, "test_adios_i_case_rearr_%d.nc", rearranger[r]);

        if (my_rank == 0)
        {
            printf("Test writing %s with ADIOS IO type start\n", filename);
            fflush(stdout);
        }

        ncid = -1;
        ret = PIOc_createfile(iosysid, &ncid, &format, filename, PIO_CLOBBER); ERR

        /* Put some global attributes */
        ret = PIOc_put_att_text(ncid, PIO_GLOBAL, "title", strlen("ELM History file information"), "ELM History file information"); ERR
        ret = PIOc_put_att_text(ncid, PIO_GLOBAL, "source", strlen("E3SM Land Model"), "E3SM Land Model"); ERR
        const int vegetated_or_bare_soil_val = 1;
        ret = PIOc_put_att(ncid, PIO_GLOBAL, "ltype_vegetated_or_bare_soil", PIO_INT, 1, &vegetated_or_bare_soil_val); ERR
        const int crop_val = 2;
        ret = PIOc_put_att(ncid, PIO_GLOBAL, "ltype_crop", PIO_INT, 1, &crop_val); ERR

        ret = PIOc_def_dim(ncid, "time", PIO_UNLIMITED, &dimid_time); ERR
        ret = PIOc_def_dim(ncid, "lon", 144, &dimid_lon); ERR
        ret = PIOc_def_dim(ncid, "lat", 96, &dimid_lat); ERR

        ret = PIOc_def_var(ncid, "time", PIO_FLOAT, 1, &dimid_time, &varid_time); ERR
        ret = PIOc_def_var(ncid, "lon", PIO_FLOAT, 1, &dimid_lon, &varid_lon); ERR
        ret = PIOc_def_var(ncid, "lat", PIO_FLOAT, 1, &dimid_lat, &varid_lat); ERR

        /* Put some local attributes */
        const float att_fill_Value = 1E36;
        const float att_missing_value = 1E36;

        ret = PIOc_put_att(ncid, varid_time, "long_name", PIO_CHAR, strlen("time"), "time"); ERR
        ret = PIOc_put_att(ncid, varid_time, "units", PIO_CHAR, strlen("days since 0001-01-01 00:00:00"), "days since 0001-01-01 00:00:00"); ERR
        ret = PIOc_put_att(ncid, varid_time, "calendar", PIO_CHAR, strlen("noleap"), "noleap"); ERR
        ret = PIOc_put_att(ncid, varid_time, "bounds", PIO_CHAR, strlen("time_bounds"), "time_bounds"); ERR

        ret = PIOc_put_att(ncid, varid_lon, "long_name", PIO_CHAR, strlen("coordinate longitude"), "coordinate longitude"); ERR
        ret = PIOc_put_att(ncid, varid_lon, "units", PIO_CHAR, strlen("degrees_east"), "degrees_east"); ERR
        ret = PIOc_put_att(ncid, varid_lon, "_FillValue", PIO_FLOAT, 1, &att_fill_Value); ERR
        ret = PIOc_put_att(ncid, varid_lon, "missing_value", PIO_FLOAT, 1, &att_missing_value); ERR

        ret = PIOc_put_att(ncid, varid_lat, "long_name", PIO_CHAR, strlen("coordinate latitude"), "coordinate latitude"); ERR
        ret = PIOc_put_att(ncid, varid_lat, "units", PIO_CHAR, strlen("degrees_north"), "degrees_north"); ERR
        ret = PIOc_put_att(ncid, varid_lat, "_FillValue", PIO_FLOAT, 1, &att_fill_Value); ERR
        ret = PIOc_put_att(ncid, varid_lat, "missing_value", PIO_FLOAT, 1, &att_missing_value); ERR

        dimids[0] = dimid_time;
        dimids[1] = dimid_lat;
        dimids[2] = dimid_lon;
        for (int i = 0; i < MAX_LAT_LON_VARS; i++)
        {
            snprintf(varname, PIO_MAX_NAME, "var_lat_lon_%03d", i);
            ret = PIOc_def_var(ncid, varname, PIO_FLOAT, 3, dimids, &varid); ERR
        }

        ret = PIOc_enddef(ncid); ERR

        ret = PIOc_closefile(ncid); ERR

        if (my_rank == 0)
        {
            printf("Test writing %s with ADIOS IO type end\n", filename);
            fflush(stdout);
        }

        if (my_rank == 0)
        {
            printf("Test reading %s with ADIOS IO type start\n", filename);
            fflush(stdout);
        }

        ncid = -1;
        ret = PIOc_openfile(iosysid, &ncid, &format, filename, PIO_NOWRITE); ERR

        ndims = -1;
        ret = PIOc_inq_ndims(ncid, &ndims); ERR
        assert(ndims == 3);

        dimid_time = -1;
        ret = PIOc_inq_dimid(ncid, "time", &dimid_time); ERR
        assert(dimid_time >= 0 && dimid_time < ndims);

        nvars = -1;
        ret = PIOc_inq_nvars(ncid, &nvars); ERR
        assert(nvars == (3 + MAX_LAT_LON_VARS));

        ret = PIOc_closefile(ncid); ERR

        if (my_rank == 0)
        {
            printf("Test reading %s with ADIOS IO type end\n", filename);
            fflush(stdout);
        }

        ret = PIOc_finalize(iosysid); ERR
    }

    ret = test_adios_read_case_1(); ERR

    ret = test_adios_read_case_2(); ERR

    ret = test_adios_read_case_3(); ERR

    ret = test_adios_cdf5_new_data_types(); ERR

    MPI_Finalize();

#ifdef TIMING
    GPTLfinalize();
#endif

#else
    printf("SCORPIO is not configured with ADIOS support.\n");

    /* Make the test fail if ADIOS support is not enabled/available. */
    return -1;
#endif

    return 0;
}

#ifdef _ADIOS2
/* Number of elements of the local darray data that
   will be handled by each processor. */
#define ELEMENTS_PER_PE 8

/* Length of variables to be put/get */
#define PUT_GET_VAR_LEN 10
#define PUT_GET_VAR_LEN_X 10
#define PUT_GET_VAR_LEN_Y 5
#define PUT_GET_VAR_LEN_Z 2

/* Length of attributes to be put/get */
#define PUT_GET_ATT_LEN 5

/* Max time steps */
#define MAX_TIME_STEPS 10

int test_adios_read_case_1()
{
    /* Zero-based rank of processor. */
    int my_rank;

    /* Number of processors involved in current execution. */
    int ntasks;

    int format = PIO_IOTYPE_ADIOS;

    /* Number of processors that will do IO. */
    int niotasks;

    /* Zero based rank of first processor to be used for I/O. */
    const int ioproc_start = 0;

    /* The dimension IDs. */
    int dimid_text_var_len;
    int dimid_darray_var_len;
    int dimid_put_get_var_len;
    int dimids_put_get_var_len_3D[3];
    int dimids_time_var[2];

    int dimid_darray_var_len_inq;
    PIO_Offset dimlen_darray_var_len_inq;

    int dimids_put_get_var_len_3D_inq[3];
    PIO_Offset dimlens_put_get_var_len_3D_inq[3];

    /* This simple test uses one-dimensional data. */
    const int NDIMS = 1;

    /* Lengths of the global dimensions */
    int gdimlen[NDIMS];

    int iosysid;

    /* The ncid of the netCDF file created and read in this example. */
    int ncid_write;
    int ncid_read;

    /* The IDs of the netCDF variables in the example file. */
    int varid_dummy_scalar_var_int;
    int varid_dummy_scalar_var_float;
    int varid_dummy_text_var;
    int varid_dummy_darray_var_int;
    int varid_dummy_darray_var_float;
    int varid_dummy_darray_var_double;
    int varid_dummy_time_var_int;
    int varid_dummy_put_get_var_int;
    int varid_dummy_put_get_var_int_2D;
    int varid_dummy_put_get_var_int_3D;
    int varid_dummy_put_get_var_float;
    int varid_dummy_put_get_var_double;

    /* start/count arrays for get/put var */
    PIO_Offset start[NDIMS];
    PIO_Offset count[NDIMS];
    PIO_Offset start2D[2];
    PIO_Offset count2D[2];

    /* The I/O description IDs as passed back by PIOc_InitDecomp()
       and freed in PIOc_freedecomp(). */
    int ioid_int;
    int ioid_double;

    /* Sample data for global attributes. */
    int put_global_att_int_data = -10;
    int get_global_att_int_data = 0;
    int put_global_att_arr_int_data[PUT_GET_ATT_LEN] = {-2, -4, -6, -8, -10};
    int get_global_att_arr_int_data[PUT_GET_ATT_LEN] = {0, 0, 0, 0, 0};
    float put_global_att_float_data = -9.9;
    float get_global_att_float_data = 0.0;
    float put_global_att_arr_float_data[PUT_GET_ATT_LEN] = {-1.1, -3.3, -5.5, -7.7, -9.9};
    float get_global_att_arr_float_data[PUT_GET_ATT_LEN] = {0.0, 0.0, 0.0, 0.0, 0.0};
    char put_global_att_text_data[PIO_MAX_NAME] = "Dummy global attribute string";
    char get_global_att_text_data[PIO_MAX_NAME] = "\0";

    /* Sample data for variable attributes. */
    int put_att_int_data = -100;
    int get_att_int_data = 0;
    int put_att_arr_int_data[PUT_GET_ATT_LEN] = {-20, -40, -60, -80, -100};
    int get_att_arr_int_data[PUT_GET_ATT_LEN] = {0, 0, 0, 0, 0};
    float put_att_float_data = -99.99;
    float get_att_float_data = 0.0;
    float put_att_arr_float_data[PUT_GET_ATT_LEN] = {-11.11, -33.33, -55.55, -77.77, -99.99};
    float get_att_arr_float_data[PUT_GET_ATT_LEN] = {0.0, 0.0, 0.0, 0.0, 0.0};
    char put_att_text_data[PIO_MAX_NAME] = "Dummy variable attribute string";
    char get_att_text_data[PIO_MAX_NAME] = "\0";

    /* Sample data for scalar variables. */
    int put_scalar_int_data = -1000;
    int get_scalar_int_data = 0;
    float put_scalar_float_data = -999.999;
    float get_scalar_float_data = 0.0;

    /* Sample string for text variable. */
    char put_text_data[PIO_MAX_NAME] = "Dummy text variable string";
    char get_text_data[PIO_MAX_NAME] = "\0";

    /* Buffers for sample write/read darray data. */
    int write_darray_buffer_int[ELEMENTS_PER_PE];
    int read_darray_buffer_int[ELEMENTS_PER_PE];
    double write_darray_buffer_double[ELEMENTS_PER_PE];
    double read_darray_buffer_double[ELEMENTS_PER_PE];

    /* Buffers for sample write/read variables with time steps */
    int write_time_var_buffer_int[MAX_TIME_STEPS][ELEMENTS_PER_PE];
    int read_time_var_buffer_int[MAX_TIME_STEPS][ELEMENTS_PER_PE];

    /* Buffers for sample put/get var data. */
    int put_var_buffer_int[PUT_GET_VAR_LEN];
    int get_var_buffer_int[PUT_GET_VAR_LEN];
    int put_var_buffer_int_2D[PUT_GET_VAR_LEN_X][PUT_GET_VAR_LEN_Y];
    int get_var_buffer_int_2D[PUT_GET_VAR_LEN_X][PUT_GET_VAR_LEN_Y];
    int put_var_buffer_int_3D[PUT_GET_VAR_LEN_X][PUT_GET_VAR_LEN_Y][PUT_GET_VAR_LEN_Z];
    int get_var_buffer_int_3D[PUT_GET_VAR_LEN_X][PUT_GET_VAR_LEN_Y][PUT_GET_VAR_LEN_Z];
    double put_var_buffer_double[PUT_GET_VAR_LEN];
    double get_var_buffer_double[PUT_GET_VAR_LEN];

    /* A 1-D array which holds the decomposition mapping for this example. */
    PIO_Offset *compmap;

    float diff_float;
    double diff_double;

    int ret = PIO_NOERR;

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    /* Set lengths of the global dimensions (1D in this example). */
    gdimlen[0] = ELEMENTS_PER_PE * ntasks;

    /* Stride in the MPI rank between IO tasks.  */
    int ioproc_stride = 2;
    if (ntasks == 1)
        ioproc_stride = 1;

    niotasks = ntasks / ioproc_stride;

    /* Initialize the PIO IO system. This specifies how
       many and which processors are involved in I/O. */
    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_SUBSET, &iosysid); ERR

    /* Describe the decomposition. This is a 1-based array. */
    compmap = malloc(ELEMENTS_PER_PE * sizeof(PIO_Offset));
    for (int i = 0; i < ELEMENTS_PER_PE; i++)
        compmap[i] = my_rank * ELEMENTS_PER_PE + i + 1;

    /* Create the decomposition for this example. */
    ret = PIOc_InitDecomp(iosysid, PIO_INT, NDIMS, gdimlen, ELEMENTS_PER_PE, compmap, &ioid_int, NULL, NULL, NULL); ERR
    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, NDIMS, gdimlen, ELEMENTS_PER_PE, compmap, &ioid_double, NULL, NULL, NULL); ERR
    free(compmap);

    /* Prepare sample data for write buffers and initialize read buffers. */
    for (int i = 0; i < ELEMENTS_PER_PE; i++)
    {
        write_darray_buffer_int[i] = my_rank + 1;
        write_darray_buffer_double[i] = my_rank * 0.1 + 1.0;
        read_darray_buffer_int[i] = 0;
        read_darray_buffer_double[i] = 0.0;
    }

    for (int t = 0; t < MAX_TIME_STEPS; t++)
    {
        for (int i = 0; i < ELEMENTS_PER_PE; i++)
        {
            write_time_var_buffer_int[t][i] = my_rank + (t + 1) * 1000;
            read_time_var_buffer_int[t][i] = 0;
        }
    }

    for (int i = 0; i < PUT_GET_VAR_LEN; i++)
    {
        put_var_buffer_int[i] = i + 1;
        put_var_buffer_double[i] = (i + 1) * 0.1;
        get_var_buffer_int[i] = 0;
        get_var_buffer_double[i] = 0.0;
    }

    for (int i = 0; i < PUT_GET_VAR_LEN_X; i++)
    {
        for (int j = 0; j < PUT_GET_VAR_LEN_Y; j++)
        {
            put_var_buffer_int_2D[i][j] = i + 1 + j * 1000;
            get_var_buffer_int_2D[i][j] = 0;
        }
    }

    for (int i = 0; i < PUT_GET_VAR_LEN_X; i++)
    {
        for (int j = 0; j < PUT_GET_VAR_LEN_Y; j++)
        {
            for (int k = 0; k < PUT_GET_VAR_LEN_Z; k++)
            {
                if ((i + j + k) % 2 == 0)
                    put_var_buffer_int_3D[i][j][k] = 1;
                else
                    put_var_buffer_int_3D[i][j][k] = -1;

                get_var_buffer_int_3D[i][j][k] = 0;
            }
        }
    }

    ret = PIOc_createfile(iosysid, &ncid_write, &format, "test_adios_read_case_1.nc", PIO_CLOBBER); ERR

    /* Put some global attributes. */
    ret = PIOc_put_att(ncid_write, PIO_GLOBAL, "dummy_global_att_int", PIO_INT, 1, &put_global_att_int_data); ERR
    ret = PIOc_put_att(ncid_write, PIO_GLOBAL, "dummy_global_att_arr_int", PIO_INT, PUT_GET_ATT_LEN, put_global_att_arr_int_data); ERR
    ret = PIOc_put_att(ncid_write, PIO_GLOBAL, "dummy_global_att_float", PIO_FLOAT, 1, &put_global_att_float_data); ERR
    ret = PIOc_put_att(ncid_write, PIO_GLOBAL, "dummy_global_att_arr_float", PIO_FLOAT, PUT_GET_ATT_LEN, put_global_att_arr_float_data); ERR
    ret = PIOc_put_att_text(ncid_write, PIO_GLOBAL, "dummy_global_att_text", strlen(put_global_att_text_data), put_global_att_text_data); ERR

    /* Define some scalar variables. */
    ret = PIOc_def_var(ncid_write, "dummy_scalar_var_int", PIO_INT, 0, NULL, &varid_dummy_scalar_var_int); ERR
    ret = PIOc_def_var(ncid_write, "dummy_scalar_var_float", PIO_FLOAT, 0, NULL, &varid_dummy_scalar_var_float); ERR

    /* Define a text variable. */
    ret = PIOc_def_dim(ncid_write, "text_var_len", PIO_MAX_NAME, &dimid_text_var_len); ERR
    ret = PIOc_def_var(ncid_write, "dummy_text_var", PIO_CHAR, 1, &dimid_text_var_len, &varid_dummy_text_var); ERR

    /* Define some variables for PIOc_write_darray. */
    ret = PIOc_def_dim(ncid_write, "darray_var_len", (PIO_Offset)gdimlen[0], &dimid_darray_var_len); ERR
    ret = PIOc_def_var(ncid_write, "dummy_darray_var_int", PIO_INT, NDIMS, &dimid_darray_var_len, &varid_dummy_darray_var_int); ERR
    ret = PIOc_def_var(ncid_write, "dummy_darray_var_float", PIO_FLOAT, NDIMS, &dimid_darray_var_len, &varid_dummy_darray_var_float); ERR
    ret = PIOc_def_var(ncid_write, "dummy_darray_var_double", PIO_DOUBLE, NDIMS, &dimid_darray_var_len, &varid_dummy_darray_var_double); ERR

    /* Put some local attributes for variable dummy_darray_var_int. */
    ret = PIOc_put_att(ncid_write, varid_dummy_darray_var_int, "dummy_att_float", PIO_FLOAT, 1, &put_att_float_data); ERR
    ret = PIOc_put_att(ncid_write, varid_dummy_darray_var_int, "dummy_att_arr_float", PIO_FLOAT, PUT_GET_ATT_LEN, put_att_arr_float_data); ERR
    ret = PIOc_put_att(ncid_write, varid_dummy_darray_var_int, "dummy_att_int", PIO_INT, 1, &put_att_int_data); ERR
    ret = PIOc_put_att(ncid_write, varid_dummy_darray_var_int, "dummy_att_arr_int", PIO_INT, PUT_GET_ATT_LEN, put_att_arr_int_data); ERR
    ret = PIOc_put_att_text(ncid_write, varid_dummy_darray_var_int, "dummy_att_text", strlen(put_att_text_data), put_att_text_data); ERR

    /* Define some variables for PIOc_put_vars. */
    ret = PIOc_def_dim(ncid_write, "put_get_var_len", PUT_GET_VAR_LEN, &dimid_put_get_var_len); ERR
    ret = PIOc_def_var(ncid_write, "dummy_put_get_var_int", PIO_INT, NDIMS, &dimid_put_get_var_len, &varid_dummy_put_get_var_int); ERR

    ret = PIOc_def_dim(ncid_write, "put_get_var_len_x", PUT_GET_VAR_LEN_X, &dimids_put_get_var_len_3D[0]); ERR
    ret = PIOc_def_dim(ncid_write, "put_get_var_len_y", PUT_GET_VAR_LEN_Y, &dimids_put_get_var_len_3D[1]); ERR
    ret = PIOc_def_dim(ncid_write, "put_get_var_len_z", PUT_GET_VAR_LEN_Z, &dimids_put_get_var_len_3D[2]); ERR

    ret = PIOc_def_var(ncid_write, "dummy_put_get_var_int_2D", PIO_INT, 2, dimids_put_get_var_len_3D, &varid_dummy_put_get_var_int_2D); ERR
    ret = PIOc_def_var(ncid_write, "dummy_put_get_var_int_3D", PIO_INT, 3, dimids_put_get_var_len_3D, &varid_dummy_put_get_var_int_3D); ERR

    ret = PIOc_def_var(ncid_write, "dummy_put_get_var_float", PIO_FLOAT, NDIMS, &dimid_put_get_var_len, &varid_dummy_put_get_var_float); ERR
    ret = PIOc_def_var(ncid_write, "dummy_put_get_var_double", PIO_DOUBLE, NDIMS, &dimid_put_get_var_len, &varid_dummy_put_get_var_double); ERR

    /* Define an int variable with time steps. */
    ret = PIOc_def_dim(ncid_write, "time", NC_UNLIMITED, &dimids_time_var[0]); ERR
    ret = PIOc_def_dim(ncid_write, "time_var_len", (PIO_Offset)gdimlen[0], &dimids_time_var[1]); ERR
    ret = PIOc_def_var(ncid_write, "dummy_time_var_int", PIO_INT, NDIMS + 1, dimids_time_var, &varid_dummy_time_var_int); ERR

    ret = PIOc_enddef(ncid_write); ERR

    /* Put some scalar variables. */
    ret = PIOc_put_var_int(ncid_write, varid_dummy_scalar_var_int, &put_scalar_int_data); ERR
    ret = PIOc_put_var_float(ncid_write, varid_dummy_scalar_var_float, &put_scalar_float_data); ERR

    /* Put a text variable. */
    ret = PIOc_put_var_text(ncid_write, varid_dummy_text_var, put_text_data); ERR

    /* Put int type data to an int type variable, type conversions will not be performed. */
    /* Put 1st half data first */
    start[0] = 0;
    count[0] = PUT_GET_VAR_LEN / 2;
    ret = PIOc_put_vars_int(ncid_write, varid_dummy_put_get_var_int, start, count, NULL, put_var_buffer_int); ERR
    /* Put 2nd half data */
    start[0] = PUT_GET_VAR_LEN / 2;
    count[0] = PUT_GET_VAR_LEN / 2;
    ret = PIOc_put_vars_int(ncid_write, varid_dummy_put_get_var_int, start, count, NULL, put_var_buffer_int + (PUT_GET_VAR_LEN / 2)); ERR

    /* Put double type data to a float type variable, type conversions will be performed. */
    /* Put 2nd half data first */
    start[0] = PUT_GET_VAR_LEN / 2;
    count[0] = PUT_GET_VAR_LEN / 2;
    ret = PIOc_put_vars_double(ncid_write, varid_dummy_put_get_var_float, start, count, NULL, put_var_buffer_double + (PUT_GET_VAR_LEN / 2)); ERR
    ret = PIOc_put_vars_double(ncid_write, varid_dummy_put_get_var_double, start, count, NULL, put_var_buffer_double + (PUT_GET_VAR_LEN / 2)); ERR
    /* Put 1st half data */
    start[0] = 0;
    count[0] = PUT_GET_VAR_LEN / 2;
    ret = PIOc_put_vars_double(ncid_write, varid_dummy_put_get_var_float, start, count, NULL, put_var_buffer_double); ERR
    ret = PIOc_put_vars_double(ncid_write, varid_dummy_put_get_var_double, start, count, NULL, put_var_buffer_double); ERR
#if 0
    /* 2D in array */
      start2D[0] = 0;
      count2D[0] = PUT_GET_VAR_LEN_X;
      start2D[1] = 0;
      count2D[1] = PUT_GET_VAR_LEN_Y;
      ret = PIOc_put_vars_int(ncid_write, varid_dummy_put_get_var_int_2D, start, count, NULL,
                              (const int *) put_var_buffer_int_2D); ERR
#endif

    ret = PIOc_put_var_int(ncid_write, varid_dummy_put_get_var_int_2D, (const int *) put_var_buffer_int_2D); ERR
    ret = PIOc_put_var_int(ncid_write, varid_dummy_put_get_var_int_3D, (const int *) put_var_buffer_int_3D); ERR

    /* Write to int type variable with int type decomposition, type conversions will not be performed. */
    ret = PIOc_write_darray(ncid_write, varid_dummy_darray_var_int, ioid_int, ELEMENTS_PER_PE, write_darray_buffer_int, NULL); ERR

    /* Write to float type variable with double type decomposition, type conversions will be performed. */
    ret = PIOc_write_darray(ncid_write, varid_dummy_darray_var_float, ioid_double, ELEMENTS_PER_PE, write_darray_buffer_double, NULL); ERR

    /* Write to double type variable with double type decomposition, type conversions will not be performed. */
    ret = PIOc_write_darray(ncid_write, varid_dummy_darray_var_double, ioid_double, ELEMENTS_PER_PE, write_darray_buffer_double, NULL); ERR

    /* Write to int variable with time steps, frame 0 */
    PIOc_setframe(ncid_write, varid_dummy_time_var_int, 0);
    ret = PIOc_write_darray(ncid_write, varid_dummy_time_var_int, ioid_int, ELEMENTS_PER_PE, write_time_var_buffer_int[0], NULL); ERR

    /* Write to int variable with time steps, frame 1 */
    PIOc_setframe(ncid_write, varid_dummy_time_var_int, 1);
    ret = PIOc_write_darray(ncid_write, varid_dummy_time_var_int, ioid_int, ELEMENTS_PER_PE, write_time_var_buffer_int[1], NULL); ERR

    /* Write to variable with time steps, frame 2 */
    PIOc_setframe(ncid_write, varid_dummy_time_var_int, 2);
    ret = PIOc_write_darray(ncid_write, varid_dummy_time_var_int, ioid_int, ELEMENTS_PER_PE, write_time_var_buffer_int[2], NULL); ERR

    /* Write to variable with time steps, frame 3 */
    PIOc_setframe(ncid_write, varid_dummy_time_var_int, 3);
    ret = PIOc_write_darray(ncid_write, varid_dummy_time_var_int, ioid_int, ELEMENTS_PER_PE, write_time_var_buffer_int[3], NULL); ERR

    ret = PIOc_closefile(ncid_write); ERR

    ret = PIOc_openfile(iosysid, &ncid_read, &format, "test_adios_read_case_1.nc", PIO_NOWRITE); ERR

    int total_dims = -1;
    ret = PIOc_inq_ndims(ncid_read, &total_dims); ERR

    dimid_darray_var_len_inq = -1;
    ret = PIOc_inq_dimid(ncid_read, "darray_var_len", &dimid_darray_var_len_inq); ERR
    if (dimid_darray_var_len_inq < 0 || dimid_darray_var_len_inq > total_dims)
    {
        printf("rank = %d, read wrong ID for dimension darray_var_len\n", my_rank);
        return -1;
    }

    dimlen_darray_var_len_inq = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimid_darray_var_len_inq, &dimlen_darray_var_len_inq); ERR
    if (dimlen_darray_var_len_inq != (PIO_Offset) gdimlen[0])
    {
        printf("rank = %d, read wrong length for dimension darray_var_len\n", my_rank);
        return -1;
    }

    /* Get int type global attribute. */
    ret = PIOc_get_att(ncid_read, PIO_GLOBAL, "dummy_global_att_int", &get_global_att_int_data); ERR
    if (get_global_att_int_data != put_global_att_int_data)
    {
        printf("rank = %d, read wrong data for dummy_global_att_int\n", my_rank);
        return -1;
    }

    ret = PIOc_get_att(ncid_read, PIO_GLOBAL, "dummy_global_att_arr_int", get_global_att_arr_int_data); ERR
    for (int i = 0; i < PUT_GET_ATT_LEN; i++)
    {
        if (get_global_att_arr_int_data[i] != put_global_att_arr_int_data[i])
        {
            printf("rank = %d, read wrong data for dummy_global_att_arr_int at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get float type global attribute. */
    ret = PIOc_get_att(ncid_read, PIO_GLOBAL, "dummy_global_att_float", &get_global_att_float_data); ERR
    diff_float = get_global_att_float_data - put_global_att_float_data;
    if (fabs(diff_float) > 1E-5)
    {
        printf("rank = %d, read wrong data for dummy_global_att_float\n", my_rank);
        return -1;
    }

    ret = PIOc_get_att(ncid_read, PIO_GLOBAL, "dummy_global_att_arr_float", get_global_att_arr_float_data); ERR
    for (int i = 0; i < PUT_GET_ATT_LEN; i++)
    {
        diff_float = get_global_att_arr_float_data[i] - put_global_att_arr_float_data[i];
        if (fabs(diff_float) > 1E-5)
        {
            printf("rank = %d, read wrong data for dummy_global_att_arr_float at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get text type global attribute. */
    ret = PIOc_get_att_text(ncid_read, PIO_GLOBAL, "dummy_global_att_text", get_global_att_text_data); ERR
    if (strncmp(get_global_att_text_data, put_global_att_text_data, strlen(put_global_att_text_data)))
    {
        printf("rank = %d, read wrong data for dummy_global_att_text\n", my_rank);
        return -1;
    }

    /* Get int type scalar variable. */
    ret = PIOc_inq_varid(ncid_read, "dummy_scalar_var_int", &varid_dummy_scalar_var_int); ERR
    ret = PIOc_get_var_int(ncid_read, varid_dummy_scalar_var_int, &get_scalar_int_data); ERR
    if (get_scalar_int_data != put_scalar_int_data)
    {
        printf("rank = %d, read wrong data for dummy_scalar_var_int\n", my_rank);
        return -1;
    }

    /* Get float type scalar variable. */
    ret = PIOc_inq_varid(ncid_read, "dummy_scalar_var_float", &varid_dummy_scalar_var_float); ERR
    ret = PIOc_get_var_float(ncid_read, varid_dummy_scalar_var_float, &get_scalar_float_data); ERR
    diff_float = get_scalar_float_data - put_scalar_float_data;
    if (fabs(diff_float) > 1E-5)
    {
        printf("rank = %d, read wrong data for dummy_scalar_var_float\n", my_rank);
        return -1;
    }

    /* Get text type variable. */
    ret = PIOc_inq_varid(ncid_read, "dummy_text_var", &varid_dummy_text_var); ERR
    ret = PIOc_get_var_text(ncid_read, varid_dummy_text_var, get_text_data); ERR
    if (strncmp(get_text_data, put_text_data, strlen(put_text_data)))
    {
        printf("rank = %d, read wrong data for dummy_text_var\n", my_rank);
        return -1;
    }

    /* Read int type variable with int type decomposition, type conversions will not be performed. */
    ret = PIOc_inq_varid(ncid_read, "dummy_darray_var_int", &varid_dummy_darray_var_int); ERR
    ret = PIOc_read_darray(ncid_read, varid_dummy_darray_var_int, ioid_int, ELEMENTS_PER_PE, read_darray_buffer_int); ERR
    for (int i = 0; i < ELEMENTS_PER_PE; i++)
    {
        if (read_darray_buffer_int[i] != write_darray_buffer_int[i])
        {
            printf("rank = %d, read wrong data for dummy_darray_var_int at index %d\n", my_rank, i);
            return -1;
        }
    }

    dimid_darray_var_len_inq = -1;
    ret = PIOc_inq_vardimid(ncid_read, varid_dummy_darray_var_int, &dimid_darray_var_len_inq); ERR

    dimlen_darray_var_len_inq = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimid_darray_var_len_inq, &dimlen_darray_var_len_inq); ERR
    if (dimlen_darray_var_len_inq != (PIO_Offset) gdimlen[0])
    {
        printf("rank = %d, read wrong length for dimension darray_var_len\n", my_rank);
        return -1;
    }

    /* Get varid of the int variable with time steps. */
    ret = PIOc_inq_varid(ncid_read, "dummy_time_var_int", &varid_dummy_time_var_int); ERR

    /* Read from int variable with time steps, frame 2 */
    PIOc_setframe(ncid_read, varid_dummy_time_var_int, 2);
    ret = PIOc_read_darray(ncid_read, varid_dummy_time_var_int, ioid_int, ELEMENTS_PER_PE, read_time_var_buffer_int[2]); ERR

    /* Read from int variable with time steps, frame 1 */
    PIOc_setframe(ncid_read, varid_dummy_time_var_int, 1);
    ret = PIOc_read_darray(ncid_read, varid_dummy_time_var_int, ioid_int, ELEMENTS_PER_PE, read_time_var_buffer_int[1]); ERR

    /* Read from int variable with time steps, frame 0 */
    PIOc_setframe(ncid_read, varid_dummy_time_var_int, 0);
    ret = PIOc_read_darray(ncid_read, varid_dummy_time_var_int, ioid_int, ELEMENTS_PER_PE, read_time_var_buffer_int[0]); ERR

    /* Check data read from 3 time steps. */
    for (int t = 0; t < 3; t++)
    {
        for (int i = 0; i < ELEMENTS_PER_PE; i++)
        {
            if (read_time_var_buffer_int[t][i] != write_time_var_buffer_int[t][i])
            {
                printf("rank = %d, read wrong data for dummy_darray_var_int at time step %d and index %d\n", my_rank, t, i);
                return -1;
            }
        }
    }

    /* Get int type attribute of variable dummy_darray_var_int. */
    ret = PIOc_get_att(ncid_read, varid_dummy_darray_var_int, "dummy_att_int", &get_att_int_data); ERR
    if (get_att_int_data != put_att_int_data)
    {
        printf("rank = %d, read wrong data for dummy_att_int of dummy_darray_var_int\n", my_rank);
        return -1;
    }

    ret = PIOc_get_att(ncid_read, varid_dummy_darray_var_int, "dummy_att_arr_int", get_att_arr_int_data); ERR
    for (int i = 0; i < PUT_GET_ATT_LEN; i++)
    {
        if (get_att_arr_int_data[i] != put_att_arr_int_data[i])
        {
            printf("rank = %d, read wrong data for dummy_att_arr_int at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get float type attribute of variable dummy_darray_var_int. */
    ret = PIOc_get_att(ncid_read, varid_dummy_darray_var_int, "dummy_att_float", &get_att_float_data); ERR
    diff_float = get_att_float_data - put_att_float_data;
    if (fabs(diff_float) > 1E-5)
    {
        printf("rank = %d, read wrong data for dummy_att_float of dummy_darray_var_int\n", my_rank);
        return -1;
    }

    ret = PIOc_get_att(ncid_read, varid_dummy_darray_var_int, "dummy_att_arr_float", get_att_arr_float_data); ERR
    for (int i = 0; i < PUT_GET_ATT_LEN; i++)
    {
        diff_float = get_att_arr_float_data[i] - put_att_arr_float_data[i];
        if (fabs(diff_float) > 1E-5)
        {
            printf("rank = %d, read wrong data for dummy_att_arr_float at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get text type attribute of variable dummy_darray_var_int. */
    ret = PIOc_get_att_text(ncid_read, varid_dummy_darray_var_int, "dummy_att_text", get_att_text_data); ERR
    if (strncmp(get_att_text_data, put_att_text_data, strlen(put_att_text_data)))
    {
        printf("rank = %d, read wrong data for dummy_att_text of dummy_darray_var_int\n", my_rank);
        return -1;
    }

    /* Read float type variable with double type decomposition, type conversions will be performed. */
    ret = PIOc_inq_varid(ncid_read, "dummy_darray_var_float", &varid_dummy_darray_var_float); ERR
    for (int i = 0; i < ELEMENTS_PER_PE; i++)
        read_darray_buffer_double[i] = 0.0;

    ret = PIOc_read_darray(ncid_read, varid_dummy_darray_var_float, ioid_double, ELEMENTS_PER_PE, read_darray_buffer_double); ERR
    for (int i = 0; i < ELEMENTS_PER_PE; i++)
    {
        diff_double = read_darray_buffer_double[i] - write_darray_buffer_double[i];
        if (fabs(diff_double) > 1E-5)
        {
            printf("rank = %d, read wrong data for dummy_darray_var_float at index %d expected %f got %f \n", my_rank, i, write_darray_buffer_double[i], read_darray_buffer_double[i]);
            return -1;
        }
    }

    /* Read double type variable with double type decomposition, type conversions will not be performed. */
    for (int i = 0; i < ELEMENTS_PER_PE; i++)
        read_darray_buffer_double[i] = 0.0;

    ret = PIOc_inq_varid(ncid_read, "dummy_darray_var_double", &varid_dummy_darray_var_double); ERR

    ret = PIOc_read_darray(ncid_read, varid_dummy_darray_var_double, ioid_double, ELEMENTS_PER_PE, read_darray_buffer_double); ERR
    for (int i = 0; i < ELEMENTS_PER_PE; i++)
    {
        diff_double = read_darray_buffer_double[i] - write_darray_buffer_double[i];
        if (fabs(diff_double) > 1E-5)
        {
            printf("rank = %d, read wrong data for dummy_darray_var_double at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get int type variable with int type decomposition, type conversions will not be performed. */
    ret = PIOc_inq_varid(ncid_read, "dummy_put_get_var_int", &varid_dummy_put_get_var_int); ERR

    /* Partial get: excluding the first and the last elements. */
    start[0] = 1;
    count[0] = PUT_GET_VAR_LEN - 2;
    ret = PIOc_get_vars_int(ncid_read, varid_dummy_put_get_var_int, start, count, NULL, get_var_buffer_int + 1); ERR
    for (int i = 1; i < PUT_GET_VAR_LEN - 1; i++)
    {
        if (get_var_buffer_int[i] != put_var_buffer_int[i])
        {
            printf("rank = %d, get wrong data for dummy_put_get_var_int at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get float type variable with double type decomposition, type conversions will be performed. */
    ret = PIOc_inq_varid(ncid_read, "dummy_put_get_var_float", &varid_dummy_put_get_var_float); ERR

    /* Partial get: excluding the first and the last elements. */
    start[0] = 1;
    count[0] = PUT_GET_VAR_LEN - 2;
    ret = PIOc_get_vars_double(ncid_read, varid_dummy_put_get_var_float, start, count, NULL, get_var_buffer_double + 1); ERR
    for (int i = 1; i < PUT_GET_VAR_LEN - 1; i++)
    {
        diff_double = get_var_buffer_double[i] - put_var_buffer_double[i];
        if (fabs(diff_double) > 1E-5)
        {
            printf("rank = %d, get wrong data for dummy_put_get_var_float at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get double type variable with double type decomposition */
    ret = PIOc_inq_varid(ncid_read, "dummy_put_get_var_double", &varid_dummy_put_get_var_double); ERR

    /* Partial get: excluding the first and the last elements. */
    start[0] = 1;
    count[0] = PUT_GET_VAR_LEN - 2;
    for (int i = 0; i < PUT_GET_VAR_LEN; i++)
        get_var_buffer_double[i] = 0.0;
    ret = PIOc_get_vars_double(ncid_read, varid_dummy_put_get_var_double, start, count, NULL, get_var_buffer_double + 1); ERR
    for (int i = 1; i < PUT_GET_VAR_LEN - 1; i++)
    {
        diff_double = get_var_buffer_double[i] - put_var_buffer_double[i];
        if (fabs(diff_double) > 1E-5)
        {
            printf("rank = %d, get wrong data for dummy_put_get_var_double at index %d\n", my_rank, i);
            return -1;
        }
    }

#if 0
    /* Get int type variable with int type decomposition, type conversions will not be performed. */
    ret = PIOc_inq_varid(ncid_read, "dummy_put_get_var_int_2D", &varid_dummy_put_get_var_int_2D); ERR

    /* Partial get: excluding the first and the last elements. */
    start2D[0] = 0;
    count2D[0] = PUT_GET_VAR_LEN_X;
    start2D[1] = 0;
    count2D[1] = PUT_GET_VAR_LEN_Y;
    ret = PIOc_get_vars_int(ncid_read, varid_dummy_put_get_var_int_2D, start, count, NULL,
                            (int *) get_var_buffer_int_2D); ERR
    for (int i = 0; i < PUT_GET_VAR_LEN_X; i++)
    {
        for (int j = 0; j < PUT_GET_VAR_LEN_Y; j++)
        {
            if (get_var_buffer_int_2D[i][j] != put_var_buffer_int_2D[i][j])
            {
                printf("rank = %d, get wrong data for dummy_put_get_var_int_2D at index x = %d y = %d\n", my_rank, i, j);
                return -1;
            }
        }
    }
#endif

    varid_dummy_put_get_var_int_2D = -1;
    ret = PIOc_inq_varid(ncid_read, "dummy_put_get_var_int_2D", &varid_dummy_put_get_var_int_2D); ERR

    ret = PIOc_get_var_int(ncid_read, varid_dummy_put_get_var_int_2D, (int *) get_var_buffer_int_2D); ERR
    for (int i = 0; i < PUT_GET_VAR_LEN_X; i++)
    {
        for (int j = 0; j < PUT_GET_VAR_LEN_Y; j++)
        {
            if (get_var_buffer_int_2D[i][j] != put_var_buffer_int_2D[i][j])
            {
                printf("rank = %d, get wrong data for dummy_put_get_var_int_2D at index x = %d y = %d\n", my_rank, i, j);
                return -1;
            }
        }
    }

    varid_dummy_put_get_var_int_3D = -1;
    ret = PIOc_inq_varid(ncid_read, "dummy_put_get_var_int_3D", &varid_dummy_put_get_var_int_3D); ERR

    ret = PIOc_get_var_int(ncid_read, varid_dummy_put_get_var_int_3D, (int *) get_var_buffer_int_3D); ERR
    for (int i = 0; i < PUT_GET_VAR_LEN_X; i++)
    {
        for (int j = 0; j < PUT_GET_VAR_LEN_Y; j++)
        {
            for (int k = 0; k < PUT_GET_VAR_LEN_Z; k++)
            {
                if (get_var_buffer_int_3D[i][j][k] != put_var_buffer_int_3D[i][j][k])
                {
                    printf("rank = %d, get wrong data for dummy_put_get_var_int_3D at index x = %d y = %d z = %d\n", my_rank, i, j, k);
                    return -1;
                }
            }
        }
    }

    dimids_put_get_var_len_3D_inq[0] = -1;
    dimids_put_get_var_len_3D_inq[1] = -1;
    dimids_put_get_var_len_3D_inq[2] = -1;
    ret = PIOc_inq_vardimid(ncid_read, varid_dummy_put_get_var_int_3D, dimids_put_get_var_len_3D_inq); ERR

    dimlens_put_get_var_len_3D_inq[0] = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimids_put_get_var_len_3D_inq[0], &dimlens_put_get_var_len_3D_inq[0]); ERR
    if (dimlens_put_get_var_len_3D_inq[0] != PUT_GET_VAR_LEN_X)
    {
        printf("rank = %d, read wrong length for dimension put_get_var_len_x\n", my_rank);
        return -1;
    }

    dimlens_put_get_var_len_3D_inq[1] = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimids_put_get_var_len_3D_inq[1], &dimlens_put_get_var_len_3D_inq[1]); ERR
    if (dimlens_put_get_var_len_3D_inq[1] != PUT_GET_VAR_LEN_Y)
    {
        printf("rank = %d, read wrong length for dimension put_get_var_len_y\n", my_rank);
        return -1;
    }

    dimlens_put_get_var_len_3D_inq[2] = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimids_put_get_var_len_3D_inq[2], &dimlens_put_get_var_len_3D_inq[2]); ERR
    if (dimlens_put_get_var_len_3D_inq[2] != PUT_GET_VAR_LEN_Z)
    {
        printf("rank = %d, read wrong length for dimension put_get_var_len_z\n", my_rank);
        return -1;
    }

    ret = PIOc_closefile(ncid_read); ERR

    ret = PIOc_freedecomp(iosysid, ioid_int); ERR
    ret = PIOc_freedecomp(iosysid, ioid_double); ERR

    ret = PIOc_finalize(iosysid); ERR

    return 0;
}

#define COLUMN_LEN 4096
#define LEVSNO_LEN 5
#define BUDG_FLUX_LEN 30
#define NTAPES 2
#define MAX_CHARS 256

int test_adios_read_case_2()
{
    /* Zero-based rank of processor. */
    int my_rank;

    /* Number of processors involved in current execution. */
    int ntasks;

    int format = PIO_IOTYPE_ADIOS;

    /* Number of processors that will do IO. */
    int niotasks;

    /* Zero based rank of first processor to be used for I/O. */
    const int ioproc_start = 0;

    int dimid_column;
    int dimid_levsno;
    int dimid_budg_flux;
    int dimid_ntapes;
    int dimid_max_chars;

    int dimids[2];

    PIO_Offset dimlen;

    int total_dims;

    int gdimlen[2];

    int iosysid;

    int ncid_write;
    int ncid_read;

    int varid_timemgr_rst_type;
    int varid_FSD24_PERIOD;
    int varid_cols1d_wtxy;
    int varid_DZSNO;
    int varid_locfnh;
    int varid_locfnhr;
    int varid_budg_fluxG;

    /* Sample data for scalar variables. */
    int put_scalar_int_data = -1;
    int get_scalar_int_data = -1;

    PIO_Offset start_1D[1];
    PIO_Offset count_1D[1];

    PIO_Offset start_2D[2];
    PIO_Offset count_2D[2];

    int ioid_1D;
    int ioid_2D;

    /* Buffers for sample write/read darray data. */
    double write_darray_buffer_1D[COLUMN_LEN];
    double read_darray_buffer_1D[COLUMN_LEN];
    double write_darray_buffer_2D[COLUMN_LEN * LEVSNO_LEN];
    double read_darray_buffer_2D[COLUMN_LEN * LEVSNO_LEN];

    /* Buffers for sample put/get var data. */
    double put_var_buffer_1D[BUDG_FLUX_LEN];
    double get_var_buffer_1D[BUDG_FLUX_LEN];
    char put_string_array[NTAPES][MAX_CHARS];
    char get_string_array[NTAPES][MAX_CHARS];

    PIO_Offset *compmap_1D;
    PIO_Offset *compmap_2D;

    int element_per_pe_1D;
    int element_per_pe_2D;

    int ret = PIO_NOERR;

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    int ioproc_stride = 2;
    if (ntasks == 1)
        ioproc_stride = 1;

    niotasks = ntasks / ioproc_stride;

    /* Initialize the PIO IO system. This specifies how
       many and which processors are involved in I/O. */
    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_SUBSET, &iosysid); ERR

    element_per_pe_1D = COLUMN_LEN / ntasks;
    compmap_1D = malloc(element_per_pe_1D * sizeof(PIO_Offset));
    for (int i = 0; i < element_per_pe_1D; i++)
        compmap_1D[i] = my_rank * element_per_pe_1D + i + 1;

    gdimlen[0] = COLUMN_LEN;
    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen, element_per_pe_1D, compmap_1D, &ioid_1D, NULL, NULL, NULL); ERR
    free(compmap_1D);

    element_per_pe_2D = (COLUMN_LEN * LEVSNO_LEN) / ntasks;
    compmap_2D = malloc(element_per_pe_2D * sizeof(PIO_Offset));
    for (int i = 0; i < element_per_pe_2D; i++)
        compmap_2D[i] = my_rank * element_per_pe_2D + i + 1;

    gdimlen[0] = COLUMN_LEN;
    gdimlen[1] = LEVSNO_LEN;
    ret = PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen, element_per_pe_2D, compmap_2D, &ioid_2D, NULL, NULL, NULL); ERR
    free(compmap_2D);

    /* Prepare sample data for write buffers and initialize read buffers. */
    for (int i = 0; i < element_per_pe_1D; i++)
    {
        write_darray_buffer_1D[i] = my_rank;
        read_darray_buffer_1D[i] = 0;
    }

    for (int i = 0; i < element_per_pe_2D; i++)
    {
        write_darray_buffer_2D[i] = my_rank;
        read_darray_buffer_2D[i] = 0;
    }

    for (int i = 0; i < BUDG_FLUX_LEN; i++)
    {
        put_var_buffer_1D[i] = (i + 1) * 0.1;
        get_var_buffer_1D[i] = -1.0;
    }

    ret = PIOc_createfile(iosysid, &ncid_write, &format, "test_adios_read_case_2.nc", PIO_CLOBBER); ERR

    /* Define two scalar variables for PIOc_put_vars. */
    ret = PIOc_def_var(ncid_write, "timemgr_rst_type", PIO_INT, 0, NULL, &varid_timemgr_rst_type); ERR
    ret = PIOc_def_var(ncid_write, "FSD24_PERIOD", PIO_INT, 0, NULL, &varid_FSD24_PERIOD); ERR

    /* Define dimensions. */
    ret = PIOc_def_dim(ncid_write, "column", COLUMN_LEN, &dimid_column); ERR
    ret = PIOc_def_dim(ncid_write, "levsno", LEVSNO_LEN, &dimid_levsno); ERR
    ret = PIOc_def_dim(ncid_write, "budg_flux", BUDG_FLUX_LEN, &dimid_budg_flux); ERR
    ret = PIOc_def_dim(ncid_write, "ntapes", NTAPES, &dimid_ntapes); ERR
    ret = PIOc_def_dim(ncid_write, "max_chars", MAX_CHARS, &dimid_max_chars); ERR

    /* Define two double variables for PIOc_write_darray. */
    dimids[0] = dimid_column;
    dimids[1] = dimid_levsno;
    ret = PIOc_def_var(ncid_write, "cols1d_wtxy", PIO_DOUBLE, 1, dimids, &varid_cols1d_wtxy); ERR
    ret = PIOc_def_var(ncid_write, "DZSNO", PIO_DOUBLE, 2, dimids, &varid_DZSNO); ERR

    /* Define two text variables for PIOc_put_vars. */
    dimids[0] = dimid_ntapes;
    dimids[1] = dimid_max_chars;
    ret = PIOc_def_var(ncid_write, "locfnh", PIO_CHAR, 2, dimids, &varid_locfnh); ERR
    ret = PIOc_def_var(ncid_write, "locfnhr", PIO_CHAR, 2, dimids, &varid_locfnhr); ERR

    /* Define one double variables for PIOc_put_vars. */
    dimids[0] = dimid_budg_flux;
    ret = PIOc_def_var(ncid_write, "budg_fluxG", PIO_DOUBLE, 1, dimids, &varid_budg_fluxG); ERR

    ret = PIOc_enddef(ncid_write); ERR

    /* Put one scalar variable. */
    put_scalar_int_data = 1;
    ret = PIOc_put_var_int(ncid_write, varid_timemgr_rst_type, &put_scalar_int_data); ERR

    /* Write one 1D double variable. */
    ret = PIOc_write_darray(ncid_write, varid_cols1d_wtxy, ioid_1D, element_per_pe_1D, write_darray_buffer_1D, NULL); ERR

    /* Write one 2D double variable. */
    ret = PIOc_write_darray(ncid_write, varid_DZSNO, ioid_2D, element_per_pe_2D, write_darray_buffer_2D, NULL); ERR

    /* Put one scalar variable. */
    put_scalar_int_data = 12;
    ret = PIOc_put_var_int(ncid_write, varid_FSD24_PERIOD, &put_scalar_int_data); ERR

    /* Put two text variables. */
    start_2D[0] = 0;
    count_2D[0] = NTAPES;
    start_2D[1] = 0;
    count_2D[1] = MAX_CHARS;

    /* Empty strings for locfnh */
    memset(put_string_array, 0, sizeof(put_string_array));
    ret = PIOc_put_vara_text(ncid_write, varid_locfnh, start_2D, count_2D, put_string_array); ERR

    /* Non-empty strings for locfnhr */
    for (int t = 0; t < NTAPES; t++)
      snprintf(put_string_array[t], MAX_CHARS, "tape_%d.nc", t);
    ret = PIOc_put_vara_text(ncid_write, varid_locfnhr, start_2D, count_2D, put_string_array); ERR

    /* Put one 1D double variable. */
    ret = PIOc_put_var_double(ncid_write, varid_budg_fluxG, put_var_buffer_1D); ERR

    ret = PIOc_closefile(ncid_write); ERR

    ret = PIOc_openfile(iosysid, &ncid_read, &format, "test_adios_read_case_2.nc", PIO_NOWRITE); ERR

    /* Get one scalar variable. */
    varid_timemgr_rst_type = -1;
    ret = PIOc_inq_varid(ncid_read, "timemgr_rst_type", &varid_timemgr_rst_type); ERR

    get_scalar_int_data = -1;
    ret = PIOc_get_var_int(ncid_read, varid_timemgr_rst_type, &get_scalar_int_data); ERR
    if (get_scalar_int_data != 1)
    {
        printf("rank = %d, read wrong data for timemgr_rst_type\n", my_rank);
        return -1;
    }

    /* Close file and open it later. */
    ret = PIOc_closefile(ncid_read); ERR

    /* Reopen file. */
    ret = PIOc_openfile(iosysid, &ncid_read, &format, "test_adios_read_case_2.nc", PIO_NOWRITE); ERR

    total_dims = 0;
    ret = PIOc_inq_ndims(ncid_read, &total_dims); ERR

    dimid_column = -1;
    ret = PIOc_inq_dimid(ncid_read, "column", &dimid_column); ERR

    if (dimid_column < 0 || dimid_column > total_dims)
    {
        printf("rank = %d, read wrong ID for dimension column\n", my_rank);
        return -1;
    }

    dimlen = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimid_column, &dimlen); ERR
    if (dimlen != COLUMN_LEN)
    {
        printf("rank = %d, read wrong length for dimension column\n", my_rank);
        return -1;
    }

    varid_DZSNO = -1;
    ret = PIOc_inq_varid(ncid_read, "DZSNO", &varid_DZSNO); ERR

    dimids[0] = -1;
    dimids[1] = -1;
    ret = PIOc_inq_vardimid(ncid_read, varid_DZSNO, dimids); ERR

    if (dimids[0] < 0 || dimids[0] > total_dims)
    {
        printf("rank = %d, read wrong ID for dimension column\n", my_rank);
        return -1;
    }

    dimlen = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimids[0], &dimlen); ERR
    if (dimlen != COLUMN_LEN)
    {
        printf("rank = %d, read wrong length for dimension column\n", my_rank);
        return -1;
    }

    if (dimids[1] < 0 || dimids[1] > total_dims)
    {
        printf("rank = %d, read wrong ID for dimension levsno\n", my_rank);
        return -1;
    }

    dimlen = -1;
    ret = PIOc_inq_dimlen(ncid_read, dimids[1], &dimlen); ERR
    if (dimlen != LEVSNO_LEN)
    {
        printf("rank = %d, read wrong length for dimension levsno\n", my_rank);
        return -1;
    }

    /* Read one 1D double variable. */
    varid_cols1d_wtxy = -1;
    ret = PIOc_inq_varid(ncid_read, "cols1d_wtxy", &varid_cols1d_wtxy); ERR

    for (int i = 0; i < element_per_pe_1D; i++)
        read_darray_buffer_1D[i] = 0.0;
    ret = PIOc_read_darray(ncid_read, varid_cols1d_wtxy, ioid_1D, element_per_pe_1D, read_darray_buffer_1D); ERR

    for (int i = 0; i < element_per_pe_1D; i++)
    {
        double diff = read_darray_buffer_1D[i] - write_darray_buffer_1D[i];
        if (fabs(diff) > 1E-5)
        {
            printf("rank = %d, read wrong data for cols1d_wtxy at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Read one 2D double variable. */
    varid_DZSNO = -1;
    ret = PIOc_inq_varid(ncid_read, "DZSNO", &varid_DZSNO); ERR

    for (int i = 0; i < element_per_pe_2D; i++)
        read_darray_buffer_2D[i] = 0.0;
    ret = PIOc_read_darray(ncid_read, varid_DZSNO, ioid_2D, element_per_pe_2D, read_darray_buffer_2D); ERR

    for (int i = 0; i < element_per_pe_2D; i++)
    {
        double diff = read_darray_buffer_2D[i] - write_darray_buffer_2D[i];
        if (fabs(diff) > 1E-5)
        {
            printf("rank = %d, read wrong data for DZSNO at index %d\n", my_rank, i);
            return -1;
        }
    }

    /* Get one scalar variable. */
    varid_FSD24_PERIOD = -1;
    ret = PIOc_inq_varid(ncid_read, "FSD24_PERIOD", &varid_FSD24_PERIOD); ERR

    get_scalar_int_data = -1;
    ret = PIOc_get_var_int(ncid_read, varid_FSD24_PERIOD, &get_scalar_int_data); ERR
    if (get_scalar_int_data != 12)
    {
        printf("rank = %d, read wrong data for FSD24_PERIOD\n", my_rank);
        return -1;
    }

    /* Get two text variables. */
    varid_locfnh = -1;
    ret = PIOc_inq_varid(ncid_read, "locfnh", &varid_locfnh); ERR

    memset(get_string_array, 0, sizeof(get_string_array));
    ret = PIOc_get_var_text(ncid_read, varid_locfnh, get_string_array); ERR

    for (int t = 0; t < NTAPES; t++)
    {
        if (strlen(get_string_array[t]) > 0)
        {
            printf("rank = %d, tape = %d, read wrong text (expected: empty, actual: %s) for locfnh\n", my_rank, t, get_string_array[t]);
            return -1;
        }
    }

    memset(get_string_array, 0, sizeof(get_string_array));
    ret = PIOc_get_var_text(ncid_read, varid_locfnhr, get_string_array); ERR

    for (int t = 0; t < NTAPES; t++)
    {
        if (strncmp(get_string_array[t], put_string_array[t], MAX_CHARS))
        {
            printf("rank = %d, tape = %d, read wrong text (expected: %s, actual: %s) for locfnhr\n", my_rank, t, put_string_array[t], get_string_array[t]);
            return -1;
        }
    }

    /* Get one 1D double variable. */
    varid_budg_fluxG = -1;
    ret = PIOc_inq_varid(ncid_read, "budg_fluxG", &varid_budg_fluxG); ERR

    for (int i = 0; i < BUDG_FLUX_LEN; i++)
        get_var_buffer_1D[i] = -1.0;

    ret = PIOc_get_var_double(ncid_read, varid_budg_fluxG, get_var_buffer_1D); ERR
    for (int i = 0; i < BUDG_FLUX_LEN; i++)
    {
        double diff = get_var_buffer_1D[i] - put_var_buffer_1D[i];
        if (fabs(diff) > 1E-5)
        {
            printf("rank = %d, get wrong data for budg_fluxG at index %d\n", my_rank, i);
            return -1;
        }
    }

    ret = PIOc_closefile(ncid_read); ERR

    ret = PIOc_freedecomp(iosysid, ioid_1D); ERR
    ret = PIOc_freedecomp(iosysid, ioid_2D); ERR

    ret = PIOc_finalize(iosysid); ERR

    return 0;
}

int test_adios_read_case_3()
{
    /* Zero-based rank of processor. */
    int my_rank;

    /* Number of processors involved in current execution. */
    int ntasks;

    int format = PIO_IOTYPE_ADIOS;

    /* Number of processors that will do IO. */
    int niotasks;

    /* Stride in the MPI rank between IO tasks. Always 1 in this example. */
    const int ioproc_stride = 1;

    /* Zero based rank of first processor to be used for I/O. */
    const int ioproc_start = 0;

    int iosysid;

    /* The ncid of the netCDF file created and read in this example. */
    int ncid_write;
    int ncid_read;

    /* The IDs of the netCDF variables in the example file. */
    int varid_dummy_scalar_var_int_write = -1;
    int varid_dummy_scalar_var_int_read = -1;

    int ret = PIO_NOERR;

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    /* Keep things simple - 1 IO task per MPI process */
    niotasks = ntasks;

    /* Initialize the PIO IO system. This specifies how
       many and which processors are involved in I/O. */
    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_SUBSET, &iosysid); ERR

    ret = PIOc_createfile(iosysid, &ncid_write, &format, "test_adios_read_case_3.nc", PIO_CLOBBER); ERR

    /* Define some variables for PIOc_write_darray. */
    ret = PIOc_def_var(ncid_write, "dummy_scalar_var_int", PIO_INT, 0, NULL, &varid_dummy_scalar_var_int_write); ERR
    if (my_rank == 0)
        printf("varid_dummy_scalar_var_int_write = %d\n", varid_dummy_scalar_var_int_write);

    ret = PIOc_enddef(ncid_write); ERR

    ret = PIOc_closefile(ncid_write); ERR

    ret = PIOc_openfile(iosysid, &ncid_read, &format, "test_adios_read_case_3.nc", PIO_NOWRITE); ERR

    ret = PIOc_inq_varid(ncid_read, "dummy_scalar_var_int", &varid_dummy_scalar_var_int_read); ERR
    if (my_rank == 0)
        printf("varid_dummy_scalar_var_int_read = %d\n", varid_dummy_scalar_var_int_read);

    ret = PIOc_closefile(ncid_read); ERR

    ret = PIOc_finalize(iosysid); ERR

    return 0;
}

int test_adios_cdf5_new_data_types()
{
    /* Zero-based rank of processor. */
    int my_rank;

    /* Number of processors involved in current execution. */
    int ntasks;

    int format = PIO_IOTYPE_ADIOS;

    /* Number of processors that will do IO. */
    int niotasks;

    /* Stride in the MPI rank between IO tasks. Always 1 in this example. */
    const int ioproc_stride = 1;

    /* Zero based rank of first processor to be used for I/O. */
    const int ioproc_start = 0;

    int iosysid;

    /* The ncid of the netCDF file created and read in this example. */
    int ncid;

    /* The IDs of the netCDF variables in the example file. */
    int varid_ubyte;
    int varid_ushort;
    int varid_uint;
    int varid_int64;
    int varid_uint64;

    /* Write buffers with sample data for attributes and variables. */
    unsigned char ubyte_data = 43;
    unsigned short ushort_data = 666;
    unsigned int uint_data = 666666;
    long long int64_data = -99999999999;
    unsigned long long uint64_data = 99999999999;

    /* Read buffers for attributes and variables. */
    unsigned char ubyte_data_in;
    unsigned short ushort_data_in;
    unsigned int uint_data_in;
    long long int64_data_in;
    unsigned long long uint64_data_in;

    int ret = PIO_NOERR;

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    /* Keep things simple - 1 IO task per MPI process */
    niotasks = ntasks;

    /* Initialize the PIO IO system. This specifies how
       many and which processors are involved in I/O. */
    ret = PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_SUBSET, &iosysid); ERR

    ret = PIOc_createfile(iosysid, &ncid, &format, "test_adios_cdf5_new_data_types.nc", PIO_CLOBBER); ERR

    ret = PIOc_put_att_uchar(ncid, PIO_GLOBAL, "att_uchar", PIO_UBYTE, 1, &ubyte_data); ERR

    ret = PIOc_put_att_ushort(ncid, PIO_GLOBAL, "att_ushort", PIO_USHORT, 1, &ushort_data); ERR

    ret = PIOc_put_att_uint(ncid, PIO_GLOBAL, "att_uint", PIO_UINT, 1, &uint_data); ERR

    ret = PIOc_put_att_longlong(ncid, PIO_GLOBAL, "att_int64", PIO_INT64, 1, &int64_data); ERR

    ret = PIOc_put_att_ulonglong(ncid, PIO_GLOBAL, "att_uint64", PIO_UINT64, 1, &uint64_data); ERR

    ret = PIOc_def_var(ncid, "var_ubyte", PIO_UBYTE, 0, NULL, &varid_ubyte); ERR

    ret = PIOc_def_var(ncid, "var_ushort", PIO_USHORT, 0, NULL, &varid_ushort); ERR

    ret = PIOc_def_var(ncid, "var_uint", PIO_UINT, 0, NULL, &varid_uint); ERR

    ret = PIOc_def_var(ncid, "var_int64", PIO_INT64, 0, NULL, &varid_int64); ERR

    ret = PIOc_def_var(ncid, "var_uint64", PIO_UINT64, 0, NULL, &varid_uint64); ERR

    ret = PIOc_enddef(ncid); ERR

    ret = PIOc_put_var_uchar(ncid, varid_ubyte, &ubyte_data); ERR

    ret = PIOc_put_var_ushort(ncid, varid_ushort, &ushort_data); ERR

    ret = PIOc_put_var_uint(ncid, varid_uint, &uint_data); ERR

    ret = PIOc_put_var_longlong(ncid, varid_int64, &int64_data); ERR

    ret = PIOc_put_var_ulonglong(ncid, varid_uint64, &uint64_data); ERR

    ret = PIOc_closefile(ncid); ERR

    ret = PIOc_openfile(iosysid, &ncid, &format, "test_adios_cdf5_new_data_types.nc", PIO_NOWRITE); ERR

    ubyte_data_in = 0;
    ret = PIOc_get_att_uchar(ncid, PIO_GLOBAL, "att_uchar", &ubyte_data_in); ERR
    ret = (ubyte_data_in == ubyte_data)? PIO_NOERR : -1; ERR

    ushort_data_in = 0;
    ret = PIOc_get_att_ushort(ncid, PIO_GLOBAL, "att_ushort", &ushort_data_in); ERR
    ret = (ushort_data_in == ushort_data)? PIO_NOERR : -1; ERR

    uint_data_in = 0;
    ret = PIOc_get_att_uint(ncid, PIO_GLOBAL, "att_uint", &uint_data_in); ERR
    ret = (uint_data_in == uint_data)? PIO_NOERR : -1; ERR

    int64_data_in = 0;
    ret = PIOc_get_att_longlong(ncid, PIO_GLOBAL, "att_int64", &int64_data_in); ERR
    ret = (int64_data_in == int64_data)? PIO_NOERR : -1; ERR

    uint64_data_in = 0;
    ret = PIOc_get_att_ulonglong(ncid, PIO_GLOBAL, "att_uint64", &uint64_data_in); ERR
    ret = (uint64_data_in == uint64_data)? PIO_NOERR : -1; ERR

    varid_ubyte = -1;
    ret = PIOc_inq_varid(ncid, "var_ubyte", &varid_ubyte); ERR

    ubyte_data_in = 0;
    ret = PIOc_get_var_uchar(ncid, varid_ubyte, &ubyte_data_in); ERR
    ret = (ubyte_data_in == ubyte_data)? PIO_NOERR : -1; ERR

    varid_ushort = -1;
    ret = PIOc_inq_varid(ncid, "var_ushort", &varid_ushort); ERR

    ushort_data_in = 0;
    ret = PIOc_get_var_ushort(ncid, varid_ushort, &ushort_data_in); ERR
    ret = (ushort_data_in == ushort_data)? PIO_NOERR : -1; ERR

    varid_uint = -1;
    ret = PIOc_inq_varid(ncid, "var_uint", &varid_uint); ERR

    uint_data_in = 0;
    ret = PIOc_get_var_uint(ncid, varid_uint, &uint_data_in); ERR
    ret = (uint_data_in == uint_data)? PIO_NOERR : -1; ERR

    varid_int64 = -1;
    ret = PIOc_inq_varid(ncid, "var_int64", &varid_int64); ERR

    int64_data_in = 0;
    ret = PIOc_get_var_longlong(ncid, varid_int64, &int64_data_in); ERR
    ret = (int64_data_in == int64_data)? PIO_NOERR : -1; ERR

    varid_uint64 = -1;
    ret = PIOc_inq_varid(ncid, "var_uint64", &varid_uint64); ERR

    uint64_data_in = 0;
    ret = PIOc_get_var_ulonglong(ncid, varid_uint64, &uint64_data_in); ERR
    ret = (uint64_data_in == uint64_data)? PIO_NOERR : -1; ERR

    ret = PIOc_closefile(ncid); ERR

    ret = PIOc_finalize(iosysid); ERR

    return 0;
}
#endif
