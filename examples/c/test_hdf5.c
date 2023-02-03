#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#include <assert.h>
#include <math.h>
#ifdef TIMING
#include <gptl.h>
#endif

#define ERR { if (ret != PIO_NOERR) printf("rank = %d, error at line = %d\n", my_rank, __LINE__); }

#define DIM_LEV_LEN 3
#define MAX_STR_LEN 8
#define MAX_LAT_LON_VARS 400

int main(int argc, char* argv[])
{
#ifdef _HDF5
    int my_rank;
    int ntasks;
    int format = PIO_IOTYPE_HDF5;
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

    int att_type;
    PIO_Offset att_len;

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
        snprintf(filename, PIO_MAX_NAME, "test_hdf5_f_case_rearr_%d.nc", rearranger[r]);

        if (my_rank == 0)
        {
            printf("Test writing %s with HDF5 IO type start\n", filename);
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
        ret = PIOc_put_att(ncid, varid_P0, "units", PIO_CHAR, strlen("Pa"), "Pa"); ERR
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
            printf("Test writing %s with HDF5 IO type end\n", filename);
            fflush(stdout);
        }

#ifdef _NETCDF4
        /* Direct read support for HDF5 IO type is not implemented yet: SCORPIO implicitly switches HDF5 type to
         * NETCDF4 type for reading back output files (generated by HDF5 type, fully NETCDF4 compatible). */
        if (my_rank == 0)
        {
            printf("Test reading %s with NETCDF4 IO type start\n", filename);
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

        char get_att_text[PIO_MAX_NAME + 1] = "\0";
        ret = PIOc_get_att_text(ncid, varid_P0, "units", get_att_text); ERR
        assert(strncmp(get_att_text, "Pa", 2) == 0);

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
            printf("Test reading %s with NETCDF4 IO type end\n", filename);
            fflush(stdout);
        }
#endif

        ret = PIOc_freedecomp(iosysid, ioid_double_1D); ERR
        ret = PIOc_freedecomp(iosysid, ioid_double_2D); ERR

        /* Simulate history/restart files of E3SM I case (compset I1850GSWCNPRDCTCBC, res f19_g16) */
        snprintf(filename, PIO_MAX_NAME, "test_hdf5_i_case_rearr_%d.nc", rearranger[r]);

        if (my_rank == 0)
        {
            printf("Test writing %s with HDF5 IO type start\n", filename);
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
            printf("Test writing %s with HDF5 IO type end\n", filename);
            fflush(stdout);
        }

#ifdef _NETCDF4
        /* Direct read support for HDF5 IO type is not implemented yet: SCORPIO implicitly switches HDF5 type to
         * NETCDF4 type for reading back output files (generated by HDF5 type, fully NETCDF4 compatible). */
        if (my_rank == 0)
        {
            printf("Test reading %s with NETCDF4 IO type start\n", filename);
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
            printf("Test reading %s with NETCDF4 IO type end\n", filename);
            fflush(stdout);
        }
#endif

        ret = PIOc_finalize(iosysid); ERR
    }

    MPI_Finalize();

#ifdef TIMING
    GPTLfinalize();
#endif

#else
    printf("SCORPIO is not configured with HDF5 support.\n");
#endif

    return 0;
}
