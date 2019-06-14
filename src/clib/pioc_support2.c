/** @file
 * Support functions for the PIO library.
 */
#include <config.h>
#include <stdio.h>
#if PIO_ENABLE_LOGGING
#include <stdarg.h>
#include <unistd.h>
#endif /* PIO_ENABLE_LOGGING */
#include <pio.h>
#include <pio_internal.h>

#include <execinfo.h>

#ifdef _ADIOS
#include <dirent.h>
#endif

#define VERSNO 2001

extern bool fortran_order;

/* Read the decomp information from a netCDF decomp file. This is an
 * internal function.
 *
 * @param iosysid the IO system ID.
 * @param filename the name the decomp file will have.
 * @param ndims pointer to int that will get number of dims in the
 * data being described. Ignored if NULL.
 * @param global_dimlen a pointer that gets an array, of size ndims,
 * that will have the size of the global array in each
 * dimension. Ignored if NULL, otherwise must be freed by caller.
 * @param num_tasks pointer to int that gets the number of tasks the
 * data are decomposed over. Ignored if NULL.
 * @param task_maplen pointer that gets array of size num_tasks that
 * gets the length of the map for each task. Ignored if NULL,
 * otherwise must be freed by caller.
 * @param max_maplen pointer to int that gets the maximum maplen for
 * any task. Ignored if NULL.
 * @param map pointer that gets a 2D array of size [num_tasks][max_maplen]
 * that will have the 0-based mapping from local to global array
 * elements. Ignored if NULL, otherwise must be freed by caller.
 * @param title pointer that will get the contents of title attribute,
 * if present. If present, title will be < PIO_MAX_NAME + 1 in
 * length. Ignored if NULL.
 * @param history pointer that will get the contents of history attribute,
 * if present. If present, history will be < PIO_MAX_NAME + 1 in
 * length. Ignored if NULL.
 * @param source pointer that will get the contents of source
 * attribute. Source will be < PIO_MAX_NAME + 1 in length. Ignored if
 * NULL.
 * @param version pointer that will get the contents of version
 * attribute. It will be < PIO_MAX_NAME + 1 in length. Ignored if
 * NULL.
 * @param fortran_order int pointer that will get a 0 if this
 * decomposition file uses C array ordering, 1 if it uses Fortran
 * array ordering.
 * @returns 0 for success, error code otherwise.
 */
int pioc_read_nc_decomp_int(int iosysid, const char *filename, int *ndims, int **global_dimlen,
                            int *num_tasks, int **task_maplen, int *max_maplen, int **map, char *title,
                            char *history, char *source, char *version, int *fortran_order)
{
    iosystem_desc_t *ios;
    int ncid;
    int ret;

    /*
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    if (!filename)
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);

    LOG((1, "pioc_read_nc_decomp_int iosysid = %d filename = %s", iosysid, filename));

    if ((ret = PIOc_open(iosysid, filename, NC_WRITE, &ncid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    */

    char version_in[PIO_MAX_NAME + 1];
    /*
    if ((ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_VERSION_ATT_NAME, version_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    LOG((3, "version_in = %s", version_in));
    if (version)
        strncpy(version, version_in, PIO_MAX_NAME + 1);
    */

    char order_in[PIO_MAX_NAME + 1];
    /*
    if ((ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_ORDER_ATT_NAME, order_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    LOG((3, "order_in = %s", order_in));
    if (fortran_order)
    {
        if (!strncmp(order_in, DECOMP_C_ORDER_STR, PIO_MAX_NAME + 1))
            *fortran_order = 0;
        else if (!strncmp(order_in, DECOMP_FORTRAN_ORDER_STR, PIO_MAX_NAME + 1))
            *fortran_order = 1;
        else
            return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);
    }
    */

    int max_maplen_in;
    /*
    if ((ret = PIOc_get_att_int(ncid, NC_GLOBAL, DECOMP_MAX_MAPLEN_ATT_NAME, &max_maplen_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    LOG((3, "max_maplen_in = %d", max_maplen_in));
    if (max_maplen)
        *max_maplen = max_maplen_in;
    */

    char title_in[PIO_MAX_NAME + 1];
    /*
    ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_TITLE_ATT_NAME, title_in);
    if (ret == PIO_NOERR)
    {
        if (title)
            strncpy(title, title_in, PIO_MAX_NAME + 1);
    }
    else if (ret == PIO_ENOTATT)
    {
        if (title)
            title[0] = '\0';
    }
    else
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    */

    char history_in[PIO_MAX_NAME + 1];
    /*
    ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_HISTORY_ATT_NAME, history_in);
    if (ret == PIO_NOERR)
    {
        if (history)
            strncpy(history, history_in, PIO_MAX_NAME + 1);
    }
    else if (ret == PIO_ENOTATT)
    {
        if (history)
            history[0] = '\0';
    }
    else
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    */

    char source_in[PIO_MAX_NAME + 1];
    /*
    if ((ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_SOURCE_ATT_NAME, source_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (source)
        strncpy(source, source_in, PIO_MAX_NAME + 1);
    */

    int dim_dimid;
    PIO_Offset ndims_in;
    /*
    if ((ret = PIOc_inq_dimid(ncid, DECOMP_DIM_DIM, &dim_dimid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if ((ret = PIOc_inq_dim(ncid, dim_dimid, NULL, &ndims_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (ndims)
        *ndims = ndims_in;
    */

    int gsize_varid;
    int global_dimlen_in[ndims_in];
    /*
    if ((ret = PIOc_inq_varid(ncid, DECOMP_GLOBAL_SIZE_VAR_NAME, &gsize_varid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if ((ret = PIOc_get_var_int(ncid, gsize_varid, global_dimlen_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (global_dimlen)
    {
        if (!(*global_dimlen = malloc(ndims_in * sizeof(int))))
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        for (int d = 0; d < ndims_in; d++)
            (*global_dimlen)[d] = global_dimlen_in[d];
    }
    */

    int task_dimid;
    PIO_Offset num_tasks_in;
    /*
    if ((ret = PIOc_inq_dimid(ncid, DECOMP_TASK_DIM_NAME, &task_dimid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if ((ret = PIOc_inq_dim(ncid, task_dimid, NULL, &num_tasks_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (num_tasks)
        *num_tasks = num_tasks_in;
    */

    int maplen_varid;
    int task_maplen_in[num_tasks_in];
    /*
    if ((ret = PIOc_inq_varid(ncid, DECOMP_MAPLEN_VAR_NAME, &maplen_varid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if ((ret = PIOc_get_var_int(ncid, maplen_varid, task_maplen_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (task_maplen)
    {
        if (!(*task_maplen = malloc(num_tasks_in * sizeof(int))))
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        for (int t = 0; t < num_tasks_in; t++)
            (*task_maplen)[t] = task_maplen_in[t];
    }
    */

    int map_varid;
    /* int map_in[num_tasks_in][max_maplen_in]; */
    /*
    if ((ret = PIOc_inq_varid(ncid, DECOMP_MAP_VAR_NAME, &map_varid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    */
    /*
    if ((ret = PIOc_get_var_int(ncid, map_varid, (int *)map_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    */
    /* PIOc_get_var_int(ncid, map_varid, (int *)map_in);
    if (map)
    {
        if (!(*map = malloc(num_tasks_in * max_maplen_in * sizeof(int))))
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        for (int t = 0; t < num_tasks_in; t++)
            for (int l = 0; l < max_maplen_in; l++)
                (*map)[t * max_maplen_in + l] = map_in[t][l];
    }
    */
    if (map)
    {
        if ((ret = PIOc_inq_varid(ncid, DECOMP_MAP_VAR_NAME, &map_varid)))
            return pio_err(ios, NULL, ret, __FILE__, __LINE__);

        if ((ret = PIOc_get_var_int(ncid, map_varid, *map)))
            return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    }


    if ((ret = PIOc_closefile(ncid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Write the decomposition map to a file.
 *
 * @param file the filename to be used.
 * @param iosysid the IO system ID.
 * @param ioid the ID of the IO description.
 * @param comm an MPI communicator.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_write_decomp(const char *file, int iosysid, int ioid, MPI_Comm comm)
{
    iosystem_desc_t *ios;
    io_desc_t *iodesc;

    LOG((1, "PIOc_write_decomp file = %s iosysid = %d ioid = %d", file, iosysid, ioid));

    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
        return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__);

    return PIOc_writemap(file, iodesc->ioid, iodesc->ndims, iodesc->dimlen, iodesc->maplen, iodesc->map,
                         comm);
}

/**
 * Write the decomposition map to a file.
 *
 * @param file the filename
 * @param ioid id of the decomposition
 * @param ndims the number of dimensions
 * @param gdims an array of dimension ids
 * @param maplen the length of the map
 * @param map the map array
 * @param comm an MPI communicator.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_writemap(const char *file, int ioid, int ndims, const int *gdims, PIO_Offset maplen,
                  PIO_Offset *map, MPI_Comm comm)
{
    int npes, myrank;
    PIO_Offset *nmaplen = NULL;
    MPI_Status status;
    int i;
    PIO_Offset *nmap;
    int gdims_reversed[ndims];
    int mpierr = MPI_SUCCESS; /* Return code for MPI calls. */

    LOG((1, "PIOc_writemap file = %s ioid = %d ndims = %d maplen = %d", file, ioid, ndims, maplen));

    if ((mpierr = MPI_Comm_size(comm, &npes)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    if ((mpierr = MPI_Comm_rank(comm, &myrank)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    LOG((2, "npes = %d myrank = %d", npes, myrank));

    /* Allocate memory for the nmaplen. */
    if (myrank == 0)
        if (!(nmaplen = malloc(npes * sizeof(PIO_Offset))))
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);

    if ((mpierr = MPI_Gather(&maplen, 1, PIO_OFFSET, nmaplen, 1, PIO_OFFSET, 0, comm)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

    /* Only rank 0 writes the file. */
    if (myrank == 0)
    {
        FILE *fp;

        /* Open the file to write. */
        if (!(fp = fopen(file, "w")))
            return pio_err(NULL, NULL, PIO_EIO, __FILE__, __LINE__);

        /* Write the version and dimension info. */
        fprintf(fp,"version %d npes %d ndims %d \n", VERSNO, npes, ndims);
        if(fortran_order)
        {
            for(int i=0; i<ndims; i++)
            {
                gdims_reversed[i] = gdims[ndims - 1 - i];
            }
            gdims = gdims_reversed;
        }
        for (i = 0; i < ndims; i++)
            fprintf(fp, "%d ", gdims[i]);
        fprintf(fp, "\n");

        /* Write the map. */
        fprintf(fp, "0 %lld\n", nmaplen[0]);
        for (i = 0; i < nmaplen[0]; i++)
            fprintf(fp, "%lld ", map[i]);
        fprintf(fp,"\n");

        for (i = 1; i < npes; i++)
        {
            LOG((2, "creating nmap for i = %d", i));
            nmap = (PIO_Offset *)malloc(nmaplen[i] * sizeof(PIO_Offset));

            if ((mpierr = MPI_Send(&i, 1, MPI_INT, i, npes + i, comm)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            if ((mpierr = MPI_Recv(nmap, nmaplen[i], PIO_OFFSET, i, i, comm, &status)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            LOG((2,"MPI_Recv map complete"));

            fprintf(fp, "%d %lld\n", i, nmaplen[i]);
            for (int j = 0; j < nmaplen[i]; j++)
                fprintf(fp, "%lld ", nmap[j]);
            fprintf(fp, "\n");

            free(nmap);
        }
        /* Free memory for the nmaplen. */
        free(nmaplen);
        fprintf(fp, "\n");
        print_trace(fp);

        /* Print the decomposition id */
        fprintf(fp, "ioid\t%d\n", ioid);
        /* Close the file. */
        fclose(fp);
        LOG((2,"decomp file closed."));
    }
    else
    {
        LOG((2,"ready to MPI_Recv..."));
        if ((mpierr = MPI_Recv(&i, 1, MPI_INT, 0, npes+myrank, comm, &status)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        LOG((2,"MPI_Recv got %d", i));
        if ((mpierr = MPI_Send(map, maplen, PIO_OFFSET, 0, myrank, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        LOG((2,"MPI_Send map complete"));
    }

    return PIO_NOERR;
}

/**
 * Write the decomposition map to a file for F90.
 *
 * @param file the filename
 * @param ndims the number of dimensions
 * @param gdims an array of dimension ids
 * @param maplen the length of the map
 * @param map the map array
 * @param comm an MPI communicator.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_writemap_from_f90(const char *file, int ioid, int ndims, const int *gdims,
                           PIO_Offset maplen, const PIO_Offset *map, int f90_comm)
{
    return PIOc_writemap(file, ioid, ndims, gdims, maplen, (PIO_Offset *)map,
                         MPI_Comm_f2c(f90_comm));
}


/**
 * The PIO library maintains its own set of ncids. This is the next
 * ncid number that will be assigned.
 */

/**
 * Create a new file using pio. This is an internal function that is
 * called by both PIOc_create() and PIOc_createfile(). Input
 * parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid A defined pio system ID, obtained from
 * PIOc_InitIntercomm() or PIOc_InitAsync().
 * @param ncidp A pointer that gets the ncid of the newly created
 * file.
 * @param iotype A pointer to a pio output format. Must be one of
 * PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C, or
 * PIO_IOTYPE_NETCDF4P.
 * @param filename The filename to create.
 * @param mode The netcdf mode for the create operation.
 * @returns 0 for success, error code otherwise.
 * @ingroup PIO_createfile
 */
int PIOc_createfile_int(int iosysid, int *ncidp, int *iotype, const char *filename,
                        int mode)
{
  return PIO_NOERR;
}

/**
 * Check that a file meets PIO requirements for use of unlimited
 * dimensions. This function is only called on netCDF-4 files. If the
 * file is found to violate PIO requirements it is closed.
 * 
 * @param ncid the file->fh for this file (the real netCDF ncid, not
 * the pio_ncid).
 * @returns 0 if file is OK, error code otherwise.
 * @author Ed Hartnett
 */
int check_unlim_use(int ncid)
{
  return PIO_NOERR;
}

/**
 * Check whether an IO type is valid for the build.
 *
 * @param iotype the IO type to check
 * @returns 0 if not valid, non-zero otherwise.
 */
int iotype_is_valid(int iotype)
{
  return 1;
}

