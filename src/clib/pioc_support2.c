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

#ifdef _ADIOS
/**
 * Utility function to remove a directory and all its contents.
 */
int remove_directory(const char *path)
{
    DIR *d = opendir(path);
    size_t path_len = strlen(path);
    int r = -1;

    if (d)
    {
        struct dirent *p;

        r = 0;

        while (!r && (p = readdir(d)))
        {
            int r2 = -1;
            char *buf;
            size_t len;

            /* Skip the names "." and ".." as we don't want to recurse on them. */
            if (!strcmp(p->d_name, ".") || !strcmp(p->d_name, ".."))
                continue;

            len = path_len + strlen(p->d_name) + 2;
            buf = malloc(len);

            if (buf)
            {
                struct stat statbuf;

                snprintf(buf, len, "%s/%s", path, p->d_name);

                if (!stat(buf, &statbuf))
                {
                    if (S_ISDIR(statbuf.st_mode))
                        r2 = remove_directory(buf);
                    else
                        r2 = unlink(buf);
                }

                free(buf);
            }

            r = r2;
        }

        closedir(d);
    }

    if (!r)
        r = rmdir(path);

    return r;
}
#endif

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

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* Check inputs. */
    if (!filename)
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);

    LOG((1, "pioc_read_nc_decomp_int iosysid = %d filename = %s", iosysid, filename));

    /* Open the netCDF decomp file. */
    if ((ret = PIOc_open(iosysid, filename, NC_WRITE, &ncid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Read version attribute. */
    char version_in[PIO_MAX_NAME + 1];
    if ((ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_VERSION_ATT_NAME, version_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    LOG((3, "version_in = %s", version_in));
    if (version)
        strncpy(version, version_in, PIO_MAX_NAME + 1);

    /* Read order attribute. */
    char order_in[PIO_MAX_NAME + 1];
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

    /* Read attribute with the max map len. */
    int max_maplen_in;
    if ((ret = PIOc_get_att_int(ncid, NC_GLOBAL, DECOMP_MAX_MAPLEN_ATT_NAME, &max_maplen_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    LOG((3, "max_maplen_in = %d", max_maplen_in));
    if (max_maplen)
        *max_maplen = max_maplen_in;

    /* Read title attribute, if it is in the file. */
    char title_in[PIO_MAX_NAME + 1];
    ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_TITLE_ATT_NAME, title_in);
    if (ret == PIO_NOERR)
    {
        /* If the caller wants it, copy the title for them. */
        if (title)
            strncpy(title, title_in, PIO_MAX_NAME + 1);
    }
    else if (ret == PIO_ENOTATT)
    {
        /* No title attribute. */
        if (title)
            title[0] = '\0';
    }
    else
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Read history attribute, if it is in the file. */
    char history_in[PIO_MAX_NAME + 1];
    ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_HISTORY_ATT_NAME, history_in);
    if (ret == PIO_NOERR)
    {
        /* If the caller wants it, copy the history for them. */
        if (history)
            strncpy(history, history_in, PIO_MAX_NAME + 1);
    }
    else if (ret == PIO_ENOTATT)
    {
        /* No history attribute. */
        if (history)
            history[0] = '\0';
    }
    else
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Read source attribute. */
    char source_in[PIO_MAX_NAME + 1];
    if ((ret = PIOc_get_att_text(ncid, NC_GLOBAL, DECOMP_SOURCE_ATT_NAME, source_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (source)
        strncpy(source, source_in, PIO_MAX_NAME + 1);

    /* Read dimension for the dimensions in the data. (Example: for 4D
     * data we will need to store 4 dimension IDs.) */
    int dim_dimid;
    PIO_Offset ndims_in;
    if ((ret = PIOc_inq_dimid(ncid, DECOMP_DIM_DIM, &dim_dimid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if ((ret = PIOc_inq_dim(ncid, dim_dimid, NULL, &ndims_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (ndims)
        *ndims = ndims_in;

    /* Read the global sizes of the array. */
    int gsize_varid;
    int global_dimlen_in[ndims_in];
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

    /* Read dimension for tasks. If we have 4 tasks, we need to store
     * an array of length 4 with the size of the local array on each
     * task. */
    int task_dimid;
    PIO_Offset num_tasks_in;
    if ((ret = PIOc_inq_dimid(ncid, DECOMP_TASK_DIM_NAME, &task_dimid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if ((ret = PIOc_inq_dim(ncid, task_dimid, NULL, &num_tasks_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (num_tasks)
        *num_tasks = num_tasks_in;

    /* Read the length if the local array on each task. */
    int maplen_varid;
    int task_maplen_in[num_tasks_in];
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

    /* Read the map. */
    int map_varid;
    int map_in[num_tasks_in][max_maplen_in];
    if ((ret = PIOc_inq_varid(ncid, DECOMP_MAP_VAR_NAME, &map_varid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if ((ret = PIOc_get_var_int(ncid, map_varid, (int *)map_in)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
    if (map)
    {
        if (!(*map = malloc(num_tasks_in * max_maplen_in * sizeof(int))))
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        for (int t = 0; t < num_tasks_in; t++)
            for (int l = 0; l < max_maplen_in; l++)
                (*map)[t * max_maplen_in + l] = map_in[t][l];
    }

    /* Close the netCDF decomp file. */
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
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */
    int ierr = PIO_NOERR;              /* Return code from function calls. */

#ifdef TIMING
    GPTLstart("PIO:PIOc_createfile_int");
#endif
    /* Get the IO system info from the iosysid. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* User must provide valid input for these parameters. */
    if (!ncidp || !iotype || !filename || strlen(filename) > PIO_MAX_NAME)
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);

    /* A valid iotype must be specified. */
    if (!iotype_is_valid(*iotype))
        return pio_err(ios, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__);

    LOG((1, "PIOc_createfile iosysid = %d iotype = %d filename = %s mode = %d",
         iosysid, *iotype, filename, mode));

    /* Allocate space for the file info. */
    if (!(file = calloc(sizeof(file_desc_t), 1)))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);

    /* Fill in some file values. */
    file->fh = -1;
    strncpy(file->fname, filename, PIO_MAX_NAME);
    file->iosystem = ios;
    file->iotype = *iotype;
    file->buffer.ioid = -1;
    /*
    file->num_unlim_dimids = 0;
    file->unlim_dimids = NULL;
    */
    for (int i = 0; i < PIO_MAX_VARS; i++)
    {
        file->varlist[i].vname[0] = '\0';
        file->varlist[i].record = -1;
        file->varlist[i].request = NULL;
        file->varlist[i].nreqs = 0;
        file->varlist[i].fillvalue = NULL;
        file->varlist[i].pio_type = 0;
        file->varlist[i].type_size = 0;
        file->varlist[i].use_fill = 0;
        file->varlist[i].fillbuf = NULL;
    }
    file->mode = mode;

    /* Set to true if this task should participate in IO (only true for
     * one task with netcdf serial files. */
    if (file->iotype == PIO_IOTYPE_NETCDF4P || file->iotype == PIO_IOTYPE_PNETCDF ||
        ios->io_rank == 0)
        file->do_io = 1;

    LOG((2, "file->do_io = %d ios->async = %d", file->do_io, ios->async));

    for (int i = 0; i < PIO_IODESC_MAX_IDS; i++)
        file->iobuf[i] = NULL;

    /* If async is in use, and this is not an IO task, bcast the
     * parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_CREATE_FILE;
        size_t len = strlen(filename) + 1;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, len, filename, file->iotype, file->mode);
        if(ierr != PIO_NOERR)
        {
            LOG((1, "Sending async message, to create a file, failed"));
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__);
        }
    }

    /* ADIOS: assume all procs are also IO tasks */
#ifdef _ADIOS
    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        LOG((2, "Calling adios_open mode = %d", file->mode));
        /* 
         * Create a new ADIOS variable group, names the same as the
         * filename for lack of better solution here
         */
        int len = strlen(filename);
        file->filename = malloc(len + 3 + 3);
        if (file->filename == NULL)
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        sprintf(file->filename, "%s.bp", filename);

        ierr = PIO_NOERR;
        if (file->mode & PIO_NOCLOBBER) /* Check adios file/folder exists */
        {
            struct stat sf, sd;
            char *filefolder = malloc(strlen(file->filename) + 6);
            sprintf(filefolder, "%s.dir", file->filename);
            if (0 == stat(file->filename, &sf) || 0 == stat(filefolder, &sd))
                ierr = PIO_EEXIST;
            free(filefolder);
        }
        else
        {
            /* Delete directory filename.bp.dir if it exists */
            if (ios->union_rank == 0)
            {
                char bpdirname[PIO_MAX_NAME + 1];
                assert(len + 7 <= PIO_MAX_NAME);
                sprintf(bpdirname, "%s.bp.dir", filename);
                struct stat sd;
                if (0 == stat(bpdirname, &sd))
                    remove_directory(bpdirname);
            }

            /* Make sure that no task is trying to operate on the
             * directory while it is being deleted */
            if ((mpierr = MPI_Barrier(ios->union_comm)))
                return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
        }

        if (PIO_NOERR == ierr)
        {
            adios_declare_group(&file->adios_group, file->filename, NULL, adios_stat_default);

            int do_aggregate = (ios->num_comptasks != ios->num_iotasks);
            if (do_aggregate)
            {
                sprintf(file->transport, "%s", "MPI_AGGREGATE");
                sprintf(file->params, "num_aggregators=%d,random_offset=1,striping_count=1,have_metadata_file=0",
                        ios->num_iotasks);
            }
            else
            {
                int num_adios_io_tasks = ios->num_comptasks / 16;
                if (num_adios_io_tasks == 0)
                    num_adios_io_tasks = ios->num_comptasks;
                sprintf(file->transport, "%s", "MPI_AGGREGATE");
                sprintf(file->params, "num_aggregators=%d,random_offset=1,striping_count=1,have_metadata_file=0",
                        num_adios_io_tasks);
            }

            adios_select_method(file->adios_group, file->transport, file->params, "");
            ierr = adios_open(&file->adios_fh, file->filename, file->filename, "w", ios->union_comm);

            memset(file->dim_names, 0, sizeof(file->dim_names));

            file->num_dim_vars = 0;
            file->num_vars = 0;
            file->num_gattrs = 0;
            file->fillmode = NC_NOFILL;
            file->n_written_ioids = 0;

            if (ios->union_rank==0)
                file->adios_iomaster = MPI_ROOT;
            else
                file->adios_iomaster = MPI_PROC_NULL;

            /* Track attributes */
            file->num_attrs = 0;

            int64_t vid = adios_define_var(file->adios_group, "/__pio__/info/nproc", "", adios_integer, "", "", "");
            adios_write_byid(file->adios_fh, vid, &ios->num_uniontasks);
        }
    }
#endif
 
    /* If this task is in the IO component, do the IO. */
    if (ios->ioproc)
    {
#ifdef _NETCDF4
        /* All NetCDF4 files use the CDF5 file format by default,
         * - 64bit offset, 64bit data
         * However the NetCDF library does not allow setting the 
         * NC_64BIT_OFFSET or NC_64BIT_DATA flags for NetCDF4 types 
         * - this internal reset of flags is for user convenience */
        if ((file->iotype == PIO_IOTYPE_NETCDF4P) ||
            (file->iotype == PIO_IOTYPE_NETCDF4C))
        {
            LOG((2, "File create mode (before change) = %x",file->mode));
            if ((file->mode & NC_64BIT_OFFSET) == NC_64BIT_OFFSET)
            {
                file->mode &= ~NC_64BIT_OFFSET;
            }
            if ((file->mode & NC_64BIT_DATA) == NC_64BIT_DATA)
            {
                file->mode &= ~NC_64BIT_DATA;
            }
            LOG((2, "File create mode (after change) = %x",file->mode));
        }
#endif

        switch (file->iotype)
        {
#ifdef _NETCDF4
        case PIO_IOTYPE_NETCDF4P:
            file->mode = file->mode |  NC_MPIIO | NC_NETCDF4;
            LOG((2, "Calling nc_create_par io_comm = %d mode = %d fh = %d",
                 ios->io_comm, file->mode, file->fh));
            ierr = nc_create_par(filename, file->mode, ios->io_comm, ios->info, &file->fh);
            LOG((2, "nc_create_par returned %d file->fh = %d", ierr, file->fh));
            break;
        case PIO_IOTYPE_NETCDF4C:
            file->mode = file->mode | NC_NETCDF4;
#endif
#ifdef _NETCDF
        case PIO_IOTYPE_NETCDF:
            if (!ios->io_rank)
            {
                LOG((2, "Calling nc_create mode = %d", file->mode));
                ierr = nc_create(filename, file->mode, &file->fh);
            }
            break;
#endif
#ifdef _PNETCDF
        case PIO_IOTYPE_PNETCDF:
            LOG((2, "Calling ncmpi_create mode = %d", file->mode));
            if (ios->info == MPI_INFO_NULL)
                MPI_Info_create(&ios->info);

            /* Set some MPI-IO hints below */

            /* ROMIO will not perform data sieving for writes. Data sieving is
               designed for I/O patterns that read or write small, noncontiguous
               file regions. It does not help if the aggregated writes are always
               contiguous, covering the entire variables.
             */
            MPI_Info_set(ios->info, "romio_ds_write", "disable");

            /* Enable ROMIO's collective buffering for writes. Collective
               buffering, also called two-phase collective I/O, reorganizes
               data across processes to match data layout in file.
             */
            MPI_Info_set(ios->info, "romio_cb_write", "enable"); 

            /* Disable independent file operations. ROMIO will make an effort to
               avoid performing any file operation on non-aggregator processes.
             */
            MPI_Info_set(ios->info, "romio_no_indep_rw", "true"); 

            /* Set some PnetCDF I/O hints below */

            /* Do not align the starting file offsets of individual fixed-size
               variables. If applications use PnetCDF nonblocking APIs to
               aggregate write requests to multiple variables, then the best
               practice is to disable the variable alignment. This will prevent
               creating gaps in file space between two consecutive fixed-size
               variables and thus the writes to file system can be contiguous.
             */
            MPI_Info_set(ios->info, "nc_var_align_size", "1");

            /* Enable in-place byte swap on Little Endian architectures. With
               this option, PnetCDF performs byte swap on user I/O buffers
               whenever possible. This results in the least amount of internal
               memory usage. However, if an immutable user buffer is used,
               segmentation fault may occur when byte swap is performed on
               user buffer in place.
             */
            MPI_Info_set(ios->info, "nc_in_place_swap", "enable");

            /* Set the size of a temporal buffer to be allocated by PnetCDF
               internally to pack noncontiguous user write buffers supplied
               to the nonblocking requests into a contiguous space. On some
               systems, using noncontiguous user buffers in MPI collective
               write functions performs significantly worse than using
               contiguous buffers. This hint is supported by latest PnetCDF
               (version 1.11.0 and later).
               
               [More information]
               Noncontiguous write buffers are almost unavoidable:
               1) Each IO decomposition has its own writer buffer for a file
               2) PnetCDF might use noncontiguous helper buffers to perform
                  data type conversion

               Without this hint, we have seen hanging issues on Cori and
               Titan for some E3SM cases run with SUBSET rearranger. This
               hint is optional if BOX rearranger is used. 

               The default buffer size is 16 MiB in PnetCDF and we tentatively
               set it to 64 MiB. For E3SM production runs, if SUBSET rearranger
               is used, we might need an even larger buffer size in PnetCDF. For
               example, if 150 IO tasks are used to write a file of size 80 GiB,
               we should try a buffer size larger than 546 MiB.
             */
            MPI_Info_set(ios->info, "nc_ibuf_size", "67108864");

            ierr = ncmpi_create(ios->io_comm, filename, file->mode, ios->info, &file->fh);
            if (!ierr)
                ierr = ncmpi_buffer_attach(file->fh, pio_buffer_size_limit);
            break;
#endif
        }
    }

    ierr = check_netcdf(ios, NULL, ierr, __FILE__, __LINE__);
    /* If there was an error, free the memory we allocated and handle error. */
    if(ierr != PIO_NOERR){
        free(file);
#ifdef TIMING
        GPTLstop("PIO:PIOc_createfile_int");
#endif
        LOG((1, "PIOc_create_file_int failed, ierr = %d\n", ierr));
        return ierr;
    }

    /* Broadcast mode to all tasks. */
    if ((mpierr = MPI_Bcast(&file->mode, 1, MPI_INT, ios->ioroot, ios->union_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    /* This flag is implied by netcdf create functions but we need
       to know if its set. */
    file->mode = file->mode | PIO_WRITE;

    /* Add the struct with this files info to the global list of
     * open files. */
    MPI_Comm comm = MPI_COMM_NULL;
    if(ios->async)
    {
        /* For asynchronous I/O service, since file ids are passed across
         * disjoint comms we need it to be unique across the union comm
         */
        comm = ios->union_comm;
    }
    *ncidp = pio_add_to_file_list(file, comm);

    LOG((2, "Created file %s file->fh = %d file->pio_ncid = %d", filename,
         file->fh, file->pio_ncid));

#ifdef TIMING
    GPTLstop("PIO:PIOc_createfile_int");
#endif
    return ierr;
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
#ifdef _NETCDF4
    int nunlimdims; /* Number of unlimited dims in file. */
    int nvars;       /* Number of vars in file. */
    int ierr;        /* Return code. */

    /* Are there 2 or more unlimited dims in this file? */
    if ((ierr = nc_inq_unlimdims(ncid, &nunlimdims, NULL)))
        return ierr;
    if (nunlimdims < 2)
        return PIO_NOERR;

    /* How many vars in file? */
    if ((ierr = nc_inq_nvars(ncid, &nvars)))
        return ierr;

    /* Check each var. */
    for (int v = 0; v < nvars && !ierr; v++)
    {
        int nvardims;
        if ((ierr = nc_inq_varndims(ncid, v, &nvardims)))
            return ierr;
        int vardimid[nvardims];
        if ((ierr = nc_inq_vardimid(ncid, v, vardimid)))
            return ierr;

        /* Check all var dimensions, except the first. If we find
         * unlimited, that's a problem. */
        for (int vd = 1; vd < nvardims; vd++)
        {
            size_t dimlen;
            if ((ierr = nc_inq_dimlen(ncid, vardimid[vd], &dimlen)))
                return ierr;
            if (dimlen == NC_UNLIMITED)
            {
                nc_close(ncid);
                return PIO_EINVAL;
            }
        }
    }
#endif /* _NETCDF4 */
    
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
    /* Assume it's not valid. */
    int ret = 0;

    /* Some builds include netCDF. */
#ifdef _NETCDF
    if (iotype == PIO_IOTYPE_NETCDF)
        ret++;
#endif /* _NETCDF */

    /* Some builds include netCDF-4. */
#ifdef _NETCDF4
    if (iotype == PIO_IOTYPE_NETCDF4C || iotype == PIO_IOTYPE_NETCDF4P)
        ret++;
#endif /* _NETCDF4 */

    /* Some builds include pnetcdf. */
#ifdef _PNETCDF
    if (iotype == PIO_IOTYPE_PNETCDF)
        ret++;
#endif /* _PNETCDF */

#ifdef _ADIOS
    if (iotype == PIO_IOTYPE_ADIOS)
        ret++;
#endif

    return ret;
}

#ifdef _ADIOS
enum ADIOS_DATATYPES PIOc_get_adios_type(nc_type xtype)
{
    enum ADIOS_DATATYPES t;
    switch (xtype)
    {
    case NC_BYTE:
        t = adios_byte;
        break;
    case NC_CHAR:
        t = adios_byte;
        break;
    case NC_SHORT:
        t = adios_short;
        break;
    case NC_INT:
        t = adios_integer;
        break;
    case NC_FLOAT:
        t = adios_real;
        break;
    case NC_DOUBLE:
        t = adios_double;
        break;
    case NC_UBYTE:
        t = adios_unsigned_byte;
        break;
    case NC_USHORT:
        t = adios_unsigned_short;
        break;
    case NC_UINT:
        t = adios_unsigned_integer;
        break;
    case NC_INT64:
        t = adios_long;
        break;
    case NC_UINT64:
        t = adios_unsigned_long;
        break;
    case NC_STRING:
        t = adios_string;
        break;
    default:
        t = adios_byte;
        break;
    }

    return t;
}

nc_type PIOc_get_nctype_from_adios_type(enum ADIOS_DATATYPES atype)
{
    nc_type t;
    switch (atype)
    {
    case adios_byte:
        t = NC_BYTE;
        break;
    case adios_short:
        t = NC_SHORT;
        break;
    case adios_integer:
        t = NC_INT;
        break;
    case adios_real:
        t = NC_FLOAT;
        break;
    case adios_double:
        t = NC_DOUBLE;
        break;
    case adios_unsigned_byte:
        t = NC_UBYTE;
        break;
    case adios_unsigned_short:
        t = NC_USHORT;
        break;
    case adios_unsigned_integer:
        t = NC_UINT;
        break;
    case adios_long:
        t = NC_INT64;
        break;
    case adios_unsigned_long:
        t = NC_UINT64;
        break;
    case adios_string:
        t = NC_CHAR;
        break;
    default:
        t = NC_BYTE;
        break;
    }

    return t;
}

#ifndef strdup
char *strdup(const char *str)
{
    int n = strlen(str) + 1;
    char *dup = (char*)malloc(n);
    if (dup)
    {
        strcpy(dup, str);
    }

    return dup;
}
#endif

#endif /* _ADIOS */
