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

/**
 * Open an existing file using PIO library. This is an internal
 * function. Depending on the value of the retry parameter, a failed
 * open operation will be handled differently. If retry is non-zero,
 * then a failed attempt to open a file with netCDF-4 (serial or
 * parallel), or parallel-netcdf will be followed by an attempt to
 * open the file as a serial classic netCDF file. This is an important
 * feature to some NCAR users. The functionality is exposed to the
 * user as PIOc_openfile() (which does the retry), and PIOc_open()
 * (which does not do the retry).
 *
 * Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid: A defined pio system descriptor (input)
 * @param ncidp: A pio file descriptor (output)
 * @param iotype: A pio output format (input)
 * @param filename: The filename to open
 * @param mode: The netcdf mode for the open operation
 * @param retry: non-zero to automatically retry with netCDF serial
 * classic.
 *
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_openfile_retry(int iosysid, int *ncidp, int *iotype, const char *filename,
                        int mode, int retry)
{
    iosystem_desc_t *ios;      /* Pointer to io system information. */
    file_desc_t *file;         /* Pointer to file information. */
    int imode;                 /* Internal mode val for netcdf4 file open. */
    int mpierr = MPI_SUCCESS, mpierr2;  /** Return code from MPI function codes. */
    int ierr = PIO_NOERR;      /* Return code from function calls. */

    /* Get the IO system info from the iosysid. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* User must provide valid input for these parameters. */
    if (!ncidp || !iotype || !filename)
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);

    /* A valid iotype must be specified. */
    if (!iotype_is_valid(*iotype))
        return pio_err(ios, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__);

    LOG((2, "PIOc_openfile_retry iosysid = %d iotype = %d filename = %s mode = %d retry = %d",
         iosysid, *iotype, filename, mode, retry));

    /* Allocate space for the file info. */
    if (!(file = calloc(sizeof(*file), 1)))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);

    /* Fill in some file values. */
    file->fh = -1;
    strncpy(file->fname, filename, PIO_MAX_NAME);
    file->iotype = *iotype;
#ifdef _ADIOS
    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
#ifdef _PNETCDF
        file->iotype = PIO_IOTYPE_PNETCDF;
#else
#ifdef _NETCDF4
#ifdef _MPISERIAL
        file->iotype = PIO_IOTYPE_NETCDF4C;
#else
        file->iotype = PIO_IOTYPE_NETCDF4P;
#endif
#endif
#endif
    }
#endif
    file->iosystem = ios;
    file->mode = mode;
    /*
    file->num_unlim_dimids = 0;
    file->unlim_dimids = NULL;
    */

    for (int i = 0; i < PIO_MAX_VARS; i++)
    {
        file->varlist[i].vname[0] = '\0';
        file->varlist[i].record = -1;
    }

    /* Set to true if this task should participate in IO (only true
     * for one task with netcdf serial files. */
    if (file->iotype == PIO_IOTYPE_NETCDF4P || file->iotype == PIO_IOTYPE_PNETCDF ||
        ios->io_rank == 0)
        file->do_io = 1;

    for (int i = 0; i < PIO_IODESC_MAX_IDS; i++)
        file->iobuf[i] = NULL;

    /* If async is in use, bcast the parameters from compute to I/O procs. */
    if(ios->async)
    {
        int len = strlen(filename) + 1;
        PIO_SEND_ASYNC_MSG(ios, PIO_MSG_OPEN_FILE, &ierr, len, filename, file->iotype, file->mode);
        if(ierr != PIO_NOERR)
        {
            return pio_err(ios, file, ierr, __FILE__, __LINE__);
        }
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        switch (file->iotype)
        {
#ifdef _NETCDF4

        case PIO_IOTYPE_NETCDF4P:
#ifdef _MPISERIAL
            ierr = nc_open(filename, file->mode, &file->fh);
#else
            imode = file->mode |  NC_MPIIO;
            if ((ierr = nc_open_par(filename, imode, ios->io_comm, ios->info, &file->fh)))
                break;
            file->mode = imode;

            /* Check the vars for valid use of unlim dims. */
            if ((ierr = check_unlim_use(file->fh)))
                break;
            LOG((2, "PIOc_openfile_retry:nc_open_par filename = %s mode = %d imode = %d ierr = %d",
                 filename, file->mode, imode, ierr));
#endif
            break;

        case PIO_IOTYPE_NETCDF4C:
            if (ios->io_rank == 0)
            {
                imode = file->mode | NC_NETCDF4;
                if ((ierr = nc_open(filename, imode, &file->fh)))
                    break;
                file->mode = imode;
                /* Check the vars for valid use of unlim dims. */
                if ((ierr = check_unlim_use(file->fh)))
                    break;                    
            }
            break;
#endif /* _NETCDF4 */

#ifdef _NETCDF
        case PIO_IOTYPE_NETCDF:
            if (ios->io_rank == 0)
                ierr = nc_open(filename, file->mode, &file->fh);
            break;
#endif /* _NETCDF */

#ifdef _PNETCDF
        case PIO_IOTYPE_PNETCDF:
            ierr = ncmpi_open(ios->io_comm, filename, file->mode, ios->info, &file->fh);

            // This should only be done with a file opened to append
            if (ierr == PIO_NOERR && (file->mode & PIO_WRITE))
            {
                if (ios->iomaster == MPI_ROOT)
                    LOG((2, "%d Setting IO buffer %ld", __LINE__, pio_buffer_size_limit));
                ierr = ncmpi_buffer_attach(file->fh, pio_buffer_size_limit);
            }
            LOG((2, "ncmpi_open(%s) : fd = %d", filename, file->fh));
            break;
#endif

        default:
            free(file);
            return pio_err(ios, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__);
        }

        /* If the caller requested a retry, and we failed to open a
           file due to an incompatible type of NetCDF, try it once
           with just plain old basic NetCDF. */
        if (retry)
        {
#ifdef _NETCDF
            LOG((2, "retry error code ierr = %d io_rank %d", ierr, ios->io_rank));
            /* Bcast error code from io rank 0 to all io procs */
            mpierr = MPI_Bcast(&ierr, 1, MPI_INT, 0, ios->io_comm);
            if(mpierr != MPI_SUCCESS){
                return check_mpi(NULL, file, ierr, __FILE__, __LINE__);
            }
            if ((ierr != NC_NOERR) && (file->iotype != PIO_IOTYPE_NETCDF))
            {
                if (ios->iomaster == MPI_ROOT)
                    printf("PIO2 pio_file.c retry NETCDF\n");

                /* reset ierr on all tasks */
                ierr = PIO_NOERR;

                /* reset file markers for NETCDF on all tasks */
                file->iotype = PIO_IOTYPE_NETCDF;

                /* open netcdf file serially on main task */
                if (ios->io_rank == 0)
                {
                    ierr = nc_open(filename, file->mode, &file->fh);
                    if(ierr == NC_NOERR)
                    {
                        printf("PIO: Opening file (%s) with iotype=%d failed. Switching iotype to PIO_IOTYPE_NETCDF\n", filename, *iotype);
                    }
                }
                else
                    file->do_io = 0;
            }
            LOG((2, "retry nc_open(%s) : fd = %d, iotype = %d, do_io = %d, ierr = %d",
                 filename, file->fh, file->iotype, file->do_io, ierr));
#endif /* _NETCDF */
        }
    }

    ierr = check_netcdf(ios, NULL, ierr, __FILE__, __LINE__);
    /* If there was an error, free allocated memory and deal with the error. */
    if(ierr != PIO_NOERR){
        free(file);
        LOG((1, "PIOc_openfile_retry failed, ierr = %d", ierr));
        return ierr;
    }

    /* Broadcast open mode to all tasks. */
    if ((mpierr = MPI_Bcast(&file->mode, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    /* Add this file to the list of currently open files. */
    MPI_Comm comm = MPI_COMM_NULL;
    if(ios->async)
    {
        /* For asynchronous I/O service, since file ids are passed across
         * disjoint comms we need it to be unique across the union comm
         */
        comm = ios->union_comm;
    }
    *ncidp = pio_add_to_file_list(file, comm);

    LOG((2, "Opened file %s file->pio_ncid = %d file->fh = %d ierr = %d",
         filename, file->pio_ncid, file->fh, ierr));

    /* Check if the file has unlimited dimensions */
    if(!ios->async || !ios->ioproc)
    {
        ierr = PIOc_inq_unlimdims(*ncidp, &(file->num_unlim_dimids), NULL);
        if(ierr != PIO_NOERR)
        {
            return pio_err(ios, file, ierr, __FILE__, __LINE__);
        }
        if(file->num_unlim_dimids > 0)
        {
            file->unlim_dimids = (int *)malloc(file->num_unlim_dimids * sizeof(int));
            if(!file->unlim_dimids)
            {
                return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
            }
            ierr = PIOc_inq_unlimdims(*ncidp, NULL, file->unlim_dimids);
            if(ierr != PIO_NOERR)
            {
                return pio_err(ios, file, ierr, __FILE__, __LINE__);
            }
        }
        LOG((3, "File has %d unlimited dimensions", file->num_unlim_dimids));
    }

    return ierr;
}

/**
 * Internal function used when opening an existing file. This function
 * is called by PIOc_openfile() and PIOc_openfile2(). It opens the
 * file and then learns some things about the metadata in that file.
 *
 * Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid: A defined pio system descriptor (input)
 * @param ncidp: A pio file descriptor (output)
 * @param iotype: A pio output format (input)
 * @param filename: The filename to open
 * @param mode: The netcdf mode for the open operation
 * @param retry: non-zero to automatically retry with netCDF serial
 * classic.
 *
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Ed Hartnett
 */
int openfile_int(int iosysid, int *ncidp, int *iotype, const char *filename,
                 int mode, int retry)
{
    int nvars;             /* The number of vars in the file. */
    int nunlimdim;         /* The number of unlimited dimensions. */
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    /* Get the IO system info from the iosysid. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);
    
    /* Open the file. */
    if ((ierr = PIOc_openfile_retry(iosysid, ncidp, iotype, filename, mode, retry)))
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Internal function to provide inq_type function for pnetcdf.
 *
 * @param ncid ignored because pnetcdf does not have user-defined
 * types.
 * @param xtype type to learn about.
 * @param name pointer that gets name of type. Ignored if NULL.
 * @param sizep pointer that gets size of type. Ignored if NULL.
 * @returns 0 on success, error code otherwise.
 */
int pioc_pnetcdf_inq_type(int ncid, nc_type xtype, char *name,
                          PIO_Offset *sizep)
{
    int typelen;

    switch (xtype)
    {
    case NC_UBYTE:
    case NC_BYTE:
    case NC_CHAR:
        typelen = 1;
        break;
    case NC_SHORT:
    case NC_USHORT:
        typelen = 2;
        break;
    case NC_UINT:
    case NC_INT:
    case NC_FLOAT:
        typelen = 4;
        break;
    case NC_UINT64:
    case NC_INT64:
    case NC_DOUBLE:
        typelen = 8;
        break;
    default:
        return PIO_EBADTYPE;
    }

    /* If pointers were supplied, copy results. */
    if (sizep)
        *sizep = typelen;
    if (name)
        strcpy(name, "some type");

    return PIO_NOERR;
}

/**
 * This is an internal function that handles both PIOc_enddef and
 * PIOc_redef.
 *
 * @param ncid the ncid of the file to enddef or redef
 * @param is_enddef set to non-zero for enddef, 0 for redef.
 * @returns PIO_NOERR on success, error code on failure. */
int pioc_change_def(int ncid, int is_enddef)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI functions. */

    LOG((2, "pioc_change_def ncid = %d is_enddef = %d", ncid, is_enddef));

    /* Find the info about this file. When I check the return code
     * here, some tests fail. ???*/
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = is_enddef ? PIO_MSG_ENDDEF : PIO_MSG_REDEF;
        
        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid);
        if(ierr != PIO_NOERR)
        {
            LOG((1, "Error sending async msg for PIO_MSG_ENDDEF/PIO_MSG_REDEF"));
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__);
        }
    }

    /* If this is an IO task, then call the netCDF function. */
    LOG((3, "pioc_change_def ios->ioproc = %d", ios->ioproc));
    if (ios->ioproc)
    {
        LOG((3, "pioc_change_def calling netcdf function file->fh = %d file->do_io = %d iotype = %d",
             file->fh, file->do_io, file->iotype));
#ifdef _PNETCDF
        if (file->iotype == PIO_IOTYPE_PNETCDF)
        {
            if (is_enddef)
                ierr = ncmpi_enddef(file->fh);
            else
                ierr = ncmpi_redef(file->fh);
        }
#endif /* _PNETCDF */
#ifdef _NETCDF
        if (file->iotype != PIO_IOTYPE_PNETCDF && file->iotype != PIO_IOTYPE_ADIOS && file->do_io)
        {
            if (is_enddef)
            {
                LOG((3, "pioc_change_def calling nc_enddef file->fh = %d", file->fh));
                ierr = nc_enddef(file->fh);
            }
            else
                ierr = nc_redef(file->fh);
        }
#endif /* _NETCDF */
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
      LOG((1, "pioc_change_def failed, ierr = %d", ierr));
      return ierr;
    }
    LOG((3, "pioc_change_def succeeded"));

    return ierr;
}

/**
 * Internal function to compare rearranger flow control options.
 *
 * @param opt pointer to rearranger flow control options to compare.
 * @param exp_opt pointer to rearranger flow control options with
 * expected values.
 * @return true if values in opt == values in exp_opt, false
 * otherwise.
 */
bool cmp_rearr_comm_fc_opts(const rearr_comm_fc_opt_t *opt,
                            const rearr_comm_fc_opt_t *exp_opt)
{
    bool is_same = true;

    assert(opt && exp_opt);

    if (opt->hs != exp_opt->hs)
    {
        LOG((1, "Warning rearranger hs = %s, expected = %s",
             opt->hs ? "TRUE" : "FALSE", exp_opt->hs ? "TRUE" : "FALSE"));
        is_same = false;
    }

    if (opt->isend != exp_opt->isend)
    {
        LOG((1, "Warning rearranger isend = %s, expected = %s",
             opt->isend ? "TRUE" : "FALSE", exp_opt->isend ? "TRUE" : "FALSE"));
        is_same = false;
    }

    if (opt->max_pend_req != exp_opt->max_pend_req)
    {
        LOG((1, "Warning rearranger max_pend_req = %d, expected = %d",
             opt->max_pend_req, exp_opt->max_pend_req));
        is_same = false;
    }

    return is_same;
}

/**
 * Internal function to compare rearranger options.
 *
 * @param rearr_opts pointer to rearranger options to compare
 * @param exp_rearr_opts pointer to rearranger options with the
 * expected value
 * @return true if values in rearr_opts == values in exp_rearr_opts
 * false otherwise
 */
bool cmp_rearr_opts(const rearr_opt_t *rearr_opts, const rearr_opt_t *exp_rearr_opts)
{
    bool is_same = true;

    assert(rearr_opts && exp_rearr_opts);

    if (rearr_opts->comm_type != exp_rearr_opts->comm_type)
    {
        LOG((1, "Warning rearranger comm_type = %d, expected = %d. ", rearr_opts->comm_type,
             exp_rearr_opts->comm_type));
        is_same = false;
    }

    if (rearr_opts->fcd != exp_rearr_opts->fcd)
    {
        LOG((1, "Warning rearranger fcd = %d, expected = %d. ", rearr_opts->fcd,
             exp_rearr_opts->fcd));
        is_same = false;
    }

    is_same = is_same && cmp_rearr_comm_fc_opts(&(rearr_opts->comp2io),
                                                &(exp_rearr_opts->comp2io));
    is_same = is_same && cmp_rearr_comm_fc_opts(&(rearr_opts->io2comp),
                                                &(exp_rearr_opts->io2comp));

    return is_same;
}

/**
 * Internal function to reset rearranger opts in iosystem to valid values.
 * The only values reset here are options that are not set (or of interest)
 * to the user. e.g. Setting the io2comp/comp2io settings to defaults when
 * user chooses coll for rearrangement.
 * The old default for max pending requests was DEF_P2P_MAXREQ = 64.
 *
 * @param rearr_opt pointer to rearranger options
 * @return return error if the rearr_opt is invalid
 */
int check_and_reset_rearr_opts(rearr_opt_t *rearr_opt)
{
    /* Disable handshake/isend and set max_pend_req to unlimited */
    const rearr_comm_fc_opt_t def_comm_nofc_opts =
        { false, false, PIO_REARR_COMM_UNLIMITED_PEND_REQ };
    /* Disable handshake /isend and set max_pend_req = 0 to turn off throttling */
    const rearr_comm_fc_opt_t def_coll_comm_fc_opts = { false, false, 0 };
    const rearr_opt_t def_coll_rearr_opts = {
        PIO_REARR_COMM_COLL,
        PIO_REARR_COMM_FC_2D_DISABLE,
        def_coll_comm_fc_opts,
        def_coll_comm_fc_opts
    };

    assert(rearr_opt);

    /* Reset to defaults, if needed (user did not set it correctly) */
    if (rearr_opt->comm_type == PIO_REARR_COMM_COLL)
    {
        /* Compare and log the user and default rearr opts for coll. */
        cmp_rearr_opts(rearr_opt, &def_coll_rearr_opts);
        /* Hard reset flow control options. */
        *rearr_opt = def_coll_rearr_opts;
    }
    else if (rearr_opt->comm_type == PIO_REARR_COMM_P2P)
    {
        if (rearr_opt->fcd == PIO_REARR_COMM_FC_2D_DISABLE)
        {
            /* Compare and log user and default opts. */
            cmp_rearr_comm_fc_opts(&(rearr_opt->comp2io),
                                   &def_comm_nofc_opts);
            cmp_rearr_comm_fc_opts(&(rearr_opt->io2comp),
                                   &def_comm_nofc_opts);
            /* Hard reset flow control opts to defaults. */
            rearr_opt->comp2io = def_comm_nofc_opts;
            rearr_opt->io2comp = def_comm_nofc_opts;
        }
        else if (rearr_opt->fcd == PIO_REARR_COMM_FC_1D_COMP2IO)
        {
            /* Compare and log user and default opts. */
            cmp_rearr_comm_fc_opts(&(rearr_opt->io2comp),
                                   &def_comm_nofc_opts);
            /* Hard reset io2comp dir to defaults. */
            rearr_opt->io2comp = def_comm_nofc_opts;
        }
        else if (rearr_opt->fcd == PIO_REARR_COMM_FC_1D_IO2COMP)
        {
            /* Compare and log user and default opts. */
            cmp_rearr_comm_fc_opts(&(rearr_opt->comp2io),
                                   &def_comm_nofc_opts);
            /* Hard reset comp2io dir to defaults. */
            rearr_opt->comp2io = def_comm_nofc_opts;
        }
        else
        {
            if (rearr_opt->fcd != PIO_REARR_COMM_FC_2D_ENABLE)
                return PIO_EINVAL;

            /* Don't reset if flow control is enabled in both directions
             * by user. */
        }
    }
    else
    {
        return PIO_EINVAL;
    }

    if (( (rearr_opt->comp2io.max_pend_req !=
            PIO_REARR_COMM_UNLIMITED_PEND_REQ) &&
          (rearr_opt->comp2io.max_pend_req < 0)  ) ||
        ( (rearr_opt->io2comp.max_pend_req !=
            PIO_REARR_COMM_UNLIMITED_PEND_REQ) &&
          (rearr_opt->io2comp.max_pend_req < 0)  ))
        return PIO_EINVAL;

    return PIO_NOERR;
}

/**
 * Set the rearranger options associated with an iosystem
 *
 * @param comm_type Type of communication (pt2pt/coll) used
 * by the rearranger. See PIO_REARR_COMM_TYPE for more detail.
 * Possible values are :
 * PIO_REARR_COMM_P2P (Point to point communication)
 * PIO_REARR_COMM_COLL (Collective communication)
 * @param fcd Flow control direction for the rearranger.
 * See PIO_REARR_COMM_FC_DIR for more detail.
 * Possible values are :
 * PIO_REARR_COMM_FC_2D_ENABLE : Enable flow control from
 * compute processes to io processes and vice versa
 * PIO_REARR_COMM_FC_1D_COMP2IO : Enable flow control from
 * compute processes to io processes (only)
 * PIO_REARR_COMM_FC_1D_IO2COMP : Enable flow control from
 * io processes to compute processes (only)
 * PIO_REARR_COMM_FC_2D_DISABLE : Disable flow control from
 * compute processes to io processes and vice versa.
 * @param enable_hs_c2i Enable handshake while rearranging
 * data, from compute to io processes
 * @param enable_isend_c2i Enable isends while rearranging
 * data, from compute to io processes
 * @param max_pend_req_c2i Maximum pending requests during
 * data rearragment from compute processes to io processes
 * @param enable_hs_i2c Enable handshake while rearranging
 * data, from io to compute processes
 * @param enable_isend_i2c Enable isends while rearranging
 * data, from io to compute processes
 * @param max_pend_req_i2c Maximum pending requests during
 * data rearragment from io processes to compute processes
 * @param iosysidp index of the defined system descriptor
 * @return 0 on success, otherwise a PIO error code.
 */
int PIOc_set_rearr_opts(int iosysid, int comm_type, int fcd, bool enable_hs_c2i,
                        bool enable_isend_c2i, int max_pend_req_c2i,
                        bool enable_hs_i2c, bool enable_isend_i2c,
                        int max_pend_req_i2c)
{
    iosystem_desc_t *ios;
    int ret = PIO_NOERR;
    rearr_opt_t user_rearr_opts = {
        comm_type, fcd,
        {enable_hs_c2i,enable_isend_c2i, max_pend_req_c2i},
        {enable_hs_i2c, enable_isend_i2c, max_pend_req_i2c}
    };

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* Perform sanity checks on the user supplied values and reset 
     * values not set (or of no interest) by the user 
     */
    ret = check_and_reset_rearr_opts(&user_rearr_opts);
    if (ret != PIO_NOERR)
        return ret;

    /* Set the options. */
    ios->rearr_opts = user_rearr_opts;

    return ret;
}

/* Calculate and cache the variable record size 
 * for the variable corresponding to varid
 * Note: Since this function calls many PIOc_* functions
 * only compute procs should call this function for async
 * i/o calls
 * */
int calc_var_rec_sz(int ncid, int varid)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ndims;
    nc_type vtype;
    PIO_Offset vtype_sz = 0;
    int ierr = PIO_NOERR, mpierr = MPI_SUCCESS;

    ierr = pio_get_file(ncid, &file);
    if(ierr != PIO_NOERR)
    {
        LOG((1, "Unable to get file corresponding to ncid = %d", ncid));
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);
    }
    ios = file->iosystem;
    assert(ios != NULL);

    /* Async io is still under development and write/read darrays need to
     * be implemented correctly before we remove the check below
     */
    if(ios->async)
    {
        LOG((1, "WARNING: Cannot calculate record size (not supported for async)"));
        return PIO_NOERR;
    }

    /* Calculate and cache the size of a single record/timestep */
    ierr = PIOc_inq_var(ncid, varid, NULL, 0, &vtype, &ndims, NULL, NULL);
    if(ierr != PIO_NOERR)
    {
        LOG((1, "Unable to query ndims/type for var"));
        return pio_err(ios, file, ierr, __FILE__, __LINE__);
    }
    if(ndims > 0)
    {
        int dimids[ndims];
        PIO_Offset dimlen[ndims];

        ierr = PIOc_inq_type(ncid, vtype, NULL, &vtype_sz);
        if(ierr != PIO_NOERR)
        {
            LOG((1, "Unable to query type info"));
            return pio_err(ios, file, ierr, __FILE__, __LINE__);
        }

        ierr = PIOc_inq_vardimid(ncid, varid, dimids);
        if(ierr != PIO_NOERR)
        {
            LOG((1, "Unable to query dimids for var"));
            return pio_err(ios, file, ierr, __FILE__, __LINE__);
        }

        for(int i=0; i<ndims; i++)
        {
            bool is_rec_dim = false;
            /* For record variables check if dim is an unlimited
              * dimension. For record dims set dimlen = 1
              */ 
            if(file->varlist[varid].rec_var)
            {
                for(int j=0; j<file->num_unlim_dimids; j++)
                {
                    if(dimids[i] == file->unlim_dimids[j])
                    {
                        is_rec_dim = true;
                        dimlen[i] = 1;
                        break;
                    }
                }
            }
            if(!is_rec_dim)
            {
                ierr = PIOc_inq_dim(ncid, dimids[i], NULL, &(dimlen[i]));
                if(ierr != PIO_NOERR)
                {
                    LOG((1, "Unable to query dims"));
                    return pio_err(ios, file, ierr, __FILE__, __LINE__);
                }
            }
            file->varlist[varid].vrsize = 
              ((file->varlist[varid].vrsize) ? file->varlist[varid].vrsize : 1)
              * dimlen[i]; 
        }
    }
    mpierr = MPI_Bcast(&(file->varlist[varid].vrsize), 1, MPI_OFFSET,
                        ios->ioroot, ios->my_comm);
    if(mpierr != MPI_SUCCESS)
    {
        LOG((1, "Unable to bcast vrsize"));
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }
    return ierr;
}

/* Get a description of the variable 
 * @param ncid PIO id for the file
 * @param varid PIO id for the variable
 * @param Any user string that needs to be prepended to the variable
 * description, can optionally be NULL
 * Returns a string that describes the variable associated with varid
 * - The returned string should be copied by the user since the
 *   contents of the buffer returned can change in the next call
 *   to this function.
 * */
const char *get_var_desc_str(int ncid, int varid, const char *desc_prefix)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    static const char EMPTY_STR[] = "";
    int ierr = PIO_NOERR;

    ierr = pio_get_file(ncid, &file);
    if(ierr != PIO_NOERR)
    {
        LOG((1, "Unable to get file corresponding to ncid = %d", ncid));
        return EMPTY_STR;
    }
    ios = file->iosystem;
    assert(ios != NULL);

    snprintf(file->varlist[varid].vdesc, PIO_MAX_NAME,
              "%s %s %s %llu %llu %llu %llu %llu",
              (desc_prefix)?desc_prefix:"",
              file->varlist[varid].vname,
              file->fname,
              (unsigned long long int)file->varlist[varid].vrsize,
              (unsigned long long int)file->varlist[varid].rb_pend,
              (unsigned long long int)file->varlist[varid].wb_pend,
              (unsigned long long int)file->rb_pend,
              (unsigned long long int)file->wb_pend
              );

    return file->varlist[varid].vdesc;
}

/* A ROMIO patch from PnetCDF's E3SM-IO benchmark program
 * https://github.com/Parallel-NetCDF/E3SM-IO/blob/master/romio_patch.c */
#if defined(MPICH_NUMVERSION) && (MPICH_NUMVERSION < 30300000)
/*
 *
 *   Copyright (C) 2014 UChicgo/Argonne, LLC.
 *   See COPYRIGHT notice in top-level directory.
 */

/* utility function for creating large contiguous types: algorithim from BigMPI
 * https://github.com/jeffhammond/BigMPI */

static int type_create_contiguous_x(MPI_Count count, MPI_Datatype oldtype, MPI_Datatype * newtype)
{
    /* to make 'count' fit MPI-3 type processing routines (which take integer
     * counts), we construct a type consisting of N INT_MAX chunks followed by
     * a remainder.  e.g for a count of 4000000000 bytes you would end up with
     * one 2147483647-byte chunk followed immediately by a 1852516353-byte
     * chunk */
    MPI_Datatype chunks, remainder;
    MPI_Aint lb, extent, disps[2];
    int blocklens[2];
    MPI_Datatype types[2];

    /* truly stupendously large counts will overflow an integer with this math,
     * but that is a problem for a few decades from now.  Sorry, few decades
     * from now! */
    assert(count / INT_MAX == (int) (count / INT_MAX));
    int c = (int) (count / INT_MAX);    /* OK to cast until 'count' is 256 bits */
    int r = count % INT_MAX;

    MPI_Type_vector(c, INT_MAX, INT_MAX, oldtype, &chunks);
    MPI_Type_contiguous(r, oldtype, &remainder);

    MPI_Type_get_extent(oldtype, &lb, &extent);

    blocklens[0] = 1;
    blocklens[1] = 1;
    disps[0] = 0;
    disps[1] = c * extent * INT_MAX;
    types[0] = chunks;
    types[1] = remainder;

    MPI_Type_create_struct(2, blocklens, disps, types, newtype);

    MPI_Type_free(&chunks);
    MPI_Type_free(&remainder);

    return MPI_SUCCESS;
}

/* like MPI_Type_create_hindexed, except array_of_lengths can be a larger datatype.
 *
 * Hindexed provides 'count' pairs of (displacement, length), but what if
 * length is longer than an integer?  We will create 'count' types, using
 * contig if length is small enough, or something more complex if not */

int __wrap_ADIOI_Type_create_hindexed_x(int count,
                                 const MPI_Count array_of_blocklengths[],
                                 const MPI_Aint array_of_displacements[],
                                 MPI_Datatype oldtype, MPI_Datatype * newtype)
{
    int i, ret;
    MPI_Datatype *types;
    int *blocklens;
    int is_big = 0;

    types = (MPI_Datatype*) malloc(count * sizeof(MPI_Datatype));
    blocklens = (int*) malloc(count * sizeof(int));

    /* squashing two loops into one.
     * - Look in the array_of_blocklengths for any large values
     * - convert MPI_Count items (if they are not too big) into int-sized items
     * after this loop we will know if we can use MPI_type_hindexed or if we
     * need a more complicated BigMPI-style struct-of-chunks.
     *
     * Why not use the struct-of-chunks in all cases?  HDF5 reported a bug,
     * which I have not yet precicesly nailed down, but appears to have
     * something to do with struct-of-chunks when the chunks are small */

#ifdef USE_ORIGINAL_MPICH_3_2
    for(i=0; i<count; i++) {
        if (array_of_blocklengths[i] > INT_MAX) {
            blocklens[i] = 1;
            is_big=1;
            type_create_contiguous_x(array_of_blocklengths[i], oldtype,  &(types[i]));
        } else {
            /* OK to cast: checked for "bigness" above */
            blocklens[i] = (int)array_of_blocklengths[i];
            MPI_Type_contiguous(blocklens[i], oldtype, &(types[i]));
        }
    }

    if (is_big) {
        ret = MPI_Type_create_struct(count, blocklens, array_of_displacements,
                types, newtype);
    } else {
        ret = MPI_Type_create_hindexed(count, blocklens, array_of_displacements, oldtype, newtype);
    }
    for (i=0; i< count; i++)
        MPI_Type_free(&(types[i]));
#else
    /* See https://github.com/pmodels/mpich/pull/3089 */
    for (i = 0; i < count; i++) {
        if (array_of_blocklengths[i] > INT_MAX) {
            blocklens[i] = 1;
            is_big = 1;
            type_create_contiguous_x(array_of_blocklengths[i], oldtype, &(types[i]));
        } else {
            /* OK to cast: checked for "bigness" above */
            blocklens[i] = (int) array_of_blocklengths[i];
            types[i] = oldtype;
        }
    }

    if (is_big) {
        ret = MPI_Type_create_struct(count, blocklens, array_of_displacements, types, newtype);
        for (i = 0; i < count; i++)
            if (types[i] != oldtype)
                MPI_Type_free(&(types[i]));
    } else {
        ret = MPI_Type_create_hindexed(count, blocklens, array_of_displacements, oldtype, newtype);
    }
#endif
    free(types);
    free(blocklens);

    return ret;
}
#endif

