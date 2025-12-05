/**
 * @file
 * PIO File Handling
 */
#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#include "spio_io_summary.h"
#include <sys/stat.h>
#include <unistd.h>
#include <mutex>
#include <thread>
#include <chrono>

#ifdef _ADIOS2
#include "../../tools/adios2pio-nm/adios2pio-nm-lib-c.h"
#endif

#include "spio_async_utils.hpp"

/**
 * Open an existing file using PIO library.
 *
 * If the open fails, try again as netCDF serial before giving
 * up. Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * Note that the file is opened with default fill mode, NOFILL for
 * pnetcdf, and FILL for netCDF classic and netCDF-4 files.
 *
 * @param iosysid : A defined pio system descriptor (input)
 * @param ncidp : A pio file descriptor (output)
 * @param iotype : A pio output format (input)
 * @param filename : The filename to open
 * @param mode : The netcdf mode for the open operation
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_openfile_impl(int iosysid, int *ncidp, int *iotype, const char *filename,
                  int mode)
{
    return openfile_int(iosysid, ncidp, iotype, filename, mode, 1);
}

/**
 * Open an existing file using PIO library.
 *
 * This is like PIOc_openfile(), but if the open fails, this function
 * will not try to open again as netCDF serial before giving
 * up. Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * Note that the file is opened with default fill mode, NOFILL for
 * pnetcdf, and FILL for netCDF classic and netCDF-4 files.
 *
 * @param iosysid : A defined pio system descriptor (input)
 * @param ncidp : A pio file descriptor (output)
 * @param iotype : A pio output format (input)
 * @param filename : The filename to open
 * @param mode : The netcdf mode for the open operation
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Ed Hartnett
 */
int PIOc_openfile2_impl(int iosysid, int *ncidp, int *iotype, const char *filename,
                   int mode)
{
    return openfile_int(iosysid, ncidp, iotype, filename, mode, 0);
}

/**
 * Open an existing file using PIO library.
 *
 * Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid A defined pio system descriptor
 * @param path The filename to open
 * @param mode The netcdf mode for the open operation
 * @param ncidp pointer to int where ncid will go
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Ed Hartnett
 */
int PIOc_open_impl(int iosysid, const char *path, int mode, int *ncidp)
{
    int iotype;

    LOG((1, "PIOc_open iosysid = %d path = %s mode = %x", iosysid, path, mode));

    /* Set the default iotype. */
#ifdef _NETCDF
    iotype = PIO_IOTYPE_NETCDF;
#else /* Assume that _PNETCDF is defined. */
    iotype = PIO_IOTYPE_PNETCDF;
#endif

    /* Figure out the iotype. */
    if (mode & NC_NETCDF4)
    {
#ifdef _NETCDF4
        /* FIXME: Add logic to find NCZARR type */
        if (mode & NC_MPIIO || mode & NC_MPIPOSIX)
            iotype = PIO_IOTYPE_NETCDF4P;
        else
            iotype = PIO_IOTYPE_NETCDF4C;
#endif
    }
    else
    {
#ifdef _PNETCDF
        if (mode & NC_PNETCDF || mode & NC_MPIIO)
            iotype = PIO_IOTYPE_PNETCDF;
#endif
    }

    /* Open the file. If the open fails, do not retry as serial
     * netCDF. Just return the error code. */
    return PIOc_openfile_retry_impl(iosysid, ncidp, &iotype, path, mode, 0);
}

/**
 * Create a new file using pio. Input parameters are read on comp task
 * 0 and ignored elsewhere. NOFILL mode will be turned on in all
 * cases.
 *
 * @param iosysid A defined pio system ID, obtained from
 * PIOc_InitIntercomm() or PIOc_InitAsync().
 * @param ncidp A pointer that gets the ncid of the newly created
 * file.
 * @param iotype A pointer to a pio output format. Must be one of
 * PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C,
 * PIO_IOTYPE_NETCDF4P or PIO_IOTYPE_NETCDF4P_NCZARR.
 * @param filename The filename to create.
 * @param mode The netcdf mode for the create operation.
 * @returns 0 for success, error code otherwise.
 * @ingroup PIO_createfile
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_createfile_impl(int iosysid, int *ncidp, const int *iotype, const char *filename,
                    int mode)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    int ret;               /* Return code from function calls. */

    GPTLstart("PIO:write_total");
    if ((*iotype == PIO_IOTYPE_ADIOS) || (*iotype == PIO_IOTYPE_ADIOSC))
    {
        GPTLstart("PIO:write_total_adios");
    }

    /* Get the IO system info from the id. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        GPTLstop("PIO:write_total");
        if ((*iotype == PIO_IOTYPE_ADIOS) || (*iotype == PIO_IOTYPE_ADIOSC))
        {
            GPTLstop("PIO:write_total_adios");
        }
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Unable to create file (%s, mode = %d, iotype=%s). Invalid arguments provided, invalid iosystem id (iosysid = %d)", (filename) ? filename : "NULL", mode, (!iotype) ? "UNKNOWN" : pio_iotype_to_string(*iotype), iosysid);
    }

    spio_ltimer_start(ios->io_fstats->wr_timer_name);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    /* Create the file. */
    if ((ret = spio_createfile_int(iosysid, ncidp, iotype, filename, mode)))
    {
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        if ((*iotype == PIO_IOTYPE_ADIOS) || (*iotype == PIO_IOTYPE_ADIOSC))
        {
            GPTLstop("PIO:write_total_adios");
        }

        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Unable to create file (%s, mode = %d, iotype=%s) on iosystem (iosystem id = %d). Internal error creating the file", (filename) ? filename : "NULL", mode, (!iotype) ? "UNKNOWN" : pio_iotype_to_string(*iotype), iosysid);
    }

    /* Run this on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. (Because otherwise, in async
     * mode, set_fill would be called twice by each IO task, since
     * PIOc_createfile() will already be called on each IO task.) */
    if (!ios->async || !ios->ioproc)
    {
        /* Set the fill mode to NOFILL. */
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        if ((ret = PIOc_set_fill_impl(*ncidp, NC_NOFILL, NULL)))
        {
            GPTLstop("PIO:write_total");
            if ((*iotype == PIO_IOTYPE_ADIOS) || (*iotype == PIO_IOTYPE_ADIOSC))
            {
                GPTLstop("PIO:write_total_adios");
            }
            return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                            "Unable to create file (%s, mode = %d, iotype=%s) on iosystem (iosystem id = %d). Setting fill mode to NOFILL failed.", (filename) ? filename : "NULL", mode, (!iotype) ? "UNKNOWN" : pio_iotype_to_string(*iotype), iosysid);
        }
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
    }

    GPTLstop("PIO:write_total");
    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    if ((*iotype == PIO_IOTYPE_ADIOS) || (*iotype == PIO_IOTYPE_ADIOSC))
    {
        GPTLstop("PIO:write_total_adios");
    }

    return ret;
}

/**
 * Open a new file using pio. The default fill mode will be used (FILL
 * for netCDF and netCDF-4 formats, NOFILL for pnetcdf.) Input
 * parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid : A defined pio system descriptor (input)
 * @param cmode : The netcdf mode for the create operation.
 * @param filename : The filename to open
 * @param ncidp : A pio file descriptor (output)
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_create
 * @author Ed Hartnett
 */
int PIOc_create_impl(int iosysid, const char *filename, int cmode, int *ncidp)
{
    int iotype;            /* The PIO IO type. */

    /* Set the default iotype. */
#ifdef _NETCDF
    iotype = PIO_IOTYPE_NETCDF;
#else /* Assume that _PNETCDF is defined. */
    iotype = PIO_IOTYPE_PNETCDF;
#endif

    /* Figure out the iotype. */
    if (cmode & NC_NETCDF4)
    {
#ifdef _NETCDF4
        /* FIXME: Add logic to find NCZARR */
        if (cmode & NC_MPIIO || cmode & NC_MPIPOSIX)
            iotype = PIO_IOTYPE_NETCDF4P;
        else
            iotype = PIO_IOTYPE_NETCDF4C;
#endif
    }
    else
    {
#ifdef _PNETCDF
        if (cmode & NC_PNETCDF || cmode & NC_MPIIO)
            iotype = PIO_IOTYPE_PNETCDF;
#endif
    }

    return spio_createfile_int(iosysid, ncidp, &iotype, filename, cmode);
}

/* Internal helper function to perform sync operations
 * ncid : the ncid of the file to sync
 * Returns PIO_NOERR for success, error code otherwise
 */
static int sync_file(int ncid)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file = NULL;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    LOG((1, "sync_file ncid = %d", ncid));

    /* Get the file info from the ncid. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Syncing file (ncid=%d) failed. Invalid file id. Unable to find internal structure associated with the file id", ncid);
    }

    assert(file);
    ios = file->iosystem;
    assert(ios);

    if(file->mode & PIO_WRITE)
    {
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(file->io_fstats->wr_timer_name);
    }
    spio_ltimer_start(ios->io_fstats->tot_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);

#ifdef _ADIOS2
    if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
    {
        if (file->mode & PIO_WRITE)
        {
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
        }
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);

        return ierr;
    }
#endif

    /* Flush data buffers on computational tasks. */
    if (!ios->async || !ios->ioproc)
    {
        if (file->mode & PIO_WRITE)
        {
            wmulti_buffer *wmb, *twmb;

            LOG((3, "sync_file checking buffers"));
            wmb = &file->buffer;
            while (wmb)
            {
                /* If there are any data arrays waiting in the
                 * multibuffer, flush it to IO tasks. */
                if (wmb->num_arrays > 0)
                {
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    flush_buffer(ncid, wmb, false);
                    spio_ltimer_start(ios->io_fstats->wr_timer_name);
                    spio_ltimer_start(ios->io_fstats->tot_timer_name);
                    spio_ltimer_start(file->io_fstats->wr_timer_name);
                    spio_ltimer_start(file->io_fstats->tot_timer_name);
                }
                twmb = wmb;
                wmb = wmb->next;
                if (twmb == &file->buffer)
                {
                    twmb->ioid = -1;
                    twmb->next = NULL;
                }
                else
                {
                    free(twmb);
                }
            }
        }
    }

    /* If async is in use, send message to IO master tasks. */
    if (ios->async)
    {
        int msg = PIO_MSG_SYNC;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid);
        if (ierr != PIO_NOERR)
        {
            if(file->mode & PIO_WRITE)
            {
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
            }
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Syncing file %s (ncid=%d) failed. Unable to send asynchronous message, PIO_MSG_SYNC, on iosystem (iosysid=%d)", pio_get_fname_from_file(file), ncid, ios->iosysid);
        }
    }

    /* Call the sync function on IO tasks.

       We choose not to call ncmpi_sync() for PIO_IOTYPE_PNETCDF,
       as it has been confirmed to have a very high cost on some
       systems. ncmpi_sync() itself does nothing but simply calls
       MPI_File_sync(), which usually incurs a huge performance
       penalty by calling POSIX sync internally. It is designed
       to ensure the data is safely stored on the disk hardware,
       before the function returns. People use it for extremely
       cautious behavior only.
     */
    if (file->mode & PIO_WRITE)
    {
        if (ios->ioproc)
        {
            switch (file->iotype)
            {
#ifdef _NETCDF4
            case PIO_IOTYPE_NETCDF4P:
            case PIO_IOTYPE_NETCDF4P_NCZARR:
                ierr = nc_sync(file->fh);
                break;
            case PIO_IOTYPE_NETCDF4C:
#endif
#ifdef _NETCDF
            case PIO_IOTYPE_NETCDF:
                if (ios->io_rank == 0)
                    ierr = nc_sync(file->fh);
                break;
#endif
#ifdef _PNETCDF
            case PIO_IOTYPE_PNETCDF:
                ierr = flush_output_buffer(file, true, 0);
                break;
#endif
#ifdef _HDF5
            case PIO_IOTYPE_HDF5:
            case PIO_IOTYPE_HDF5C:
                ierr = PIO_NOERR;
                break;
#endif
            default:
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__,
                                "Syncing file %s (ncid=%d) failed. Invalid/Unsupported iotype (%s:%d) provided", pio_get_fname_from_file(file), ncid, pio_iotype_to_string(file->iotype), file->iotype);
            }
        }
        LOG((2, "sync_file ierr = %d", ierr));
    }

    ierr = check_netcdf(ios, NULL, ierr, __FILE__, __LINE__);
    if (ierr != PIO_NOERR)
    {
        LOG((1, "nc*_sync failed, ierr = %d", ierr));
        if(file->mode & PIO_WRITE)
        {
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
        }
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return ierr;
    }

    if(file->mode & PIO_WRITE)
    {
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
    }
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return PIO_NOERR;
}

int spio_wait_on_hard_close(iosystem_desc_t *ios, file_desc_t *file)
{
  const int SLEEP_TIME_IN_MILLISECONDS = 500;
  while(!file->is_hard_closed){
    std::this_thread::sleep_for(std::chrono::milliseconds(SLEEP_TIME_IN_MILLISECONDS));
  }

  return PIO_NOERR;
}

/* Close the file ("hard close")
 * @param ios: Pointer to the iosystem_desc
 * @param file: Pointer to the file_desc for the file
 * @returns PIO_NOERR for success, a pio error code otherwise
 */
int spio_hard_closefile(iosystem_desc_t *ios, file_desc_t *file,
      bool sync_with_ioprocs)
{
    int ierr = PIO_NOERR;
#ifdef _ADIOS2
    char outfilename[PIO_MAX_NAME + 1];
    size_t len = 0;
#endif

    assert(ios && file);

    /* Get the lock before proceeding */
    std::lock_guard<std::mutex> lg(*(file->pmtx));

    int ncid = file->pio_ncid;

    if(sync_with_ioprocs){
      //if(ios->ioproc) { while(file->npend_ops){} }
      MPI_Barrier(ios->union_comm);
    }

    if(file->is_hard_closed) { return PIO_NOERR; }

    /* ADIOS: assume all procs are also IO tasks */
#ifdef _ADIOS2
    if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
    {
        adios2_error adiosErr = adios2_error_none;

        if (file->mode & PIO_WRITE) /* ADIOS write mode */
        {
            if (file->adios_io_process == 1 && file->engineH != NULL)
            {
                LOG((2, "ADIOS close file %s", file->filename));

                ierr = begin_adios2_step(file, NULL);
                if (ierr != PIO_NOERR)
                {
                    return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                                   "adios2_begin_step failed for file (%s)", pio_get_fname_from_file(file));
                }

                if (file->adios_rank == 0)
                {
                    adios2_attribute *attributeH = adios2_inquire_attribute(file->ioH, "/__pio__/fillmode");
                    if (attributeH == NULL)
                    {
                        attributeH = adios2_define_attribute(file->ioH, "/__pio__/fillmode", adios2_type_int32_t, &file->fillmode);
                        if (attributeH == NULL)
                        {
                            return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                           "Defining (ADIOS) attribute (name=/__pio__/fillmode) failed for file (%s, ncid=%d)",
                                           pio_get_fname_from_file(file), file->pio_ncid);
                        }
                    }
                }

                /* This is needed to write out the attribute /__pio__/fillmode */
                {
                    adios2_variable *variableH = adios2_inquire_variable(file->ioH, "/__pio__/info/testing");
                    if (variableH == NULL)
                    {
                        variableH = spio_define_adios2_variable(ios, file, file->ioH,
                                                                "/__pio__/info/testing", adios2_type_int32_t,
                                                                0, NULL, NULL, NULL,
                                                                adios2_constant_dims_true);
                        if (variableH == NULL)
                        {
                            return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                           "Defining (ADIOS) variable (name=/__pio__/info/testing) failed for file (%s)",
                                           pio_get_fname_from_file(file));
                        }
                    }

                    adiosErr = adios2_put(file->engineH, variableH, &ios->num_uniontasks, adios2_mode_sync);
                    if (adiosErr != adios2_error_none)
                    {
                        return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Putting (ADIOS) variable (name=/__pio__/info/testing) failed (adios2_error=%s) for file (%s)",
                                       convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
                    }
                }

                GPTLstart("end_adios2_step_PIOc_closefile");
                ierr = end_adios2_step(file, ios);
                GPTLstop("end_adios2_step_PIOc_closefile");
                if (ierr != PIO_NOERR)
                {
                    return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                                   "adios2_end_step failed for file (%s)", pio_get_fname_from_file(file));
                }

                adiosErr = adios2_close(file->engineH);
                if (adiosErr != adios2_error_none)
                {
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__, "Closing (ADIOS) file (%s, ncid=%d) failed (adios2_error=%s)",
                                   pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
                }

                file->engineH = NULL;
            } /* End if (file->adios_io_process == 1 && file->engineH != NULL) */
        } /* End if (file->mode & PIO_WRITE) */
        else /* ADIOS read mode */
        {
            adiosErr = adios2_close(file->engineH);
            if (adiosErr != adios2_error_none)
            {
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Closing file (%s, ncid=%d) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to close all transports in adios2_Engine (adios2_error=%s)",
                               pio_get_fname_from_file(file), ncid, convert_adios2_error_to_string(adiosErr));
            }

            file->begin_step_called = 0;
            file->engineH = NULL;

            /* Remove io object in ADIOS read mode */
            adios2_bool result = adios2_false;
            adiosErr = adios2_remove_io(&result, ios->adios_readerH, file->io_name_reader);
            if (adiosErr != adios2_error_none)
            {
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Closing file (%s, ncid=%d) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to remove an io created with adios2_declare_io (adios2_error=%s)",
                               pio_get_fname_from_file(file), ncid, convert_adios2_error_to_string(adiosErr));
            }

            if (result == adios2_false)
            {
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Closing file (%s, ncid=%d) using ADIOS iotype failed. "
                               "adios2_remove_io: io not found and not removed",
                               pio_get_fname_from_file(file), ncid);
            }
        }

        for (int i = 0; i < file->num_dim_vars; i++)
        {
            if (file->dim_names[i] != NULL)
            {
                free(file->dim_names[i]);
                file->dim_names[i] = NULL;
            }
        }

        file->num_dim_vars = 0;

        for (int i = 0; i < file->num_vars; i++)
        {
            if (file->adios_vars[i].name != NULL)
            {
                free(file->adios_vars[i].name);
                file->adios_vars[i].name = NULL;
            }
            if (file->adios_vars[i].gdimids != NULL)
            {
                free(file->adios_vars[i].gdimids);
                file->adios_vars[i].gdimids = NULL;
            }
            file->adios_vars[i].adios_varid = NULL;
            file->adios_vars[i].decomp_varid = NULL;
            file->adios_vars[i].frame_varid = NULL;
            file->adios_vars[i].fillval_varid = NULL;

            if (file->adios_vars[i].fillval_buffer != NULL)
            {
                free(file->adios_vars[i].fillval_buffer);
                file->adios_vars[i].fillval_buffer = NULL;
                file->adios_vars[i].fillval_cnt = 0;
            }
            if (file->adios_vars[i].decomp_buffer != NULL)
            {
                free(file->adios_vars[i].decomp_buffer);
                file->adios_vars[i].decomp_buffer = NULL;
                file->adios_vars[i].decomp_cnt = 0;
            }
            if (file->adios_vars[i].frame_buffer != NULL)
            {
                free(file->adios_vars[i].frame_buffer);
                file->adios_vars[i].frame_buffer = NULL;
                file->adios_vars[i].frame_cnt = 0;
            }
            if (file->adios_vars[i].num_wb_buffer != NULL)
            {
                free(file->adios_vars[i].num_wb_buffer);
                file->adios_vars[i].num_wb_buffer = NULL;
                file->adios_vars[i].num_wb_cnt = 0;
            }
            if (file->adios_vars[i].interval_map != NULL)
            {
                for (int s = 0; s < file->adios_vars[i].interval_map->n_adios_steps; s++)
                    free(file->adios_vars[i].interval_map->map[s]);

                free(file->adios_vars[i].interval_map->map);
                free(file->adios_vars[i].interval_map);

                file->adios_vars[i].interval_map = NULL;
            }
        }

        file->num_vars = 0;

        /* Track attributes */
        for (int i = 0; i < file->num_attrs; i++)
        {
            if (file->adios_attrs[i].att_name != NULL)
            {
                free(file->adios_attrs[i].att_name);
                file->adios_attrs[i].att_name = NULL;
            }
        }

        file->num_attrs = 0;

        /* Block merging */
        if (file->block_myrank == 0)
        {
            if (file->block_array != NULL)
            {
                free(file->block_array);
                file->block_array = NULL;
                file->block_array_size = 0;
            }
            if (file->array_counts != NULL)
            {
                free(file->array_counts);
                file->array_counts = NULL;
                file->array_counts_size = 0;
            }
            if (file->array_disp != NULL)
            {
                free(file->array_disp);
                file->array_disp = NULL;
                file->array_disp_size = 0;
            }
            if (file->block_list != NULL)
            {
                free(file->block_list);
                file->block_list = NULL;
            }
        }

#ifdef _ADIOS_BP2NC_TEST /* Comment out for large scale run */
        if (file->mode & PIO_WRITE)
        {
#ifdef _PNETCDF
            char conv_iotype[] = "pnetcdf";
#else
            char conv_iotype[] = "netcdf";
#endif

            int rearr_type = PIO_REARR_SUBSET;

            /* Convert XXXX.nc.bp to XXXX.nc */
            len = strlen(file->filename);
            assert(len > 6 && len <= PIO_MAX_NAME);
            strncpy(outfilename, file->filename, len - 3);
            outfilename[len - 3] = '\0';
            LOG((1, "CONVERTING: %s", file->filename));
            MPI_Barrier(ios->union_comm);
            ierr = C_API_ConvertBPToNC(file->filename, outfilename, conv_iotype, rearr_type, ios->union_comm);
            MPI_Barrier(ios->union_comm);
            LOG((1, "DONE CONVERTING: %s", file->filename));
            if (ierr != PIO_NOERR)
            {
                return pio_err(ios, file, ierr, __FILE__, __LINE__,
                                "C_API_ConvertBPToNC(infile = %s, outfile = %s, piotype = %s) failed", file->filename, outfilename, conv_iotype);
            }
        }
#endif
        if (file->filename != NULL)
        {
            free(file->filename);
            file->filename = NULL;
        }

        return PIO_NOERR;
    }
#endif

    assert((file->iotype != PIO_IOTYPE_ADIOS) && (file->iotype != PIO_IOTYPE_ADIOSC));

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        switch (file->iotype)
        {
#ifdef _NETCDF4
        case PIO_IOTYPE_NETCDF4P:
        case PIO_IOTYPE_NETCDF4P_NCZARR:
            ierr = nc_close(file->fh);
            break;
        case PIO_IOTYPE_NETCDF4C:
#endif
#ifdef _NETCDF
        case PIO_IOTYPE_NETCDF:
            if (ios->io_rank == 0)
                ierr = nc_close(file->fh);
            break;
#endif
#ifdef _PNETCDF
        case PIO_IOTYPE_PNETCDF:
            if ((file->mode & PIO_WRITE)){
                ierr = ncmpi_buffer_detach(file->fh);
            }
            ierr = ncmpi_close(file->fh);
            break;
#endif
#ifdef _HDF5
        case PIO_IOTYPE_HDF5:
        case PIO_IOTYPE_HDF5C:
            ierr = spio_hdf5_close(ios, file);
            break;
#endif
        default:
            return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__,
                            "Closing file (%s, ncid=%d) failed. Unsupported iotype (%d) specified", pio_get_fname_from_file(file), file->pio_ncid, file->iotype);
        }
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        LOG((1, "nc*_close failed, ierr = %d", ierr));
        return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                        "Closing file (%s, ncid=%d) failed. Underlying I/O library (iotype=%s) call failed", pio_get_fname_from_file(file), file->pio_ncid, pio_iotype_to_string(file->iotype));
    }

#ifdef _HDF5
    if ((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C))
    {
        for (int i = 0; i < file->hdf5_num_dims; i++)
        {
            free(file->hdf5_dims[i].name);
            file->hdf5_dims[i].name = NULL;
        }

        file->hdf5_num_dims = 0;

        for (int i = 0; i < file->hdf5_num_vars; i++)
        {
            free(file->hdf5_vars[i].name);
            file->hdf5_vars[i].name = NULL;

            free(file->hdf5_vars[i].alt_name);
            file->hdf5_vars[i].alt_name = NULL;

            free(file->hdf5_vars[i].hdf5_dimids);
            file->hdf5_vars[i].hdf5_dimids = NULL;
        }

        file->hdf5_num_vars = 0;

        for (int i = 0; i < file->hdf5_num_attrs; i++)
        {
            if (file->hdf5_attrs[i].att_name != NULL)
            {
                free(file->hdf5_attrs[i].att_name);
                file->hdf5_attrs[i].att_name = NULL;
            }
        }

        file->hdf5_num_attrs = 0;
        file->hdf5_num_gattrs = 0;
    }
#endif

    file->is_hard_closed = true;

    return PIO_NOERR;
}

/* "Soft close" the file
 * The function assumes that only writes are pending on this file
 * @param ios: Pointer to the iosystem_desc
 * @param file: Pointer to the file_desc for the file
 * @returns PIO_NOERR for success, a pio error code otherwise
 */
int spio_soft_closefile(iosystem_desc_t *ios, file_desc_t *file)
{
  assert(ios && file && ios->ioproc);

  return pio_iosys_async_file_close_op_add(file);
}

/**
 * Close a file previously opened with PIO.
 *
 * @param ncid: the file pointer
 * @returns PIO_NOERR for success, error code otherwise.
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_closefile_impl(int ncid)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file = NULL;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    LOG((1, "PIOc_closefile ncid = %d", ncid));

    /* Find the info about this file. */
    if((ierr = pio_get_file(ncid, &file))){
      return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                      "Closing file failed. Invalid file id (ncid=%d) provided", ncid);
    }
    assert(file);
    ios = file->iosystem;
    assert(ios);

    if((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC)){
      if(file->mode & PIO_WRITE){
        GPTLstart("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
        GPTLstart("PIO:write_total");
#endif
      }
    }
    else{
      if(file->mode & PIO_WRITE){
        GPTLstart("PIO:PIOc_closefile_write_mode");
        GPTLstart("PIO:write_total");
      }
    }

    /* Sync changes before closing on all tasks if async is not in
     * use, but only on non-IO tasks if async is in use. */
    if(!ios->async || !ios->ioproc){
      if(file->mode & PIO_WRITE){
        sync_file(ncid);
      }
    }

    if((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC)){
#ifndef _ADIOS_BP2NC_TEST
      if(file->mode & PIO_WRITE){
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(file->io_fstats->wr_timer_name);
      }
      spio_ltimer_start(ios->io_fstats->tot_timer_name);
      spio_ltimer_start(file->io_fstats->tot_timer_name);
#endif
    }
    else{
      if(file->mode & PIO_WRITE){
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(file->io_fstats->wr_timer_name);
      }
      spio_ltimer_start(ios->io_fstats->tot_timer_name);
      spio_ltimer_start(file->io_fstats->tot_timer_name);
    }

    /* If async is in use and this is a comp tasks, then the compmaster
     * sends a msg to the pio_msg_handler running on the IO master and
     * waiting for a message. Then broadcast the ncid over the intercomm
     * to the IO tasks. */
    if(ios->async){
      int msg = PIO_MSG_CLOSE_FILE;

      PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid);
      if(ierr != PIO_NOERR){
        if((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC)){
          if(file->mode & PIO_WRITE){ GPTLstop("PIO:write_total_adios"); }

#ifndef _ADIOS_BP2NC_TEST
          if(file->mode & PIO_WRITE){
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
          }
          spio_ltimer_stop(ios->io_fstats->tot_timer_name);
          spio_ltimer_stop(file->io_fstats->tot_timer_name);
#endif
        }
        else{
          if(file->mode & PIO_WRITE){
            GPTLstop("PIO:PIOc_closefile_write_mode");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
          }
          spio_ltimer_stop(ios->io_fstats->tot_timer_name);
          spio_ltimer_stop(file->io_fstats->tot_timer_name);
        }
        return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Closing file (%s, ncid=%d) failed. Error sending async msg PIO_MSG_CLOSE_FILE", pio_get_fname_from_file(file), ncid);
      }
    }

    bool soft_close = false;
#if PIO_USE_ASYNC_WR_THREAD
    if((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C)){
      if(ios->ioproc){
        soft_close = true;
      }
    }
#endif

    if(soft_close){
      ierr = spio_soft_closefile(ios, file);
    }
    else{
      ierr = spio_hard_closefile(ios, file, false);
    }

    if((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC)){
      if (file->mode & PIO_WRITE) { GPTLstop("PIO:write_total_adios"); }

    #ifndef _ADIOS_BP2NC_TEST
      if(file->mode & PIO_WRITE){
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
      }
      spio_ltimer_stop(ios->io_fstats->tot_timer_name);
      spio_ltimer_stop(file->io_fstats->tot_timer_name);
    #endif
    }
    else{
      if(file->mode & PIO_WRITE) {
        GPTLstop("PIO:PIOc_closefile_write_mode");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
      }
      spio_ltimer_stop(ios->io_fstats->tot_timer_name);
      spio_ltimer_stop(file->io_fstats->tot_timer_name);
    }

    /* FIXME: How to account for async case ? */
    spio_write_file_io_summary(file);

    if(!soft_close){
      /* Delete file from our list of open files. */
      pio_delete_file_from_list(ncid);
    }

    return ierr;
}

/**
 * Delete a file.
 *
 * @param iosysid a pio system handle.
 * @param filename a filename.
 * @returns PIO_NOERR for success, error code otherwise.
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_deletefile_impl(int iosysid, const char *filename)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function codes. */
     int msg = PIO_MSG_DELETE_FILE;
    size_t len;

    LOG((1, "PIOc_deletefile iosysid = %d filename = %s", iosysid, filename));

    /* Get the IO system info from the id. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Deleting file (%s) failed. Invalid I/O system id (iosysid=%d) specified.", (filename) ? filename : "NULL", iosysid);
    }

    assert(ios);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    /* If async is in use, send message to IO master task. */
    if (ios->async)
    {
        len = strlen(filename) + 1;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, len, filename);
        if(ierr != PIO_NOERR)
        {
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                        "Deleting file (%s) failed. Sending async message, PIO_MSG_DELETE_FILE, failed", (filename) ? filename : "NULL");
        }
    }

#if PIO_USE_ASYNC_WR_THREAD
    ierr = spio_close_soft_closed_file(filename);
    if(ierr != PIO_NOERR){
      return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                      "Deleting file (%s) failed. Error closing previous soft closed file", filename);
    }
#endif

    /* If this is an IO task, then call the netCDF function. The
     * barriers are needed to assure that no task is trying to operate
     * on the file while it is being deleted. IOTYPE is not known, but
     * nc_delete() will delete any type of file. */
    mpierr = MPI_Barrier(ios->union_comm);
    if (ios->ioproc)
    {
        if (!mpierr && ios->io_rank == 0)
        {
            struct stat sd;
#ifdef _ADIOS2
            /* Append ".bp" to filename for the corresponding ADIOS BP filename */
            static const char adios_bp_filename_extn[] = ".bp";
            size_t adios_bp_filename_len = strlen(filename) + sizeof(adios_bp_filename_extn) + 1;
            char *adios_bp_filename = (char *) calloc(adios_bp_filename_len, sizeof(char));
            if (adios_bp_filename == NULL)
            {
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Deleting file (%s) failed. Allocating memory for adios filename failed", (filename) ? filename : "NULL");
            }
            snprintf(adios_bp_filename, adios_bp_filename_len, "%s%s", filename, adios_bp_filename_extn);

            if (0 == stat(adios_bp_filename, &sd))
            {
                spio_remove_directory(adios_bp_filename);
            }

            free(adios_bp_filename);
#endif

            int ret = stat(filename, &sd);
            if ((ret == 0) && S_ISDIR(sd.st_mode))
            {
                /* Delete the directory pointed to by filename (e.g. NCZarr files) */
                spio_remove_directory(filename);
            }
            else
            {
                /* Delete the file (for ADIOS BP files, delete the symlink file).
                    Ignore stat errors (dangling symlinks etc) and force unlink
                */
                ierr = unlink(filename);
            }
        }

    }
    mpierr = MPI_Barrier(ios->union_comm);
    LOG((2, "PIOc_deletefile ierr = %d", ierr));

    ierr = check_netcdf(ios, NULL, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                    "Deleting file (%s) failed. Internal I/O library call failed.", (filename) ? filename : "NULL");
    }

    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    return ierr;
}

/**
 * PIO interface to nc_sync This routine is called collectively by all
 * tasks in the communicator ios.union_comm.
 *
 * Refer to the <A
 * HREF="http://www.unidata.ucar.edu/software/netcdf/docs/modules.html"
 * target="_blank"> netcdf </A> documentation.
 *
 * @param ncid the ncid of the file to sync.
 * @returns PIO_NOERR for success, error code otherwise.
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_sync_impl(int ncid)
{
    file_desc_t *file = NULL;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    LOG((1, "PIOc_sync ncid = %d", ncid));

    /* Get the file info from the ncid. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Syncing file (ncid=%d) failed. Invalid file id. Unable to find internal structure associated with the file id", ncid);
    }
    assert(file);

    if (file->mode & PIO_WRITE)
    {
        GPTLstart("PIO:write_total");
        if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
            GPTLstart("PIO:write_total_adios");
    }

    ierr = sync_file(ncid);

    if (file->mode & PIO_WRITE)
    {
        GPTLstop("PIO:write_total");
        if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
            GPTLstop("PIO:write_total_adios");
    }

    return ierr;
}
