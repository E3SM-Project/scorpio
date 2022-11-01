/**
 * @file
 * Internal PIO functions to get and put attributes and data
 * (excluding varm functions).
 *
 * @author Ed Hartnett
 * @date  2016
 *
 * @see http://code.google.com/p/parallelio/
 */

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#include "spio_io_summary.h"

/**
 * Write a netCDF attribute of any type, converting to any type.
 *
 * This routine is called collectively by all tasks in the communicator
 * ios.union_comm.
 *
 * @param ncid the ncid of the open file, obtained from
 * PIOc_openfile() or PIOc_createfile().
 * @param varid the variable ID.
 * @param name the name of the attribute.
 * @param atttype the nc_type of the attribute.
 * @param len the length of the attribute array.
 * @param op a pointer with the attribute data.
 * @return PIO_NOERR for success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_put_att_tc(int ncid, int varid, const char *name, nc_type atttype,
                    PIO_Offset len, nc_type memtype, const void *op)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    PIO_Offset atttype_len = 0;    /* Length (in bytes) of the att type in file. */
    PIO_Offset memtype_len = 0;    /* Length of the att data type in memory. */
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function codes. */
    int ierr = PIO_NOERR;           /* Return code from function calls. */

    GPTLstart("PIO:PIOc_put_att_tc");
    GPTLstart("PIO:write_total");
    /* Find the info about this file. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        GPTLstop("PIO:PIOc_put_att_tc");
        GPTLstop("PIO:write_total");
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Writing variable (varid=%d) attribute (%s) to file failed. Invalid file id (ncid=%d) provided", varid, (name) ? name : "NULL", ncid);
    }
    assert(file);
    spio_ltimer_start(file->io_fstats->wr_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);
    ios = file->iosystem;
    assert(ios);

    spio_ltimer_start(ios->io_fstats->wr_timer_name);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        GPTLstart("PIO:PIOc_put_att_tc_adios");
        GPTLstart("PIO:write_total_adios");
    }

    /* User must provide some valid parameters. */
    if (!name || !op || strlen(name) > PIO_MAX_NAME || len < 0)
    {
        GPTLstop("PIO:PIOc_put_att_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        if (file->iotype == PIO_IOTYPE_ADIOS)
        {
            GPTLstop("PIO:PIOc_put_att_tc_adios");
            GPTLstop("PIO:write_total_adios");
        }
        return pio_err(ios, file, PIO_EINVAL, __FILE__, __LINE__,
                        "Writing variable (%s, varid=%d) attribute (%s) to file (%s, ncid=%d) failed. Invalid arguments provided, Attribute name (name) is %s (expected not NULL), Attribute data pointer (op) is %s (expected not NULL), Attribute name length is %lld (expected <= %d), Attribute length is %lld (expected >= 0)", pio_get_vname_from_file(file, varid), varid, PIO_IS_NULL(name), pio_get_fname_from_file(file), file->pio_ncid, PIO_IS_NULL(name), PIO_IS_NULL(op), (name) ? ((unsigned long long )strlen(name)) : 0,  PIO_MAX_NAME, (unsigned long long ) len);
    }

    LOG((1, "PIOc_put_att_tc ncid = %d varid = %d name = %s atttype = %d len = %d memtype = %d",
         ncid, varid, name, atttype, len, memtype));

    /* Run these on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. */
    if (!ios->async || !ios->ioproc)
    {
        /* Get the length (in bytes) of the type in file. */
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        ierr = PIOc_inq_type(ncid, atttype, NULL, &atttype_len);
        if(ierr != PIO_NOERR){
            LOG((1, "PIOc_inq_type failed, ierr = %d", ierr));
            GPTLstop("PIO:PIOc_put_att_tc");
            GPTLstop("PIO:write_total");
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_att_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return ierr;
        }

        /* Get the length (in bytes) of the type in memory. */
        if (memtype == PIO_LONG_INTERNAL)
            memtype_len = sizeof(long int);
        else
        {
            ierr = PIOc_inq_type(ncid, memtype, NULL, &memtype_len);
            if(ierr != PIO_NOERR){
                GPTLstop("PIO:PIOc_put_att_tc");
                GPTLstop("PIO:write_total");
                if (file->iotype == PIO_IOTYPE_ADIOS)
                {
                    GPTLstop("PIO:PIOc_put_att_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                }
                LOG((1, "PIOc_inq_type failed, ierr = %d", ierr));
                return ierr;
            }
        }
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
        spio_ltimer_start(file->io_fstats->wr_timer_name);
        spio_ltimer_start(file->io_fstats->tot_timer_name);
        LOG((2, "PIOc_put_att atttype_len = %d memtype_len = %d", ncid, atttype_len, memtype_len));
    }

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_PUT_ATT;
        int namelen = strlen(name) + 1;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid, varid, namelen, name,
            atttype, len, atttype_len, memtype, memtype_len,
            len * memtype_len, op);
        if(ierr != PIO_NOERR)
        {
            GPTLstop("PIO:PIOc_put_att_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_att_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Writing variable (%s, varid=%d) attribute (%s) to file (%s, ncid=%d) failed. Error sending asynchronous message, PIO_MSG_PUT_ATT",  pio_get_vname_from_file(file, varid), varid, name, pio_get_fname_from_file(file), file->pio_ncid);
        }

        /* Broadcast values currently only known on computation tasks to IO tasks. */
        if ((mpierr = MPI_Bcast(&atttype_len, 1, MPI_OFFSET, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_put_att_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_att_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        if ((mpierr = MPI_Bcast(&memtype_len, 1, MPI_OFFSET, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_put_att_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_att_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        LOG((2, "PIOc_put_att bcast from comproot = %d atttype_len = %d", ios->comproot,
             atttype_len, memtype_len));
    }

    /* ADIOS: assume all procs are also IO tasks */
#ifdef _ADIOS2
    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        LOG((2, "ADIOS define attribute %s, varid %d, type %d", name, varid, atttype));

        ierr = begin_adios2_step(file, ios);
        if (ierr != PIO_NOERR)
        {
            GPTLstop("PIO:PIOc_put_att_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            GPTLstop("PIO:PIOc_put_att_tc_adios");
            GPTLstop("PIO:write_total_adios");
            return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                           "adios2_begin_step failed for file (%s)", pio_get_fname_from_file(file));
        }

        adios2_type adios_type = PIOc_get_adios_type(atttype);

        char path[PIO_MAX_NAME];
        if (varid != PIO_GLOBAL)
        {
            assert((strlen("/__pio__/var/") + strlen(file->adios_vars[varid].name)) < PIO_MAX_NAME);
            snprintf(path, PIO_MAX_NAME, "/__pio__/var/%s", file->adios_vars[varid].name);
            ++file->adios_vars[varid].nattrs;
        }
        else
        {
            snprintf(path, PIO_MAX_NAME, "/__pio__/global");
            file->num_gattrs++;
        }

        /* Track attributes */
        int num_attrs = file->num_attrs;
        if (num_attrs >= PIO_MAX_VARS)
        {
            fprintf(stderr, "ERROR: Num of attributes exceeds maximum (%d).\n", PIO_MAX_VARS);
            GPTLstop("PIO:PIOc_put_att_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            GPTLstop("PIO:PIOc_put_att_tc_adios");
            GPTLstop("PIO:write_total_adios");
            return pio_err(NULL, file, PIO_EMAXATTS, __FILE__, __LINE__,
                           "num_attrs (%d) is larger than or equal to pio_max_vars (%d) for file (%s)",
                           num_attrs, PIO_MAX_VARS, pio_get_fname_from_file(file));
        }
        file->adios_attrs[num_attrs].att_name = strdup(name);
        file->adios_attrs[num_attrs].att_len = len;
        file->adios_attrs[num_attrs].att_type = atttype;
        file->adios_attrs[num_attrs].att_varid = varid;
        file->adios_attrs[num_attrs].att_ncid = ncid;
        file->adios_attrs[num_attrs].adios_type = adios_type;
        file->num_attrs++;

        if (file->all_rank == MPI_ROOT)
        {
            ios->io_fstats->wb += len * atttype_len;
            file->io_fstats->wb += len * atttype_len;
        }

        char att_name[PIO_MAX_NAME];
        if (file->adios_io_process == 1)
        {
            snprintf(att_name, PIO_MAX_NAME, "%s/%s", path, name);
            adios2_attribute *attributeH = adios2_inquire_attribute(file->ioH, att_name);
            if (attributeH == NULL)
            {
                if (NC_CHAR == atttype || adios2_type_string == adios_type)
                    attributeH = adios2_define_attribute(file->ioH, att_name, adios2_type_string, op);
                else
                    attributeH = adios2_define_attribute(file->ioH, att_name, adios_type, op);

                if (attributeH == NULL)
                {
                    GPTLstop("PIO:PIOc_put_att_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_att_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Defining (ADIOS) attribute (name=%s) failed for file (%s, ncid=%d)",
                                   att_name, pio_get_fname_from_file(file), file->pio_ncid);
                }
                file->num_written_blocks += 1;
            }
        }

        GPTLstop("PIO:PIOc_put_att_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        GPTLstop("PIO:PIOc_put_att_tc_adios");
        GPTLstop("PIO:write_total_adios");

        return PIO_NOERR;
    }
#endif

#ifdef _HDF5
    if (file->iotype == PIO_IOTYPE_HDF5)
    {
        if (ios->ioproc)
        {
            hid_t attr_id;
            hid_t space_id;
            hsize_t asize = len;
            htri_t att_exists;
            hid_t loc_id;
            hid_t h5_xtype;

            if (varid == PIO_GLOBAL)
                loc_id = file->hdf5_file_id;
            else
                loc_id = file->hdf5_vars[varid].hdf5_dataset_id;

            if (atttype == NC_CHAR)
            {
                /* String type */
                space_id = H5Screate(H5S_SCALAR);
                h5_xtype = H5Tcopy(H5T_C_S1);
                H5Tset_size(h5_xtype, asize);
                H5Tset_strpad(h5_xtype, H5T_STR_NULLTERM);
                H5Tset_cset(h5_xtype, H5T_CSET_ASCII);
            }
            else
            {
                space_id = H5Screate_simple(1, &asize, &asize);
                h5_xtype = nc_type_to_hdf5_type(atttype);
            }

            /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
            att_exists = H5Aexists(loc_id, name);
            if (att_exists > 0)
            {
                attr_id = H5Aopen(loc_id, name, H5P_DEFAULT);
            }
            else if (att_exists == 0)
            {
                attr_id = H5Acreate2(loc_id, name, h5_xtype, space_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            else
            {
                /* Error determining whether an attribute with a given name exists on an object */
            }

            H5Awrite(attr_id, h5_xtype, op);

            H5Sclose(space_id);
            H5Aclose(attr_id);

            /* String attribute */
            if (atttype == NC_CHAR)
                H5Tclose(h5_xtype);
        }

        GPTLstop("PIO:PIOc_put_att_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);

        return PIO_NOERR;
    }
#endif /* _HDF5 */

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _PNETCDF
        if (file->iotype == PIO_IOTYPE_PNETCDF)
        {
            if (ios->iomaster == MPI_ROOT)
            {
                ios->io_fstats->wb += len * atttype_len;
                file->io_fstats->wb += len * atttype_len;
            }

            switch(memtype)
            {
            case NC_BYTE:
                ierr = ncmpi_put_att_schar(file->fh, varid, name, atttype, len, op);
                break;
            case NC_CHAR:
                ierr = ncmpi_put_att_text(file->fh, varid, name, len, op);
                break;
            case NC_SHORT:
                ierr = ncmpi_put_att_short(file->fh, varid, name, atttype, len, op);
                break;
            case NC_INT:
                ierr = ncmpi_put_att_int(file->fh, varid, name, atttype, len, op);
                break;
            case PIO_LONG_INTERNAL:
                ierr = ncmpi_put_att_long(file->fh, varid, name, atttype, len, op);
                break;
            case NC_FLOAT:
                ierr = ncmpi_put_att_float(file->fh, varid, name, atttype, len, op);
                break;
            case NC_DOUBLE:
                ierr = ncmpi_put_att_double(file->fh, varid, name, atttype, len, op);
                break;
            default:
                GPTLstop("PIO:PIOc_put_att_tc");
                GPTLstop("PIO:write_total");
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__,
                                "Writing variable (%s, varid=%d) attribute (%s) to file (%s, ncid=%d) failed. Unsupported PnetCDF attribute type (memtype = %x)", pio_get_vname_from_file(file, varid), varid, name, pio_get_fname_from_file(file), file->pio_ncid, memtype);
            }
        }
#endif /* _PNETCDF */

        if (file->iotype != PIO_IOTYPE_PNETCDF && file->iotype != PIO_IOTYPE_ADIOS && file->iotype != PIO_IOTYPE_HDF5 && file->do_io)
        {
            if (ios->iomaster == MPI_ROOT)
            {
                ios->io_fstats->wb += len * atttype_len;
                file->io_fstats->wb += len * atttype_len;
            }

            switch(memtype)
            {
#ifdef _NETCDF
            case NC_CHAR:
                ierr = nc_put_att_text(file->fh, varid, name, len, op);
                break;
            case NC_BYTE:
                ierr = nc_put_att_schar(file->fh, varid, name, atttype, len, op);
                break;
            case NC_SHORT:
                ierr = nc_put_att_short(file->fh, varid, name, atttype, len, op);
                break;
            case NC_INT:
                ierr = nc_put_att_int(file->fh, varid, name, atttype, len, op);
                break;
            case PIO_LONG_INTERNAL:
                ierr = nc_put_att_long(file->fh, varid, name, atttype, len, op);
                break;
            case NC_FLOAT:
                ierr = nc_put_att_float(file->fh, varid, name, atttype, len, op);
                break;
            case NC_DOUBLE:
                ierr = nc_put_att_double(file->fh, varid, name, atttype, len, op);
                break;
#endif /* _NETCDF */
#ifdef _NETCDF4
            case NC_UBYTE:
                ierr = nc_put_att_uchar(file->fh, varid, name, atttype, len, op);
                break;
            case NC_USHORT:
                ierr = nc_put_att_ushort(file->fh, varid, name, atttype, len, op);
                break;
            case NC_UINT:
                ierr = nc_put_att_uint(file->fh, varid, name, atttype, len, op);
                break;
            case NC_INT64:
                LOG((3, "about to call nc_put_att_longlong"));
                ierr = nc_put_att_longlong(file->fh, varid, name, atttype, len, op);
                break;
            case NC_UINT64:
                ierr = nc_put_att_ulonglong(file->fh, varid, name, atttype, len, op);
                break;
                /* case NC_STRING: */
                /*      ierr = nc_put_att_string(file->fh, varid, name, atttype, len, op); */
                /*      break; */
#endif /* _NETCDF4 */
            default:
                GPTLstop("PIO:PIOc_put_att_tc");
                GPTLstop("PIO:write_total");
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__,
                                "Writing variable (%s, varid=%d) attribute (%s) to file (%s, ncid=%d) failed. Unsupported attribute type (memtype = %x)", pio_get_vname_from_file(file, varid), varid, name, pio_get_fname_from_file(file), file->pio_ncid, memtype);
            }
        }
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        LOG((1, "nc*_put_att_* failed, ierr = %d", ierr));
        GPTLstop("PIO:PIOc_put_att_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                        "Writing variable (%s, varid=%d) attribute (%s) to file (%s, ncid=%d) failed. Internal I/O library (%s) call failed", pio_get_vname_from_file(file, varid), varid, name, pio_get_fname_from_file(file), file->pio_ncid, pio_iotype_to_string(file->iotype));
    }

    GPTLstop("PIO:PIOc_put_att_tc");
    GPTLstop("PIO:write_total");
    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->wr_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return PIO_NOERR;
}

/**
 * Get the value of an attribute of any type, converting to any type.
 *
 * This routine is called collectively by all tasks in the communicator
 * ios.union_comm.
 *
 * @param ncid the ncid of the open file, obtained from
 * PIOc_openfile() or PIOc_createfile().
 * @param varid the variable ID.
 * @param name the name of the attribute to get
 * @param memtype the type of the data in memory (if different from
 * the type of the attribute, the data will be converted to
 * memtype). The ip pointer points to memory to hold att_len elements
 * of type memtype.
 * @param ip a pointer that will get the attribute value.
 * @return PIO_NOERR for success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_get_att_tc(int ncid, int varid, const char *name, nc_type memtype, void *ip)
{
    iosystem_desc_t *ios;   /* Pointer to io system information. */
    file_desc_t *file;      /* Pointer to file information. */
    nc_type atttype = PIO_NAT;   /* The type of the attribute. */
    PIO_Offset attlen = 0;      /* Number of elements in the attribute array. */
    PIO_Offset atttype_len = 0; /* Length in bytes of one element of the attribute type. */
    PIO_Offset memtype_len = 0; /* Length in bytes of one element of the memory type. */
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function calls. */
    int ierr = PIO_NOERR;               /* Return code from function calls. */

    GPTLstart("PIO:PIOc_get_att_tc");
    /* Find the info about this file. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        GPTLstop("PIO:PIOc_get_att_tc");
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Reading attribute (%s) from file failed. Invalid file id (ncid=%d) provided", (name) ? name : "NULL", ncid);
    }
    assert(file);
    spio_ltimer_start(file->io_fstats->rd_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);
    ios = file->iosystem;
    assert(ios);

    spio_ltimer_start(ios->io_fstats->rd_timer_name);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    /* User must provide a name and destination pointer. */
    if (!name || !ip || strlen(name) > PIO_MAX_NAME)
    {
        GPTLstop("PIO:PIOc_get_att_tc");
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return pio_err(ios, file, PIO_EINVAL, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) attribute (%s) failed. Invalid arguments provided, Attribute name is %s (expected not NULL), attribute data pointer is %s (expected not NULL), attribute name length = %lld (expected <= %d)", pio_get_vname_from_file(file, varid), varid, (name) ? name : "UNKNOWN", PIO_IS_NULL(name), PIO_IS_NULL(ip), (name) ? ((unsigned long long )strlen(name)) : 0, PIO_MAX_NAME);
    }

    LOG((1, "PIOc_get_att_tc ncid %d varid %d name %s memtype %d",
         ncid, varid, name, memtype));

    /* Run these on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. */
    if (!ios->async || !ios->ioproc)
    {
        /* Get the type and length of the attribute. */
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        ierr = PIOc_inq_att(ncid, varid, name, &atttype, &attlen);
        if(ierr != PIO_NOERR){
            LOG((1, "PIOc_inq_att failed, ierr = %d", ierr));
            GPTLstop("PIO:PIOc_get_att_tc");
            return ierr;
        }
        LOG((2, "atttype = %d attlen = %d", atttype, attlen));

        /* Get the length (in bytes) of the type of the attribute. */
        ierr = PIOc_inq_type(ncid, atttype, NULL, &atttype_len);
        if(ierr != PIO_NOERR){
            LOG((1, "PIOc_inq_type failed, ierr=%d", ierr));
            GPTLstop("PIO:PIOc_get_att_tc");
            return ierr;
        }

        /* Get the length (in bytes) of the type that the user wants
         * the data converted to. */
        if (memtype == PIO_LONG_INTERNAL)
            memtype_len = sizeof(long int);
        else
        {
            ierr = PIOc_inq_type(ncid, memtype, NULL, &memtype_len);
            if(ierr != PIO_NOERR){
                LOG((1, "PIOc_inq_type failed, ierr = %d", ierr));
                GPTLstop("PIO:PIOc_get_att_tc");
                return ierr;
            }
        }
        spio_ltimer_start(ios->io_fstats->rd_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
        spio_ltimer_start(file->io_fstats->rd_timer_name);
        spio_ltimer_start(file->io_fstats->tot_timer_name);
    }
    LOG((2, "atttype_len = %d memtype_len = %d", atttype_len, memtype_len));

    /* If async is in use, and this is not an IO task, bcast the
     * parameters and the attribute and type information we fetched. */
    if (ios->async)
    {
        int msg = PIO_MSG_GET_ATT;
        int namelen = strlen(name) + 1;
        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid, varid, namelen, name,
            file->iotype, atttype, attlen, atttype_len, memtype, memtype_len);
        if(ierr != PIO_NOERR)
        {
            LOG((1, "Error sending async msg for PIO_MSG_GET_ATT"));
            GPTLstop("PIO:PIOc_get_att_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Reading variable (%s, varid=%d) attribute (%s) failed. Error sending asynchronous message, PIO_MSG_GET_ATT", (varid != PIO_GLOBAL) ? file->varlist[varid].vname : "PIO_GLOBAL", varid, name);
        }

        /* Broadcast values currently only known on computation tasks to IO tasks. */
        LOG((2, "PIOc_get_att_tc bcast from comproot = %d attlen = %d atttype_len = %d", ios->comproot, attlen, atttype_len));
        if ((mpierr = MPI_Bcast(&attlen, 1, MPI_OFFSET, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_get_att_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        if ((mpierr = MPI_Bcast(&atttype_len, 1, MPI_OFFSET, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_get_att_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        if ((mpierr = MPI_Bcast(&memtype_len, 1, MPI_OFFSET, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_get_att_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        LOG((2, "PIOc_get_att_tc bcast complete attlen = %d atttype_len = %d memtype_len = %d", attlen, atttype_len,
             memtype_len));
    }

    ios->io_fstats->rb += attlen * atttype_len;
    file->io_fstats->rb += attlen * atttype_len;

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        LOG((2, "calling pnetcdf/netcdf"));
#ifdef _PNETCDF
        if (file->iotype == PIO_IOTYPE_PNETCDF)
        {
            switch(memtype)
            {
            case NC_BYTE:
                ierr = ncmpi_get_att_schar(file->fh, varid, name, ip);
                break;
            case NC_CHAR:
                ierr = ncmpi_get_att_text(file->fh, varid, name, ip);
                break;
            case NC_SHORT:
                ierr = ncmpi_get_att_short(file->fh, varid, name, ip);
                break;
            case NC_INT:
                ierr = ncmpi_get_att_int(file->fh, varid, name, ip);
                break;
            case PIO_LONG_INTERNAL:
                ierr = ncmpi_get_att_long(file->fh, varid, name, ip);
                break;
            case NC_FLOAT:
                ierr = ncmpi_get_att_float(file->fh, varid, name, ip);
                break;
            case NC_DOUBLE:
                ierr = ncmpi_get_att_double(file->fh, varid, name, ip);
                break;
            default:
                GPTLstop("PIO:PIOc_get_att_tc");
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__,
                                "Reading variable (%s, varid=%d) attribute (%s) failed. Unsupported PnetCDF attribute type (type = %x)", (varid != PIO_GLOBAL) ? file->varlist[varid].vname : "PIO_GLOBAL", varid, name, memtype);
            }
        }
#endif /* _PNETCDF */

        if (file->iotype != PIO_IOTYPE_PNETCDF && file->do_io)
        {
            switch(memtype)
            {
#ifdef _NETCDF
            case NC_CHAR:
                ierr = nc_get_att_text(file->fh, varid, name, ip);
                break;
            case NC_BYTE:
                ierr = nc_get_att_schar(file->fh, varid, name, ip);
                break;
            case NC_SHORT:
                ierr = nc_get_att_short(file->fh, varid, name, ip);
                break;
            case NC_INT:
                ierr = nc_get_att_int(file->fh, varid, name, ip);
                break;
            case PIO_LONG_INTERNAL:
                ierr = nc_get_att_long(file->fh, varid, name, ip);
                break;
            case NC_FLOAT:
                ierr = nc_get_att_float(file->fh, varid, name, ip);
                break;
            case NC_DOUBLE:
                ierr = nc_get_att_double(file->fh, varid, name, ip);
                break;
#endif /* _NETCDF */
#ifdef _NETCDF4
            case NC_UBYTE:
                ierr = nc_get_att_uchar(file->fh, varid, name, ip);
                break;
            case NC_USHORT:
                ierr = nc_get_att_ushort(file->fh, varid, name, ip);
                break;
            case NC_UINT:
                ierr = nc_get_att_uint(file->fh, varid, name, ip);
                break;
            case NC_INT64:
                LOG((3, "about to call nc_get_att_longlong"));
                ierr = nc_get_att_longlong(file->fh, varid, name, ip);
                break;
            case NC_UINT64:
                ierr = nc_get_att_ulonglong(file->fh, varid, name, ip);
                break;
                /* case NC_STRING: */
                /*      ierr = nc_get_att_string(file->fh, varid, name, ip); */
                /*      break; */
#endif /* _NETCDF4 */
            default:
                GPTLstop("PIO:PIOc_get_att_tc");
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__,
                                "Reading variable (%s, varid=%d) attribute (%s) failed. Unsupported attribute type (type = %x)", (varid != PIO_GLOBAL) ? file->varlist[varid].vname : "PIO_GLOBAL", varid, name, memtype);
            }
        }
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        LOG((1, "nc*_get_att_* failed, ierr = %d", ierr));
        GPTLstop("PIO:PIOc_get_att_tc");
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) attribute (%s) failed. Internal I/O library (%s) call failed", (varid != PIO_GLOBAL) ? file->varlist[varid].vname : "PIO_GLOBAL", varid, name, pio_iotype_to_string(file->iotype));
    }

    /* Broadcast results to all tasks. */
    LOG((2, "bcasting att values attlen = %d memtype_len = %d", attlen, memtype_len));
    if ((mpierr = MPI_Bcast(ip, (int)attlen * memtype_len, MPI_BYTE, ios->ioroot,
                            ios->my_comm)))
    {
        GPTLstop("PIO:PIOc_get_att_tc");
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    LOG((2, "get_att_tc data bcast complete"));
    GPTLstop("PIO:PIOc_get_att_tc");
    spio_ltimer_stop(ios->io_fstats->rd_timer_name);
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->rd_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return PIO_NOERR;
}

/**
 * Internal PIO function which provides a type-neutral interface to
 * nc_get_vars.
 *
 * Users should not call this function directly. Instead, call one of
 * the derived functions, depending on the type of data you are
 * reading: PIOc_get_vars_text(), PIOc_get_vars_uchar(),
 * PIOc_get_vars_schar(), PIOc_get_vars_ushort(),
 * PIOc_get_vars_short(), PIOc_get_vars_uint(), PIOc_get_vars_int(),
 * PIOc_get_vars_long(), PIOc_get_vars_float(),
 * PIOc_get_vars_double(), PIOc_get_vars_ulonglong(),
 * PIOc_get_vars_longlong()
 *
 * This routine is called collectively by all tasks in the
 * communicator ios.union_comm.
 *
 * @param ncid identifies the netCDF file
 * @param varid the variable ID number
 * @param start an array of start indicies (must have same number of
 * entries as variable has dimensions). If NULL, indices of 0 will be
 * used.
 * @param count an array of counts (must have same number of entries
 * as variable has dimensions). If NULL, counts matching the size of
 * the variable will be used.
 * @param stride an array of strides (must have same number of
 * entries as variable has dimensions). If NULL, strides of 1 will be
 * used.
 * @param xtype the netCDF type of the data being passed in buf. Data
 * will be automatically covnerted from the type of the variable being
 * read from to this type. If PIO_NAT then the variable's file type
 * will be used. Use special PIO_LONG_INTERNAL for _long() functions.
 * @param buf pointer to the data to be written.
 * @return PIO_NOERR on success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_get_vars_tc(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                     const PIO_Offset *stride, nc_type xtype, void *buf)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ndims = 0;         /* The number of dimensions in the variable. */
    PIO_Offset typelen = 0; /* Size (in bytes) of the data type of data in buf. */
    PIO_Offset num_elem = 1; /* Number of data elements in the buffer. */
    nc_type vartype = PIO_NAT; /* The type of the var we are reading from. */
    char start_present = start ? true : false;
    char count_present = count ? true : false;
    char stride_present = stride ? true : false;
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function codes. */
    int ierr = PIO_NOERR;                           /* Return code. */

    GPTLstart("PIO:PIOc_get_vars_tc");
    LOG((1, "PIOc_get_vars_tc ncid = %d varid = %d xtype = %d start_present = %d "
         "count_present = %d stride_present = %d", ncid, varid, xtype, start_present,
         count_present, stride_present));

    /* Find the info about this file. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        GPTLstop("PIO:PIOc_get_vars_tc");
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Reading variable (varid=%d) from file failed. Invalid file id (ncid=%d) provided", varid, ncid);
    }
    assert(file);
    spio_ltimer_start(file->io_fstats->rd_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);
    ios = file->iosystem;
    assert(ios);

    spio_ltimer_start(ios->io_fstats->rd_timer_name);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    /* User must provide a place to put some data. */
    if (!buf)
    {
        GPTLstop("PIO:PIOc_get_vars_tc");
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return pio_err(ios, file, PIO_EINVAL, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. The user buffer (buf) provided is NULL (expected a valid user buffer to read data)", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
    }

    /* Run these on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. */
    if (!ios->async || !ios->ioproc)
    {
        /* Get the type of this var. */
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        ierr = PIOc_inq_vartype(ncid, varid, &vartype);
        if(ierr != PIO_NOERR){
            GPTLstop("PIO:PIOc_get_vars_tc");
            return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Inquiring the variable type failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }

        /* If no type was specified, use the var type. */
        if (xtype == PIO_NAT)
            xtype = vartype;

        /* Handle _long() calls with an special type. */
        if (xtype == PIO_LONG_INTERNAL)
            typelen = sizeof(long int);
        else
        {
            ierr = PIOc_inq_type(ncid, xtype, NULL, &typelen);
            if(ierr != PIO_NOERR){
                GPTLstop("PIO:PIOc_get_vars_tc");
                return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                            "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Inquiring the variable type length failed", pio_get_vname_from_file(file, varid), varid, pio_get_vname_from_file(file, varid), ncid);
            }
        }

        /* Get the number of dims for this var. */
        ierr = PIOc_inq_varndims(ncid, varid, &ndims);
        if(ierr != PIO_NOERR){
            GPTLstop("PIO:PIOc_get_vars_tc");
            return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Inquiring the number of variable dimensions failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }
        spio_ltimer_start(ios->io_fstats->rd_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
        spio_ltimer_start(file->io_fstats->rd_timer_name);
        spio_ltimer_start(file->io_fstats->tot_timer_name);
        LOG((3, "ndims = %d", ndims));

        /* The number of dimensions should be non-negative (zero for scalar vars) */
        pioassert(ndims >= 0, "Unexpected number of dimensions", __FILE__, __LINE__);

        /* Only scalar vars can pass NULL for start/count. */
        pioassert(ndims == 0 || (start && count), "need start/count", __FILE__, __LINE__);

        /* How many elements in buf? (For scalars, ndims is 0 and
         * num_elem will remain 1). */
        for (int vd = 0; vd < ndims; vd++)
            num_elem *= count[vd];
        LOG((2, "PIOc_get_vars_tc num_elem = %d", num_elem));
    }

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_GET_VARS;
        PIO_Offset *amsg_startp = NULL, *amsg_countp = NULL, *amsg_stridep = NULL;
        /* Handle scalars too, ndims == 0 */
        int start_sz = (ndims > 0) ? ndims : 1;
        int count_sz = (ndims > 0) ? ndims : 1;
        int stride_sz = (ndims > 0) ? ndims : 1;
        if(!start_present)
        {
            amsg_startp = calloc(start_sz, sizeof(PIO_Offset));
        }
        if(!count_present)
        {
            amsg_countp = calloc(count_sz, sizeof(PIO_Offset));
        }
        if(!stride_present)
        {
            amsg_stridep = calloc(stride_sz, sizeof(PIO_Offset));
        }

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid, varid, ndims,
                            start_present, start_sz,
                            (start_present) ? start : amsg_startp, 
                            count_present, count_sz,
                            (count_present) ? count : amsg_countp, 
                            stride_present, stride_sz,
                            (stride_present) ? stride : amsg_stridep,
                            xtype, num_elem, typelen);
        if(ierr != PIO_NOERR)
        {
            GPTLstop("PIO:PIOc_get_vars_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Error sending asynchronous message, PIO_MSG_GET_VARS", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }

        if(!start_present)
        {
            free(amsg_startp);
        }
        if(!count_present)
        {
            free(amsg_countp);
        }
        if(!stride_present)
        {
            free(amsg_stridep);
        }

        /* Broadcast values currently only known on computation tasks to IO tasks. */
        if ((mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_get_vars_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        if ((mpierr = MPI_Bcast(&num_elem, 1, MPI_OFFSET, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_get_vars_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        if ((mpierr = MPI_Bcast(&typelen, 1, MPI_OFFSET, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_get_vars_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        if ((mpierr = MPI_Bcast(&xtype, 1, MPI_INT, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_get_vars_tc");
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        LOG((2, "file->iotype = %d xtype = %d file->do_io = %d", file->iotype, xtype, file->do_io));
#ifdef _PNETCDF
        if (file->iotype == PIO_IOTYPE_PNETCDF)
        {
            LOG((2, "pnetcdf calling ncmpi_get_vars_*() file->fh = %d varid = %d", file->fh, varid));
#if PIO_USE_INDEP_MODE
            /* Turn on independent access for pnetcdf file. */
            if ((ierr = ncmpi_begin_indep_data(file->fh)))
            {
                GPTLstop("PIO:PIOc_get_vars_tc");
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, ierr, __FILE__, __LINE__,
                                "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Starting independent (across processes) access failed on the file", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
            }

            /* Only the IO master does the IO, so we are not really
             * getting parallel IO here. */
            if (ios->iomaster == MPI_ROOT)
            {
                switch(xtype)
                {
                case NC_BYTE:
                    ierr = ncmpi_get_vars_schar(file->fh, varid, start, count, stride, buf);
                    break;
                case NC_CHAR:
                    ierr = ncmpi_get_vars_text(file->fh, varid, start, count, stride, buf);
                    break;
                case NC_SHORT:
                    ierr = ncmpi_get_vars_short(file->fh, varid, start, count, stride, buf);
                    break;
                case NC_INT:
                    ierr = ncmpi_get_vars_int(file->fh, varid, start, count, stride, buf);
                    break;
                case PIO_LONG_INTERNAL:
                    ierr = ncmpi_get_vars_long(file->fh, varid, start, count, stride, buf);
                    break;
                case NC_FLOAT:
                    ierr = ncmpi_get_vars_float(file->fh, varid, start, count, stride, buf);
                    break;
                case NC_DOUBLE:
                    ierr = ncmpi_get_vars_double(file->fh, varid, start, count, stride, buf);
                    break;
                default:
                    GPTLstop("PIO:PIOc_get_vars_tc");
                    spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->rd_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__,
                                    "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Unsupported variable type (type=%x)", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, xtype);
                }
            }

            /* Turn off independent access for pnetcdf file. */
            if ((ierr = ncmpi_end_indep_data(file->fh)))
            {
                GPTLstop("PIO:PIOc_get_vars_tc");
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, ierr, __FILE__, __LINE__,
                                "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Ending independent (across processes) access failed on the file", pio_get_vname_from_file(file, varid), varid, pio_get_vname_from_file(file, varid), ncid);
            }
#else
            PIO_Offset *cnt;
            if (ios->iomaster == MPI_ROOT)
                cnt = (PIO_Offset*)count;
            else
            {
                /* non-root reads nothing */
                if (ndims == 0)
                    cnt = NULL; /* scalar variables can pass NULL for start/count */
                else
                {
                    cnt = (PIO_Offset*) calloc(ndims, sizeof(PIO_Offset));
                    if (cnt == NULL)
                    {
                        GPTLstop("PIO:PIOc_get_vars_tc");
                        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->rd_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                       "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Out of memory allocating %lld bytes for a temporary count array",
                                       pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int) (ndims * sizeof(PIO_Offset)));
                    }
                }
            }

            switch(xtype)
            {
            case NC_BYTE:
                ierr = ncmpi_get_vars_schar_all(file->fh, varid, start, cnt, stride, buf);
                break;
            case NC_CHAR:
                ierr = ncmpi_get_vars_text_all(file->fh, varid, start, cnt, stride, buf);
                break;
            case NC_SHORT:
                ierr = ncmpi_get_vars_short_all(file->fh, varid, start, cnt, stride, buf);
                break;
            case NC_INT:
                ierr = ncmpi_get_vars_int_all(file->fh, varid, start, cnt, stride, buf);
                break;
            case PIO_LONG_INTERNAL:
                ierr = ncmpi_get_vars_long_all(file->fh, varid, start, cnt, stride, buf);
                break;
            case NC_FLOAT:
                ierr = ncmpi_get_vars_float_all(file->fh, varid, start, cnt, stride, buf);
                break;
            case NC_DOUBLE:
                ierr = ncmpi_get_vars_double_all(file->fh, varid, start, cnt, stride, buf);
                break;
            default:
                GPTLstop("PIO:PIOc_get_vars_tc");
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__,
                                "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Unsupported variable type (type=%x)", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, xtype);
            }

            if (ios->iomaster != MPI_ROOT)
                free(cnt);
#endif /* PIO_USE_INDEP_MODE */
        }
#endif /* _PNETCDF */

        if (file->iotype != PIO_IOTYPE_PNETCDF && file->do_io)
            switch(xtype)
            {
#ifdef _NETCDF
            case NC_BYTE:
                ierr = nc_get_vars_schar(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_CHAR:
                ierr = nc_get_vars_text(file->fh, varid, (size_t *)start, (size_t *)count,
                                        (ptrdiff_t *)stride, buf);
                break;
            case NC_SHORT:
                ierr = nc_get_vars_short(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_INT:
                ierr = nc_get_vars_int(file->fh, varid, (size_t *)start, (size_t *)count,
                                       (ptrdiff_t *)stride, buf);
                break;
            case PIO_LONG_INTERNAL:
                ierr = nc_get_vars_long(file->fh, varid, (size_t *)start, (size_t *)count,
                                        (ptrdiff_t *)stride, buf);
                break;
            case NC_FLOAT:
                ierr = nc_get_vars_float(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_DOUBLE:
                ierr = nc_get_vars_double(file->fh, varid, (size_t *)start, (size_t *)count,
                                          (ptrdiff_t *)stride, buf);
                break;
#endif
#ifdef _NETCDF4
            case NC_UBYTE:
                ierr = nc_get_vars_uchar(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_USHORT:
                ierr = nc_get_vars_ushort(file->fh, varid, (size_t *)start, (size_t *)count,
                                          (ptrdiff_t *)stride, buf);
                break;
            case NC_UINT:
                ierr = nc_get_vars_uint(file->fh, varid, (size_t *)start, (size_t *)count,
                                        (ptrdiff_t *)stride, buf);
                break;
            case NC_INT64:
                LOG((3, "about to call nc_get_vars_longlong"));
                ierr = nc_get_vars_longlong(file->fh, varid, (size_t *)start, (size_t *)count,
                                            (ptrdiff_t *)stride, buf);
                break;
            case NC_UINT64:
                ierr = nc_get_vars_ulonglong(file->fh, varid, (size_t *)start, (size_t *)count,
                                             (ptrdiff_t *)stride, buf);
                break;
                /* case NC_STRING: */
                /*      ierr = nc_get_vars_string(file->fh, varid, (size_t *)start, (size_t *)count, */
                /*                                (ptrdiff_t *)stride, (void *)buf); */
                /*      break; */
#endif /* _NETCDF4 */
            default:
                GPTLstop("PIO:PIOc_get_vars_tc");
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__,
                                "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Unsupported variable type (type=%x)", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, xtype);
            }
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        LOG((1, "nc*_get_vars_* failed, ierr = %d", ierr));
        GPTLstop("PIO:PIOc_get_vars_tc");
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. The internal I/O library (%s) call failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, pio_iotype_to_string(file->iotype));
    }

    /* Send the data. */
    LOG((2, "PIOc_get_vars_tc bcasting data num_elem = %d typelen = %d ios->ioroot = %d", num_elem,
         typelen, ios->ioroot));
    if ((mpierr = MPI_Bcast(buf, num_elem * typelen, MPI_BYTE, ios->ioroot, ios->my_comm)))
    {
        GPTLstop("PIO:PIOc_get_vars_tc");
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }
    LOG((2, "PIOc_get_vars_tc bcasting data complete"));

    ios->io_fstats->rb += num_elem * typelen;
    file->io_fstats->rb += num_elem * typelen;

    GPTLstop("PIO:PIOc_get_vars_tc");
    spio_ltimer_stop(ios->io_fstats->rd_timer_name);
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->rd_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return PIO_NOERR;
}

/**
 * Get one value of a variable of any type. This is an internal
 * function.
 *
 * This routine is called collectively by all tasks in the
 * communicator ios.union_comm.
 *
 * @param ncid identifies the netCDF file
 * @param varid the variable ID number
 * @param index an array of start indicies (must have same number of
 * entries as variable has dimensions). If NULL, indices of 0 will be
 * used.
 * @param xtype the netcdf type of the variable.
 * @param buf pointer that will get the data.
 * @return PIO_NOERR on success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_get_var1_tc(int ncid, int varid, const PIO_Offset *index, nc_type xtype,
                     void *buf)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ndims;   /* The number of dimensions in the variable. */
    int ierr = PIO_NOERR;    /* Return code from function calls. */

    /* Find the info about this file. We need this for error handling. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Reading variable (varid=%d) from file failed. Invalid file id (ncid=%d) provided", varid, ncid);
    }
    ios = file->iosystem;

    /* Find the number of dimensions. */
    if ((ierr = PIOc_inq_varndims(ncid, varid, &ndims)))
    {
        return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Unable to inquire the number of dimensions in the variable", pio_get_vname_from_file(file, varid), varid, pio_get_vname_from_file(file, varid), file->pio_ncid);
    }

    /* Set up count array. */
    PIO_Offset count[ndims];
    for (int c = 0; c < ndims; c++)
        count[c] = 1;

    return PIOc_get_vars_tc(ncid, varid, index, count, NULL, xtype, buf);
}

/**
 * Get a complete variable of any type. This is an internal function.
 *
 * This routine is called collectively by all tasks in the
 * communicator ios.union_comm.
 *
 * @param ncid identifies the netCDF file
 * @param varid the variable ID number
 * @param index an array of start indicies (must have same number of
 * entries as variable has dimensions). If NULL, indices of 0 will be
 * used.
 * @param xtype the netcdf type of the variable.
 * @param buf pointer that will get the data.
 * @return PIO_NOERR on success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_get_var_tc(int ncid, int varid, nc_type xtype, void *buf)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    PIO_Offset *startp = NULL; /* Pointer to start array. */
    PIO_Offset *countp = NULL; /* Pointer to count array. */
    int ndims;   /* The number of dimensions in the variable. */
    PIO_Offset my_start[PIO_MAX_DIMS];
    PIO_Offset dimlen[PIO_MAX_DIMS];
    int ierr = PIO_NOERR;    /* Return code from function calls. */

    LOG((1, "PIOc_get_var_tc ncid = %d varid = %d xtype = %d", ncid, varid,
         xtype));

    /* Find the info about this file. We need this for error handling. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Reading variable (varid=%d) from file failed. Invalid file id (ncid=%d) failed", varid, ncid);
    }
    ios = file->iosystem;

    /* Find the number of dimensions. */
    if ((ierr = PIOc_inq_varndims(ncid, varid, &ndims)))
    {
        return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Inquiring number of dimensions in the variable failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
    }

    /* Scalar vars (which have ndims == 0) should just pass NULLs for
     * start/count. */
    if (ndims)
    {
        /* Find the dimension IDs. */
        int dimids[ndims];
        if ((ierr = PIOc_inq_vardimid(ncid, varid, dimids)))
        {
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                            "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Inquiring variable dimension ids failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }

        /* Find the dimension lengths. */
        for (int d = 0; d < ndims; d++)
            if ((ierr = PIOc_inq_dimlen(ncid, dimids[d], &dimlen[d])))
            {
                return pio_err(ios, file, ierr, __FILE__, __LINE__,
                                "Reading variable (%s, varid=%d) from file (%s, ncid=%d) failed. Inquiring variable dimension length for dim %d (dimid = %d) failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, d, dimids[d]);
            }

        /* Set up start array. */
        for (int d = 0; d < ndims; d++)
        {
            my_start[d] = 0;
            LOG((3, "my_start[%d] = %d dimlen[%d] = %d", d, my_start[d], d,
                 dimlen[d]));
        }

        /* Set the start/count arrays. */
        startp = my_start;
        countp = dimlen;
    }

    return PIOc_get_vars_tc(ncid, varid, startp, countp, NULL, xtype, buf);
}

/**
 * Internal PIO function which provides a type-neutral interface to
 * nc_put_vars.
 *
 * Users should not call this function directly. Instead, call one of
 * the derived functions, depending on the type of data you are
 * writing: PIOc_put_vars_text(), PIOc_put_vars_uchar(),
 * PIOc_put_vars_schar(), PIOc_put_vars_ushort(),
 * PIOc_put_vars_short(), PIOc_put_vars_uint(), PIOc_put_vars_int(),
 * PIOc_put_vars_long(), PIOc_put_vars_float(),
 * PIOc_put_vars_longlong(), PIOc_put_vars_double(),
 * PIOc_put_vars_ulonglong().
 *
 * This routine is called collectively by all tasks in the
 * communicator ios.union_comm.
 *
 * @param ncid identifies the netCDF file
 * @param varid the variable ID number
 * @param start an array of start indicies (must have same number of
 * entries as variable has dimensions). If NULL, indices of 0 will be
 * used.
 * @param count an array of counts (must have same number of entries
 * as variable has dimensions). If NULL, counts matching the size of
 * the variable will be used.
 * @param stride an array of strides (must have same number of
 * entries as variable has dimensions). If NULL, strides of 1 will be
 * used.
 * @param xtype the netCDF type of the data being passed in buf. Data
 * will be automatically covnerted from this type to the type of the
 * variable being written to. If PIO_NAT then the variable's file type
 * will be used. Use special PIO_LONG_INTERNAL for _long() functions.
 * @param buf pointer to the data to be written.
 *
 * @return PIO_NOERR on success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_put_vars_tc(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                     const PIO_Offset *stride, nc_type xtype, const void *buf)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;  /* Pointer to file information. */
    int ndims = 0;      /* The number of dimensions in the variable. */
    PIO_Offset typelen = 0; /* Size (in bytes) of the data type of data in buf. */
    PIO_Offset num_elem = 1; /* Number of data elements in the buffer. */
    char start_present = start ? true : false;    /* Is start non-NULL? */
    char count_present = count ? true : false;    /* Is count non-NULL? */
    char stride_present = stride ? true : false;  /* Is stride non-NULL? */
    var_desc_t *vdesc;
    int *request = NULL;
    PIO_Offset *request_sz = NULL;
    nc_type vartype = PIO_NAT;   /* The type of the var we are reading from. */
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function codes. */
    int ierr = PIO_NOERR;          /* Return code from function calls. */

    GPTLstart("PIO:PIOc_put_vars_tc");
    GPTLstart("PIO:write_total");
    LOG((1, "PIOc_put_vars_tc ncid = %d varid = %d start_present = %d "
         "count_present = %d stride_present = %d xtype = %d", ncid, varid,
         start_present, count_present, stride_present, xtype));

    /* Get file info. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        GPTLstop("PIO:PIOc_put_vars_tc");
        GPTLstop("PIO:write_total");
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Writing variable (varid=%d) to file failed. Invalid file id (ncid=%d) provided", varid, ncid);
    }
    assert(file);
    spio_ltimer_start(file->io_fstats->wr_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);
    ios = file->iosystem;
    assert(ios);

    spio_ltimer_start(ios->io_fstats->wr_timer_name);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        GPTLstart("PIO:PIOc_put_vars_tc_adios");
        GPTLstart("PIO:write_total_adios");
    }

    /* User must provide a place to put some data. */
    if (!buf)
    {
        GPTLstop("PIO:PIOc_put_vars_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        if (file->iotype == PIO_IOTYPE_ADIOS)
        {
            GPTLstop("PIO:PIOc_put_vars_tc_adios");
            GPTLstop("PIO:write_total_adios");
        }
        return pio_err(ios, file, PIO_EINVAL, __FILE__, __LINE__,
                        "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Invalid/NULL user buffer provided", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
    }

    /* Run these on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. */
    if (!ios->async || !ios->ioproc)
    {
        /* Get the type of this var. */
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        ierr = PIOc_inq_vartype(ncid, varid, &vartype);
        if(ierr != PIO_NOERR){
            GPTLstop("PIO:PIOc_put_vars_tc");
            GPTLstop("PIO:write_total");
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_vars_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                            "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Inquiring variable type failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }

        /* If no type was specified, use the var type. */
        if (xtype == PIO_NAT)
            xtype = vartype;

        /* Get the number of dims for this var. */
        ierr = PIOc_inq_varndims(ncid, varid, &ndims);
        if(ierr != PIO_NOERR){
            GPTLstop("PIO:PIOc_put_vars_tc");
            GPTLstop("PIO:write_total");
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_vars_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                            "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Inquiring number of dimensions of the variable failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }

        /* Get the length of the data type. */
        if (xtype == PIO_LONG_INTERNAL)
            typelen = sizeof(long int);
        else
        {
            ierr = PIOc_inq_type(ncid, xtype, NULL, &typelen);
            if(ierr != PIO_NOERR){
                GPTLstop("PIO:PIOc_put_vars_tc");
                GPTLstop("PIO:write_total");
                if (file->iotype == PIO_IOTYPE_ADIOS)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                }
                return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                                "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Inquiring variable type length failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
            }
        }
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
        spio_ltimer_start(file->io_fstats->wr_timer_name);
        spio_ltimer_start(file->io_fstats->tot_timer_name);

        LOG((2, "ndims = %d typelen = %d", ndims, typelen));

        /* How many elements of data? If no count array was passed,
         * this is a scalar. */
        if (count)
            for (int vd = 0; vd < ndims; vd++)
                num_elem *= count[vd];
    }

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_PUT_VARS;
        PIO_Offset *amsg_startp = NULL, *amsg_countp = NULL, *amsg_stridep = NULL;
        /* Make sure we handle scalars too, ndims == 0 */
        int start_sz = (ndims > 0) ? ndims : 1;
        int count_sz = (ndims > 0) ? ndims : 1;
        int stride_sz = (ndims > 0) ? ndims : 1;
        if(!start_present)
        {
            amsg_startp = calloc(start_sz, sizeof(PIO_Offset));
        }
        if(!count_present)
        {
            amsg_countp = calloc(count_sz, sizeof(PIO_Offset));
        }
        if(!stride_present)
        {
            amsg_stridep = calloc(stride_sz, sizeof(PIO_Offset));
        }

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid, varid, ndims,
                            start_present, start_sz,
                            (start_present) ? start : amsg_startp, 
                            count_present, count_sz,
                            (count_present) ? count : amsg_countp, 
                            stride_present, stride_sz,
                            (stride_present) ? stride : amsg_stridep,
                            xtype, num_elem, typelen,
                            num_elem * typelen, buf); 
        if(ierr != PIO_NOERR)
        {
            GPTLstop("PIO:PIOc_put_vars_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_vars_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Error sending asynchronous message, PIO_MSG_PUT_VARS", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }

        if(!start_present)
        {
            free(amsg_startp);
        }
        if(!count_present)
        {
            free(amsg_countp);
        }
        if(!stride_present)
        {
            free(amsg_stridep);
        }

        /* Broadcast values currently only known on computation tasks to IO tasks. */
        LOG((2, "PIOc_put_vars_tc bcast from comproot"));
        if ((mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_put_vars_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_vars_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        if ((mpierr = MPI_Bcast(&xtype, 1, MPI_INT, ios->comproot, ios->my_comm)))
        {
            GPTLstop("PIO:PIOc_put_vars_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_put_vars_tc_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
        LOG((2, "PIOc_put_vars_tc complete bcast from comproot ndims = %d", ndims));
    }

    /* ADIOS: assume all procs are also IO tasks */
#ifdef _ADIOS2
    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        if (varid < 0 || varid >= file->num_vars)
        {
            GPTLstop("PIO:PIOc_put_vars_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            GPTLstop("PIO:PIOc_put_vars_tc_adios");
            GPTLstop("PIO:write_total_adios");
            return pio_err(ios, file, PIO_EBADID, __FILE__, __LINE__,
                           "Writing variable to file (%s, ncid=%d) failed. Invalid variable id (varid=%d, expected >=0 and < number of variables in the file, %d) provided",
                           pio_get_fname_from_file(file), ncid, varid, file->num_vars);
        }

        ierr = begin_adios2_step(file, ios);
        if (ierr != PIO_NOERR)
        {
            GPTLstop("PIO:PIOc_put_vars_tc");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            GPTLstop("PIO:PIOc_put_vars_tc_adios");
            GPTLstop("PIO:write_total_adios");
            return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                           "adios2_begin_step failed for file (%s)", pio_get_fname_from_file(file));
        }

        adios2_error adiosErr = adios2_error_none;

        /* First we need to define the variable now that we know it's decomposition */
        adios_var_desc_t *av = &(file->adios_vars[varid]);

        /* Write ADIOS with memory type since ADIOS does not do conversions.
         * Add an attribute describing the target output type (defined type).
         */
        if (xtype == PIO_NAT)
            xtype = vartype;

        if (xtype == PIO_LONG_INTERNAL)
        {
            int typesize = sizeof(long int);
            if (typesize == 4)
                xtype = PIO_INT;
            else
                xtype = PIO_INT64;
        }

        /* No conversion to vartype will be made. Use the in-memory type of the data buffer */
        av->adios_type = PIOc_get_adios_type(xtype);
        av->adios_type_size = get_adios2_type_size(av->adios_type, NULL);
        assert(av->adios_type_size > 0);

        char vname[PIO_MAX_NAME];

        /* Scalars have to be handled differently. */
        if (av->ndims == 0)
        {
            /* Only the IO master does the IO, so we are not really
             * getting parallel IO here. */
            if (file->adios_io_process == 1 && file->adios_rank == 0)
            {
                if (start)
                {
                    /* Ignore if user specifies start[0] == 0 */
                    if (start[0] != 0)
                    {
                        printf("PIO: WARNING: Ignoring user-specified start indices while writing the scalar variable (%s) to file (%s, ncid=%d). An invalid start index (start[0] = %lld) provided (%s:%d)\n", av->name, pio_get_fname_from_file(file), file->pio_ncid, (long long int)start[0], __FILE__, __LINE__);
                    }
                }
                if (count)
                {
                    /* Ignore if user specifies count[0] == 1 */
                    if (count[0] != 1)
                    {
                        printf("PIO: WARNING: Ignoring user-specified counts while writing the scalar variable (%s) to file (%s, ncid=%d). An invalid count (count[0] = %lld) provided (%s:%d)\n", av->name, pio_get_fname_from_file(file), file->pio_ncid, (long long int)count[0], __FILE__, __LINE__);
                    }
                }
                if (stride)
                {
                    /* Ignore if user specifies stride[0] == 1 */
                    if (stride[0] != 1)
                    {
                        printf("PIO: WARNING: Ignoring user-specified strides while writing the scalar variable (%s) to file (%s, ncid=%d). An invalid stride (stride[0] = %lld) provided (%s:%d)\n", av->name, pio_get_fname_from_file(file), file->pio_ncid, (long long int)stride[0], __FILE__, __LINE__);
                    }
                }

                ios->io_fstats->wb += num_elem * typelen;
                file->io_fstats->wb += num_elem * typelen;

                assert((strlen("/__pio__/var/") + strlen(av->name)) < PIO_MAX_NAME);
                snprintf(vname, PIO_MAX_NAME, "/__pio__/var/%s", av->name);
                av->adios_varid = adios2_inquire_variable(file->ioH, vname);
                if (av->adios_varid == NULL)
                {
                    av->adios_varid = adios2_define_variable(file->ioH, vname, av->adios_type,
                                                             0, NULL, NULL, NULL,
                                                             adios2_constant_dims_false);
                    if (av->adios_varid == NULL)
                    {
                        GPTLstop("PIO:PIOc_put_vars_tc");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        GPTLstop("PIO:PIOc_put_vars_tc_adios");
                        GPTLstop("PIO:write_total_adios");
                        return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                    "Defining (ADIOS) variable (name=%s) failed for file (%s, ncid=%d)",
                                    vname, pio_get_fname_from_file(file), file->pio_ncid);
                    }
                }

                adiosErr = adios2_put(file->engineH, av->adios_varid, buf, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   vname, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }
                (file->num_written_blocks)++;
            }
        }
        else
        {
            /* This is not a scalar var. */

            if (stride_present)
            {
                LOG((2, "ADIOS does not support striding %s:%s\n"
                        "Variable %s will be corrupted in the output"
                        , __FILE__, __func__, av->name));
                GPTLstop("PIO:PIOc_put_vars_tc");
                GPTLstop("PIO:write_total");
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                GPTLstop("PIO:PIOc_put_vars_tc_adios");
                GPTLstop("PIO:write_total_adios");
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "ADIOS does not support striding. Variable %s file (%s, ncid=%d)",
                               av->name, pio_get_fname_from_file(file), file->pio_ncid);
            }

            /* Only the IO master actually does these ADIOS calls. */
            if (file->adios_io_process == 1 && file->adios_rank == 0)
            {
                /* Write start and count arrays to be able to reconstruct the variable during conversion. */
                int64_t pio_var_start[PIO_MAX_DIMS], pio_var_count[PIO_MAX_DIMS];
                if (start)
                {
                    for (int d = 0; d < av->ndims; d++)
                    {
                        pio_var_start[d] = (int64_t)start[d];
                    }
                }
                else
                {
                    pio_var_start[0] = -1; /* if start is NULL */
                }

                if (count)
                {
                    for (int d = 0; d < av->ndims; d++)
                    {
                        pio_var_count[d] = (int64_t)count[d];
                    }
                }
                else
                {
                    pio_var_count[0] = -1; /* if count is NULL */
                }

                /* Create a one-dimensional byte array to combine start, count and buf */
                size_t av_size  = 2 * av->ndims * sizeof(int64_t); /* pio_var_start and pio_var_count */
                size_t buf_size = 1;
                if (count)
                {
                    for (int d = 0; d < av->ndims; d++)
                    {
                        if (file->dim_values[av->gdimids[d]] != PIO_UNLIMITED)
                        {
                            buf_size *= (size_t)count[d];
                        }
                    }
                }
                else
                {
                    for (int d = 0; d < av->ndims; d++)
                    {
                        if (file->dim_values[av->gdimids[d]] != PIO_UNLIMITED)
                        {
                            buf_size *= (size_t)file->dim_values[av->gdimids[d]];
                        }
                    }
                }
                buf_size *= av->adios_type_size;
                av_size += buf_size;

                ios->io_fstats->wb += num_elem * typelen;
                file->io_fstats->wb += num_elem * typelen;

                /* PIOc_put_var may be called multiple times with different start/count values
                 * for a variable. ADIOS should output data for each of those calls not just
                 * when the variable is not defined */
                assert((strlen("/__pio__/var/") + strlen(av->name)) < PIO_MAX_NAME);
                snprintf(vname, PIO_MAX_NAME, "/__pio__/var/%s", av->name);
                av->adios_varid = adios2_inquire_variable(file->ioH, vname);
                if (av->adios_varid == NULL)
                {
                    av->adios_varid = adios2_define_variable(file->ioH, vname, adios2_type_uint8_t,
                                                             1, NULL, NULL, &av_size,
                                                             adios2_constant_dims_false);
                    if (av->adios_varid == NULL)
                    {
                        GPTLstop("PIO:PIOc_put_vars_tc");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        GPTLstop("PIO:PIOc_put_vars_tc_adios");
                        GPTLstop("PIO:write_total_adios");
                        return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Defining (ADIOS) variable (name=%s) failed for file (%s, ncid=%d)",
                                       vname, pio_get_fname_from_file(file), file->pio_ncid);
                    }
                }
                else
                {
                    adiosErr = adios2_set_selection(av->adios_varid, 1, NULL, &av_size);
                    if (adiosErr != adios2_error_none)
                    {
                        GPTLstop("PIO:PIOc_put_vars_tc");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        GPTLstop("PIO:PIOc_put_vars_tc_adios");
                        GPTLstop("PIO:write_total_adios");
                        return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Setting (ADIOS) selection to variable (name=%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                       av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                    }
                }

                char *mem_buffer = (char*)calloc(av_size, sizeof(char));
                if (mem_buffer == NULL)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                   "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Out of memory, allocating memory (%lld bytes) for putting ADIOS variable (name = %s)",
                                   pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int) (av_size * sizeof(unsigned char)), av->name);
                }
                char *tmp_buf = mem_buffer;
                memcpy(tmp_buf, pio_var_start, av->ndims * sizeof(int64_t));
                tmp_buf += (av->ndims * sizeof(int64_t));
                memcpy(tmp_buf, pio_var_count, av->ndims * sizeof(int64_t));
                tmp_buf += (av->ndims * sizeof(int64_t));
                memcpy(tmp_buf, buf, buf_size);
                adiosErr = adios2_put(file->engineH, av->adios_varid, mem_buffer, adios2_mode_sync);
                free(mem_buffer);
                mem_buffer = NULL;
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }

                char* dimnames[PIO_MAX_DIMS];
                assert(av->ndims <= PIO_MAX_DIMS);

                /* Record the NC dimensions in an attribute, including the unlimited dimension */
                for (int i = 0; i < av->ndims; i++)
                {
                    dimnames[i] = file->dim_names[av->gdimids[i]];
                }

                char att_name[PIO_MAX_NAME];
                assert((strlen("/__pio__/var/") + strlen("/def/dims") + strlen(av->name)) < PIO_MAX_NAME);
                snprintf(att_name, PIO_MAX_NAME, "/__pio__/var/%s/def/dims", av->name);
                adios2_attribute *attributeH = adios2_inquire_attribute(file->ioH, att_name);
                if (attributeH == NULL)
                {
                    attributeH = adios2_define_attribute_array(file->ioH, att_name, adios2_type_string, dimnames, av->ndims);
                    if (attributeH == NULL)
                    {
                        GPTLstop("PIO:PIOc_put_vars_tc");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        GPTLstop("PIO:PIOc_put_vars_tc_adios");
                        GPTLstop("PIO:write_total_adios");
                        return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Defining (ADIOS) attribute array (name=%s, size=%d) failed for file (%s, ncid=%d)",
                                       att_name, av->ndims, pio_get_fname_from_file(file), file->pio_ncid);
                    }
                }
                file->num_written_blocks += 2;
            }
        }

        /* Only the IO master actually does these ADIOS calls. */
        if (file->adios_io_process == 1 && file->adios_rank == 0)
        {
            char att_name[PIO_MAX_NAME];
            assert((strlen("/__pio__/var/") + strlen("/def/ndims") + strlen(av->name)) < PIO_MAX_NAME);
            snprintf(att_name, PIO_MAX_NAME, "/__pio__/var/%s/def/ndims", av->name);
            adios2_attribute *attributeH = adios2_inquire_attribute(file->ioH, att_name);
            if (attributeH == NULL)
            {
                attributeH = adios2_define_attribute(file->ioH, att_name, adios2_type_int32_t, &av->ndims);
                if (attributeH == NULL)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Defining (ADIOS) attribute (name=%s) failed for file (%s, ncid=%d)",
                                   att_name, pio_get_fname_from_file(file), file->pio_ncid);
                }
            }

            assert((strlen("/__pio__/var/") + strlen("/def/nctype") + strlen(av->name)) < PIO_MAX_NAME);
            snprintf(att_name, PIO_MAX_NAME, "/__pio__/var/%s/def/nctype", av->name);
            attributeH = adios2_inquire_attribute(file->ioH, att_name);
            if (attributeH == NULL)
            {
                attributeH = adios2_define_attribute(file->ioH, att_name, adios2_type_int32_t, &av->nc_type);
                if (attributeH == NULL)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Defining (ADIOS) attribute (name=%s) failed for file (%s, ncid=%d)",
                                   att_name, pio_get_fname_from_file(file), file->pio_ncid);
                }
            }

            /* Need to save adios type for conversion, since we merge blocks as char arrays */
            assert((strlen("/__pio__/var/") + strlen("/def/adiostype") + strlen(av->name)) < PIO_MAX_NAME);
            snprintf(att_name, PIO_MAX_NAME, "/__pio__/var/%s/def/adiostype", av->name);
            attributeH = adios2_inquire_attribute(file->ioH, att_name);
            if (attributeH == NULL)
            {
                int save_adios_type = (int) (av->adios_type);
                attributeH = adios2_define_attribute(file->ioH, att_name, adios2_type_int32_t, &save_adios_type);
                if (attributeH == NULL)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Defining (ADIOS) attribute (name=%s) failed for file (%s, ncid=%d)",
                                   att_name, pio_get_fname_from_file(file), file->pio_ncid);
                }
            }

            assert((strlen("/__pio__/var/") + strlen("/def/ncop") + strlen(av->name)) < PIO_MAX_NAME);
            snprintf(att_name, PIO_MAX_NAME, "/__pio__/var/%s/def/ncop", av->name);
            attributeH = adios2_inquire_attribute(file->ioH, att_name);
            if (attributeH == NULL)
            {
                attributeH = adios2_define_attribute(file->ioH, att_name, adios2_type_string, "put_var");
                if (attributeH == NULL)
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    GPTLstop("PIO:PIOc_put_vars_tc_adios");
                    GPTLstop("PIO:write_total_adios");
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Defining (ADIOS) attribute (name=%s) failed for file (%s, ncid=%d)",
                                   att_name, pio_get_fname_from_file(file), file->pio_ncid);
                }
            }
            file->num_written_blocks += 4;
        }

        GPTLstop("PIO:PIOc_put_vars_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        GPTLstop("PIO:PIOc_put_vars_tc_adios");
        GPTLstop("PIO:write_total_adios");

        return PIO_NOERR;
    }
#endif

#ifdef _HDF5
    if (file->iotype == PIO_IOTYPE_HDF5)
    {
        if (ios->ioproc)
        {
            hsize_t dims[H5S_MAX_RANK];
            hsize_t mdims[H5S_MAX_RANK];

            hid_t file_space_id = H5Dget_space(file->hdf5_vars[varid].hdf5_dataset_id);
            H5Sget_simple_extent_dims(file_space_id, dims, mdims);

            int ndims = file->hdf5_vars[varid].ndims;

            /* Extend record dimension if needed */
            if (ndims > 0 && start != NULL && count != NULL && mdims[0] == H5S_UNLIMITED && dims[0] < (hsize_t)(start[0] + count[0]))
            {
                dims[0] = (hsize_t) (start[0] + count[0]);
                H5Sclose(file_space_id);
                H5Dextend(file->hdf5_vars[varid].hdf5_dataset_id, dims);
                file_space_id = H5Dget_space(file->hdf5_vars[varid].hdf5_dataset_id);
            }

            /* Only the IO master does the IO */
            if (ios->iomaster == MPI_ROOT)
            {
                hsize_t hstart[H5S_MAX_RANK];
                hsize_t hcount[H5S_MAX_RANK];
                hsize_t hstride[H5S_MAX_RANK];

                if (start)
                {
                    for (int i = 0; i < ndims; i++)
                        hstart[i] = (hsize_t)start[i];
                }
                else
                {
                    for (int i = 0; i < ndims; i++)
                        hstart[i] = 0;
                }

                if (count)
                {
                    for (int i = 0; i < ndims; i++)
                        hcount[i] = (hsize_t)count[i];
                }
                else
                {
                    for (int i = 0; i < ndims; i++)
                        hcount[i] = dims[i];
                }

                if (stride)
                {
                    for (int i = 0; i < ndims; i++)
                        hstride[i] = (hsize_t)stride[i];
                }
                else
                {
                    for (int i = 0; i < ndims; i++)
                        hstride[i] = 1;
                }

                hid_t mem_space_id = H5Screate_simple(ndims, hcount, hcount);

                if (ndims > 0)
                    H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, hstart, hstride, hcount, NULL);


                hid_t mem_type_id;
                if (xtype == NC_CHAR)
                {
                    /* String type */
                    mem_type_id = H5Tcopy(H5T_C_S1);
                    H5Tset_strpad(mem_type_id, H5T_STR_NULLTERM);
                    H5Tset_cset(mem_type_id, H5T_CSET_ASCII);
                }
                else
                    mem_type_id = nc_type_to_hdf5_type(xtype);

                /* Independent write */
                H5Dwrite(file->hdf5_vars[varid].hdf5_dataset_id, mem_type_id, mem_space_id, file_space_id, file->dxplid_indep, buf);

                H5Sclose(mem_space_id);

                if (xtype == NC_CHAR)
                    H5Tclose(mem_type_id);
            }

            H5Sclose(file_space_id);
        }

        GPTLstop("PIO:PIOc_put_vars_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);

        return PIO_NOERR;
    }
#endif /* _HDF5 */

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _PNETCDF
        if (file->iotype == PIO_IOTYPE_PNETCDF)
        {
            LOG((2, "PIOc_put_vars_tc calling pnetcdf function"));
            vdesc = &file->varlist[varid];
            if (vdesc->nreqs % PIO_REQUEST_ALLOC_CHUNK == 0)
            {
                if (!(vdesc->request = realloc(vdesc->request,
                                               sizeof(int) * (vdesc->nreqs + PIO_REQUEST_ALLOC_CHUNK))))
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                    "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Out of memory, reallocating memory (%lld bytes) for array to store PnetCDF request handles", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int) (sizeof(int) * (vdesc->nreqs + PIO_REQUEST_ALLOC_CHUNK)));
                }
            
                vdesc->request_sz = realloc(vdesc->request_sz,
                                            sizeof(PIO_Offset) *
                                            (vdesc->nreqs +
                                              PIO_REQUEST_ALLOC_CHUNK));
                if(!(vdesc->request_sz))
                {
                    GPTLstop("PIO:PIOc_put_vars_tc");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                    "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Out of memory, reallocating memory (%lld bytes) for array to store PnetCDF request handles", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int) (sizeof(int) * (vdesc->nreqs + PIO_REQUEST_ALLOC_CHUNK)));
                }
            }
            request = vdesc->request + vdesc->nreqs;
            LOG((2, "PIOc_put_vars_tc request = %d", vdesc->request));
            request_sz = vdesc->request_sz + vdesc->nreqs;

            /* Scalars have to be handled differently. */
            if (ndims == 0)
            {
                /* This is a scalar var. */
                LOG((2, "pnetcdf writing scalar with ncmpi_put_vars_*() file->fh = %d varid = %d",
                     file->fh, varid));

                /* Only the IO master does the IO, so we are not really
                 * getting parallel IO here. */
                if (ios->iomaster == MPI_ROOT)
                {
                    if (start)
                    {
                        /* Ignore if user specifies start[0] == 0 */
                        if (start[0] != 0)
                        {
                            printf("PIO: WARNING: Ignoring user-specified start indices while writing the scalar variable (%s, varid=%d) to file (%s, ncid=%d). An invalid start index (start[0] = %lld) provided (%s:%d)\n", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int)start[0], __FILE__, __LINE__);
                        }
                    }
                    if (count)
                    {
                        /* Ignore if user specifies count[0] == 1 */
                        if (count[0] != 1)
                        {
                            printf("PIO: WARNING: Ignoring user-specified counts while writing the scalar variable (%s, varid=%d) to file (%s, ncid=%d). An invalid count (count[0] = %lld) provided (%s:%d)\n", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int)count[0], __FILE__, __LINE__);
                        }
                    }
                    if (stride)
                    {
                        /* Ignore if user specifies stride[0] == 1 */
                        if (stride[0] != 1)
                        {
                            printf("PIO: WARNING: Ignoring user-specified strides while writing the scalar variable (%s, varid=%d) to file (%s, ncid=%d). An invalid stride (stride[0] = %lld) provided (%s:%d)\n", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int)stride[0], __FILE__, __LINE__);
                        }
                    }

                    ios->io_fstats->wb += num_elem * typelen;
                    file->io_fstats->wb += num_elem * typelen;

                    switch(xtype)
                    {
                    case NC_BYTE:
                        ierr = ncmpi_bput_var_schar(file->fh, varid, buf, request);
                        break;
                    case NC_CHAR:
                        ierr = ncmpi_bput_var_text(file->fh, varid, buf, request);
                        break;
                    case NC_SHORT:
                        ierr = ncmpi_bput_var_short(file->fh, varid, buf, request);
                        break;
                    case NC_INT:
                        ierr = ncmpi_bput_var_int(file->fh, varid, buf, request);
                        break;
                    case PIO_LONG_INTERNAL:
                        ierr = ncmpi_bput_var_long(file->fh, varid, buf, request);
                        break;
                    case NC_FLOAT:
                        ierr = ncmpi_bput_var_float(file->fh, varid, buf, request);
                        break;
                    case NC_DOUBLE:
                        ierr = ncmpi_bput_var_double(file->fh, varid, buf, request);
                        break;
                    default:
                        GPTLstop("PIO:PIOc_put_vars_tc");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__,
                                        "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Unsupported PnetCDF variable type (type=%x)", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, xtype);
                    }
                    LOG((2, "PIOc_put_vars_tc io_rank 0 done with pnetcdf call, ierr=%d", ierr));
                    *request_sz = num_elem * typelen;
                }
                else
                {
                    *request = PIO_REQ_NULL;
                    *request_sz = 0;
                }

                vdesc->nreqs++;
                if (ierr == PIO_NOERR)
                {
                    ierr = flush_output_buffer(file, false, 0);
                    LOG((2, "PIOc_put_vars_tc flushed output buffer, ierr=%d", ierr));
                }
            }
            else
            {
                /* This is not a scalar var. */
                PIO_Offset *fake_stride;

                if (!stride_present)
                {
                    LOG((2, "stride not present"));
                    if (!(fake_stride = malloc(ndims * sizeof(PIO_Offset))))
                    {
                        GPTLstop("PIO:PIOc_put_vars_tc");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                        "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Out of memory, allocating memory (%lld bytes) for default variable stride", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, (long long int) (ndims * sizeof(PIO_Offset)));
                    }
                    for (int d = 0; d < ndims; d++)
                        fake_stride[d] = 1;
                }
                else
                    fake_stride = (PIO_Offset *)stride;

                /* Only the IO master actually does the call. */
                if (ios->iomaster == MPI_ROOT)
                {
                    ios->io_fstats->wb += num_elem * typelen;
                    file->io_fstats->wb += num_elem * typelen;
                    switch(xtype)
                    {
                    case NC_BYTE:
                        ierr = ncmpi_bput_vars_schar(file->fh, varid, start, count, fake_stride, buf, request);
                        break;
                    case NC_CHAR:
                        ierr = ncmpi_bput_vars_text(file->fh, varid, start, count, fake_stride, buf, request);
                        break;
                    case NC_SHORT:
                        ierr = ncmpi_bput_vars_short(file->fh, varid, start, count, fake_stride, buf, request);
                        break;
                    case NC_INT:
                        ierr = ncmpi_bput_vars_int(file->fh, varid, start, count, fake_stride, buf, request);
                        break;
                    case PIO_LONG_INTERNAL:
                        ierr = ncmpi_bput_vars_long(file->fh, varid, start, count, fake_stride, buf, request);
                        break;
                    case NC_FLOAT:
                        ierr = ncmpi_bput_vars_float(file->fh, varid, start, count, fake_stride, buf, request);
                        break;
                    case NC_DOUBLE:
                        ierr = ncmpi_bput_vars_double(file->fh, varid, start, count, fake_stride, buf, request);
                        break;
                    default:
                        GPTLstop("PIO:PIOc_put_vars_tc");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__,
                                        "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Unsupported PnetCDF variable type (%x)", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, xtype);
                    }
                    LOG((2, "PIOc_put_vars_tc io_rank 0 done with pnetcdf call, ierr=%d", ierr));
                    *request_sz = num_elem * typelen;
                }
                else
                {
                    *request = PIO_REQ_NULL;
                    *request_sz = 0;
                }

                vdesc->nreqs++;
                if (ierr == PIO_NOERR)
                {
                    ierr = flush_output_buffer(file, false, 0);
                    LOG((2, "PIOc_put_vars_tc flushed output buffer, ierr=%d", ierr));
                }

                /* Free malloced resources. */
                if (!stride_present)
                    free(fake_stride);
            } /* endif ndims == 0 */
        }
#endif /* _PNETCDF */

        if (file->iotype != PIO_IOTYPE_PNETCDF && file->iotype != PIO_IOTYPE_ADIOS && file->iotype != PIO_IOTYPE_HDF5 && file->do_io)
        {
            LOG((2, "PIOc_put_vars_tc calling netcdf function file->iotype = %d",
                 file->iotype));
            ios->io_fstats->wb += num_elem * typelen;
            file->io_fstats->wb += num_elem * typelen;
            switch(xtype)
            {
#ifdef _NETCDF
            case NC_BYTE:
                ierr = nc_put_vars_schar(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_CHAR:
                ierr = nc_put_vars_text(file->fh, varid, (size_t *)start, (size_t *)count,
                                        (ptrdiff_t *)stride, buf);
                break;
            case NC_SHORT:
                ierr = nc_put_vars_short(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_INT:
                ierr = nc_put_vars_int(file->fh, varid, (size_t *)start, (size_t *)count,
                                       (ptrdiff_t *)stride, buf);
                break;
            case PIO_LONG_INTERNAL:
                ierr = nc_put_vars_long(file->fh, varid, (size_t *)start, (size_t *)count,
                                        (ptrdiff_t *)stride, buf);
                break;
            case NC_FLOAT:
                ierr = nc_put_vars_float(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_DOUBLE:
                ierr = nc_put_vars_double(file->fh, varid, (size_t *)start, (size_t *)count,
                                          (ptrdiff_t *)stride, buf);
                break;
#endif
#ifdef _NETCDF4
            case NC_UBYTE:
                ierr = nc_put_vars_uchar(file->fh, varid, (size_t *)start, (size_t *)count,
                                         (ptrdiff_t *)stride, buf);
                break;
            case NC_USHORT:
                ierr = nc_put_vars_ushort(file->fh, varid, (size_t *)start, (size_t *)count,
                                          (ptrdiff_t *)stride, buf);
                break;
            case NC_UINT:
                ierr = nc_put_vars_uint(file->fh, varid, (size_t *)start, (size_t *)count,
                                        (ptrdiff_t *)stride, buf);
                break;
            case NC_INT64:
                ierr = nc_put_vars_longlong(file->fh, varid, (size_t *)start, (size_t *)count,
                                            (ptrdiff_t *)stride, buf);
                break;
            case NC_UINT64:
                ierr = nc_put_vars_ulonglong(file->fh, varid, (size_t *)start, (size_t *)count,
                                             (ptrdiff_t *)stride, buf);
                break;
                /* case NC_STRING: */
                /*      ierr = nc_put_vars_string(file->fh, varid, (size_t *)start, (size_t *)count, */
                /*                                (ptrdiff_t *)stride, (void *)buf); */
                /*      break; */
#endif /* _NETCDF4 */
            default:
                GPTLstop("PIO:PIOc_put_vars_tc");
                GPTLstop("PIO:write_total");
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__,
                                "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Unsupported variable type (%x) for iotype=%s", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, xtype, pio_iotype_to_string(file->iotype));
            }
            LOG((2, "PIOc_put_vars_tc io_rank 0 done with netcdf call, ierr=%d", ierr));
        }
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        LOG((1, "nc*_put_vars_* failed, ierr = %d", ierr));
        GPTLstop("PIO:PIOc_put_vars_tc");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                        "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Underlying I/O library call failed(iotype=%x:%s) ", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, xtype, pio_iotype_to_string(file->iotype));
                    
    }

    LOG((2, "PIOc_put_vars_tc bcast netcdf return code %d complete", ierr));

    GPTLstop("PIO:PIOc_put_vars_tc");
    GPTLstop("PIO:write_total");
    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->wr_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return PIO_NOERR;
}

/**
 * Internal PIO function which provides a type-neutral interface to
 * nc_put_var1 calls.
 *
 * Users should not call this function directly. Instead, call one of
 * the derived functions, depending on the type of data you are
 * writing: PIOc_put_var1_text(), PIOc_put_var1_uchar(),
 * PIOc_put_var1_schar(), PIOc_put_var1_ushort(),
 * PIOc_put_var1_short(), PIOc_put_var1_uint(), PIOc_put_var1_int(),
 * PIOc_put_var1_long(), PIOc_put_var1_float(),
 * PIOc_put_var1_longlong(), PIOc_put_var1_double(),
 * PIOc_put_var1_ulonglong().
 *
 * This routine is called collectively by all tasks in the
 * communicator ios.union_comm.
 *
 * @param ncid identifies the netCDF file
 * @param varid the variable ID number
 * @param index an array of start indicies (must have same number of
 * entries as variable has dimensions). If NULL, indices of 0 will be
 * used.
 * @param xtype the netCDF type of the data being passed in buf. Data
 * will be automatically covnerted from this type to the type of the
 * variable being written to.
 * @param op pointer to the data to be written.
 *
 * @return PIO_NOERR on success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_put_var1_tc(int ncid, int varid, const PIO_Offset *index, nc_type xtype,
                     const void *op)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ndims;   /* The number of dimensions in the variable. */
    int ierr = PIO_NOERR;    /* Return code from function calls. */

    /* Find the info about this file. We need this for error handling. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Writing variable (varid=%d) to file failed. Invalid file id (ncid = %d) provided", varid, ncid);
    }
    ios = file->iosystem;

    /* Find the number of dimensions. */
    if ((ierr = PIOc_inq_varndims(ncid, varid, &ndims)))
    {
        return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Finding the number of dimensions of the variable failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
    }

    /* Set up count array. */
    PIO_Offset count[ndims];
    for (int c = 0; c < ndims; c++)
        count[c] = 1;

    return PIOc_put_vars_tc(ncid, varid, index, count, NULL, xtype, op);
}

/**
 * Internal PIO function which provides a type-neutral interface to
 * nc_put_var calls.
 *
 * Users should not call this function directly. Instead, call one of
 * the derived functions, depending on the type of data you are
 * writing: PIOc_put_var_text(), PIOc_put_var_uchar(),
 * PIOc_put_var_schar(), PIOc_put_var_ushort(),
 * PIOc_put_var_short(), PIOc_put_var_uint(), PIOc_put_var_int(),
 * PIOc_put_var_long(), PIOc_put_var_float(),
 * PIOc_put_var_longlong(), PIOc_put_var_double(),
 * PIOc_put_var_ulonglong().
 *
 * This routine is called collectively by all tasks in the
 * communicator ios.union_comm.
 *
 * @param ncid identifies the netCDF file
 * @param varid the variable ID number
 * @param xtype the netCDF type of the data being passed in buf. Data
 * will be automatically covnerted from this type to the type of the
 * variable being written to.
 * @param op pointer to the data to be written.
 *
 * @return PIO_NOERR on success, error code otherwise.
 * @author Ed Hartnett
 */
int PIOc_put_var_tc(int ncid, int varid, nc_type xtype, const void *op)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    PIO_Offset *startp = NULL; /* Pointer to start array. */
    PIO_Offset *countp = NULL; /* Pointer to count array. */
    PIO_Offset start[PIO_MAX_DIMS];
    PIO_Offset count[PIO_MAX_DIMS];
    int ndims;   /* The number of dimensions in the variable. */
    int ierr = PIO_NOERR;    /* Return code from function calls. */

    LOG((1, "PIOc_put_var_tc ncid = %d varid = %d xtype = %d", ncid,
         varid, xtype));

    /* Find the info about this file. We need this for error handling. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Writing variable (varid=%d) to file failed. Invalid file id (ncid = %d) provided", varid, ncid);
    }
    ios = file->iosystem;

    /* Find the number of dimensions. */
    if ((ierr = PIOc_inq_varndims(ncid, varid, &ndims)))
    {
        return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Finding the number of dimensions of the variable failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
    }

    /* Scalar vars (which have ndims == 0) should just pass NULLs for
     * start/count. */
    if (ndims)
    {
        int dimid[ndims];

        /* Set up start array. */
        for (int d = 0; d < ndims; d++)
            start[d] = 0;

        /* Get the dimids for this var. */
        ierr = PIOc_inq_vardimid(ncid, varid, dimid);
        if(ierr != PIO_NOERR){
            LOG((1, "PIOc_inq_vardimid failed, ierr = %d", ierr));
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                            "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Finding the dimension ids of dimensions in the file failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
        }

        /* Count array are the dimlens. */
        for (int d = 0; d < ndims; d++)
            if ((ierr = PIOc_inq_dimlen(ncid, dimid[d], &count[d])))
            {
                return pio_err(ios, file, ierr, __FILE__, __LINE__,
                                "Writing variable (%s, varid=%d) to file (%s, ncid=%d) failed. Finding the dimension length of dim %d in the file failed", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid, d);
            }

        /* Set the array pointers. */
        startp = start;
        countp = count;
    }

    return PIOc_put_vars_tc(ncid, varid, startp, countp, NULL, xtype, op);
}
