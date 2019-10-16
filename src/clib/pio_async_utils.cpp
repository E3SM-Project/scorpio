/** @file
 * Support functions for the PIO library.
 */

#include <iostream>
#include <cassert>

extern "C"{

#include <pio_config.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#if PIO_ENABLE_LOGGING
#include <unistd.h>
#endif /* PIO_ENABLE_LOGGING */
#include <pio.h>
#include <pio_internal.h>
#include "pio_timer.h"
#if PIO_USE_ASYNC_WR_THREAD
#include "pio_async_tpool_cint.h"
#endif
#include "pio_async_utils.h"

} // extern "C"

/* Use this function for op kinds with no wait functions
 * We use it to indicate,
 * 1) No wait function available
 * 2) Make sure that the wait function is not called (a more
 *    optimized wait function is available, so make sure that
 *    the wait function for this async op is not called)
 * @author Jayesh Krishna
 */
int pio_async_wait_func_unavail(void *pdata)
{
    assert(0);
}

/* Use this function for op kinds with no poke function
 * Some asynchronous operations have no poke/test functions
 * so any generic code that uses the poke function must
 * check the existence of this function before using it
 * @author Jayesh Krishna
 */
int pio_async_poke_func_unavail(void *pdata, int *flag)
{
    assert(0);
}

/* Wait for pending asynchronous operations on this file
 * This is the generic wait function for waiting on all
 * async ops on a file
 * @param file Pointer to the file_desc for the file
 * Returns PIO_NOERR on success, a pio error code otherwise
 * @author Jayesh Krishna
 */
int pio_file_async_pend_ops_wait(file_desc_t *file)
{
    int ret;
    assert(file != NULL);

    if(file->nasync_pend_ops == 0)
    {
        return PIO_NOERR;
    }

    pio_async_op_t *p = file->async_pend_ops, *q=NULL;
    while(p)
    {
        LOG((2, "Waiting on async op, kind = %s",
            (p->op_type == PIO_ASYNC_REARR_OP) ? "PIO_ASYNC_REARR_OP" :
            ((p->op_type == PIO_ASYNC_PNETCDF_WRITE_OP) ? "PIO_ASYNC_PNETCDF_WRITE_OP" :
            "UNKNOWN")));
        ret = p->wait(p->pdata);
        if(ret != PIO_NOERR)
        {
            return pio_err(NULL, NULL, PIO_EINTERNAL, __FILE__, __LINE__,
                            "Error waiting for pending asynchronous operation on file, %s (ncid = %d)", pio_get_fname_from_file(file), file->pio_ncid);
        }
        q = p;
        p = p->next;
        q->free(q->pdata);
        free(q);
    }
    file->async_pend_ops = NULL;
    file->nasync_pend_ops = 0;

    return PIO_NOERR;
}

#ifdef PIO_MICRO_TIMING
/* A struct to store information on pnetcdf write timers */
typedef struct pio_async_pnetcdf_wr_timer_info{
    int nvars;
    bool *var_timer_was_running;
    mtimer_t wait_timer;
} pio_async_pnetcdf_wr_timer_info_t;

/* Pause all var wr timers, create a temp wait timer and return it */
/* @author Jayesh Krishna */
int pio_async_pnetcdf_setup_wr_timers(file_desc_t *file, var_desc_t **vdescs,
    int nvdescs,
    pio_async_pnetcdf_wr_timer_info_t *wr_info)
{
    int ret;

    if(nvdescs == 0)
    {
        return PIO_NOERR;
    }
    assert(file && vdescs && wr_info);
    wr_info->nvars = nvdescs;
    wr_info->var_timer_was_running = (bool *)malloc(wr_info->nvars * sizeof(bool));
    if(!wr_info->var_timer_was_running)
    {
        return pio_err(file->iosystem, file, PIO_ENOMEM, __FILE__, __LINE__,
                        "Setting up asynchronous write timers for the PIO_IOTYPE_PNETCDF I/O type failed on file (%s, ncid=%d). Unable to allocate %lld bytes to store timer state for the multiple variables (number of variables=%d) in the file", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (wr_info->nvars * sizeof(bool)), wr_info->nvars);
    }

    /* Timer to keep track of wait time */
    wr_info->wait_timer = mtimer_create("Temp_wait_timer", file->iosystem->my_comm,
        "piowaitlog");
    if(!mtimer_is_valid(wr_info->wait_timer))
    {
        return pio_err(file->iosystem, file, PIO_EINTERNAL, __FILE__, __LINE__,
                        "Setting up asynchronous write timers for the PIO_IOTYPE_PNETCDF I/O type failed on file (%s, ncid=%d). Unable to create a temporary timer to measure wait time for asynchronous operations on the file", pio_get_fname_from_file(file), file->pio_ncid);
    }

    /* Pause all timers */
    for(int i=0; i<nvdescs; i++)
    {
        wr_info->var_timer_was_running[i] = false;
        assert(vdescs[i]);
        if(mtimer_is_valid(vdescs[i]->wr_mtimer))
        {
            ret = mtimer_pause(vdescs[i]->wr_mtimer, &(wr_info->var_timer_was_running[i]));
            if(ret != PIO_NOERR)
            {
                LOG((1, "Unable to pause timer"));
                return ret;
            }
        }
    }

    ret = mtimer_start(wr_info->wait_timer);
    if(ret != PIO_NOERR)
    {
        LOG((1, "Unable to start the temp wait timer"));
        return ret;
    } 

    return PIO_NOERR;
}

/*
 * 1) Update the variable wr timers with the wait time
 * 2) Restart timers (stopped during setup phase), if needed
 * 3) Release the wr_info
 * @author Jayesh Krishna
 */
int pio_async_pnetcdf_finalize_wr_timers(file_desc_t *file, var_desc_t **vdescs,
    int nvdescs,
    pio_async_pnetcdf_wr_timer_info_t *wr_info)
{
    int ret;

    if(nvdescs == 0)
    {
        return PIO_NOERR;
    }
    assert(file && vdescs && wr_info);
    assert(nvdescs == wr_info->nvars);

    /* Calculate the wait time */
    ret = mtimer_pause(wr_info->wait_timer, NULL);
    if(ret != PIO_NOERR)
    {
        LOG((1, "Unable to pause temp wait timer"));
        return ret;
    }

    double wait_time = 0.0;
    ret = mtimer_get_wtime(wr_info->wait_timer, &wait_time);
    if(ret != PIO_NOERR)
    {
        LOG((1, "Unable to get time from wait timer"));
        return ret;
    }

    ret = mtimer_destroy(&(wr_info->wait_timer));
    if(ret != PIO_NOERR)
    {
        LOG((1, "Destroying temp wait timer failed"));
        /* Continue */
    }

    /* Find avg wait time per variable */
    wait_time /= nvdescs;

    /* Update timers for vars with the avg wait time */
    for(int i=0; i<wr_info->nvars; i++)
    {
        ret = mtimer_update(vdescs[i]->wr_mtimer, wait_time);
        if(ret != PIO_NOERR)
        {
            LOG((1, "Unable to update var write timer"));
            return ret;
        }
        
        /* Wait is now complete - no more async events in progress */
        ret = mtimer_async_event_in_progress(vdescs[i]->wr_mtimer, false);
        if(ret != PIO_NOERR)
        {
            LOG((1, "Unable to disable async events for var"));
            return ret;
        }

        /* If the timer was already running before we paused it in setup phase,
         * restart it or else flush it */
        if(wr_info->var_timer_was_running[i])
        {
            ret = mtimer_resume(vdescs[i]->wr_mtimer);
            if(ret != PIO_NOERR)
            {
                LOG((1, "Unable to resume variable write timer"));
                return ret;
            }
        }
        else
        {
            ret = mtimer_flush(vdescs[i]->wr_mtimer,
                      get_var_desc_str(file->pio_ncid, vdescs[i]->varid, NULL));
            if(ret != PIO_NOERR)
            {
                LOG((1, "Unable to flush timer"));
                return ret;
            }
        }
    }

    free(wr_info->var_timer_was_running);
    wr_info->var_timer_was_running = NULL;

    return PIO_NOERR;
}
#endif

/* Reset a vdesc after writing all data associated with it */
/* @author Jayesh Krishna */
int pio_async_pnetcdf_reset_vdesc(var_desc_t **vdescs, int nvdescs)
{
    /* FIXME: These operations need to be asynchronous */
    for(int i=0; i<nvdescs; i++)
    {
        vdescs[i]->wb_pend = 0;
        if(vdescs[i]->fillbuf)
        {
            brel(vdescs[i]->fillbuf);
            vdescs[i]->fillbuf = NULL;
        }
    }
    return PIO_NOERR;
}

/* Optimized wait function for pnetcdf writes */
/* Instead of waiting for each async op of type, PNETCDF_WRITE_OP,
 * wait for the ops collectively
 * @author Jayesh Krishna
 */
int pio_async_pnetcdf_write_kwait(void *f)
{
    int ret;
    file_desc_t *file = (file_desc_t *)f;
    assert(file);
    assert(file->iotype == PIO_IOTYPE_PNETCDF);

    /* Gather up all requests corresponding to all vdescs associated
     * with the pending async ops
     * Also delete these async ops from the list since we wait for 
     * the requests associated with the ops here
     * */
    if(file->nasync_pend_ops > 0)
    {
        int nreqs = 0;
        int *reqs = (int *)malloc(file->nasync_pend_ops * sizeof(int));
        if(!reqs)
        {
            return pio_err(file->iosystem, file, PIO_ENOMEM, __FILE__, __LINE__,
                            "Error while waiting for asynchronous writes to complete for the PIO_IOTYPE_PNETCDF I/O type on file (%s, ncid=%d). Unable to allocate %lld bytes to consolidate requests associated with pending asynchronous operations on the file", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (file->nasync_pend_ops * sizeof(int)));
        }
        int nvdescs = 0;
        var_desc_t **vdescs = (var_desc_t **)malloc(file->nasync_pend_ops * sizeof(var_desc_t *));
        if(!vdescs)
        {
            return pio_err(file->iosystem, file, PIO_ENOMEM, __FILE__, __LINE__,
                            "Error while waiting for asynchronous writes to complete for the PIO_IOTYPE_PNETCDF I/O type on file (%s, ncid=%d). Unable to allocate %lld bytes to keep track of variable descriptors associated with pending asynchronous operations on the file", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (file->nasync_pend_ops * sizeof(var_desc_t *)));
        }

        viobuf_cache_t **viobufs = (viobuf_cache_t **) malloc(file->nasync_pend_ops * sizeof(viobuf_cache_t *));
        if(!viobufs)
        {
            return pio_err(file->iosystem, file, PIO_ENOMEM, __FILE__, __LINE__,
                            "Error while waiting for asynchronous writes to complete for the PIO_IOTYPE_PNETCDF I/O type on file (%s, ncid=%d). Unable to allocate %lld bytes to keep track of buffered data associated with pending asynchronous operations on the file", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (file->nasync_pend_ops * sizeof(viobuf_cache_t *)));
        }
        
        pio_async_op_t *p = file->async_pend_ops, *q=NULL;
        pio_async_op_t *prev = file->async_pend_ops;
        while(p)
        {
            if(p->op_type == PIO_ASYNC_PNETCDF_WRITE_OP)
            {
                viobuf_cache_t *pviobuf = (viobuf_cache_t *)(p->pdata);
                assert(pviobuf);

                reqs[nreqs] = pviobuf->req;
                viobufs[nreqs] = pviobuf;
                nreqs++;

                vdescs[nvdescs] = pviobuf->vdesc;
                nvdescs++;

                q = p->next;
                /* Free node */
                free(p);
                /* p == file->async_pend_ops => First node */
                if(p == file->async_pend_ops)
                {
                    /* Update head and prev */
                    file->async_pend_ops = q;
                    prev = q;
                }
                else
                {
                    /* Skip node p, already deleted */
                    prev->next = q;
                }
                p = q;
            }
            else
            {
                /* Ignore op kinds/types that are not pnetcdf writes */
                prev = p;
                p = p->next;
            }
        }

        /* We don't expect any other pending operations when writes are
         * pending on this file
         * This was a constraint that was introduced by resuing a
         * single file buffer, file->iobuf, for rearrange and write.
         * So a write needed to complete before a rearrange occurs.
         * FIXME: Since this is no longer a constraint for async writes
         * investigate on how to relax the constraint
         */ 
        assert(nreqs <= file->nasync_pend_ops);
        LOG((2, "pio_async_pnetcdf_write_kwait(): nreqs= %d, file->nasync_pend_ops= %d\n",
                  nreqs, file->nasync_pend_ops));

#ifdef PIO_MICRO_TIMING
        pio_async_pnetcdf_wr_timer_info_t wr_info;
        ret = pio_async_pnetcdf_setup_wr_timers(file, vdescs, nvdescs, &wr_info);
        if(ret != PIO_NOERR)
        {
            LOG((1, "Initializing var write timers failed"));
            return ret;
        }
#endif

        /* Wait on all requests in one call */
        /* We don't care about the status of each request, we
         * only care whether wait succeeded or not 
         * Requires pnetcdf ver >= 1.7.0 to support a
         * NULL value for the status array
         */
        ret = ncmpi_wait_all(file->fh, nreqs, reqs, NULL);
        if(ret != NC_NOERR)
        {
            return pio_err(file->iosystem, file, ret, __FILE__, __LINE__,
                            "Error while waiting for asynchronous writes to complete for the PIO_IOTYPE_PNETCDF I/O type on file (%s, ncid=%d). Internal I/O library error while waiting for pending PnetCDF operations.", pio_get_fname_from_file(file), file->pio_ncid);
        }

        for(int i=0; i<nreqs; i++)
        {
            pio_viobuf_free(viobufs[i]);
        }

#ifdef PIO_MICRO_TIMING
        ret = pio_async_pnetcdf_finalize_wr_timers(file, vdescs, nvdescs, &wr_info);
        if(ret != PIO_NOERR)
        {
            LOG((1, "Finalizing var write timers failed"));
            return ret;
        }
#endif
        ret = pio_async_pnetcdf_reset_vdesc(vdescs, nvdescs);
        if(ret != PIO_NOERR)
        {
            return pio_err(file->iosystem, file, PIO_EINTERNAL, __FILE__, __LINE__,
                            "Error while waiting for asynchronous writes to complete for the PIO_IOTYPE_PNETCDF I/O type on file (%s, ncid=%d). Resetting variable descriptors associated with the completed asynchronous operations failed", pio_get_fname_from_file(file), file->pio_ncid);
        }

        free(viobufs);
        free(vdescs);
        free(reqs);

        file->nasync_pend_ops -= nreqs;
    }

    return PIO_NOERR;
}

/* Wait only for rearr async ops on a file */
/* @author Jayesh Krishna */
int pio_async_rearr_kwait(void *f)
{
    int ret;
    file_desc_t *file = (file_desc_t *)f;
    assert(file);

    if(file->nasync_pend_ops > 0)
    {
        int nreqs = 0;
        pio_async_op_t *p = file->async_pend_ops, *q=NULL;
        pio_async_op_t *prev = file->async_pend_ops;
        while(p)
        {
            if(p->op_type == PIO_ASYNC_REARR_OP)
            {
                nreqs++;
                ret = p->wait(p->pdata);
                if(ret != PIO_NOERR)
                {
                    LOG((1, "Waiting for rearr async op failed"));
                    return pio_err(file->iosystem, file, PIO_EINTERNAL,
                                    __FILE__, __LINE__,
                                    "Internal error while waiting for asynchronous rearrangement operations (number of pending ops = %d) on file (%s, ncid=%d)", file->nasync_pend_ops, pio_get_fname_from_file(file), file->pio_ncid);
                }
                p->free(p->pdata);
                q = p->next;
                /* Free node */
                free(p);
                /* p == file->async_pend_ops => First node */
                if(p == file->async_pend_ops)
                {
                    /* Update head and prev to delete/skip node p */
                    file->async_pend_ops = q;
                    prev = q;
                }
                else
                {
                    /* Delete node p, already freed */
                    prev->next = q;
                }
                p = q;
            }
            else
            {
                /* Ignore op kinds/types that are not pnetcdf writes */
                prev = p;
                p = p->next;
            }
        }
        file->nasync_pend_ops -= nreqs;
    }

    return PIO_NOERR;
}

/* Optimized wait functions for different async op kinds/types on a file */
typedef int (*file_async_pend_ops_kwait_func_t) (void *file);
static file_async_pend_ops_kwait_func_t
    file_async_pend_ops_kwait_funcs[PIO_ASYNC_NUM_OP_TYPES] = {
      /* PIO_ASYNC_INVALID_OP */
      pio_async_wait_func_unavail,
      /* PIO_ASYNC_REARR_OP */
      pio_async_rearr_kwait,
      /* PIO_ASYNC_PNETCDF_WRITE_OP */
      pio_async_pnetcdf_write_kwait
    };

/* Wait for pending asynchronous operations of kind, op_kind, on this file
 * @param file Pointer to the file_desc for the file
 * Returns PIO_NOERR on success, a pio error code otherwise
 * @author Jayesh Krishna
 */
int pio_file_async_pend_ops_kwait(file_desc_t *file, pio_async_op_type_t op_kind)
{
    assert(file);
    assert((op_kind > PIO_ASYNC_INVALID_OP)
            && (op_kind < PIO_ASYNC_NUM_OP_TYPES));

    int ret = file_async_pend_ops_kwait_funcs[op_kind](file);
    if(ret != PIO_NOERR)
    {
        return pio_err(NULL, NULL, PIO_EINTERNAL, __FILE__, __LINE__,
                        "Internal error while waiting for pending asynchronous operations on file (%s, ncid=%d)", pio_get_fname_from_file(file), file->pio_ncid);
    }

    return PIO_NOERR;
}

/* Free a variable iobuf cache */
/* @author Jayesh Krishna */
void pio_viobuf_free(void *p)
{
    viobuf_cache_t *pviobuf = (viobuf_cache_t *)p;
    assert(pviobuf);

    /* iobuf can be NULL associated with writes that are just
     * fillvalues (these writes don't have individual iobufs,
     * they just use a single common buffer associated with
     * the vdesc, vdesc->fillbuf
     * These dummy viobufs created to represent writes that
     * contain only fillvalues (SUBSET) don't have any
     * iobuf or fillvalue associated with it
     * The dummy viobufs are also used to represent
     * ncmpi_bput_* requests and since data is buffered by
     * pnetcdf we don't have an iobuf associated with these
     * requests
     */
    if(pviobuf->iobuf)
    {
        brel(pviobuf->iobuf);
        pviobuf->iobuf = NULL;
    }

    /* pviobuf->fillvalue is only valid if
     * 1) Var has a fillvalue defined in file, vdesc->fillvalue is valid
     * or
     * 2) iodesc->needsfill is true
     */ 
    if(pviobuf->fillvalue)
    {
        free(pviobuf->fillvalue);
    }

    free(p);
}

/* Add an async op to the list of pending ops for a file
 * @param file Pointer to the file_desc for the file
 * @param op_type Type of asynchronous operation added
 * @param pdata Pointer to user defined data for this async op
 * Returns PIO_NOERR on success, a pio error code otherwise
 * @author Jayesh Krishna
 */
int pio_file_async_pend_op_add(file_desc_t *file,
      pio_async_op_type_t op_type, void *pdata)
{
    assert(file != NULL);
    assert((op_type > PIO_ASYNC_INVALID_OP)
            && (op_type < PIO_ASYNC_NUM_OP_TYPES));
    assert(pdata != NULL);

    pio_async_op_t *pnew = (pio_async_op_t *) calloc(1, sizeof(pio_async_op_t));
    if(pnew == NULL)
    {
        return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                      "Adding an asynchronous operation to file (%s, ncid=%d) failed. Unable to allocate %lld bytes to cache the asynchronous operation", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (sizeof(pio_async_op_t)));
    }

    pnew->op_type = op_type;
    pnew->pdata = pdata;
    if(op_type == PIO_ASYNC_REARR_OP)
    {
        pnew->wait = pio_swapm_wait;
        pnew->poke = pio_swapm_iwait;
        pnew->free = pio_swapm_req_free;
    }
    else if(op_type == PIO_ASYNC_PNETCDF_WRITE_OP)
    {
        /* Don't wait on individual ops, do coll wait on all ops */
        pnew->wait = pio_async_wait_func_unavail;
        /* PnetCDF does not have a non-blocking test/poke function */
        pnew->poke = pio_async_poke_func_unavail;
        pnew->free = pio_viobuf_free;
    }
    pnew->next = file->async_pend_ops;

    file->async_pend_ops = pnew;
    file->nasync_pend_ops++;

    return PIO_NOERR;
}

/* Start rearranging data pointed to by buf and cache the rearranged
 * data. The rearranged data is cached in the ioprocs and the data
 * pointed to by buf is expected to be valid until the rearrange
 * operation completes.
 * Note: Called by all procs
 * @author Jayesh Krishna
 */
int pio_var_rearr_and_cache(file_desc_t *file, var_desc_t *vdesc,
      io_desc_t *iodesc, void *buf,
      size_t buflen, void *fillvalue, int rec_num)
{
    int ierr;
    iosystem_desc_t *ios;

    LOG((2, "pio_var_rearr_and_cache : file=%p, vdesc=%p, iodesc=%p, rec_num=%d\n",
            file, vdesc, iodesc, rec_num));
    /* Note: buf can be NULL or buflen can be 0 if a compute process
     * has no data to send to io procs
     */
    assert(file && vdesc && iodesc && ((rec_num >= -1)));
    ios = file->iosystem;
    assert(ios);

    void *sbuf = buf;
    void *rbuf = NULL;
    viobuf_cache_t *pnew = NULL;
    /* iodesc->maxiobuflen can be zero if all procs have
     * no data to receive from other procs in a subset
     * of procs (SUBSET rearranger)
     */
    if((ios->ioproc) && (iodesc->maxiobuflen > 0))
    {
        /* viobuf cache list keeps track of iobuf that contains the
         * rearranged data
         */
        viobuf_cache_t *p = vdesc->viobuf_ltail;
        pnew = (viobuf_cache_t *)calloc(1, sizeof(viobuf_cache_t));
        if(!pnew)
        {
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                            "Error while queueing asynchronous operation to start rearranging of data on file (%s, ncid=%d). Unable to allocate %lld bytes for internal data structure to keep track of cached user data", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (sizeof(viobuf_cache_t)));
        }

        pnew->ubuf = buf;
        pnew->ubuf_sz = buflen;
        /* Allocate mem for rearranged data, since different procs could
         * potentially have different amount of rearranged data (SUBSET)
         * allocate memory for max memory required among all procs,
         * i.e., maxiobuflen
         */
        pnew->iobuf_sz = iodesc->mpitype_size * iodesc->maxiobuflen;
        pnew->iobuf = bget(pnew->iobuf_sz);
        if(!(pnew->iobuf))
        {
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                            "Error while queueing asynchronous operation to start rearranging of data on file (%s, ncid=%d). Unable to allocate %lld bytes for caching rearranged data", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (pnew->iobuf_sz));
        }
        rbuf = pnew->iobuf;
        pnew->fillvalue = NULL;
        pnew->fillvalue_sz = 0;
        if(iodesc->needsfill)
        {
            pnew->fillvalue = malloc(iodesc->mpitype_size);
            if(!(pnew->fillvalue))
            {
                return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                "Error while queueing asynchronous operation to start rearranging of data on file (%s, ncid=%d). Unable to allocate %lld bytes for caching fillvalue associated with user data", pio_get_fname_from_file(file), file->pio_ncid, (unsigned long long) (iodesc->mpitype_size));
            }
            pnew->fillvalue_sz = iodesc->mpitype_size;
            assert(fillvalue || vdesc->fillvalue);
            if(fillvalue)
            {
                /* Use the given fillvalue (explicit fillvalue specified
                 * by the user
                 */
                memcpy(pnew->fillvalue, fillvalue, iodesc->mpitype_size);
            }
            else
            {
                /* Use default fillvalue for the variable */
                memcpy(pnew->fillvalue, vdesc->fillvalue, iodesc->mpitype_size);
            }
        }
        
        /* For the BOX rearranger we need contiguous blocks of data on
         * each io process, including the fill values. Only data that
         * are not fillvalues are transferred from compute to io procs,
         * so init the buffer, to store rearranged data, with fill
         * values
         */
        if(iodesc->needsfill && (iodesc->rearranger == PIO_REARR_BOX))
        {
            assert(pnew->fillvalue);
            for(int i=0; i<iodesc->maxiobuflen; i++)
            {
                memcpy(&((char *)pnew->iobuf)[iodesc->mpitype_size * i],
                    pnew->fillvalue, iodesc->mpitype_size);
            }
        }

        pnew->rec_num = rec_num;
        /* The requests associated with data rearrangemnent is handled
         * by separate async operations. These reqs are used to keep
         * track of writes (after data rearrangement) using iobuf
         */
        pnew->req = PIO_REQ_NULL;
        pnew->vdesc = vdesc;
    }

    /* Start rearrange of the data - asynchronous op, so will return before
     * rearrange is complete
     */
    int nvars = 1;
    ierr = rearrange_comp2io(ios, iodesc, file, sbuf, rbuf, nvars);
    if(ierr != PIO_NOERR)
    {
        return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Error while rearranging data from compute processes to I/O processes before writing it out to file (%s, ncid=%d). Internal error occured while rearranging data", pio_get_fname_from_file(file), file->pio_ncid);
    }

    if((ios->ioproc) && (iodesc->maxiobuflen > 0))
    {
        /* Add the new node to the end of the list */
        if(vdesc->viobuf_ltail)
        {
            vdesc->viobuf_ltail->next = pnew;
            vdesc->viobuf_ltail = pnew;
        }
        else
        {
            /* First node */
            vdesc->viobuf_ltail = pnew;
        }
        /* First node in the list, update the head */
        if(!(vdesc->viobuf_lhead))
        {
            vdesc->viobuf_lhead = vdesc->viobuf_ltail;
        }
    }

    return PIO_NOERR;
}

/* Get the cached data associated with a variable
 * @author Jayesh Krishna
 */
int pio_var_get_cache_data(var_desc_t *vdesc, int rec, void **buf, size_t *buflen)
{
    /* Use pio_var_del_cache_data that returns the iobuf and deletes the cache
     * instead
     */ 
    assert(0);
}

/* Remove the cached viobuf corresponding to rec_num for a variable defined by vdesc
 * The removed viobuf_cache is returned via pviobuf
 * Note : Only called from io procs (to get the cached rearranged data)
 * @author Jayesh Krishna
 */
int pio_var_rem_cache_data(var_desc_t *vdesc, int rec_num, viobuf_cache_t **pviobuf)
{
    viobuf_cache_t *p = vdesc->viobuf_lhead;
    viobuf_cache_t *prev = p;

    assert(vdesc && (rec_num >= -1) && pviobuf);

    /* We expect the list to have at least one element */
    assert(vdesc->viobuf_lhead && vdesc->viobuf_ltail);
    *pviobuf = NULL;

    /* If the frames/records are accessed/deleted in the same order as they
     * were added this search will always end at the head
     */
    while(p && (p->rec_num != rec_num))
    {
        prev = p;
        p = p->next;
    }

    /* We assume that the search always succeeds */
    assert(p);

    *pviobuf = p;

    /* Delete node from list */
    if(vdesc->viobuf_lhead == vdesc->viobuf_ltail)
    {
        /* Single node in the list */
        vdesc->viobuf_lhead = NULL;
        vdesc->viobuf_ltail = NULL;
    }
    else
    {
        if(p == vdesc->viobuf_lhead)
        {
            vdesc->viobuf_lhead = p->next;
        }

        if(p == vdesc->viobuf_ltail)
        {
            vdesc->viobuf_ltail = prev;
        }

        prev->next = p->next;
    }

    return PIO_NOERR;
}

/* A helper function to copy rearranged data corresponding to variables
 * specified by varids array to a single buffer (and the var frames
 * referred by the frames array).
 * The varids array can have duplicates, where the corresponding
 * element in the frames array have different values
 * The dest buffer is expected to be a contiguous region of valid
 * values, rearranged as specified by iodesc
 * This helper function is used by serial writes to copy data corresponding
 * to multiple variables to a single buffer, this makes transferring data for
 * multiple variables more efficient (compared to separately transferring
 * data for each variable)
 * Note: The function expects that data rearrangement is already complete
 * and the rearranged data is available in viodesc caches in var_desc
 * @author Jayesh Krishna
 */
int pio_file_compact_and_copy_rearr_data(void *dest, size_t dest_sz,
      io_desc_t *iodesc, file_desc_t *file, const int *varids,
      const int *frames, int nvars)
{
    int ret;
    size_t off = 0;
    size_t rem_sz = dest_sz;

    for(int i=0; i<nvars; i++)
    {
        viobuf_cache_t *pviobuf = NULL;
        int cur_frame = (frames) ? frames[i] : -1;
        /* Get the iobuf (with rearranged data, for this var+frame */
        ret = pio_var_rem_cache_data(file->varlist + varids[i],
                cur_frame, &pviobuf);
        if(ret != PIO_NOERR)
        {
            return pio_err(file->iosystem, file, ret, __FILE__, __LINE__,
                            "Error while compacting and copying rearranged data for asynchronous writes for %d variables on file (%s, ncid=%d). Getting internal buffer with rearranged data for var %d failed", nvars, pio_get_fname_from_file(file), file->pio_ncid, i);
        }
        assert(pviobuf);

        /* Copy this iobuf to dest buffer */
        /* Note that for each variable we only copy iodesc->llen values, that
         * contain the valid values. Each pviobuf->iobuf is of size 
         * pviobuf->iobuf_sz (== iodesc->maxiobuflen) 
         * >= iodesc->llen * iodesc->mpitype_size.
         */ 
        size_t iobuf_sz = iodesc->mpitype_size * iodesc->llen;
        assert(rem_sz >= iobuf_sz);
        memcpy((void *) ((char *)dest + off), pviobuf->iobuf, iobuf_sz);
        off += iobuf_sz;
        rem_sz -= iobuf_sz;

        pio_viobuf_free(pviobuf);
    }

    return PIO_NOERR;
}

/* Wait for all pending asynchronous operations on this iosystem
 * This is the generic wait function for waiting on all
 * async ops on an iosystem
 * @param iosys Pointer to the iosystem_desc for the iosystem
 * Returns PIO_NOERR on success, a pio error code otherwise
 * @author Jayesh Krishna
 */
int pio_iosys_async_pend_ops_wait(iosystem_desc_t *iosys)
{
    int ret;
    assert(iosys != NULL);

    if(iosys->nasync_pend_ops == 0)
    {
        return PIO_NOERR;
    }

    pio_async_op_t *p = iosys->async_pend_ops, *q=NULL;
    while(p)
    {
        LOG((2, "Waiting on async op, kind = %s",
            (p->op_type == PIO_ASYNC_REARR_OP) ? "PIO_ASYNC_REARR_OP" :
            ((p->op_type == PIO_ASYNC_PNETCDF_WRITE_OP) ? "PIO_ASYNC_PNETCDF_WRITE_OP" :
            ((p->op_type == PIO_ASYNC_FILE_WRITE_OPS) ? "PIO_ASYNC_FILE_WRITE_OPS" :
            "UNKNOWN"))));
        assert(p->op_type == PIO_ASYNC_FILE_WRITE_OPS);
        ret = p->wait(p->pdata);
        if(ret != PIO_NOERR)
        {
            return pio_err(NULL, NULL, PIO_EINTERNAL, __FILE__, __LINE__,
                            "Internal error waiting for pending asynchronous operations on iosystem (iosysid=%d). Waiting for an asynchronous operation failed.", iosys->iosysid);
        }
        q = p;
        p = p->next;
        q->free(q->pdata);
        free(q);
    }
    iosys->async_pend_ops = NULL;
    iosys->nasync_pend_ops = 0;

    return PIO_NOERR;
}

/* Wait for all pending asynchronous operations on a file
 * This is the generic wait function for waiting on all
 * async ops a file
 * @param pdata Pointer to user data (pointer to file_desc
 * corresponding to a file)
 * Returns PIO_NOERR on success, a pio error code otherwise
 * @author Jayesh Krishna
 */
int pio_file_async_pend_op_wait(void *pdata)
{
    int ret;
    assert(pdata != NULL);

    file_desc_t *file = (file_desc_t *)pdata;
    if(file->nasync_pend_ops == 0)
    {
        return PIO_NOERR;
    }

    /* We only wait for pending pnetcdf writes. So the caller
     * needs to make sure that no data rearrangement ops are
     * pending */
    ret = pio_file_async_pend_ops_kwait(file, PIO_ASYNC_PNETCDF_WRITE_OP);
    if(ret != PIO_NOERR)
    {
        return pio_err(file->iosystem, file, ret, __FILE__, __LINE__,
                        "Internal error while waiting for pending asynchronous write operations on file (%s, ncid=%d) for the PIO_IOTYPE_PNETCDF iotype", pio_get_fname_from_file(file), file->pio_ncid);
    }

    file->wb_pend = 0;
    file->npend_ops = 0;

    return PIO_NOERR;
}

/* Free file_desc and close the file
 * @param pdata Pointer to user data (pointer to file_desc
 * corresponding to a file)
 * @author Jayesh Krishna
 */
void pio_file_close_and_free(void *pdata)
{
    int ret;
    assert(pdata != NULL);

    file_desc_t *file = (file_desc_t *)pdata;
    bool sync_with_ioprocs = false;
    ret = PIO_hard_closefile(file->iosystem, file, sync_with_ioprocs);
    if(ret != PIO_NOERR)
    {
        LOG((1, "Closing file (id=%d) failed (ignoring the error)", file->pio_ncid));
    }
}


/* Add an async op to the list of pending ops for an iosystem
 * @param iosys Pointer to the iosystem_desc
 * @param op_type Type of asynchronous operation added
 * @param pdata Pointer to user defined data for this async op
 * Returns PIO_NOERR on success, a pio error code otherwise
 * @author Jayesh Krishna
 */
int pio_iosys_async_pend_op_add(iosystem_desc_t *iosys,
      pio_async_op_type_t op_type, void *pdata)
{
    assert(iosys != NULL);
    assert((op_type > PIO_ASYNC_INVALID_OP)
            && (op_type < PIO_ASYNC_NUM_OP_TYPES));
    assert(pdata != NULL);
    assert(op_type == PIO_ASYNC_FILE_WRITE_OPS);

    pio_async_op_t *pnew = (pio_async_op_t *) calloc(1, sizeof(pio_async_op_t));
    if(pnew == NULL)
    {
        return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while adding a pending asynchronous operations on the iosystem (iosysid=%d). Unable to allocate %lld bytes to keep track of the asynchronous operation", iosys->iosysid, (unsigned long long) (sizeof(pio_async_op_t)));
    }

    pnew->op_type = op_type;
    pnew->pdata = pdata;
    if(op_type == PIO_ASYNC_FILE_WRITE_OPS)
    {
        pnew->wait = pio_file_async_pend_op_wait;
        /* File writes do not have a non-blocking test/poke function */
        pnew->poke = pio_async_poke_func_unavail;
        pnew->free = pio_file_close_and_free;
    }
    pnew->next = iosys->async_pend_ops;

    iosys->async_pend_ops = pnew;
    iosys->nasync_pend_ops++;

    return PIO_NOERR;
}

#if PIO_USE_ASYNC_WR_THREAD
/* Add an async op to the list of pending ops in the thread pool
 * @param iosys Pointer to the iosystem_desc
 * @param op_type Type of asynchronous operation added
 * @param pdata Pointer to user defined data for this async op
 * Returns PIO_NOERR on success, a pio error code otherwise
 * @author Jayesh Krishna
 */
int pio_tpool_async_pend_op_add(iosystem_desc_t *iosys,
      pio_async_op_type_t op_type, void *pdata)
{
    int ret;
    assert(iosys != NULL);
    assert((op_type > PIO_ASYNC_INVALID_OP)
            && (op_type < PIO_ASYNC_NUM_OP_TYPES));
    assert(pdata != NULL);
    assert(op_type == PIO_ASYNC_FILE_WRITE_OPS);

    pio_async_op_t *pnew = (pio_async_op_t *) calloc(1, sizeof(pio_async_op_t));
    if(pnew == NULL)
    {
        return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while adding asynchronous pending operation to the thread pool (iosystem = %d). Unable to allocate %lld bytes to keep track of the asynchronous operation", iosys->iosysid, (unsigned long long) sizeof(pio_async_op_t));
    }

    pnew->op_type = op_type;
    pnew->pdata = pdata;
    if(op_type == PIO_ASYNC_FILE_WRITE_OPS)
    {
        pnew->wait = pio_file_async_pend_op_wait;
        /* File writes do not have a non-blocking test/poke function */
        pnew->poke = pio_async_poke_func_unavail;
        pnew->free = pio_file_close_and_free;
    }

    ret = pio_async_tpool_op_add(pnew);
    if(ret != PIO_NOERR)
    {
        LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
        return pio_err(iosys, NULL, ret, __FILE__, __LINE__,
                        "Internal error while adding asynchronous pending operation to the thread pool (iosystem = %d). Adding the asynchronous operation failed", iosys->iosysid);
    }

    return PIO_NOERR;
}
#endif // PIO_USE_ASYNC_WR_THREAD
