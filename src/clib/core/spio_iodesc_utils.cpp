/** @file
 * Utils related to I/O decomposition
 */
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_io_summary.h"
#include "pio_rearr_contig.hpp"

/**
 * Allocate a region struct, and initialize it.
 *
 * @param ios pointer to the IO system info, used for error
 * handling. Ignored if NULL.
 * @param ndims the number of dimensions for the data in this region.
 * @param a pointer that gets a pointer to the newly allocated
 * io_region struct.
 * @returns 0 for success, error code otherwise.
 */
int alloc_region2(iosystem_desc_t *ios, int ndims, io_region **regionp)
{
    io_region *region;

    /* Check inputs. */
    pioassert(ndims >= 0 && regionp, "invalid input", __FILE__, __LINE__);
    LOG((1, "alloc_region2 ndims = %d sizeof(io_region) = %d", ndims,
         sizeof(io_region)));
    
    /* Allocate memory for the io_region struct. */
    if (!(region = (io_region *) calloc(1, sizeof(io_region))))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while allocating region. Out of memory allocating %lld bytes I/O region", (unsigned long long) sizeof(io_region));
    }

    /* Allocate memory for the array of start indicies. */
    if (!(region->start = (PIO_Offset *) calloc(ndims, sizeof(PIO_Offset))))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while allocating region. Out of memory allocating %lld bytes  for start array in the I/O region", (unsigned long long) (ndims * sizeof(PIO_Offset)));
    }

    /* Allocate memory for the array of counts. */
    if (!(region->count = (PIO_Offset *) calloc(ndims, sizeof(PIO_Offset))))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while allocating region. Out of memory allocating %lld bytes  for count array in the I/O region", (unsigned long long) (ndims * sizeof(PIO_Offset)));
    }

    /* Return pointer to new region to caller. */
    *regionp = region;
    
    return PIO_NOERR;
}

/**
 * Allocate space for an IO description struct, and initialize it.
 *
 * @param ios pointer to the IO system info, used for error
 * handling.
 * @param piotype the PIO data type (ex. PIO_FLOAT, PIO_INT, etc.).
 * @param ndims the number of dimensions.
 * @param maplen the length of the local decomposition map
 * @param iodesc pointer that gets the newly allocated io_desc_t.
 * @returns 0 for success, error code otherwise.
 */
int malloc_iodesc(iosystem_desc_t *ios, int piotype, int ndims, int maplen,
                  io_desc_t **iodesc)
{
    MPI_Datatype mpi_type;
    PIO_Offset type_size;
    int mpierr = MPI_SUCCESS;
    int ret;

    /* Check input. */
    pioassert(ios && piotype > 0 && ndims >= 0 && iodesc,
              "invalid input", __FILE__, __LINE__);

    LOG((1, "malloc_iodesc piotype = %d ndims = %d", piotype, ndims));

    /* Get the MPI type corresponding with the PIO type. */
    if((ret = find_mpi_type(piotype, &mpi_type, NULL))){
      return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                      "Internal error while allocating memory for iodesc. Unable to find MPI type corresponding to PIO type (%d)", piotype);
    }

    /* What is the size of the pio type? */
    if((ret = spio_pnetcdf_inq_type(0, piotype, NULL, &type_size))){
      return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                      "Internal error while allocating memory for iodesc. Finding the size of PIO type (%d) failed", piotype);
    }

    /* Allocate space for the io_desc_t struct. */
    if(!(*iodesc = (io_desc_t *) calloc(1, sizeof(io_desc_t)))){
      return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                      "Internal error while allocating memory for iodesc. Out of memory allocating %lld bytes for the I/O descriptor", (unsigned long long) sizeof(io_desc_t));
    }

    /* Remember the pio type and its size. */
    (*iodesc)->piotype = piotype;
    (*iodesc)->piotype_size = type_size;

    /* Remember the MPI type. */
    (*iodesc)->mpitype = mpi_type;

    /* Get the size of the type. */
    if((mpierr = MPI_Type_size((*iodesc)->mpitype, &(*iodesc)->mpitype_size))){
      return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* Initialize some values in the struct. */
    (*iodesc)->maxregions = 1;
    (*iodesc)->ioid = -1;
    (*iodesc)->maplen = maplen;
    (*iodesc)->ndims = ndims;

    /* Allocate space for, and initialize, the first region. */
    if((ret = alloc_region2(ios, ndims, &((*iodesc)->firstregion)))){
      return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                      "Internal error while allocating memory for iodesc. Allocating memory for 1st region failed. Out of memory allocating memory for I/O region in the I/O descriptor");
    }

    /* Allocate memory for the local decomposition map */
    if(!((*iodesc)->map = (PIO_Offset *) malloc(sizeof(PIO_Offset) * maplen))){
      return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                      "Internal error while allocating memory (%lld bytes) to store I/O decomposition map", (unsigned long long) (sizeof(PIO_Offset) * maplen));
    }

    /* Allocate memory for storing the dimension lengths of variables that use this decomposition */
    if(!((*iodesc)->dimlen = (int *)malloc(sizeof(int) * ndims))){
      return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                      "Internal error while allocating memory (%lld bytes) for storing dimension sizes in the I/O decomposition map", (unsigned long long) (sizeof(int) * ndims));
    }

    /* Set the swap memory settings to defaults for this IO system. */
    (*iodesc)->rearr_opts = ios->rearr_opts;

#if PIO_SAVE_DECOMPS
    /* The descriptor is not yet saved to disk */
    (*iodesc)->is_saved = false;
#endif

    return PIO_NOERR;
}

/**
 * Free a region list.
 *
 * top a pointer to the start of the list to free.
 */
void free_region_list(io_region *top)
{
    io_region *ptr, *tptr;

    ptr = top;
    while (ptr)
    {
        if (ptr->start)
            free(ptr->start);
        if (ptr->count)
            free(ptr->count);
        tptr = ptr;
        ptr = ptr->next;
        free(tptr);
    }
}

/**
 * Free a decomposition map.
 *
 * @param iosysid the IO system ID.
 * @param ioid the ID of the decomposition map to free.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_freedecomp_impl(int iosysid, int ioid)
{
    iosystem_desc_t *ios;
    io_desc_t *iodesc;
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function calls. */
    int ret = 0;

    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Freeing PIO decomposition failed. Invalid iosystem id (%d) provided", iosysid);
    }
    assert(ios);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
    {
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Freeing PIO decomposition failed. Invalid io decomposition id (%d) provided", ioid);
    }

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_FREEDECOMP; /* Message for async notification. */

        PIO_SEND_ASYNC_MSG(ios, msg, &ret, iosysid, ioid);
        if(ret != PIO_NOERR)
        {
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                            "Freeing PIO decomposition failed (iosysid = %d, iodesc id=%d). Error sending asynchronous message, PIO_MSG_FREEDECOMP, on iosystem", iosysid, ioid);
        }
    }

    if(iodesc->nasync_pend_ops > 0){
      /* Let I/O desc be freed during finalize */
      spio_ltimer_stop(ios->io_fstats->tot_timer_name);
      return PIO_NOERR;
    }

    /* Free the map. */
    free(iodesc->map);

    /* Free the dimlens. */
    free(iodesc->dimlen);

    if (iodesc->rfrom)
        free(iodesc->rfrom);

    if (iodesc->rtype)
    {
        for (int i = 0; i < iodesc->nrecvs; i++)
            if (iodesc->rtype[i] != MPI_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(&iodesc->rtype[i])))
                {
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
                }

        free(iodesc->rtype);
    }

    if (iodesc->stype)
    {
        for (int i = 0; i < iodesc->num_stypes; i++)
            if (iodesc->stype[i] != MPI_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(iodesc->stype + i)))
                {
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
                }

        iodesc->num_stypes = 0;
        free(iodesc->stype);
    }

    if (iodesc->scount)
        free(iodesc->scount);

    if (iodesc->rcount)
        free(iodesc->rcount);

    if (iodesc->sindex)
        free(iodesc->sindex);

    if (iodesc->rindex)
        free(iodesc->rindex);

    if (iodesc->firstregion)
        free_region_list(iodesc->firstregion);

    if (iodesc->fillregion)
        free_region_list(iodesc->fillregion);

    if (iodesc->rearranger == PIO_REARR_SUBSET)
        if ((mpierr = MPI_Comm_free(&iodesc->subset_comm)))
        {
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
        }

    if(iodesc->rearr){
      iodesc->rearr->finalize();
      delete iodesc->rearr;
    }

    ret = pio_delete_iodesc_from_list(ioid);
    if (ret != PIO_NOERR)
    {
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Freeing PIO decomposition failed (iosysid = %d, ioid=%d). Error while trying to delete I/O descriptor from internal list", iosysid, ioid); 
    }
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);

    return ret;
}

