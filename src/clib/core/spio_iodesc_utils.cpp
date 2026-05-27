/** @file
 * Utils related to I/O decomposition
 */
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_io_summary.h"
#include "pio_rearr_contig.hpp"

#include <stdexcept>

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

io_desc_t::io_desc_t(iosystem_desc_t *ios, int piotype,
  int ndims, const int *gdimlen,
  int maplen, const PIO_Offset *compmap,
  int rearranger, bool map_zero_based): ioid(SPIO_INVALID_ID),
    maplen(0), map(NULL), nrecvs(0), ndof(0), ndims(0), dimlen(NULL),
    num_aiotasks(0), rearranger(rearranger), maxregions(0), needsfill(false),
    maxbytes(0), piotype(piotype), piotype_size(0), mpitype(MPI_DATATYPE_NULL),
    mpitype_size(0), llen(0), maxiobuflen(0), rfrom(NULL), rcount(NULL),
    scount(NULL), sindex(NULL), rindex(NULL), rtype(NULL), stype(NULL),
    num_stypes(0), holegridsize(0), maxholegridsize(0), maxfillregions(0),
    firstregion(NULL), fillregion(NULL),
    rearr_opts({PIO_REARR_COMM_P2P, PIO_REARR_COMM_FC_2D_ENABLE,
      {true, true, PIO_REARR_COMM_UNLIMITED_PEND_REQ},
      {true, true, PIO_REARR_COMM_UNLIMITED_PEND_REQ}}),
    subset_comm(MPI_COMM_NULL), is_saved(false), rearr(NULL),
    nasync_pend_ops(0)
{
  int ret;

  assert(ios && (piotype > 0) && (ndims >= 0));

  ret = find_mpi_type(piotype, &mpitype, NULL);
  if(ret == PIO_NOERR) { ret = MPI_Type_size(mpitype, &mpitype_size); }
  if(ret != PIO_NOERR){
    throw std::runtime_error(std::string("Internal error while allocating memory for iodesc. Unable to find MPI type (or type size) corresponding to PIO type (") + std::to_string(piotype) + ")");
  }

  /* FIXME: Add version of func to inq type size as ints */
  PIO_Offset tmp_sz = 0;
  ret = spio_pnetcdf_inq_type(0, piotype, NULL, &tmp_sz);
  if(ret != PIO_NOERR){
    throw std::runtime_error(std::string("Internal error while allocating memory for iodesc. Unable to find the size of PIO type (") + std::to_string(piotype) + ")");
  }
  piotype_size = static_cast<int>(tmp_sz);

  /* FIXME: Move to C++ lists of classes instead of these custom lists */
  ret = alloc_region2(ios, ndims, &firstregion);
  if(ret != PIO_NOERR){
    throw std::runtime_error("Internal error while allocating memory for iodesc. Allocating memory for 1st region failed. Out of memory allocating memory for I/O region in the I/O descriptor");
  }
  maxregions = 1;

  /* Cache the local decomposition map (and transform it as needed) */
  init_map(maplen, compmap, rearranger, map_zero_based);

  /* FIXME: Move to a vector, instead of malloc()ed array */
  /* Cache global dimension lengths */
  dimlen = static_cast<int *>(malloc(sizeof(int) * ndims));
  if(dimlen){
    this->ndims = ndims;
    std::copy(gdimlen, gdimlen + ndims, dimlen);
  }
  else{
    throw std::runtime_error(std::string("Internal error while allocating memory (") + std::to_string(sizeof(int) * ndims) + "bytes) for storing dimension sizes in the I/O decomposition map");
  }

  /* Set the swap memory settings to defaults for this IO system. */
  rearr_opts = ios->rearr_opts;
}

void io_desc_t::init_map(int maplen, const PIO_Offset *compmap, int rearranger, bool map_zero_based)
{
  if(maplen == 0) { return; }

  /* FIXME: Move to a vector, instead of malloc()ed array */
  map = static_cast<PIO_Offset *> (malloc(sizeof(PIO_Offset) * maplen));
  if(!map){
    throw std::runtime_error(std::string("Internal error while allocating memory (") + std::to_string(sizeof(PIO_Offset) * maplen) + ") bytes to store I/O decomposition map");
  }
  this->maplen = maplen;

  /* Cache the local decomposition map */
  if(map_zero_based){
    /* BOX and SUBSET rearrangers expect map to the 1-based */
    if((rearranger == PIO_REARR_BOX) || (rearranger == PIO_REARR_SUBSET)){
      std::transform(compmap, compmap + maplen, map,
        [](PIO_Offset off) { return off + 1; });
    }
    else{
      std::copy(compmap, compmap + maplen, map);
    }
  }
  else{
    /* The decomposition map is 1-based */
    if(rearranger == PIO_REARR_CONTIG){
      /* CONTIG rearranger expects map to be 0-based */
      std::transform(compmap, compmap + maplen, map,
        [](PIO_Offset off) { return off - 1; });
    }
    else{
      std::copy(compmap, compmap + maplen, map);
    }
  }
}

io_desc_t::~io_desc_t()
{
  if(map) { free(map); }
  if(dimlen) { free(dimlen); }
  if(rfrom) { free(rfrom); }

  free_all_mpi_types();

  if(scount) { free(scount); }
  if(rcount) { free(rcount); }
  if(sindex) { free(sindex); }
  if(rindex) { free(rindex); }

  free_all_regions();

  if(rearranger == PIO_REARR_SUBSET) { MPI_Comm_free(&subset_comm); }

  if(rearr){
    rearr->finalize();
    delete rearr;
  }
}

/* FIXME: Move to C++ lists instead of custom lists */
/* Free an I/O region list */
void io_desc_t::free_region_list(io_region *phead)
{
  io_region *p = phead, *prev = NULL;

  while(p){
    if(p->start) { free(p->start); }
    if(p->count) { free(p->count); }
    prev = p;
    p = p->next;
    free(prev);
  }
}

/* Free all I/O regions */
void io_desc_t::free_all_regions(void )
{
  free_region_list(firstregion); firstregion = NULL;
  free_region_list(fillregion);  fillregion = NULL;
}

void io_desc_t::free_mpi_types(MPI_Datatype *ptypes, int ntypes)
{
  assert(ntypes >= 0);

  if(!ptypes) { return; }

  for(int i = 0; i < ntypes; i++){
    if(ptypes[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&ptypes[i]);
    }
  }
}

void io_desc_t::free_all_mpi_types(void )
{
  if(rtype){
    free_mpi_types(rtype, nrecvs);
    free(rtype); rtype = NULL;
  }

  if(stype){
    free_mpi_types(stype, num_stypes);
    free(stype); stype = NULL;
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

  if(!(ios = pio_get_iosystem_from_id(iosysid))){
    return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                    "Freeing PIO decomposition failed. Invalid iosystem id (%d) provided", iosysid);
  }

  assert(ios);
  spio_ltimer_start(ios->io_fstats->tot_timer_name);

  if(!(iodesc = pio_get_iodesc_from_id(ioid))){
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__,
                    "Freeing PIO decomposition failed. Invalid io decomposition id (%d) provided", ioid);
  }

  /* If async is in use, and this is not an IO task, bcast the parameters. */
  if(ios->async){
    int msg = PIO_MSG_FREEDECOMP; /* Message for async notification. */

    PIO_SEND_ASYNC_MSG(ios, msg, &ret, iosysid, ioid);
    if(ret != PIO_NOERR){
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

  ret = pio_delete_iodesc_from_list(ioid);
  if(ret != PIO_NOERR){
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Freeing PIO decomposition failed (iosysid = %d, ioid=%d). Error while trying to delete I/O descriptor from internal list", iosysid, ioid); 
  }

  spio_ltimer_stop(ios->io_fstats->tot_timer_name);
  return ret;
}
