#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* ============= APIs for an I/O system (like MPI communicators) =================== */
/* Initializing I/O system for asynchronous I/O */
int PIOc_init_async(MPI_Comm world, int num_io_procs, const int *io_proc_list, int component_count,
                    const int *num_procs_per_comp, const int **proc_list, MPI_Comm *io_comm, MPI_Comm *comp_comm,
                    int rearranger, int *iosysidp)
{
  return PIOc_init_async_impl(world, num_io_procs, io_proc_list, component_count,
                              num_procs_per_comp, proc_list, io_comm, comp_comm,
                              rearranger, iosysidp);
}


/* Initializing I/O system for asynchronous I/O */
int PIOc_init_intercomm(int component_count, const MPI_Comm peer_comm,
                        const MPI_Comm *ucomp_comms, const MPI_Comm uio_comm,
                        int rearranger, int *iosysidps)
{
  return PIOc_init_intercomm_impl(component_count, peer_comm, ucomp_comms,
                                  uio_comm, rearranger, iosysidps);
}

int PIOc_Init_Intercomm_from_F90(int component_count, int f90_peer_comm,
                                  const int *f90_comp_comms, int f90_io_comm,
                                  int rearranger, int *iosysidps)
{
  return PIOc_Init_Intercomm_from_F90_impl(component_count, f90_peer_comm,
                                            f90_comp_comms, f90_io_comm,
                                            rearranger, iosysidps);
}

int PIOc_get_numiotasks(int iosysid, int *numiotasks)
{
  return PIOc_get_numiotasks_impl(iosysid, numiotasks);
}

/* Initialize an I/O system */
int PIOc_Init_Intracomm(MPI_Comm comp_comm, int num_iotasks, int stride, int base, int rearr,
                        int *iosysidp)
{
  return PIOc_Init_Intracomm_impl(comp_comm, num_iotasks, stride, base, rearr, iosysidp);
}

int PIOc_Init_Intracomm_from_F90(int f90_comp_comm,
                                 const int num_iotasks, const int stride,
                                 const int base, const int rearr,
                                 rearr_opt_t *rearr_opts, int *iosysidp)
{
  return PIOc_Init_Intracomm_from_F90_impl(f90_comp_comm, num_iotasks, stride,
                                            base, rearr, rearr_opts, iosysidp);
}

/* Finalize an I/O system */
int PIOc_finalize(int iosysid)
{
  return PIOc_finalize_impl(iosysid);
}


/* Set error handling for entire io system. */
int PIOc_Set_IOSystem_Error_Handling(int iosysid, int method)
{
  return PIOc_Set_IOSystem_Error_Handling_impl(iosysid, method);
}


/* Set error handling for entire io system. */
int PIOc_set_iosystem_error_handling(int iosysid, int method, int *old_method)
{
  return PIOc_set_iosystem_error_handling_impl(iosysid, method, old_method);
}


int PIOc_iam_iotask(int iosysid, bool *ioproc)
{
  return PIOc_iam_iotask_impl(iosysid, ioproc);
}

int PIOc_iotask_rank(int iosysid, int *iorank)
{
  return PIOc_iotask_rank_impl(iosysid, iorank);
}

int PIOc_iosystem_is_active(int iosysid, bool *active)
{
  return PIOc_iosystem_is_active_impl(iosysid, active);
}

int PIOc_iotype_available(int iotype)
{
  return PIOc_iotype_available_impl(iotype);
}

int PIOc_set_rearr_opts(int iosysid, int comm_type, int fcd,
                        bool enable_hs_c2i, bool enable_isend_c2i,
                        int max_pend_req_c2i,
                        bool enable_hs_i2c, bool enable_isend_i2c,
                        int max_pend_req_i2c)
{
  return PIOc_set_rearr_opts_impl(iosysid, comm_type, fcd,
                                  enable_hs_c2i, enable_isend_c2i,
                                  max_pend_req_c2i,
                                  enable_hs_i2c, enable_isend_i2c,
                                  max_pend_req_i2c);
}

int PIOc_set_hint(int iosysid, const char *hint, const char *hintval)
{
  return PIOc_set_hint_impl(iosysid, hint, hintval);
}

int PIOc_set_chunk_cache(int iosysid, int iotype, PIO_Offset size, PIO_Offset nelems,
                         float preemption)
{
  return PIOc_set_chunk_cache_impl(iosysid, iotype, size, nelems, preemption);
}

int PIOc_get_chunk_cache(int iosysid, int iotype, PIO_Offset *sizep, PIO_Offset *nelemsp,
                         float *preemptionp)
{
  return PIOc_get_chunk_cache_impl(iosysid, iotype, sizep, nelemsp, preemptionp);
}

int PIOc_set_blocksize(int newblocksize)
{
  return PIOc_set_blocksize_impl(newblocksize);
}

/* Set the IO node data buffer size limit. */
PIO_Offset PIOc_set_buffer_size_limit(PIO_Offset limit)
{
  return PIOc_set_buffer_size_limit_impl(limit);
}
