#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* ============= APIs for an I/O system (like MPI communicators) =================== */
/* Initializing I/O system for asynchronous I/O */
int PIOc_init_async(MPI_Comm world, int num_io_procs, const int *io_proc_list, int component_count,
                    const int *num_procs_per_comp, const int **proc_list, MPI_Comm *io_comm, MPI_Comm *comp_comm,
                    int rearranger, int *iosysidp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_init_async");
  tr.add_arg("world", world).add_arg("num_io_procs", num_io_procs).
    add_arg("*io_proc_list", io_proc_list).add_arg("component_count", component_count).
    add_arg("*num_procs_per_comp", num_procs_per_comp).add_arg("**proc_list", proc_list).
    add_arg("*io_comm", io_comm).add_arg("*comp_comm", comp_comm).
    add_arg("rearranger", rearranger).add_arg("*iosysidp", iosysidp);
#endif
  ret = PIOc_init_async_impl(world, num_io_procs, io_proc_list, component_count,
                              num_procs_per_comp, proc_list, io_comm, comp_comm,
                              rearranger, iosysidp);
#if SPIO_ENABLE_API_TRACING
  tr.set_iosys_id(iosysidp[0]).flush();
#endif
  return ret;
}


/* Initializing I/O system for asynchronous I/O */
int PIOc_init_intercomm(int component_count, const MPI_Comm peer_comm,
                        const MPI_Comm *ucomp_comms, const MPI_Comm uio_comm,
                        int rearranger, int *iosysidps)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_init_intercomm");
  bool is_io_proc = false;
  int my_comp_idx = 0;
  if(uio_comm != MPI_COMM_NULL){
    is_io_proc = true;
  }
  else{
    for(int i = 0; i < component_count; i++){
      if(ucomp_comms[i] != MPI_COMM_NULL){
        my_comp_idx = i;
        break;
      }
    }
  }
  tr.add_arg("component_count", component_count).
    add_arg("peer_comm", peer_comm).add_arg("ucomp_comms", ucomp_comms, component_count).
    add_arg("uio_comm", uio_comm).add_arg("rearranger", rearranger).
    add_arg("*iosysidps", iosysidps);
#endif
  ret = PIOc_init_intercomm_impl(component_count, peer_comm, ucomp_comms,
                                  uio_comm, rearranger, iosysidps);
#if SPIO_ENABLE_API_TRACING
  if(!is_io_proc){
    tr.set_iosys_id(iosysidps[my_comp_idx]).flush();
    tr.add_rval("*iosysidps", iosysidps, component_count);
  }
#endif
  return ret;
}

int PIOc_Init_Intercomm_from_F90(int component_count, int f90_peer_comm,
                                  const int *f90_comp_comms, int f90_io_comm,
                                  int rearranger, int *iosysidps)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_Init_Intercomm_from_F90");
  bool is_io_proc = false;
  int my_comp_idx = 0;
  MPI_Comm ccomp_comms[component_count];
  if(MPI_Comm_f2c(f90_io_comm) != MPI_COMM_NULL){
    is_io_proc = true;
  }
  else{
    for(int i = 0; i < component_count; i++){
      ccomp_comms[i] = MPI_Comm_f2c(f90_comp_comms[i]);
      if(ccomp_comms[i] != MPI_COMM_NULL){
        my_comp_idx = i;
      }
    }
  }
  tr.add_arg("component_count", component_count).
    add_arg("f90_peer_comm", MPI_Comm_f2c(f90_peer_comm)).
    add_arg("f90_comp_comms", ccomp_comms, component_count).
    add_arg("f90_io_comm", MPI_Comm_f2c(f90_io_comm)).add_arg("rearranger", rearranger).
    add_arg("*iosysidps", iosysidps);
#endif
  ret = PIOc_Init_Intercomm_from_F90_impl(component_count, f90_peer_comm,
                                            f90_comp_comms, f90_io_comm,
                                            rearranger, iosysidps);
#if SPIO_ENABLE_API_TRACING
  if(!is_io_proc){
    tr.set_iosys_id(iosysidps[my_comp_idx]).flush();
    tr.add_rval("*iosysidps", iosysidps, component_count);
  }
#endif
  return ret;
}

int PIOc_get_numiotasks(int iosysid, int *numiotasks)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_num_iotasks");
  tr.set_iosys_id(iosysid).add_arg("*numiotasks", numiotasks).flush();
#endif
  ret = PIOc_get_numiotasks_impl(iosysid, numiotasks);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*numiotasks", (numiotasks) ? (*numiotasks) : 0);
#endif
  return ret;
}

/* Initialize an I/O system */
int PIOc_Init_Intracomm(MPI_Comm comp_comm, int num_iotasks, int stride, int base, int rearr,
                        int *iosysidp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_Init_Intracomm");
  tr.add_arg("comp_comm", comp_comm).add_arg("num_iotasks", num_iotasks).
    add_arg("stride", stride).add_arg("base", base).add_arg("rearr", rearr).
    add_arg("*iosysidp", iosysidp);
#endif
  ret = PIOc_Init_Intracomm_impl(comp_comm, num_iotasks, stride, base, rearr, iosysidp);
#if SPIO_ENABLE_API_TRACING
  tr.set_iosys_id(*iosysidp).flush();

  tr.add_rval("*iosysidp", (iosysidp) ? (*iosysidp) : -1);
#endif
  return ret;
}

int PIOc_Init_Intracomm_from_F90(int f90_comp_comm,
                                 const int num_iotasks, const int stride,
                                 const int base, const int rearr,
                                 rearr_opt_t *rearr_opts, int *iosysidp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_Init_Intracomm_from_F90");
  tr.add_arg("f90_comp_comm", MPI_Comm_f2c(f90_comp_comm)).
    add_arg("num_iotasks", num_iotasks).add_arg("stride", stride).
    add_arg("base", base).add_arg("rearr", rearr).
    add_arg("*rearr_opts", rearr_opts).add_arg("*iosysidp", iosysidp);
#endif
  ret = PIOc_Init_Intracomm_from_F90_impl(f90_comp_comm, num_iotasks, stride,
                                            base, rearr, rearr_opts, iosysidp);
#if SPIO_ENABLE_API_TRACING
  tr.set_iosys_id(*iosysidp).flush();
  tr.add_rval("*iosysidp", (iosysidp) ? (*iosysidp) : -1);
#endif
  return ret;
}

/* Finalize an I/O system */
int PIOc_finalize(int iosysid)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_finalize");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).flush();
  tr.finalize();
#endif
  ret = PIOc_finalize_impl(iosysid);
  return ret;
}


/* Set error handling for entire io system. */
int PIOc_Set_IOSystem_Error_Handling(int iosysid, int method)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_Set_IOSystem_Error_Handling");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("method", method).flush();
#endif
  return PIOc_Set_IOSystem_Error_Handling_impl(iosysid, method);
}


/* Set error handling for entire io system. */
int PIOc_set_iosystem_error_handling(int iosysid, int method, int *old_method)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_set_iosystem_error_handling");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("method", method).
    add_arg("*old_method", old_method).flush();
#endif
  return PIOc_set_iosystem_error_handling_impl(iosysid, method, old_method);
}


int PIOc_iam_iotask(int iosysid, bool *ioproc)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_iam_iotask");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("*ioproc", ioproc).flush();
#endif
  ret = PIOc_iam_iotask_impl(iosysid, ioproc);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ioproc", (ioproc) ? (*ioproc) : false);
#endif
  return ret;
}

int PIOc_iotask_rank(int iosysid, int *iorank)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_iotask_rank");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("*iorank", iorank).flush();
#endif
  ret = PIOc_iotask_rank_impl(iosysid, iorank);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*iorank", (iorank) ? (*iorank) : -1);
#endif
  return ret;
}

int PIOc_iosystem_is_active(int iosysid, bool *active)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_iosystem_is_active");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("*active", active).flush();
#endif
  ret = PIOc_iosystem_is_active_impl(iosysid, active);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*active", (active) ? (*active) : false);
#endif
  return ret;
}

int PIOc_iotype_available(int iotype)
{
  /* FIXME: Figure out how to trace these non I/O system specific calls */
  return PIOc_iotype_available_impl(iotype);
}

int PIOc_set_rearr_opts(int iosysid, int comm_type, int fcd,
                        bool enable_hs_c2i, bool enable_isend_c2i,
                        int max_pend_req_c2i,
                        bool enable_hs_i2c, bool enable_isend_i2c,
                        int max_pend_req_i2c)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_set_rearr_opts");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("comm_type", comm_type).add_arg("fcd", fcd).
    add_arg("enable_hs_c2i", enable_hs_c2i).
    add_arg("enable_isend_c2i", enable_isend_c2i).
    add_arg("max_pend_req_c2i", max_pend_req_c2i).
    add_arg("enable_hs_i2c", enable_hs_i2c).
    add_arg("enable_isend_i2c", enable_isend_i2c).
    add_arg("max_pend_req_i2c", max_pend_req_i2c).flush();
#endif
  return PIOc_set_rearr_opts_impl(iosysid, comm_type, fcd,
                                  enable_hs_c2i, enable_isend_c2i,
                                  max_pend_req_c2i,
                                  enable_hs_i2c, enable_isend_i2c,
                                  max_pend_req_i2c);
}

int PIOc_set_hint(int iosysid, const char *hint, const char *hintval)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_set_hint");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*hint", hint).add_arg("*hintval", hintval).flush();
#endif
  return PIOc_set_hint_impl(iosysid, hint, hintval);
}

int PIOc_set_chunk_cache(int iosysid, int iotype, PIO_Offset size, PIO_Offset nelems,
                         float preemption)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_set_chunk_cache");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("iotype", iotype).
    add_arg("size", static_cast<long long int>(size)).
    add_arg("nelems", static_cast<long long int>(nelems)).
    add_arg("preemption", preemption).flush();
#endif
  return PIOc_set_chunk_cache_impl(iosysid, iotype, size, nelems, preemption);
}

int PIOc_get_chunk_cache(int iosysid, int iotype, PIO_Offset *sizep, PIO_Offset *nelemsp,
                         float *preemptionp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_chunk_cache");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("iotype", iotype).
    add_arg("*sizep", sizep).add_arg("*nelemsp", nelemsp).
    add_arg("*preemptionp", preemptionp).flush();
#endif
  ret = PIOc_get_chunk_cache_impl(iosysid, iotype, sizep, nelemsp, preemptionp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*sizep", (sizep) ? (static_cast<long long int>(*sizep)) : -1).
    add_rval("*nelemsp", (nelemsp) ? (static_cast<long long int>(*nelemsp)) : -1).
    add_rval("*preemptionp", (preemptionp) ? (*preemptionp) : 0.0);
#endif
  return ret;
}

int PIOc_set_blocksize(int newblocksize)
{
  /* FIXME: Figure out how to trace these non I/O system specific calls */
  return PIOc_set_blocksize_impl(newblocksize);
}

/* Set the IO node data buffer size limit. */
PIO_Offset PIOc_set_buffer_size_limit(PIO_Offset limit)
{
  /* FIXME: Figure out how to trace these non I/O system specific calls */
  return PIOc_set_buffer_size_limit_impl(limit);
}
