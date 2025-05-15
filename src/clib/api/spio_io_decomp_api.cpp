#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"
#include "spio_gptl_utils.hpp"

/* ========== APIs to handle I/O Decomposition =============== */

/* Init decomposition with 1-based compmap array. */
int PIOc_InitDecomp(int iosysid, int pio_type, int ndims, const int *gdimlen, int maplen,
                    const PIO_Offset *compmap, int *ioidp, const int *rearr,
                    const PIO_Offset *iostart, const PIO_Offset *iocount)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_InitDecomp");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_InitDecomp");
  assert((ndims > 0) && gdimlen);
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("pio_type", pio_type).add_arg("ndims", ndims).add_arg("*gdimlen", gdimlen, ndims).
    add_arg("maplen", maplen).add_arg("*compmap", compmap).add_arg("*ioidp", ioidp).
    add_arg("*rearr", rearr).add_arg("*iostart", iostart).add_arg("*iocount", iocount).flush();
#endif
  ret = PIOc_InitDecomp_impl(iosysid, pio_type, ndims, gdimlen, maplen,
                              compmap, ioidp, rearr, iostart, iocount);

#if SPIO_ENABLE_API_TRACING
  tr.set_decomp_info((ioidp) ? (*ioidp) : -1, compmap, maplen);
  tr.add_rval("*ioidp", (ioidp) ? (*ioidp) : -1);
#endif
  return ret;
}

int PIOc_InitDecomp_bc(int iosysid, int basetype, int ndims, const int *gdimlen,
                       const long int *start, const long int *count, int *ioidp)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_InitDecomp_bc");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_InitDecomp_bc");
  assert((ndims > 0) && gdimlen);
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("basetype", basetype).
    add_arg("ndims", ndims).add_arg("*gdimlen", gdimlen, ndims).add_arg("*start", start).
    add_arg("*count", count).add_arg("*ioidp", ioidp).flush();
#endif
  ret = PIOc_InitDecomp_bc_impl(iosysid, basetype, ndims, gdimlen,
                                  start, count, ioidp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ioidp", (ioidp) ? (*ioidp) : -1);
#endif
  return ret;
}


/* Init decomposition with 0-based compmap array. */
int PIOc_init_decomp(int iosysid, int pio_type, int ndims, const int *gdimlen, int maplen,
                     const PIO_Offset *compmap, int *ioidp, int rearranger,
                     const PIO_Offset *iostart, const PIO_Offset *iocount)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_init_decomp");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_init_decomp");
  assert((ndims > 0) && gdimlen);
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("pio_type", pio_type).add_arg("ndims", ndims).add_arg("*gdimlen", gdimlen, ndims).
    add_arg("maplen", maplen).add_arg("*compmap", compmap).add_arg("*ioidp", ioidp).
    add_arg("rearranger", rearranger).add_arg("*iostart", iostart).
    add_arg("*iocount", iocount).flush();
#endif
  ret = PIOc_init_decomp_impl(iosysid, pio_type, ndims, gdimlen, maplen,
                                compmap, ioidp, rearranger, iostart, iocount);

#if SPIO_ENABLE_API_TRACING
  tr.set_decomp_info((ioidp) ? (*ioidp) : -1, compmap, maplen);
  tr.add_rval("*ioidp", (ioidp) ? (*ioidp) : -1);
#endif
  return ret;
}


/* Free resources associated with a decomposition. */
int PIOc_freedecomp(int iosysid, int ioid)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_freedecomp");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_freedecomp");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).add_arg("ioid", ioid).flush();
#endif
  return PIOc_freedecomp_impl(iosysid, ioid);
}


/* Write/Read I/O decompositions */
int PIOc_readmap(const char *file, int *ndims, int **gdims, PIO_Offset *fmaplen,
                 PIO_Offset **map, MPI_Comm comm)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_readmap");
  /* FIXME: Add support for tracing comm-specific calls */
  return PIOc_readmap_impl(file, ndims, gdims, fmaplen, map, comm);
}

int PIOc_readmap_from_f90(const char *file,int *ndims, int **gdims, PIO_Offset *maplen,
                          PIO_Offset **map, int f90_comm)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_readmap_from_f90");
  /* FIXME: Add support for tracing comm-specific calls */
  return PIOc_readmap_from_f90_impl(file, ndims, gdims, maplen, map, f90_comm);
}

int PIOc_writemap(const char *file, int ioid, int ndims, const int *gdims, PIO_Offset maplen,
                  const PIO_Offset *map, MPI_Comm comm)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_writemap");
  /* FIXME: Add support for tracing comm-specific calls */
  return PIOc_writemap_impl(file, ioid, ndims, gdims, maplen, map, comm);
}

int PIOc_writemap_from_f90(const char *file, int ioid, int ndims, const int *gdims,
                           PIO_Offset maplen, const PIO_Offset *map, int f90_comm)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_writemap_from_f90");
  /* FIXME: Add support for tracing comm-specific calls */
  return PIOc_writemap_from_f90_impl(file, ioid, ndims, gdims, maplen, map, f90_comm);
}


/* Write a decomposition file. */
int PIOc_write_decomp(const char *file, int iosysid, int ioid, MPI_Comm comm)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_write_decomp");
  /* FIXME: Add support for tracing comm-specific calls */
  return PIOc_write_decomp_impl(file, iosysid, ioid, comm);
}


/* Write a decomposition file using netCDF. */
int PIOc_write_nc_decomp(int iosysid, const char *filename, int cmode, int ioid,
                         const char *title, const char *history, int fortran_order)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_write_nc_decomp");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_write_nc_decomp");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*filename", filename).add_arg("cmode", cmode).add_arg("ioid", ioid).
    add_arg("*title", title).add_arg("*history", history).
    add_arg("fortran_order", fortran_order).flush();
#endif
  return PIOc_write_nc_decomp_impl(iosysid, filename, cmode, ioid,
                                    title, history, fortran_order);
}


/* Read a netCDF decomposition file. */
int PIOc_read_nc_decomp(int iosysid, const char *filename, int *ioid, MPI_Comm comm,
                        int pio_type, char *title, char *history, int *fortran_order)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_read_nc_decomp");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_read_nc_decomp");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*filename", filename).add_arg("*ioid", ioid).
    add_mpi_arg("comm", comm).add_arg("pio_type", pio_type).
    add_arg("*title", title).add_arg("*history", history).
    add_arg("fortran_order", fortran_order).flush();
#endif
  return PIOc_read_nc_decomp_impl(iosysid, filename, ioid, comm,
                                  pio_type, title, history, fortran_order);
}
