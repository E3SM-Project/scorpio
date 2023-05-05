#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* ========== APIs to handle I/O Decomposition =============== */

/* Init decomposition with 1-based compmap array. */
int PIOc_InitDecomp(int iosysid, int pio_type, int ndims, const int *gdimlen, int maplen,
                    const PIO_Offset *compmap, int *ioidp, const int *rearr,
                    const PIO_Offset *iostart, const PIO_Offset *iocount)
{
  return PIOc_InitDecomp_impl(iosysid, pio_type, ndims, gdimlen, maplen,
                              compmap, ioidp, rearr, iostart, iocount);
}

int PIOc_InitDecomp_bc(int iosysid, int basetype, int ndims, const int *gdimlen,
                       const long int *start, const long int *count, int *ioidp)
{
  return PIOc_InitDecomp_bc_impl(iosysid, basetype, ndims, gdimlen,
                                  start, count, ioidp);
}


/* Init decomposition with 0-based compmap array. */
int PIOc_init_decomp(int iosysid, int pio_type, int ndims, const int *gdimlen, int maplen,
                     const PIO_Offset *compmap, int *ioidp, int rearranger,
                     const PIO_Offset *iostart, const PIO_Offset *iocount)
{
  return PIOc_init_decomp_impl(iosysid, pio_type, ndims, gdimlen, maplen,
                                compmap, ioidp, rearranger, iostart, iocount);
}


/* Free resources associated with a decomposition. */
int PIOc_freedecomp(int iosysid, int ioid)
{
  return PIOc_freedecomp_impl(iosysid, ioid);
}


/* Write/Read I/O decompositions */
int PIOc_readmap(const char *file, int *ndims, int **gdims, PIO_Offset *fmaplen,
                 PIO_Offset **map, MPI_Comm comm)
{
  return PIOc_readmap_impl(file, ndims, gdims, fmaplen, map, comm);
}

int PIOc_readmap_from_f90(const char *file,int *ndims, int **gdims, PIO_Offset *maplen,
                          PIO_Offset **map, int f90_comm)
{
  return PIOc_readmap_from_f90_impl(file, ndims, gdims, maplen, map, f90_comm);
}

int PIOc_writemap(const char *file, int ioid, int ndims, const int *gdims, PIO_Offset maplen,
                  const PIO_Offset *map, MPI_Comm comm)
{
  return PIOc_writemap_impl(file, ioid, ndims, gdims, maplen, map, comm);
}

int PIOc_writemap_from_f90(const char *file, int ioid, int ndims, const int *gdims,
                           PIO_Offset maplen, const PIO_Offset *map, int f90_comm)
{
  return PIOc_writemap_from_f90_impl(file, ioid, ndims, gdims, maplen, map, f90_comm);
}


/* Write a decomposition file. */
int PIOc_write_decomp(const char *file, int iosysid, int ioid, MPI_Comm comm)
{
  return PIOc_write_decomp_impl(file, iosysid, ioid, comm);
}


/* Write a decomposition file using netCDF. */
int PIOc_write_nc_decomp(int iosysid, const char *filename, int cmode, int ioid,
                         const char *title, const char *history, int fortran_order)
{
  return PIOc_write_nc_decomp_impl(iosysid, filename, cmode, ioid,
                                    title, history, fortran_order);
}


/* Read a netCDF decomposition file. */
int PIOc_read_nc_decomp(int iosysid, const char *filename, int *ioid, MPI_Comm comm,
                        int pio_type, char *title, char *history, int *fortran_order)
{
  return PIOc_read_nc_decomp_impl(iosysid, filename, ioid, comm,
                                  pio_type, title, history, fortran_order);
}
