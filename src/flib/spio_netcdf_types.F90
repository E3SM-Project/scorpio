!> @file
!! @brief Utilities functions
!! This file contains constants/types specific to the NetCDF library
!!

!> @internal
!! @def __PIO_FILE__
!! @brief This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_netcdf_types.F90'

MODULE spio_netcdf_types
!>
!! @public
!! @defgroup PIO_chunking_settings PIO_chunking_settings
!! @brief The NetCDF4/HDF5 chunking settings available
!! @details
!!  - PIO_CONTIGUOUS : (Default) Store data contiguously (no chunking)
!!  - PIO_CHUNKED : Store data in chunked format
!!  - PIO_COMPACT : Store data in the file header for compactness (variables < 64MB)
!<

#ifdef HAVE_NC4_CHUNK_CONSTS
#include <netcdf.inc>
  integer, public, parameter :: PIO_CONTIGUOUS = NF_CONTIGUOUS
  integer, public, parameter :: PIO_CHUNKED = NF_CHUNKED
  integer, public, parameter :: PIO_COMPACT = NF_COMPACT
#else
  integer, public, parameter :: PIO_CONTIGUOUS = 0
  integer, public, parameter :: PIO_CHUNKED = 0
  integer, public, parameter :: PIO_COMPACT = 0
#endif
END MODULE spio_netcdf_types
