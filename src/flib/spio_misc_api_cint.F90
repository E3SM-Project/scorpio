!>
!! @file
!! @brief C Interfaces for miscellaneous SCORPIO APIs

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_misc_api_cint'

MODULE spio_misc_api_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Fortran interface to C function to set the upper limit
!! for the internal buffer used to cache data written by the user
!!
!! @param[in] limit The new upper limit (in bytes) of the internal
!! buffer used to cache data written out by the user
!! @returns Returns the previous buffer limit
  INTEGER(PIO_OFFSET_F2C_TYPE_KIND) FUNCTION PIOc_set_buffer_size_limit(limit)&
                          bind(C,name="PIOc_set_buffer_size_limit")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE :: limit
  END FUNCTION PIOc_set_buffer_size_limit
END INTERFACE

INTERFACE
!> @private
!! @brief Fortran interface to C function to check if an iotype
!! is available. @copydoc PIO_iotype
!!
!! @param[in] iotype The iotype to check
!! @returns Returns 1 if the iotype is available, 0 otherwise
  INTEGER(C_INT) FUNCTION PIOc_iotype_available(iotype)&
                          bind(C,name="PIOc_iotype_available")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: iotype
  END FUNCTION PIOc_iotype_available
END INTERFACE

INTERFACE
!> @private
!! @brief Set the chunk cache settings for files of an I/O type
!! in an I/O system. This option is currently only supported for
!! NetCDF4 files
!!
!! @param[in] iosysid The id of the I/O system
!! @param[in] iotype The I/O type. @copydoc PIO_iotype
!! @param[in] chunk_cache_size The size of the chunk cache
!! @param[in] chunk_cache_nelems The number of elements in the chunk cache
!! @param[in] chunk_cache_preemption A value between 0 (LRU) to 1 (chunks
!!                                    evicted immediately after a read/write)
!!                                    to control the cache preemption/eviction
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_set_chunk_cache(iosysid, iotype, chunk_cache_size,&
                            chunk_cache_nelems, chunk_cache_preemption)&
                          bind(C,name="PIOc_set_chunk_cache")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: iotype
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE :: chunk_cache_size
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE :: chunk_cache_nelems
    REAL(C_FLOAT), VALUE :: chunk_cache_preemption
  END FUNCTION PIOc_set_chunk_cache
END INTERFACE

INTERFACE
!> @private
!! @brief Get the chunk cache gettings for files of an I/O type
!! in an I/O system. This option is currently only supported for
!! NetCDF4 files
!!
!! @param[in] iosysid The id of the I/O system
!! @param[in] iotype The I/O type. @copydoc PIO_iotype
!! @param[out] chunk_cache_size The size of the chunk cache
!! @param[out] chunk_cache_nelems The number of elements in the chunk cache
!! @param[out] chunk_cache_preemption A value between 0 (LRU) to 1 (chunks
!!                                    evicted immediately after a read/write)
!!                                    to control the cache preemption/eviction
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_get_chunk_cache(iosysid, iotype, chunk_cache_size,&
                            chunk_cache_nelems, chunk_cache_preemption)&
                          bind(C,name="PIOc_get_chunk_cache")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: iotype
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: chunk_cache_size
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: chunk_cache_nelems
    REAL(C_FLOAT) :: chunk_cache_preemption
  END FUNCTION PIOc_get_chunk_cache
END INTERFACE

INTERFACE
!> @private
!! @brief Copy an attribute from one file to another
!!
!! @details
!! @param[in] from_fh The file id of the input file (to copy attribute from).
!! @param[in] from_varid The id of the variable to copy the attribute from
!! @param[in] attname The name of the attribute to copy
!! @param[in] to_fh The file id of the output file (to copy attribute to).
!! @param[in] to_varid The id of the variable to copy the attribute to
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_copy_att(from_fh, from_varid, attname,&
                            to_fh, to_varid)&
                          bind(C,name="PIOc_copy_att")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: from_fh
    INTEGER(C_INT), VALUE :: from_varid
    CHARACTER(C_CHAR) :: attname(*)
    INTEGER(C_INT), VALUE :: to_fh
    INTEGER(C_INT), VALUE :: to_varid
  END FUNCTION PIOc_copy_att
END INTERFACE

END MODULE spio_misc_api_cint
