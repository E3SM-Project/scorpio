!> @file
!! @brief Miscellaneous SCORPIO APIs
!! This file contains the miscellaneous SCORPIO APIs
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_misc_api.F90'

MODULE spio_misc_api
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t
  USE pio_kinds, ONLY : PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_misc_api_cint
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_set_buffer_size_limit, pio_iotype_available,&
            pio_set_chunk_cache, pio_get_chunk_cache,&
            pio_copy_att

CONTAINS

!> @defgroup pio_set_buffer_size_limit pio_set_buffer_size_limit
!! @public
!! @brief Sets the limit for the internal buffer used to cache data written
!! by the user
!! @details
!! @param[in] limit The new buffer limit (in bytes)
!! @param[out] prev_limit Optional parameter that returns the
!! previous/old buffer limit (in bytes)
!!
  SUBROUTINE pio_set_buffer_size_limit(limit, prev_limit)
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: limit
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT), OPTIONAL :: prev_limit

    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: lim

    lim = PIOc_set_buffer_size_limit(INT(limit, PIO_OFFSET_F2C_TYPE_KIND))
    IF(PRESENT(prev_limit)) THEN
      prev_limit = INT(lim, PIO_OFFSET_KIND)
    END IF
  END SUBROUTINE pio_set_buffer_size_limit

!> @defgroup PIO_iotype_available PIO_iotype_available
!! @public
!! @brief Check if an iotype is available. @copydoc PIO_iotype
!!
!! @details
!! @param[in] The iotype to check for availability
!! @returns .TRUE. if iotype is available, .FALSE. otherwise
!!
  LOGICAL FUNCTION pio_iotype_available(iotype) RESULT(is_avail)
    INTEGER, INTENT(IN) :: iotype

    INTEGER(C_INT) :: cret

    cret = PIOc_iotype_available(INT(iotype, C_INT))
    IF(cret /= 0) THEN
      is_avail = .TRUE.
    ELSE
      is_avail = .FALSE.
    END IF

  END FUNCTION pio_iotype_available

!> @defgroup PIO_set_chunk_cache PIO_set_chunk_cache
!! @public
!! @brief Set the chunk cache settings for all files of a specific
!! I/O type in the I/O system.
!!
!! @details
!! Control the different settings for the chunk cache (the cache used internally
!! by the low level I/O library when reading/writing data) for files.
!! The NetCDF library sets the chunk cache to 64MB by default. Note that
!! this setting is discarded when the file is closed.
!!
!! This option is currently only supported for NetCDF4/HDF5 files.
!!
!! @param[in] iosysid The id of the I/O system
!! @param[in] iotype The I/O type to set this option. This option
!!                    is currently supported only for the NetCDF4/HDF5
!!                    files @copydoc PIO_iotype
!! @param[in] chunk_cache_size The size of the chunk cache.
!! @param[in] chunk_cache_nelems The number of elements in the chunk cache
!! @param[in] chunk_cache_preemption The cache preemption strategy for chunk caches.
!!                                    This option controls how often read/write data
!!                                    chunks are evicted from the chunk cache. The
!!                                    value is in the range [0,1], 0 implying that
!!                                    read/written chunks are treated like other
!!                                    chunks (LRU eviction policy) and a 1 implying
!!                                    that read/write chunks are evicted immediately
!!                                    after the data is read/written. A value between
!!                                    0 & 1 allows the user to control the eviction
!!                                    policy between these two schemes.
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION PIO_set_chunk_cache(iosysid, iotype, chunk_cache_size,&
                                        chunk_cache_nelems, chunk_cache_preemption) RESULT(ierr)
    INTEGER, INTENT(IN) :: iosysid
    INTEGER, INTENT(IN) :: iotype
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: chunk_cache_size
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: chunk_cache_nelems
    REAL, INTENT(IN)  :: chunk_cache_preemption

    ierr = PIOc_set_chunk_cache(INT(iosysid, C_INT), INT(iotype, C_INT),&
                                INT(chunk_cache_size, PIO_OFFSET_F2C_TYPE_KIND),&
                                INT(chunk_cache_nelems, PIO_OFFSET_F2C_TYPE_KIND),&
                                REAL(chunk_cache_preemption, C_FLOAT))
  END FUNCTION PIO_set_chunk_cache

!> @defgroup PIO_get_chunk_cache PIO_get_chunk_cache
!! @public
!! @brief Get the chunk cache gettings for all files of a specific
!! I/O type in the I/O system.
!!
!! @details
!! Get the different gettings for the chunk cache (the cache used internally
!! by the low level I/O library when reading/writing data) for files.
!!
!! This option is currently only supported for NetCDF4/HDF5 files.
!!
!! @param[in] iosysid The id of the I/O system
!! @param[in] iotype The I/O type to get this option. This option
!!                    is currently supported only for the NetCDF4/HDF5
!!                    files @copydoc PIO_iotype
!! @param[out] chunk_cache_size The size of the chunk cache.
!! @param[out] chunk_cache_nelems The number of elements in the chunk cache
!! @param[out] chunk_cache_preemption The cache preemption strategy for chunk caches.
!!                                    This option controls how often read/write data
!!                                    chunks are evicted from the chunk cache. The
!!                                    value is in the range [0,1], 0 implying that
!!                                    read/written chunks are treated like other
!!                                    chunks (LRU eviction policy) and a 1 implying
!!                                    that read/write chunks are evicted immediately
!!                                    after the data is read/written.
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION PIO_get_chunk_cache(iosysid, iotype, chunk_cache_size,&
                                        chunk_cache_nelems, chunk_cache_preemption) RESULT(ierr)
    INTEGER, INTENT(IN) :: iosysid
    INTEGER, INTENT(IN) :: iotype
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: chunk_cache_size
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: chunk_cache_nelems
    REAL, INTENT(OUT)  :: chunk_cache_preemption

    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cchunk_cache_size
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cchunk_cache_nelems
    REAL(C_FLOAT) :: cchunk_cache_preemption

    ierr = PIOc_get_chunk_cache(INT(iosysid, C_INT), INT(iotype, C_INT),&
                                cchunk_cache_size, cchunk_cache_nelems, cchunk_cache_preemption)

    chunk_cache_size = INT(cchunk_cache_size, PIO_OFFSET_F2C_TYPE_KIND)
    chunk_cache_nelems = INT(cchunk_cache_nelems, PIO_OFFSET_F2C_TYPE_KIND)
    chunk_cache_preemption = REAL(cchunk_cache_preemption)

  END FUNCTION PIO_get_chunk_cache

!> @defgroup PIO_copy_att PIO_copy_att
!! @public
!! @brief Copy an attribute from one file to another
!!
!! @details
!! @param[in] from_file The file handle of the input file (to copy attribute from).
!!                      @copydoc file_desc_t
!! @param[in] from_varid The id of the variable to copy the attribute from
!! @param[in] attname The name of the attribute to copy
!! @param[in] to_file The file handle of the output file (to copy attribute to).
!!                      @copydoc file_desc_t
!! @param[in] to_varid The id of the variable to copy the attribute to
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_copy_att(from_file, from_varid, attname,&
                                to_file, to_varid) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: from_file
    INTEGER, INTENT(IN) :: from_varid
    CHARACTER(LEN=*), INTENT(IN) :: attname
    TYPE(file_desc_t), INTENT(IN) :: to_file
    INTEGER, INTENT(IN) :: to_varid

    ierr = PIOc_copy_att(from_file%fh, INT(from_varid, C_INT) - 1,&
                          TRIM(attname) // C_NULL_CHAR,&
                          to_file%fh, INT(to_varid, C_INT) - 1)

  END FUNCTION pio_copy_att

END MODULE spio_misc_api
