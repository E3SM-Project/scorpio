!>
!! @file
!! @brief C Interfaces for SCORPIO APIs for defining variable
!! dimensions

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_def_var_cint'

MODULE spio_def_var_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to define a variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] vname The name of the variable
!! @param[in] vtype The type of the variable
!! @param[in] ndims The number of dimensions (0 for scalar) in the variable
!! @param[in] dimids The ids of the variable dimensions
!! @param[out] varid The id of the variable is returned in this argument
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_def_var(fh, vname, vtype, ndims, dimids, varid)&
                          bind(C,name="PIOc_def_var")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    CHARACTER(C_CHAR), DIMENSION(*) :: vname
    INTEGER(C_INT), VALUE :: vtype
    INTEGER(C_INT), VALUE :: ndims
    INTEGER(C_INT), DIMENSION(*) :: dimids
    INTEGER(C_INT) :: varid
  END FUNCTION PIOc_def_var
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to define compression settings for
!! a variable. Currently only supported for NetCDF4/HDF5 variables
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] shuffle The shuffle filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate The deflate filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate_level The deflate filter level setting for the variable. The levels
!!                          range from 1 (lowest compression) to 9 (highest level of compression)
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_def_var_deflate(fh, varid, shuffle, deflate, deflate_level)&
                          bind(C,name="PIOc_def_var_deflate")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT), VALUE :: shuffle
    INTEGER(C_INT), VALUE :: deflate
    INTEGER(C_INT), VALUE :: deflate_level
  END FUNCTION PIOc_def_var_deflate
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to define chunk settings for
!! a variable. Currently only supported for NetCDF4/HDF5 variables
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] storage The storage setting for the variable. @copydoc PIO_chunking_settings
!! @param[in] chunksizes The chunk sizes for the variable. The size of this array should
!!                        be equal to the number of dimensions of the variable and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_def_var_chunking(fh, varid, storage, chunksizes)&
                          bind(C,name="PIOc_def_var_chunking")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT), VALUE :: storage
    INTEGER(C_INT), DIMENSION(*) :: chunksizes
  END FUNCTION PIOc_def_var_chunking
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to setting chunk cache settings for
!! a variable. Currently only supported for NetCDF4/HDF5 variables
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
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
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_set_var_chunk_cache(fh, varid, chunk_cache_size,&
                                                    chunk_cache_nelems,&
                                                    chunk_cache_preemption)&
                          bind(C,name="PIOc_set_var_chunk_cache")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE :: chunk_cache_size
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE :: chunk_cache_nelems
    REAL(C_FLOAT), VALUE :: chunk_cache_preemption
  END FUNCTION PIOc_set_var_chunk_cache
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to getting chunk cache settings for
!! a variable. Currently only supported for NetCDF4/HDF5 variables
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] chunk_cache_size The size of the chunk cache.
!! @param[out] chunk_cache_nelems The number of elements in the chunk cache
!! @param[out] chunk_cache_preemption The cache preemption strategy for chunk caches.
!!                                    This option controls how often read/write data
!!                                    chunks are evicted from the chunk cache. The
!!                                    value is in the range [0,1], 0 implying that
!!                                    read/written chunks are treated like other
!!                                    chunks (LRU eviction policy) and a 1 implying
!!                                    that read/write chunks are evicted immediately
!!                                    after the data is read/written. A value between
!!                                    0 & 1 allows the user to control the eviction
!!                                    policy between these two schemes.
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_get_var_chunk_cache(fh, varid, chunk_cache_size,&
                                                    chunk_cache_nelems,&
                                                    chunk_cache_preemption)&
                          bind(C,name="PIOc_get_var_chunk_cache")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: chunk_cache_size
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: chunk_cache_nelems
    REAL(C_FLOAT) :: chunk_cache_preemption
  END FUNCTION PIOc_get_var_chunk_cache
END INTERFACE

INTERFACE
!> @private
!! @brief C function to get the id of the I/O system associated with file
!! (used by the Fortran interface)
!!
!! @details
!! @param[in] fh The file handle/id
!! @returns The I/O system id (for the I/O system used to create/open the file) &
!! -1 on error
  INTEGER(C_INT) FUNCTION PIOc_get_iosystem(fh) &
                            bind(C,name="PIOc_get_iosystem")
    USE iso_c_binding
    INTEGER(C_INT), INTENT(IN), VALUE :: fh
  END FUNCTION PIOc_get_iosystem
END INTERFACE

END MODULE spio_def_var_cint
