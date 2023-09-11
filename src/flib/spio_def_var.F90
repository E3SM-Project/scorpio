!> @file
!! @brief SCORPIO APIs for defining variables
!! This file contains the SCORPIO APIs for inquiring about
!! attribute (id, type, dimensions etc)
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_def_var.F90'

MODULE spio_def_var
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t,&
                        PIO_MAX_NAME, PIO_MAX_VAR_DIMS, PIO_NOERR, PIO_EINTERNAL
  USE pio_kinds, ONLY : i4, PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_util, ONLY : f2cstring, get_text_var_sz, get_var_dim_sz
  USE spio_err, ONLY  : pio_error, pio_warn
  USE spio_def_var_cint
#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_def_var, pio_def_var_deflate, pio_def_var_chunking,&
            pio_set_var_chunk_cache, pio_get_var_chunk_cache

!> @defgroup PIO_def_var PIO_def_var
!! @public
!! @brief Define a variable
!!
!! @details
!! This API can be used to define a variable in a file. The variables need
!! to defined before writing data into them. Variables can be used to store
!! scalar (0 dimensions) and array (multidimensional) data into a file. Each
!! dimension of a multidimensional array needs to be defined separately before
!! defining a variable (Note that multiple variables can share the same
!! dimensions). A variable can also be associated with attributes to store
!! meta-data about the variables.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_def_var
    !! pio_def_var_file_vid version of the function accepts a file desc &
    !! a variable desc as arguments.
    MODULE PROCEDURE pio_def_var_md_file_vdesc
    MODULE PROCEDURE pio_def_var_0d_file_vdesc
    !! pio_def_var_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_def_var_md_fh_vid
    MODULE PROCEDURE pio_def_var_0d_fh_vid
  END INTERFACE

!> @defgroup PIO_def_var_deflate PIO_def_var_deflate
!! @public
!! @brief Set deflate & shuffle filter setting for the variable
!!
!! @details
!! This API can be used to set/change the deflate (NetCDF4/HDF5 compression) filter
!! setting for the variable. The deflate levels range from 1 (low compression)
!! to 9 (highest compression). This setting is only valid for NetCDF4/HDF5
!! files.
!!
!! Note that frequently the shuffle filter setting is also enabled (to shuffle
!! data before compression, to improve the ratio of compression) when the
!! deflate setting is enabled. The shuffle filter is a low cost filter that
!! improves the compression of data.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_def_var_deflate
    !! pio_def_var_deflate_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_def_var_deflate_file_vdesc
    !! pio_def_var_deflate_file_vid version of the function accepts a file desc &
    !! a variable id as arguments
    MODULE PROCEDURE pio_def_var_deflate_file_vid
    !! pio_def_var_deflate_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_def_var_deflate_fh_vid
  END INTERFACE

!> @defgroup PIO_def_var_chunking PIO_def_var_chunking
!! @public
!! @brief Set the chunk setting for the variable
!!
!! @details
!! This API can be used to set/change the variable chunking settings for a
!! variable in a NetCDF4/HDF5 file.
!!
!! Instead of storing the array based data contiguously in the output file
!! users can split ("chunk") the data into chunks that correspond to the
!! semantic shape of how the data is accessed in the application. Storing
!! data in these semantic chunks that correspond to how the data is accessed
!! in the application can significantly improve the read I/O performance of the
!! application.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_def_var_chunking
    !! pio_def_var_chunking_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_def_var_chunking_file_vdesc
    !! pio_def_var_chunking_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_def_var_chunking_fh_vid
  END INTERFACE

!> @defgroup PIO_set_var_chunk_cache PIO_set_var_chunk_cache
!! @public
!! @brief Set the chunk cache setting for a variable
!!
!! @details
!! This API can be used to set/change the variable chunking cache settings for a
!! variable in a NetCDF4/HDF5 file.
!!
!! Instead of storing the array based data contiguously in the output file
!! users can split ("chunk") the data into chunks that correspond to the
!! semantic shape of how the data is accessed in the application. Storing
!! data in these semantic chunks that correspond to how the data is accessed
!! in the application can significantly improve the read I/O performance of the
!! application. For reads/writes the low level I/O library uses caches for
!! buffering chunks of data. This API can be used to control the settings for
!! this chunk cache.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_set_var_chunk_cache
    !! pio_set_var_chunk_cache_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_set_var_chunk_cache_file_vdesc
    !! pio_set_var_chunk_cache_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_set_var_chunk_cache_fh_vid
  END INTERFACE

!> @defgroup PIO_get_var_chunk_cache PIO_get_var_chunk_cache
!! @public
!! @brief Get the chunk cache setting for a variable
!!
!! @details
!! This API can be used to set/change the variable chunking cache settings for a
!! variable in a NetCDF4/HDF5 file.
!!
!! Instead of storing the array based data contiguously in the output file
!! users can split ("chunk") the data into chunks that correspond to the
!! semantic shape of how the data is accessed in the application. Storing
!! data in these semantic chunks that correspond to how the data is accessed
!! in the application can significantly improve the read I/O performance of the
!! application. For reads/writes the low level I/O library uses caches for
!! buffering chunks of data. This API can be used to get the settings for
!! this chunk cache.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_get_var_chunk_cache
    !! pio_get_var_chunk_cache_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_get_var_chunk_cache_file_vdesc
    !! pio_get_var_chunk_cache_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_get_var_chunk_cache_fh_vid
  END INTERFACE

CONTAINS

!>
!! @public
!! @ingroup PIO_def_var
!! @brief Define a multidimensional variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vname The name of the variable
!! @param[in] vtype The type of the variable. @copydoc PIO_kinds
!! @param[in] vdimids The ids of the dimensions of the variable
!! @param[out] vdesc The descriptor of the variable created is stored in this argument.
!!                    @copydoc var_desc_t
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_md_file_vdesc(file, vname, vtype, vdimids, vdesc) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: vname
    INTEGER, INTENT(IN) :: vtype
    INTEGER, DIMENSION(:), INTENT(IN) :: vdimids
    TYPE(var_desc_t), INTENT(OUT) :: vdesc

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER :: i, ndims
    INTEGER(C_INT), PARAMETER :: INVALID_VARID = -1
    INTEGER(C_INT) :: cvarid = INVALID_VARID
    INTEGER(C_INT), DIMENSION(:), ALLOCATABLE :: cvdimids

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_md_file_vdesc")
#endif
    IF(LEN_TRIM(vname) == 0) THEN
      WRITE(log_msg, *) "Invalid variable name (empty string) passed to pio_def_var() function,",&
                        " pio_def_var_md_file_vdesc(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ndims = SIZE(vdimids)
    ALLOCATE(cvdimids(ndims))
    DO i=1,ndims
      cvdimids(i) = INT(vdimids(ndims - i + 1), C_INT) - 1
    END DO

    ierr = PIOc_def_var(file%fh, TRIM(vname) // C_NULL_CHAR, INT(vtype, C_INT),&
                        INT(ndims, C_INT), cvdimids, cvarid)

    ! FIXME: Fix the type of varid, so that its a C_INT or an INTEGER
    vdesc%varid = INT(cvarid, i4) + 1

    DEALLOCATE(cvdimids)
#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_md_file_vdesc")
#endif
  END FUNCTION pio_def_var_md_file_vdesc

!>
!! @public
!! @ingroup PIO_def_var
!! @brief Define a scalar variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vname The name of the variable
!! @param[in] vtype The type of the variable. @copydoc PIO_kinds
!! @param[out] vdesc The descriptor of the variable created is stored in this argument.
!!                    @copydoc var_desc_t
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_0d_file_vdesc(file, vname, vtype, vdesc) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: vname
    INTEGER, INTENT(IN) :: vtype
    TYPE(var_desc_t), INTENT(OUT) :: vdesc

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT), PARAMETER :: INVALID_VARID = -1
    INTEGER(C_INT) :: cvarid = INVALID_VARID
    INTEGER(C_INT) :: cvdimids(0)

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_0d_file_vdesc")
#endif
    IF(LEN_TRIM(vname) == 0) THEN
      WRITE(log_msg, *) "Invalid variable name (empty string) passed to pio_def_var() function,",&
                        " pio_def_var_0d_file_vdesc(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_def_var(file%fh, TRIM(vname) // C_NULL_CHAR, INT(vtype, C_INT),&
                        0, cvdimids, cvarid)

    ! FIXME: Fix the type of varid, so that its a C_INT or an INTEGER
    vdesc%varid = INT(cvarid, i4) + 1

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_0d_file_vdesc")
#endif
  END FUNCTION pio_def_var_0d_file_vdesc

!>
!! @public
!! @ingroup PIO_def_var
!! @brief Define a multidimensional variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] vname The name of the variable
!! @param[in] vtype The type of the variable. @copydoc PIO_kinds
!! @param[in] vdimids The ids of the dimensions of the variable
!! @param[out] varid The id of the variable is returned in this argument
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_md_fh_vid(fh, vname, vtype, vdimids, varid) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    CHARACTER(LEN=*), INTENT(IN) :: vname
    INTEGER, INTENT(IN) :: vtype
    INTEGER, DIMENSION(:), INTENT(IN) :: vdimids
    INTEGER, INTENT(OUT) :: varid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys
    TYPE(var_desc_t) :: vdesc

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_md_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(fh < 0) THEN
      WRITE(log_msg, *) "Invalid file handle (fh = ", fh, ")",&
                        " passed to pio_def_var() function,",&
                        " pio_def_var_md_fh_vid."
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF
    IF(LEN_TRIM(vname) == 0) THEN
      WRITE(log_msg, *) "Invalid variable name (empty string) passed to pio_def_var() function,",&
                        " pio_def_var_md_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_def_var(file, vname, vtype, vdimids, vdesc)
    varid = INT(vdesc%varid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_md_fh_vid")
#endif
  END FUNCTION pio_def_var_md_fh_vid

!>
!! @public
!! @ingroup PIO_def_var
!! @brief Define a scalar variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] vname The name of the variable
!! @param[in] vtype The type of the variable. @copydoc PIO_kinds
!! @param[out] varid The id of the variable is returned in this argument
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_0d_fh_vid(fh, vname, vtype, varid) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    CHARACTER(LEN=*), INTENT(IN) :: vname
    INTEGER, INTENT(IN) :: vtype
    INTEGER, INTENT(OUT) :: varid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys
    TYPE(var_desc_t) :: vdesc

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_0d_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(fh < 0) THEN
      WRITE(log_msg, *) "Invalid file handle (fh = ", fh, ")",&
                        " passed to pio_def_var() function,",&
                        " pio_def_var_0d_fh_vid."
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF
    IF(LEN_TRIM(vname) == 0) THEN
      WRITE(log_msg, *) "Invalid variable name (empty string) passed to pio_def_var() function,",&
                        " pio_def_var_0d_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_def_var(file, vname, vtype, vdesc)
    varid = INT(vdesc%varid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_0d_fh_vid")
#endif
  END FUNCTION pio_def_var_0d_fh_vid

!>
!! @public
!! @ingroup PIO_def_var_deflate
!! @brief Set/Change the deflate filter (NetCDF4/HDF5 compression) settings for a variable
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The variable id
!! @param[in] shuffle The shuffle filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate The deflate filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate_level The deflate filter level setting for the variable. The levels
!!                          range from 1 (lowest compression) to 9 (highest level of compression)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_deflate_file_vid(file, varid, shuffle,&
                                                  deflate, deflate_level) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: shuffle
    INTEGER, INTENT(IN) :: deflate
    INTEGER, INTENT(IN) :: deflate_level

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_deflate_file_vid")
#endif
    IF((shuffle /= 0) .AND. (shuffle /= 1)) THEN
      WRITE(log_msg, *) "Invalid shuffle filter setting (shuffle = ", shuffle, ")",&
                        " passed to pio_def_var_deflate() function, expected 0 or 1",&
                        " pio_def_var_deflate_file_vid(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    IF((deflate /= 0) .AND. (deflate /= 1)) THEN
      WRITE(log_msg, *) "Invalid deflate filter setting (deflate = ", deflate, ")",&
                        " passed to pio_def_var_deflate() function, expected 0 or 1",&
                        " pio_def_var_deflate_file_vid(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    IF((deflate < 0) .OR. (deflate > 9)) THEN
      WRITE(log_msg, *) "Invalid deflate filter level (deflate_level = ", deflate_level, ")",&
                        " passed to pio_def_var_deflate() function, expected in range [0, 9]",&
                        " pio_def_var_deflate_file_vid(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_def_var_deflate(file%fh, INT(varid, C_INT) - 1, INT(shuffle, C_INT),&
                                INT(deflate, C_INT), INT(deflate_level, C_INT))

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_deflate_file_vid")
#endif
  END FUNCTION pio_def_var_deflate_file_vid

!>
!! @public
!! @ingroup PIO_def_var_deflate
!! @brief Set/Change the deflate filter (NetCDF4/HDF5 compression) settings for a variable
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[in] shuffle The shuffle filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate The deflate filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate_level The deflate filter level setting for the variable. The levels
!!                          range from 1 (lowest compression) to 9 (highest level of compression)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_deflate_file_vdesc(file, vdesc, shuffle,&
                                                  deflate, deflate_level) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(IN) :: shuffle
    INTEGER, INTENT(IN) :: deflate
    INTEGER, INTENT(IN) :: deflate_level

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_deflate_file_vdesc")
#endif

    IF((shuffle /= 0) .AND. (shuffle /= 1)) THEN
      WRITE(log_msg, *) "Invalid shuffle filter setting (shuffle = ", shuffle, ")",&
                        " passed to pio_def_var_deflate() function, expected 0 or 1",&
                        " pio_def_var_deflate_file_vdesc(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    IF((deflate /= 0) .AND. (deflate /= 1)) THEN
      WRITE(log_msg, *) "Invalid deflate filter setting (deflate = ", deflate, ")",&
                        " passed to pio_def_var_deflate() function, expected 0 or 1",&
                        " pio_def_var_deflate_file_vdesc(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    IF((deflate < 0) .OR. (deflate > 9)) THEN
      WRITE(log_msg, *) "Invalid deflate filter level (deflate_level = ", deflate_level, ")",&
                        " passed to pio_def_var_deflate() function, expected in range [0, 9]",&
                        " pio_def_var_deflate_file_vdesc(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_def_var_deflate(file, INT(vdesc%varid), shuffle, deflate, deflate_level)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_deflate_file_vdesc")
#endif
  END FUNCTION pio_def_var_deflate_file_vdesc

!>
!! @public
!! @ingroup PIO_def_var_deflate
!! @brief Set/Change the deflate filter (NetCDF4/HDF5 compression) settings for a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] shuffle The shuffle filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate The deflate filter setting (0 to disable, 1 to enable) for the variable
!! @param[in] deflate_level The deflate filter level setting for the variable. The levels
!!                          range from 1 (lowest compression) to 9 (highest level of compression)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_deflate_fh_vid(fh, varid, shuffle, deflate, deflate_level) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: shuffle
    INTEGER, INTENT(IN) :: deflate
    INTEGER, INTENT(IN) :: deflate_level

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_deflate_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys

    IF((fh < 0) .OR. (varid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle (fh = ", fh, ")",&
                        " or variable id (varid = ", varid, ")",&
                        " passed to pio_def_var_deflate() function,",&
                        " pio_def_var_deflate_fh_vid."
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_def_var_deflate(file, varid, shuffle, deflate, deflate_level)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_deflate_fh_vid")
#endif
  END FUNCTION pio_def_var_deflate_fh_vid

!>
!! @public
!! @ingroup PIO_def_var_chunking
!! @brief Set/Change the chunking (NetCDF4/HDF5 chunking) settings for a variable
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[in] storage The storage setting for the variable. @copydoc PIO_chunking_settings
!! @param[in] chunksizes The chunk sizes for the variable. The size of this array should
!!                        be equal to the number of dimensions of the variable and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_chunking_file_vdesc(file, vdesc, storage, chunksizes) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(IN) :: storage
    INTEGER, DIMENSION(:), INTENT(IN) :: chunksizes

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER :: i, ndims
    INTEGER(C_INT), DIMENSION(:), ALLOCATABLE :: cchunksizes

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_chunking_file_vdesc")
#endif
    ! FIXME: Add checks on whether the size of chunksizes is equal to the number of dimensions
    ndims = SIZE(chunksizes)
    ALLOCATE(cchunksizes(ndims))

    DO i=1,ndims
      cchunksizes(i) = INT(chunksizes(ndims - i + 1), C_INT)
    END DO

#ifndef HAVE_NC4_CHUNK_CONSTS
    ! NetCDF4 support is available but the chunking constants (for storage option) is not
    ! defined
    WRITE(log_msg, *) "NetCDF4 support is available but the NetCDF4 chunking constants",&
                      " were not defined. The chunking storage options specified by the",&
                      " user might be ignored (pio_def_var_chunking_file_vdesc()).",&
                      " fh = ", file%fh, ", varid = ", vdesc%varid
    CALL pio_warn(file%iosystem, __PIO_FILE__, __LINE__, TRIM(log_msg))
#endif

    ierr = PIOc_def_var_chunking(file%fh, INT(vdesc%varid, C_INT) - 1,&
                                  INT(storage, C_INT), cchunksizes)

    DEALLOCATE(cchunksizes)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_chunking_file_vdesc")
#endif
  END FUNCTION pio_def_var_chunking_file_vdesc

!>
!! @public
!! @ingroup PIO_def_var_chunking
!! @brief Set/Change the chunking (NetCDF4/HDF5 chunking) settings for a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] storage The storage setting for the variable. @copydoc PIO_chunking_settings
!! @param[in] chunksizes The chunk sizes for the variable. The size of this array should
!!                        be equal to the number of dimensions of the variable and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_var_chunking_fh_vid(fh, varid, storage, chunksizes) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: storage
    INTEGER, DIMENSION(:), INTENT(IN) :: chunksizes

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys
    TYPE(var_desc_t) :: vdesc

#ifdef TIMING
    CALL t_startf("PIO:pio_def_var_chunking_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys

    IF((fh < 0) .OR. (varid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle (fh = ", fh, ")",&
                        " or variable id (varid = ", varid, ")",&
                        " passed to pio_def_var_chunking() function,",&
                        " pio_def_var_chunking_fh_vid."
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    vdesc%varid = INT(varid, i4)
    
    ierr = pio_def_var_chunking(file, vdesc, storage, chunksizes)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_var_chunking_fh_vid")
#endif
  END FUNCTION pio_def_var_chunking_fh_vid

!> @defgroup PIO_set_var_chunk_cache PIO_set_var_chunk_cache
!! @public
!! @brief Set the chunk cache settings for a variable in a file
!!
!! @details
!! Control the different settings for the chunk cache (the cache used internally
!! by the low level I/O library when reading/writing data) for files.
!! The NetCDF library sets the chunk cache to 64MB by default. Note that
!! this setting is discarded when the file is closed.
!!
!! This option is currently only supported for NetCDF4/HDF5 files.
!!
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The variable handle. @copydoc var_desc_t
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
  INTEGER FUNCTION PIO_set_var_chunk_cache_file_vdesc(file, vdesc, chunk_cache_size,&
                                        chunk_cache_nelems, chunk_cache_preemption) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: chunk_cache_size
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: chunk_cache_nelems
    REAL, INTENT(IN)  :: chunk_cache_preemption

    ierr = PIOc_set_var_chunk_cache(file%fh, INT(vdesc%varid, C_INT) - 1,&
                                INT(chunk_cache_size, PIO_OFFSET_F2C_TYPE_KIND),&
                                INT(chunk_cache_nelems, PIO_OFFSET_F2C_TYPE_KIND),&
                                REAL(chunk_cache_preemption, C_FLOAT))
  END FUNCTION PIO_set_var_chunk_cache_file_vdesc

!> @defgroup PIO_set_var_chunk_cache PIO_set_var_chunk_cache
!! @public
!! @brief Set the chunk cache settings for a variable in a file
!!
!! @details
!! Control the different settings for the chunk cache (the cache used internally
!! by the low level I/O library when reading/writing data) for files.
!! The NetCDF library sets the chunk cache to 64MB by default. Note that
!! this setting is discarded when the file is closed.
!!
!! This option is currently only supported for NetCDF4/HDF5 files.
!!
!! @param[in] fh The file id/handle
!! @param[in] varid The variable id
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
  INTEGER FUNCTION PIO_set_var_chunk_cache_fh_vid(fh, varid, chunk_cache_size,&
                                        chunk_cache_nelems, chunk_cache_preemption) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: chunk_cache_size
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: chunk_cache_nelems
    REAL, INTENT(IN)  :: chunk_cache_preemption

    ierr = PIOc_set_var_chunk_cache(INT(fh, C_INT), INT(varid, C_INT) - 1,&
                                INT(chunk_cache_size, PIO_OFFSET_F2C_TYPE_KIND),&
                                INT(chunk_cache_nelems, PIO_OFFSET_F2C_TYPE_KIND),&
                                REAL(chunk_cache_preemption, C_FLOAT))
  END FUNCTION PIO_set_var_chunk_cache_fh_vid

!> @defgroup PIO_get_var_chunk_cache PIO_get_var_chunk_cache
!! @public
!! @brief Get the chunk cache gettings for a variable in a file
!!
!! @details
!! Get the different gettings for the chunk cache (the cache used internally
!! by the low level I/O library when reading/writing data) for files.
!!
!! This option is currently only supported for NetCDF4/HDF5 files.
!!
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The variable handle. @copydoc var_desc_t
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
  INTEGER FUNCTION PIO_get_var_chunk_cache_file_vdesc(file, vdesc, chunk_cache_size,&
                                        chunk_cache_nelems, chunk_cache_preemption) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: chunk_cache_size
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: chunk_cache_nelems
    REAL, INTENT(OUT)  :: chunk_cache_preemption

    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cchunk_cache_size
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cchunk_cache_nelems
    REAL(C_FLOAT) :: cchunk_cache_preemption

    ierr = PIOc_get_var_chunk_cache(file%fh, INT(vdesc%varid, C_INT) - 1,&
                                cchunk_cache_size, cchunk_cache_nelems, cchunk_cache_preemption)

    chunk_cache_size = INT(cchunk_cache_size, PIO_OFFSET_F2C_TYPE_KIND)
    chunk_cache_nelems = INT(cchunk_cache_nelems, PIO_OFFSET_F2C_TYPE_KIND)
    chunk_cache_preemption = REAL(cchunk_cache_preemption)

  END FUNCTION PIO_get_var_chunk_cache_file_vdesc

!> @defgroup PIO_get_var_chunk_cache PIO_get_var_chunk_cache
!! @public
!! @brief Get the chunk cache gettings for a variable in a file
!!
!! @details
!! Get the different gettings for the chunk cache (the cache used internally
!! by the low level I/O library when reading/writing data) for files.
!!
!! This option is currently only supported for NetCDF4/HDF5 files.
!!
!! @param[in] fh The file id/handle
!! @param[in] varid The variable id/handle
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
  INTEGER FUNCTION PIO_get_var_chunk_cache_fh_vid(fh, varid, chunk_cache_size,&
                                        chunk_cache_nelems, chunk_cache_preemption) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: chunk_cache_size
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: chunk_cache_nelems
    REAL, INTENT(OUT)  :: chunk_cache_preemption

    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cchunk_cache_size
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cchunk_cache_nelems
    REAL(C_FLOAT) :: cchunk_cache_preemption

    ! FIXME : Add sanity checks for fh/varid
    ierr = PIOc_get_var_chunk_cache(fh, INT(varid, C_INT) - 1,&
                                cchunk_cache_size, cchunk_cache_nelems, cchunk_cache_preemption)

    chunk_cache_size = INT(cchunk_cache_size, PIO_OFFSET_F2C_TYPE_KIND)
    chunk_cache_nelems = INT(cchunk_cache_nelems, PIO_OFFSET_F2C_TYPE_KIND)
    chunk_cache_preemption = REAL(cchunk_cache_preemption)

  END FUNCTION PIO_get_var_chunk_cache_fh_vid

END MODULE spio_def_var
