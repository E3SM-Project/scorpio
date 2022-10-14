!> @file
!! @brief SCORPIO APIs for inquiring variables
!! This file contains the SCORPIO APIs for inquiring about
!! variable (id, type, dimensions etc)
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_var.F90'

MODULE spio_inq_var
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t,&
                        PIO_MAX_NAME, PIO_MAX_VAR_DIMS, PIO_NOERR, PIO_EINTERNAL
  USE pio_kinds, ONLY : i4, PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_util, ONLY : f2cstring, c2fstring, get_text_var_sz, get_var_dim_sz
  USE spio_err, ONLY  : pio_error, pio_warn
  USE spio_inq_var_cint
#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_inq_varid, pio_inq_varname, pio_inq_vartype, pio_inq_varndims,&
            pio_inq_varnatts, pio_inq_vardimid, pio_inq_var_deflate,&
            pio_inquire_variable

!> @defgroup PIO_inq_varid PIO_inq_varid
!! @public
!! @brief Inquire variable id
!!
!! @details
!! This API can be used to inquire the unique id associated with a variable in
!! a file.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_varid
    !! pio_inq_varid_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varid_file_vid
    !! pio_inq_varid_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varid_fh_vid
    !! pio_inq_varid_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_varid_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_varname PIO_inq_varname
!! @public
!! @brief Inquire variable name
!!
!! @details
!! This API can be used to inquire the name of a variable in a file.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_varname
    !! pio_inq_varname_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varname_file_vid
    !! pio_inq_varname_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varname_fh_vid
    !! pio_inq_varname_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_varname_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_vartype PIO_inq_vartype
!! @public
!! @brief Inquire the type of a variable
!!
!! @details
!! This API can be used to inquire the type of a variable in a file
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_vartype
    !! pio_inq_vartype_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_vartype_file_vid
    !! pio_inq_vartype_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_vartype_fh_vid
    !! pio_inq_vartype_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_vartype_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_varndims PIO_inq_varndims
!! @public
!! @brief Inquire the number of dimensions of a variable
!!
!! @details
!! This API can be used to inquire the number of dimensions in a variable in a file
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_varndims
    !! pio_inq_varndims_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varndims_file_vid
    !! pio_inq_varndims_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varndims_fh_vid
    !! pio_inq_varndims_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_varndims_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_varnatts PIO_inq_varnatts
!! @public
!! @brief Inquire the number of attributes associated with a variable
!!
!! @details
!! This API can be used to inquire the number of attributes associated with a
!! variable in a file
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_varnatts
    !! pio_inq_varnatts_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varnatts_file_vid
    !! pio_inq_varnatts_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_varnatts_fh_vid
    !! pio_inq_varnatts_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_varnatts_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_vardimid PIO_inq_vardimid
!! @public
!! @brief Inquire the ids of the dimensions of a variable
!!
!! @details
!! This API can be used to inquire the unique ids of the dimensions of a
!! variable in a file. The dimensions of the variables are defined using
!! @fn PIO_def_dim
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_vardimid
    !! pio_inq_vardimid_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_vardimid_file_vid
    !! pio_inq_vardimid_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_vardimid_fh_vid
    !! pio_inq_vardimid_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_vardimid_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_var_deflate PIO_inq_var_deflate
!! @public
!! @brief Inquire the compression settings for a variable in a NetCDF4 file
!!
!! @details
!! This API can be used to inquire the HDF5 deflate/shuffle compression settings
!! of a variable in a NetCDF4 file. An error code is returned if the provided
!! file is not in the NetCDF4 (HDF5) file format.
!!
!! Shuffle and Deflate are filters available with HDF5 to aid in data compression.
!! The shuffle option is frequently used/enabled with the deflate option to
!! shuffle the data prior to compression, usually improving the compression
!! ratio of the data. The deflate option is a set of levels, 1-9, that can be
!! used to control the amount of compression (deflate level 1 is faster than
!! level 9, however level 9 would compress data more than level 1).
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_var_deflate
    !! pio_inq_var_deflate_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_var_deflate_file_vid
    !! pio_inq_var_deflate_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_var_deflate_fh_vid
    !! pio_inq_var_deflate_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_var_deflate_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_var_chunking PIO_inq_var_chunking
!! @public
!! @brief Inquire the chunk settings for a variable in a NetCDF4 file
!!
!! @details
!! This API can be used to inquire the HDF5 chunking settings of a variable
!! in a NetCDF4 file. An error code is returned if the provided file is not
!! in the NetCDF4 (HDF5) file format.
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
  INTERFACE pio_inq_var_chunking
    !! pio_inq_var_chunking_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_var_chunking_file_vid
    !! pio_inq_var_chunking_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_var_chunking_fh_vid
    !! pio_inq_var_chunking_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_var_chunking_file_vdesc
  END INTERFACE

!> @defgroup PIO_inquire_variable PIO_inquire_variable
!! @public
!! @brief Inquire meta-data associated with a variable
!!
!! @details
!! This API can be used to inquire the meta-data associated with a
!! variable in a file. The user can query the name, type, number of dimensions &
!! attributes and the dimension ids of a variable.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inquire_variable
    !! pio_inquire_variable_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inquire_variable_file_vid
    !! pio_inquire_variable_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inquire_variable_fh_vid
    !! pio_inquire_variable_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inquire_variable_file_vdesc
  END INTERFACE

CONTAINS

!>
!! @public
!! @ingroup PIO_inq_varid
!! @brief Inquire/get the unique id associated with a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vname  The name of the variable
!! @param[out] varid The id of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varid_file_vid(file, vname, varid) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: vname
    INTEGER, INTENT(OUT) :: varid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    ! Since we always increment varid in Fortran after receiving it from C
    ! ensure that its invalid even after the increment
    INTEGER(C_INT), PARAMETER :: INVALID_VARID = -2
    INTEGER(C_INT) :: cvarid = INVALID_VARID

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varid_file_vid")
#endif
    IF(LEN_TRIM(vname) == 0) THEN
      WRITE(log_msg, *) "Invalid variable name (empty string) passed to pio_inq_varid() function,",&
                        " pio_inq_varid_file_vid. variable name = ",&
                        TRIM(vname), ", file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_varid(file%fh, TRIM(vname) // C_NULL_CHAR, cvarid)
    ! FIXME: Do we need to follow the convention of Fortran varids starting from 1?
    varid = INT(cvarid) + 1

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varid_file_vid")
#endif
  END FUNCTION pio_inq_varid_file_vid

!>
!! @public
!! @ingroup PIO_inq_varid
!! @brief Inquire/get the unique id associated with a variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] vname  The name of the variable
!! @param[out] varid The id of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varid_fh_vid(fh, vname, varid) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    CHARACTER(LEN=*), INTENT(IN) :: vname
    INTEGER, INTENT(OUT) :: varid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varid_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(LEN_TRIM(vname) == 0) THEN
      WRITE(log_msg, *) "Invalid variable name (empty string) passed to pio_inq_varid() function,",&
                        " pio_inq_varid_fh_vid. variable name = ", TRIM(vname), ", file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_varid(file, vname, varid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varid_fh_vid")
#endif
  END FUNCTION pio_inq_varid_fh_vid

!>
!! @public
!! @ingroup PIO_inq_varid
!! @brief Inquire/get the unique id associated with a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vname  The name of the variable
!! @param[out] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varid_file_vdesc(file, vname, vdesc) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: vname
    TYPE(var_desc_t), INTENT(OUT) :: vdesc

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER :: varid

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varid_file_vdesc")
#endif
    IF(LEN_TRIM(vname) == 0) THEN
      WRITE(log_msg, *) "Invalid variable name (empty string) passed to pio_inq_varid() function,",&
                        " pio_inq_varid_file_vdesc(). variable name = ",&
                        TRIM(vname), ", file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_varid(file, vname, varid)
    ! FIXME: Why are varids i4s instead of regular integers or c_ints?
    vdesc%varid = INT(varid, i4)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varid_file_vdesc")
#endif
  END FUNCTION pio_inq_varid_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_varname
!! @brief Inquire/get the name of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] vname  The name of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varname_file_vid(file, varid, vname) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(OUT) :: vname

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    CHARACTER(C_CHAR) :: cvname(PIO_MAX_NAME)

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varname_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_varname() function,",&
                        " pio_inq_varname_file_vid(). varid = ", varid, ", file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    cvname = C_NULL_CHAR
    ierr = PIOc_inq_varname(file%fh, varid - 1, cvname, PIO_MAX_NAME)
    IF(ierr == PIO_NOERR) THEN
      ierr = c2fstring(file%iosystem, cvname, PIO_MAX_NAME, PIO_MAX_NAME, vname)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to convert variable name from C string to a Fortran string",&
                          "(varid = ", varid,") in pio_inq_varname() function,",&
                          " pio_inq_varname_file_vid(). varid = ", varid, ", file id = ", file%fh
        ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varname_file_vid")
#endif
  END FUNCTION pio_inq_varname_file_vid

!>
!! @public
!! @ingroup PIO_inq_varname
!! @brief Inquire/get the name of a variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vname  The name of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varname_fh_vid(fh, varid, vname) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(OUT) :: vname

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varname_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (varid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") or variable id (varid = ", varid,&
                        ") passed to pio_inq_varname() function, pio_inq_varname_fh_vid()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_varname(file, varid, vname)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varname_fh_vid")
#endif
  END FUNCTION pio_inq_varname_fh_vid

!>
!! @public
!! @ingroup PIO_inq_varname
!! @brief Inquire/get the name of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] vname  The name of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varname_file_vdesc(file, vdesc, vname) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    CHARACTER(LEN=*), INTENT(OUT) :: vname

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varname_file_vdesc")
#endif
    ierr = pio_inq_varname(file, INT(vdesc%varid), vname)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varname_file_vdesc")
#endif
  END FUNCTION pio_inq_varname_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_vartype
!! @brief Inquire/get the type of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] vtype The type of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_vartype_file_vid(file, varid, vtype) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: vtype

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cvtype

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_vartype_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_vartype() function,",&
                        " pio_inq_vartype_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_vartype(file%fh, INT(varid, C_INT) - 1, cvtype)
    vtype = INT(cvtype)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_vartype_file_vid")
#endif
  END FUNCTION pio_inq_vartype_file_vid

!>
!! @public
!! @ingroup PIO_inq_vartype
!! @brief Inquire/get the type of a variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vtype The type of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_vartype_fh_vid(fh, varid, vtype) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: vtype

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_vartype_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_vartype() function,",&
                        " pio_inq_vartype_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_vartype(file, varid, vtype)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_vartype_fh_vid")
#endif
  END FUNCTION pio_inq_vartype_fh_vid

!>
!! @public
!! @ingroup PIO_inq_vartype
!! @brief Inquire/get the type of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] vtype  The type of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_vartype_file_vdesc(file, vdesc, vtype) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(OUT) :: vtype

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_vartype_file_vdesc")
#endif

    ierr = pio_inq_vartype(file, INT(vdesc%varid), vtype)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_vartype_file_vdesc")
#endif
  END FUNCTION pio_inq_vartype_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_varndims
!! @brief Inquire/get the number of dimensions of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] vndims The number of dimensions of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varndims_file_vid(file, varid, vndims) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: vndims

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cvndims

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varndims_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_varndims() function,",&
                        " pio_inq_varndims_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_varndims(file%fh, INT(varid, C_INT) - 1, cvndims)
    vndims = INT(cvndims)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varndims_file_vid")
#endif
  END FUNCTION pio_inq_varndims_file_vid

!>
!! @public
!! @ingroup PIO_inq_varndims
!! @brief Inquire/get the number of dimensions of a variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vndims The number of dimensions of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varndims_fh_vid(fh, varid, vndims) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: vndims

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varndims_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_varndims() function,",&
                        " pio_inq_varndims_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_varndims(file, varid, vndims)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varndims_fh_vid")
#endif
  END FUNCTION pio_inq_varndims_fh_vid

!>
!! @public
!! @ingroup PIO_inq_varndims
!! @brief Inquire/get the number of dimensions of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] vndims The number of dimensions of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varndims_file_vdesc(file, vdesc, vndims) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(OUT) :: vndims

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varndims_file_vdesc")
#endif

    ierr = pio_inq_varndims(file, INT(vdesc%varid), vndims)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varndims_file_vdesc")
#endif
  END FUNCTION pio_inq_varndims_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_varnatts
!! @brief Inquire/get the number of attributes associated with a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] vnatts The number of attributes associated with the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varnatts_file_vid(file, varid, vnatts) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: vnatts

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cvnatts

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varnatts_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_varnatts() function,",&
                        " pio_inq_varnatts_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_varnatts(file%fh, varid - 1, cvnatts)
    vnatts = INT(cvnatts)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varnatts_file_vid")
#endif
  END FUNCTION pio_inq_varnatts_file_vid

!>
!! @public
!! @ingroup PIO_inq_varnatts
!! @brief Inquire/get the number of attributes associated with a variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vnatts The number of attributes associated with the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varnatts_fh_vid(fh, varid, vnatts) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: vnatts

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varnatts_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_varnatts() function,",&
                        " pio_inq_varnatts_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_varnatts(file, varid, vnatts)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varnatts_fh_vid")
#endif
  END FUNCTION pio_inq_varnatts_fh_vid

!>
!! @public
!! @ingroup PIO_inq_varnatts
!! @brief Inquire/get the number of attributes associated with a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] vnatts The number of attributes associated with the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_varnatts_file_vdesc(file, vdesc, vnatts) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(OUT) :: vnatts

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_varnatts_file_vdesc")
#endif

    ierr = pio_inq_varnatts(file, INT(vdesc%varid), vnatts)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_varnatts_file_vdesc")
#endif
  END FUNCTION pio_inq_varnatts_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_vardimid
!! @brief Inquire/get the ids of the dimensions of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] vdimids The ids of the dimensions of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_vardimid_file_vid(file, varid, vdimids) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, DIMENSION(:), INTENT(OUT) :: vdimids

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cvndims
    INTEGER(C_INT), ALLOCATABLE, DIMENSION(:), TARGET :: cvdimids
    INTEGER :: i, vndims

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_vardimid_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_vardimid() function,",&
                        " pio_inq_vardimid_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_varndims(file%fh, varid - 1, cvndims)
    IF(ierr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Unable to query number of dimensions of the variable(varid = ", varid,&
                        ") in pio_inq_vardimid() function,",&
                        " pio_inq_vardimid_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    vndims = INT(cvndims)
    IF(SIZE(vdimids) < vndims) THEN
      WRITE(log_msg, *) "Insufficient space in user array (size = ", SIZE(vdimids), ")",&
                        " to store variable dimension ids, varid = ", varid,&
                        ", the number of variable dimensions = ", vndims,& 
                        ", file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ALLOCATE(cvdimids(vndims))
    ierr = PIOc_inq_vardimid(file%fh, varid - 1, cvdimids)
    IF(ierr /= PIO_NOERR) THEN
      DO i = 1,vndims
        ! FIXME: Do we need to follow the convention of having the Fortran dimension ids
        ! start from 1 instead of 0 (the C convention)
        vdimids(i) = INT(cvdimids(vndims - i + 1)) + 1
      END DO
    END IF

    DEALLOCATE(cvdimids)
#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_vardimid_file_vid")
#endif
  END FUNCTION pio_inq_vardimid_file_vid

!>
!! @public
!! @ingroup PIO_inq_vardimid
!! @brief Inquire/get the ids of the dimensions of a variable in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vdimids The ids of the dimensions of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_vardimid_fh_vid(fh, varid, vdimids) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, DIMENSION(:), INTENT(OUT) :: vdimids

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_vardimid_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_vardimid() function,",&
                        " pio_inq_vardimid_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_vardimid(file, varid, vdimids)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_vardimid_fh_vid")
#endif
  END FUNCTION pio_inq_vardimid_fh_vid

!>
!! @public
!! @ingroup PIO_inq_vardimid
!! @brief Inquire/get the ids of the dimensions of a variable in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] vdimids The ids of the dimensions of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_vardimid_file_vdesc(file, vdesc, vdimids) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, DIMENSION(:), INTENT(OUT) :: vdimids

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_vardimid_file_vdesc")
#endif

    ierr = pio_inq_vardimid(file, INT(vdesc%varid), vdimids)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_vardimid_file_vdesc")
#endif
  END FUNCTION pio_inq_vardimid_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_var_deflate
!! @brief Inquire the compression settings for a variable in a NetCDF4 file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] shuffle_status The status of the shuffle filter setting for the variable.
!!                            Set to 1 if the shuffle filter is enabled, 0 otherwise
!! @param[out] deflate_status The status of the deflate filter setting for the variable.
!!                            Set to 1 if the deflate filter is enabled, 0 otherwise
!! @param[out] deflate_level  The level of the deflate filter setting for the variable.
!!                            The deflate levels range from 1 (least compression) to 9.
!!                            The deflate level is set to 0 if the deflate filter is not
!!                            enabled for the variable (i.e., deflate_status == 0)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_var_deflate_file_vid(file, varid, shuffle_status,&
                                                deflate_status, deflate_level) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: shuffle_status
    INTEGER, INTENT(OUT) :: deflate_status
    INTEGER, INTENT(OUT) :: deflate_level

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cshuffle_status, cdeflate_status, cdeflate_level
    INTEGER(C_INT) :: ret

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_var_deflate_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_var_deflate() function,",&
                        " pio_inq_var_deflate_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF
    
    cshuffle_status = 0
    cdeflate_status = 0
    cdeflate_level = 0

    ierr = PIOc_inq_var_deflate(file%fh, INT(varid, C_INT) - 1,&
                                cshuffle_status, cdeflate_status, cdeflate_level)

    shuffle_status = INT(cshuffle_status)
    deflate_status = INT(cdeflate_status)
    deflate_level = INT(cdeflate_level)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_var_deflate_file_vid")
#endif
  END FUNCTION pio_inq_var_deflate_file_vid

!>
!! @public
!! @ingroup PIO_inq_var_deflate
!! @brief Inquire the compression settings for a variable in a NetCDF4 file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] shuffle_status The status of the shuffle filter setting for the variable.
!!                            Set to 1 if the shuffle filter is enabled, 0 otherwise
!! @param[out] deflate_status The status of the deflate filter setting for the variable.
!!                            Set to 1 if the deflate filter is enabled, 0 otherwise
!! @param[out] deflate_level  The level of the deflate filter setting for the variable.
!!                            The deflate levels range from 1 (least compression) to 9.
!!                            The deflate level is set to 0 if the deflate filter is not
!!                            enabled for the variable (i.e., deflate_status == 0)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_var_deflate_fh_vid(fh, varid, shuffle_status,&
                                              deflate_status, deflate_level) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: shuffle_status
    INTEGER, INTENT(OUT) :: deflate_status
    INTEGER, INTENT(OUT) :: deflate_level

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_var_deflate_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_var_deflate() function,",&
                        " pio_inq_var_deflate_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_var_deflate(file, varid, shuffle_status, deflate_status, deflate_level)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_var_deflate_fh_vid")
#endif
  END FUNCTION pio_inq_var_deflate_fh_vid

!>
!! @public
!! @ingroup PIO_inq_var_deflate
!! @brief Inquire the compression settings for a variable in a NetCDF4 file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] shuffle_status The status of the shuffle filter setting for the variable.
!!                            Set to 1 if the shuffle filter is enabled, 0 otherwise
!! @param[out] deflate_status The status of the deflate filter setting for the variable.
!!                            Set to 1 if the deflate filter is enabled, 0 otherwise
!! @param[out] deflate_level  The level of the deflate filter setting for the variable.
!!                            The deflate levels range from 1 (least compression) to 9.
!!                            The deflate level is set to 0 if the deflate filter is not
!!                            enabled for the variable (i.e., deflate_status == 0)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_var_deflate_file_vdesc(file, vdesc, shuffle_status,&
                                                  deflate_status, deflate_level) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(OUT) :: shuffle_status
    INTEGER, INTENT(OUT) :: deflate_status
    INTEGER, INTENT(OUT) :: deflate_level

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_var_deflate_file_vdesc")
#endif

    ierr = pio_inq_var_deflate(file, INT(vdesc%varid), shuffle_status,&
                                deflate_status, deflate_level)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_var_deflate_file_vdesc")
#endif
  END FUNCTION pio_inq_var_deflate_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_var_chunking
!! @brief Inquire the chunking settings for a variable in a NetCDF4 file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] storage The storage setting for the variable. @copydoc PIO_chunking_settings
!! @param[out] chunksizes The chunk sizes for the variable. The size of this array should
!!                        be equal to the number of dimensions of the variable and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_var_chunking_file_vid(file, varid, storage, chunksizes) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: storage
    INTEGER, DIMENSION(:), INTENT(OUT) :: chunksizes

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cstorage
    INTEGER(C_INT), DIMENSION(:), ALLOCATABLE :: cchunksizes
    INTEGER(C_INT) :: ret
    INTEGER :: i

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_var_chunking_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_var_chunking() function,",&
                        " pio_inq_var_chunking_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF
    
    cstorage = 0
    ALLOCATE(cchunksizes(SIZE(chunksizes)))
    DO i = 1, SIZE(chunksizes)
      cchunksizes(i) = 0
    END DO
    ierr = PIOc_inq_var_chunking(file%fh, INT(varid, C_INT) - 1, cstorage, cchunksizes)

    storage = INT(cstorage)
    DO i = 1, SIZE(chunksizes)
      chunksizes(i) = INT(cchunksizes(i))
    END DO
    DEALLOCATE(cchunksizes)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_var_chunking_file_vid")
#endif
  END FUNCTION pio_inq_var_chunking_file_vid

!>
!! @public
!! @ingroup PIO_inq_var_chunking
!! @brief Inquire the chunking settings for a variable in a NetCDF4 file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] storage The storage setting for the variable. @copydoc PIO_chunking_settings
!! @param[out] chunksizes The chunk sizes for the variable. The size of this array should
!!                        be equal to the number of dimensions of the variable and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_var_chunking_fh_vid(fh, varid, storage, chunksizes) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: storage
    INTEGER, DIMENSION(:), INTENT(OUT) :: chunksizes

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_var_chunking_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_var_chunking() function,",&
                        " pio_inq_var_chunking_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_var_chunking(file, varid, storage, chunksizes)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_var_chunking_fh_vid")
#endif
  END FUNCTION pio_inq_var_chunking_fh_vid

!>
!! @public
!! @ingroup PIO_inq_var_chunking
!! @brief Inquire the chunking settings for a variable in a NetCDF4 file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] storage The storage setting for the variable. @copydoc PIO_chunking_settings
!! @param[out] chunksizes The chunk sizes for the variable. The size of this array should
!!                        be equal to the number of dimensions of the variable and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_var_chunking_file_vdesc(file, vdesc, storage, chunksizes) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(OUT) :: storage
    INTEGER, DIMENSION(:), INTENT(OUT) :: chunksizes

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_var_chunking_file_vdesc")
#endif

    ierr = pio_inq_var_chunking(file, INT(vdesc%varid), storage, chunksizes)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_var_chunking_file_vdesc")
#endif
  END FUNCTION pio_inq_var_chunking_file_vdesc

!>
!! @public
!! @ingroup PIO_inquire_variable
!! @brief Get the meta-data associated with a variable
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable
!! @param[out] name  (Optional) The name of the variable
!! @param[out] xtype (Optional) The type of the variable
!! @param[out] ndims (Optional) The number of dimensions in the variable
!! @param[out] dimids (Optional) The dimension ids of the dimensions of the variable
!! @param[out] natts (Optional) The number of attributes of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inquire_variable_file_vid(file, varid, name, xtype,&
                                                  ndims, dimids, natts) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: name
    INTEGER, OPTIONAL, INTENT(OUT) :: xtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ndims
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: dimids
    INTEGER, OPTIONAL, INTENT(OUT) :: natts

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_inquire_variable_file_vid")
#endif
    IF(varid <= 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inquire_variable() function,",&
                        " pio_inquire_variable_file_vid(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIO_NOERR

    IF(PRESENT(name)) THEN
      ierr = pio_inq_varname(file, varid, name)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query variable name (varid = ", varid,&
                          ") in pio_inquire_variable() function,",&
                          " pio_inquire_variable_file_vid(). file id = ", file%fh
        ierr = pio_error(file%iosystem, ierr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(xtype)) THEN
      ierr = pio_inq_vartype(file, varid, xtype)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query variable type (varid = ", varid,&
                          ") in pio_inquire_variable() function,",&
                          " pio_inquire_variable_file_vid(). file id = ", file%fh
        ierr = pio_error(file%iosystem, ierr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(ndims)) THEN
      ierr = pio_inq_varndims(file, varid, ndims)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query the number of dimensions in variable (varid = ", varid,&
                          ") in pio_inquire_variable() function,",&
                          " pio_inquire_variable_file_vid(). file id = ", file%fh
        ierr = pio_error(file%iosystem, ierr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(dimids)) THEN
      ierr = pio_inq_vardimid(file, varid, dimids)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query the ids of dimensions in variable (varid = ", varid,&
                          ") in pio_inquire_variable() function,",&
                          " pio_inquire_variable_file_vid(). file id = ", file%fh
        ierr = pio_error(file%iosystem, ierr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(natts)) THEN
      ierr = pio_inq_varnatts(file, varid, natts)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query the number of attributes in variable (varid = ", varid,&
                          ") in pio_inquire_variable() function,",&
                          " pio_inquire_variable_file_vid(). file id = ", file%fh
        ierr = pio_error(file%iosystem, ierr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inquire_variable_file_vid")
#endif
  END FUNCTION pio_inquire_variable_file_vid

!>
!! @public
!! @ingroup PIO_inquire_variable
!! @brief Get the meta-data associated with a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] name  (Optional) The name of the variable
!! @param[out] xtype (Optional) The type of the variable
!! @param[out] ndims (Optional) The number of dimensions in the variable
!! @param[out] dimids (Optional) The dimension ids of the dimensions of the variable
!! @param[out] natts (Optional) The number of attributes of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inquire_variable_fh_vid(fh, varid, name, xtype,&
                                                  ndims, dimids, natts) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: name
    INTEGER, OPTIONAL, INTENT(OUT) :: xtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ndims
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: dimids
    INTEGER, OPTIONAL, INTENT(OUT) :: natts

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inquire_variable_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (varid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") or variable id (varid = ", varid,&
                        ") passed to pio_inquire_variable() function, pio_inquire_variable_fh_vid()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inquire_variable(file, varid, name, xtype, ndims, dimids, natts)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inquire_variable_fh_vid")
#endif
  END FUNCTION pio_inquire_variable_fh_vid

!>
!! @public
!! @ingroup PIO_inquire_variable
!! @brief Get the meta-data associated with a variable
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[out] name  (Optional) The name of the variable
!! @param[out] xtype (Optional) The type of the variable
!! @param[out] ndims (Optional) The number of dimensions in the variable
!! @param[out] dimids (Optional) The dimension ids of the dimensions of the variable
!! @param[out] natts (Optional) The number of attributes of the variable
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inquire_variable_file_vdesc(file, vdesc, name, xtype,&
                                                    ndims, dimids, natts) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: name
    INTEGER, OPTIONAL, INTENT(OUT) :: xtype
    INTEGER, OPTIONAL, INTENT(OUT) :: ndims
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: dimids
    INTEGER, OPTIONAL, INTENT(OUT) :: natts

#ifdef TIMING
    CALL t_startf("PIO:pio_inquire_variable_file_vdesc")
#endif
    ierr = pio_inquire_variable(file, INT(vdesc%varid), name, xtype, ndims, dimids, natts)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inquire_variable_file_vdesc")
#endif
  END FUNCTION pio_inquire_variable_file_vdesc

END MODULE spio_inq_var
