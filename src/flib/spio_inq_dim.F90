!> @file
!! @brief SCORPIO APIs for inquiring variables
!! This file contains the SCORPIO APIs for inquiring about
!! variable (id, type, dimensions etc)
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_dim.F90'

MODULE spio_inq_dim
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t,&
                        PIO_MAX_NAME, PIO_MAX_VAR_DIMS, PIO_NOERR, PIO_EINTERNAL
  USE pio_kinds, ONLY : i4, PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_util, ONLY : f2cstring, c2fstring, get_text_var_sz, get_var_dim_sz
  USE spio_err, ONLY  : pio_error, pio_warn
  USE spio_inq_dim_cint
#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_inq_dimid, pio_inq_dimname, pio_inq_dimlen,&
            pio_inquire_dimension

!> @defgroup PIO_inq_dimid PIO_inq_dimid
!! @public
!! @brief Inquire the id of a dimension
!!
!! @details
!! This API can be used to inquire the unique id associated with a dimension
!! using the dimension name.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_dimid
    !! pio_inq_dimid_file_dimid version of the function accepts a file desc &
    !! a dimension id/handle as arguments
    MODULE PROCEDURE pio_inq_dimid_file_dimid
    !! pio_inq_dimid_fh_dimid version of the function accepts a file id/handle &
    !! a dimension id/handle as arguments
    MODULE PROCEDURE pio_inq_dimid_fh_dimid
  END INTERFACE

!> @defgroup PIO_inq_dimname PIO_inq_dimname
!! @public
!! @brief Inquire the name of a dimension
!!
!! @details
!! This API can be used to inquire the name of a dimension using the dimension
!! id
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_dimname
    !! pio_inq_dimname_file_dimid version of the function accepts a file desc &
    !! a dimension id/handle as arguments
    MODULE PROCEDURE pio_inq_dimname_file_dimid
    !! pio_inq_dimname_fh_dimid version of the function accepts a file id/handle &
    !! a dimension id/handle as arguments
    MODULE PROCEDURE pio_inq_dimname_fh_dimid
  END INTERFACE

!> @defgroup PIO_inq_dimlen PIO_inq_dimlen
!! @public
!! @brief Inquire the length/size of a dimension
!!
!! @details
!! This API can be used to inquire the length of a dimension
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_dimlen
    !! pio_inq_dimlen_file_dimid version of the function accepts a file desc,
    !! a dimension id/handle & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_dimlen_file_dimid_offlen
    !! pio_inq_dimlen_file_dimid version of the function accepts a file desc &
    !! a dimension id/handle as arguments
    MODULE PROCEDURE pio_inq_dimlen_file_dimid
    !! pio_inq_dimlen_fh_dimid version of the function accepts a file id/handle &
    !! a dimension id/handle as arguments
    MODULE PROCEDURE pio_inq_dimlen_fh_dimid
    !! pio_inq_dimlen_fh_dimid version of the function accepts a file id/handle,
    !! a dimension id/handle & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_dimlen_fh_dimid_offlen
  END INTERFACE

!> @defgroup PIO_inquire_dimension PIO_inquire_dimension
!! @public
!! @brief Inquire meta-data associated with a dimension
!!
!! @details
!! This API can be used to inquire the meta-data associated with a
!! dimension in a file. The user can query the name & length of a dimension
!! using the id of the dimension.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inquire_dimension
    !! FIXME: Since we cannot have polymorphic functions that are different only
    !! on optional args, we cannot have versions of the function for regular
    !! integers (only for PIO_OFFSET_KINDs)

    !! pio_inquire_dimension_file_dimid version of the function accepts a file desc,
    !! a dimension id/handle & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inquire_dimension_file_offlen
    !! pio_inquire_dimension_fh_dimid version of the function accepts a file id/handle,
    !! a dimension id/handle & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inquire_dimension_fh_offlen
  END INTERFACE

CONTAINS

!>
!! @public
!! @ingroup PIO_inq_dimid
!! @brief Inquire/get the unique id associated with a dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dname  The name of the dimension
!! @param[out] dimid The id of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimid_file_dimid(file, dname, dimid) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: dname
    INTEGER, INTENT(OUT) :: dimid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    ! Since we always increment dimid in Fortran after receiving it from C
    ! ensure that its invalid even after the increment
    INTEGER(C_INT), PARAMETER :: INVALID_DIMID = -2
    INTEGER(C_INT) :: cdimid = INVALID_DIMID

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimid_file_dimid")
#endif
    IF(LEN_TRIM(dname) == 0) THEN
      WRITE(log_msg, *) "Invalid dimension name (empty string) passed to pio_inq_dimid() function,",&
                        " pio_inq_dimid_file_dimid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_dimid(file%fh, TRIM(dname) // C_NULL_CHAR, cdimid)
    ! FIXME: Do we need to follow the convention of Fortran dimids starting from 1?
    dimid = INT(cdimid) + 1

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimid_file_dimid")
#endif
  END FUNCTION pio_inq_dimid_file_dimid

!>
!! @public
!! @ingroup PIO_inq_dimid
!! @brief Inquire/get the unique id associated with a dimension in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dname  The name of the dimension
!! @param[out] dimid The id of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimid_fh_dimid(fh, dname, dimid) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    CHARACTER(LEN=*), INTENT(IN) :: dname
    INTEGER, INTENT(OUT) :: dimid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimid_fh_dimid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(LEN_TRIM(dname) == 0) THEN
      WRITE(log_msg, *) "Invalid dimension name (empty string) passed to pio_inq_dimid() function,",&
                        " pio_inq_dimid_fh_dimid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_dimid(file, dname, dimid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimid_fh_dimid")
#endif
  END FUNCTION pio_inq_dimid_fh_dimid

!>
!! @public
!! @ingroup PIO_inq_dimname
!! @brief Inquire/get the name of a dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dimid The id of the dimension
!! @param[out] dname  The name of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimname_file_dimid(file, dimid, dname) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: dimid
    CHARACTER(LEN=*), INTENT(OUT) :: dname

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    CHARACTER(C_CHAR) :: cdname(PIO_MAX_NAME)

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimname_file_dimid")
#endif
    IF(dimid <= 0) THEN
      WRITE(log_msg, *) "Invalid dimension id (dimid = ", dimid,&
                        ") passed to pio_inq_dimname() function,",&
                        " pio_inq_dimname_file_dimid(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    cdname = C_NULL_CHAR
    ierr = PIOc_inq_dimname(file%fh, INT(dimid, C_INT) - 1, cdname, PIO_MAX_NAME)
    IF(ierr == PIO_NOERR) THEN
      ierr = c2fstring(file%iosystem, cdname, PIO_MAX_NAME, PIO_MAX_NAME, dname)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to convert dimension name from C string to a Fortran string",&
                          "(dimid = ", dimid,") in pio_inq_dimname() function,",&
                          " pio_inq_dimname_file_dimid(). dimid = ", dimid, ", file id = ", file%fh
        ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimname_file_dimid")
#endif
  END FUNCTION pio_inq_dimname_file_dimid

!>
!! @public
!! @ingroup PIO_inq_dimname
!! @brief Inquire/get the name of a dimension in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dimid The id of the dimension
!! @param[out] dname  The name of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimname_fh_dimid(fh, dimid, dname) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: dimid
    CHARACTER(LEN=*), INTENT(OUT) :: dname

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimname_fh_dimid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (dimid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") or dimension id (dimid = ", dimid,&
                        ") passed to pio_inq_dimname() function, pio_inq_dimname_fh_dimid()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_dimname(file, dimid, dname)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimname_fh_dimid")
#endif
  END FUNCTION pio_inq_dimname_fh_dimid

!>
!! @public
!! @ingroup PIO_inq_dimlen
!! @brief Inquire/get the length of a dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dimid The id of the dimension
!! @param[out] dimlen The length (of KIND=PIO_OFFSET_KIND) of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimlen_file_dimid_offlen(file, dimid, dimlen) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: dimid
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: dimlen

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cdimlen = 0

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimlen_file_dimid_offlen")
#endif
    IF(dimid <= 0) THEN
      WRITE(log_msg, *) "Invalid dimension id (dimid = ", dimid,&
                        ") passed to pio_inq_dimlen() function,",&
                        " pio_inq_dimlen_file_dimid_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_dimlen(file%fh, INT(dimid, C_INT) - 1, cdimlen)
    dimlen = INT(cdimlen, PIO_OFFSET_KIND)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimlen_file_dimid_offlen")
#endif
  END FUNCTION pio_inq_dimlen_file_dimid_offlen

!>
!! @public
!! @ingroup PIO_inq_dimlen
!! @brief Inquire/get the length of a dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dimid The id of the dimension
!! @param[out] dimlen The length of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimlen_file_dimid(file, dimid, dimlen) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: dimid
    INTEGER, INTENT(OUT) :: dimlen

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(PIO_OFFSET_KIND) :: off_dimlen = 0

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimlen_file_dimid")
#endif
    IF(dimid <= 0) THEN
      WRITE(log_msg, *) "Invalid dimension id (dimid = ", dimid,&
                        ") passed to pio_inq_dimlen() function,",&
                        " pio_inq_dimlen_file_dimid(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_dimlen(file, dimid, off_dimlen)
    dimlen = INT(off_dimlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimlen_file_dimid")
#endif
  END FUNCTION pio_inq_dimlen_file_dimid

!>
!! @public
!! @ingroup PIO_inq_dimlen
!! @brief Inquire/get the length of a dimension in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dimid The id of the dimension
!! @param[out] dimlen The length of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimlen_fh_dimid(fh, dimid, dimlen) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: dimid
    INTEGER, INTENT(OUT) :: dimlen

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimlen_fh_dimid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (dimid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") or dimension id (dimid = ", dimid,&
                        ") passed to pio_inq_dimlen() function, pio_inq_dimlen_fh_dimid()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_dimlen(file, dimid, dimlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimlen_fh_dimid")
#endif
  END FUNCTION pio_inq_dimlen_fh_dimid

!>
!! @public
!! @ingroup PIO_inq_dimlen
!! @brief Inquire/get the length of a dimension in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dimid The id of the dimension
!! @param[out] dimlen The length (of KIND=PIO_OFFSET_KIND) of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_dimlen_fh_dimid_offlen(fh, dimid, dimlen) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: dimid
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: dimlen

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_dimlen_fh_dimid_offlen")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (dimid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") or dimension id (dimid = ", dimid,&
                        ") passed to pio_inq_dimlen() function, pio_inq_dimlen_fh_dimid_offlen()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_dimlen(file, dimid, dimlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_dimlen_fh_dimid_offlen")
#endif
  END FUNCTION pio_inq_dimlen_fh_dimid_offlen

!>
!! @public
!! @ingroup PIO_inquire_dimension
!! @brief Inquire the meta-data associated with a dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dimid The id of the dimension
!! @param[out] name (Optional) The dimension name
!! @param[out] len (Optional) The length (KIND=PIO_OFFSET_KIND) of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inquire_dimension_file_offlen(file, dimid, name, len) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: dimid
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: name
    INTEGER(PIO_OFFSET_KIND), OPTIONAL, INTENT(OUT) :: len

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_inquire_dimension_file_offlen")
#endif

    IF(dimid <= 0) THEN
      WRITE(log_msg, *) "Invalid dimension id (dimid = ", dimid,&
                        ") passed to pio_inquire_dimension() function,",&
                        " pio_inquire_dimension_file_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIO_NOERR

    IF(PRESENT(name)) THEN
      ierr = pio_inq_dimname(file, dimid, name)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query dimension name (dimid = ", dimid,&
                          ") in pio_inquire_dimension() function,",&
                          " pio_inquire_dimension_file_offlen(). file id = ", file%fh
        ierr = pio_error(file%iosystem, ierr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(len)) THEN
      ierr = pio_inq_dimlen(file, dimid, len)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query dimension length (dimid = ", dimid,&
                          ") in pio_inquire_dimension() function,",&
                          " pio_inquire_dimension_file_offlen(). file id = ", file%fh
        ierr = pio_error(file%iosystem, ierr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inquire_dimension_file_offlen")
#endif
  END FUNCTION pio_inquire_dimension_file_offlen

!>
!! @public
!! @ingroup PIO_inquire_dimension
!! @brief Inquire the meta-data associated with a dimension in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dimid The id of the dimension
!! @param[out] name (Optional) The dimension name
!! @param[out] len (Optional) The length (KIND=PIO_OFFSET_KIND) of the dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inquire_dimension_fh_offlen(fh, dimid, name, len) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: dimid
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: name
    INTEGER(PIO_OFFSET_KIND), OPTIONAL, INTENT(OUT) :: len

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inquire_dimension_fh_offlen")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (dimid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") or dimension id (dimid = ", dimid,&
                        ") passed to pio_inquire_dimension() function, pio_inquire_dimension_fh_offlen()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    !ierr = pio_inquire_dimension(file, dimid, name, len)
    ierr = pio_inquire_dimension_file_offlen(file, dimid, name, len)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inquire_dimension_fh_offlen")
#endif
  END FUNCTION pio_inquire_dimension_fh_offlen

END MODULE spio_inq_dim
