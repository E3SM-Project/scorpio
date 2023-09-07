!> @file
!! @brief SCORPIO APIs for inquiring attributes
!! This file contains the SCORPIO APIs for inquiring about
!! attribute (id, type, dimensions etc)
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_att.F90'

MODULE spio_inq_att
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t,&
                        PIO_MAX_NAME, PIO_MAX_VAR_DIMS, PIO_NOERR, PIO_EINTERNAL
  USE pio_kinds, ONLY : i4, PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_util, ONLY : f2cstring, c2fstring, get_text_var_sz, get_var_dim_sz
  USE spio_err, ONLY  : pio_error, pio_warn
  USE spio_inq_att_cint
#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_inq_attid, pio_inq_attname, pio_inq_atttype, pio_inq_attlen,&
            pio_inq_att

!> @defgroup PIO_inq_attid PIO_inq_attid
!! @public
!! @brief Inquire attribute id
!!
!! @details
!! This API can be used to inquire the unique id associated with a attribute in
!! a file. 
!!
!! The attributes can be associated with a variable or not (a global attribute).
!! Attributes associated with a variable needs to be queried using the variable
!! id and global attributes can be queried using PIO_GLOBAL as the variable id
!! (indicating that the attribute is a global attribute that is not associated
!! with a single variable)
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_attid
    !! pio_inq_attid_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_attid_file_vid
    !! pio_inq_attid_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_attid_fh_vid
    !! pio_inq_attid_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_attid_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_attname PIO_inq_attname
!! @public
!! @brief Inquire attribute name
!!
!! @details
!! This API can be used to inquire the name of a attribute in a file.
!!
!! The attributes can be associated with a variable or not (a global attribute).
!! Attributes associated with a variable needs to be queried using the variable
!! id and global attributes can be queried using PIO_GLOBAL as the variable id
!! (indicating that the attribute is a global attribute that is not associated
!! with a single variable)
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_attname
    !! pio_inq_attname_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_attname_file_vid
    !! pio_inq_attname_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_attname_fh_vid
    !! pio_inq_attname_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_attname_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_atttype PIO_inq_atttype
!! @public
!! @brief Inquire the type of an attribute
!!
!! @details
!! This API can be used to inquire the type of an attribute in a file
!!
!! The attributes can be associated with a variable or not (a global attribute).
!! Attributes associated with a variable needs to be queried using the variable
!! id and global attributes can be queried using PIO_GLOBAL as the variable id
!! (indicating that the attribute is a global attribute that is not associated
!! with a single variable)
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_atttype
    !! pio_inq_atttype_file_vid version of the function accepts a file desc &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_atttype_file_vid
    !! pio_inq_atttype_fh_vid version of the function accepts a file id/handle &
    !! a variable id/handle as arguments
    MODULE PROCEDURE pio_inq_atttype_fh_vid
    !! pio_inq_atttype_file_vdesc version of the function accepts a file desc &
    !! a variable desc as arguments
    MODULE PROCEDURE pio_inq_atttype_file_vdesc
  END INTERFACE

!> @defgroup PIO_inq_attlen PIO_inq_attlen
!! @public
!! @brief Inquire the length/size of an attribute
!!
!! @details
!! This API can be used to inquire the length of an attribute
!!
!! The attributes can be associated with a variable or not (a global attribute).
!! Attributes associated with a variable needs to be queried using the variable
!! id and global attributes can be queried using PIO_GLOBAL as the variable id
!! (indicating that the attribute is a global attribute that is not associated
!! with a single variable)
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_attlen
    !! Note: Since Fortran does not support functional polymorphism based on 
    !! KIND of the variable, we cannot have support for regular INTEGERs
    !! (we only support PIO_OFFSET_KINDs) for attribute lengths

    !! pio_inq_attlen_file_varid version of the function accepts a file desc,
    !! a variable id/handle, attribute id  & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_attlen_file_varid_attid_offlen
    !! pio_inq_attlen_file_varid version of the function accepts a file desc,
    !! a variable id, attribute name  & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_attlen_file_varid_attname_offlen
    !! pio_inq_attlen_fh_varid version of the function accepts a file id/handle,
    !! a variable id, attribute id & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_attlen_fh_varid_attid_offlen
    !! pio_inq_attlen_fh_varid version of the function accepts a file id/handle,
    !! a variable id, attribute name & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_attlen_fh_varid_attname_offlen
    !! pio_inq_attlen_file_vdesc version of the function accepts a file id/handle,
    !! a variable desc, attribute id & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_attlen_file_vdesc_attid_offlen
    !! pio_inq_attlen_file_vdesc version of the function accepts a file id/handle,
    !! a variable desc, attribute name & a long (PIO_OFFSET_KIND) length as arguments 
    MODULE PROCEDURE pio_inq_attlen_file_vdesc_attname_offlen
  END INTERFACE

!> @defgroup PIO_inq_att PIO_inq_att
!! @public
!! @brief Inquire meta-data associated with an attribute
!!
!! @details
!! This API can be used to inquire the meta-data associated with an
!! attribute in a file. The user can query the name, type, number of dimensions &
!! attributes and the dimension ids of an attribute.
!!
!! The attributes can be associated with a variable or not (a global attribute).
!! Attributes associated with a variable needs to be queried using the variable
!! id and global attributes can be queried using PIO_GLOBAL as the variable id
!! (indicating that the attribute is a global attribute that is not associated
!! with a single variable)
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_att
    !! Note: Since Fortran does not support functional polymorphism based on 
    !! KIND of the variable, we cannot have support for regular INTEGERs
    !! (we only support PIO_OFFSET_KINDs) for attribute lengths

    !! pio_inq_att_file_vid_offlen version of the function accepts a file desc,
    !! a variable id/handle & attribute length (KIND=PIO_OFFSET_KIND) as arguments
    MODULE PROCEDURE pio_inq_att_file_vid_offlen
    !! pio_inq_att_fh_vid_offlen version of the function accepts a file id/handle,
    !! a variable id/handle & attribute length (KIND=PIO_OFFSET_KIND) as arguments
    MODULE PROCEDURE pio_inq_att_fh_vid_offlen
    !! pio_inq_att_file_vdesc_offlen version of the function accepts a file desc,
    !! a variable desc & attribute length (KIND=PIO_OFFSET_KIND) as arguments
    MODULE PROCEDURE pio_inq_att_file_vdesc_offlen
  END INTERFACE

CONTAINS

!>
!! @public
!! @ingroup PIO_inq_attid
!! @brief Inquire/get the unique id associated with an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable associated with the attribute. Use PIO_GLOBAL
!!                  for global attributes
!! @param[in] attname  The name of the attribute
!! @param[out] attid The id of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attid_file_vid(file, varid, attname, attid) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER, INTENT(OUT) :: attid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    ! Since we always increment attid in Fortran after receiving it from C
    ! ensure that its invalid even after the increment
    INTEGER(C_INT), PARAMETER :: INVALID_ATTID = -2
    INTEGER(C_INT) :: cattid = INVALID_ATTID

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attid_file_vid")
#endif
    IF(varid < 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid, ")",&
                        " passed to pio_inq_attid() function,",&
                        " pio_inq_attid_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF
    IF(LEN_TRIM(attname) == 0) THEN
      WRITE(log_msg, *) "Invalid attribute name (empty string) passed to pio_inq_attid() function,",&
                        " pio_inq_attid_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_attid(file%fh, INT(varid, C_INT) - 1, TRIM(attname) // C_NULL_CHAR, cattid)
    ! FIXME: Do we need to follow the convention of Fortran ids starting from 1?
    attid = INT(cattid) + 1

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attid_file_vid")
#endif
  END FUNCTION pio_inq_attid_file_vid

!>
!! @public
!! @ingroup PIO_inq_attid
!! @brief Inquire/get the unique id associated with an attribute in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable associated with the attribute. Use PIO_GLOBAL
!!                  for global attributes
!! @param[in] attname  The name of the attribute
!! @param[out] attid The id of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attid_fh_vid(fh, varid, attname, attid) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER, INTENT(OUT) :: attid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attid_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF(varid < 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid, ")",&
                        " passed to pio_inq_attid() function,",&
                        " pio_inq_attid_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF
    IF(LEN_TRIM(attname) == 0) THEN
      WRITE(log_msg, *) "Invalid attribute name (empty string) passed to pio_inq_attid() function,",&
                        " pio_inq_attid_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attid(file, varid, attname, attid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attid_fh_vid")
#endif
  END FUNCTION pio_inq_attid_fh_vid

!>
!! @public
!! @ingroup PIO_inq_attid
!! @brief Inquire/get the unique id associated with a attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable with the attribute. @copydoc var_desc_t
!! @param[in] attname  The name of the attribute
!! @param[out] attid The id of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attid_file_vdesc(file, vdesc, attname, attid) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(OUT) :: vdesc
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER, INTENT(OUT) :: attid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attid_file_vdesc")
#endif
    IF(LEN_TRIM(attname) == 0) THEN
      WRITE(log_msg, *) "Invalid attribute name (empty string) passed to pio_inq_attid() function,",&
                        " pio_inq_attid_file_vdesc(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attid(file, INT(vdesc%varid), attname, attid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attid_file_vdesc")
#endif
  END FUNCTION pio_inq_attid_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_attname
!! @brief Inquire/get the name of an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable associated with the attribute. Use PIO_GLOBAL
!!                  for global attributes
!! @param[in] attid The id of the attribute
!! @param[out] attname  The name of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attname_file_vid(file, varid, attid, attname) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: attid
    CHARACTER(LEN=*), INTENT(OUT) :: attname

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    CHARACTER(C_CHAR) :: cattname(PIO_MAX_NAME)

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attname_file_vid")
#endif
    IF(varid < 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_attname() function,",&
                        " pio_inq_attname_file_vid(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    IF(LEN(attname) < PIO_MAX_NAME) THEN
      WRITE(log_msg, *) "Insufficient user buffer size (User buffer size :",&
                        LEN(attname), " chars, expected PIO_MAX_NAME=",&
                        PIO_MAX_NAME, " chars.) The attribute name maybe truncated"
      CALL pio_warn(file%iosystem, __PIO_FILE__, __LINE__, TRIM(log_msg))
    END IF

    cattname = C_NULL_CHAR
    ierr = PIOc_inq_attname(file%fh, INT(varid, C_INT) - 1, INT(attid, C_INT) - 1, cattname)
    IF(ierr == PIO_NOERR) THEN
      ierr = c2fstring(file%iosystem, cattname, PIO_MAX_NAME, PIO_MAX_NAME, attname)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to convert attribute name from C string to a Fortran string",&
                          "(attid = ", attid,") in pio_inq_attname() function,",&
                          " pio_inq_attname_file_vid(). varid = ", varid, ", file id = ", file%fh
        ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attname_file_vid")
#endif
  END FUNCTION pio_inq_attname_file_vid

!>
!! @public
!! @ingroup PIO_inq_attname
!! @brief Inquire/get the name of a attribute in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable associated with the attribute. Use PIO_GLOBAL
!!                  for global attributes
!! @param[in] attid The id of the attribute
!! @param[out] attname  The name of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attname_fh_vid(fh, varid, attid, attname) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: attid
    CHARACTER(LEN=*), INTENT(OUT) :: attname

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attname_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (varid < 0) .OR. (attid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") ",&
                        "or variable id (varid = ", varid,&
                        "or attribute id (attid = ", attid,&
                        ") passed to pio_inq_attname() function, pio_inq_attname_fh_vid()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attname(file, varid, attid, attname)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attname_fh_vid")
#endif
  END FUNCTION pio_inq_attname_fh_vid

!>
!! @public
!! @ingroup PIO_inq_attname
!! @brief Inquire/get the name of an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the variable. @copydoc var_desc_t
!! @param[in] attid The id of the attribute
!! @param[out] attname  The name of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attname_file_vdesc(file, vdesc, attid, attname) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(IN) :: attid
    CHARACTER(LEN=*), INTENT(OUT) :: attname

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attname_file_vdesc")
#endif

    ierr = pio_inq_attname(file, INT(vdesc%varid), attid, attname)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attname_file_vdesc")
#endif
  END FUNCTION pio_inq_attname_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_atttype
!! @brief Inquire/get the type of an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable associated with the attribute. Use
!!                  PIO_GLOBAL as the variable id for global attributes
!! @param[in] attid The id of the attribute
!! @param[out] atttype The type of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_atttype_file_vid(file, varid, attid, atttype) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: attid
    INTEGER, INTENT(OUT) :: atttype

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    CHARACTER(C_CHAR) :: cattname(PIO_MAX_NAME)
    INTEGER(C_INT) :: catttype

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_atttype_file_vid")
#endif
    IF((varid < 0) .OR. (attid <= 0)) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,")",&
                        " or invalid attribute id (attid = ", attid, ")",&
                        " passed to pio_inq_atttype() function,",&
                        " pio_inq_atttype_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_attname(file%fh, INT(varid, C_INT) - 1, INT(attid, C_INT) - 1, cattname)
    IF(ierr /= PIO_NOERR) THEN
      WRITE(log_msg, *) " Unable to query the attribute name, ",&
                        "(varid = ", varid, ", attid = ", attid, ")",&
                        " in pio_inq_atttype() function,",&
                        " pio_inq_atttype_file_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_atttype(file%fh, INT(varid, C_INT) - 1, cattname, catttype)
    atttype = INT(catttype)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_atttype_file_vid")
#endif
  END FUNCTION pio_inq_atttype_file_vid

!>
!! @public
!! @ingroup PIO_inq_atttype
!! @brief Inquire/get the type of an attribute in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable associated with the attribute. Use
!!                  PIO_GLOBAL as the variable id for global attributes
!! @param[in] attid The id of the attribute
!! @param[out] atttype The type of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_atttype_fh_vid(fh, varid, attid, atttype) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: attid
    INTEGER, INTENT(OUT) :: atttype

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_atttype_fh_vid")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((varid < 0) .OR. (attid <= 0)) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,")",&
                        " or invalid attribute id (attid = ", attid, ")",&
                        " passed to pio_inq_atttype() function,",&
                        " pio_inq_atttype_fh_vid. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_atttype(file, varid, attid, atttype)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_atttype_fh_vid")
#endif
  END FUNCTION pio_inq_atttype_fh_vid

!>
!! @public
!! @ingroup PIO_inq_atttype
!! @brief Inquire/get the type of an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the attribute. @copydoc var_desc_t
!! @param[in] attid The id of the attribute
!! @param[out] atttype The type of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_atttype_file_vdesc(file, vdesc, attid, atttype) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(IN) :: attid
    INTEGER, INTENT(OUT) :: atttype

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
#ifdef TIMING
    CALL t_startf("PIO:pio_inq_atttype_file_vdesc")
#endif
    IF(attid <= 0) THEN
      WRITE(log_msg, *) "Invalid attribute id (attid = ", attid, ")",&
                        " passed to pio_inq_atttype() function,",&
                        " pio_inq_atttype_file_vdesc. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_atttype(file, INT(vdesc%varid), attid, atttype)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_atttype_file_vdesc")
#endif
  END FUNCTION pio_inq_atttype_file_vdesc

!>
!! @public
!! @ingroup PIO_inq_attlen
!! @brief Inquire/get the length of an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable associated with the attribute.
!!                  Use PIO_GLOBAL for global attributes (attributes not associated
!!                  with any variable)
!! @param[in] attid The id of the attribute
!! @param[out] attlen The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attlen_file_varid_attid_offlen(file, varid, attid, attlen) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: attid
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: attlen

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    CHARACTER(C_CHAR) :: cattname(PIO_MAX_NAME)
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: cattlen = 0

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attlen_file_varid_attid_offlen")
#endif
    IF((varid < 0) .OR. (attid <= 0)) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid, ")",&
                        " or an invalid attribute id (attid = ", attid, ")",&
                        " passed to pio_inq_attlen() function,",&
                        " pio_inq_attlen_file_varid_attid_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_attname(file%fh, INT(varid, C_INT) - 1, INT(attid, C_INT) - 1, cattname)
    IF(ierr /= PIO_NOERR) THEN
      WRITE(log_msg, *) " Unable to query the attribute name, ",&
                        "(varid = ", varid, ", attid = ", attid, ")",&
                        " in pio_inq_attlen() function,",&
                        " pio_inq_attlen_file_varid_attid_offlen. file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_attlen(file%fh, INT(varid, C_INT) - 1, cattname, cattlen)
    attlen = INT(cattlen, PIO_OFFSET_KIND)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attlen_file_varid_attid_offlen")
#endif
  END FUNCTION pio_inq_attlen_file_varid_attid_offlen

!>
!! @public
!! @ingroup PIO_inq_attlen
!! @brief Inquire/get the length of an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable associated with the attribute.
!!                  Use PIO_GLOBAL for global attributes (attributes not associated
!!                  with any variable)
!! @param[in] attname The name of the attribute
!! @param[out] attlen The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attlen_file_varid_attname_offlen(file, varid, attname, attlen) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: attlen

    INTEGER :: attid
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attlen_file_varid_attname_offlen")
#endif
    IF((varid < 0) .OR. (LEN_TRIM(attname) == 0)) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid, ")",&
                        " or an invalid attribute name (empty string)",&
                        " passed to pio_inq_attlen() function,",&
                        " pio_inq_attlen_file_varid_attname_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attid(file, varid, attname, attid)
    IF(ierr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Unable to query the attribute id for attribute ",&
                        " (attribute name = ", TRIM(attname), ")",&
                        " in pio_inq_attlen() function,",&
                        " pio_inq_attlen_file_varid_attname_offlen().",&
                        " file id = ", file%fh, ", varid = ", varid
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attlen(file, varid, attid, attlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attlen_file_varid_attname_offlen")
#endif
  END FUNCTION pio_inq_attlen_file_varid_attname_offlen

!>
!! @public
!! @ingroup PIO_inq_attlen
!! @brief Inquire/get the length of an attribute in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable associated with the attribute.
!!                  Use PIO_GLOBAL for global attributes (attributes not associated
!!                  with any variable)
!! @param[in] attid The id of the attribute
!! @param[out] attlen The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attlen_fh_varid_attid_offlen(fh, varid, attid, attlen) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(IN) :: attid
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: attlen

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attlen_fh_varid_attid_offlen")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (varid < 0) .OR. (attid <= 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ")",&
                        "or variable id (varid = ", varid,")",&
                        "or attribute id (attid = ", attid,")",&
                        " passed to pio_inq_attlen() function, pio_inq_attlen_fh_varid_attid_offlen()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attlen(file, varid, attid, attlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attlen_fh_varid_attid_offlen")
#endif
  END FUNCTION pio_inq_attlen_fh_varid_attid_offlen

!>
!! @public
!! @ingroup PIO_inq_attlen
!! @brief Inquire/get the length of an attribute in a file
!!
!! @details
!! @param[in] fh The file handle/id
!! @param[in] varid The id of the variable associated with the attribute.
!!                  Use PIO_GLOBAL for global attributes (attributes not associated
!!                  with any variable)
!! @param[in] attname The name of the attribute
!! @param[out] attlen The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attlen_fh_varid_attname_offlen(fh, varid, attname, attlen) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: attlen

    INTEGER :: attid
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attlen_fh_varid_attname_offlen")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys

    IF((varid < 0) .OR. (LEN_TRIM(attname) == 0)) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid, ")",&
                        " or an invalid attribute name (empty string)",&
                        " passed to pio_inq_attlen() function,",&
                        " pio_inq_attlen_fh_varid_attname_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attid(file, varid, attname, attid)
    IF(ierr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Unable to query the attribute id for attribute ",&
                        " (attribute name = ", TRIM(attname), ")",&
                        " in pio_inq_attlen() function,",&
                        " pio_inq_attlen_fh_varid_attname_offlen().",&
                        " file id = ", file%fh, ", varid = ", varid
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attlen(file, varid, attid, attlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attlen_fh_varid_attname_offlen")
#endif
  END FUNCTION pio_inq_attlen_fh_varid_attname_offlen

!>
!! @public
!! @ingroup PIO_inq_attlen
!! @brief Inquire/get the length of an attribute in a file
!!
!! @details
!! @param[in] file The file id/handle. @copydoc file_desc_t
!! @param[in] vdesc The handle to the variable. @copydoc var_desc_t
!! @param[in] attid The id of the attribute
!! @param[out] attlen The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attlen_file_vdesc_attid_offlen(file, vdesc, attid, attlen) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, INTENT(IN) :: attid
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: attlen

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attlen_file_vdesc_attid_offlen")
#endif
    
    ierr = pio_inq_attlen(file, INT(vdesc%varid), attid, attlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attlen_file_vdesc_attid_offlen")
#endif
  END FUNCTION pio_inq_attlen_file_vdesc_attid_offlen

!>
!! @public
!! @ingroup PIO_inq_attlen
!! @brief Inquire/get the length of an attribute in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The handle to the variable. @copydoc var_desc_t
!! @param[in] attname The name of the attribute
!! @param[out] attlen The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_attlen_file_vdesc_attname_offlen(file, vdesc, attname, attlen) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: attlen

    INTEGER :: attid
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_attlen_file_vdesc_attname_offlen")
#endif
    IF(LEN_TRIM(attname) == 0) THEN
      WRITE(log_msg, *) "Invalid attribute name (empty string)",&
                        " passed to pio_inq_attlen() function,",&
                        " pio_inq_attlen_file_vdesc_attname_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attid(file, INT(vdesc%varid), attname, attid)
    IF(ierr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Unable to query the attribute id for attribute ",&
                        " (attribute name = ", TRIM(attname), ")",&
                        " in pio_inq_attlen() function,",&
                        " pio_inq_attlen_file_vdesc_attname_offlen().",&
                        " file id = ", file%fh, ", varid = ", vdesc%varid
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_attlen(file, INT(vdesc%varid), attid, attlen)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_attlen_file_vdesc_attname_offlen")
#endif
  END FUNCTION pio_inq_attlen_file_vdesc_attname_offlen

!>
!! @public
!! @ingroup PIO_inq_att
!! @brief Get the meta-data associated with a attribute
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] varid The id of the variable with the attribute. Use PIO_GLOBAL
!!                  as the variable id for global attributes
!! @param[in] attname The name of the attribute
!! @param[out] xtype (Optional) The type of the attribute
!! @param[out] len (Optional) The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_att_file_vid_offlen(file, varid, attname, xtype, len) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER, OPTIONAL, INTENT(OUT) :: xtype
    INTEGER(PIO_OFFSET_KIND), OPTIONAL, INTENT(OUT) :: len

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cxtype
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: clen

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_att_file_vid_offlen")
#endif
    IF(varid < 0) THEN
      WRITE(log_msg, *) "Invalid variable id (varid = ", varid,&
                        ") passed to pio_inq_att() function,",&
                        " pio_inq_att_file_vid_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    IF(LEN_TRIM(attname) == 0) THEN
      WRITE(log_msg, *) "Invalid attribute name (empty string",&
                        " passed to pio_inq_att() function,",&
                        " pio_inq_att_file_vid_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_inq_att(file%fh, INT(varid, C_INT) - 1, TRIM(attname) // C_NULL_CHAR,&
                        cxtype, clen)

    IF(PRESENT(xtype)) THEN
      xtype = INT(cxtype)
    END IF

    IF(PRESENT(len)) THEN
      len = INT(clen, PIO_OFFSET_KIND)
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_att_file_vid_offlen")
#endif
  END FUNCTION pio_inq_att_file_vid_offlen

!>
!! @public
!! @ingroup PIO_inq_att
!! @brief Get the meta-data associated with an attribute
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable associated with the attribute. Use PIO_GLOBAL
!!                  as the variable id for global attributes
!! @param[in] attname The name of the attribute
!! @param[out] xtype (Optional) The type of the attribute
!! @param[out] len (Optional) The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_att_fh_vid_offlen(fh, varid, attname, xtype, len) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: varid
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER, OPTIONAL, INTENT(OUT) :: xtype
    INTEGER(PIO_OFFSET_KIND), OPTIONAL, INTENT(OUT) :: len

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_att_fh_vid_offlen")
#endif
    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    IF((fh <= 0) .OR. (varid < 0)) THEN
      WRITE(log_msg, *) "Invalid file handle/id (fh =", fh, ") or variable id (varid = ", varid,&
                        ") passed to pio_inq_att() function, pio_inq_att_fh_vid()"
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    IF(LEN_TRIM(attname) == 0) THEN
      WRITE(log_msg, *) "Invalid attribute name (empty string",&
                        " passed to pio_inq_att() function,",&
                        " pio_inq_att_fh_vid_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = pio_inq_att(file, varid, attname, xtype, len)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_att_fh_vid_offlen")
#endif
  END FUNCTION pio_inq_att_fh_vid_offlen

!>
!! @public
!! @ingroup PIO_inq_att
!! @brief Get the meta-data associated with a attribute
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] vdesc The descriptor of the attribute. @copydoc var_desc_t
!! @param[in] attname The name of the attribute
!! @param[out] xtype (Optional) The type of the attribute
!! @param[out] len (Optional) The length (KIND=PIO_OFFSET_KIND) of the attribute
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_att_file_vdesc_offlen(file, vdesc, attname, xtype, len) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    CHARACTER(LEN=*), INTENT(IN) :: attname
    INTEGER, OPTIONAL, INTENT(OUT) :: xtype
    INTEGER(PIO_OFFSET_KIND), OPTIONAL, INTENT(OUT) :: len

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_att_file_vdesc_offlen")
#endif

    ierr = pio_inq_att(file, INT(vdesc%varid), attname, xtype, len)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_att_file_vdesc_offlen")
#endif
  END FUNCTION pio_inq_att_file_vdesc_offlen

END MODULE spio_inq_att
