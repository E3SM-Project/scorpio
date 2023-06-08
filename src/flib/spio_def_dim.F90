!> @file
!! @brief SCORPIO APIs for defining dimensions
!! This file contains the SCORPIO APIs for defining variable
!! dimensions
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_def_dim.F90'

MODULE spio_def_dim
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t,&
                        PIO_MAX_NAME, PIO_MAX_VAR_DIMS, PIO_NOERR, PIO_EINTERNAL
  USE pio_kinds, ONLY : i4, PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_util, ONLY : f2cstring, get_text_var_sz, get_var_dim_sz
  USE spio_err, ONLY  : pio_error, pio_warn
  USE spio_def_dim_cint
#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_def_dim

!> @defgroup PIO_def_dim PIO_def_dim
!! @public
!! @brief Define a dimension
!!
!! @details
!! This API can be used to define a variable dimension in a file. The dimensions
!! are used to specify the variable dimensions when defining a variable. The
!! dimensions can only be defined when the file is in the "define" mode
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_def_dim
    !! pio_def_dim_file version of the function accepts a file desc as an arg
    MODULE PROCEDURE pio_def_dim_file
    MODULE PROCEDURE pio_def_dim_file_offlen
    !! pio_def_dim_fh version of the function accepts a file id/handle as an arg 
    MODULE PROCEDURE pio_def_dim_fh
    MODULE PROCEDURE pio_def_dim_fh_offlen
  END INTERFACE

CONTAINS

!>
!! @public
!! @ingroup PIO_def_dim
!! @brief Define a dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dname The name of the dimension
!! @param[in] dlen The length of the dimension
!! @param[out] dimid The id of the dimension is returned in this argument
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_dim_file(file, dname, dlen, dimid) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: dname
    INTEGER, INTENT(IN) :: dlen
    INTEGER, INTENT(OUT) :: dimid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT), PARAMETER :: INVALID_DIMID = -2
    INTEGER(C_INT) :: cdimid = INVALID_DIMID

#ifdef TIMING
    CALL t_startf("PIO:pio_def_dim_file")
#endif
    IF(LEN_TRIM(dname) == 0) THEN
      WRITE(log_msg, *) "Invalid dimension name (empty string) passed to pio_def_dim() function,",&
                        " pio_def_dim_file(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_def_dim(file%fh, TRIM(dname) // C_NULL_CHAR,&
                        INT(dlen, PIO_OFFSET_F2C_TYPE_KIND), cdimid)

    dimid = INT(cdimid) + 1 

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_dim_file")
#endif
  END FUNCTION pio_def_dim_file

!>
!! @public
!! @ingroup PIO_def_dim
!! @brief Define a multidimensional dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dname The name of the dimension
!! @param[in] dlen The length (KIND=PIO_OFFSET_KIND) of the dimension
!! @param[out] dimid The id of the dimension is returned in this argument
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_dim_file_offlen(file, dname, dlen, dimid) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: dname
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: dlen
    INTEGER, INTENT(OUT) :: dimid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT), PARAMETER :: INVALID_DIMID = -2
    INTEGER(C_INT) :: cdimid = INVALID_DIMID

#ifdef TIMING
    CALL t_startf("PIO:pio_def_dim_file_offlen")
#endif
    IF(LEN_TRIM(dname) == 0) THEN
      WRITE(log_msg, *) "Invalid dimension name (empty string) passed to pio_def_dim() function,",&
                        " pio_def_dim_file_offlen(). file id = ", file%fh
      ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      RETURN
    END IF

    ierr = PIOc_def_dim(file%fh, TRIM(dname) // C_NULL_CHAR,&
                        INT(dlen, PIO_OFFSET_F2C_TYPE_KIND), cdimid)

    dimid = INT(cdimid) + 1 

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_dim_file_offlen")
#endif
  END FUNCTION pio_def_dim_file_offlen

!>
!! @public
!! @ingroup PIO_def_dim
!! @brief Define a multidimensional dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dname The name of the dimension
!! @param[in] dlen The length of the dimension
!! @param[out] dimid The id of the dimension is returned in this argument
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_dim_fh(fh, dname, dlen, dimid) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    CHARACTER(LEN=*), INTENT(IN) :: dname
    INTEGER, INTENT(IN) :: dlen
    INTEGER, INTENT(OUT) :: dimid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_def_dim_fh")
#endif

    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    ierr = pio_def_dim(file, dname, dlen, dimid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_dim_fh")
#endif
  END FUNCTION pio_def_dim_fh

!>
!! @public
!! @ingroup PIO_def_dim
!! @brief Define a multidimensional dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[in] dname The name of the dimension
!! @param[in] dlen The length (KIND=PIO_OFFSET_KIND) of the dimension
!! @param[out] dimid The id of the dimension is returned in this argument
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_def_dim_fh_offlen(fh, dname, dlen, dimid) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    CHARACTER(LEN=*), INTENT(IN) :: dname
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: dlen
    INTEGER, INTENT(OUT) :: dimid

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_def_dim_fh_offlen")
#endif

    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    ierr = pio_def_dim(file, dname, dlen, dimid)

#ifdef TIMING
    CALL t_stopf("PIO:pio_def_dim_fh_offlen")
#endif
  END FUNCTION pio_def_dim_fh_offlen

END MODULE spio_def_dim
