!> @file
!! @brief SCORPIO APIs for inquiring variables
!! This file contains the SCORPIO APIs for inquiring about
!! variable (id, type, dimensions etc)
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_file.F90'

MODULE spio_inq_file
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t, iosystem_desc_t,&
                        PIO_MAX_NAME, PIO_MAX_VAR_DIMS, PIO_NOERR, PIO_EINTERNAL
  USE pio_kinds, ONLY : i4, PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_util, ONLY : f2cstring, get_text_var_sz, get_var_dim_sz
  USE spio_err, ONLY  : pio_error, pio_warn
  USE spio_inq_file_cint
#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_inq_nvars, pio_inq_ndims, pio_inq_natts, pio_inq_unlimdim,&
            pio_inquire

!! @defgroup pio_inq_nvars pio_inq_nvars
!! @public
!! @brief Inquire the number of variables in a file
!! @details
!!  Get the number of variables in a file
!!
  INTERFACE pio_inq_nvars
    ! This version of the function accepts a file_desc_t as an argument
    MODULE PROCEDURE pio_inq_nvars_file
    ! This version of the function accepts the file id as argument 
    MODULE PROCEDURE pio_inq_nvars_fh
  END INTERFACE

!! @defgroup pio_inq_ndims pio_inq_ndims
!! @public
!! @brief Inquire the number of dimensions in a file
!! @details
!!  Get the number of dimensions in a file
!!
  INTERFACE pio_inq_ndims
    ! This version of the function accepts a file_desc_t as argument
    MODULE PROCEDURE pio_inq_ndims_file
    ! This version of the function accepts the file id as argument 
    MODULE PROCEDURE pio_inq_ndims_fh
  END INTERFACE

!! @defgroup pio_inq_natts pio_inq_natts
!! @public
!! @brief Inquire the number of attributes in a file
!! @details
!!  Get the number of attributes in a file
!!
  INTERFACE pio_inq_natts
    ! This version of the function accepts a file_desc_t as argument
    MODULE PROCEDURE pio_inq_natts_file
    ! This version of the function accepts the file id as argument
    MODULE PROCEDURE pio_inq_natts_fh
  END INTERFACE

!> @defgroup PIO_inq_unlimdim PIO_inq_unlimdim
!! @public
!! @brief Inquire the id of a unlimited dimension
!!
!! @details
!! This API can be used to inquire the unique id associated with the unlimited
!! dimension
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_inq_unlimdim
    !! pio_inq_unlimdim_file version of the function accepts a file desc as
    !! the argument
    MODULE PROCEDURE pio_inq_unlimdim_file
    !! pio_inq_unlimdim_fh version of the function accepts a file id/handle as 
    !! the argument
    MODULE PROCEDURE pio_inq_unlimdim_fh
  END INTERFACE

!! @defgroup pio_inquire pio_inquire
!! @public
!! @brief Inquire the meta-data information from a file
!! @details
!!  Get the meta-data information (the number of variables/dimensions/attributes)
!! from a file. 
!!
  INTERFACE pio_inquire
    ! This version of the function accepts a file_desc_t as argument
    MODULE PROCEDURE pio_inquire_file
    ! This version of the function accepts the file id as argument
    MODULE PROCEDURE pio_inquire_fh
  END INTERFACE

CONTAINS

!>
!! @public
!! @ingroup PIO_inq_nvars
!! @brief Inquire/get the number of variables in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[out] nvars The number of variables in the file
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_nvars_file(file, nvars) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(OUT) :: nvars

    INTEGER(C_INT), PARAMETER :: INVALID_NVARS = -2
    INTEGER(C_INT) :: cnvars = INVALID_NVARS

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_nvars_file")
#endif

    ierr = PIOc_inq_nvars(file%fh, cnvars)
    nvars = INT(cnvars)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_nvars_file")
#endif
  END FUNCTION pio_inq_nvars_file

!>
!! @public
!! @ingroup PIO_inq_nvars
!! @brief Inquire/get the number of variables in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] nvars The number of variables in the file
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_nvars_fh(fh, nvars) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(OUT) :: nvars

    INTEGER(C_INT), PARAMETER :: INVALID_NVARS = -2
    INTEGER(C_INT) :: cnvars = INVALID_NVARS

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_nvars_fh")
#endif

    ierr = PIOc_inq_nvars(fh, cnvars)
    nvars = INT(cnvars)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_nvars_fh")
#endif
  END FUNCTION pio_inq_nvars_fh

!>
!! @public
!! @ingroup PIO_inq_ndims
!! @brief Inquire/get the number of dimensions in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[out] ndims The number of dimensions in the file
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_ndims_file(file, ndims) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(OUT) :: ndims

    INTEGER(C_INT), PARAMETER :: INVALID_NVARS = -2
    INTEGER(C_INT) :: cndims = INVALID_NVARS

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_ndims_file")
#endif

    ierr = PIOc_inq_ndims(file%fh, cndims)
    ndims = INT(cndims)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_ndims_file")
#endif
  END FUNCTION pio_inq_ndims_file

!>
!! @public
!! @ingroup PIO_inq_ndims
!! @brief Inquire/get the number of dimensions in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] ndims The number of dimensions in the file
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_ndims_fh(fh, ndims) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(OUT) :: ndims

    INTEGER(C_INT), PARAMETER :: INVALID_NVARS = -2
    INTEGER(C_INT) :: cndims = INVALID_NVARS

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_ndims_fh")
#endif

    ierr = PIOc_inq_ndims(fh, cndims)
    ndims = INT(cndims)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_ndims_fh")
#endif
  END FUNCTION pio_inq_ndims_fh

!>
!! @public
!! @ingroup PIO_inq_natts
!! @brief Inquire/get the number of attributes in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[out] natts The number of attributes in the file
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_natts_file(file, natts) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(OUT) :: natts

    INTEGER(C_INT), PARAMETER :: INVALID_NVARS = -2
    INTEGER(C_INT) :: cnatts = INVALID_NVARS

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_natts_file")
#endif

    ierr = PIOc_inq_natts(file%fh, cnatts)
    natts = INT(cnatts)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_natts_file")
#endif
  END FUNCTION pio_inq_natts_file

!>
!! @public
!! @ingroup PIO_inq_natts
!! @brief Inquire/get the number of attributes in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] natts The number of attributes in the file
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_natts_fh(fh, natts) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(OUT) :: natts

    INTEGER(C_INT), PARAMETER :: INVALID_NVARS = -2
    INTEGER(C_INT) :: cnatts = INVALID_NVARS

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_natts_fh")
#endif

    ierr = PIOc_inq_natts(fh, cnatts)
    natts = INT(cnatts)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_natts_fh")
#endif
  END FUNCTION pio_inq_natts_fh

!>
!! @public
!! @ingroup PIO_inq_unlimdim
!! @brief Get the id of the unlimited dimension in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[out] unlimdim The id of the unlimited dimension is returned in this argument
!!                      If there is no unlimited dimension in the file a -1 is returned
!!                      in this argument.
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_unlimdim_file(file, unlimdim) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(OUT) :: unlimdim

    INTEGER(C_INT) :: cunlimdim = -1

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_unlimdim_file")
#endif

    ierr = PIOc_inq_unlimdim(file%fh, cunlimdim)
    IF(cunlimdim >= 0) THEN
      ! FIXME: Do we need to use Fortran-style, starts from 1, of dimension ids?
      unlimdim = INT(cunlimdim) + 1
    ELSE
      unlimdim = -1
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_unlimdim_file")
#endif
  END FUNCTION pio_inq_unlimdim_file

!>
!! @public
!! @ingroup PIO_inq_unlimdim
!! @brief Get the id of the unlimited dimension in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] unlimdim The id of the unlimited dimension is returned in this argument
!!                      If there is no unlimited dimension in the file a -1 is returned
!!                      in this argument.
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inq_unlimdim_fh(fh, unlimdim) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, INTENT(OUT) :: unlimdim

    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inq_unlimdim_fh")
#endif

    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys
    
    ierr = pio_inq_unlimdim(file, unlimdim)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inq_unlimdim_fh")
#endif
  END FUNCTION pio_inq_unlimdim_fh

!>
!! @public
!! @ingroup PIO_inquire
!! @brief Get the number of variables/dimensions/attributes in a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @param[out] nDimensions (Optional) The number of dimensions in the file
!! @param[out] nVariables (Optional) The number of variables in the file
!! @param[out] nAttributes (Optional) The number of attributes in the file
!! @param[out] unlimitedDimID (Optional) The id of the unlimited dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inquire_file(file, nDimensions, nVariables, nAttributes,&
                                    unlimitedDimID) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, OPTIONAL, INTENT(OUT) :: nDimensions
    INTEGER, OPTIONAL, INTENT(OUT) :: nVariables
    INTEGER, OPTIONAL, INTENT(OUT) :: nAttributes
    INTEGER, OPTIONAL, INTENT(OUT) :: unlimitedDimID

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
#ifdef TIMING
    CALL t_startf("PIO:pio_inquire_file")
#endif

    ierr = PIO_NOERR

    IF(PRESENT(nDimensions)) THEN
      ierr = pio_inq_ndims(file, nDimensions)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query the number of dimensions in file,",&
                          "in function, pio_inquire_file(), file%fh = ", file%fh
        ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(nVariables)) THEN
      ierr = pio_inq_nvars(file, nVariables)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query the number of variables in file,",&
                          "in function, pio_inquire_file(), file%fh = ", file%fh
        ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(nAttributes)) THEN
      ierr = pio_inq_natts(file, nAttributes)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query the number of attributes in file,",&
                          "in function, pio_inquire_file(), file%fh = ", file%fh
        ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

    IF(PRESENT(unlimitedDimID)) THEN
      ierr = pio_inq_unlimdim(file, unlimitedDimID)
      IF(ierr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to query the unlimited dimension in file,",&
                          "in function, pio_inquire_file(), file%fh = ", file%fh
        ierr = pio_error(file%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        RETURN
      END IF
    END IF

#ifdef TIMING
    CALL t_stopf("PIO:pio_inquire_file")
#endif
  END FUNCTION pio_inquire_file

!>
!! @public
!! @ingroup PIO_inquire
!! @brief Get the number of variables/dimensions/attributes in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] nDimensions (Optional) The number of dimensions in the file
!! @param[out] nVariables (Optional) The number of variables in the file
!! @param[out] nAttributes (Optional) The number of attributes in the file
!! @param[out] unlimitedDimID (Optional) The id of the unlimited dimension
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_inquire_fh(fh, nDimensions, nVariables, nAttributes,&
                                    unlimitedDimID) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh
    INTEGER, OPTIONAL, INTENT(OUT) :: nDimensions
    INTEGER, OPTIONAL, INTENT(OUT) :: nVariables
    INTEGER, OPTIONAL, INTENT(OUT) :: nAttributes
    INTEGER, OPTIONAL, INTENT(OUT) :: unlimitedDimID

    TYPE(file_desc_t) :: file
    TYPE(iosystem_desc_t), TARGET :: iosys

#ifdef TIMING
    CALL t_startf("PIO:pio_inquire_fh")
#endif

    iosys%iosysid = PIOc_get_iosystem(INT(fh, C_INT))
    file%fh = fh
    file%iosystem => iosys

    ierr = pio_inquire_file(file, nDimensions, nVariables, nAttributes, unlimitedDimID)

#ifdef TIMING
    CALL t_stopf("PIO:pio_inquire_fh")
#endif
  END FUNCTION pio_inquire_fh

END MODULE spio_inq_file
