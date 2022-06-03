!> @file
!! @brief APIs for handling errors
!! This file contains the APIs for handling errors in the Fortran
!! interface

!> @internal
!! @def __PIO_FILE__
!! @brief This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_err.F90'

MODULE spio_err
  USE pio_types, ONLY : iosystem_desc_t, file_desc_t,&
                        PIO_IOSYSID_INVALID, PIO_FH_INVALID
  USE spio_err_cint
#ifdef TIMING
  use perf_mod, only : t_startf, t_stopf   !_EXTERNAL
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_error

!> @defgroup PIO_error PIO_error
!! @brief Handles error codes in the Fortran interface
!!
  INTERFACE pio_error
    MODULE PROCEDURE pio_iosys_error
    MODULE PROCEDURE pio_file_error
  END INTERFACE

CONTAINS

!> @ingroup PIO_error
!> @private
!! @brief Function that handles error codes in the fortran interface
!!
!! @param[in] iosys The I/O system associated with the error
!! @param[in] err_num The error number/code
!! @param[in] fname The name of the source file with the error
!! @param[in] line The line number in the source file with the error
!! @param[in] uerr_msg The user error message
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise (The function can also abort on error)
  FUNCTION pio_iosys_error (iosys, err_num, fname, line, uerr_msg) RESULT(ierr)
    USE iso_c_binding

    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    INTEGER, INTENT(IN) :: err_num
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uerr_msg

    INTEGER :: ierr
    INTEGER(C_INT) :: cerr

    ! FIXME: Do fortran strings (trim(fname)) need to be converted to
    ! C_CHAR arrays here?
    cerr = PIOc_error(iosys%iosysid, PIO_FH_INVALID, INT(err_num, C_INT),&
                      trim(fname) // C_NULL_CHAR, line,&
                      trim(uerr_msg) // C_NULL_CHAR)
    ierr = INT(cerr)
  END FUNCTION pio_iosys_error

!> @ingroup PIO_error
!> @private
!! @brief Function that handles error codes in the fortran interface
!!
!! @param[in] file The file associated with the error
!! @param[in] err_num The error number/code
!! @param[in] fname The name of the source file with the error
!! @param[in] line The line number in the source file with the error
!! @param[in] uerr_msg The user error message
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise (The function can also abort on error)
  FUNCTION pio_file_error (file, err_num, fname, line, uerr_msg) RESULT(ierr)
    USE iso_c_binding

    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: err_num
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uerr_msg

    INTEGER :: ierr
    INTEGER(C_INT) :: cerr

    cerr = PIOc_error(PIO_IOSYSID_INVALID, file%fh, INT(err_num, C_INT),&
                      trim(fname) // C_NULL_CHAR, line,&
                      trim(uerr_msg) // C_NULL_CHAR)
    ierr = INT(cerr)
  END FUNCTION pio_file_error
END MODULE spio_err
