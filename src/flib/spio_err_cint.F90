!>
!! @file
!! @brief C Interfaces for APIs that handle error codes
!!

MODULE spio_err_cint
  USE iso_c_binding
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Fortran interface to C function that handles error codes
!!
!! @param[in] iosysid The handle to the I/O system with the error
!! @param[in] fh The file handle for the file with the error
!! @param[in] err_num The error number/code
!! @param[in] fname The name of the source file with the error
!! @param[in] line The line number in the source file with the error
!! @param[in] uerr_msg The user error message
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise (The function can also abort on error)
  INTEGER(C_INT) FUNCTION PIOc_error(iosysid, fh,&
                                     err_num, fname, line,&
                                     uerr_msg)&
                          bind(C,name="PIOc_error")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: err_num
    CHARACTER(C_CHAR), DIMENSION(*) :: fname
    INTEGER(C_INT), VALUE :: line
    CHARACTER(C_CHAR), DIMENSION(*) :: uerr_msg
  END FUNCTION PIOc_error
END INTERFACE

END MODULE spio_err_cint
