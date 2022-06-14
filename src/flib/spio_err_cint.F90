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

INTERFACE
!> @private
!! @brief Fortran interface to C function that handles warning messages
!!
!! @param[in] iosysid The handle to the I/O system with the warning
!! @param[in] fh The file handle for the file with the warning
!! @param[in] fname The name of the source file with the warning
!! @param[in] line The line number in the source file with the warning
!! @param[in] uwarn_msg The user warning message
  SUBROUTINE PIOc_warn(iosysid, fh, fname, line, uwarn_msg)&
                          bind(C,name="PIOc_warn")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: fh
    CHARACTER(C_CHAR), DIMENSION(*) :: fname
    INTEGER(C_INT), VALUE :: line
    CHARACTER(C_CHAR), DIMENSION(*) :: uwarn_msg
  END SUBROUTINE PIOc_warn
END INTERFACE

INTERFACE
!> @private
!! @brief Set the level for debug messages from the I/O library
!!
!! @param[in] lvl The new debug level for debug messages
!! @returns Returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIO_set_log_level(lvl)&
                          bind(C,name="PIO_set_log_level")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: lvl
  END FUNCTION PIO_set_log_level
END INTERFACE

INTERFACE
!> @private
!! @brief Set the error handler for an I/O system
!!
!! @param[in] iosysid The handle to the I/O system
!! @param[in] eh  The new error handler to use for the I/O system
!!                @copydoc PIO_error_method
!! @returns The old error handler used by the I/O system
  INTEGER(C_INT) FUNCTION PIOc_Set_IOSystem_Error_handling(iosysid, eh)&
                          bind(C,name="PIOc_Set_IOSystem_Error_handling")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: eh
  END FUNCTION PIOc_Set_IOSystem_Error_handling
END INTERFACE

END MODULE spio_err_cint
