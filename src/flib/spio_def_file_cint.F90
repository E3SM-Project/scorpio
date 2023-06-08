!>
!! @file
!! @brief C Interfaces for SCORPIO APIs for entering/exiting
!! "define" mode in an output file

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_def_file_cint'

MODULE spio_def_file_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to enter the define mode
!! in an output file
!!
!! @details
!! @param[in] fh The file id/handle
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_redef(fh)&
                          bind(C,name="PIOc_redef")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
  END FUNCTION PIOc_redef
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to exit/end the define mode
!! in an output file
!!
!! @details
!! @param[in] fh The file id/handle
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_enddef(fh)&
                          bind(C,name="PIOc_enddef")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
  END FUNCTION PIOc_enddef
END INTERFACE

END MODULE spio_def_file_cint
