!>
!! @file
!! @brief C Interfaces for SCORPIO APIs for defining variable
!! dimensions

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_def_dim_cint'

MODULE spio_def_dim_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to define a dimension in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dname The name of the dimension
!! @param[in] dlen The length of the dimension
!! @param[out] dimid The id of the dimension is returned in this argument
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_def_dim(fh, dname, dlen, dimid)&
                          bind(C,name="PIOc_def_dim")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: fh
    CHARACTER(C_CHAR), DIMENSION(*) :: dname
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE :: dlen
    INTEGER(C_INT) :: dimid
  END FUNCTION PIOc_def_dim
END INTERFACE

INTERFACE
!> @private
!! @brief C function to get the id of the I/O system associated with file
!! (used by the Fortran interface)
!!
!! @details
!! @param[in] fh The file handle/id
!! @returns The I/O system id (for the I/O system used to create/open the file) &
!! -1 on error
  INTEGER(C_INT) FUNCTION PIOc_get_iosystem(fh) &
                            bind(C,name="PIOc_get_iosystem")
    USE iso_c_binding
    INTEGER(C_INT), INTENT(IN), VALUE :: fh
  END FUNCTION PIOc_get_iosystem
END INTERFACE

END MODULE spio_def_dim_cint
