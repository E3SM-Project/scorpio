!>
!! @file
!! @brief C Interfaces for SCORPIO APIs for inquiring a dimension

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_dim_cint'

MODULE spio_inq_dim_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the id of a dimension
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dname The name of the dimension
!! @param[out] dimid The id of the dimension is returned in this argument
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_dimid(fh, dname, dimid)&
                          bind(C,name="PIOc_inq_dimid")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    CHARACTER(C_CHAR), DIMENSION(*) :: dname
    INTEGER(C_INT) :: dimid
  END FUNCTION PIOc_inq_dimid
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the name of a dimension
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dimid The id of the dimension
!! @param[out] dname The name of the dimension
!! @param[in] dnamelen The length of the user string/buffer, @p dname
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_dimname(fh, dimid, dname, dnamelen)&
                          bind(C,name="PIOc_inq_dimname")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: dimid
    CHARACTER(C_CHAR), DIMENSION(*) :: dname
    INTEGER(C_INT), VALUE :: dnamelen
  END FUNCTION PIOc_inq_dimname
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the length of a dimension
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] dimid The id of the dimension
!! @param[out] dimlen The length of the dimension
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_dimlen(fh, dimid, dimlen)&
                          bind(C,name="PIOc_inq_dimlen")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: dimid
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: dimlen
  END FUNCTION PIOc_inq_dimlen
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

END MODULE spio_inq_dim_cint
