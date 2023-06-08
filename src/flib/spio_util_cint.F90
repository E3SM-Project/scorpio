!>
!! @file
!! @brief C Interfaces for miscellaneous util SCORPIO APIs

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_util_cint'

MODULE spio_util_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Fortran interface to C function to inquire the number of
!! dimensions of a variable in a file
!!
!! @param[in] fh  The file handle/id
!! @param[in] varid The variable id
!! @param[out] ndims The number of dimensions of the variable is
!! returned in this argument
!! @returns Returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_varndims(fh, varid, ndims)&
                          bind(C,name="PIOc_inq_varndims")
    USE iso_c_binding
    INTEGER(C_INT), INTENT(IN), VALUE :: fh
    INTEGER(C_INT), INTENT(IN), VALUE :: varid
    INTEGER(C_INT), INTENT(OUT) :: ndims
  END FUNCTION PIOc_inq_varndims
END INTERFACE

INTERFACE
!> @private
!! @brief Fortran interface to C function to inquire variable
!! dimension ids
!!
!! @param[in] fh  The file handle/id
!! @param[in] varid The variable id
!! @param[out] dimids The dimension ids for the different dimensions
!! of the variable will be returned in this argument. The size of this
!! array should be equal to the number of dimensions of the variable
!! @returns Returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_vardimid(fh, varid, dimids)&
                          bind(C,name="PIOc_inq_vardimid")
    USE iso_c_binding
    INTEGER(C_INT), INTENT(IN), VALUE :: fh
    INTEGER(C_INT), INTENT(IN), VALUE :: varid
    INTEGER(C_INT), DIMENSION(*)  :: dimids
  END FUNCTION PIOc_inq_vardimid
END INTERFACE

INTERFACE
!> @private
!! @brief Fortran interface to C function to inquire the length
!! of a dimension in a file
!!
!! @param[in] fh  The file handle/id
!! @param[in] dimid The dimension id
!! @param[out] len The length of the dimension is returned in
!! this argument
!! @returns Returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_dimlen(fh, dimid, len)&
                          bind(C,name="PIOc_inq_dimlen")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), INTENT(IN), VALUE :: fh
    INTEGER(C_INT), INTENT(IN), VALUE :: dimid
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), INTENT(OUT) :: len
  END FUNCTION PIOc_inq_dimlen
END INTERFACE

END MODULE spio_util_cint
