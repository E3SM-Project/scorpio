!>
!! @file
!! @brief C Interfaces for SCORPIO APIs for inquiring an attribute

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_att_cint'

MODULE spio_inq_att_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the id of an attribute
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] attname The name of the attribute
!! @param[out] attid The id of the attribute is returned in this argument
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_attid(fh, varid, attname, attid)&
                          bind(C,name="PIOc_inq_attid")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    CHARACTER(C_CHAR), DIMENSION(*) :: attname
    INTEGER(C_INT) :: attid
  END FUNCTION PIOc_inq_attid
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the name of an attribute
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The variable id
!! @param[in] attid The id of the attribute
!! @param[out] attname The name of the attribute
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_attname(fh, varid, attid, attname)&
                          bind(C,name="PIOc_inq_attname")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT), VALUE :: attid
    CHARACTER(C_CHAR), DIMENSION(*) :: attname
  END FUNCTION PIOc_inq_attname
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the type of an attribute
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] attname The name of the attribute
!! @param[out] atttype The type of the attribute
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_atttype(fh, varid, attname, atttype)&
                          bind(C,name="PIOc_inq_atttype")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    CHARACTER(C_CHAR), DIMENSION(*) :: attname
    INTEGER(C_INT) :: atttype
  END FUNCTION PIOc_inq_atttype
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the length of an attribute
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] attname The name of the attribute
!! @param[out] attlen The length of the attribute
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_attlen(fh, varid, attname, attlen)&
                          bind(C,name="PIOc_inq_attlen")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    CHARACTER(C_CHAR), DIMENSION(*) :: attname
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: attlen
  END FUNCTION PIOc_inq_attlen
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire an attribute
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[in] attname The name of the attribute
!! @param[out] atttype The type of the attribute
!! @param[out] attlen The length of the attribute
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_att(fh, varid, attname, atttype, attlen)&
                          bind(C,name="PIOc_inq_att")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    CHARACTER(C_CHAR), DIMENSION(*) :: attname
    INTEGER(C_INT) :: atttype
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: attlen
  END FUNCTION PIOc_inq_att
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

END MODULE spio_inq_att_cint
