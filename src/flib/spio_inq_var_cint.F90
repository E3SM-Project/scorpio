!>
!! @file
!! @brief C Interfaces for SCORPIO APIs for inquiring a variable

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_var_cint'

MODULE spio_inq_var_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

!! FIXME : The variable and attribute types are declared here as
!! INTEGER(C_INT)s, however the C interface uses nc_type (not an int).
!! The C interfaces need to be changed to accept ints (rather than
!! nc_types)
INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the id of a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] vname The name of the variable
!! @param[out] varid The id of the variable is returned in this argument
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_varid(fh, vname, varid)&
                          bind(C,name="PIOc_inq_varid")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    CHARACTER(C_CHAR), DIMENSION(*) :: vname
    INTEGER(C_INT) :: varid
  END FUNCTION PIOc_inq_varid
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the name of a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vname The name of the variable
!! @param[in] vnamelen The length of the user string/buffer, @p vname
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_varname(fh, varid, vname, vnamelen)&
                          bind(C,name="PIOc_inq_varname")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    CHARACTER(C_CHAR), DIMENSION(*) :: vname
    INTEGER(C_INT), VALUE :: vnamelen
  END FUNCTION PIOc_inq_varname
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the type of a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vtype The type of the variable
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_vartype(fh, varid, vtype)&
                          bind(C,name="PIOc_inq_vartype")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: vtype
  END FUNCTION PIOc_inq_vartype
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the number of dimensions of a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vndims The number of dimensions of the variable
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_varndims(fh, varid, vndims)&
                          bind(C,name="PIOc_inq_varndims")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: vndims
  END FUNCTION PIOc_inq_varndims
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the number of attributes of a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vnatts The number of attributes of the variable
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_varnatts(fh, varid, vnatts)&
                          bind(C,name="PIOc_inq_varnatts")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: vnatts
  END FUNCTION PIOc_inq_varnatts
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the ids of dimensions of a variable
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] vdimids The ids of dimensions of the variable
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_vardimid(fh, varid, vdimids)&
                          bind(C,name="PIOc_inq_vardimid")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT), DIMENSION(*) :: vdimids
  END FUNCTION PIOc_inq_vardimid
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire compression settings for
!! a variable. Currently only supported for NetCDF4/HDF5 variables
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] shuffle The shuffle filter setting (0 to disable, 1 to enable) for the variable
!! @param[out] deflate The deflate filter setting (0 to disable, 1 to enable) for the variable
!! @param[out] deflate_level The deflate filter level setting for the variable. The levels
!!                          range from 1 (lowest compression) to 9 (highest level of compression)
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_var_deflate(fh, varid, shuffle, deflate, deflate_level)&
                          bind(C,name="PIOc_inq_var_deflate")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: shuffle
    INTEGER(C_INT) :: deflate
    INTEGER(C_INT) :: deflate_level
  END FUNCTION PIOc_inq_var_deflate
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire chunk settings for
!! a variable. Currently only supported for NetCDF4/HDF5 variables
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the variable
!! @param[out] storage The storage setting for the variable. @copydoc PIO_chunking_settings
!! @param[out] chunksizes The chunk sizes for the variable. The size of this array should
!!                        be equal to the number of dimensions of the variable and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_var_chunking(fh, varid, storage, chunksizes)&
                          bind(C,name="PIOc_inq_var_chunking")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: storage
    INTEGER(C_INT), DIMENSION(*) :: chunksizes
  END FUNCTION PIOc_inq_var_chunking
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

END MODULE spio_inq_var_cint
