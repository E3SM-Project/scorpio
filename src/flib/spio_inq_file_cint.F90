!>
!! @file
!! @brief C Interfaces for SCORPIO APIs for inquiring a file

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_inq_file_cint'

MODULE spio_inq_file_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the number of variables
!! in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] nvars The number of variables in a file
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_nvars(fh, nvars)&
                          bind(C,name="PIOc_inq_nvars")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT) :: nvars
  END FUNCTION PIOc_inq_nvars
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the number of dimensions
!! in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] ndims The number of dimensions in a file
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_ndims(fh, ndims)&
                          bind(C,name="PIOc_inq_ndims")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT) :: ndims
  END FUNCTION PIOc_inq_ndims
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the number of attributes
!! in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] natts The number of attributes in a file
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_natts(fh, natts)&
                          bind(C,name="PIOc_inq_natts")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT) :: natts
  END FUNCTION PIOc_inq_natts
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the unlimited dimension
!! in a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[out] unlimdim The id of the unlimited dimension in the file
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_unlimdim(fh, unlimdim)&
                          bind(C,name="PIOc_inq_unlimdim")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT) :: unlimdim
  END FUNCTION PIOc_inq_unlimdim
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the name of a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the file
!! @param[out] vname The name of the file
!! @param[in] vnamelen The length of the user string/buffer, @p vname
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_filename(fh, varid, vname, vnamelen)&
                          bind(C,name="PIOc_inq_filename")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    CHARACTER(C_CHAR), DIMENSION(*) :: vname
    INTEGER(C_INT), VALUE :: vnamelen
  END FUNCTION PIOc_inq_filename
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the type of a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the file
!! @param[out] vtype The type of the file
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_type(fh, varid, vtype)&
                          bind(C,name="PIOc_inq_type")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: vtype
  END FUNCTION PIOc_inq_type
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire the ids of dimensions of a file
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the file
!! @param[out] vdimids The ids of dimensions of the file
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_dimid(fh, varid, vdimids)&
                          bind(C,name="PIOc_inq_dimid")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT), DIMENSION(*) :: vdimids
  END FUNCTION PIOc_inq_dimid
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire compression settings for
!! a file. Currently only supported for NetCDF4/HDF5 files
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the file
!! @param[out] shuffle The shuffle filter setting (0 to disable, 1 to enable) for the file
!! @param[out] deflate The deflate filter setting (0 to disable, 1 to enable) for the file
!! @param[out] deflate_level The deflate filter level setting for the file. The levels
!!                          range from 1 (lowest compression) to 9 (highest level of compression)
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_file_deflate(fh, varid, shuffle, deflate, deflate_level)&
                          bind(C,name="PIOc_inq_file_deflate")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: shuffle
    INTEGER(C_INT) :: deflate
    INTEGER(C_INT) :: deflate_level
  END FUNCTION PIOc_inq_file_deflate
END INTERFACE

INTERFACE
!>
!! @private
!! @brief Fortran interface to C function to inquire chunk settings for
!! a file. Currently only supported for NetCDF4/HDF5 files
!!
!! @details
!! @param[in] fh The file id/handle
!! @param[in] varid The id of the file
!! @param[out] storage The storage setting for the file. @copydoc PIO_chunking_settings
!! @param[out] chunksizes The chunk sizes for the file. The size of this array should
!!                        be equal to the number of dimensions of the file and specifies
!!                        the size of individual chunks (chunksizes(i) is the size of the
!!                        chunk for dimension i)
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_inq_file_chunking(fh, varid, storage, chunksizes)&
                          bind(C,name="PIOc_inq_file_chunking")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT) :: storage
    INTEGER(C_INT), DIMENSION(*) :: chunksizes
  END FUNCTION PIOc_inq_file_chunking
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

END MODULE spio_inq_file_cint
