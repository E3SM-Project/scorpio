!>
!! @file
!! @brief C Interfaces for APIs that handle error codes
!!

MODULE spio_file_cint
  USE iso_c_binding
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Open an existing file
!!
!! @details
!! @param[in] iosysid The handle/id to the I/O system.
!! @param[out] fh The handle/id to the opened file is returned via this arg.
!! @param[inout] iotype @copydoc PIO_iotype
!! @param[in] fname The name of the file to be opened
!! @param[in] mode The file access mode. @copydoc open_file_modes
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_openfile(iosysid, fh, iotype, fname, mode)&
                          bind(C,name="PIOc_openfile")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT)  :: fh
    INTEGER(C_INT)  :: iotype
    CHARACTER(C_CHAR), DIMENSION(*) :: fname
    INTEGER(C_INT), VALUE :: mode
  END FUNCTION PIOc_openfile
END INTERFACE

INTERFACE
!> @private
!! @brief Create a file
!!
!! @details
!! @param[in] iosysid The handle/id to the I/O system.
!! @param[out] fh The handle/id to the created file is returned via this arg.
!! @param[inout] iotype @copydoc PIO_iotype
!! @param[in] fname The name of the file to be created
!! @param[in] mode The file access mode. @copydoc create_file_modes
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_createfile(iosysid, fh, iotype, fname, mode)&
                          bind(C,name="PIOc_createfile")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT)  :: fh
    INTEGER(C_INT)  :: iotype
    CHARACTER(C_CHAR), DIMENSION(*) :: fname
    INTEGER(C_INT), VALUE :: mode
  END FUNCTION PIOc_createfile
END INTERFACE

INTERFACE
!> @private
!! @brief Close a file
!!
!! @details
!! @param[in] fh The handle/id to the created file is returned via this arg.
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_closefile(fh)&
                          bind(C,name="PIOc_closefile")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: fh
  END FUNCTION PIOc_closefile
END INTERFACE

INTERFACE
!> @private
!! @brief Delete a file
!!
!! @details
!! @param[in] iosysid The handle/id to the I/O system.
!! @param[in] fname The name of the file being deleted
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_deletefile(iosysid, fname)&
                          bind(C,name="PIOc_deletefile")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: iosysid
    CHARACTER(C_CHAR), DIMENSION(*) :: fname
  END FUNCTION PIOc_deletefile
END INTERFACE

INTERFACE
!> @private
!! @brief Set the frame/record number (the index in the record dimension)
!! for a variable
!!
!! @details
!! @param[in] fh The handle/id to the file with the variable
!! @param[in] varid The handle/id to the variable
!! @param[in] frame The frame number
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_setframe(fh, varid, frame)&
                          bind(C,name="PIOc_setframe")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
    INTEGER(C_INT), VALUE :: frame
  END FUNCTION PIOc_setframe
END INTERFACE

INTERFACE
!> @private
!! @brief Advance the frame/record number (the index in the record dimension)
!! for a variable
!!
!! @details
!! @param[in] fh The handle/id to the file with the variable
!! @param[in] varid The handle/id to the variable
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_advanceframe(fh, varid)&
                          bind(C,name="PIOc_advanceframe")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: varid
  END FUNCTION PIOc_advanceframe
END INTERFACE

INTERFACE
!> @private
!! @brief Synchronize contents of a file to the filesystem (all data written
!! out to this file is flushed to the filesystem)
!!
!! @details
!! @param[in] fh The handle/id to the file
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_sync(fh)&
                          bind(C,name="PIOc_sync")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: fh
  END FUNCTION PIOc_sync
END INTERFACE

INTERFACE
!> @private
!! @brief Query if a file is currently open
!!
!! @details
!! @param[in] fh The handle/id to the file
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_File_is_Open(fh)&
                          bind(C,name="PIOc_File_is_Open")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: fh
  END FUNCTION PIOc_File_is_Open
END INTERFACE

INTERFACE
!> @private
!! @brief Set the default fillmode to use for variables in a file
!!
!! @details
!! @param[in] fh The handle/id to the file with the variable
!! @param[in] fillmode The fillmode to use for variables in the file
!! @param[out]  prev_fillmode The previous fillmode set for this file
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_set_fill(fh, fillmode, prev_fillmode)&
                          bind(C,name="PIOc_set_fill")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: fh
    INTEGER(C_INT), VALUE :: fillmode
    INTEGER(C_INT)        :: prev_fillmode
  END FUNCTION PIOc_set_fill
END INTERFACE

END MODULE spio_file_cint
