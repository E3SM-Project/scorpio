!> @file
!! @brief APIs for handling errors
!! This file contains the APIs for handling errors in the Fortran
!! interface

!> @internal
!! @def __PIO_FILE__
!! @brief This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_err.F90'

MODULE spio_err
  USE iso_c_binding
  USE pio_types, ONLY : iosystem_desc_t, file_desc_t,&
                        PIO_IOSYSID_INVALID, PIO_FH_INVALID, PIO_MAX_NAME,&
                        PIO_NOERR
  USE spio_err_cint
#ifdef TIMING
  use perf_mod, only : t_startf, t_stopf   !_EXTERNAL
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_error, pio_warn, pio_setdebuglevel, pio_seterrorhandling,&
            pio_strerror, pio_set_log_level

!> @defgroup PIO_error PIO_error
!! @brief Handles error codes in the Fortran interface
!!
  INTERFACE pio_error
    MODULE PROCEDURE pio_iosys_error
    MODULE PROCEDURE pio_file_error
    MODULE PROCEDURE pio_comm_error
  END INTERFACE

!> @defgroup PIO_warn PIO_warn
!! @brief Handles warning messages in the Fortran interface
!!
  INTERFACE pio_warn
    MODULE PROCEDURE pio_iosys_warn
    MODULE PROCEDURE pio_file_warn
    MODULE PROCEDURE pio_comm_warn
  END INTERFACE

!> @defgroup PIO_seterrorhandling PIO_seterrorhandling
!! @brief Set the error handler to use for an I/O system of file
!!        @copydoc PIO_error_method
!!
  INTERFACE pio_seterrorhandling
    MODULE PROCEDURE pio_seterrorhandling_iosysid
    MODULE PROCEDURE pio_seterrorhandling_iosys
    MODULE PROCEDURE pio_seterrorhandling_file
  END INTERFACE

CONTAINS

!> @ingroup PIO_error
!> @private
!! @brief Function that handles error codes in the fortran interface
!!
!! @param[in] iosys The I/O system associated with the error
!! @param[in] err_num The error number/code
!! @param[in] fname The name of the source file with the error
!! @param[in] line The line number in the source file with the error
!! @param[in] uerr_msg The user error message
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise (The function can also abort on error)
  INTEGER FUNCTION pio_iosys_error (iosys, err_num, fname, line, uerr_msg) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    INTEGER, INTENT(IN) :: err_num
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uerr_msg

    INTEGER(C_INT) :: cerr

    ! FIXME: Do fortran strings (trim(fname)) need to be converted to
    ! C_CHAR arrays here?
    cerr = PIOc_error(iosys%iosysid, PIO_FH_INVALID, INT(err_num, C_INT),&
                      trim(fname) // C_NULL_CHAR, line,&
                      trim(uerr_msg) // C_NULL_CHAR)
    ierr = INT(cerr)
  END FUNCTION pio_iosys_error

!> @ingroup PIO_error
!> @private
!! @brief Function that handles error codes in the fortran interface
!!
!! @param[in] file The file associated with the error
!! @param[in] err_num The error number/code
!! @param[in] fname The name of the source file with the error
!! @param[in] line The line number in the source file with the error
!! @param[in] uerr_msg The user error message
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise (The function can also abort on error)
  INTEGER FUNCTION pio_file_error (file, err_num, fname, line, uerr_msg) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: err_num
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uerr_msg

    INTEGER(C_INT) :: cerr

    cerr = PIOc_error(PIO_IOSYSID_INVALID, file%fh, INT(err_num, C_INT),&
                      trim(fname) // C_NULL_CHAR, line,&
                      trim(uerr_msg) // C_NULL_CHAR)
    ierr = INT(cerr)
  END FUNCTION pio_file_error

!> @ingroup PIO_error
!> @private
!! @brief Function that handles misc error codes (not associated with an I/O
!! system or file) associated with an MPI communicator in the fortran interface
!!
!! @param[in] comm The MPI communicator associated with the error
!! @param[in] err_num The error number/code
!! @param[in] fname The name of the source file with the error
!! @param[in] line The line number in the source file with the error
!! @param[in] uerr_msg The user error message
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise (The function can also abort on error)
  INTEGER FUNCTION pio_comm_error (comm, err_num, fname, line, uerr_msg) RESULT(ierr)
    INTEGER, INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: err_num
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uerr_msg

    INTEGER, PARAMETER :: ROOT_RANK = 0
    INTEGER :: rank

    CALL MPI_Comm_rank(comm, rank, ierr)
    IF(rank == ROOT_RANK) THEN
      PRINT *, "ERROR: ", TRIM(uerr_msg), ", err_num =", err_num, ", ", trim(fname), ":", line
    ENDIF

    ierr = err_num
  END FUNCTION pio_comm_error

!> @ingroup PIO_warn
!> @private
!! @brief Function that handles warning messages associated with an I/O system
!!
!! @param[in] iosys The I/O system associated with the warning
!! @param[in] fname The name of the source file with the warning
!! @param[in] line The line number in the source file with the warning
!! @param[in] uwarn_msg The user warning message
  SUBROUTINE pio_iosys_warn(iosys, fname, line, uwarn_msg)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uwarn_msg

    CALL PIOc_warn(iosys%iosysid, PIO_FH_INVALID, trim(fname) // C_NULL_CHAR, line,&
                      trim(uwarn_msg) // C_NULL_CHAR)
  END SUBROUTINE pio_iosys_warn

!> @ingroup PIO_warn
!> @private
!! @brief Function that handles warning messages associated with a file
!!
!! @param[in] file The file associated with the error
!! @param[in] fname The name of the source file with the warning
!! @param[in] line The line number in the source file with the warning
!! @param[in] uwarn_msg The user warning message
  SUBROUTINE pio_file_warn(file, fname, line, uwarn_msg)
    TYPE(file_desc_t), INTENT(IN) :: file
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uwarn_msg

    CALL PIOc_warn(PIO_IOSYSID_INVALID, file%fh, trim(fname) // C_NULL_CHAR, line,&
                      trim(uwarn_msg) // C_NULL_CHAR)
  END SUBROUTINE pio_file_warn

!> @ingroup PIO_warn
!> @private
!! @brief Function that handles warning messages associated with an MPI communicator
!!
!! @param[in] comm The MPI communicator associated with the error
!! @param[in] fname The name of the source file with the warning
!! @param[in] line The line number in the source file with the warning
!! @param[in] uwarn_msg The user warning message
  SUBROUTINE pio_comm_warn(comm, fname, line, uwarn_msg)
    INTEGER, INTENT(IN) :: comm
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: uwarn_msg

    INTEGER, PARAMETER :: ROOT_RANK = 0
    INTEGER :: rank
    INTEGER :: ierr

    CALL MPI_Comm_rank(comm, rank, ierr)
    IF(rank == ROOT_RANK) THEN
      PRINT *, "WARNING: ", TRIM(uwarn_msg), ",", trim(fname), ":", line
    ENDIF
  END SUBROUTINE pio_comm_warn

!> @defgroup PIO_setdebuglevel PIO_setdebuglevel
!> @public
!! @brief Set the debug level for debug messages from the library
!!
!! @param[in] level The debug level for debug messages from the library
!! @param[out] ierr (Optional) @copydoc error_return
  SUBROUTINE pio_setdebuglevel(lvl, ierr)
    USE mpi, only : MPI_COMM_WORLD

    INTEGER, INTENT(IN) :: lvl
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cerr, ret

    cerr = PIOc_set_log_level(lvl)
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ELSE
      WRITE(log_msg, *) "Setting the log level to ", lvl, "failed, err = ", cerr
      ret = pio_error(MPI_COMM_WORLD, INT(cerr), __FILE__, __LINE__, trim(log_msg))
    END IF

  END SUBROUTINE pio_setdebuglevel

!> @ingroup PIO_seterrorhandling
!> @public
!! @brief Set the error handler to use for an I/O system
!!
!! @param[in] iosysid The id/handle of the I/O subsystem where the error
!!                    handler needs to be set. @copydoc iosystem_desc_t
!! @param[in] eh  The error handler method to use for this I/O subsystem.
!!                @copydoc PIO_error_method
!! @param[out] old_eh (Optional) The old error handler method used by the
!!                    I/O subsystem.
!! @param[out] ierr (Optional) @copydoc error_return
  SUBROUTINE pio_seterrorhandling_iosysid(iosysid, eh, old_eh, ierr)
    INTEGER, INTENT(IN) :: iosysid
    INTEGER, INTENT(IN) :: eh
    INTEGER, OPTIONAL, INTENT(OUT) :: old_eh
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: ceh
    INTEGER(C_INT) :: cerr, ret

    ceh = PIOc_Set_IOSystem_Error_Handling(iosysid, eh)
    IF(PRESENT(old_eh)) THEN
      old_eh = INT(ceh)
    END IF
    IF(PRESENT(ierr)) THEN
      ierr = PIO_NOERR
    END IF

  END SUBROUTINE pio_seterrorhandling_iosysid

!> @ingroup PIO_seterrorhandling
!> @public
!! @brief Set the error handler to use for an I/O system
!!
!! @param[in] iosys The I/O subsystem where the error handler needs to be set
!!                  @copydoc iosystem_desc_t
!! @param[in] eh  The error handler method to use for this I/O subsystem.
!!                @copydoc PIO_error_method
!! @param[out] old_eh (Optional) The old error handler method used by the
!!                    I/O subsystem.
!! @param[out] ierr (Optional) @copydoc error_return
  SUBROUTINE pio_seterrorhandling_iosys(iosys, eh, old_eh, ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    INTEGER, INTENT(IN) :: eh
    INTEGER, OPTIONAL, INTENT(OUT) :: old_eh
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: ceh
    INTEGER(C_INT) :: cerr, ret

    ceh = PIOc_Set_IOSystem_Error_Handling(iosys%iosysid, eh)
    IF(PRESENT(old_eh)) THEN
      old_eh = INT(ceh)
    END IF
    IF(PRESENT(ierr)) THEN
      ierr = PIO_NOERR
    END IF

  END SUBROUTINE pio_seterrorhandling_iosys

!> @ingroup PIO_seterrorhandling
!> @public
!! @brief Set the error handler to use for a file.
!!
!! @details
!! Set the error handler to use for a file. Note that setting the error
!! handler for a file also sets the error handler for the I/O system
!! associated with the file
!! @param[in] file The file to set the error handler
!!                 @copydoc file_desc_t
!! @param[in] eh  The error handler method to use for this file & I/O subsystem.
!!                @copydoc PIO_error_method
!! @param[out] old_eh (Optional) The old error handler method used by the
!!                    I/O subsystem associated with this file
!! @param[out] ierr (Optional) @copydoc error_return
  SUBROUTINE pio_seterrorhandling_file(file, eh, old_eh, ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: eh
    INTEGER, OPTIONAL, INTENT(OUT) :: old_eh
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: ceh
    INTEGER(C_INT) :: cerr, ret

    ceh = PIOc_Set_IOSystem_Error_Handling(file%iosystem%iosysid, eh)
    IF(PRESENT(old_eh)) THEN
      old_eh = INT(ceh)
    END IF
    IF(PRESENT(ierr)) THEN
      ierr = PIO_NOERR
    END IF

  END SUBROUTINE pio_seterrorhandling_file

!> @defgroup PIO_strerror
!!
!! @brief Get a descriptive string/text for an error code
!!
!! @param[in] errcode The error code
!! @param[out] errmsg The error message corresponding to the error code
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise
  INTEGER FUNCTION pio_strerror(errcode, errmsg) RESULT(ierr)
    INTEGER, INTENT(IN) :: errcode
    CHARACTER(LEN=*), INTENT(OUT) :: errmsg

    CHARACTER(C_CHAR) :: cerrmsg(PIO_MAX_NAME)
    INTEGER :: i

    cerrmsg = C_NULL_CHAR
    errmsg = ' '
    ierr = PIOc_strerror(errcode, cerrmsg, INT(PIO_MAX_NAME, C_SIZE_T))
    IF(ierr == PIO_NOERR) THEN
      ! FIXME: Since we don't have access to the I/O system we cannot use c2fstring here
      DO i=1,MIN(PIO_MAX_NAME, LEN(errmsg))
        IF(cerrmsg(i) == C_NULL_CHAR) THEN
          EXIT
        END IF
        errmsg(i:i) = cerrmsg(i)
      END DO
    END IF

  END FUNCTION pio_strerror

!> @defgroup PIO_set_log_level
!!
!! @brief Set the log level for the I/O library
!!
!! @param[in] log_level The new log level for the I/O library
!! @returns Returns PIO_NOERR on success, the error code, err_num,
!! otherwise
  INTEGER FUNCTION pio_set_log_level(log_level) RESULT(ierr)
    INTEGER, INTENT(IN) :: log_level

    ierr = PIOc_set_log_level(log_level)

  END FUNCTION pio_set_log_level

END MODULE spio_err
