!> @file
!! @brief APIs related to files (open, close etc)
!!
!! This file contains the SCORPIO APIs to access files in the
!! filesystem
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_file.F90'

MODULE spio_file
  USE iso_c_binding
  USE pio_types, ONLY : iosystem_desc_t, file_desc_t, var_desc_t, PIO_FMODE_CLR
  USE pio_kinds, ONLY : PIO_OFFSET_KIND
  USE spio_file_cint
#ifdef TIMING
  use perf_mod, only : t_startf, t_stopf   !_EXTERNAL
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_openfile, pio_createfile, pio_closefile,&
            pio_deletefile, pio_setframe, pio_advanceframe,&
            pio_syncfile, pio_file_is_open, pio_set_fill

CONTAINS

!> @defgroup pio_openfile pio_openfile
!! @public
!! @brief Open an existing file
!!
!! @details
!! @param[in] iosys The handle to the I/O system. @copydoc iosystem_desc_t
!! @param[out] file The handle to the opened file is returned via this arg.
!!              @copydoc file_desc_t
!! @param[inout] iotype The I/O type to use to open the file. The library
!!                      can modify the I/O type when retrying with other
!!                      I/O types (when the user specified I/O type fails).
!!                      If a different I/O type is used by the library this
!!                      argument is updated accordingly. @copydoc PIO_iotype
!! @param[in] fname The name of the file to be opened
!! @param[in] mode (Optional) The file access mode. @copydoc open_file_modes
!! @return @copydoc error_return
!!
  INTEGER FUNCTION pio_openfile(iosys, file, iotype, fname, mode) RESULT(ierr)
    TYPE(iosystem_desc_t), TARGET, INTENT(IN) :: iosys
    TYPE(file_desc_t), INTENT(OUT) :: file
    INTEGER, INTENT(IN) :: iotype
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, OPTIONAL, INTENT(IN) :: mode

    INTEGER(C_INT) :: ciotype, cmode, cerr

#ifdef TIMING
    call t_startf("PIO:openfile")
#endif

    IF(PRESENT(mode)) THEN
      cmode = INT(mode, C_INT)
    ELSE
      cmode = INT(PIO_FMODE_CLR, C_INT)
    ENDIF

    ! Note: The I/O type is provided by the user but can be overridden
    ! (and modified) by the library. e.g. When the library retries
    ! opening files with a different I/O type (when the user specified
    ! I/O type fails to open the file)
    ciotype = INT(iotype, C_INT)
    cerr = PIOc_openfile(iosys%iosysid, file%fh, ciotype,&
                          trim(fname) // C_NULL_CHAR, cmode)
    ierr = INT(cerr)

    ! FIXME: A lot of E3SM code expects that the iotype passed in is not
    ! modified, this is problematic when the C library retries opening
    ! the file using a different I/O type. So we eventually need to
    ! uncomment the line below and change the iotype arg to an INOUT arg
    !iotype = INT(ciotype)

    file%iosystem => iosys

#ifdef TIMING
    call t_stopf("PIO:openfile")
#endif

  END FUNCTION pio_openfile

!> @defgroup pio_createfile pio_createfile
!! @public
!! @brief Create a new file
!!
!! @details
!! @param[in] iosys The handle to the I/O system. @copydoc iosystem_desc_t
!! @param[out] file The handle to the created file is returned via this arg.
!!              @copydoc file_desc_t
!! @param[inout] iotype The I/O type to use to create the file. The library
!!                      can modify the I/O type when retrying with other
!!                      I/O types (when the user specified I/O type fails).
!!                      If a different I/O type is used by the library this
!!                      argument is updated accordingly. @copydoc PIO_iotype
!! @param[in] fname The name of the file to be created
!! @param[in] mode (Optional) The file access mode. @copydoc create_file_modes
!! @return @copydoc error_return
!!
  INTEGER FUNCTION pio_createfile(iosys, file, iotype, fname, mode) RESULT(ierr)
    TYPE(iosystem_desc_t), TARGET, INTENT(IN) :: iosys
    TYPE(file_desc_t), INTENT(OUT) :: file
    INTEGER, INTENT(IN) :: iotype
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, OPTIONAL, INTENT(IN) :: mode

    INTEGER(C_INT) :: ciotype, cmode, cerr

#ifdef TIMING
    call t_startf("PIO:createfile")
#endif

    IF(PRESENT(mode)) THEN
      cmode = INT(mode, C_INT)
    ELSE
      cmode = INT(PIO_FMODE_CLR, C_INT)
    ENDIF

    ! Note: The I/O type is provided by the user but can be overridden
    ! (and modified) by the library. e.g. When the library retries
    ! creating files with a different I/O type (when the user specified
    ! I/O type fails to create the file)
    ciotype = INT(iotype, C_INT)
    cerr = PIOc_createfile(iosys%iosysid, file%fh, ciotype,&
                          trim(fname) // C_NULL_CHAR, cmode)
    ierr = INT(cerr)

    ! FIXME: A lot of E3SM code expects that the iotype passed in is not
    ! modified, this is problematic when the C library retries opening
    ! the file using a different I/O type. So we eventually need to
    ! uncomment the line below and change the iotype arg to an INOUT arg
    !iotype = INT(ciotype)
    file%iosystem => iosys

#ifdef TIMING
    call t_stopf("PIO:createfile")
#endif

  END FUNCTION pio_createfile

!> @defgroup pio_closefile pio_closefile
!! @public
!! @brief Close a file
!!
!! @details
!! @param[inout] file The handle to the file being closed. @copydoc file_desc_t
!! @retval ierr (Optional) The return value
!!
  SUBROUTINE pio_closefile(file, ierr)
    TYPE(file_desc_t), INTENT(INOUT) :: file
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: cerr

#ifdef TIMING
    call t_startf("PIO:closefile")
#endif

    cerr = PIOc_closefile(file%fh)
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ENDIF

    nullify(file%iosystem)

#ifdef TIMING
    call t_stopf("PIO:closefile")
#endif

  END SUBROUTINE pio_closefile

!> @defgroup pio_deletefile pio_deletefile
!! @public
!! @brief Delete a file
!!
!! @details
!! @param[in] iosys The handle to the I/O system. @copydoc iosystem_desc_t
!! @param[in] fname The name of the file to delete
!! @retval ierr (Optional) The return value
!!
  SUBROUTINE pio_deletefile(iosys, fname, ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: cerr

#ifdef TIMING
    call t_startf("PIO:deletefile")
#endif

    cerr = PIOc_deletefile(iosys%iosysid, trim(fname) // C_NULL_CHAR)
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ENDIF

#ifdef TIMING
    call t_stopf("PIO:deletefile")
#endif

  END SUBROUTINE pio_deletefile

!> @defgroup pio_setframe pio_setframe
!! @public
!! @brief Sets the frame number (the value of the record dimension) for a
!! variable. When data is written out one record at a time (e.g. writing
!! simulation data 1 timestep at a time), the record/frame number is set
!! by the user before writing out the current record/frame.
!!
!! @details
!! @param[in] file The handle to the file that contains the variable.
!!                    @copydoc file_desc_t
!! @param[in] vdesc The handle to the variable. The frame number of this
!!                     variable is set by the function. @copydoc file_desc_t
!! @param[in] frame The frame number to set
!! @retval ierr (Optional) The return value
!!
  SUBROUTINE pio_setframe(file, vdesc, frame, ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: frame
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: cerr

#ifdef TIMING
    ! Lightweight call, skip timing
    !call t_startf("PIO:setframe")
#endif

    cerr = PIOc_setframe(file%fh, vdesc%varid - 1, INT(frame - 1, C_INT))
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ENDIF

#ifdef TIMING
    ! Lightweight call, skip timing
    !call t_stopf("PIO:setframe")
#endif

  END SUBROUTINE pio_setframe

!> @defgroup pio_advanceframe pio_advanceframe
!! @public
!! @brief Advance the frame number (the value of the record dimension) for a
!! variable. When data is written out one record at a time (e.g. writing
!! simulation data 1 timestep at a time), the record/frame number is advanced
!! by the user before writing out the next record/frame.
!!
!! @details
!! @param[in] file The handle to the file that contains the variable.
!!                    @copydoc file_desc_t
!! @param[in] vdesc The handle to the variable. The frame number of this
!!                     variable is advanced by the function. @copydoc file_desc_t
!! @retval ierr (Optional) The return value
!!
  SUBROUTINE pio_advanceframe(file, vdesc, ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    TYPE(var_desc_t), INTENT(IN) :: vdesc
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: cerr

#ifdef TIMING
    ! Lightweight call, skip timing
    !call t_startf("PIO:advanceframe")
#endif

    cerr = PIOc_advanceframe(file%fh, vdesc%varid - 1)
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ENDIF

#ifdef TIMING
    ! Lightweight call, skip timing
    !call t_stopf("PIO:advanceframe")
#endif

  END SUBROUTINE pio_advanceframe

!> @defgroup pio_syncfile pio_syncfile
!! @public
!! @brief Synchronize a file. The data written to the file is flushed to the
!! disk (before the call returns to the user)
!!
!! @details
!! @param[inout] file The handle to the file being synced. @copydoc file_desc_t
!! @retval ierr (Optional) The return value
!!
  SUBROUTINE pio_syncfile(file, ierr)
    TYPE(file_desc_t), INTENT(INOUT) :: file
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: cerr

#ifdef TIMING
    call t_startf("PIO:syncfile")
#endif

    cerr = PIOc_sync(file%fh)
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ENDIF

#ifdef TIMING
    call t_stopf("PIO:syncfile")
#endif

  END SUBROUTINE pio_syncfile

!> @defgroup pio_file_is_open pio_file_is_open
!! @public
!! @brief Check if a file is open
!!
!! @details
!! @param[inout] file The handle to the file. @copydoc file_desc_t
!! @return .true. if the file is currently open, .false. otherwise
!!
  LOGICAL FUNCTION pio_file_is_open(file) RESULT(file_is_open)
    TYPE(file_desc_t), INTENT(INOUT) :: file

    INTEGER(C_INT) :: cis_open

    file_is_open = .false.
#ifdef TIMING
    ! Lightweight call, skip timing
    ! call t_startf("PIO:pio_file_is_open")
#endif

    IF(ASSOCIATED(file%iosystem)) THEN
      cis_open = PIOc_File_is_Open(file%fh)
      IF(cis_open == 1) THEN
        file_is_open = .true.
      ENDIF
    ENDIF

#ifdef TIMING
    ! Lightweight call, skip timing
    ! call t_stopf("PIO:pio_file_is_open")
#endif

  END FUNCTION pio_file_is_open

!> @defgroup pio_set_fill pio_set_fill
!! @public
!! @brief Set the fillmode for a file
!!
!! @details
!! The fillvalue mode for all variables in a file can be set
!! using this function. This mode can be used by the library to choose
!! whether variables are pre-filled with fillvalues. Users can choose
!! to disable pre-filling of variables when variables are written out
!! completely, without any missing values, to improve write performance
!! (by avoiding writing the variable data twice - with missing values
!! and then the actual data).
!!
!! Note that this is considered as a hint to the library that can
!! possibly improve write performance in certain use cases. However
!! the library can ignore this hint if it is not supported by the low
!! level I/O library.
!!
!! @param[in] file The handle to the file. @copydoc file_desc_t
!! @param[in] fillmode The new fillmode for the file. Setting fillmode
!! to PIO_FILL (the default) ensures that all variables are pre-filled
!! with fillvalues. Set the fillmode to PIO_NOFILL to disable pre-filling
!! of variables with fillvalues
!! @param[out] prev_fillmode (Optional) The function returns the previous
!! fillmode for the file in this arg
!! @return PIO_NOERR if successful an error code otherwise
!!
  INTEGER FUNCTION pio_set_fill(file, fillmode, prev_fillmode) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: fillmode
    INTEGER, INTENT(OUT), OPTIONAL :: prev_fillmode

    INTEGER(C_INT) :: cfillmode, cprev_fillmode, cerr

    cfillmode = INT(fillmode, C_INT)
    cerr = PIOc_set_fill(file%fh, cfillmode, cprev_fillmode)
    ierr = INT(cerr)

    IF(PRESENT(prev_fillmode)) THEN
      prev_fillmode = INT(cprev_fillmode)
    END IF
  END FUNCTION pio_set_fill

END MODULE spio_file
