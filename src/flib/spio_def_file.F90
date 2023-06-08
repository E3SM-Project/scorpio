!> @file
!! @brief SCORPIO APIs for entering/exiting define mode
!! This file contains the SCORPIO APIs for entering/exiting
!! the define mode for a file. The define mode is used to
!! define meta-data associated with data in a file.
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_def_file.F90'

MODULE spio_def_file
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t, PIO_MAX_NAME,&
                        PIO_MAX_VAR_DIMS, PIO_NOERR, PIO_EINTERNAL
  USE pio_kinds, ONLY : i4, PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_util, ONLY : f2cstring, get_text_var_sz, get_var_dim_sz
  USE spio_err, ONLY  : pio_error, pio_warn
  USE spio_def_file_cint
#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_enddef, pio_redef

!> @defgroup PIO_enddef PIO_enddef
!! @public
!! @brief End the "define" mode of the file
!!
!! @details
!! This API can be used to end the "define" mode for a file. The define
!! mode is a setting for the file to define the meta-data associated with
!! the data in the file (define the variables, dimensions, attributes etc).
!! A file enters the "define" mode when its created or explicitly using the
!! @p PIO_redef call. The user needs to end/exit the "define" mode to start
!! writing data to the file.
!!
!! The meta-data specified during the "define" mode is typically stored in
!! the output file header section.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_enddef
    !! pio_enddef_fh version of the function accepts a file id/handle as an arg 
    MODULE PROCEDURE pio_enddef_fh
    !! pio_enddef_file version of the function accepts a file desc as an arg
    MODULE PROCEDURE pio_enddef_file
  END INTERFACE

!> @defgroup PIO_redef PIO_redef
!! @public
!! @brief Enter the "define" mode of the file
!!
!! @details
!! This API can be used to re-enter the "define" mode for a file. The define
!! mode is a setting for the file to define the meta-data associated with
!! the data in the file (define the variables, dimensions, attributes etc).
!! A file enters the "define" mode when its created or explicitly using the
!! @p PIO_redef call. The user needs to end/exit the "define" mode to start
!! writing data to the file.
!!
!! If a file is opened, the user must explicitly enter the "define" mode
!! using this API to add meta-data (define variables/dimensions/attributes).
!! A user may re-enter the "define" mode multiple times but needs to exit
!! the mode appropriately (@p PIO_enddef) each time. Note that if a file
!! already contains data, adding more meta-data increases the size of the
!! header and can potentially result in data movement (move the data section
!! of the file to accomodate the new meta-data in the header) negatively
!! impacting the I/O performance.
!!
!! If a file is created by the user the file is already in the "define"
!! mode, so users don't need to explicitly "enter" the define mode using
!! this call.
!!
!! The meta-data specified during the "define" mode is typically stored in
!! the output file header section.
!!
!! This API is a collective call (Although the library might internally choose
!! to make progress on a single or small subset of MPI processes) on the I/O
!! system associated with the file
!! 
  INTERFACE pio_redef
    !! pio_redef_fh version of the function accepts a file id/handle as an arg 
    MODULE PROCEDURE pio_redef_fh
    !! pio_redef_file version of the function accepts a file desc as an arg
    MODULE PROCEDURE pio_redef_file
  END INTERFACE

CONTAINS

!>
!! @public
!! @ingroup PIO_enddef
!! @brief End/Exit the "define" mode of a file
!!
!! @details
!! @param[in] fh The file id/handle.
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_enddef_fh(fh) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_enddef_fh")
#endif

    ierr = PIOc_enddef(fh)

#ifdef TIMING
    CALL t_stopf("PIO:pio_enddef_fh")
#endif
  END FUNCTION pio_enddef_fh

!>
!! @public
!! @ingroup PIO_enddef
!! @brief End/Exit the "define" mode of a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_enddef_file(file) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_enddef_file")
#endif

    ierr = PIOc_enddef(file%fh)

#ifdef TIMING
    CALL t_stopf("PIO:pio_enddef_file")
#endif
  END FUNCTION pio_enddef_file

!>
!! @public
!! @ingroup PIO_redef
!! @brief Enter the "define" mode of a file
!!
!! @details
!! @param[in] fh The file id/handle.
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_redef_fh(fh) RESULT(ierr)
    INTEGER, INTENT(IN) :: fh

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_redef_fh")
#endif

    ierr = PIOc_redef(fh)

#ifdef TIMING
    CALL t_stopf("PIO:pio_redef_fh")
#endif
  END FUNCTION pio_redef_fh

!>
!! @public
!! @ingroup PIO_redef
!! @brief Enter the "define" mode of a file
!!
!! @details
!! @param[in] file The file handle. @copydoc file_desc_t
!! @retval ierr @copydoc error_return
!!
  INTEGER FUNCTION pio_redef_file(file) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file

    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg

#ifdef TIMING
    CALL t_startf("PIO:pio_redef_file")
#endif

    ierr = PIOc_redef(file%fh)

#ifdef TIMING
    CALL t_stopf("PIO:pio_redef_file")
#endif
  END FUNCTION pio_redef_file

END MODULE spio_def_file
