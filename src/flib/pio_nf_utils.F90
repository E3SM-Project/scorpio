!> @file
!! @brief Miscellaneous SCORPIO Util APIs
!! This file contains the SCORPIO Utility APIs that built on top
!! of other SCORPIO APIs
!! This file also includes misc APIs for copying variables and
!! attributes across files
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ "pio_nf_utils.F90"

MODULE pio_nf_utils
  USE pio_types, ONLY : file_desc_t, var_desc_t, pio_noerr,&
                        PIO_MAX_NAME, PIO_NOERR, PIO_EINTERNAL
  USE spio_inq_var, ONLY : pio_inq_vartype, pio_inq_varname
  USE spio_get_var, ONLY : pio_get_var
  USE spio_put_var, ONLY : pio_put_var
  USE pio_types, ONLY : pio_int, pio_REAL, pio_double, pio_char
  USE pio_kinds, ONLY : i4, r4, r8
  USE spio_err, ONLY : pio_error, pio_warn

#ifdef TIMING
  USE perf_mod, ONLY  : t_startf, t_stopf
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: copy_pio_var

!> @defgroup copy_pio_var copy_pio_var
!! @brief Copy variables between two files
  INTERFACE copy_pio_var
     MODULE PROCEDURE copy_pio_var01d
     MODULE PROCEDURE copy_pio_var2d
  END INTERFACE

CONTAINS

! FIXME : This function needs to be moved to the C library
!>
!! @public
!! @ingroup copy_pio_var
!! @brief Copy a 0d/scalar/character string or a 1d variable/array_of_strings
!! from one file to another
!!
!! @details
!! @param[in] ifile The file handle of the input file. @copydoc file_desc_t
!! @param[in] ofile The file handle of the output file. @copydoc file_desc_t
!! @param[in] ivdesc The descriptor of the input variable (in the input file).
!!                    @copydoc var_desc_t
!! @param[in] ovdesc The descriptor of the output variable (in the output file).
!!                    @copydoc var_desc_t
!! @param[in] varsz The size of the variable (or the length of each string)
!! @param[in] strlength (Optional) The size/length of the character string array,
!!                      if the variable being copied is a 1d string array
!! @param[out] ierr (Optional) The return value.
!!
SUBROUTINE copy_pio_var01d(ifile, ofile, ivdesc, ovdesc, varsz, strlength, ierr)
  TYPE(file_desc_t), INTENT(IN) :: ifile, ofile
  TYPE(var_desc_t), INTENT(IN) ::  ivdesc, ovdesc
  INTEGER, INTENT(IN) :: varsz
  INTEGER, INTENT(IN), OPTIONAL :: strlength
  INTEGER, INTENT(OUT), OPTIONAL :: ierr

  INTEGER :: itype, otype
  INTEGER(i4), ALLOCATABLE :: ival(:)
  REAL(r4), ALLOCATABLE :: rval(:)
  REAL(r8), ALLOCATABLE :: dval(:)
  CHARACTER(LEN=varsz), ALLOCATABLE :: cval(:)

  CHARACTER(LEN=PIO_MAX_NAME) :: ivname, ovname
  CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
  INTEGER :: ret

  IF(PRESENT(ierr)) THEN
    ierr = PIO_NOERR
  END IF
  
  ret = pio_inq_varname(ifile, ivdesc, ivname)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the name of the variable (varid = ", ivdesc%varid, ")",&
                      " in input file (file id = ", ifile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF

  ret = pio_inq_varname(ofile, ovdesc, ovname)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the name of the variable (varid = ", ovdesc%varid, ")",&
                      " in output file (file id = ", ofile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF

  ret = pio_inq_vartype(ifile, ivdesc, itype)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the type of the variable, ", TRIM(ivname),&
                      "(varid = ", ivdesc%varid, "),",&
                      " in input file (file id = ", ifile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF

  ret = pio_inq_vartype(ofile, ovdesc, otype)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the type of the variable, ", TRIM(ovname),&
                      "(varid = ", ovdesc%varid, "),",&
                      " in output file (file id = ", ofile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF
  
  IF(itype .ne. otype) THEN
    WRITE(log_msg, *) "The types of the input variable, ", TRIM(ivname),&
                      "(varid = ", ivdesc%varid, "), in the input file",&
                      "(file id = ", ifile%fh, ") and ",&
                      "the output variable, ", TRIM(ovname),&
                      "(varid = ", ovdesc%varid, "), in the output file",&
                      "(file id = ", ofile%fh, ") do not match.",&
                      "Copying will result in coercing of the variable types"
    CALL pio_warn(ifile%iosystem, __PIO_FILE__, __LINE__, TRIM(log_msg))
  END IF

  SELECT CASE(itype)
    CASE(PIO_int)
      ALLOCATE(ival(varsz))

      ret = pio_get_var(ifile, ivdesc%varid, ival)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to get the variable, ", TRIM(ivname),&
                          "(varid = ", ivdesc%varid, "),",&
                          " in the input file (file id = ", ifile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      ret = pio_put_var(ofile, ovdesc%varid, ival)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to put the variable, ", TRIM(ovname),&
                          "(varid = ", ovdesc%varid, "),",&
                          " in the output file (file id = ", ofile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      DEALLOCATE(ival)
    CASE(PIO_real)
      ALLOCATE(rval(varsz))

      ret = pio_get_var(ifile, ivdesc%varid, rval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to get the variable, ", TRIM(ivname),&
                          "(varid = ", ivdesc%varid, "),",&
                          " in the input file (file id = ", ifile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      ret = pio_put_var(ofile, ovdesc%varid, rval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to put the variable, ", TRIM(ovname),&
                          "(varid = ", ovdesc%varid, "),",&
                          " in the output file (file id = ", ofile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      DEALLOCATE(rval)
    CASE(PIO_double)
      ALLOCATE(dval(varsz))

      ret = pio_get_var(ifile, ivdesc%varid, dval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to get the variable, ", TRIM(ivname),&
                          "(varid = ", ivdesc%varid, "),",&
                          " in the input file (file id = ", ifile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      ret = pio_put_var(ofile, ovdesc%varid, dval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to put the variable, ", TRIM(ovname),&
                          "(varid = ", ovdesc%varid, "),",&
                          " in the output file (file id = ", ofile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      DEALLOCATE(dval)
    CASE(PIO_char)
      ! Each string in the cval(:) array is of size 'varsz'
      IF(present(strlength)) THEN
        ALLOCATE(cval(strlength))
      ELSE
        ALLOCATE(cval(1))
      END IF
      ret = pio_get_var(ifile, ivdesc%varid, cval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to get the variable, ", TRIM(ivname),&
                          "(varid = ", ivdesc%varid, "),",&
                          " in the input file (file id = ", ifile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      ret = pio_put_var(ofile, ovdesc%varid, cval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to put the variable, ", TRIM(ovname),&
                          "(varid = ", ovdesc%varid, "),",&
                          " in the output file (file id = ", ofile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      DEALLOCATE(cval)
  END SELECT
END SUBROUTINE copy_pio_var01d

! FIXME : This function needs to be moved to the C library
!>
!! @public
!! @ingroup copy_pio_var
!! @brief Copy a 2d variable from one file to another
!!
!! @details
!! @param[in] ifile The file handle of the input file. @copydoc file_desc_t
!! @param[in] ofile The file handle of the output file. @copydoc file_desc_t
!! @param[in] ivdesc The descriptor of the input variable (in the input file).
!!                    @copydoc var_desc_t
!! @param[in] ovdesc The descriptor of the output variable (in the output file).
!!                    @copydoc var_desc_t
!! @param[in] varsz The variable dimension sizes
!! @param[in] strlength (Optional) The length of the string, if the variable
!!                      being copied is a string
!! @param[out] ierr (Optional) The return value.
!!
SUBROUTINE copy_pio_var2d(ifile, ofile, ivdesc, ovdesc, varsz, ierr)
  TYPE(file_desc_t) :: ifile, ofile
  TYPE(var_desc_t) ::  ivdesc, ovdesc
  INTEGER, INTENT(IN) :: varsz(:)
!  INTEGER, INTENT(IN), OPTIONAL :: strlength
  INTEGER, OPTIONAL, INTENT(OUT) :: ierr

  INTEGER :: itype, otype
  INTEGER(i4), ALLOCATABLE :: ival(:,:)
  REAL(r4), ALLOCATABLE :: rval(:,:)
  REAL(r8), ALLOCATABLE :: dval(:,:)
!  CHARACTER(LEN=varsz), ALLOCATABLE :: cval(:,:)

  CHARACTER(LEN=PIO_MAX_NAME) :: ivname, ovname
  CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
  INTEGER :: ret
  
  IF(PRESENT(ierr)) THEN
    ierr = PIO_NOERR
  END IF
  
  ret = pio_inq_varname(ifile, ivdesc, ivname)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the name of the variable (varid = ", ivdesc%varid, ")",&
                      " in input file (file id = ", ifile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF

  ret = pio_inq_varname(ofile, ovdesc, ovname)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the name of the variable (varid = ", ovdesc%varid, ")",&
                      " in output file (file id = ", ofile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF

  ret = pio_inq_vartype(ifile, ivdesc, itype)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the type of the variable, ", TRIM(ivname),&
                      "(varid = ", ivdesc%varid, "),",&
                      " in input file (file id = ", ifile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF

  ret = pio_inq_vartype(ofile, ovdesc, otype)
  IF(ret /= PIO_NOERR) THEN
    WRITE(log_msg, *) "Unable to copy variable across files.",&
                      "Unable to query the type of the variable, ", TRIM(ovname),&
                      "(varid = ", ovdesc%varid, "),",&
                      " in output file (file id = ", ofile%fh, ")"
    ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
    IF(PRESENT(ierr)) THEN
      ierr = ret
    END IF
    RETURN
  END IF
  
  IF(itype .ne. otype) THEN
    WRITE(log_msg, *) "The types of the input variable, ", TRIM(ivname),&
                      "(varid = ", ivdesc%varid, "), in the input file",&
                      "(file id = ", ifile%fh, ") and ",&
                      "the output variable, ", TRIM(ovname),&
                      "(varid = ", ovdesc%varid, "), in the output file",&
                      "(file id = ", ofile%fh, ") do not match.",&
                      "Copying will result in coercing of the variable types"
    CALL pio_warn(ifile%iosystem, __PIO_FILE__, __LINE__, TRIM(log_msg))
  END IF

  SELECT CASE(itype)
    CASE(PIO_int)
      ALLOCATE(ival(varsz(1), varsz(2)))

      ret = pio_get_var(ifile, ivdesc%varid, ival)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to get the variable, ", TRIM(ivname),&
                          "(varid = ", ivdesc%varid, "),",&
                          " in the input file (file id = ", ifile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      ret = pio_put_var(ofile, ovdesc%varid, ival)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to put the variable, ", TRIM(ovname),&
                          "(varid = ", ovdesc%varid, "),",&
                          " in the output file (file id = ", ofile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      DEALLOCATE(ival)
    CASE(PIO_real)
      ALLOCATE(rval(varsz(1), varsz(2)))

      ret = pio_get_var(ifile, ivdesc%varid, rval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to get the variable, ", TRIM(ivname),&
                          "(varid = ", ivdesc%varid, "),",&
                          " in the input file (file id = ", ifile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      ret = pio_put_var(ofile, ovdesc%varid, rval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to put the variable, ", TRIM(ovname),&
                          "(varid = ", ovdesc%varid, "),",&
                          " in the output file (file id = ", ofile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      DEALLOCATE(rval)
    CASE(PIO_double)
      ALLOCATE(dval(varsz(1), varsz(2)))

      ret = pio_get_var(ifile, ivdesc%varid, dval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to get the variable, ", TRIM(ivname),&
                          "(varid = ", ivdesc%varid, "),",&
                          " in the input file (file id = ", ifile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      ret = pio_put_var(ofile, ovdesc%varid, dval)
      IF(ret /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Unable to copy variable across files.",&
                          "Unable to put the variable, ", TRIM(ovname),&
                          "(varid = ", ovdesc%varid, "),",&
                          " in the output file (file id = ", ofile%fh, ")"
        ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
        IF(PRESENT(ierr)) THEN
          ierr = ret
        END IF
        RETURN
      END IF

      DEALLOCATE(dval)
    CASE(PIO_char)
      WRITE(log_msg, *) "Unable to copy variable across files.",&
                        "Copying 2d string variables is currently not supported"
      ret = pio_error(ifile%iosystem, PIO_EINTERNAL, __PIO_FILE__, __LINE__, TRIM(log_msg))
      IF(PRESENT(ierr)) THEN
        ierr = ret
      END IF
      RETURN
  END SELECT

END SUBROUTINE copy_pio_var2d

END MODULE pio_nf_utils
