!> @file
!! @brief Utilities functions
!! This file contains the utilities used by the Fortran Interface
!!

!> @internal
!! @def __PIO_FILE__
!! @brief This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_util.F90'

MODULE spio_util
  USE iso_c_binding
  USE pio_types, ONLY : iosystem_desc_t, file_desc_t,&
                        PIO_IOSYSID_INVALID, PIO_FH_INVALID, PIO_MAX_NAME,&
                        PIO_EINTERNAL, PIO_NOERR
  USE pio_kinds, ONLY : PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_err, ONLY  : pio_error
  USE spio_util_cint
#ifdef TIMING
  USE perf_mod, ONLY : t_startf, t_stopf   !_EXTERNAL
#endif
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: f2cstring, c2fstring, get_text_var_sz, get_var_dim_sz

!> @private
!! @defgroup f2cstring f2cstring
!! @brief Converts Fortran strings to C strings
!!
!! @details Convert Fortran strings to C strings (e.g. during "put"s and writes)
!! The converted Fortran strings are packed into a single buffer (@p cstr)
!!
!! @param[in] iosys The handle to the I/O system. @copydoc iosystem_desc_t
!! @param[out] fstr The fortran string (or array of Fortran strings) to be
!!                  converted
!! @param[in] cstr The converted C string (or multiple strings that are stored in
!!                  the same buffer) is returned via this arg
!! @param[in] cstr_sz The size of the C string buffer, @p cstr
!! @param[in] max_clen The maximum length of each string in the C string
!!                      buffer, @p cstr
!! @param[in] cstr_add_null If set to .TRUE. a C NULL character is added at the
!!                          end of each converted C string, if set to .FALSE. no
!!                          string delimiter is added at the end of each converted
!!                          C string
!! @returns The error code. @copydoc error_code
  INTERFACE f2cstring
    MODULE PROCEDURE f2cstring_0d
    MODULE PROCEDURE f2cstring_1d
    MODULE PROCEDURE f2cstring_2d
    MODULE PROCEDURE f2cstring_3d
    MODULE PROCEDURE f2cstring_4d
    MODULE PROCEDURE f2cstring_5d
  END INTERFACE

!> @private
!! @defgroup c2fstring c2fstring
!! @brief Converts C strings to Fortran strings
!!
!! @details Convert C strings from the C library to Fortran strings
!! (e.g. during "get"s and reads)
!! @param[in] iosys The handle to the I/O system. @copydoc iosystem_desc_t
!! @param[in] cstr The C string (or multiple strings that are stored in
!!                  a single buffer) to be converted to Fortran string(s)
!! @param[in] cstr_sz The total size of the C string buffer, @p cstr
!! @param[in] max_clen The maximum length of each string in the C string
!!                      buffer, @p cstr
!! @param[out] fstr The converted fortran string (or array of Fortran strings)
!!                  is returned via this arg
!! @returns The error code. @copydoc error_code
  INTERFACE c2fstring
    MODULE PROCEDURE c2fstring_0d
    MODULE PROCEDURE c2fstring_1d
    MODULE PROCEDURE c2fstring_2d
    MODULE PROCEDURE c2fstring_3d
    MODULE PROCEDURE c2fstring_4d
    MODULE PROCEDURE c2fstring_5d
  END INTERFACE

CONTAINS

!> @private
!! @defgroup f2cstring f2cstring
!! @brief Converts a Fortran string to a C string
!!
  INTEGER FUNCTION f2cstring_0d(iosys, fstr, cstr, cstr_sz, max_clen, cstr_add_null) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fstr
    CHARACTER(C_CHAR), INTENT(OUT) :: cstr(:)
    ! The total size of cstr, the total number  of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The max length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    LOGICAL, INTENT(IN), OPTIONAL :: cstr_add_null

    LOGICAL :: add_c_null = .true.
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i

    ierr = PIO_NOERR

    IF(PRESENT(cstr_add_null)) THEN
      add_c_null = cstr_add_null
    END IF

    max_nstrs = cstr_sz / max_clen
    IF(max_nstrs < 1) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying a string",&
                        " (space available to copy only ", max_nstrs, " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    cstr = C_NULL_CHAR
    cstr_idx = 0
    max_flen = LEN(fstr)
    DO i=1,MIN(max_clen, max_flen)
      cstr(i) = fstr(i:i)
    END DO
    IF(add_c_null .AND. (max_clen < max_flen + 1)) THEN
      ! We have exhausted the C write buffer
      ! C string = Fortran string chars + C_NULL_CHAR
      ! Warn the user of the truncation
      cstr(max_clen) = C_NULL_CHAR
      WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                        max_clen, " chars to write strings with ", max_flen, " chars).",&
                        "The contents will be truncated to : ", trim(fstr)
      CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
    END IF

  END FUNCTION f2cstring_0d

!> @private
!! @defgroup f2cstring f2cstring
!! @brief Converts a 1d array of Fortran strings to C strings
!! packed in a single buffer
!!
  INTEGER FUNCTION f2cstring_1d(iosys, fstr, cstr, cstr_sz, max_clen, cstr_add_null) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fstr(:)
    CHARACTER(C_CHAR), INTENT(OUT) :: cstr(:)
    ! The total size of cstr, the total number  of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The max length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    LOGICAL, INTENT(IN), OPTIONAL :: cstr_add_null

    LOGICAL :: add_c_null = .true.
    INTEGER, PARAMETER :: MAX_NDIMS = 1
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j

    ierr = PIO_NOERR

    IF(PRESENT(cstr_add_null)) THEN
      add_c_null = cstr_add_null
    END IF

    max_nstrs = cstr_sz / max_clen
    IF(SIZE(fstr) > max_nstrs) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying ", SIZE(fstr), "strings",&
                        " (space available to copy only ", max_nstrs, " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    cstr = C_NULL_CHAR
    cstr_idx = 0
    DO j=1,fstr_dim_sz(1)
      max_flen = LEN(fstr(j))
      DO i=1,MIN(max_clen, max_flen)
        cstr(cstr_idx * max_clen + i) = fstr(j)(i:i)
      END DO
      IF(add_c_null .AND. (max_clen < max_flen + 1)) THEN
        ! We have exhausted the C write buffer
        ! C string = Fortran string chars + C_NULL_CHAR
        ! Warn the user of the truncation
        cstr(cstr_idx * max_clen + max_clen) = C_NULL_CHAR
        WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                          max_clen, " chars to write strings with ", max_flen, " chars).",&
                          "The contents will be truncated to : ", trim(fstr(j)(1:max_clen-1))
        CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
      END IF
      cstr_idx = cstr_idx + 1
    END DO

  END FUNCTION f2cstring_1d

!> @private
!! @defgroup f2cstring f2cstring
!! @brief Converts a 2d array of Fortran strings to C strings
!! packed in a single buffer
!!
  INTEGER FUNCTION f2cstring_2d(iosys, fstr, cstr, cstr_sz, max_clen, cstr_add_null) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fstr(:,:)
    CHARACTER(C_CHAR), INTENT(OUT) :: cstr(:)
    ! The total size of cstr, the total number  of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The max length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    LOGICAL, INTENT(IN), OPTIONAL :: cstr_add_null

    LOGICAL :: add_c_null = .true.
    INTEGER, PARAMETER :: MAX_NDIMS = 2
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k

    ierr = PIO_NOERR

    IF(PRESENT(cstr_add_null)) THEN
      add_c_null = cstr_add_null
    END IF

    max_nstrs = cstr_sz / max_clen
    IF(SIZE(fstr) > max_nstrs) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying ", SIZE(fstr), "strings",&
                        " (space available to copy only ", max_nstrs, " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    cstr = C_NULL_CHAR
    cstr_idx = 0
    DO k=1,fstr_dim_sz(2)
      DO j=1,fstr_dim_sz(1)
        max_flen = LEN(fstr(j,k))
        DO i=1,MIN(max_clen, max_flen)
          cstr(cstr_idx * max_clen + i) = fstr(j,k)(i:i)
        END DO
        IF(add_c_null .AND. (max_clen < max_flen + 1)) THEN
          ! We have exhausted the C write buffer
          ! C string = Fortran string chars + C_NULL_CHAR
          ! Warn the user of the truncation
          cstr(cstr_idx * max_clen + max_clen) = C_NULL_CHAR
          WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                            max_clen, " chars to write strings with ", max_flen, " chars).",&
                            "The contents will be truncated to : ", trim(fstr(j,k)(1:max_clen-1))
          CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
        END IF
        cstr_idx = cstr_idx + 1
      END DO ! DO j=1,fstr_dim_sz(1)
    END DO ! DO k=1,fstr_dim_sz(2)

  END FUNCTION f2cstring_2d

!> @private
!! @defgroup f2cstring f2cstring
!! @brief Converts a 3d array of Fortran strings to C strings
!! packed in a single buffer
!!
  INTEGER FUNCTION f2cstring_3d(iosys, fstr, cstr, cstr_sz, max_clen, cstr_add_null) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fstr(:,:,:)
    CHARACTER(C_CHAR), INTENT(OUT) :: cstr(:)
    ! The total size of cstr, the total number  of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The max length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    LOGICAL, INTENT(IN), OPTIONAL :: cstr_add_null

    LOGICAL :: add_c_null = .true.
    INTEGER, PARAMETER :: MAX_NDIMS = 3
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k, m

    ierr = PIO_NOERR

    IF(PRESENT(cstr_add_null)) THEN
      add_c_null = cstr_add_null
    END IF

    max_nstrs = cstr_sz / max_clen
    IF(SIZE(fstr) > max_nstrs) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying ", SIZE(fstr), "strings",&
                        " (space available to copy only ", max_nstrs, " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    cstr = C_NULL_CHAR
    cstr_idx = 0
    DO m=1,fstr_dim_sz(3)
      DO k=1,fstr_dim_sz(2)
        DO j=1,fstr_dim_sz(1)
          max_flen = LEN(fstr(j,k,m))
          DO i=1,MIN(max_clen, max_flen)
            cstr(cstr_idx * max_clen + i) = fstr(j,k,m)(i:i)
          END DO
          IF(add_c_null .AND. (max_clen < max_flen + 1)) THEN
            ! We have exhausted the C write buffer
            ! C string = Fortran string chars + C_NULL_CHAR
            ! Warn the user of the truncation
            cstr(cstr_idx * max_clen + max_clen) = C_NULL_CHAR
            WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                              max_clen, " chars to write strings with ", max_flen, " chars).",&
                              "The contents will be truncated to : ", trim(fstr(j,k,m)(1:max_clen-1))
            CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
          END IF
          cstr_idx = cstr_idx + 1
        END DO ! DO j=1,fstr_dim_sz(1)
      END DO ! DO k=1,fstr_dim_sz(2)
    END DO ! DO m=1,fstr_dim_sz(3)

  END FUNCTION f2cstring_3d

!> @private
!! @defgroup f2cstring f2cstring
!! @brief Converts a 4d array of Fortran strings to C strings
!! packed in a single buffer
!!
  INTEGER FUNCTION f2cstring_4d(iosys, fstr, cstr, cstr_sz, max_clen, cstr_add_null) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fstr(:,:,:,:)
    CHARACTER(C_CHAR), INTENT(OUT) :: cstr(:)
    ! The total size of cstr, the total number  of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The max length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    LOGICAL, INTENT(IN), OPTIONAL :: cstr_add_null

    LOGICAL :: add_c_null = .true.
    INTEGER, PARAMETER :: MAX_NDIMS = 4
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k, m, n

    ierr = PIO_NOERR

    IF(PRESENT(cstr_add_null)) THEN
      add_c_null = cstr_add_null
    END IF

    max_nstrs = cstr_sz / max_clen
    IF(SIZE(fstr) > max_nstrs) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying ", SIZE(fstr), "strings",&
                        " (space available to copy only ", max_nstrs, " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    cstr = C_NULL_CHAR
    cstr_idx = 0
    DO n=1,fstr_dim_sz(4)
      DO m=1,fstr_dim_sz(3)
        DO k=1,fstr_dim_sz(2)
          DO j=1,fstr_dim_sz(1)
            max_flen = LEN(fstr(j,k,m,n))
            DO i=1,MIN(max_clen, max_flen)
              cstr(cstr_idx * max_clen + i) = fstr(j,k,m,n)(i:i)
            END DO
            IF(add_c_null .AND. (max_clen < max_flen + 1)) THEN
              ! We have exhausted the C write buffer
              ! C string = Fortran string chars + C_NULL_CHAR
              ! Warn the user of the truncation
              cstr(cstr_idx * max_clen + max_clen) = C_NULL_CHAR
              WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                                max_clen, " chars to write strings with ", max_flen, " chars).",&
                                "The contents will be truncated to : ",&
                                trim(fstr(j,k,m,n)(1:max_clen-1))
              CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
            END IF
            cstr_idx = cstr_idx + 1
          END DO ! DO j=1,fstr_dim_sz(1)
        END DO ! DO k=1,fstr_dim_sz(2)
      END DO ! DO m=1,fstr_dim_sz(3)
    END DO ! DO n=1,fstr_dim_sz(4)

  END FUNCTION f2cstring_4d

!> @private
!! @defgroup f2cstring f2cstring
!! @brief Converts a 5d array of Fortran strings to C strings
!! packed in a single buffer
!!
  INTEGER FUNCTION f2cstring_5d(iosys, fstr, cstr, cstr_sz, max_clen, cstr_add_null) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(LEN=*), INTENT(IN) :: fstr(:,:,:,:,:)
    CHARACTER(C_CHAR), INTENT(OUT) :: cstr(:)
    ! The total size of cstr, the total number  of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The max length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    LOGICAL, INTENT(IN), OPTIONAL :: cstr_add_null

    LOGICAL :: add_c_null = .true.
    INTEGER, PARAMETER :: MAX_NDIMS = 5
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k, m, n, q

    ierr = PIO_NOERR

    IF(PRESENT(cstr_add_null)) THEN
      add_c_null = cstr_add_null
    END IF

    max_nstrs = cstr_sz / max_clen
    IF(SIZE(fstr) > max_nstrs) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying ", SIZE(fstr), "strings",&
                        " (space available to copy only ", max_nstrs, " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    cstr = C_NULL_CHAR
    cstr_idx = 0
    DO q=1,fstr_dim_sz(5)
      DO n=1,fstr_dim_sz(4)
        DO m=1,fstr_dim_sz(3)
          DO k=1,fstr_dim_sz(2)
            DO j=1,fstr_dim_sz(1)
              max_flen = LEN(fstr(j,k,m,n,q))
              DO i=1,MIN(max_clen, max_flen)
                cstr(cstr_idx * max_clen + i) = fstr(j,k,m,n,q)(i:i)
              END DO
              IF(add_c_null .AND. (max_clen < max_flen + 1)) THEN
                ! We have exhausted the C write buffer
                ! C string = Fortran string chars + C_NULL_CHAR
                ! Warn the user of the truncation
                cstr(cstr_idx * max_clen + max_clen) = C_NULL_CHAR
                WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                                  max_clen, " chars to write strings with ", max_flen, " chars).",&
                                  "The contents will be truncated to : ",&
                                  trim(fstr(j,k,m,n,q)(1:max_clen-1))
                CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
              END IF
              cstr_idx = cstr_idx + 1
            END DO ! DO j=1,fstr_dim_sz(1)
          END DO ! DO k=1,fstr_dim_sz(2)
        END DO ! DO m=1,fstr_dim_sz(3)
      END DO ! DO n=1,fstr_dim_sz(4)
    END DO ! DO q=1,fstr_dim_sz(5)

  END FUNCTION f2cstring_5d

!> @private
!! @ingroup c2fstring
!! @brief Converts a single C string to a Fortran string
!!
  INTEGER FUNCTION c2fstring_0d(iosys, cstr, cstr_sz, max_clen, fstr) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(C_CHAR), INTENT(IN) :: cstr(:)
    ! The total size of cstr, the total number of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    CHARACTER(LEN=*), INTENT(OUT) :: fstr

    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    LOGICAL :: cstr_copied
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i

    CHARACTER, PARAMETER :: F_SPACE_CHAR = ' '

    ierr = PIO_NOERR

    max_nstrs = cstr_sz / max_clen
    IF(max_nstrs > 1) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have enough space for copying ", max_nstrs, "strings",&
                        " (space only available to copy 1 string)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF
    fstr = F_SPACE_CHAR
    cstr_idx = 0
    max_flen= LEN(fstr)
    cstr_copied = .FALSE.
    DO i=1,MIN(max_clen, max_flen)
      fstr(i:i) = cstr(i)
      IF(fstr(i:i) == C_NULL_CHAR) THEN
        fstr(i:i) = F_SPACE_CHAR
        cstr_copied = .TRUE.
        EXIT
      END IF
    END DO
    IF((.NOT. cstr_copied) .AND. (max_flen < max_clen)) THEN
      ! We have exhausted the Fortran write buffer
      ! Warn the user of the truncation
      WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                        max_flen, " chars to read string with ", max_clen, " chars).",&
                        "The contents will be truncated to : ", trim(fstr)
      CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
      fstr(max_flen:max_flen) = F_SPACE_CHAR
    END IF
  END FUNCTION c2fstring_0d

!> @private
!! @ingroup c2fstring
!! @brief Converts a buffer containing an array of C strings to
!! a 1d array of Fortran strings
!!
  INTEGER FUNCTION c2fstring_1d(iosys, cstr, cstr_sz, max_clen, fstr) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(C_CHAR), INTENT(IN) :: cstr(:)
    ! The total size of cstr, the total number of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    CHARACTER(LEN=*), INTENT(OUT) :: fstr(:)

    INTEGER, PARAMETER :: MAX_NDIMS = 1
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    LOGICAL :: cstr_copied
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j

    CHARACTER, PARAMETER :: F_SPACE_CHAR = ' '

    ierr = PIO_NOERR

    max_nstrs = cstr_sz / max_clen
    IF(max_nstrs > SIZE(fstr)) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have enough space for copying ", max_nstrs, "strings",&
                        " (space only available to copy ", SIZE(fstr), " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    fstr_dim_sz(1) = SIZE(fstr,1)

    fstr = F_SPACE_CHAR
    cstr_idx = 0
    max_flen= LEN(fstr(1))
    DO j=1,fstr_dim_sz(1)
      cstr_copied = .FALSE.
      IF(cstr_idx * max_clen + MIN(max_clen, max_flen) > cstr_sz) THEN
        ! This is just a warning since the user might have supplied a large Fortran
        ! output array
        WRITE(log_msg, *) "WARNING : User buffer is larger than data. ",&
                          "The number of strings read/converted (", cstr_idx, ") ",&
                          "is less than the number of expected strings in the ",&
                          "Fortran array (", fstr_dim_sz(1), " strings) "
        CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
        EXIT
      END IF
      DO i=1,MIN(max_clen, max_flen)
        fstr(j)(i:i) = cstr(cstr_idx * max_clen + i)
        IF(fstr(j)(i:i) == C_NULL_CHAR) THEN
          fstr(j)(i:i) = F_SPACE_CHAR
          cstr_copied = .TRUE.
          EXIT
        END IF
      END DO
      IF((.NOT. cstr_copied) .AND. (max_flen < max_clen)) THEN
        ! We have exhausted the Fortran write buffer
        ! Warn the user of the truncation
        WRITE(log_msg, *) "WARNING: The user buffer has insufficient space (",&
                          max_flen, " chars to read strings with ", max_clen, " chars).",&
                          "The contents will be truncated to : ", trim(fstr(j))
        CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
        fstr(j)(max_flen:max_flen) = F_SPACE_CHAR
      END IF
      cstr_idx = cstr_idx + 1
    END DO

  END FUNCTION c2fstring_1d

!> @private
!! @ingroup c2fstring
!! @brief Converts a buffer containing an array of C strings to
!! a 2d array of Fortran strings
!!
  INTEGER FUNCTION c2fstring_2d(iosys, cstr, cstr_sz, max_clen, fstr) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(C_CHAR), INTENT(IN) :: cstr(:)
    ! The total size of cstr, the total number of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    CHARACTER(LEN=*), INTENT(OUT) :: fstr(:,:)

    INTEGER, PARAMETER :: MAX_NDIMS = 2
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    LOGICAL :: cstr_copied
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k

    CHARACTER, PARAMETER :: F_SPACE_CHAR = ' '

    ierr = PIO_NOERR

    max_nstrs = cstr_sz / max_clen
    IF(max_nstrs > SIZE(fstr)) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying ", max_nstrs, " strings",&
                        " (space available to copy only ", SIZE(fstr), " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    fstr = F_SPACE_CHAR
    cstr_idx = 0
    max_flen= LEN(fstr(1,1))
    DO k=1,fstr_dim_sz(2)
      DO j=1,fstr_dim_sz(1)
        cstr_copied = .FALSE.
        IF(cstr_idx * max_clen + MIN(max_clen, max_flen) > cstr_sz) THEN
          WRITE(log_msg, *) "WARNING: User buffer is larger than the data. ",&
                            "The number of strings read (", cstr_idx, ") ",&
                            "is less than the number of expected strings in the ",&
                            "Fortran array (", fstr_dim_sz(1) * fstr_dim_sz(2), " strings) "
          CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
          EXIT
        END IF
        DO i=1,MIN(max_clen, max_flen)
          fstr(j,k)(i:i) = cstr(cstr_idx * max_clen + i)
          IF(fstr(j,k)(i:i) == C_NULL_CHAR) THEN
            fstr(j,k)(i:i) = F_SPACE_CHAR
            cstr_copied = .TRUE.
            EXIT
          END IF
        END DO
        IF((.NOT. cstr_copied) .AND. (max_flen < max_clen)) THEN
          ! We have exhausted the Fortran write buffer
          ! Warn the user of the truncation
          WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                            max_flen, " chars to read strings with ", max_clen, " chars).",&
                            "The contents will be truncated to : ", trim(fstr(j,k))
          CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
          fstr(j,k)(max_flen:max_flen) = F_SPACE_CHAR
        END IF
        cstr_idx = cstr_idx + 1
      END DO ! DO j=1,fstr_dim_sz(1)
    END DO ! DO k=1,fstr_dim_sz(2)

  END FUNCTION c2fstring_2d

!> @private
!! @ingroup c2fstring
!! @brief Converts a buffer containing an array of C strings to
!! a 3d array of Fortran strings
!!
  INTEGER FUNCTION c2fstring_3d(iosys, cstr, cstr_sz, max_clen, fstr) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(C_CHAR), INTENT(IN) :: cstr(:)
    ! The total size of cstr, the total number of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    CHARACTER(LEN=*), INTENT(OUT) :: fstr(:,:,:)

    INTEGER, PARAMETER :: MAX_NDIMS = 3
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    LOGICAL :: cstr_copied
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k, m

    CHARACTER, PARAMETER :: F_SPACE_CHAR = ' '

    ierr = PIO_NOERR

    max_nstrs = cstr_sz / max_clen
    IF(max_nstrs > SIZE(fstr)) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does not have space for copying ", max_nstrs, "strings",&
                        " (space available to copy only ", SIZE(fstr), " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    fstr = F_SPACE_CHAR
    cstr_idx = 0

    max_flen= LEN(fstr(1,1,1))
    DO m=1,fstr_dim_sz(3)
      DO k=1,fstr_dim_sz(2)
        DO j=1,fstr_dim_sz(1)
          cstr_copied = .FALSE.
          IF(cstr_idx * max_clen + MIN(max_clen, max_flen) > cstr_sz) THEN
            WRITE(log_msg, *) "WARNING: The user buffer is larger than the data. ",&
                              "The number of strings read (", cstr_idx, ") ",&
                              "is less than the number of expected strings in the ",&
                              "Fortran array (",&
                              fstr_dim_sz(1) * fstr_dim_sz(2) * fstr_dim_sz(3), " strings) "
            CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
            EXIT
          END IF
          DO i=1,MIN(max_clen, max_flen)
            fstr(j,k,m)(i:i) = cstr(cstr_idx * max_clen + i)
            IF(fstr(j,k,m)(i:i) == C_NULL_CHAR) THEN
              fstr(j,k,m)(i:i) = F_SPACE_CHAR
              cstr_copied = .TRUE.
              EXIT
            END IF
          END DO
          IF((.NOT. cstr_copied) .AND. (max_flen < max_clen)) THEN
            ! We have exhausted the Fortran write buffer
            ! Warn the user of the truncation
            WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                              max_flen, " chars to read strings with ", max_clen, " chars).",&
                              "The contents will be truncated to : ", trim(fstr(j,k,m))
            CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
            fstr(j,k,m)(max_flen:max_flen) = F_SPACE_CHAR
          END IF
          cstr_idx = cstr_idx + 1
        END DO ! DO j=1,fstr_dim_sz(1)
      END DO ! DO k=1,fstr_dim_sz(2)
    END DO ! DO m=1,fstr_dim_sz(3)

  END FUNCTION c2fstring_3d

!> @private
!! @ingroup c2fstring
!! @brief Converts a buffer containing an array of C strings to
!! a 4d array of Fortran strings
!!
  INTEGER FUNCTION c2fstring_4d(iosys, cstr, cstr_sz, max_clen, fstr) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(C_CHAR), INTENT(IN) :: cstr(:)
    ! The total size of cstr, the total number of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    CHARACTER(LEN=*), INTENT(OUT) :: fstr(:,:,:,:)

    INTEGER, PARAMETER :: MAX_NDIMS = 4
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    LOGICAL :: cstr_copied
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k, m, n

    CHARACTER, PARAMETER :: F_SPACE_CHAR = ' '

    max_nstrs = cstr_sz / max_clen
    IF(max_nstrs > SIZE(fstr)) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does.NOT.have space for copying ", max_nstrs, "strings",&
                        " (space available to copy only ", SIZE(fstr), " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    fstr = F_SPACE_CHAR
    cstr_idx = 0

    max_flen= LEN(fstr(1,1,1,1))
    DO n=1,fstr_dim_sz(4)
      DO m=1,fstr_dim_sz(3)
        DO k=1,fstr_dim_sz(2)
          DO j=1,fstr_dim_sz(1)
            cstr_copied = .FALSE.
            IF(cstr_idx * max_clen + MIN(max_clen, max_flen) > cstr_sz) THEN
              WRITE(log_msg, *) "WARNING: The user buffer is larger than the data. ",&
                                "The number of strings read (", cstr_idx, ") ",&
                                "is less than the number of expected strings in the ",&
                                "Fortran array (",&
                                fstr_dim_sz(1) * fstr_dim_sz(2) * fstr_dim_sz(3) * fstr_dim_sz(4),&
                                " strings) "
              CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
              EXIT
            END IF
            DO i=1,MIN(max_clen, max_flen)
              fstr(j,k,m,n)(i:i) = cstr(cstr_idx * max_clen + i)
              IF(fstr(j,k,m,n)(i:i) == C_NULL_CHAR) THEN
                fstr(j,k,m,n)(i:i) = F_SPACE_CHAR
                cstr_copied = .TRUE.
                EXIT
              END IF
            END DO
            IF((.NOT. cstr_copied) .AND. (max_flen < max_clen)) THEN
              ! We have exhausted the Fortran write buffer
              ! Warn the user of the truncation
              WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                                max_flen, " chars to read strings with ", max_clen, " chars).",&
                                "The contents will be truncated to : ", trim(fstr(j,k,m,n))
              CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
              fstr(j,k,m,n)(max_flen:max_flen) = F_SPACE_CHAR
            END IF
            cstr_idx = cstr_idx + 1
          END DO ! DO j=1,fstr_dim_sz(1)
        END DO ! DO k=1,fstr_dim_sz(2)
      END DO ! DO m=1,fstr_dim_sz(3)
    END DO ! DO n=1,fstr_dim_sz(4)

  END FUNCTION c2fstring_4d

!> @private
!! @ingroup c2fstring
!! @brief Converts a buffer containing an array of C strings to
!! a 5d array of Fortran strings
!!
  INTEGER FUNCTION c2fstring_5d(iosys, cstr, cstr_sz, max_clen, fstr) RESULT(ierr)
    TYPE(iosystem_desc_t), INTENT(IN) :: iosys
    CHARACTER(C_CHAR), INTENT(IN) :: cstr(:)
    ! The total size of cstr, the total number of chars in cstr
    INTEGER, INTENT(IN) :: cstr_sz
    ! The length of each string in cstr
    INTEGER, INTENT(IN) :: max_clen
    CHARACTER(LEN=*), INTENT(OUT) :: fstr(:,:,:,:,:)

    INTEGER, PARAMETER :: MAX_NDIMS = 5
    INTEGER :: fstr_dim_sz(MAX_NDIMS)
    INTEGER :: max_nstrs, max_flen
    INTEGER :: cstr_idx
    LOGICAL :: cstr_copied
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: ret
    INTEGER :: i, j, k, m, n, q

    CHARACTER, PARAMETER :: F_SPACE_CHAR = ' '

    max_nstrs = cstr_sz / max_clen
    IF(max_nstrs > SIZE(fstr)) THEN
      WRITE(log_msg, *) "ERROR: The provided user buffer",&
                        " does.NOT.have space for copying ", max_nstrs, "strings",&
                        " (space available to copy only ", SIZE(fstr), " strings)"
      ret = pio_error(iosys, PIO_EINTERNAL, __PIO_FILE__, __LINE__, trim(log_msg))
      ierr = INT(ret)
      RETURN
    END IF

    DO i=1,MAX_NDIMS
      fstr_dim_sz(i) = SIZE(fstr,i)
    END DO

    fstr = F_SPACE_CHAR
    cstr_idx = 0

    max_flen= LEN(fstr(1,1,1,1,1))
    DO q=1,fstr_dim_sz(5)
      DO n=1,fstr_dim_sz(4)
        DO m=1,fstr_dim_sz(3)
          DO k=1,fstr_dim_sz(2)
            DO j=1,fstr_dim_sz(1)
              cstr_copied = .FALSE.
              IF(cstr_idx * max_clen + MIN(max_clen, max_flen) > cstr_sz) THEN
                WRITE(log_msg, *) "WARNING: The user buffer is larger than the data. ",&
                                  "The number of strings read (", cstr_idx, ") ",&
                                  "is less than the number of expected strings in the ",&
                                  "Fortran array (",&
                                  fstr_dim_sz(1) * fstr_dim_sz(2) * fstr_dim_sz(3) * fstr_dim_sz(4) * fstr_dim_sz(5),&
                                  " strings) "
                CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
                EXIT
              END IF
              DO i=1,MIN(max_clen, max_flen)
                fstr(j,k,m,n,q)(i:i) = cstr(cstr_idx * max_clen + i)
                IF(fstr(j,k,m,n,q)(i:i) == C_NULL_CHAR) THEN
                  fstr(j,k,m,n,q)(i:i) = F_SPACE_CHAR
                  cstr_copied = .TRUE.
                  EXIT
                END IF
              END DO
              IF((.NOT. cstr_copied) .AND. (max_flen < max_clen)) THEN
                ! We have exhausted the Fortran write buffer
                ! Warn the user of the truncation
                WRITE(log_msg, *) "WARNING: The user buffer provided had insufficient space (",&
                                  max_flen, " chars to read strings with ", max_clen, " chars).",&
                                  "The contents will be truncated to : ", trim(fstr(j,k,m,n,q))
                CALL pio_warn(iosys, __PIO_FILE__, __LINE__, trim(log_msg))
                fstr(j,k,m,n,q)(max_flen:max_flen) = F_SPACE_CHAR
              END IF
              cstr_idx = cstr_idx + 1
            END DO ! DO j=1,fstr_dim_sz(1)
          END DO ! DO k=1,fstr_dim_sz(2)
        END DO ! DO m=1,fstr_dim_sz(3)
      END DO ! DO n=1,fstr_dim_sz(4)
    END DO ! DO q=1,fstr_dim_sz(5)

  END FUNCTION c2fstring_5d

!>
!! @private
!! @brief Get the size of a text/character variable
!! (Each element of a text variable is a string)
!! @details
!! @param file  The handle to the file containing the variable
!! @param varid Id of the queried variable
!! @param var_slen  The length of each string in the variable is
!!                    returned in this arg
!! @param var_nstrs (Optional) The number of strings in the variable is
!!                    returned in this arg
!! @param var_dim_sz  (Optional) The dimension sizes of the variable is
!!                      returned in this arg
!! @returns @copydoc error_code
!!
  INTEGER FUNCTION get_text_var_sz(file, varid, var_slen,&
                                    var_nstrs, var_dim_sz) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER, INTENT(OUT) :: var_slen
    INTEGER, INTENT(OUT), OPTIONAL :: var_nstrs
    INTEGER, INTENT(OUT), OPTIONAL :: var_dim_sz(:)

    INTEGER :: ndims = 0
    INTEGER, ALLOCATABLE :: dimids(:)
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: dim_sz
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER :: i
    INTEGER(C_INT) :: cerr

    ierr = PIO_NOERR

    IF(PRESENT(var_nstrs)) THEN
      var_nstrs = 0
    END IF

    ! Get the number of dimensions in variable
    cerr = PIOc_inq_varndims(file%fh, varid-1, ndims)
    IF(cerr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Inquiring number of dims of variable failed, fh = ", file%fh,&
                        ", varid = ", varid
      cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
      ierr = INT(cerr)
      RETURN
    END IF
    IF(ndims == 0) THEN
      ! A scalar character variable
      var_slen = 1
      ! Number of strings = 0, so leave it at the default value
      ierr = PIO_NOERR
      RETURN
    END IF
    ! Get the dimension ids for the variable
    ALLOCATE(dimids(ndims))
    cerr = PIOc_inq_vardimid(file%fh, varid-1, dimids)
    IF(cerr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Inquiring the variable dimension ids for the text variable failed,",&
                        "fh = ", file%fh, ", varid = ", varid
      cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
      ierr = INT(cerr)
      RETURN
    END IF

    ! Get the size of dimension 1
    ! The dimension 1 of a text variable is the length
    ! of each string in the variable i.e., var_slen
    ! Note: The dimension ids used here were retrieved using
    !   the C interface FUNCTION, hence does not require
    !   Fortran to C conversion (to pass to a C function)
    !   i.e., using dimids(i) instead of dimids(i) - 1
    !   However the dimids are in the C order, so we
    !   need to use the reverse array for the Fortran
    !   dimension (when using it in Fortran)
    !   i.e., using dimids(ndims - i + 1) instead of dimids(i)
    cerr = PIOc_inq_dimlen(file%fh, dimids(ndims), dim_sz)
    IF(cerr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Inquiring the lengths of 0th dimension for the text variable failed,",&
                        "fh = ", file%fh, ", varid = ", varid
      cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
      ierr = INT(cerr)
      RETURN
    END IF

    var_slen = INT(dim_sz)
    IF(PRESENT(var_nstrs)) THEN
      var_nstrs = 1
    END IF

    IF(ndims > 1) THEN
      IF(PRESENT(var_dim_sz)) THEN
        IF(SIZE(var_dim_sz) < ndims - 1) THEN
          WRITE(log_msg, *) "Not enough space to copy back the dimension sizes of the text variable,",&
                            "fh = ", file%fh, ", varid = ", varid
          cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
          ierr = INT(cerr)
          RETURN
        END IF
      END IF

      ! Get the size of the other dimensions
      ! These dimensions determine the size of the string array
      DO i=2,ndims
        cerr = PIOc_inq_dimlen(file%fh, dimids(ndims - i + 1), dim_sz)
        IF(cerr /= PIO_NOERR) THEN
          WRITE(log_msg, *) "Inquiring the lengths of dimension", i, " for the text variable failed.",&
                            "fh = ", file%fh, ", varid = ", varid
          cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
          ierr = INT(cerr)
          RETURN
        END IF

        IF(PRESENT(var_dim_sz)) THEN
          var_dim_sz(i-1) = INT(dim_sz)
        END IF
        IF(PRESENT(var_nstrs)) THEN
          var_nstrs = var_nstrs * INT(dim_sz)
        END IF
      END DO
    END IF
    DEALLOCATE(dimids)

  END FUNCTION get_text_var_sz

!> @private
!! @brief Get the dim size of a variable
!!
!! @details
!! @param file  The handle to the file containing the variable
!! @param varid Id of the queried variable
!! @param var_sz  The total size of the variable is
!!                      returned in this arg
!! @param var_dim_sz  (Optional) The dimension sizes of the variable is
!!                      returned in this arg
!! @returns @copydoc error_code
!!
  INTEGER FUNCTION get_var_dim_sz(file, varid, var_sz, var_dim_sz) RESULT(ierr)
    TYPE(file_desc_t), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: varid
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT) :: var_sz
    INTEGER(PIO_OFFSET_KIND), INTENT(INOUT), ALLOCATABLE, OPTIONAL :: var_dim_sz(:)

    INTEGER :: ndims = 0
    INTEGER, ALLOCATABLE :: dimids(:)
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), ALLOCATABLE :: dim_sz(:)
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER :: i
    INTEGER(C_INT) :: cerr

    ierr = PIO_NOERR

    var_sz = 0
    ! Get the number of dimensions in variable
    cerr = PIOc_inq_varndims(file%fh, varid-1, ndims)
    IF(cerr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "ERROR: Inquiring number of dims of variable failed.",&
                        "fh = ", file%fh, ", varid = ", varid
      cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
      ierr = INT(cerr)
      RETURN
    END IF
    IF(ndims == 0) THEN
      ! A scalar variable
      var_sz = 1

      IF(PRESENT(var_dim_sz)) THEN
        ALLOCATE(var_dim_sz(1))
        var_dim_sz(1) = 1
      END IF

      ierr = PIO_NOERR
      RETURN
    END IF

    ! Get the dimension ids for the variable
    ALLOCATE(dimids(ndims))
    cerr = PIOc_inq_vardimid(file%fh, varid-1, dimids)
    IF(cerr /= PIO_NOERR) THEN
      WRITE(log_msg, *) "Inquiring the variable dimension ids for the variable failed,",&
                        "fh = ", file%fh, ", varid = ", varid
      cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
      ierr = INT(cerr)
      RETURN
    END IF

    ALLOCATE(dim_sz(ndims))
    IF(PRESENT(var_dim_sz)) THEN
      ALLOCATE(var_dim_sz(ndims))
    END IF
    var_sz = 1
    DO i=1,ndims
      cerr = PIOc_inq_dimlen(file%fh, dimids(i), dim_sz(i))
      IF(cerr /= PIO_NOERR) THEN
        WRITE(log_msg, *) "Inquiring the lengths of dimension for the variable failed,",&
                          "fh = ", file%fh, ", varid = ", varid
        cerr = pio_error(file%iosystem, cerr, __PIO_FILE__, __LINE__, TRIM(log_msg))
        ierr = INT(cerr)
        RETURN
      END IF
      var_sz = var_sz * INT(dim_sz(i), PIO_OFFSET_KIND)
      IF(PRESENT(var_dim_sz)) THEN
        var_dim_sz(ndims - i + 1) = INT(dim_sz(i), PIO_OFFSET_KIND)
      END IF
    END DO

    DEALLOCATE(dim_sz)
    DEALLOCATE(dimids)

  END FUNCTION get_var_dim_sz
END MODULE spio_util
