! Template of the program to find MPI Offset Fortran type
!
! The program tries to set a pointer (defined for a target
! of type INTEGER(KIND=<SPIO_PTR_TARGET_KIND>) or
! INTEGER *SPIO_PTR_TARGET_BYTES)
! to point to a target (of type
! INTEGER(KIND=<SPIO_ACTUAL_TARGET_KIND>) or
! INTEGER *<SPIO_ACTUAL_TARGET_BYTES>). The program fails
! to compile when the type sizes of the actual target
! (variable "off" below) and the destination of the pointer
! (what variable "ptr" below points to) are not compatible
! 
! The program needs to be built with
! "-DSPIO_USE_NON_STD_BYTE_FMT_FOR_ACTUAL_TARGET
!  -DSPIO_USE_NON_STD_BYTE_FMT_FOR_PTR_TARGET" if the actual
! and pointer target need to be defined using the "INTEGER *X"
! signature. If not, the "INTEGER (KIND=X)" signature is used.
!
! The following templated strings need to be replaced before
! compiling the code
! 1) If SPIO_USE_NON_STD_BYTE_FMT_FOR_ACTUAL_TARGET
!    is not defined replace <SPIO_ACTUAL_TARGET_KIND> with
!    the kind to test with (the kind for the target)
!    e.g. "MPI_OFFSET_KIND", "selected_int_kind(6)", ...
! 2) If SPIO_USE_NON_STD_BYTE_FMT_FOR_ACTUAL_TARGET
!    is defined replace <SPIO_ACTUAL_TARGET_BYTES> with
!    the number of bytes to use for the INTEGER. Note
!    that is NOT supported by the Fortran standard and
!    won't work with all compilers
!    e.g. "4", "8", ...
! 3) If SPIO_USE_NON_STD_BYTE_FMT_FOR_PTR_TARGET
!    is not defined replace <SPIO_PTR_TARGET_KIND> with
!    the kind for the pointer destination
!    e.g. "C_LONG_LONG", "C_LONG"
! 4) If SPIO_USE_NON_STD_BYTE_FMT_FOR_PTR_TARGET
!    is defined replace <SPIO_PTR_TARGET_BYTES> with
!    the number of bytes to use for the INTEGER. Note
!    that is NOT supported by the Fortran standard and
!    won't work with all compilers
!    e.g. "4", "8", ...
PROGRAM TryMPI_OFFSET_TYPE
  USE iso_c_binding
#ifndef NO_MPIMOD
  USE mpi, ONLY : MPI_OFFSET_KIND
  IMPLICIT NONE
#else
  IMPLICIT NONE
#include "mpif.h"
#endif

#ifdef SPIO_USE_NON_STD_BYTE_FMT_FOR_ACTUAL_TARGET
  INTEGER *<SPIO_ACTUAL_TARGET_BYTES>, TARGET :: off
#else
  INTEGER(KIND=<SPIO_ACTUAL_TARGET_KIND>), TARGET :: off
#endif

#ifdef SPIO_USE_NON_STD_BYTE_FMT_FOR_PTR_TARGET
  INTEGER *<SPIO_PTR_TARGET_BYTES>, POINTER :: ptr
#else
  INTEGER(KIND=<SPIO_PTR_TARGET_KIND>), POINTER :: ptr
#endif

  off = 1
  ptr => off
  !PRINT *, "Hello world: ", off
END PROGRAM
