!>
!! @file
!! @brief C Interfaces for miscellaneous SCORPIO APIs

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_misc_cint'

MODULE spio_misc_cint
  USE iso_c_binding
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Fortran interface to C function to set the upper limit
!! for the internal buffer used to cache data written by the user
!!
!! @param[in] limit The new upper limit (in bytes) of the internal
!! buffer used to cache data written out by the user
!! @returns Returns the previous buffer limit
  INTEGER(C_LONG_LONG) FUNCTION PIOc_set_buffer_size_limit(limit)&
                          bind(C,name="PIOc_set_buffer_size_limit")
    USE iso_c_binding
    INTEGER(C_LONG_LONG), VALUE :: limit
  END FUNCTION PIOc_set_buffer_size_limit
END INTERFACE

END MODULE spio_misc_cint
