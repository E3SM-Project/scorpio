!>
!! @file
!! @brief C Interfaces for miscellaneous SCORPIO APIs

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_misc_api_cint'

MODULE spio_misc_api_cint
  USE iso_c_binding
  USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Fortran interface to C function to set the upper limit
!! for the internal buffer used to cache data written by the user
!!
!! @param[in] limit The new upper limit (in bytes) of the internal
!! buffer used to cache data written out by the user
!! @returns Returns the previous buffer limit
  INTEGER(PIO_OFFSET_F2C_TYPE_KIND) FUNCTION PIOc_set_buffer_size_limit(limit)&
                          bind(C,name="PIOc_set_buffer_size_limit")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND

    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE :: limit
  END FUNCTION PIOc_set_buffer_size_limit
END INTERFACE

END MODULE spio_misc_api_cint
