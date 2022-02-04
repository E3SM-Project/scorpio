!> @file
!! @brief Miscellaneous SCORPIO APIs
!! This file contains the miscellaneous SCORPIO APIs
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_misc.F90'

MODULE spio_misc
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t
  USE pio_kinds, ONLY : i4, r4, r8, pio_offset_kind
  USE spio_misc_cint
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_set_buffer_size_limit

CONTAINS

!> @defgroup pio_set_buffer_size_limit pio_set_buffer_size_limit
!! @public
!! @brief Sets the limit for the internal buffer used to cache data written
!! by the user
!! @details
!! @param[in] limit The new buffer limit (in bytes)
!! @param[out] prev_limit Optional parameter that returns the
!! previous/old buffer limit (in bytes)
!!
  SUBROUTINE pio_set_buffer_size_limit(limit, prev_limit)
    INTEGER(PIO_OFFSET_KIND), INTENT(IN) :: limit
    INTEGER(PIO_OFFSET_KIND), INTENT(OUT), OPTIONAL :: prev_limit

    INTEGER(PIO_OFFSET_KIND) :: lim

    lim = PIOc_set_buffer_size_limit(limit)
    IF(PRESENT(prev_limit)) THEN
      prev_limit = lim
    END IF
  END SUBROUTINE pio_set_buffer_size_limit
END MODULE spio_misc
