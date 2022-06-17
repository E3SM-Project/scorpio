!> @file
!! @brief Miscellaneous SCORPIO APIs
!! This file contains the miscellaneous SCORPIO APIs
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_misc_api.F90'

MODULE spio_misc_api
  USE iso_c_binding
  USE pio_types, ONLY : file_desc_t, io_desc_t, var_desc_t
  USE pio_kinds, ONLY : PIO_OFFSET_KIND, PIO_OFFSET_F2C_TYPE_KIND
  USE spio_misc_api_cint
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_set_buffer_size_limit, pio_iotype_available

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

    INTEGER(PIO_OFFSET_F2C_TYPE_KIND) :: lim

    lim = PIOc_set_buffer_size_limit(INT(limit, PIO_OFFSET_F2C_TYPE_KIND))
    IF(PRESENT(prev_limit)) THEN
      prev_limit = INT(lim, PIO_OFFSET_KIND)
    END IF
  END SUBROUTINE pio_set_buffer_size_limit

!> @defgroup PIO_iotype_available PIO_iotype_available
!! @public
!! @brief Check if an iotype is available. @copydoc PIO_iotype
!!
!! @details
!! @param[in] The iotype to check for availability
!! @returns .TRUE. if iotype is available, .FALSE. otherwise
!!
  LOGICAL FUNCTION pio_iotype_available(iotype) RESULT(is_avail)
    INTEGER, INTENT(IN) :: iotype

    INTEGER(C_INT) :: cret

    cret = PIOc_iotype_available(INT(iotype, C_INT))
    IF(cret /= 0) THEN
      is_avail = .TRUE.
    ELSE
      is_avail = .FALSE.
    END IF

  END FUNCTION pio_iotype_available
END MODULE spio_misc_api
