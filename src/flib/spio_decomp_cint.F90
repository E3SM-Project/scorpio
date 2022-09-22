!>
!! @file
!! @brief C Interfaces for SCORPIO APIs to initialize/finalize
!! an I/O subsystem

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_decomp_cint'

MODULE spio_decomp_cint
  USE iso_c_binding
  USE pio_types, ONLY : iosystem_desc_t, io_desc_t, file_desc_t
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Initialize an I/O decomposition
!!
!! @details
!! @param[in] iosysid The handle to the I/O subsystem associated with
!!                    the I/O decomposition.
!! @param[in] dt    The type of the data in memory. Note that users
!!                  can use an I/O decomposition representing data in
!!                  memory with type T1 (e.g. real(r8)) to write data
!!                  for a variable with a different type, say T2
!!                  (e.g. real(r4)) as long as T1 can be safely
!!                  typecasted to T2. @copydoc PIO_kinds
!! @param[in] ndims The number of dimensions of the data represented
!!                  by this I/O decomposition
!! @param[in] gdims The global dimensions of the I/O decomposition. The
!!                  array is of size @p ndims and the dimensions are
!!                  specified in C order (fastest changing index is
!!                  the rightmost index)
!! @param[in] mapsz The size of the @p map that represents mapping
!!                  between the local data index and the global linearized
!!                  index for the local data in the I/O decomposition
!! @param[in] map   The mapping between local data index and the global
!!                  linearized index for each local data element
!!                  e.g. If after partitioning the local array contains
!!                  A(2,3) and A(3,3) of an array A[4x4], the map needs
!!                  to contain [10, 11] corrsponding to the linearized
!!                  indices of A(2,3) & A(3,3)
!! @param[out] pioid A pointer to the I/O decomposition id. The handle
!!                   to the I/O decomposition is returned via this arg.
!! @param[in] prearr  A pointer to the data rearranger to use for this I/O
!!                    decomposition. Specify C_NULL_PTR if not needed/used.
!!                    @copydoc PIO_rearr_method
!! @param[in] pstart Pointer to the start array containing the local
!!                    start index for all the dimensions. i.e., start(i)
!!                    contains the starting index for data in the current
!!                    process for dimension i and size(start) = number of
!!                    dimensions for the data. The start array is passed
!!                    in for trivial (e.g. block cyclic) decompositions.
!!                    The library internally calculates this value if
!!                    not provided by the user, so pass C_NULL_PTR for
!!                    cases where the start array is not available
!! @param[in] pcnt  Pointer to the count array containing the local
!!                  count for all the dimensions. i.e., count(i) contains
!!                  the number of elements of data in the current
!!                  process for dimension i and size(count) = number of
!!                  dimensions for the data. The count array is passed
!!                  in for trivial (e.g. block cyclic) decompositions.
!!                    The library internally calculates this value if
!!                    not provided by the user, so pass C_NULL_PTR for
!!                    cases where the start array is not available
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_InitDecomp(iosysid, dt, ndims,&
                            gdims, mapsz, map, pioid,&
                            prearr, pstart, pcnt)&
                          bind(C,name="PIOc_InitDecomp")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: dt
    INTEGER(C_INT), VALUE :: ndims
    TYPE(C_PTR), VALUE :: gdims
    INTEGER(C_INT), VALUE :: mapsz
    TYPE(C_PTR), VALUE :: map
    TYPE(C_PTR), VALUE :: pioid
    TYPE(C_PTR), VALUE :: prearr
    TYPE(C_PTR), VALUE :: pstart
    TYPE(C_PTR), VALUE :: pcnt
  END FUNCTION PIOc_InitDecomp
END INTERFACE

INTERFACE
!> @private
!! @brief Free/Finalize an I/O decomposition
!!
!! @details
!! @param[in] iosysid The handle to the I/O subsystem associated with
!!                    the I/O decomposition.
!! @param[out] ioid The I/O decomposition id for the decomposition to free
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_freedecomp(iosysid, ioid)&
                          bind(C,name="PIOc_freedecomp")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: ioid
  END FUNCTION PIOc_freedecomp
END INTERFACE

INTERFACE
!> @private
!! @brief Get local array/data size for an I/O decomposition
!!
!! @details
!! @param[out] ioid The I/O decomposition id/handle for the decomposition
!! @returns The local array/data size for the I/O decomposition
  INTEGER(C_INT) FUNCTION PIOc_get_local_array_size(ioid)&
                          bind(C,name="PIOc_get_local_array_size")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: ioid
  END FUNCTION PIOc_get_local_array_size
END INTERFACE

INTERFACE
!> @private
!! @brief Synchronize contents of a file to the filesystem (all data written
!! out to this file is flushed to the filesystem)
!!
!! @details
!! @param[in] fh The handle/id to the file
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_sync(fh)&
                          bind(C,name="PIOc_sync")
    USE iso_c_binding
    INTEGER(C_INT), VALUE :: fh
  END FUNCTION PIOc_sync
END INTERFACE

END MODULE spio_decomp_cint
