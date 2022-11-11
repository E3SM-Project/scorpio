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
!! @brief Initialize a block cyclic I/O decomposition
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
!! @param[in] pstart  Pointer to the start index array for the I/O
!!                    decomposition map for the block cyclic decomposition.
!!                    Each element in the array corresponds to the start
!!                    index for the I/O decomposition  (pstart[i]
!!                    contains the start index for dimension i in the
!!                    current process). The size of this array needs
!!                    to be equal to the number of dimensions of the
!!                    variables/data represented by this I/O decomposition.
!!                    The elements in the array are in the C order of
!!                    variable dimension (rightmost fastest varying)
!! @param[in] pcount  Pointer to the count array for the I/O decomposition
!!                    map for the block cyclic decomposition. Each
!!                    element in the array corresponds to the number of
!!                    elements for the I/O decomposition  (pcount[i]
!!                    contains the number of elements for dimension i in
!!                    the current process). The size of this array needs
!!                    to be equal to the number of dimensions of the
!!                    variables/data represented by this I/O decomposition
!!                    The elements in the array are in the C order of
!!                    variable dimension (rightmost fastest varying)
!! @param[out] pioid A pointer to the I/O decomposition id. The handle
!!                   to the I/O decomposition is returned via this arg.
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_InitDecomp_bc(iosysid, dt, ndims,&
                            gdims, pstart, pcount, pioid)&
                          bind(C,name="PIOc_InitDecomp_bc")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: iosysid
    INTEGER(C_INT), VALUE :: dt
    INTEGER(C_INT), VALUE :: ndims
    TYPE(C_PTR), VALUE :: gdims
    TYPE(C_PTR), VALUE :: pstart
    TYPE(C_PTR), VALUE :: pcount
    TYPE(C_PTR), VALUE :: pioid
  END FUNCTION PIOc_InitDecomp_bc
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

INTERFACE
!! @public
!! @brief Read an I/O decomposition from a file
!!
!! @details
!! @param[in] fname The name of the input file with the I/O decomposition
!! @param[out] ndims  The number of dimensions in the I/O decomposition
!!                    is returned in this arg
!! @param[out] gdims  Pointer to a 1D array where the global dimensions
!!                    of the I/O decomposition will be saved. The array
!!                    is allocated by the library
!! @param[out] mapsz  The size of the I/O decomposition map, @p map, is
!!                    returned in this arg
!! @param[out] map    Pointer to a 1D array where the I/O decomposition
!!                    mapping (between the local data index and the global
!!                    linearized index for each local data element) will
!!                    be saved. The array is allocated by the library
!! @param[in] comm  The MPI communicator (that contains the MPI processes
!!                  in the I/O system associated with the I/O decomposition)
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_readmap_from_f90(fname, ndims, gdims, mapsz,&
                            map, comm)&
                            bind(C,name="PIOc_readmap_from_f90")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND 

    CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
    INTEGER(C_INT), INTENT(OUT) :: ndims
    TYPE(C_PTR), INTENT(OUT) :: gdims
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), INTENT(OUT) :: mapsz
    TYPE(C_PTR), INTENT(OUT) :: map
    INTEGER(C_INT), VALUE, INTENT(IN) :: comm
  END FUNCTION PIOc_readmap_from_f90
END INTERFACE

INTERFACE
!! @public
!! @brief Write an I/O decomposition to a file
!!
!! @details
!! @param[in] fname The name of the input file with the I/O decomposition
!! @param[in] ioid  The id of the I/O decomposition
!! @param[in] ndims  The number of dimensions in the I/O decomposition
!! @param[in] gdims  Pointer to a 1D array with the global dimensions
!!                    of the I/O decomposition
!! @param[in] mapsz  The size of the I/O decomposition map, @p map
!! @param[in] map    Pointer to a 1D array with the I/O decomposition
!!                    mapping (between the local data index and the global
!!                    linearized index for each local data element)
!! @param[in] comm  The MPI communicator (that contains the MPI processes
!!                  in the I/O system associated with the I/O decomposition)
!! @return PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_writemap_from_f90(fname, ioid, ndims, gdims,&
                            mapsz, map, comm)&
                            bind(C,name="PIOc_writemap_from_f90")
    USE iso_c_binding
    USE pio_kinds, ONLY : PIO_OFFSET_F2C_TYPE_KIND 

    CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
    INTEGER(C_INT), VALUE, INTENT(IN) :: ioid
    INTEGER(C_INT), VALUE, INTENT(IN) :: ndims
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: gdims
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), VALUE, INTENT(IN) :: mapsz
    INTEGER(PIO_OFFSET_F2C_TYPE_KIND), DIMENSION(*), INTENT(IN) :: map
    INTEGER(C_INT), VALUE, INTENT(IN) :: comm
  END FUNCTION PIOc_writemap_from_f90
END INTERFACE

END MODULE spio_decomp_cint
