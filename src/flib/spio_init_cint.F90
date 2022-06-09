!>
!! @file
!! @brief C Interfaces for SCORPIO APIs to initialize/finalize
!! an I/O subsystem

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_init_cint'

MODULE spio_init_cint
  USE iso_c_binding
  USE pio_types, ONLY : iosystem_desc_t, pio_rearr_opt_t
  IMPLICIT NONE

INTERFACE
!> @private
!! @brief Fortran interface to C function to initialize I/O
!! subsystem using an MPI intra-communicator
!!
!! @details
!! @param[in] comm The MPI communicator containing all the MPI processes
!!                  initializing the I/O subsystem
!! @param[in] nioprocs The total number of I/O processes (a subset of the
!!                      total number of processes in @p comm) in the I/O
!!                      subsystem
!! @param[in] ioprocs_stride The stride (The number of MPI processes to "skip"
!!                            between assigning two consecutive I/O processes.
!!                            A stride of 1 implies consecutive MPI processes
!!                            after the @p ioprocs_base rank are designated
!!                            the I/O processes) between two I/O processes
!!                            in the I/O subsystem
!! @param[in] ioprocs_base The base (MPI rank of the 1st I/O process)
!!                         rank for I/O processes.
!! @param[in] rearr @copydoc PIO_rearr_method
!! @param[in] prearr_opts Pointer to the I/O rearranger options to use for this
!!                                  I/O subsystem. @copydoc PIO_rearr_options
!! @param[out] iosysid The handle to the initialized I/O system is returned via
!!                      this argument.
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_Init_Intracomm_from_F90(comm, nioprocs,&
                            ioprocs_stride, ioprocs_base,&
                            rearr, prearr_opts, iosysid)&
                          bind(C,name="PIOc_Init_Intracomm_from_F90")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: comm
    INTEGER(C_INT), VALUE :: nioprocs
    INTEGER(C_INT), VALUE :: ioprocs_stride
    INTEGER(C_INT), VALUE :: ioprocs_base
    INTEGER(C_INT), VALUE :: rearr
    TYPE(C_PTR), VALUE :: prearr_opts
    INTEGER(C_INT), VALUE :: iosysid
  END FUNCTION PIOc_Init_Intracomm_from_F90
END INTERFACE

INTERFACE
!> @private
!! @brief Fortran interface to C function to initialize I/O
!! subsystem using an MPI inter-communicator
!!
!! @details
!! @param[in] ncomps The number of compute components (Also determines
!!                   the size of the @p comp_comms array)
!! @param[in] peer_comm The peer MPI communicator to use to create an MPI
!!                      inter-communicator between compute and I/O MPI
!!                      communicators. This is typically the MPI communicator
!!                      from which the component and I/O communicators are
!!                      created (MPI_COMM_WORLD or the "world communicator"
!!                      for all the MPI communicators)
!! @param[in] comp_comms An array of MPI communicators corresponding to the
!!                        groups of compute processes. Typically each application
!!                        component has an MPI communicator, containing all
!!                        the processes in the component, in this array. This
!!                        array needs to contain at least @p ncomps elements,
!!                        and all processes belonging to compute component i
!!                        (a unique global index decided by the application
!!                        for each component) provides the MPI communicator for
!!                        component i in comp_comms(i). All I/O processes pass
!!                        MPI_COMM_NULL for all communicators in this array
!! #param[in] io_comm The MPI communicator containing all the I/O processes.
!!                    The I/O processes are separate (disjoint set) from the
!!                    compute processes
!!                    All I/O processes provide the MPI communicator containing
!!                    all the I/O processes via this argument. All compute
!!                    processes pass MPI_COMM_NULL in this argument.
!! @param[in] rearr (Optional) @copydoc PIO_rearr_method
!! @param[out] iosys_ids The handle to the initialized I/O system is returned via
!!                    this array. For compute component i (a unique global
!!                    index decided by the application for each component)
!!                    the initialized I/O subsystem is returned in the ith
!!                    index of this array, iosys_ids(i).
!! @returns PIO_NOERR on success, an error code otherwise
!!
  INTEGER(C_INT) FUNCTION PIOc_Init_Intercomm_from_F90(ncomps, peer_comm,&
                            comp_comms, io_comm, rearr, iosys_ids)&
                          bind(C,name="PIOc_Init_Intercomm_from_F90")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: ncomps
    INTEGER(C_INT), VALUE :: peer_comm
    TYPE(C_PTR), VALUE :: comp_comms
    INTEGER(C_INT), VALUE :: io_comm
    INTEGER(C_INT), VALUE :: rearr
    TYPE(C_PTR), VALUE :: iosys_ids
  END FUNCTION PIOc_Init_Intercomm_from_F90
END INTERFACE

INTERFACE
!> @private
!! @brief Fortran interface to C function to finalize an I/O
!! subsystem
!!
!! @details
!! @param[in] iosysid The handle to the I/O system
!! @returns PIO_NOERR on success, an error code otherwise
  INTEGER(C_INT) FUNCTION PIOc_finalize(iosysid)&
                          bind(C,name="PIOc_finalize")
    USE iso_c_binding

    INTEGER(C_INT), VALUE :: iosysid
  END FUNCTION PIOc_finalize
END INTERFACE

END MODULE spio_init_cint
