!> @file
!! @brief SCORPIO APIs for initializing and finalizing the library
!! This file contains the SCORPIO APIs for initializing and finalizing
!! the SCORPIO I/O sub systems (corresponding to MPI communicators).
!! Note that each I/O sub system (usually corresponds to individual
!! components with a distinct MPI communicator) is initialized/finalized
!! separately.
!!

!> @internal
!! @def __PIO_FILE__
!! This macro is used to define the file name in user messages
!!
#define __PIO_FILE__ 'spio_init.F90'

MODULE spio_init
  USE iso_c_binding
  USE pio_types, ONLY : iosystem_desc_t, pio_rearr_opt_t, PIO_IOSYSID_INVALID,&
                        PIO_MAX_NAME, PIO_NOERR, pio_rearr_box
  USE pio_kinds, ONLY : pio_offset_kind
  USE spio_err, ONLY  : pio_error
  USE spio_init_cint
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: pio_init, pio_finalize

!> @defgroup pio_init
!! @public
!! @brief Initialize the I/O subsystem
!!
!! @details Each I/O subsystem (corresponding to a set of MPI processes
!! that belong to an MPI communicator and collectively work on a set
!! of files) needs to be initialized before invoking any PIO APIs on
!! the subsystem
!!
INTERFACE pio_init
  MODULE PROCEDURE pio_init_intracomm
  MODULE PROCEDURE pio_init_intercomm
  MODULE PROCEDURE pio_init_intercomm_v2
END INTERFACE

CONTAINS

!> @ingroup pio_init
!! @public
!! @brief Initialize the I/O subsystem that is defined using an MPI
!! intra-communicator. This is a collective call on the MPI communicator,
!! @p comm, used to initialize the I/O subsystem.
!!
!! @details
!! @param[in] comm_rank The rank (in @p comm) of the MPI process initializing
!!                      the I/O subsystem
!! @param[in] comm The MPI communicator containing all the MPI processes
!!                  initializing the I/O subsystem
!! @param[in] nioprocs The total number of I/O processes (a subset of the
!!                      total number of processes in @p comm) in the I/O
!!                      subsystem
!! @param[in] naggprocs The total number of processes aggregating data in the
!!                      I/O subsystem (this argument is ignored now, and
!!                      is retained for API backward compatibility)
!! @param[in] ioprocs_stride The stride (The number of MPI processes to "skip"
!!                            between assigning two consecutive I/O processes.
!!                            A stride of 1 implies consecutive MPI processes
!!                            after the @p ioprocs_base rank are designated
!!                            the I/O processes) between two I/O processes
!!                            in the I/O subsystem
!! @param[in] rearr @copydoc PIO_rearr_method
!! @param[out] iosys The handle to the initialized I/O system is returned via
!!                    this argument. @copydoc iosystem_desc_t
!! @param[in] ioprocs_base (Optional) The base (MPI rank of the 1st I/O process)
!!                                    rank for I/O processes. By default, MPI
!!                                    rank 0 (in @p comm) is the base I/O process
!! @param[in] rearr_opts (Optional) The I/O rearranger options to use for this
!!                                  I/O subsystem. @copydoc PIO_rearr_options
!! @retval ierr (Optional) @copydoc error_return
!! 
  SUBROUTINE pio_init_intracomm(comm_rank, comm, nioprocs, naggprocs,&
                                ioprocs_stride, rearr, iosys,&
                                ioprocs_base, rearr_opts, ierr)
    INTEGER, INTENT(IN) :: comm_rank
    INTEGER, INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: nioprocs
    INTEGER, INTENT(IN) :: naggprocs
    INTEGER, INTENT(IN) :: ioprocs_stride
    INTEGER, INTENT(IN) :: rearr
    TYPE(iosystem_desc_t), INTENT(OUT) :: iosys
    INTEGER, OPTIONAL, INTENT(IN) :: ioprocs_base
    TYPE(pio_rearr_opt_t), OPTIONAL, TARGET, INTENT(IN) :: rearr_opts
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT), PARAMETER :: ROOT_RANK = 0
    TYPE(C_PTR) :: prearr_opts
    INTEGER(C_INT) :: cioprocs_base
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cerr
    INTEGER :: ret

#ifdef TIMING
    call t_startf("PIO:init")
#endif

    IF(PRESENT(ioprocs_base)) THEN
      cioprocs_base = ioprocs_base
    ELSE
      cioprocs_base = ROOT_RANK
    ENDIF

    IF(PRESENT(rearr_opts)) THEN
      prearr_opts = C_LOC(rearr_opts)
    ELSE
      prearr_opts = C_NULL_PTR
    ENDIF

    iosys%iosysid = PIO_IOSYSID_INVALID
    cerr = PIOc_Init_Intracomm_from_F90(INT(comm, C_INT), INT(nioprocs, C_INT),&
            INT(ioprocs_stride, C_INT), cioprocs_base, INT(rearr, C_INT),&
            prearr_opts, iosys%iosysid)
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ELSE
      WRITE(log_msg, *) "PIO_Init (pio_init_intracomm) failed, ",&
                        "comm = ", comm, ", nioprocs=", nioprocs,&
                        ", ioprocs_stride = ", ioprocs_stride,&
                        ", ioprocs_base = ", INT(cioprocs_base),&
                        ", rearr = ", rearr, ", err = ", cerr
      ret = pio_error(comm, INT(cerr), __FILE__, __LINE__, trim(log_msg))
    ENDIF

#ifdef TIMING
    call t_stopf("PIO:init")
#endif
  END SUBROUTINE pio_init_intracomm

!> @ingroup pio_init
!! @public
!! @brief Initialize the I/O subsystem that is defined using an MPI
!! inter-communicator. The I/O subsystem is created using one or more
!! groups of compute processes (Defined by an array of MPI
!! communicators, @p comp_comms, where each MPI communicator in the
!! array corresponds to a group of compute processes. The compute
!! and I/O processes are disjoint set of processes. Note that
!! typically each MPI communicator/process_group is associated with
!! an computational component in the application) and a group of
!! I/O processes (Defined by an MPI communicator, @p io_comm). The
!! compute processes redirect all I/O requests (reading/writing
!! variables, attributes etc) to the I/O processes by asynchronously
!! communicating with the I/O processes (via asynchronous messages)
!!
!! This is a collective call on the MPI communicator, @p peer_comm,
!! used to initialize the I/O subsystem. For all compute processes
!! this API returns after the I/O subsystem is initialized. The API
!! blocks inside the library for all I/O processes waiting for
!! asynchronous messages from the compute processes and returns once
!! the I/O subsystems are finalized (via PIO_finalize()) on all
!! compute processes.
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
!! @param[out] iosys The handle to the initialized I/O system is returned via
!!                    this array. For compute component i (a unique global
!!                    index decided by the application for each component)
!!                    the initialized I/O subsystem is returned in the ith
!!                    index of this array, iosys(i). @copydoc iosystem_desc_t
!! @param[in] rearr (Optional) @copydoc PIO_rearr_method
!! @retval ierr (Optional) @copydoc error_return
!! 
  SUBROUTINE pio_init_intercomm(ncomps, peer_comm, comp_comms, io_comm,&
                                iosys, rearr, ierr)
    INTEGER, INTENT(IN) :: ncomps
    INTEGER, INTENT(IN) :: peer_comm
    INTEGER, TARGET, INTENT(IN) :: comp_comms(ncomps)
    INTEGER, INTENT(IN) :: io_comm
    TYPE(iosystem_desc_t), DIMENSION(:), INTENT(OUT) :: iosys
    INTEGER, OPTIONAL, INTENT(IN) :: rearr
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: crearr
    INTEGER(C_INT), TARGET :: ccomp_comms(ncomps)
    INTEGER(C_INT), TARGET :: iosys_ids(ncomps)
    CHARACTER(LEN=PIO_MAX_NAME) :: log_msg
    INTEGER(C_INT) :: cerr
    INTEGER :: i, ret

#ifdef TIMING
    call t_startf("PIO:init")
#endif

    DO i=1,ncomps
      ccomp_comms(i) = INT(comp_comms(i), C_INT)
    END DO
    iosys_ids = INT(PIO_IOSYSID_INVALID, C_INT)
    IF(PRESENT(rearr)) THEN
      crearr = INT(rearr, C_INT)
    ELSE
      crearr = INT(pio_rearr_box, C_INT)
    ENDIF
    cerr = PIOc_Init_Intercomm_from_F90(INT(ncomps, C_INT), INT(peer_comm, C_INT),&
                                        C_LOC(ccomp_comms), INT(io_comm, C_INT),&
                                        crearr, C_LOC(iosys_ids))
    IF(PRESENT(ierr)) THEN
      ierr = INT(cerr)
    ELSE
      WRITE(log_msg, *) "PIO_Init (pio_init_intercomm) failed, ",&
                        "ncomps = ", ncomps, ", peer_comm = ", peer_comm,&
                        "io_comm = ", io_comm, ", err = ", cerr
      ret = pio_error(peer_comm, INT(cerr), __FILE__, __LINE__, trim(log_msg))
    ENDIF

    DO i=1,ncomps
      iosys(i)%iosysid = iosys_ids(i)
    END DO

#ifdef TIMING
    call t_stopf("PIO:init")
#endif
  END SUBROUTINE pio_init_intercomm

!> @ingroup pio_init
!! @public
!! @brief Initialize the I/O subsystem that is defined using an MPI
!! inter-communicator. The I/O subsystem is created using one or more
!! groups of compute processes (Defined by an array of MPI
!! communicators, @p comp_comms, where each MPI communicator in the
!! array corresponds to a group of compute processes. The compute
!! and I/O processes are disjoint set of processes. Note that
!! typically each MPI communicator/process_group is associated with
!! an computational component in the application) and a group of
!! I/O processes (Defined by an MPI communicator, @p io_comm). The
!! compute processes redirect all I/O requests (reading/writing
!! variables, attributes etc) to the I/O processes by asynchronously
!! communicating with the I/O processes (via asynchronous messages)
!!
!! This is a collective call on the MPI communicator, @p peer_comm,
!! used to initialize the I/O subsystem. For all compute processes
!! this API returns after the I/O subsystem is initialized. The API
!! blocks inside the library for all I/O processes waiting for
!! asynchronous messages from the compute processes and returns once
!! the I/O subsystems are finalized (via PIO_finalize()) on all
!! compute processes.
!!
!! @details
!! @param[out] iosys The handle to the initialized I/O system is returned via
!!                    this array. For compute component i (a unique global
!!                    index decided by the application for each component)
!!                    the initialized I/O subsystem is returned in the ith
!!                    index of this array, iosys(i). @copydoc iosystem_desc_t
!! @param[in] peer_comm The peer MPI communicator to use to create an MPI
!!                      inter-communicator between compute and I/O MPI
!!                      communicators. This is typically the MPI communicator
!!                      from which the component and I/O communicators are
!!                      created (MPI_COMM_WORLD or the "world communicator"
!!                      for all the MPI communicators)
!! @param[in] comp_comms An array of MPI communicators corresponding to the
!!                        groups of compute processes. Typically each application
!!                        component has an MPI communicator, containing all
!!                        the processes in the component, in this array.
!!                        All processes belonging to compute component i
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
!! @retval ierr (Optional) @copydoc error_return
!! 
  SUBROUTINE pio_init_intercomm_v2(iosys, peer_comm, comp_comms, io_comm,&
                                    rearr, ierr)
    TYPE(iosystem_desc_t), DIMENSION(:), INTENT(OUT) :: iosys
    INTEGER, INTENT(IN) :: peer_comm
    INTEGER, TARGET, INTENT(IN), DIMENSION(:) :: comp_comms
    INTEGER, INTENT(IN) :: io_comm
    INTEGER, OPTIONAL, INTENT(IN) :: rearr
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    call pio_init_intercomm(size(comp_comms), peer_comm, comp_comms, io_comm,&
                            iosys, rearr, ierr)

  END SUBROUTINE pio_init_intercomm_v2

!> @defgroup pio_finalize
!! @public
!! @brief Finalize the I/O subsystem
!!
!! @details This is a collective call on the I/O subsystem.
!! @param[inout] iosys The handle to the I/O system. @copydoc iosystem_desc_t
!! @retval ierr @copydoc error_return
!! 
  SUBROUTINE pio_finalize(iosys, ierr)
    TYPE(iosystem_desc_t), INTENT(INOUT) :: iosys
    INTEGER, INTENT(OUT) :: ierr

    INTEGER(C_INT) :: cerr

#ifdef TIMING
    call t_startf("PIO:finalize")
#endif

    ierr = PIO_NOERR
    IF(iosys%iosysid /= PIO_IOSYSID_INVALID) THEN
      cerr = PIOc_finalize(iosys%iosysid)
      ierr = INT(cerr)

      iosys%iosysid = PIO_IOSYSID_INVALID
    ENDIF

#ifdef TIMING
    call t_stopf("PIO:finalize")
#endif
  END SUBROUTINE pio_finalize
END MODULE spio_init
