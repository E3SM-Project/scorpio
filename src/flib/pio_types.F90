!>
!! @file
!! @brief Derived datatypes and constants for PIO Fortran API
!!
!<
module pio_types
    use pio_kinds
    use iso_c_binding
    use spio_netcdf_types, only : PIO_CONTIGUOUS, PIO_CHUNKED, PIO_COMPACT
    implicit none
    private

    !-------------------------------------------
    !  data structure to describe decomposition
    !-------------------------------------------
    type, public :: DecompMap_t
#ifdef SEQUENCE
        sequence
#endif
        integer(i4) :: start
        integer(i4) :: length
    end type

    !----------------------------------------
    ! Some constants related to types below
    !----------------------------------------
    ! Invalid I/O system id
    integer(kind=c_int), public, parameter :: PIO_IOSYSID_INVALID = -1
    ! Invalid file handle
    integer(kind=c_int), public, parameter :: PIO_FH_INVALID = -1
    ! Invalid I/O decomposition id
    integer(kind=c_int), public, parameter :: PIO_IOID_INVALID = -1
    ! Invalid variable id
    integer(kind=c_int), public, parameter :: PIO_VARID_INVALID = -1

    !------------------------------------
    !  a file descriptor data structure
    !------------------------------------
!>
!! @public
!! @struct iosystem_desc_t
!! @brief A defined PIO system descriptor created by @ref PIO_init (see pio_types)
!<
    type, public :: IOSystem_desc_t
        integer(kind=c_int) :: iosysid = -1
     end type IOSystem_desc_t

!>
!! @private
!! @struct io_data_list
!! @brief Linked list of buffers for pnetcdf non-blocking interface
!>
!    type, public :: io_data_list
!       integer :: request
!       real(r4), pointer :: data_real(:) => null()
!       integer(i4), pointer :: data_int(:) => null()
!       real(r8), pointer :: data_double(:) => null()
!       type(io_data_list), pointer :: next => null()
!    end type io_data_list


!>
!! @public
!! @struct file_desc_t
!! @brief File descriptor returned by \ref PIO_openfile or \ref PIO_createfile (see pio_types)
!!
!>
    type, public :: File_desc_t
       integer(kind=c_int) :: fh
       type(iosystem_desc_t), pointer :: iosystem => null()
    end type File_desc_t


!>
!! @private
!! @defgroup iodesc_generate io descriptors, generating
!! @brief The io descriptor structure in defined in this subroutine
!! and subsequently used in @ref PIO_read_darray, @ref PIO_write_darray,
!! @ref PIO_put_var, @ref PIO_get_var calls (see pio_types).
!<

!>
!! @public
!! @struct io_desc_t
!! @brief  An io descriptor handle that is generated in @ref PIO_initdecomp
!! (see pio_types)
!<
   type, public :: io_desc_t
#ifdef SEQUENCE
        sequence
#endif
        integer(i4)         :: ioid
    end type

!>
!! @public
!! @struct var_desc_t
!! @brief A variable descriptor returned from @ref PIO_def_var (see pio_types)
!<
    type, public :: Var_desc_t
#ifdef SEQUENCE
       sequence
#endif
       integer(i4) :: varID
       integer(i4) :: ncid
    end type Var_desc_t

!>
!! @defgroup PIO_iotype PIO_iotype
!! @public
!! @brief An integer parameter that controls the underlying I/O library and
!! the I/O library-specific options (compression etc) used by the library.
!! Note that since an iotype is associated with a file, two different files
!! can potentially be associated with two different iotypes.
!! @details
!!   - PIO_iotype_pnetcdf : parallel read/write using the PnetCDF library
!!   - PIO_iotype_netcdf : serial (only the root I/O process) read/write using the NetCDF library
!!   - PIO_iotype_netcdf4c : parallel read/serial write of NetCDF4 (HDF5) files with data compression
!!   - PIO_iotype_netcdf4p : parallel read/write of NETCDF4 (HDF5) files
!!   - PIO_iotype_adios : parallel write of ADIOS files with subset rearrangement only
!!   - PIO_iotype_hdf5 : parallel write of HDF5 files
!>
    integer(i4), public, parameter ::  &
        PIO_iotype_pnetcdf = 1, &   ! parallel read/write of pNetCDF files
        PIO_iotype_netcdf  = 2, &   ! serial read/write of NetCDF file using 'base_node'
        PIO_iotype_netcdf4c = 3, &  ! netcdf4 (hdf5 format) file opened for compression (serial write access only)
        PIO_iotype_netcdf4p = 4, &  ! netcdf4 (hdf5 format) file opened in parallel (all netcdf4 files for read will be opened this way)
        PIO_iotype_adios = 5, &     ! parallel write of ADIOS files (Write only, rearr subset only)
        PIO_iotype_hdf5 = 6         ! parallel write of HDF5 files


! These are for backward compatability and should not be used or expanded upon
    integer(i4), public, parameter ::                       &
        iotype_pnetcdf = PIO_iotype_pnetcdf,                &
        iotype_netcdf  = PIO_iotype_netcdf


!>
!! @defgroup PIO_rearr_method PIO_rearr_method
!! @public
!! @brief The three choices to control rearrangement are:
!! @details
!!  - PIO_rearr_none : Do not use any form of rearrangement
!!  - PIO_rearr_box : Use a PIO internal box rearrangement
!! -  PIO_rearr_subset : Use a PIO internal subsetting rearrangement
!>

    integer(i4), public, parameter :: PIO_rearr_box =  1
    integer(i4), public, parameter :: PIO_rearr_subset =  2

!>
!! @public
!! @defgroup PIO_error_method error_methods
!! @details
!! The four types of error handling methods are:
!!  - PIO_INTERNAL_ERROR  : abort on error from any task
!!  - PIO_BCAST_ERROR     : broadcast an error from io_rank 0 to all tasks in comm
!!  - PIO_REDUCE_ERROR     : Reduce error across all tasks in comm
!!  - PIO_RETURN_ERROR    : do nothing - allow the user to handle it
!<
  integer(i4), public, parameter :: PIO_INTERNAL_ERROR = -51
  integer(i4), public, parameter :: PIO_BCAST_ERROR = -52
  integer(i4), public, parameter :: PIO_REDUCE_ERROR = -53
  integer(i4), public, parameter :: PIO_RETURN_ERROR = -54

!>
!! @public
!! @defgroup PIO_error_method error_methods
!! @details
!! Use this instead of ios to set error handling for the library.
!<
  integer(i4), public, parameter :: PIO_DEFAULT = -1

!>
!! @public
!! @defgroup error_return error return codes
!! @brief : The error return code. Set to PIO_NOERR on success,
!! or an error code otherwise
!! (See @ref PIO_seterrorhandling for more information on how
!! to customize/set error handling)
!>

!>
!! @public
!! @defgroup open_file_modes Supported modes for opening a file
!! @brief : The flags to specify the file access mode when
!! opening a file
!! PIO_WRITE : Using this flag an existing file is opened
!! in "write" mode
!! PIO_NOWRITE : Using this flag an existing file is opened
!! in "read-only" mode
!>

!>
!! @public
!! @defgroup create_file_modes Supported modes for creating a file
!! @brief : The flags to specify the file access mode when
!! creating a file
!! PIO_CLOBBER : The file, if it exists, is truncated
!! (the old contents of the file are deleted/overwritten)
!! PIO_NOCLOBBER : This flag is used to ensure that a file,
!! if it exists, is not truncated (the current contents of
!! the file are not deleted) during file creation. If a
!! file exists and the user uses PIO_NOCLOBBER while trying
!! to recreate the file the call (PIO_createfile) will fail
!>

!>
!! @struct use_PIO_kinds
!! @brief The type of variable(s) associated with this iodesc.
!! @copydoc PIO_kinds
!<

!>
!! @public
!! @defgroup PIO_kinds PIO_kinds
!! @brief The base types supported by PIO are:
!! @details
!!  - PIO_double : 8-byte reals or double precision
!!  - PIO_real : 4-byte reals
!!  - PIO_int :  4-byte integers
!!  - PIO_char : character
!<
   integer, public, parameter :: PIO_MAX_DIMS_UB = 1024
   integer, public, parameter :: PIO_MAX_ATTRS_UB = 8192
   integer, public, parameter :: PIO_MAX_VARS_UB = 8192
   integer, public, parameter :: PIO_MAX_NAME_UB = 1024
   integer, public, parameter :: PIO_MAX_VAR_DIMS_UB = 1024
   integer, public, parameter :: PIO_FMODE_CLR = 0
#ifdef _PNETCDF
#include <pnetcdf.inc>   /* _EXTERNAL */
   integer, public, parameter :: PIO_global = nf_global
   integer, public, parameter :: PIO_unlimited = nf_unlimited
   integer, public, parameter :: PIO_double = nf_double
   integer, public, parameter :: PIO_real   = nf_real
   integer, public, parameter :: PIO_int    = nf_int
   integer, public, parameter :: PIO_char   = nf_char
   integer, public, parameter :: PIO_noerr  = nf_noerr
   integer, public, parameter :: PIO_WRITE  = nf_write
   integer, public, parameter :: PIO_nowrite  = nf_nowrite
   integer, public, parameter :: PIO_CLOBBER = nf_clobber
   integer, public, parameter :: PIO_NOCLOBBER = nf_NOclobber
   integer, public, parameter :: PIO_FILL = nf_fill
   integer, public, parameter :: PIO_NOFILL = nf_nofill
   integer, public, parameter :: PIO_MAX_DIMS = min(nf_max_dims, PIO_MAX_DIMS_UB)
   integer, public, parameter :: PIO_MAX_ATTRS = min(nf_max_attrs, PIO_MAX_ATTRS_UB)
   integer, public, parameter :: PIO_MAX_VARS = min(nf_max_vars, PIO_MAX_VARS_UB)
   integer, public, parameter :: PIO_MAX_NAME = min(nf_max_name, PIO_MAX_NAME_UB)
   integer, public, parameter :: PIO_MAX_VAR_DIMS = min(nf_max_var_dims, PIO_MAX_VAR_DIMS_UB)
   integer, public, parameter :: PIO_64BIT_OFFSET = nf_64bit_offset
   integer, public, parameter :: PIO_64BIT_DATA = nf_64bit_data
   integer, public, parameter :: PIO_FILL_CHAR = nf_fill_char;
   integer, public, parameter :: PIO_FILL_INT = nf_fill_int;
   real, public, parameter :: PIO_FILL_FLOAT = nf_fill_float;
   double precision, public, parameter :: PIO_FILL_DOUBLE = nf_fill_double;

#else
#ifdef _NETCDF
#include <netcdf.inc>   /* _EXTERNAL */
   integer, public, parameter :: PIO_global = nf_global
   integer, public, parameter :: PIO_unlimited = nf_unlimited
   integer, public, parameter :: PIO_double = nf_double
   integer, public, parameter :: PIO_real   = nf_real
   integer, public, parameter :: PIO_int    = nf_int
   integer, public, parameter :: PIO_char   = nf_char
   integer, public, parameter :: PIO_noerr  = nf_noerr
   integer, public, parameter :: PIO_WRITE  = nf_write
   integer, public, parameter :: PIO_nowrite = nf_nowrite
   integer, public, parameter :: PIO_CLOBBER = nf_clobber
   integer, public, parameter :: PIO_NOCLOBBER = nf_NOclobber
   integer, public, parameter :: PIO_FILL = nf_fill
   integer, public, parameter :: PIO_NOFILL = nf_nofill
   integer, public, parameter :: PIO_MAX_DIMS = min(nf_max_dims, PIO_MAX_DIMS_UB)
   integer, public, parameter :: PIO_MAX_ATTRS = min(nf_max_attrs, PIO_MAX_ATTRS_UB)
   integer, public, parameter :: PIO_MAX_VARS = min(nf_max_vars, PIO_MAX_VARS_UB)
   integer, public, parameter :: PIO_MAX_NAME = min(nf_max_name, PIO_MAX_NAME_UB)
   integer, public, parameter :: PIO_MAX_VAR_DIMS = min(nf_max_var_dims, PIO_MAX_VAR_DIMS_UB)
   integer, public, parameter :: PIO_64BIT_OFFSET = nf_64bit_offset
   integer, public, parameter :: PIO_64BIT_DATA = 0
   integer, public, parameter :: PIO_FILL_CHAR = nf_fill_char;
   integer, public, parameter :: PIO_FILL_INT = nf_fill_int;
   real, public, parameter :: PIO_FILL_FLOAT = nf_fill_float;
   double precision, public, parameter :: PIO_FILL_DOUBLE = nf_fill_double;
#else
   integer, public, parameter :: PIO_global = 0
   integer, public, parameter :: PIO_double = 6
   integer, public, parameter :: PIO_real   = 5
   integer, public, parameter :: PIO_int    = 4
   integer, public, parameter :: PIO_char   = 2
   integer, public, parameter :: PIO_noerr  = 0
   integer, public, parameter :: PIO_MAX_DIMS = PIO_MAX_DIMS_UB
   integer, public, parameter :: PIO_MAX_ATTRS = PIO_MAX_ATTRS_UB
   integer, public, parameter :: PIO_MAX_VARS = PIO_MAX_VARS_UB
   integer, public, parameter :: PIO_MAX_NAME = PIO_MAX_NAME_UB
   integer, public, parameter :: PIO_MAX_VAR_DIMS = 6
   integer, public, parameter :: PIO_FILL = 0
   integer, public, parameter :: PIO_NOFILL = 1
   integer, public, parameter :: PIO_CLOBBER = 10
   integer, public, parameter :: PIO_NOCLOBBER = 11
   integer, public, parameter :: PIO_WRITE = 20
   integer, public, parameter :: PIO_NOWRITE = 21
   integer, public, parameter :: PIO_64BIT_OFFSET = 0
   integer, public, parameter :: PIO_64BIT_DATA = 0
   integer, public, parameter :: PIO_FILL_CHAR = 0;
   integer, public, parameter :: PIO_FILL_INT = -2147483647;
   real, public, parameter :: PIO_FILL_FLOAT =  9.9692099683868690e+36;
   double precision, public, parameter :: PIO_FILL_DOUBLE = 9.9692099683868690e+36;
#endif
#endif

   integer, public, parameter :: PIO_num_OST =  16
   ! Generic error code for internal errors in the library
   integer, public, parameter :: PIO_EINTERNAL = -501

   ! Export types from spio_netcdf_types
   public :: PIO_CONTIGUOUS, PIO_CHUNKED, PIO_COMPACT

!>
!! @defgroup PIO_rearr_comm_t PIO_rearr_comm_t
!! @public 
!! @brief The data rearranger communication mode
!! @details
!! The data rearranger in the library rearranges data between the
!! compute processes (all the MPI processes in the I/O subsystem) and
!! I/O processes (a subset of the compute processes in the I/O subsystem or
!! a disjoint set of MPI processes in the I/O subsystem) before flushing the
!! data written out by the user to the filesystem. The data rearrangement
!! among the MPI processes can be achieved using MPI point to point or
!! collective communication.
!!  - PIO_rearr_comm_p2p : Point to point
!!  - PIO_rearr_comm_coll : Collective
!>
    enum, bind(c)
      enumerator :: PIO_rearr_comm_p2p = 0
      enumerator :: PIO_rearr_comm_coll
    end enum

!>
!! @defgroup PIO_rearr_comm_dir PIO_rearr_comm_dir
!! @public 
!! @brief The data rearrangment flow control direction
!! @details
!! The data rearranger can use flow control when rearranging data among
!! the MPI processes. The flow control can be enabled for data
!! rearrangement from compute processes to I/O processes (e.g. during a
!! write operation), from I/O processes to compute processes (e.g. during
!! a read operation) or both.
!!  - PIO_rearr_comm_fc_2d_enable : compute procs to I/O procs and vice versa
!!  - PIO_rearr_comm_fc_1d_comp2io: compute procs to I/O procs only
!!  - PIO_rearr_comm_fc_1d_io2comp: I/O procs to compute procs only
!!  - PIO_rearr_comm_fc_2d_disable: disable flow control
!>
    enum, bind(c)
      enumerator :: PIO_rearr_comm_fc_2d_enable = 0
      enumerator :: PIO_rearr_comm_fc_1d_comp2io
      enumerator :: PIO_rearr_comm_fc_1d_io2comp
      enumerator :: PIO_rearr_comm_fc_2d_disable
    end enum

!>
!! @defgroup PIO_rearr_comm_fc_options PIO_rearr_comm_fc_options
!! @brief The data rearranger flow control options
!! @details
!! The data rearranger supports flow control when rearranging data among
!! the MPI processes.
!!  - enable_hs : Enable handshake (logical). If this option is enabled
!!                a "handshake" is sent between communicating processes
!!                before data is sent
!!  - enable_isend : Enable non-blocking sends (logical). If this option
!!                    is enabled non-blocking sends (instead of blocking
!!                    sends) are used to send data between MPI processes
!!                    while rearranging data
!!  - max_pend_req : The maximum pending requests allowed at a time when
!!                    MPI processes communicate for rearranging data among
!!                    themselves. (Use PIO_REARR_COMM_UNLIMITED_PEND_REQ
!!                    for allowing unlimited number of pending requests)
!>
   type, bind(c), public :: PIO_rearr_comm_fc_opt_t
      logical(c_bool) :: enable_hs            ! Enable handshake?
      logical(c_bool) :: enable_isend         ! Enable isends?
      integer(c_int) :: max_pend_req         ! Maximum pending requests
    end type PIO_rearr_comm_fc_opt_t

    integer, public, parameter :: PIO_REARR_COMM_UNLIMITED_PEND_REQ = -1
!>
!! @defgroup PIO_rearr_options PIO_rearr_options
!! @brief The data rearranger options
!! @details
!! The library includes support for a data rearranger that rearranges data
!! among MPI processes to improve the I/O throughput of the application.
!! The user can control the data rearrangement by passing the data
!! rearranger options to the library.
!!  - comm_type : @copydoc PIO_rearr_comm_t
!!  - fcd : @copydoc PIO_rearr_comm_dir
!!  - comm_fc_opts : @copydoc PIO_rearr_comm_fc_options
!>
    type, bind(c), public :: PIO_rearr_opt_t
      integer(c_int)                         :: comm_type
      integer(c_int)                         :: fcd       ! Flow control direction
      type(PIO_rearr_comm_fc_opt_t)   :: comm_fc_opts_comp2io
      type(PIO_rearr_comm_fc_opt_t)   :: comm_fc_opts_io2comp
    end type PIO_rearr_opt_t

    public :: PIO_rearr_comm_p2p, PIO_rearr_comm_coll,&
              PIO_rearr_comm_fc_2d_enable, PIO_rearr_comm_fc_1d_comp2io,&
              PIO_rearr_comm_fc_1d_io2comp, PIO_rearr_comm_fc_2d_disable

end module pio_types
