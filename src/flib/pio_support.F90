#define __PIO_FILE__ "pio_support.F90"
!>
!! @file pio_support.F90
!! @brief internal code for compiler workarounds, aborts and debug functions
!!
!<
module pio_support
  use pio_kinds
  use iso_c_binding
#ifndef NO_MPIMOD
  use mpi !_EXTERNAL
#endif
  use pio_types, only : file_desc_t, pio_noerr
  implicit none
  private
#ifdef NO_MPIMOD
  include 'mpif.h'    ! _EXTERNAL
#endif
  public :: piodie
  public :: CheckMPIreturn
  public :: pio_readdof
  public :: pio_writedof
  public :: replace_c_null
  public :: get_text_var_sz

  logical, public :: Debug=.FALSE.
  logical, public :: DebugIO=.FALSE.
  logical, public :: DebugAsync=.FALSE.
  integer,private,parameter :: versno = 1001

  character(len=*), parameter :: modName='pio_support'

contains
!> 
!! @public
!! @brief Remove null termination (C-style) from strings for Fortran.
!<
  subroutine replace_c_null(istr, ilen)
    use iso_c_binding, only : C_NULL_CHAR
    character(len=*),intent(inout) :: istr
    integer(kind=pio_offset_kind), optional, intent(in) :: ilen
    integer :: i, slen
    if(present(ilen)) then
       slen = int(ilen)
    else
       slen = len(istr)
    endif
    do i=1,slen
       if(istr(i:i) == C_NULL_CHAR) exit
    end do
    istr(i:slen)=''
  end subroutine replace_c_null

!>
!! @public
!! @brief Get the size of a text/character variable
!! (Each element of a text variable is a string)
!! @details
!! @param File : File containing the variable
!! @param varid : Id of the queried variable
!! @param var_slen : The length of each string in the variable is
!!                    returned in this arg
!! @param var_nstrs : The number of strings in the variable is
!!                    returned in this arg (OPTIONAL)
!! @param var_dim_sz : The dimension sizes of the variable is
!!                      returned in this arg (OPTIONAL)
!! @retval ierr
!<
  integer function get_text_var_sz(File, varid, var_slen,&
      var_nstrs, var_dim_sz)
    type (File_desc_t), intent(in) :: File
    integer, intent(in) :: varid
    integer, intent(out) :: var_slen
    integer, intent(out), optional :: var_nstrs
    integer, intent(out), optional :: var_dim_sz(:)

    integer :: ndims = 0
    integer, allocatable :: dimids(:)
    integer(C_SIZE_T) :: dim_sz
    integer :: i, ierr = PIO_NOERR

    interface
       integer(C_INT) function PIOc_inq_varndims(ncid       ,varid,ndims) &
            bind(C                                          ,name="PIOc_inq_varndims")
         use iso_c_binding
         integer(C_INT)                                     , value :: ncid
         integer(C_INT)                                     , value :: varid
         integer(C_INT) :: ndims
       end function PIOc_inq_varndims
    end interface
    interface
       integer(C_INT) function PIOc_inq_vardimid(ncid       ,varid,dimids) &
            bind(C                                          ,name="PIOc_inq_vardimid")
         use iso_c_binding
         integer(C_INT)                                     , value :: ncid
         integer(C_INT)                                     , value :: varid
         integer(C_INT) :: dimids(*)
       end function PIOc_inq_vardimid
    end interface
    interface
       integer(C_INT) function PIOc_inq_dimlen(ncid         ,dimid,len) &
            bind(C                                          ,name="PIOc_inq_dimlen")
         use iso_c_binding
         integer(C_INT)                                     , value :: ncid
         integer(c_int)                                     , value :: dimid
         integer(c_size_t) :: len
       end function PIOc_inq_dimlen
    end interface

    if(present(var_nstrs)) then
      var_nstrs = 0
    end if

    ! Get the number of dimensions in variable
    ierr = PIOc_inq_varndims(File%fh, varid-1, ndims)
    if(ierr /= PIO_NOERR) then
      call piodie(__PIO_FILE__, __LINE__, "Inquiring number of dims of variable failed")
    end if
    if(ndims == 0) then
      ! A scalar character variable
      var_slen = 1
      ! Number of strings = 0
      get_text_var_sz = PIO_NOERR
      return
    end if
    ! Get the dimension ids for the variable
    allocate(dimids(ndims))
    ierr = PIOc_inq_vardimid(File%fh, varid-1, dimids)
    if(ierr /= PIO_NOERR) then
      call piodie(__PIO_FILE__, __LINE__, "Inquiring the variable dimension ids for the text variable failed")
    end if

    ! Get the size of dimension 1
    ! The dimension 1 of a text variable is the length
    ! of each string in the variable i.e., var_slen
    ! Note: The dimension ids used here were retrieved using
    !   the C interface function, hence does not require
    !   Fortran to C conversion
    !   i.e., using dimids(i) instead of dimids(i) - 1
    !   However the dimids are in the C order, so we
    !   need to use the reverse array for the Fortran
    !   dimension
    !   i.e., using dimids(ndims - i + 1) instead of dimids(i)
    ierr = PIOc_inq_dimlen(File%fh, dimids(ndims), dim_sz)
    if(ierr /= PIO_NOERR) then
      call piodie(__PIO_FILE__, __LINE__, "Inquiring the lengths of 0th dimension for the text variable failed")
    end if

    var_slen = int(dim_sz)
    if(present(var_nstrs)) then
      var_nstrs = 1
    end if

    if(ndims > 1) then
      if(present(var_dim_sz)) then
        if(size(var_dim_sz) < ndims - 1) then
          call piodie(__PIO_FILE__, __LINE__, "Not enough space to copy back the dimension sizes of the text variable")
        end if
      end if

      ! Get the size of the other dimensions
      ! These dimensions determine the size of the string array
      do i=2,ndims
        ierr = PIOc_inq_dimlen(File%fh, dimids(ndims - i + 1), dim_sz)
        if(ierr /= PIO_NOERR) then
          call piodie(__PIO_FILE__, __LINE__, "Inquiring the lengths of dimension", i, " for the text variable failed")
        end if

        if(present(var_dim_sz)) then
          var_dim_sz(i-1) = int(dim_sz)
        end if
        if(present(var_nstrs)) then
          var_nstrs = var_nstrs * int(dim_sz)
        end if
      end do
    end if
    deallocate(dimids)

    get_text_var_sz = ierr
  end function get_text_var_sz

!>
!! @public
!! @brief Abort the model for abnormal termination.
!! @param file : File where piodie is called from.
!! @param line : Line number where it is called.
!! @param msg,msg2,msg3,ival1,ival2,ival3,mpirank : Optional argument for error messages.
!<
  subroutine piodie (file,line, msg, ival1, msg2, ival2, msg3, ival3, mpirank)
    !-----------------------------------------------------------------------
    ! Purpose:
    !
    ! Abort the model for abnormal termination
    !
    ! Author: Jim Edwards
    !
    ! Change History
    ! 20070608 R. Loy  added optional args
    !-----------------------------------------------------------------------
    ! $Id$
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    character(len=*), intent(in) :: file
    integer,intent(in) :: line
    character(len=*), intent(in), optional :: msg,msg2,msg3
    integer,intent(in),optional :: ival1,ival2,ival3, mpirank

    character(len=*), parameter :: subName=modName//'::pio_die'
    integer :: ierr, myrank=-1
    
    if(present(mpirank)) myrank=mpirank

    if (present(ival3)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ', &
            msg,ival1,msg2,ival2,msg3,ival3
    else if (present(msg3)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ', &
            msg,ival1,msg2,ival2,msg3
    else if (present(ival2)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg,ival1,msg2,ival2
    else if (present(msg2)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg,ival1,msg2
    else if (present(ival1)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg,ival1
    else if (present(msg)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg
    else
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': (no message)'
    endif


#if defined(CPRXLF) && !defined(BGQ)
  close(5)    ! needed to prevent batch jobs from hanging in xl__trbk
  call xl__trbk()
#endif

    ! passing an argument of 1 to mpi_abort will lead to a STOPALL output 
    ! error code of 257
    call mpi_abort (MPI_COMM_WORLD, 1, ierr)  

#ifdef CPRNAG
    stop
#else
    call abort
#endif


  end subroutine piodie

!=============================================
!  CheckMPIreturn:
!
!      Check and prints an error message
!  if an error occured in a MPI subroutine.
!=============================================
!>
!! @public
!! @brief Check and prints an error message if an error occured in an MPI 
!! subroutine.
!! @param locmesg : Message to output
!! @param errcode : MPI error code
!! @param file : The file where the error message originated.
!! @param line : The line number where the error message originated.
!<
  subroutine CheckMPIreturn(locmesg, errcode, file, line)

     character(len=*), intent(in) :: locmesg
     integer(i4), intent(in) :: errcode
     character(len=*),optional :: file
     integer, intent(in),optional :: line
     character(len=MPI_MAX_ERROR_STRING) :: errorstring

     integer(i4) :: errorlen

     integer(i4) :: ierr
     if (errcode .ne. MPI_SUCCESS) then
        call MPI_Error_String(errcode,errorstring,errorlen,ierr)
        write(*,*) TRIM(ADJUSTL(locmesg))//errorstring(1:errorlen)
        if(present(file).and.present(line)) then
           call piodie(file,line)
        endif
     end if
  end subroutine CheckMPIreturn

!>
!! @public
!! @brief Fortran interface to write a mapping file
!! @param file : The file where the decomp map will be written.
!! @param gdims : The dimensions of the data array in memory.
!! @param DOF : The multidimensional array of indexes that describes how 
!! data in memory are written to a file.
!! @param comm : The MPI comm index.
!! @param punit : Optional argument that is no longer used.
!<
  subroutine pio_writedof (file, gdims, DOF, comm, punit)
    !-----------------------------------------------------------------------
    ! Purpose:
    !
    ! Write a DOF to standard format
    !
    ! Author: T Craig
    !
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    character(len=*),intent(in) :: file
    integer, intent(in) :: gdims(:)
    integer(PIO_OFFSET_KIND)  ,intent(in) :: dof(:)
    integer         ,intent(in) :: comm
    integer,optional,intent(in) :: punit
    integer :: err
    integer :: ndims

    integer :: ioid = -1
    
    interface
       integer(c_int) function PIOc_writemap_from_f90(file, ioid, ndims, gdims, maplen, map, f90_comm) &
            bind(C,name="PIOc_writemap_from_f90")
         use iso_c_binding
         character(C_CHAR), intent(in) :: file
         integer(C_INT), value, intent(in) :: ioid
         integer(C_INT), value, intent(in) :: ndims
         integer(C_INT), intent(in) :: gdims(*)
         integer(C_SIZE_T), value, intent(in) :: maplen 
         integer(C_SIZE_T), intent(in) :: map(*)
         integer(C_INT), value, intent(in) :: f90_comm
       end function PIOc_writemap_from_f90
    end interface

    if (present(punit)) then
        call piodie(__PIO_FILE__, __LINE__,&
                    "Writing PIO decomposition failed. Specifying a unit number, punit, to write the decomposition is no longer supported")
    end if

    ndims = size(gdims)
    err = PIOc_writemap_from_f90(trim(file)//C_NULL_CHAR, ioid, ndims, gdims, int(size(dof),C_SIZE_T), dof, comm)

  end subroutine pio_writedof

!>
!! @public
!! @brief Fortran interface to read a mapping file
!! @param file : The file from where the decomp map is read.
!! @param ndims : The number of dimensions of the data.
!! @param gdims : The actual dimensions of the data (pointer to an integer array of length ndims).
!! @param DOF : Pointer to an integer array where the Decomp map will be stored.
!! @param comm : MPI comm index
!! @param punit : Optional argument that is no longer used.
!<
  subroutine pio_readdof (file, ndims, gdims, DOF, comm, punit)
    !-----------------------------------------------------------------------
    ! Purpose:
    !
    ! Read a DOF to standard format
    !
    ! Author: T Craig
    !
    ! Change History
    ! 
    !-----------------------------------------------------------------------
    ! $Id$
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    character(len=*),intent(in) :: file
    integer(PIO_OFFSET_KIND),pointer:: dof(:)
    integer         ,intent(in) :: comm
    integer,optional,intent(in) :: punit
    integer, intent(out) :: ndims
    integer, pointer :: gdims(:)
    integer(PIO_OFFSET_KIND) :: maplen
    integer :: ierr
    type(C_PTR) :: tgdims, tmap
    interface
       integer(C_INT) function PIOc_readmap_from_f90(file, ndims, gdims, maplen, map, f90_comm) &
            bind(C,name="PIOc_readmap_from_f90") 
         use iso_c_binding
         character(C_CHAR), intent(in) :: file
         integer(C_INT), intent(out) :: ndims
         type(C_PTR), intent(out) :: gdims
         integer(C_SIZE_T), intent(out) :: maplen
         type(C_PTR) :: map
         integer(C_INT), value, intent(in) :: f90_comm
       end function PIOc_readmap_from_f90
    end interface

    if (present(punit)) then
        call piodie(__PIO_FILE__, __LINE__,&
                    "Reading PIO decomposition failed. Specifying a unit number, punit, to read the decomposition from is no longer supported")
    end if

    ierr = PIOc_readmap_from_f90(trim(file)//C_NULL_CHAR, ndims, tgdims, maplen, tmap, comm);

    call c_f_pointer(tgdims, gdims, (/ndims/))
    call c_f_pointer(tmap, DOF, (/maplen/))
!    DOF = DOF+1
  end subroutine pio_readdof



end module pio_support
