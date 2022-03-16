module pioExample
    use pio, only : PIO_init, PIO_rearr_subset, iosystem_desc_t, file_desc_t
    use pio, only : PIO_finalize, PIO_iotype_netcdf4p, PIO_createfile
    use pio, only : PIO_char, var_desc_t, PIO_def_dim, PIO_def_var, PIO_enddef
    use pio, only : PIO_put_var, PIO_closefile, io_desc_t
    use pio, only : PIO_clobber

    implicit none

    private

    include 'mpif.h'

    type, public :: pioExampleClass
        integer :: myRank
        integer :: ntasks
        integer :: niotasks
        integer :: stride        
        integer :: numAggregator
        integer :: optBase
        type(iosystem_desc_t) :: pioIoSystem
        type(file_desc_t)     :: pioFileDesc
        type(var_desc_t)      :: pioVar
        integer               :: iotype
        integer, dimension(3) :: dimIds
        integer, dimension(3) :: dimLens
        character(len=255) :: fileName

    contains

        procedure,  public  :: init
        procedure,  public  :: createFile
        procedure,  public  :: defineVar
        procedure,  public  :: writeVar
        procedure,  public  :: closeFile
        procedure,  public  :: cleanUp

    end type pioExampleClass

contains

    subroutine init(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: ierr

        call MPI_Init(ierr)
        call MPI_Comm_rank(MPI_COMM_WORLD, this%myRank, ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, this%ntasks , ierr)

        this%stride        = 2
        this%numAggregator = 0
        this%optBase       = 0
        this%iotype        = PIO_iotype_netcdf4p
        this%fileName      = "test_netcdf4p.nc"
        this%dimLens(1)     = 16
        this%dimLens(2)     = 2
        this%dimLens(3)     = 1

        this%niotasks = this%ntasks / this%stride

        call PIO_init(this%myRank,      & ! MPI rank
            MPI_COMM_WORLD,             & ! MPI communicator
            this%niotasks,              & ! Number of iotasks (ntasks/stride)
            this%numAggregator,         & ! number of aggregators to use
            this%stride,                & ! stride
            PIO_rearr_subset,           & ! do not use any form of rearrangement
            this%pioIoSystem,           & ! iosystem
            base=this%optBase)            ! base (optional argument)

    end subroutine init

    subroutine createFile(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: retVal

        retVal = PIO_createfile(this%pioIoSystem, this%pioFileDesc, this%iotype, trim(this%fileName), PIO_clobber)

    end subroutine createFile

    subroutine defineVar(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: retVal

        retVal = PIO_def_dim(this%pioFileDesc, 'slen', this%dimLens(1) , this%dimIds(1))
        retVal = PIO_def_dim(this%pioFileDesc, 'row', this%dimLens(2) , this%dimIds(2))
        retVal = PIO_def_dim(this%pioFileDesc, 'col', this%dimLens(3) , this%dimIds(3))

        retVal = PIO_def_var(this%pioFileDesc, 'var2d', PIO_char, (/this%dimIds/), this%pioVar)

        retVal = PIO_enddef(this%pioFileDesc)

    end subroutine defineVar

    subroutine writeVar(this)

    implicit none

        class(pioExampleClass), intent(inout) :: this

        integer, dimension(3) :: start_3D, count_3D
        character(len=1), dimension(:), allocatable :: tmp_str

        integer :: retVal

        start_3D(1) = 5
        start_3D(2) = 1
        start_3D(3) = 1
        count_3D(1) = 1
        count_3D(2) = 2
        count_3D(3) = 1

        allocate(tmp_str(2))
        tmp_str(1) = 'X'
        tmp_str(2) = 'Y'
 
        retVal = PIO_put_var(this%pioFileDesc, this%pioVar, start_3D, count_3D, tmp_str);
        deallocate(tmp_str)

    end subroutine writeVar

    subroutine closeFile(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        call PIO_closefile(this%pioFileDesc)

    end subroutine closeFile

    subroutine cleanUp(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: ierr

        call PIO_finalize(this%pioIoSystem, ierr)
        call MPI_Finalize(ierr)

    end subroutine cleanUp

end module pioExample

program main

    use pioExample, only : pioExampleClass

    implicit none

    type(pioExampleClass) :: pioExInst

    call pioExInst%init()
    call pioExInst%createFile()
    call pioExInst%defineVar()
    call pioExInst%writeVar()
    call pioExInst%closeFile()
    call pioExInst%cleanUp()

end program main
