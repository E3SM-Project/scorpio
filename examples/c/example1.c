#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#ifdef TIMING
#include <gptl.h>
#endif

int main(int argc, char* argv[])
{
  int my_rank;
  int ntasks;
  int format = PIO_IOTYPE_PNETCDF;
  int cmode;
  int iosysid;
  int ncid;

#ifdef TIMING    
  GPTLinitialize();
#endif   

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

  PIOc_Init_Intracomm(MPI_COMM_WORLD, ntasks, 1, 0, PIO_REARR_SUBSET, &iosysid);

  if (my_rank == 0)
    cmode = PIO_CLOBBER;
  else
    cmode = PIO_CLOBBER | PIO_WRITE;

  PIOc_createfile(iosysid, &ncid, &format, "test_pnetcdf_file_mode.nc", cmode);

  PIOc_closefile(ncid);

  PIOc_finalize(iosysid);

  MPI_Finalize();

#ifdef TIMING    
  GPTLfinalize();
#endif 

  return 0;
}

