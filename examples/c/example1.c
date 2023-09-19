#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#ifdef TIMING
#include <gptl.h>
#endif

/*
A simple "3 x 6" 2D array, with 1-based indices (offsets) shown as below:
 1   2   3   4   5   6
 7   8   9  10  11  12
13  14  15  16  17  18

[Description of the decompostion map]
rank 0 writes 5 values at: {11, 10, 14, 13, 12}
rank 1 writes 4 values at: {17, 18, 15, 16}
rank 2 writes 5 values at: {3, 1, 5, 2, 4}
rank 3 writes 4 values at: {9, 8, 6, 7}

For rank i which writes at offset j, define the value to be written as "100 * (i + 1) + j".
This can make verification easier.
301   302   303   304   305   406
407   408   409   110   111   112
113   114   215   216   217   218

Expected ncdump:

netcdf test_decomp {
dimensions:
	row = 3 ;
	col = 6 ;
variables:
	int var(row, col) ;
data:

 var =
  301, 302, 303, 304, 305, 406,
  407, 408, 409, 110, 111, 112,
  113, 114, 215, 216, 217, 218 ;
}
*/
  
int main(int argc, char* argv[])
{
    int my_rank;
    int ntasks;
    int format = PIO_IOTYPE_PNETCDF;
    int niotasks;
    int ioproc_stride = 1;
    int ioproc_start = 0;
    int dimids[2];
    PIO_Offset elements_per_pe;
    int dim_lens[2] = {3, 6};
    int iosysid;
    int ncid;
    int varid;
    int ioid;
    int write_buffer[5];
    int read_buffer[5];
    PIO_Offset compdof[5];

#ifdef TIMING    
  GPTLinitialize();
#endif   

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    if (ntasks != 4)
        fprintf(stderr, "Number of processors must be 4!\n");

    niotasks = ntasks; 
    PIOc_Init_Intracomm(MPI_COMM_WORLD, niotasks, ioproc_stride, ioproc_start, PIO_REARR_BOX, &iosysid);

    if (my_rank == 0)
    {
        elements_per_pe = 5;
        compdof[0] = 11; compdof[1] = 10; compdof[2] = 14; compdof[3] = 13; compdof[4] = 12;
    }
    else if (my_rank == 1)
    {
        elements_per_pe = 4;
        compdof[0] = 17; compdof[1] = 18; compdof[2] = 15; compdof[3] = 16;
    }
    else if (my_rank == 2)
    {
        elements_per_pe = 5;
        compdof[0] = 3; compdof[1] = 1; compdof[2] = 5; compdof[3] = 2; compdof[4] = 4;
    }
    else if (my_rank == 3)
    {
        elements_per_pe = 4;
        compdof[0] = 9; compdof[1] = 8; compdof[2] = 6; compdof[3] = 7;
    }

    /* Set write buffer: for rank i which writes at index j, we know the value to be written is "100 * (i + 1) + j" */
    /* Also initialize read buffer */
    for (int i = 0; i < elements_per_pe; i++)
    {
         write_buffer[i] = 100 * (my_rank + 1) + compdof[i];
         read_buffer[i] = -1;
    }

    PIOc_InitDecomp(iosysid, PIO_INT, 2, dim_lens, elements_per_pe, compdof, &ioid, NULL, NULL, NULL);

    PIOc_createfile(iosysid, &ncid, &format, "test_decomp.nc", PIO_CLOBBER);
    PIOc_def_dim(ncid, "row", (PIO_Offset)dim_lens[0], &dimids[0]);
    PIOc_def_dim(ncid, "col", (PIO_Offset)dim_lens[1], &dimids[1]);
    PIOc_def_var(ncid, "var", PIO_INT, 2, dimids, &varid);
    PIOc_enddef(ncid);

    PIOc_write_darray(ncid, varid, ioid, elements_per_pe, write_buffer, NULL);

    PIOc_closefile(ncid);

    PIOc_openfile(iosysid, &ncid, &format, "test_decomp.nc", PIO_NOWRITE);

    PIOc_inq_varid(ncid, "var", &varid);
    PIOc_read_darray(ncid, varid, ioid, elements_per_pe, read_buffer);

    for (int i = 0; i < elements_per_pe; i++)
    {
        if (read_buffer[i] != write_buffer[i])
        {
            printf("rank = %d, read wrong data at index %d\n", my_rank, i);
            return -1;
        }
    }

    PIOc_closefile(ncid);
	
    PIOc_freedecomp(iosysid, ioid);

    PIOc_finalize(iosysid);

    MPI_Finalize();

#ifdef TIMING    
  GPTLfinalize();
#endif 

    return 0;
}
