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

rank 0 writes 5 values at: {1, 2, 3, 4, 5}
rank 1 writes 4 values at: {6, 7, 8, 9}
rank 2 writes 5 values at: {10, 11, 12, 13, 14}
rank 3 writes 4 values at: {15, 16, 17, 18}

For rank i which writes at index j, the value to be written is "100 * (i + 1) + j"
101   102   103   104   105   206
207   208   209   310   311   312
313   314   415   416   417   418

Decomposition A uses ordered indices (offests, dof) to write variable A
Decomposition B uses non-ordered indices (offests, dof) to write variable B

Expected ncdump:

netcdf test_decomp {
dimensions:
	row = 3 ;
	col = 6 ;
variables:
	int VarA(row, col) ;
	int VarB(row, col) ;
data:

 VarA =
  101, 102, 103, 104, 105, 206,
  207, 208, 209, 310, 311, 312,
  313, 314, 415, 416, 417, 418 ;

 VarB =
  101, 102, 103, 104, 105, 206,
  207, 208, 209, 310, 311, 312,
  313, 314, 415, 416, 417, 418 ;
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
    int varidA;
    int varidB;
    int ioidA;
    int ioidB;
    int bufferA[5];
    int bufferB[5];
    PIO_Offset compdofA[5];
    PIO_Offset compdofB[5];

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

    /* Decompostion A uses ordered indices (offests, dof), decomposition B uses shuffled indices */
    if (my_rank == 0)
    {
        elements_per_pe = 5;
        compdofA[0] = 1; compdofA[1] = 2; compdofA[2] = 3; compdofA[3] = 4; compdofA[4] = 5;
        compdofB[0] = 3; compdofB[1] = 1; compdofB[2] = 5; compdofB[3] = 2; compdofB[4] = 4;
    }
    else if (my_rank == 1)
    {
        elements_per_pe = 4;
        compdofA[0] = 6; compdofA[1] = 7; compdofA[2] = 8; compdofA[3] = 9;
        compdofB[0] = 9; compdofB[1] = 8; compdofB[2] = 6; compdofB[3] = 7;
    }
    else if (my_rank == 2)
    {
        elements_per_pe = 5;
        compdofA[0] = 10; compdofA[1] = 11; compdofA[2] = 12; compdofA[3] = 13; compdofA[4] = 14;
        compdofB[0] = 11; compdofB[1] = 10; compdofB[2] = 14; compdofB[3] = 13; compdofB[4] = 12;
    }
    else if (my_rank == 3)
    {
        elements_per_pe = 4;
        compdofA[0] = 15; compdofA[1] = 16; compdofA[2] = 17; compdofA[3] = 18;
        compdofB[0] = 17; compdofB[1] = 18; compdofB[2] = 15; compdofB[3] = 16;
    }

    /* Set write buffers: for rank i which writes at index j, the value to be written is "100 * (i + 1) + j" */
    for (int i = 0; i < elements_per_pe; i++)
    {
        bufferA[i] = 100 * (my_rank + 1) + compdofA[i];
        bufferB[i] = 100 * (my_rank + 1) + compdofB[i];
    }

    PIOc_InitDecomp(iosysid, PIO_INT, 2, dim_lens, elements_per_pe, compdofA, &ioidA, NULL, NULL, NULL);
    PIOc_InitDecomp(iosysid, PIO_INT, 2, dim_lens, elements_per_pe, compdofB, &ioidB, NULL, NULL, NULL);

    PIOc_createfile(iosysid, &ncid, &format, "test_decomp.nc", PIO_CLOBBER);
    PIOc_def_dim(ncid, "row", (PIO_Offset)dim_lens[0], &dimids[0]);
    PIOc_def_dim(ncid, "col", (PIO_Offset)dim_lens[1], &dimids[1]);
    PIOc_def_var(ncid, "VarA", PIO_INT, 2, dimids, &varidA);
    PIOc_def_var(ncid, "VarB", PIO_INT, 2, dimids, &varidB);
    PIOc_enddef(ncid);

    PIOc_write_darray(ncid, varidA, ioidA, elements_per_pe, bufferA, NULL);
    PIOc_write_darray(ncid, varidB, ioidB, elements_per_pe, bufferB, NULL);

    PIOc_closefile(ncid);
	
    PIOc_freedecomp(iosysid, ioidA);
    PIOc_freedecomp(iosysid, ioidB);

    PIOc_finalize(iosysid);

    MPI_Finalize();

#ifdef TIMING    
  GPTLfinalize();
#endif 

    return 0;
}
