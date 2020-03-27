#include <stdio.h>
#include <mpi.h>
#include <pio.h>
#ifdef TIMING
#include <gptl.h>
#endif

#define NDIM 1
#define DIM_LEN 1024

int main(int argc, char* argv[])
{
  int world_rank;
  int world_size;

  int group;
  MPI_Comm comp_group_comm;
  int comp_group_comm_rank;
  int comp_group_comm_size;

  int format = PIO_IOTYPE_PNETCDF;
  int iosysid;
  int ncid;
  int varid;
  int iodesc;
  PIO_Offset *compdof = NULL;
  int *buffer = NULL;
  int dimid;
  PIO_Offset elements_per_pe;
  int dim_len[1] = {DIM_LEN};

  int i;

#ifdef TIMING
  GPTLinitialize();
#endif

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);

  if (world_size != 16) {
    if (world_rank == 0)
      fprintf(stderr, "Number of processors must be 16!\n");

    MPI_Finalize();
    return 1;
  }

  group = world_rank < 8 ? 1 : 2;
  MPI_Comm_split(MPI_COMM_WORLD, group, world_rank, &comp_group_comm);
  MPI_Comm_rank(comp_group_comm, &comp_group_comm_rank);
  MPI_Comm_size(comp_group_comm, &comp_group_comm_size);

  /* Create an IO system with two groups */
  /* comptasks = 8, iotasks = 4, stride = 2, base = 1 */
  /* io_comm #1 created based on comp_group_comm of group #1 */
  /* io_comm #2 created based on comp_group_comm of gourp #2 */
  PIOc_Init_Intracomm(comp_group_comm, 4, 2, 1, PIO_REARR_BOX, &iosysid);

  /*
    world rank  0: group #1, comp_group_comm_rank = 0, member of io_comm #1 = no
    world rank  1: group #1, comp_group_comm_rank = 1, member of io_comm #1 = yes, io rank = 0
    world rank  2: group #1, comp_group_comm_rank = 2, member of io_comm #1 = no
    world rank  3: group #1, comp_group_comm_rank = 3, member of io_comm #1 = yes, io rank = 1
    world rank  4: group #1, comp_group_comm_rank = 4, member of io_comm #1 = no
    world rank  5: group #1, comp_group_comm_rank = 5, member of io_comm #1 = yes, io rank = 2
    world rank  6: group #1, comp_group_comm_rank = 6, member of io_comm #1 = no
    world rank  7: group #1, comp_group_comm_rank = 7, member of io_comm #1 = yes, io rank = 3

    world rank  8: group #2, comp_group_comm_rank = 0, member of io_comm #2 = no
    world rank  9: group #2, comp_group_comm_rank = 1, member of io_comm #2 = yes, io rank = 0
    world rank 10: group #2, comp_group_comm_rank = 2, member of io_comm #2 = no
    world rank 11: group #2, comp_group_comm_rank = 3, member of io_comm #2 = yes, io rank = 1
    world rank 12: group #2, comp_group_comm_rank = 4, member of io_comm #2 = no
    world rank 13: group #2, comp_group_comm_rank = 5, member of io_comm #2 = yes, io rank = 2
    world rank 14: group #2, comp_group_comm_rank = 6, member of io_comm #2 = no
    world rank 15: group #2, comp_group_comm_rank = 7, member of io_comm #2 = yes, io rank = 3
  */

  /* Both groups create the test file */
  PIOc_createfile(iosysid, &ncid, &format, "test_file.nc", PIO_CLOBBER);

  /*
    world rank  1: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #1
    world rank  3: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #1
    world rank  5: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #1
    world rank  7: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #1
    world rank  9: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #2
    world rank 11: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #2
    world rank 13: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #2
    world rank 15: collectively calls ncmpi_create(MPI_Comm comm, ...) with io_comm #2
  */

  PIOc_def_dim(ncid, "row", DIM_LEN, &dimid);
  PIOc_def_var(ncid, "foo", PIO_INT, NDIM, &dimid, &varid);
  PIOc_enddef(ncid);

  /* group #1 and group #2 use the same decomposition map (evenly distribution to 8 compute tasks) */
  elements_per_pe = DIM_LEN / comp_group_comm_size;
  compdof = malloc(elements_per_pe * sizeof(PIO_Offset));
  for (i = 0; i < elements_per_pe; i++)
    compdof[i] = comp_group_comm_rank * elements_per_pe + i + 1;

  PIOc_InitDecomp(iosysid, PIO_INT, 1, dim_len, (PIO_Offset)elements_per_pe,
                  compdof, &iodesc, NULL, NULL, NULL);

  /* group #1 and group #2 use the same buffer content for writing */
  buffer = malloc(elements_per_pe * sizeof(int));
  for (i = 0; i < elements_per_pe; i++)
    buffer[i] = compdof[i];

  free(compdof);

  PIOc_write_darray(ncid, varid, iodesc, elements_per_pe, buffer, NULL);

  /*
    After rearranging data from 16 compute tasks to 8 IO tasks,
    Dupliate/Overlapped writing begins.

    world rank  1: write with start =   0, count = 256
    world rank  3: write with start = 256, count = 256
    world rank  5: write with start = 512, count = 256
    world rank  7: write with start = 768, count = 256
    world rank  9: write with start =   0, count = 256, same buffer data as world rank 1
    world rank 11: write with start = 256, count = 256, same buffer data as world rank 3
    world rank 13: write with start = 512, count = 256, same buffer data as world rank 5 
    world rank 15: write with start = 768, count = 256, same buffer data as world rank 7
  */

  PIOc_closefile(ncid);

  PIOc_freedecomp(iosysid, iodesc);

  free(buffer);

  PIOc_finalize(iosysid);

  MPI_Comm_free(&comp_group_comm);

  MPI_Finalize();

#ifdef TIMING
  GPTLfinalize();
#endif

  return 0;
}
