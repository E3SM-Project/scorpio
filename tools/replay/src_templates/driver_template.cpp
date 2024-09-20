#include <iostream>
#include <string>
#include <cassert>
#include "mpi.h"

#include "spio_replay_driver.hpp"

int main(int argc, char *argv[])
{
  int rank = -1, sz = 0;
  int ret = MPI_SUCCESS;
  std::string banner_line = "=========================================";
  std::string tool_banner = banner_line + "\n" +
                            "\tSCORPIO REPLAY TOOL\n" +
                            banner_line;

  ret = MPI_Init(&argc, &argv);
  assert(ret == MPI_SUCCESS);

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  assert(ret == MPI_SUCCESS);

  ret = MPI_Comm_size(MPI_COMM_WORLD, &sz);
  assert(ret == MPI_SUCCESS);

  if(rank == 0){
    std::cout << tool_banner.c_str() << "\n";
  }

  __DRIVER_RUN_SEQUENCE__

  if(rank == 0){
    std::cout << banner_line.c_str() << "\n";
  }

  ret = MPI_Finalize();
  assert(ret == MPI_SUCCESS);
}

