#include <iostream>
#include <vector>
#include <functional>
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

  std::vector<spio_replay_driver::iosys> iosys_infos = {__DRIVER_IOSYS_INFO__};
  for(std::size_t i = 0; i < iosys_infos.size(); i++){
    int ret = iosys_infos[i].init();
    if(ret != 0){
      std::cerr << "ERROR: Error initializing I/O system : " << i
        << "(" << iosys_infos[i].info.c_str() << ")\n";
    }
  }

  const std::vector<int> iosys_run_idx = {__DRIVER_RUN_SEQUENCE__};
  for(std::size_t i = 0; i < iosys_run_idx.size(); i++){
    int ret = iosys_infos[i].run(iosys_infos[i].phase++);
    if(ret != 0){
      std::cerr << "ERROR: Error running I/O system : " << i
        << "(" << iosys_infos[i].info.c_str() << ")\n";
    }
  }

  for(std::size_t i = 0; i < iosys_infos.size(); i++){
    int ret = iosys_infos[i].finalize();
    if(ret != 0){
      std::cerr << "ERROR: Error finalizing I/O system : " << i
        << "(" << iosys_infos[i].info.c_str() << ")\n";
    }
  }

  if(rank == 0){
    std::cout << banner_line.c_str() << "\n";
  }

  ret = MPI_Finalize();
  assert(ret == MPI_SUCCESS);

  return 0;
}
