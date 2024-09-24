#include <iostream>
#include <vector>
#include <functional>
#include <cassert>
#include "mpi.h"

#include "spio_replay_iosys_trace___IOSYSID__.hpp"

namespace gvars___IOSYSID__{
  static MPI_Comm iosys_comm = MPI_COMM_NULL;
  static bool is_proc_in_iosys = false;
}

int iosys_init___IOSYSID__(void )
{
  int rank = -1, sz = 0;
  int ret = MPI_SUCCESS;

  /* The global map of colors for the I/O system comm */
  std::vector<int> map_colors = {__MPI_PROC_MAP_COLORS__};
  std::vector<int> map_keys = {__MPI_PROC_MAP_KEYS__};

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  assert(ret == MPI_SUCCESS);

  ret = MPI_Comm_size(MPI_COMM_WORLD, &sz);
  assert(ret == MPI_SUCCESS);

  assert(map_colors.size() == sz);
  assert(map_keys.size() == sz);

  if(map_colors[rank] >= 0){
    gvars___IOSYSID__::is_proc_in_iosys = true;
  }

  ret = MPI_Comm_split(MPI_COMM_WORLD, map_colors[rank], map_keys[rank],
          &(gvars___IOSYSID__::iosys_comm));
  assert(ret == MPI_SUCCESS);

  return 0;
}

int iosys_finalize___IOSYSID__(void )
{
  int ret = MPI_SUCCESS;

  ret = MPI_Comm_free(&(gvars___IOSYSID__::iosys_comm));
  assert(ret == MPI_SUCCESS);

  return 0;
}
