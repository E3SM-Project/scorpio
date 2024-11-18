#ifndef __PIO_REARR_UTILS_HPP__
#define __PIO_REARR_UTILS_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_types.hpp"

#include <vector>

namespace SPIO_Util{
  namespace Rearr_Util{
    int gatherw(const void *sendbuf, int sendcount,
                  MPI_Datatype sendtype,
                  void *recvbuf, const std::vector<int> &recvcounts,
                  const std::vector<int> &rdispls,
                  const std::vector<MPI_Datatype> &recvtypes,
                  int root, MPI_Comm comm, const rearr_comm_fc_opt_t *fc);
    int scatterw(const void *sendbuf, const std::vector<int> &sendcounts,
                  const std::vector<int> &sdispls,
                  const std::vector<MPI_Datatype> &sendtypes,
                  void *recvbuf, int recvcount,
                  MPI_Datatype recvtype,
                  int root, MPI_Comm comm, const rearr_comm_fc_opt_t *fc);

    int alltoallw(const void *sendbuf, const int *sendcounts,
                  const int *sdispls, const MPI_Datatype *sendtypes,
                  void *recvbuf, const int *recvcounts,
                  const int *rdispls, const MPI_Datatype *recvtypes,
                  MPI_Comm comm, const rearr_comm_fc_opt_t *fc);

    int alltoall(const void *sendbuf, int sendcount,
                  MPI_Datatype sendtype,
                  void *recvbuf, int recvcount,
                  MPI_Datatype recvtype,
                  MPI_Comm comm, const rearr_comm_fc_opt_t *fc);

  } // namespace Rearr_Util
} // namespace SPIO_Util

#endif // __PIO_REARR_UTILS_HPP__
