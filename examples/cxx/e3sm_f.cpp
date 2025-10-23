#include "e3sm_fgi_utils.hpp"

int E3SM_FGI::test_e3sm_f_case(MPI_Comm comm, const std::vector<int> &iotypes,
      const std::vector<int> &rearrs, int nioprocs)
{
  int comm_rank, comm_sz, ret;
  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  assert(nioprocs <= comm_sz);

  Util::zip_for_each(iotypes.cbegin(), iotypes.cend(), rearrs.cbegin(), rearrs.cend(),
    [](int iotype, int rearr){
      Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing F Case, iotype = " + Util::GVars::iotype2str(iotype) + ", rearr = " + Util::GVars::rearr2str(rearr) + "\n");
    });

  return PIO_NOERR;
}

