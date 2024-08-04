#ifndef __SPIO_TRACER_MDATA_HPP__
#define __SPIO_TRACER_MDATA_HPP__

#include <string>
#include <sstream>
#include <vector>
#include <utility>

#include "mpi.h"
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_logger.hpp"
#include "spio_ltimer.hpp"

namespace SPIO_Util{
  namespace Tracer{

    std::string get_trace_decomp_fname(int iosysid, int mpi_wrank);
    std::string get_trace_mdata_fname(int iosysid, int mpi_wrank);

    std::string get_mpi_comm_info(MPI_Comm comm);
    SPIO_Util::Logger::MPI_logger<std::ofstream> &get_iosys_trace_mdata_logger(int iosysid, int mpi_wrank);
    void finalize_iosys_trace_mdata_logger(std::string iosys_key);
    //void finalize_file_trace_logger(int fh);
  } // namespace Tracer
} // namespace SPIO_Util

#endif /* __SPIO_TRACER_MDATA_HPP__ */
