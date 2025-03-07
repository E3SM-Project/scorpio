#ifndef __SPIO_TRACER_DECOMP_HPP__
#define __SPIO_TRACER_DECOMP_HPP__

#include <string>
#include <sstream>
#include <vector>
#include <utility>
#include <map>
#include <iostream>

#include "mpi.h"
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_logger.hpp"
#include "spio_ltimer.hpp"

namespace SPIO_Util{
  namespace Tracer{
    namespace Decomp_Utils{

      /* A I/O decomp logger : Stores the I/O decomposition in NetCDF output files */
      class Decomp_nc_logger{
        public:
          Decomp_nc_logger() : iosysid_(INVALID_IOSYSID), comm_(MPI_COMM_NULL), comm_rank_(INVALID_MPI_RANK), comm_sz_(INVALID_MPI_SIZE), ncid_(INVALID_NCID) {}
          Decomp_nc_logger(int iosysid) : iosysid_(iosysid), comm_(MPI_COMM_NULL), comm_rank_(INVALID_MPI_RANK), comm_sz_(INVALID_MPI_SIZE), ncid_(INVALID_NCID) {}
          Decomp_nc_logger(int iosysid, const std::string &name) : iosysid_(iosysid), comm_(MPI_COMM_NULL), comm_rank_(INVALID_MPI_RANK), comm_sz_(INVALID_MPI_SIZE), fname_(name), ncid_(INVALID_NCID) {}
          void init();
          Decomp_nc_logger &set_iosysid(int iosysid);
          void log_decomp(int ioid, const PIO_Offset *map, int mapsz);
          std::string get_log_fname(void ){ return fname_; }
          void finalize(void );
        private:
          static const int INVALID_NCID = -1;
          static const int INVALID_MPI_RANK = -1;
          static const int INVALID_MPI_SIZE = -1;
          static const int INVALID_IOSYSID = -2;

          int iosysid_;
          MPI_Comm comm_;
          int comm_rank_;
          int comm_sz_;
          std::string fname_;
          int ncid_;

          /* gsz_start_count_for_saved_decomps_[] = {decomp0_global_sz, decomp0_start, decomp0_count,
           *  decomp1_global_sz, decomp1_start, decomp1_count, ...}
           */
          std::vector<long long> gsz_start_count_for_saved_decomps_;
          std::map<PIO_Offset, int> dim_sz_to_id_;

          static int uniq_dimid_idx_;
          int get_dim_id(PIO_Offset dimsz);
          std::string get_trace_decomp_tmp_fname(int iosysid, MPI_Comm comm, int comm_rank, int mpi_wrank);
      };

    } // namespace DecompUtils

    std::string get_trace_decomp_fname(int iosysid, int mpi_wrank);
    Decomp_Utils::Decomp_nc_logger &get_trace_decomp_logger(int iosysid, int mpi_wrank);
    void trace_decomp(int iosysid, int mpi_wrank, int ioid, const PIO_Offset *map, int mapsz);
    void finalize_trace_decomp(int iosysid);

    //void finalize_file_trace_logger(int fh);
  } // namespace Tracer
} // namespace SPIO_Util

#endif /* __SPIO_TRACER_DECOMP_HPP__ */
