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

      class Decomp_nc_logger{
        public:
          Decomp_nc_logger() : iosysid_(INVALID_IOSYSID), wrank_(INVALID_MPI_RANK), wsz_(INVALID_MPI_SIZE), comm_(MPI_COMM_NULL), comm_rank_(INVALID_MPI_RANK), comm_sz_(INVALID_MPI_SIZE), ncid_(INVALID_NCID) {}
          Decomp_nc_logger(int iosysid) : iosysid_(iosysid), wrank_(INVALID_MPI_RANK), wsz_(INVALID_MPI_SIZE), comm_(MPI_COMM_NULL), comm_rank_(INVALID_MPI_RANK), comm_sz_(INVALID_MPI_SIZE), ncid_(INVALID_NCID) {}
          Decomp_nc_logger(int iosysid, const std::string &name) : iosysid_(iosysid), wrank_(INVALID_MPI_RANK), wsz_(INVALID_MPI_SIZE), comm_(MPI_COMM_NULL), comm_rank_(INVALID_MPI_RANK), comm_sz_(INVALID_MPI_SIZE), fname_(name), ncid_(INVALID_NCID) {}
          void init();
          Decomp_nc_logger &set_iosysid(int iosysid);
          void log_decomp(int ioid, const PIO_Offset *map, int mapsz);
          void finalize(void );
        private:
          static const int INVALID_NCID = -1;
          static const int INVALID_MPI_RANK = -1;
          static const int INVALID_MPI_SIZE = -1;
          static const int INVALID_IOSYSID = -2;

          int iosysid_;
          int wrank_;
          int wsz_;
          MPI_Comm comm_;
          int comm_rank_;
          int comm_sz_;
          std::string fname_;
          int ncid_;

          /* gsz_start_count_for_saved_decomps_[] = {decomp0_global_sz, decomp0_start, decomp0_count,
           *  decomp1_global_sz, decomp1_start, decomp1_count, ...}
           */
          std::vector<long long> gsz_start_count_for_saved_decomps_;
          std::map<int, int> dim_sz_to_id_;

          int get_dim_id(int dimsz){
            int dimid = INVALID_NCID, ret = 0;
            static int dimid_idx = 0;

            std::map<int, int>::iterator dim_id_iter = dim_sz_to_id_.find(dimsz);
            if(dim_id_iter == dim_sz_to_id_.end()){
              assert(ncid_ != INVALID_NCID);

              ret = ncmpi_redef(ncid_);
              assert(ret == NC_NOERR);

              std::string dimid_name = std::string("dimid_") + std::to_string(dimid_idx++);

              ret = ncmpi_def_dim(ncid_, dimid_name.c_str(), dimsz, &dimid);
              assert(ret == NC_NOERR);

              dim_sz_to_id_[dimsz] = dimid;

              ret = ncmpi_enddef(ncid_);
              assert(ret == NC_NOERR);
            }
            else{
              dimid = dim_id_iter->second;
            }
            return dimid;
          }
      };

    } // namespace DecompUtils

    std::string get_trace_decomp_fname(int iosysid, int mpi_wrank);
    void trace_decomp(int iosysid, int mpi_wrank, int ioid, const PIO_Offset *map, int mapsz);
    void finalize_trace_decomp(int iosysid);

    //void finalize_file_trace_logger(int fh);
  } // namespace Tracer
} // namespace SPIO_Util

#endif /* __SPIO_TRACER_DECOMP_HPP__ */
