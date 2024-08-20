#ifndef __SPIO_TRACER_MDATA_HPP__
#define __SPIO_TRACER_MDATA_HPP__

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

    class Tracer_mdata{
      public:
        void add_iosysid(int iosysid){ if(iosysid > 0) { iosysid_infos_[iosysid].iosysid_ = iosysid; } }
        void add_ioid(int iosysid) { iosysid_infos_[iosysid].nioids_++; }
        void add_ncid(int iosysid, int ncid) { iosysid_infos_[iosysid].ncid_infos_[ncid].ncid_ = ncid; }
        void add_dimid(int iosysid, int ncid, int dimid) { iosysid_infos_[iosysid].ncid_infos_[ncid].dimids_[dimid] = true; }
        void add_varid(int iosysid, int ncid, int varid) { iosysid_infos_[iosysid].ncid_infos_[ncid].varids_[varid] = true; }

        std::string get_mdata(int iosysid) const{
          return iosysid_infos_.at(iosysid).get_info();
        }

        std::string get_mdata(void ) const{
          std::string info = std::string("[") +
                              std::string("niosysids=") + std::to_string(iosysid_infos_.size()) + std::string(",") +
                              std::string("iosysids=[");
          bool first_element = true;
          for(std::map<int, Iosysid_info>::const_iterator citer = iosysid_infos_.cbegin(); citer != iosysid_infos_.cend(); ++citer){
            if(!first_element){
              info += std::string(",");
            }
            else{
              first_element = false;
            }

            info += (*citer).second.get_info();
          }
          info += std::string("]]");
          return info;
        }
      private:
        /* FIXME: Decide if we need to leave the internal classes with all public member variables */
        class Ncid_info{
          public:
            Ncid_info():ncid_(0){}
            Ncid_info(int ncid):ncid_(ncid){}

            std::string get_info(void ) const {
              return  std::string("[") +
                      std::string("ncid=") + std::to_string(ncid_) + std::string(",") +
                      std::string("ndimids=") + std::to_string(dimids_.size()) + std::string(",") +
                      std::string("nvarids=") + std::to_string(varids_.size()) +
                      std::string("]");
            }

            int ncid_;
            std::map<int, bool> dimids_;
            std::map<int, bool> varids_;
        };

        class Iosysid_info{
          public:
            Iosysid_info():iosysid_(INVALID_IOSYSID), nioids_(0){}
            Iosysid_info(int iosysid):iosysid_(iosysid), nioids_(0){}

            std::string get_info(void ) const {
              std::string info =  std::string("[") +
                                  std::string("iosysid=") + std::to_string(iosysid_) + std::string(",") +
                                  std::string("nioids=") + std::to_string(nioids_) + std::string(",") +
                                  std::string("nncids=") + std::to_string(ncid_infos_.size()) + std::string(",") +
                                  std::string("ncids=[");
              bool first_element = true;
              for(std::map<int, Ncid_info>::const_iterator citer = ncid_infos_.cbegin(); citer != ncid_infos_.cend(); ++citer){
                if(!first_element){
                  info += std::string(",");
                }
                else{
                  first_element = false;
                }

                info += (*citer).second.get_info();
              }
              info += std::string("]]");
              return info;
            }

            int iosysid_;
            int nioids_;
            std::map<int, Ncid_info> ncid_infos_;

          private:
            static const int INVALID_IOSYSID = -2;
        };

        std::map<int, Iosysid_info> iosysid_infos_;
    };

    std::string get_trace_mdata_fname(int iosysid, int mpi_wrank);

    std::string get_mpi_comm_info(MPI_Comm comm);
    SPIO_Util::Logger::MPI_logger<std::ofstream> &get_iosys_trace_mdata_logger(int iosysid, int mpi_wrank);
    void finalize_iosys_trace_mdata_logger(std::string iosys_key);
    //void finalize_file_trace_logger(int fh);
  } // namespace Tracer
} // namespace SPIO_Util

#endif /* __SPIO_TRACER_MDATA_HPP__ */
