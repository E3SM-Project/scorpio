#ifndef __SPIO_DECOMP_MAP_INFO_HPP__
#define __SPIO_DECOMP_MAP_INFO_HPP__

#include <vector>
#include <map>
#include <string>
#include <iostream>
#include <sstream>
#include <iterator>
#include <algorithm>

#include "mpi.h"

namespace SPIO_Util{
  namespace Decomp_Util{
    class Decomp_map_info_pool{
      public:
        void add_decomp_map_info(int decomp_map_id, const char *decomp_map_fname){
          decomp_map_info_[decomp_map_id] = std::string(decomp_map_fname);
        }
        void add_var_info(int decomp_map_id, int fid, const char *fname,
              int vid, const char *vname){
          Var_info vinfo = {vid, decomp_map_id, decomp_map_info_[decomp_map_id], std::string(vname)};

          finfo_[fid].fid = fid;
          finfo_[fid].fname = std::string(fname);
          finfo_[fid].vinfo[vid] = vinfo;
        }

        std::string to_string(void ) const{
          std::ostringstream ostr;
          for(std::map<int, File_info>::const_iterator finfo_iter = finfo_.cbegin();
                finfo_iter != finfo_.cend(); ++finfo_iter){
            ostr << (*finfo_iter).second.to_string() << "\n";
          }
          return ostr.str();
        }
      private:
        struct Var_info{
          int vid;
          int decomp_map_id;
          std::string decomp_map_fname;
          std::string vname;

          std::string to_string(void ) const{
            return std::string("{") +
                    std::string("vid = ") + std::to_string(vid) +
                    std::string(", decomp_map_id = ") + std::to_string(decomp_map_id) +
                    std::string(", decomp_map_fname = ") + decomp_map_fname +
                    std::string(", vname = ") + vname +
                    std::string("}");
          }
        };

        struct File_info{
          int fid;
          std::string fname;
          std::map<int, Var_info> vinfo;

          std::string to_string(void ) const{
            std::ostringstream ostr;
            ostr << "{";
            ostr << "fid = " << fid;
            ostr << ", fname = " << fname.c_str();
            ostr << ", vinfo = \n";
            for(std::map<int, Var_info>::const_iterator vinfo_iter = vinfo.cbegin();
                  vinfo_iter != vinfo.cend(); ++vinfo_iter){
              ostr << (*vinfo_iter).second.to_string() << "\n";
            }
            ostr << "}";
            return ostr.str();
          }
        };

        std::map<int, std::string> decomp_map_info_;
        std::map<int, File_info> finfo_;
    };

    class Decomp_map_info_pool_manager{
      public:
        static Decomp_map_info_pool *get_decomp_map_info_pool(void );
        ~Decomp_map_info_pool_manager();
      private:
        static Decomp_map_info_pool *dpool_;
    };

    extern Decomp_map_info_pool_manager gdpool_mgr;

    void serialize_decomp_map_info_pool(MPI_Comm comm);
  } // SPIO_Util
} // Decomp_Util

#endif // __SPIO_DECOMP_MAP_INFO_HPP__
