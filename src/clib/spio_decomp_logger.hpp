#ifndef __SPIO_DECOMP_LOGGER__
#define __SPIO_DECOMP_LOGGER__

#include <string>
#include <vector>
#include <map>
#include <utility>
#include <cassert>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <cctype>

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "mpi.h"

namespace SPIO_Util{
  namespace Decomp_Util{

    /* FIXME: Allow multiple decompositions dumped into the same file */
    class Decomp_logger{
      public:
        Decomp_logger(MPI_Comm comm, MPI_Comm io_comm, MPI_Comm agg_comm, std::string &log_fname):comm_(comm), io_comm_(io_comm), agg_comm_(agg_comm), is_io_proc_(io_comm != MPI_COMM_NULL), comm_sz_(-1), comm_rank_(-1), io_comm_sz_(-1), io_comm_rank_(-1), agg_comm_sz_(-1), agg_comm_rank_(-1), log_fname_(log_fname), mode_(OPEN_MODE_RD){
          int ret = MPI_SUCCESS;

          assert(comm_ != MPI_COMM_NULL);
          assert(agg_comm_ != MPI_COMM_NULL);

          ret = MPI_Comm_size(comm_, &comm_sz_); assert(ret == MPI_SUCCESS);
          ret = MPI_Comm_rank(comm_, &comm_rank_); assert(ret == MPI_SUCCESS);
          if(is_io_proc_){
            ret = MPI_Comm_size(io_comm_, &io_comm_sz_); assert(ret == MPI_SUCCESS);
            ret = MPI_Comm_rank(io_comm_, &io_comm_rank_); assert(ret == MPI_SUCCESS);
          }
          ret = MPI_Comm_size(agg_comm_, &agg_comm_sz_); assert(ret == MPI_SUCCESS);
          ret = MPI_Comm_rank(agg_comm_, &agg_comm_rank_); assert(ret == MPI_SUCCESS);
        }

        Decomp_logger &read_only(void ){
          mode_ |= OPEN_MODE_RD;
          mode_ &= ~OPEN_MODE_WR;
          return *this;
        }

        Decomp_logger &write_only(void ){
          mode_ |= OPEN_MODE_WR;
          mode_ &= ~OPEN_MODE_RD;
          return *this;
        }

        bool is_read_only(void ) const {
          return (((mode_ & OPEN_MODE_WR) == 0) && ((mode_ & OPEN_MODE_RD) != 0));
        }
        bool is_write_only(void ) const {
          return (((mode_ & OPEN_MODE_RD) == 0) && ((mode_ & OPEN_MODE_WR) != 0));
        }

        std::string get_log_fname(void ) const { return log_fname_; }

        virtual Decomp_logger &open(void ) = 0;

        virtual void get_info(std::string &version, int &nprocs, int &ngdims, PIO_Offset &lcompmap_sz) = 0;
        virtual void get_gdims(int *gdims, std::size_t gdims_sz) = 0;
        virtual void get_lcompmap(PIO_Offset *lcompmap, std::size_t lcompmap_sz) = 0;

        virtual Decomp_logger &get(std::string &version, int &nprocs, std::vector<int> &gdims, std::vector<PIO_Offset> &lcompmap) = 0;

        virtual Decomp_logger &put(io_desc_t *iodesc) = 0;

        virtual void close(void ) = 0;

        virtual ~Decomp_logger(){
          MPI_Comm_free(&comm_);
          if(io_comm_ != MPI_COMM_NULL){
            MPI_Comm_free(&io_comm_);
          }
          MPI_Comm_free(&agg_comm_);
        }
      protected:
        static const char OPEN_MODE_RD = 0x1;
        static const char OPEN_MODE_WR = 0x10;

        MPI_Comm comm_;
        MPI_Comm io_comm_;
        MPI_Comm agg_comm_;
        bool is_io_proc_;
        int comm_sz_;
        int comm_rank_;
        int io_comm_sz_;
        int io_comm_rank_;
        int agg_comm_sz_;
        int agg_comm_rank_;
        std::string log_fname_;
        char mode_;
    };

    class Decomp_txt_logger : public Decomp_logger{
      public:
        Decomp_txt_logger(MPI_Comm comm, MPI_Comm io_comm, MPI_Comm agg_comm, std::string log_fname) : Decomp_logger(comm, io_comm, agg_comm, log_fname){}
        virtual Decomp_logger &open(void );

        virtual void get_info(std::string &version, int &nprocs, int &ngdims, PIO_Offset &lcompmap_sz);
        virtual void get_gdims(int *gdims, std::size_t gdims_sz);
        virtual void get_lcompmap(PIO_Offset *lcompmap, std::size_t lcompmap_sz);

        virtual Decomp_logger &get(std::string &version, int &nprocs, std::vector<int> &gdims, std::vector<PIO_Offset> &lcompmap);

        virtual Decomp_logger &put(io_desc_t *iodesc);

        virtual void close(void );

        virtual ~Decomp_txt_logger() {}
      private:
    };

    class Decomp_nc_logger : public Decomp_logger{
      public:
        Decomp_nc_logger(MPI_Comm comm, MPI_Comm io_comm, MPI_Comm agg_comm, std::string log_fname) : Decomp_logger(comm, io_comm, agg_comm, log_fname), ncid_(INVALID_ID), comm_sz_dimid_(INVALID_ID), info_cached_(false), version_att_name_("version"), nprocs_att_name_("nprocs"), ndims_att_name_("ndims"), gdimlen_att_name_("gdimlen"), comm_sz_dim_name_("comm_sz"), counts_var_name_("counts"), nregions_var_name_("nregions"), gmaplen_dim_name_("gmaplen"), gmap_var_name_("gmap"), gmap_nregions_dim_name_("gmap_nregions"), gmap_regions_var_name_("gmap_regions"), ioid_att_name_("ioid"), nprocs_(0){
        }
        virtual Decomp_logger &open(void );

        virtual void get_info(std::string &version, int &nprocs, int &ngdims, PIO_Offset &lcompmap_sz);
        virtual void get_gdims(int *gdims, std::size_t gdims_sz);
        virtual void get_lcompmap(PIO_Offset *lcompmap, std::size_t lcompmap_sz);

        virtual Decomp_logger &get(std::string &version, int &nprocs, std::vector<int> &gdims, std::vector<PIO_Offset> &lcompmap);

        virtual Decomp_logger &put(io_desc_t *iodesc);

        virtual void close(void );

        virtual ~Decomp_nc_logger() { assert(ncid_ == INVALID_ID); }
      private:
        static const int INVALID_ID = -1;
        int ncid_;
        int comm_sz_dimid_;
        bool info_cached_;

        std::string version_;
        const std::string version_att_name_;
        const std::string nprocs_att_name_;
        std::vector<int> gdims_;
        const std::string ndims_att_name_;
        const std::string gdimlen_att_name_;
        std::vector<PIO_Offset> lcompmap_;
        const std::string comm_sz_dim_name_;
        const std::string counts_var_name_;
        const std::string nregions_var_name_;
        const std::string gmaplen_dim_name_;
        const std::string gmap_var_name_;
        const std::string gmap_nregions_dim_name_;
        const std::string gmap_regions_var_name_;
        const std::string ioid_att_name_;
        int nprocs_;
        
        void gather_starts_counts(std::vector<int> &agg_starts, std::vector<int> &agg_counts, MPI_Offset &agg_io_chunk_sz, io_desc_t *iodesc);
        void gather_nregions_starts_counts(std::vector<int> &agg_nregions_starts, std::vector<int> &agg_nregions_counts, MPI_Offset &agg_nregions, const std::vector<PIO_Offset> &lregions);
        void gather_gmap(const std::vector<int> &starts, const std::vector<int> &counts, std::vector<MPI_Offset> &gmap_chunk, io_desc_t *iodesc);
        void gather_gmap_regions(const std::vector<int> &starts, const std::vector<int> &counts, std::vector<PIO_Offset> &gmap_regions, const std::vector<PIO_Offset> &lregions);
        void read_and_cache_info(void );
        void get_contig_map_regions(std::vector<PIO_Offset> &lregions, io_desc_t *iodesc);
        void get_map_from_regions(std::vector<PIO_Offset> &lregions, std::vector<PIO_Offset> &lcompmap);
    };

    inline Decomp_logger *create_decomp_logger(MPI_Comm comm, MPI_Comm io_comm, MPI_Comm agg_comm, std::string log_fname){
#ifdef _PNETCDF
      char NC_FILE_EXTN_SEP = '.';
      std::string NC_FILE_EXTN("NC");
      std::size_t file_extn_pos = log_fname.rfind(NC_FILE_EXTN_SEP);
      std::string file_extn = (file_extn_pos != std::string::npos) ? log_fname.substr(file_extn_pos + 1) : "";

      std::transform(file_extn.begin(), file_extn.end(), file_extn.begin(), [](unsigned char c){ return std::toupper(c); });
      
      if(file_extn == NC_FILE_EXTN){
        return new Decomp_nc_logger(comm, io_comm, agg_comm, log_fname);
      }
#endif
      return new Decomp_txt_logger(comm, io_comm, agg_comm, log_fname);
    }

    inline Decomp_logger *create_decomp_logger(MPI_Comm ucomm, std::string log_fname){
      MPI_Comm comm = MPI_COMM_NULL;
      MPI_Comm io_comm = MPI_COMM_NULL;
      MPI_Comm agg_comm = MPI_COMM_NULL;
      int agg_comm_rank = -1;
      int ret = MPI_SUCCESS;
      
      ret = MPI_Comm_dup(ucomm, &comm); assert(ret == MPI_SUCCESS);
      ret = MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, &agg_comm); assert(ret == MPI_SUCCESS);
      ret = MPI_Comm_rank(agg_comm, &agg_comm_rank); assert(ret == MPI_SUCCESS);

      int color = (agg_comm_rank == 0) ? 0 : MPI_UNDEFINED;
      ret = MPI_Comm_split(comm, color, 0, &io_comm); assert(ret == MPI_SUCCESS);

      return create_decomp_logger(comm, io_comm, agg_comm, log_fname);
    }

    inline Decomp_logger *create_decomp_logger(iosystem_desc_t *ios, std::string log_fname){
      MPI_Comm comm = MPI_COMM_NULL;
      MPI_Comm io_comm = MPI_COMM_NULL;
      MPI_Comm agg_comm = MPI_COMM_NULL;
      int agg_comm_rank = -1;
      int ret = MPI_SUCCESS;

      ret = MPI_Comm_dup(ios->union_comm, &comm); assert(ret == MPI_SUCCESS);
      ret = MPI_Comm_dup(ios->node_comm, &agg_comm); assert(ret == MPI_SUCCESS);
      ret = MPI_Comm_rank(agg_comm, &agg_comm_rank); assert(ret == MPI_SUCCESS);

      int color = (agg_comm_rank == 0) ? 0 : MPI_UNDEFINED;
      ret = MPI_Comm_split(comm, color, 0, &io_comm); assert(ret == MPI_SUCCESS);
      return create_decomp_logger(comm, io_comm, agg_comm, log_fname);
    }

  } //namespace Decomp_Util
} // namespace SPIO_Util
#endif // __SPIO_DECOMP_LOGGER__
