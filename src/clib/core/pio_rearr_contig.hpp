#ifndef __PIO_REARR_CONTIG_HPP__
#define __PIO_REARR_CONTIG_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_types.hpp"
#include "spio_map_sorter.hpp"

#include <vector>
#include <utility>
#include <cassert>

namespace SPIO{
  namespace DataRearr{
    struct Alltoall_info{
      std::vector<int> scounts;
      std::vector<int> sdispls;
      std::vector<MPI_Datatype> stypes;
      
      std::vector<int> rcounts;
      std::vector<int> rdispls;
      std::vector<MPI_Datatype> rtypes;
    };

    /* The stype is used by sending/compute processes during data aggregation to the
     * aggregating/IO process,
     * i.e., when writing data - gathering phase
     * The stype is used by receiving/compute processes during data dispersion from the
     * aggregating/IO process,
     * i.e., when reading data - scatter phase
     * Since different - counts/types - amount of data is received from each compute process
     * the aggregating process has a receive type for each compute process - rtypes[i] for
     * rank i in the aggregating comm
     * The rtypes[] is used in the aggregating/IO process for aggregating/gathering data
     * from compute processes during data aggregation - gathering phase
     * i.e., when writing data
     * The rtypes[] is used in the aggregating/IO process for dispersing/scattering data
     * to compute processes during data dispersion - scatter phase
     * i.e., when reading data
     */
    struct GatherScatter_info{
      MPI_Datatype stype;
      std::vector<int> rcounts;
      std::vector<MPI_Datatype> rtypes;
    };

    /* This rearranger, rearranges data from compute processes to I/O processes in two 
     * phases/steps,
     * Phase 1 : Data aggregation (from compute processes to aggregating/IO processes)
     * Phase 2 : Data rearrangement (between aggregating/IO processes)
     *
     * The rearranged data is a contiguous block of data (one block/range of data per I/O
     * process) and can be written out to the file system.
     * Note: Data caching is done outside the rearranger. The rearranger is passed in
     * data (cached data) from multiple variables for data rearrangement.
     */
    class Contig_rearr{
      public:
        Contig_rearr(iosystem_desc_t *ios):is_init_(false), ios_(ios), iodesc_(NULL),
          lcompmap_sz_(0), elem_pio_type_(PIO_NAT), elem_mpi_type_(MPI_DATATYPE_NULL),
          elem_mpi_type_sz_(0), agg_comm_(MPI_COMM_NULL), is_agg_root_(false),
          agg_comm_sz_(0), agg_iochunk_sz_(0), rearr_comm_(MPI_COMM_NULL), rearr_comm_sz_(0),
          rearr_comm_iochunk_sz_(0){}
        /* Initialize the rearranger */
        int init(int pio_type, const PIO_Offset *compmap, std::size_t compmap_sz,
                  const int *gdimlen, int ndims, io_desc_t *iodesc);

        bool is_init(void ) const{ return is_init_; }

        /* Get decomp info */
        std::size_t get_decomp_map_lsz(void ) const{
          assert(is_init_);
          return static_cast<std::size_t>(lcompmap_sz_);
        }

        std::size_t get_decomp_gsz(void ) const{
          assert(is_init_);
          return static_cast<std::size_t>(gdecomp_sz_);
        }

        /* Get rearrange info */
        std::size_t get_rearrange_buf_sz(void ) const{
          assert(is_init_ && ios_);
          if(!ios_->ioproc){
            return 0;
          }

          /* FIXME: Should we return the buffer size in bytes ? */
          PIO_Offset iochunk = rearr_comm_iochunk_sz_;
          if(rearr_comm_rank_ != (rearr_comm_sz_ - 1)){
            return iochunk;
          }
          else{
            /* The last I/O process gets the remaining data */
            return (gdecomp_sz_ - rearr_comm_rank_ * iochunk);
          }
        }

        std::vector<std::pair<PIO_Offset, PIO_Offset> > get_rearr_decomp_map_contig_ranges(int iorank);

        std::vector<std::pair<PIO_Offset, PIO_Offset> > get_rearr_decomp_map_contig_ranges(void )
        {
          return get_rearr_decomp_map_contig_ranges(rearr_comm_rank_);
        }

        std::size_t get_rearr_decomp_map_contig_max_nranges(void )
        {
          std::size_t max_nranges = 0;
          for(int i = 0; i < rearr_comm_sz_; i++){
            max_nranges = std::max(max_nranges, get_rearr_decomp_map_nranges(i));
          }
          return max_nranges;
        }

        void off_range_to_dim_range(PIO_Offset start_off, PIO_Offset end_off,
                                    PIO_Offset *start, PIO_Offset *count) const;

        /* Rearrange data */
        int rearrange_comp2io(const void *sbuf, std::size_t sbuf_sz,
                              void *rbuf, std::size_t rbuf_sz, int nvars);
        int rearrange_io2comp(const void *sbuf, std::size_t sbuf_sz,
                              void *rbuf, std::size_t rbuf_sz, int nvars);
        /* Finalize the rearranger */
        int finalize(void );
      private:
        bool is_init_;
        iosystem_desc_t *ios_;
        io_desc_t *iodesc_;

        /* The size of the compmap local to this process */
        PIO_Offset lcompmap_sz_;
        /* The global size represented by this I/O decomposition. This is the same as the
         * global size of the variable that is written out using this I/O decomposition
         * Note: This can be different from the global decomp map size since the map may
         * not represent all elements of a variable/array
         */
        PIO_Offset gdecomp_sz_;
        std::vector<int> gdimlen_;
        std::vector<PIO_Offset> dim_chunk_sz_;

        int elem_pio_type_;
        MPI_Datatype elem_mpi_type_;
        int elem_mpi_type_sz_;

        /* Gather scatter info for data aggregation */
        MPI_Comm agg_comm_;
        bool is_agg_root_;
        int agg_comm_sz_;
        /* Size/Num of elements of the data chunk in this aggregating process */
        std::size_t agg_iochunk_sz_;
        //std::vector<std::size_t> agg_compmap_sorter_;
        SPIO_Util::Map_sorter agg_compmap_sorter_;
        /* The byte displacements for each process, that is part of agg_comm_, in the aggregated data */
        std::vector<int> agg_data_byte_displs_;
        GatherScatter_info agg_gs_info_;

        /* All to all info for data rearrangement */
        MPI_Comm rearr_comm_;
        int rearr_comm_rank_;
        int rearr_comm_sz_;
        PIO_Offset rearr_comm_iochunk_sz_;
        Alltoall_info rearr_alltoall_info_;

        // FIXME: Change start_dim_idx ptr to a vector (requires changing the C array in ioregion list)
        void convert_off_to_start_dim_idx(PIO_Offset start_off, PIO_Offset *start_dim_idx) const
        {
          std::size_t ndims = gdimlen_.size();
          for(std::size_t i = 0; i < ndims; i++){
            start_dim_idx[i] = 0;
          }
          for(std::size_t i = 0; i < ndims; i++){
            start_dim_idx[i] = start_off / dim_chunk_sz_[i];

            start_off -= start_dim_idx[i] * dim_chunk_sz_[i];
            if(start_off == 0) break;
          }
          assert(start_off == 0);
        }

        // FIXME: Change count_dim_idx ptr to a vector (requires changing the C array in ioregion list)
        void convert_off_to_count_dim_idx(PIO_Offset count_off, PIO_Offset *count_dim_idx) const
        {
          std::size_t ndims = gdimlen_.size();
          for(std::size_t i = 0; i < ndims; i++){
            count_dim_idx[i] = gdimlen_[i];
          }
          for(std::size_t i = 0; i < ndims; i++){
            count_dim_idx[i] = count_off / dim_chunk_sz_[i];
            
            count_off -= count_dim_idx[i] * dim_chunk_sz_[i];
            if(count_off == 0) break;
          }

          assert(count_off == 0);
          for(std::size_t i = 0; i < ndims; i++){
            if(count_dim_idx[i] != 0){
              break;
            }
            else{
              count_dim_idx[i] = 1;
            }
          }
        }

        void convert_off_to_start_dim_idx(PIO_Offset start_off, std::vector<PIO_Offset> &start_dim_idx,
                                          std::size_t &last_contig_dim) const
        {
          last_contig_dim = 0;
          for(std::size_t i = 0; i < start_dim_idx.size(); i++){
            if(start_off == 0){
              start_dim_idx[i] = 0;
            }
            else{
              start_dim_idx[i] = start_off / dim_chunk_sz_[i];
              start_off -= start_dim_idx[i] * dim_chunk_sz_[i];
              if(start_dim_idx[i] != 0) last_contig_dim = i;
            }
          }
          assert(start_off == 0);
        }

        std::pair<PIO_Offset, PIO_Offset> get_rearr_decomp_map_range(int iorank) const
        {
          assert(gdecomp_sz_ > 0);
          assert(rearr_comm_iochunk_sz_ > 0);

          PIO_Offset iochunk = rearr_comm_iochunk_sz_;
          PIO_Offset start = iorank * iochunk;
          PIO_Offset end = start + iochunk;
          if(iorank == (rearr_comm_sz_ - 1)){
            end = gdecomp_sz_;
          }
          return std::make_pair(start, end);
        }

        std::size_t get_rearr_decomp_map_nranges(int iorank) const
        {
          assert(gdecomp_sz_ > 0);
          assert(rearr_comm_iochunk_sz_ > 0);

          PIO_Offset iochunk = rearr_comm_iochunk_sz_;
          PIO_Offset start = iorank * iochunk;
          PIO_Offset end = start + iochunk;
          if(iorank == (rearr_comm_sz_ - 1)){
            end = gdecomp_sz_;
          }
          return get_ncontig_ranges_from_off_range(start, end - start);
        }

        void get_contig_ranges_from_off_range(std::vector<std::pair<PIO_Offset, PIO_Offset> > &contig_map_ranges,
                                              PIO_Offset start_off, PIO_Offset count);
        std::size_t get_ncontig_ranges_from_off_range(PIO_Offset start_off, PIO_Offset count) const;
        int create_agg_comm(void );
        static void get_non_fval_lcompmap_counts_displs(const PIO_Offset *lcompmap,
              std::size_t lcompmap_sz,
              std::size_t &non_fval_lcompmap_sz,
              std::vector<int> &non_fval_lcompmap_counts,
              std::vector<int> &non_fval_lcompmap_displs);
        int aggregate_compmap(const PIO_Offset *lcompmap, std::size_t lcompmap_sz,
              std::size_t &non_fval_lcompmap_sz,
              std::vector<int> &non_fval_lcompmap_counts,
              std::vector<int> &non_fval_lcompmap_displs,
              std::vector<PIO_Offset> &gcompmap,
              std::vector<int> &gcompmap_counts,
              std::vector<int> &gcompmap_displs);
        int setup_data_agg_info(const PIO_Offset *lcompmap, std::size_t lcompmap_sz,
              std::size_t non_fval_lcompmap_sz,
              const std::vector<int> &non_fval_lcompmap_counts,
              const std::vector<int> &non_fval_lcompmap_displs,
              const std::vector<PIO_Offset> &gcompmap,
              const std::vector<int> &gcompmap_counts,
              const std::vector<int> &gcompmap_displs,
              const std::vector<int> &to_proc);
        int init_agg_send_type(const PIO_Offset *lcompmap, std::size_t lcompmap_sz,
              const std::vector<int> &lcompmap_counts,
              const std::vector<int> &lcompmap_displs);
        int init_agg_recv_types(const std::vector<int> &gcompmap_counts,
                                const std::vector<std::size_t> &compmap_sorter);
        int aggregate_data(const void *sbuf, std::size_t sbuf_sz,
                              void *abuf, std::size_t abuf_sz, int nvars);
        int disperse_data(const void *abuf, std::size_t abuf_sz,
                              void *rbuf, std::size_t rbuf_sz, int nvars);

        int create_rearr_comm(void );
        void set_rearr_comm_iochunk_sz(int ndims, const int *gdimlen);
        int get_rearr_toproc_map(const std::vector<PIO_Offset> &gcompmap,
              std::vector<int> &to_proc);
        int setup_data_rearr_info(std::vector<PIO_Offset> &gcompmap, std::vector<int> &to_proc,
                                  const int *gdimlen, int ndims);
        int init_rearr_send_types(const std::vector<int> &nregion_infos_sent,
                                  const std::vector<PIO_Offset> &displs_counts_sent);
        int init_rearr_recvd_types(const std::vector<int> &nregion_infos_recvd,
                                  const std::vector<PIO_Offset> &displs_counts_recvd);
        int rearrange_data(const void *sbuf, std::size_t sbuf_sz,
                            void *rbuf, std::size_t rbuf_sz, int nvars, bool agg2rearr);

    };
  } // namespace DataRearr

} // namespace SPIO
#endif // __PIO_REARR_CONTIG_HPP__
