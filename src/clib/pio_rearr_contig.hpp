#ifndef __PIO_REARR_CONTIG_HPP__
#define __PIO_REARR_CONTIG_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_types.hpp"

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
          agg_comm_sz_(0), rearr_comm_(MPI_COMM_NULL), rearr_comm_sz_(0),
          rearr_comm_iochunk_sz_(0){}
        int init(int pio_type, const PIO_Offset *compmap, std::size_t compmap_sz,
                  const int *gdimlen, int ndims, io_desc_t *iodesc);
        int rearrange_comp2io(const void *sbuf, std::size_t sbuf_sz,
                              void *rbuf, std::size_t rbuf_sz, int nvars);
        int rearrange_io2comp(const void *sbuf, std::size_t sbuf_sz,
                              void *rbuf, std::size_t rbuf_sz, int nvars);
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

        int elem_pio_type_;
        int elem_mpi_type_;
        int elem_mpi_type_sz_;

        /* Gather scatter info for data aggregation */
        MPI_Comm agg_comm_;
        bool is_agg_root_;
        int agg_comm_sz_;
        std::vector<std::size_t> agg_compmap_sorter_;
        /* The byte displacements for each process, that is part of agg_comm_, in the aggregated data */
        std::vector<int> agg_data_byte_displs_;
        GatherScatter_info agg_gs_info_;

        /* All to all info for data rearrangement */
        MPI_Comm rearr_comm_;
        int rearr_comm_rank_;
        int rearr_comm_sz_;
        PIO_Offset rearr_comm_iochunk_sz_;
        Alltoall_info rearr_alltoall_info_;

        int create_agg_comm(void );
        int aggregate_compmap(const PIO_Offset *lcompmap, std::size_t lcompmap_sz,
              std::vector<PIO_Offset> &gcompmap,
              std::vector<int> &gcompmap_counts,
              std::vector<int> &gcompmap_displs);
        int setup_data_agg_info(const PIO_Offset *lcompmap, std::size_t lcompmap_sz,
              const std::vector<PIO_Offset> &gcompmap,
              const std::vector<int> &gcompmap_counts,
              const std::vector<int> &gcompmap_displs,
              const std::vector<int> &to_proc);
        int init_agg_recv_types(const std::vector<int> &gcompmap_counts,
                                const std::vector<std::size_t> &compmap_sorter);
        int aggregate_data(const void *sbuf, std::size_t sbuf_sz,
                              void *abuf, std::size_t abuf_sz, int nvars);
        int disperse_data(const void *abuf, std::size_t abuf_sz,
                              void *rbuf, std::size_t rbuf_sz, int nvars);

        int create_rearr_comm(void );
        std::pair<PIO_Offset, PIO_Offset> get_rearr_decomp_map_range(int iorank)
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