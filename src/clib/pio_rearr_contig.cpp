#include <cassert>
#include <vector>
#include <numeric>
#include <iterator>
#include <algorithm>
#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include "pio_rearr_contig.hpp"
#include "pio_rearr_utils.hpp"
#include "spio_dbg_utils.hpp"

namespace SPIO_Util{
  template<typename CIter, typename Iter>
  static void exscan(CIter ibegin, CIter iend, Iter obegin,
    typename std::iterator_traits<Iter>::value_type ival,
    std::function<typename std::iterator_traits<Iter>::value_type
      (typename std::iterator_traits<Iter>::value_type)> scale_func)
  {
    typename std::iterator_traits<Iter>::value_type psum = ival;
    for(;ibegin != iend; ++ibegin, ++obegin){
      *obegin = scale_func(psum);
      psum += *ibegin;
    }
  }

  template<typename CIter, typename Iter>
  static void exscan(CIter ibegin, CIter iend, Iter obegin,
    typename std::iterator_traits<Iter>::value_type ival)
  {
    typename std::iterator_traits<Iter>::value_type psum = ival;
    for(;ibegin != iend; ++ibegin, ++obegin){
      *obegin = psum;
      psum += *ibegin;
    }
  }

  template<typename T>
  static void vec_map_sort(std::vector<T> &v, const std::vector<std::size_t> &map)
  {
    std::vector<T> vtmp(v);
    assert(v.size() == map.size());

    for(std::size_t i = 0; i < vtmp.size(); i++){
      v[map[i]] = vtmp[i];
    }
  }
} // namespace SPIO_Util

int SPIO::DataRearr::Contig_rearr::init(int pio_type,
      const PIO_Offset *compmap, std::size_t compmap_sz,
      const int *gdimlen, int ndims, io_desc_t *iodesc)
{
  int ret = PIO_NOERR;
  assert(ios_);

  lcompmap_sz_ = compmap_sz;
  gdecomp_sz_ = std::accumulate(gdimlen, gdimlen + ndims, 0);
  
  elem_pio_type_ = pio_type;
  ret = find_mpi_type(elem_pio_type_, &elem_mpi_type_, &elem_mpi_type_sz_);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to find MPI type corresponding to PIO type (%d)", pio_type);
  }

  /* Calculate gather scatter info for aggregation. Each I/O node is an aggregator */

  /* Create aggregate comm, rank = 0 in each aggregate comm is an I/O process */
  ret = create_agg_comm();
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to create aggregator comm (iosysid = %d)", ios_->iosysid);
  }

  /* Create rearranger comm, dup of I/O comm - contains aggregator/IO procs */
  ret = create_rearr_comm();
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to create rearranger comm (iosysid = %d)", ios_->iosysid);
  }

  /* Aggregate compmaps into the aggregating procs */
  std::vector<PIO_Offset> aggcompmap;
  std::vector<int> aggcompmap_counts, aggcompmap_displs;
  ret = aggregate_compmap(compmap, compmap_sz, aggcompmap, aggcompmap_counts, aggcompmap_displs);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to aggregate local compmaps (iosysid = %d)", ios_->iosysid);
  }

  /* Each MPI process sends compmap_sz elements to the aggregating node */
  std::vector<int> to_proc;
  ret = get_rearr_toproc_map(aggcompmap, to_proc);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to create to proc map (iosysid = %d)", ios_->iosysid);
  }

  /* Set up the types and info required to aggregate data */
  ret = setup_data_agg_info(compmap, compmap_sz, aggcompmap, aggcompmap_counts, aggcompmap_displs, to_proc);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to setup data aggregation (iosysid = %d)", ios_->iosysid);
  }

  /* Set up the types and info required to rearrange data */
  ret = setup_data_rearr_info(aggcompmap, to_proc, gdimlen, ndims);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to setup data rearrangement (iosysid = %d)", ios_->iosysid);
  }

  is_init_ = true;

  return ret;
}

int SPIO::DataRearr::Contig_rearr::rearrange_comp2io(const void *sbuf, std::size_t sbuf_sz,
      void *rbuf, std::size_t rbuf_sz, int nvars)
{
  int ret = PIO_NOERR;
  assert(is_init_);

  /* FIXME: We need a better way to find this info out */
  std::size_t agg_data_nelems = agg_compmap_sorter_.size();

  /* Aggregate data */
  void *agg_buf = NULL;
  std::size_t agg_buf_sz = nvars * agg_data_nelems * elem_mpi_type_sz_;
  if(is_agg_root_){
    agg_buf = malloc(agg_buf_sz);
    if(!agg_buf){
      return pio_err(ios_, NULL, PIO_ENOMEM, __FILE__, __LINE__,
        "Internal error while aggregating data from compute processes to aggregating processes (PIO_REARR_CONTIG). Unable to allocate memory to recv data (%lld bytes) for data aggregation (iosysid = %d)", static_cast<long long int> (agg_buf_sz), ios_->iosysid);
    }
  }

  ret = aggregate_data(sbuf, sbuf_sz, agg_buf, agg_buf_sz, nvars);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while aggregating data from compute processes to aggregating processes (PIO_REARR_CONTIG). Data aggregation failed(aggregating buffer size = %lld bytes, receive buffer size = %lld bytes, nvars = %d, iosysid = %d)", static_cast<unsigned long long int>(agg_buf_sz),
                            static_cast<unsigned long long int>(rbuf_sz), nvars, ios_->iosysid);
  }

  /* Rearrange data  - aggregate processes to rearrange processes */
  ret = rearrange_data(agg_buf, agg_buf_sz, rbuf, rbuf_sz, nvars, true);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while rearranging data between IO/aggregating processes (PIO_REARR_CONTIG). Data rearrangement failed(aggregating buffer size = %lld bytes, receive buffer size = %lld bytes, nvars = %d, iosysid = %d)", static_cast<unsigned long long int>(agg_buf_sz),
              static_cast<unsigned long long int>(rbuf_sz), nvars, ios_->iosysid);
  }

  if(agg_buf){
    free(agg_buf);
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::aggregate_data(const void *sbuf, std::size_t sbuf_sz,
      void *abuf, std::size_t abuf_sz, int nvars)
{
  int ret = PIO_NOERR;
  assert(is_init_);

  /* Gather data from compute processes in this aggregating comm to the aggregating/IO process */
  MPI_Datatype agg_stype = agg_gs_info_.stype;
  std::vector<MPI_Datatype> agg_rtypes_nvars;

  /* FIXME: We need a better way to find this info out */
  std::size_t agg_data_nelems = agg_compmap_sorter_.size();
  assert(abuf_sz == nvars * agg_data_nelems * elem_mpi_type_sz_);

  if(nvars > 1){
    /* We are aggregating a block of variables */
    /* The stride between each block - each variable - is the total size of the data aggregated from 
     * all compute processes in the aggregator. On the compute procs its a contiguous block of data
     * - composed on nvars blocks, such that each block is data corresponding to a single var
     */
    if(is_agg_root_){
      MPI_Aint stride_between_vars = static_cast<MPI_Aint>(agg_data_nelems * elem_mpi_type_sz_);
      for(std::size_t i = 0; i < agg_gs_info_.rtypes.size(); i++){
        MPI_Datatype agg_rtype = MPI_DATATYPE_NULL;
        if(agg_gs_info_.rtypes[i] != MPI_DATATYPE_NULL){
          ret = MPI_Type_hvector(nvars, 1, stride_between_vars, agg_gs_info_.rtypes[i], &agg_rtype);
          if(ret == MPI_SUCCESS){
            ret = MPI_Type_commit(&agg_rtype);
          }
          if(ret != MPI_SUCCESS){
            return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
              "Internal error while aggregating data from compute processes to aggregating processes (PIO_REARR_CONTIG). Unable to create vector types to recv data for data aggregation (iosysid = %d)", ios_->iosysid);
          }
        }
        agg_rtypes_nvars.push_back(agg_rtype);
      }
    }
  }

  int scount = (agg_stype != MPI_DATATYPE_NULL) ? nvars : 0;

  ret = SPIO_Util::Rearr_Util::gatherw(sbuf, scount, agg_stype,
          abuf, agg_gs_info_.rcounts, agg_data_byte_displs_,
          (nvars > 1) ? agg_rtypes_nvars : agg_gs_info_.rtypes,
          0, agg_comm_, NULL);
  if(ret != MPI_SUCCESS){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while aggregating data from compute processes to aggregating processes (PIO_REARR_CONTIG). Unable to gather data during data aggregation (iosysid = %d)", ios_->iosysid);
  }

  for(std::size_t i = 0; i < agg_rtypes_nvars.size(); i++){
    if(agg_rtypes_nvars[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&(agg_rtypes_nvars[i]));
    }  
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::disperse_data(const void *abuf, std::size_t abuf_sz,
      void *rbuf, std::size_t rbuf_sz, int nvars)
{
  int ret = PIO_NOERR;
  assert(is_init_);

  /* When scattering/dispersing data from aggregate process to compute processes,
   * agg_gs_info_.stype, the send type used for sending data to aggregating processes during
   * aggregation, is now the recv type
   * Similarly agg_gs_info_.rtypes, the recv types used for aggregating data on the aggregating
   * process from the compute processes, is now the type use to send/disperse data
   */
  MPI_Datatype dis_rtype = agg_gs_info_.stype;
  std::vector<MPI_Datatype> dis_stypes_nvars;

  /* FIXME: We need a better way to find this info out */
  std::size_t agg_data_nelems = agg_compmap_sorter_.size();
  assert(abuf_sz == nvars * agg_data_nelems * elem_mpi_type_sz_);

  if(nvars > 1){
    /* We are dispersing a block of variables */
    /* The stride between each block - each variable - is the total size of the data aggregated from 
     * all compute processes in the aggregator. On the compute procs its a contiguous block of data
     * - composed on nvars blocks, such that each block is data corresponding to a single var
     */
    if(is_agg_root_){
      MPI_Aint stride_between_vars = static_cast<MPI_Aint>(agg_data_nelems * elem_mpi_type_sz_);
      for(std::size_t i = 0; i < agg_gs_info_.rtypes.size(); i++){
        MPI_Datatype dis_stype = MPI_DATATYPE_NULL;
        if(agg_gs_info_.rtypes[i] != MPI_DATATYPE_NULL){
          ret = MPI_Type_hvector(nvars, 1, stride_between_vars, agg_gs_info_.rtypes[i], &dis_stype);
          if(ret == MPI_SUCCESS){
            ret = MPI_Type_commit(&dis_stype);
          }
          if(ret != MPI_SUCCESS){
            return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
              "Internal error while dispersing data from aggregating processes to compute processes (PIO_REARR_CONTIG). Unable to create vector types to send data during data dispersion (iosysid = %d)", ios_->iosysid);
          }
        }
        dis_stypes_nvars.push_back(dis_stype);
      }
    }
  }

  int rcount = (dis_rtype != MPI_DATATYPE_NULL) ? nvars : 0;

  ret = SPIO_Util::Rearr_Util::scatterw(abuf, agg_gs_info_.rcounts,
          agg_data_byte_displs_,
          (nvars > 1) ? dis_stypes_nvars : agg_gs_info_.rtypes,
          rbuf, rcount, dis_rtype,
          0, agg_comm_, NULL);
  if(ret != MPI_SUCCESS){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while dispersing data from aggregating processes to compute processes (PIO_REARR_CONTIG). Unable to disperse/scatter data during data dispersion (iosysid = %d)", ios_->iosysid);
  }

  for(std::size_t i = 0; i < dis_stypes_nvars.size(); i++){
    if(dis_stypes_nvars[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&(dis_stypes_nvars[i]));
    }  
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::rearrange_io2comp(const void *sbuf, std::size_t sbuf_sz,
      void *rbuf, std::size_t rbuf_sz, int nvars)
{
  int ret = PIO_NOERR;
  assert(is_init_);

  /* FIXME: We need a better way to find this info out */
  std::size_t agg_data_nelems = agg_compmap_sorter_.size();

  /* Disperse/scatter data */
  void *agg_buf = NULL;
  std::size_t agg_buf_sz = nvars * agg_data_nelems * elem_mpi_type_sz_;
  if(is_agg_root_){
    agg_buf = malloc(agg_buf_sz);
    if(!agg_buf){
      return pio_err(ios_, NULL, PIO_ENOMEM, __FILE__, __LINE__,
        "Internal error while dispersing data from aggregating processes to compute processes (PIO_REARR_CONTIG). Unable to allocate memory to store data (%lld bytes) for data dispersion (iosysid = %d)", static_cast<long long int> (agg_buf_sz), ios_->iosysid);
    }
  }

  /* Rearrange data  - rearrange processes to aggregate processes */
  ret = rearrange_data(sbuf, sbuf_sz, agg_buf, agg_buf_sz, nvars, false);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while rearranging data between IO/aggregating processes (PIO_REARR_CONTIG). Data rearrangement failed(aggregating buffer size = %lld bytes, receive buffer size = %lld bytes, nvars = %d, iosysid = %d)", static_cast<unsigned long long int>(agg_buf_sz),
              static_cast<unsigned long long int>(rbuf_sz), nvars, ios_->iosysid);
  }

  ret = disperse_data(agg_buf, agg_buf_sz, rbuf, rbuf_sz, nvars);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while dispersing data from aggregating processes to compute processes (PIO_REARR_CONTIG). Data dispersion/scatter failed(aggregating buffer size = %lld bytes, receive buffer size = %lld bytes, nvars = %d, iosysid = %d)", static_cast<unsigned long long int>(agg_buf_sz),
                            static_cast<unsigned long long int>(rbuf_sz), nvars, ios_->iosysid);
  }

  if(agg_buf){
    free(agg_buf);
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::finalize(void )
{
  int ret = PIO_NOERR;

  /* Free the MPI datatypes used for aggregation and rearrangement */
  /* We ignore the return values from free()s, since we want to free as much as possible here */
  if(agg_gs_info_.stype != MPI_DATATYPE_NULL){
    MPI_Type_free(&(agg_gs_info_.stype));
  }
  for(std::size_t i = 0; i < agg_gs_info_.rtypes.size(); i++){
    if(agg_gs_info_.rtypes[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&(agg_gs_info_.rtypes[i]));
    }
  }

  for(std::size_t i = 0; i < rearr_alltoall_info_.stypes.size(); i++){
    if(rearr_alltoall_info_.stypes[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&(rearr_alltoall_info_.stypes[i]));
    }
  }

  for(std::size_t i = 0; i < rearr_alltoall_info_.rtypes.size(); i++){
    if(rearr_alltoall_info_.rtypes[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&(rearr_alltoall_info_.rtypes[i]));
    }
  }

  /* Free aggregate and rearranger comms */
  if(agg_comm_ != MPI_COMM_NULL){
    MPI_Comm_free(&agg_comm_);
  }
  if(rearr_comm_ != MPI_COMM_NULL){
    MPI_Comm_free(&rearr_comm_);
  }

  is_init_ = false;

  return ret;
}

inline int SPIO::DataRearr::Contig_rearr::create_agg_comm(void )
{
  int ret = MPI_SUCCESS;

  assert(ios_);

  /* Divide union comm into (ios_->num_iotasks) groups */
  int agg_range_sz = ios_->num_uniontasks/ios_->num_iotasks;
  /* Ensure that each group/color gets an I/O process - which is also the aggregating process */
  int color = (ios_->ioproc) ? ios_->io_rank : (ios_->union_rank/agg_range_sz);
  /* Put the extra set of procs, after evenly dividing procs among I/O procs, with the last I/O proc */
  color = (color >= ios_->num_iotasks) ? (ios_->num_iotasks - 1) : color;

  /* Each I/O process is rank 0 in the new aggregator comm, and is also the aggregating proc */
  int key = (ios_->ioproc) ? 0 : 1;

  ret = MPI_Comm_split(ios_->union_comm, color, key, &agg_comm_);
  if(ret == MPI_SUCCESS){
    is_agg_root_ = (key == 0) ? true : false;
    ret = MPI_Comm_size(agg_comm_, &agg_comm_sz_);
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::aggregate_compmap(const PIO_Offset *lcompmap, std::size_t lcompmap_sz,
                                                      std::vector<PIO_Offset> &gcompmap,
                                                      std::vector<int> &gcompmap_counts,
                                                      std::vector<int> &gcompmap_displs)
{
  int ret = PIO_NOERR;
  assert(ios_ && (agg_comm_ != MPI_COMM_NULL));

  /* Get the compmap sizes from all procs in this aggregation comm */
  if(is_agg_root_){
    gcompmap_counts.resize(agg_comm_sz_);
    gcompmap_displs.resize(agg_comm_sz_);
  }

  int send_sz = static_cast<int>(lcompmap_sz);
  ret = MPI_Gather(&send_sz, 1, MPI_INT, gcompmap_counts.data(), 1, MPI_INT, 0, agg_comm_);
  if(ret != MPI_SUCCESS){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to gather local compmap sizes (iosysid=%d)", ios_->iosysid);
  }

  if(is_agg_root_){
    int total_agg_compmap_sz = std::accumulate(gcompmap_counts.cbegin(), gcompmap_counts.cend(), 0); 
    gcompmap.resize(total_agg_compmap_sz);

    /* Calculate the displacements for local compmaps in the global compmap, gcompmap */
    SPIO_Util::exscan(gcompmap_counts.cbegin(), gcompmap_counts.cend(), gcompmap_displs.begin(), 0);
    int elem_type_sz = this->elem_mpi_type_sz_;
    agg_data_byte_displs_.resize(gcompmap_displs.size());
    std::transform(gcompmap_displs.cbegin(), gcompmap_displs.cend(), agg_data_byte_displs_.begin(),
      [elem_type_sz](int i) { return i * elem_type_sz; } );
  }

  /* Gather local compmaps */
  ret = MPI_Gatherv(lcompmap, lcompmap_sz, PIO_OFFSET,
          gcompmap.data(), gcompmap_counts.data(), gcompmap_displs.data(), PIO_OFFSET,
          0, agg_comm_);
  if(ret != MPI_SUCCESS){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to gather local compmaps (iosysid=%d)", ios_->iosysid);
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::setup_data_agg_info(const PIO_Offset *lcompmap,
                                                        std::size_t lcompmap_sz,
                                                        const std::vector<PIO_Offset> &gcompmap,
                                                        const std::vector<int> &gcompmap_counts,
                                                        const std::vector<int> &gcompmap_displs,
                                                        const std::vector<int> &to_proc)
{
  int ret = PIO_NOERR;

  assert(ios_);
  /* Setup gather scatter info for sending/receiving data from compute procs to
   * aggregate/IO procs
   * Sending/receiving 1 contiguous array of lcompmap_sz length
   */
  agg_gs_info_.stype = MPI_DATATYPE_NULL;
  if(lcompmap_sz > 0){
    ret = MPI_Type_contiguous(static_cast<int>(lcompmap_sz), elem_mpi_type_, &(agg_gs_info_.stype));
    if(ret == MPI_SUCCESS){
      ret = MPI_Type_commit(&(agg_gs_info_.stype));
    }

    if(ret != MPI_SUCCESS){
      return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
        "Internal error while initializing PIO_REARR_CONTIG. Unable to create/commit contiguous type (size=%d) on compute process for data aggregation (iosysid=%d)", static_cast<int>(lcompmap_sz), ios_->iosysid);
    }
  }
  
  if(!is_agg_root_){
    /* Non-aggregating processes use the agg_gs_info_.stype to send data to aggregator process
     * and receive data from it
     */
    return ret;
  }
  /* Aggregating process, root in aggregator comm, sends a contiguous array of
   * lcompmap_sz length, if its also a compute process, but receives data from
   * all non-aggregating procs in the aggregator comm in a way that makes it
   * easier to rearrange the data with other aggregating procs - data to send to
   * each aggregating proc clustered together such that data sent to an aggregating
   * proc is sorted based on the compmap
   * The recv type will be calculated later in the function, initializing to NULL 
   */

  assert(gcompmap.size() == to_proc.size());

  /* Sort the indices of gcompmap to a view that we need for data rearrangement between
   * aggregator/IO procs
   */
  std::vector<std::size_t> gcompmap_idx(gcompmap.size());
  std::iota(gcompmap_idx.begin(), gcompmap_idx.end(), 0);

  std::sort(gcompmap_idx.begin(), gcompmap_idx.end(),
            [&gcompmap,&to_proc](PIO_Offset a, PIO_Offset b){
              if(to_proc[a] == to_proc[b]){
                /* Sort all data to send to each proc */
                return gcompmap[a] < gcompmap[b];
              }
              else{
                /* Aggregate all data to send to a proc together */
                return to_proc[a] < to_proc[b];
              }
            });

  /* Aggregate compmap sorter can be used to sort any user data based on gcompmap */
  agg_compmap_sorter_.resize(gcompmap.size());
  for(std::size_t i = 0; i < gcompmap_idx.size(); i++){
    agg_compmap_sorter_[gcompmap_idx[i]] = i;
  }

  ret = init_agg_recv_types(gcompmap_counts, agg_compmap_sorter_);
  return ret;
}

int SPIO::DataRearr::Contig_rearr::init_agg_recv_types(const std::vector<int> &gcompmap_counts,
      const std::vector<std::size_t> &compmap_sorter)
{
  int ret = PIO_NOERR;
  /* The compmap_sorter has the indices of the sorted data (based on to_proc & compmap)
   * So look for contiguous ranges in compmap_sorter array
   * We need to receive data in the aggregating process such that all data being sent
   * to another aggregating process, during rearrangement, is contiguous
   * compmap_sorter[i] gives the final intended location of data in index i received on
   * the aggregating process from compute processes (the final intended location is such
   * that all data sent to to_proc[i] are contiguous)
   */

  std::size_t compmap_sorter_idx = 0;
  for(std::vector<int>::const_iterator citer = gcompmap_counts.cbegin();
        citer != gcompmap_counts.cend(); ++citer){
    /* During data aggregation we receive, gcompmap_counts[i] elements from rank i */
    /* We need to keep track of data received in the recv type, so ignore procs that
     * have nothing to send */
    if(*citer != 0){
      std::vector<int> counts;
      std::vector<int> displs;
      std::size_t nelems = static_cast<std::size_t>(*citer);
      /* The dest range - in the recv buffer of agg process - of elements from this compute proc is :
       *  compmap_sorter[compmap_sorter_idx, compmap_sorter_idx + nelems)
       */
      counts.push_back(1);
      displs.push_back(static_cast<int>(compmap_sorter[compmap_sorter_idx] - compmap_sorter_idx));
      assert(compmap_sorter_idx + nelems - 1 < compmap_sorter.size());
      for(std::size_t i = 1; i < nelems; i++){
        int disp = static_cast<int>(compmap_sorter[compmap_sorter_idx + i] - compmap_sorter_idx);
        if(disp == displs.back() + counts.back()){
          /* The range in the compmap_sorter is contiguous : receiving contiguous data into recv
           * buf from this compute proc
           */
          counts.back() += 1;
        }
        else{
          /* Data from this compute proc is not received contiguously, start a new range */
          counts.push_back(1);
          displs.push_back(disp);
        }
      }

      assert(counts.size() == displs.size());
      MPI_Datatype rtype = MPI_DATATYPE_NULL;
      ret = MPI_Type_indexed(static_cast<int>(counts.size()), counts.data(), displs.data(),
                              elem_mpi_type_, &rtype);
      if(ret == MPI_SUCCESS){
        ret = MPI_Type_commit(&rtype);
      }
      if(ret != MPI_SUCCESS){
        return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
          "Internal error while initializing PIO_REARR_CONTIG. Unable to create/commit indexed type (size=%d, nranges=%d) on compute process to recv data for data aggregation (iosysid=%d)", static_cast<int>(nelems), static_cast<int>(counts.size()), ios_->iosysid);
      }
      agg_gs_info_.rtypes.push_back(rtype);
      agg_gs_info_.rcounts.push_back(1);
      compmap_sorter_idx += nelems;
    }
    else{
      agg_gs_info_.rtypes.push_back(MPI_DATATYPE_NULL);
      agg_gs_info_.rcounts.push_back(0);
    }
  }

  return ret;
}

inline int SPIO::DataRearr::Contig_rearr::create_rearr_comm(void )
{
  int ret = PIO_NOERR;
  assert(ios_);
  assert(gdecomp_sz_ > 0);
  
  if(!ios_->ioproc){
    return ret;
  }

  ret = MPI_Comm_dup(ios_->io_comm, &rearr_comm_);
  if(ret == MPI_SUCCESS){
    ret = MPI_Comm_size(rearr_comm_, &rearr_comm_sz_);
    if(ret == MPI_SUCCESS){
      ret = MPI_Comm_rank(rearr_comm_, &rearr_comm_rank_);
    }
  }

  if(ret == MPI_SUCCESS){
    rearr_comm_iochunk_sz_ = gdecomp_sz_ / rearr_comm_sz_;
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::get_rearr_toproc_map(const std::vector<PIO_Offset> &gcompmap,
                                                          std::vector<int> &to_proc)
{
  int ret = PIO_NOERR;
  assert(ios_);
  
  if(!ios_->ioproc){
    return ret;
  }
  assert(rearr_comm_sz_ > 0);

  to_proc.resize(gcompmap.size());
  for(std::size_t i = 0; i < gcompmap.size(); i++){
    /* Note: The last rearranger proc gets the leftover gcompmap range */
    to_proc[i] = std::min(static_cast<int>(gcompmap[i]/rearr_comm_iochunk_sz_), rearr_comm_sz_ - 1);
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::setup_data_rearr_info(std::vector<PIO_Offset> &gcompmap,
                                                          std::vector<int> &to_proc,
                                                          const int *gdimlen, int ndims)
{
  int ret = PIO_NOERR;
  assert(ios_);

  if(!ios_->ioproc){
    return ret;
  }
  assert(rearr_comm_sz_ > 0);
  assert(gcompmap.size() == agg_compmap_sorter_.size());
  assert(to_proc.size() == gcompmap.size());

  /* Sort gcompmap (the aggregated compmap from compute procs) based on compmap sorter map */
  SPIO_Util::vec_map_sort(gcompmap, agg_compmap_sorter_);
  SPIO_Util::vec_map_sort(to_proc, agg_compmap_sorter_);

  /* Exchange information about the data being sent from each process,
   * 1) The number of regions (a single region is a contiguous chunk/block of elements/data)
   * 2) Start/Count defining each region - We calculate the displacement for each receiving
   * proc on the sending proc & send that information instead. For region i, a process sends
   *
   * displs_i -> The local displacement at which to receive the data on the receiving process
   * counts_i -> Number of contiguous elements - size of a contiguous block - of data being sent
   *
   * The counts & displacements (starts) for all the receiving processes are compiled into a
   * single displs_counts_sent[] array. The nregion_infos array specifies the number of region
   * infos for each process - nregion_infos[i] specifies the number of region info pairs,
   * the number of {displ, count} pairs for rank i - and can be used to traverse the
   * displs_counts_sent[] array to find the offset in the displs_counts_sent[]
   * array for rank i
   *
   * Each process sends a contiguous block of data and receives an indexed block of data
   * based on the start/count info from each process
   */
  std::vector<int> nregion_infos_sent(rearr_comm_sz_, 0);
  /* displs_counts_sent[i] = displacement, local to rank k, for region i
   * displs_counts_sent[i+1] = size of region i
   * displs_counts_sent_rank_offs[k] = offset to displs_counts_sent[] array for regions to send to rank k
   */
  std::vector<PIO_Offset> displs_counts_sent;
  std::vector<int> displs_counts_sent_rank_offs(rearr_comm_sz_);

  std::size_t to_proc_gcompmap_idx = 0;
  if(to_proc.size() > 0){
    assert(gdecomp_sz_ > 0);
    /* This aggregating proc has some data to send to other aggregating/IO procs */
    for(int i = 0; (i < rearr_comm_sz_) && (to_proc_gcompmap_idx < to_proc.size()); i++){
      assert(to_proc_gcompmap_idx < to_proc.size());
      assert(to_proc_gcompmap_idx < gcompmap.size());
      displs_counts_sent_rank_offs[i] =
        static_cast<int>(displs_counts_sent.size() * sizeof(MPI_Offset));
      /* to_proc is sorted based on rank */
      //PIO_Offset to_proc_start_disp = get_rearr_decomp_map_range(i).first;
      PIO_Offset to_proc_start_disp = get_rearr_decomp_map_range(to_proc[to_proc_gcompmap_idx]).first;
      while((to_proc_gcompmap_idx < gcompmap.size()) &&
              (to_proc[to_proc_gcompmap_idx] == i)){
        /* There is some data to send to proc i, find the next contig region to sent to proc i */
        nregion_infos_sent[i] += 2;
        /* Add displ - displ is converted to local displ in the receiving process */
        PIO_Offset prev_disp = gcompmap[to_proc_gcompmap_idx++] - to_proc_start_disp;
        displs_counts_sent.push_back(prev_disp);
        /* Add count */
        displs_counts_sent.push_back(1);
        /* Parse the entire contig region */
        while(to_proc_gcompmap_idx < gcompmap.size()){
          PIO_Offset cur_disp = gcompmap[to_proc_gcompmap_idx] - to_proc_start_disp;
          if((to_proc[to_proc_gcompmap_idx] != i) ||
              (cur_disp != prev_disp + 1)){
            break;
          }
          /* Increment the count for this region */
          displs_counts_sent.back()++;
          prev_disp = cur_disp;
          to_proc_gcompmap_idx++;
        }
      } 
    }
  }

  /* Exchange the number of region infos sent/recvd to/from each process */
  std::vector<int> nregion_infos_recvd(rearr_comm_sz_, 0);
  ret = SPIO_Util::Rearr_Util::alltoall(nregion_infos_sent.data(), 1, MPI_INT, 
                                        nregion_infos_recvd.data(), 1, MPI_INT,
                                        rearr_comm_, &(ios_->rearr_opts.comp2io));
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to exchange number of regions for data rearrangement (iosysid=%d)", ios_->iosysid);
  }

  /* Calculate total number of region info pairs received by this process */
  int tot_nregion_infos_recvd =
    std::accumulate(nregion_infos_recvd.begin(), nregion_infos_recvd.end(), 0);

  std::vector<PIO_Offset> displs_counts_recvd(tot_nregion_infos_recvd, 0);
  std::vector<int> displs_counts_recvd_rank_offs(rearr_comm_sz_);

  SPIO_Util::exscan(nregion_infos_recvd.cbegin(), nregion_infos_recvd.cend(),
    displs_counts_recvd_rank_offs.begin(), 0,
    [](int off) { return off * sizeof(MPI_Offset); });

  std::vector<MPI_Datatype> info_types(rearr_comm_sz_, MPI_OFFSET);
  /* Exchange the info - start/count - of regions sent/recvd to/from each process */
  ret = SPIO_Util::Rearr_Util::alltoallw(displs_counts_sent.data(), nregion_infos_sent.data(),
          displs_counts_sent_rank_offs.data(), info_types.data(),
          displs_counts_recvd.data(), nregion_infos_recvd.data(),
          displs_counts_recvd_rank_offs.data(), info_types.data(),
          rearr_comm_, &(ios_->rearr_opts.comp2io));
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to exchange region info for data rearrangement (iosysid=%d)", ios_->iosysid);
  }

  /* Create send types for data rearrangement - for each process */
  ret = init_rearr_send_types(nregion_infos_sent, displs_counts_sent);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to create MPI datatypes for sending data for data rearrangement (iosysid=%d)", ios_->iosysid);
  }

  /* Create recv types for data rearrangement - for each process based on region info exchanged */
  ret = init_rearr_recvd_types(nregion_infos_recvd, displs_counts_recvd);
  if(ret != PIO_NOERR){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while initializing PIO_REARR_CONTIG. Unable to create MPI datatypes for receiving data for data rearrangement (iosysid=%d)", ios_->iosysid);
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::init_rearr_send_types(
      const std::vector<int> &nregion_infos_sent,
      const std::vector<PIO_Offset> &displs_counts_sent)
{
  int ret = PIO_NOERR;

  assert(ios_ && ios_->ioproc);
  assert(rearr_comm_sz_ > 0);
  assert(nregion_infos_sent.size() == rearr_comm_sz_);

  rearr_alltoall_info_.scounts.reserve(rearr_comm_sz_);
  rearr_alltoall_info_.sdispls.reserve(rearr_comm_sz_);
  rearr_alltoall_info_.stypes.reserve(rearr_comm_sz_);

  /* Point the idx for the first count in displs_counts_sent[] array */
  std::size_t displs_counts_sent_idx = 1;
  std::size_t data_idx = 0;
  for(int i = 0; i < rearr_comm_sz_; i++){
    if(nregion_infos_sent[i] != 0){
      assert(nregion_infos_sent[i] > 0);
      /* Since we rearrange data sorted on to_proc() - the process to send to during rearr -
       * we can send a contiguous chunk/block of data */
      /* Accumulate counts from all regions to get the total size of the contiguous chunk/block
       * of data to send to this process, i */
      int data_ranki_idx = static_cast<int>(data_idx);
      int nregions = nregion_infos_sent[i] / 2;
      int nelems = 0;
      /* Add the counts for all regions to get the total elements, nelems, sent to proc i */
      for(int i = 0; i < nregions; i++, displs_counts_sent_idx += 2){
        assert(displs_counts_sent_idx < displs_counts_sent.size());
        nelems += displs_counts_sent[displs_counts_sent_idx];
      }
      assert(nelems > 0);
      data_idx += nelems;

      MPI_Datatype stype;
      ret = MPI_Type_contiguous(nelems, elem_mpi_type_, &stype);
      if(ret == MPI_SUCCESS){
        ret = MPI_Type_commit(&stype);
      }
    
      if(ret != MPI_SUCCESS){
        return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
          "Internal error while initializing PIO_REARR_CONTIG. Unable to create MPI contig datatype (nelems=%d, sz =%lld) for sending data for data rearrangement (iosysid=%d)", nelems, static_cast<unsigned long long int>(nelems * elem_mpi_type_sz_), ios_->iosysid);
      }
       
      rearr_alltoall_info_.scounts.push_back(1);
      rearr_alltoall_info_.sdispls.push_back(data_ranki_idx * elem_mpi_type_sz_);
      rearr_alltoall_info_.stypes.push_back(stype);
    }
    else{
      rearr_alltoall_info_.scounts.push_back(0);
      rearr_alltoall_info_.sdispls.push_back(0);
      rearr_alltoall_info_.stypes.push_back(MPI_DATATYPE_NULL);
    }
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::init_rearr_recvd_types(
      const std::vector<int> &nregion_infos_recvd,
      const std::vector<PIO_Offset> &displs_counts_recvd)
{
  int ret = PIO_NOERR;

  assert(ios_ && ios_->ioproc);
  assert(rearr_comm_sz_ > 0);
  assert(nregion_infos_recvd.size() == rearr_comm_sz_);

  rearr_alltoall_info_.rcounts.reserve(rearr_comm_sz_);
  rearr_alltoall_info_.rdispls.reserve(rearr_comm_sz_);
  rearr_alltoall_info_.rtypes.reserve(rearr_comm_sz_);

  assert(displs_counts_recvd.size() % 2 == 0);

  /* Point the idx for the first displ in displs_counts_recvd[] array */
  std::size_t displs_counts_recvd_idx = 0;
  std::size_t data_idx = 0;
  for(int i = 0; i < rearr_comm_sz_; i++){
    if(nregion_infos_recvd[i] != 0){
      /* We receive an indexed type based on the region info we received from each process */
      int data_ranki_idx = static_cast<int>(data_idx);
      int nregions = nregion_infos_recvd[i] / 2;

      assert(nregions > 0);

      std::vector<int> displs;
      std::vector<int> counts;

      displs.reserve(nregions);
      counts.reserve(nregions);
      /* Parse all the regions recvd from rank i and find the displs/counts for each region */
      int nelems = 0;
      for(int i = 0; i < nregions; i++, displs_counts_recvd_idx += 2){
        displs.push_back(displs_counts_recvd[displs_counts_recvd_idx]);
        counts.push_back(displs_counts_recvd[displs_counts_recvd_idx + 1]);
        nelems += displs_counts_recvd[displs_counts_recvd_idx + 1];
      }

      assert(nelems > 0);
      data_idx += nelems;

      MPI_Datatype rtype;
      ret = MPI_Type_indexed(nregions, counts.data(), displs.data(), elem_mpi_type_, &rtype);
      if(ret == MPI_SUCCESS){
        ret = MPI_Type_commit(&rtype);
      }
      if(ret != MPI_SUCCESS){
        return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
          "Internal error while initializing PIO_REARR_CONTIG. Unable to create MPI indexed datatype (nelems=%d, nregions = %d, sz =%lld) for sending data for data rearrangement (iosysid=%d)", nelems, nregions, static_cast<unsigned long long int>(nelems * elem_mpi_type_sz_), ios_->iosysid);
      }
       
      rearr_alltoall_info_.rcounts.push_back(1);
      //rearr_alltoall_info_.rdispls.push_back(data_ranki_idx * elem_mpi_type_sz_);
      /* The displacements are already built into the indexed type, rtype, received */
      rearr_alltoall_info_.rdispls.push_back(0);
      rearr_alltoall_info_.rtypes.push_back(rtype);
    }
    else{
      rearr_alltoall_info_.rcounts.push_back(0);
      rearr_alltoall_info_.rdispls.push_back(0);
      rearr_alltoall_info_.rtypes.push_back(MPI_DATATYPE_NULL);
    }
  }

  return ret;
}

int SPIO::DataRearr::Contig_rearr::rearrange_data(const void *sbuf, std::size_t sbuf_sz,
      void *rbuf, std::size_t rbuf_sz, int nvars, bool agg2rearr)
{
  int ret = PIO_NOERR;
  assert(is_init_);

  assert(ios_);
  if(!ios_->ioproc){
    return PIO_NOERR;
  }

  /* Rearrange read/aggregated data between aggregating/IO processes */
  //MPI_Datatype agg_stype = agg_gs_info_.stype;
  //std::vector<MPI_Datatype> agg_rtypes_nvars;
  std::vector<MPI_Datatype> rearr_stypes_nvars, rearr_rtypes_nvars;

  /* FIXME: We need a better way to find this info out */
  std::size_t agg_data_nelems = agg_compmap_sorter_.size();
  //assert(abuf_sz == nvars * agg_data_nelems * elem_mpi_type_sz_);

  if(nvars > 1){
    /* We are rearranging a block of variables */
    /* The stride between each block - data for each variable
     * Aggregate procs to rearr procs (write) :
     *  Each proc sends aggregated data (aggregated from compute processes) and receives
     *  the contig block of data to write
     * Rearr procs to aggregate procs (read) :
     *  Each proc sends the contig block of data read and each proc receives the aggregated
     *  block of data that needs to be dispersed to compute processes
     */
    MPI_Aint sstride_between_vars = 0, rstride_between_vars = 0;
    MPI_Aint stride_between_agg_vars = 0, stride_between_rearr_vars = 0;

    stride_between_agg_vars = agg_data_nelems * elem_mpi_type_sz_;
    std::pair<PIO_Offset, PIO_Offset> map_range = get_rearr_decomp_map_range(rearr_comm_rank_);
    stride_between_rearr_vars =
      static_cast<MPI_Aint>( (map_range.second - map_range.first) * elem_mpi_type_sz_);
    if(agg2rearr){
      /* Rearranging from aggregating procs to rearr procs - data write - comp2io path */
      sstride_between_vars = stride_between_agg_vars;
      rstride_between_vars = stride_between_rearr_vars;
    }
    else{
      /* Rearranging from rearr procs to aggregating procs - data read - io2comp path */
      sstride_between_vars = stride_between_rearr_vars;
      rstride_between_vars = stride_between_agg_vars;
    }

    rearr_stypes_nvars.reserve(rearr_comm_sz_);
    rearr_rtypes_nvars.reserve(rearr_comm_sz_);
    assert(rearr_alltoall_info_.stypes.size() == rearr_comm_sz_);
    assert(rearr_alltoall_info_.rtypes.size() == rearr_comm_sz_);
    for(std::size_t i = 0; i < rearr_comm_sz_; i++){
      MPI_Datatype rearr_stype = MPI_DATATYPE_NULL;
      MPI_Datatype rearr_rtype = MPI_DATATYPE_NULL;
      /* Send type for block of vars */
      if((agg2rearr && (rearr_alltoall_info_.stypes[i] != MPI_DATATYPE_NULL)) || (!agg2rearr && (rearr_alltoall_info_.rtypes[i] != MPI_DATATYPE_NULL))){
        ret = MPI_Type_hvector(nvars, 1, sstride_between_vars,
                (agg2rearr) ? rearr_alltoall_info_.stypes[i] : rearr_alltoall_info_.rtypes[i], &rearr_stype);
        if(ret == MPI_SUCCESS){
          ret = MPI_Type_commit(&rearr_stype);
        }
        if(ret != MPI_SUCCESS){
          return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
            "Internal error while rearranging data from aggregating processes to IO processes (PIO_REARR_CONTIG). Unable to create vector types to send data for data aggregation (iosysid = %d, nvars = %d)", ios_->iosysid, nvars);
        }
      }
      /* Recv type for block of vars */
      if((agg2rearr && (rearr_alltoall_info_.rtypes[i] != MPI_DATATYPE_NULL)) || (!agg2rearr && (rearr_alltoall_info_.stypes[i] != MPI_DATATYPE_NULL))){
        ret = MPI_Type_hvector(nvars, 1, rstride_between_vars,
                (agg2rearr) ? rearr_alltoall_info_.rtypes[i] : rearr_alltoall_info_.stypes[i], &rearr_rtype);
        if(ret == MPI_SUCCESS){
          ret = MPI_Type_commit(&rearr_rtype);
        }
        if(ret != MPI_SUCCESS){
          return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
            "Internal error while rearranging data from aggregating processes to IO processes (PIO_REARR_CONTIG). Unable to create vector types to receive data for data aggregation (iosysid = %d, nvars = %d)", ios_->iosysid, nvars);
        }
      }
      rearr_stypes_nvars.push_back(rearr_stype);
      rearr_rtypes_nvars.push_back(rearr_rtype);
    }
  }

  ret = SPIO_Util::Rearr_Util::alltoallw(
          sbuf,
          (agg2rearr) ? rearr_alltoall_info_.scounts.data() : rearr_alltoall_info_.rcounts.data(),
          (agg2rearr) ? rearr_alltoall_info_.sdispls.data() : rearr_alltoall_info_.rdispls.data(),
          (nvars > 1) ? rearr_stypes_nvars.data() : ((agg2rearr) ? rearr_alltoall_info_.stypes.data() : rearr_alltoall_info_.rtypes.data()),
          rbuf,
          (agg2rearr) ? rearr_alltoall_info_.rcounts.data() : rearr_alltoall_info_.scounts.data(),
          (agg2rearr) ? rearr_alltoall_info_.rdispls.data() : rearr_alltoall_info_.sdispls.data(),
          (nvars > 1) ? rearr_rtypes_nvars.data() : ((agg2rearr) ? rearr_alltoall_info_.rtypes.data() : rearr_alltoall_info_.stypes.data()),
          rearr_comm_,
          (agg2rearr) ? &(ios_->rearr_opts.comp2io) : &(ios_->rearr_opts.io2comp));
  if(ret != MPI_SUCCESS){
    return pio_err(ios_, NULL, ret, __FILE__, __LINE__,
      "Internal error while aggregating data from compute processes to aggregating processes (PIO_REARR_CONTIG). Unable to gather data during data aggregation (iosysid = %d)", ios_->iosysid);
  }

  for(std::size_t i = 0; i < rearr_stypes_nvars.size(); i++){
    if(rearr_stypes_nvars[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&(rearr_stypes_nvars[i]));
    }  
  }

  for(std::size_t i = 0; i < rearr_rtypes_nvars.size(); i++){
    if(rearr_rtypes_nvars[i] != MPI_DATATYPE_NULL){
      MPI_Type_free(&(rearr_rtypes_nvars[i]));
    }  
  }

  return ret;
}