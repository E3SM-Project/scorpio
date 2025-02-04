#include "spio_decomp_logger.hpp"
#include <stdexcept>
#include <cstdint>
#include <array>
#ifdef _PNETCDF
#include "pnetcdf.h"
#endif

SPIO_Util::Decomp_Util::Decomp_logger& SPIO_Util::Decomp_Util::Decomp_nc_logger::open(void )
{
#ifdef _PNETCDF
  int ret = MPI_SUCCESS;

  version_ =  std::to_string(PIO_VERSION_MAJOR) + "." +
              std::to_string(PIO_VERSION_MINOR) + "." +
              std::to_string(PIO_VERSION_PATCH);
  nprocs_ = comm_sz_;

  if(!is_io_proc_){
    return *this;
  }

  MPI_Info info = MPI_INFO_NULL;

  ret = MPI_Info_create(&info); assert(ret == MPI_SUCCESS);
  ret = MPI_Info_set(info, "nc_var_align_size", "1"); assert(ret == MPI_SUCCESS);

  int omode = NC_64BIT_DATA;
  if(is_read_only()){
    omode |= NC_NOWRITE;

    ret = ncmpi_open(io_comm_, log_fname_.c_str(), omode, info, &ncid_);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Opening decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }

  }
  else{
    assert(is_write_only());
    omode |= (NC_WRITE | NC_CLOBBER);

    ret = ncmpi_create(io_comm_, log_fname_.c_str(), omode, info, &ncid_);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Creating decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }

    ret = ncmpi_def_dim(ncid_, comm_sz_dim_name_.c_str(), comm_sz_, &comm_sz_dimid_); 
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Defining comm_sz dimension in decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }

    ret = ncmpi_put_att_text(ncid_, NC_GLOBAL, version_att_name_.c_str(), version_.size() + 1, version_.c_str());
    if(ret == NC_NOERR){
      ret = ncmpi_put_att_int(ncid_, NC_GLOBAL, nprocs_att_name_.c_str(), NC_INT, 1, &nprocs_);
    }
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Adding SCORPIO version/nprocs attributes to decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }

    ret = ncmpi_enddef(ncid_);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Ending define mode for decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }
  }

  MPI_Info_free(&info);
#endif
  return *this;
}

void SPIO_Util::Decomp_Util::Decomp_nc_logger::get_info(std::string &version, int &nprocs, int &ngdims, PIO_Offset &lcompmap_sz)
{
#ifdef _PNETCDF
  if(!info_cached_){
    read_and_cache_info();
  }
#endif
  if(info_cached_){
    version = version_;
    nprocs = nprocs_;
    ngdims = static_cast<int>(gdims_.size());
    lcompmap_sz = static_cast<PIO_Offset>(lcompmap_.size());
    return;
  }
}

void SPIO_Util::Decomp_Util::Decomp_nc_logger::get_gdims(int *gdims, std::size_t gdims_sz)
{
#ifdef _PNETCDF
  if(!info_cached_){
    read_and_cache_info();
  }
#endif
  if(info_cached_){
    assert(gdims && (gdims_sz == gdims_.size()));
    std::copy(gdims_.cbegin(), gdims_.cend(), gdims);
    return;
  }
}

void SPIO_Util::Decomp_Util::Decomp_nc_logger::get_lcompmap(PIO_Offset *lcompmap, std::size_t lcompmap_sz)
{
#ifdef _PNETCDF
  if(!info_cached_){
    read_and_cache_info();
  }
#endif
  if(info_cached_){
    assert(lcompmap && (lcompmap_sz == lcompmap_.size()));
    std::copy(lcompmap_.cbegin(), lcompmap_.cend(), lcompmap);
    return;
  }
}

SPIO_Util::Decomp_Util::Decomp_logger& SPIO_Util::Decomp_Util::Decomp_nc_logger::get(std::string &version, int &nprocs, std::vector<int> &gdims, std::vector<PIO_Offset> &lcompmap)
{
#ifdef _PNETCDF
  if(!info_cached_){
    read_and_cache_info();
  }
#endif
  if(info_cached_){
    version = version_;
    nprocs = nprocs_;
    gdims.resize(gdims_.size());
    std::copy(gdims_.cbegin(), gdims_.cend(), gdims.begin());
    lcompmap.resize(lcompmap_.size());
    std::copy(lcompmap_.cbegin(), lcompmap_.cend(), lcompmap.begin());
  }
  return *this;
}

void SPIO_Util::Decomp_Util::Decomp_nc_logger::read_and_cache_info(void )
{
  int ret = NC_NOERR;
#ifdef _PNETCDF
  MPI_Offset version_len = 0;
  int ndims = 0;
  std::vector<int> agg_counts(agg_comm_sz_, 0);
  std::vector<MPI_Offset> agg_gmap_chunk;
  if(is_io_proc_){
    assert(ncid_ != INVALID_ID);

    ret = ncmpi_inq_attlen(ncid_, NC_GLOBAL, version_att_name_.c_str(), &version_len);
    if(ret == NC_NOERR){
      version_.resize(static_cast<std::size_t>(version_len) + 1);

      char tmp_str[version_len + 1];
      tmp_str[version_len] = '\0';
      ret = ncmpi_get_att_text(ncid_, NC_GLOBAL, version_att_name_.c_str(), tmp_str);
      version_.assign(tmp_str);
    }
    if(ret == NC_NOERR){
      ret = ncmpi_get_att_int(ncid_, NC_GLOBAL, nprocs_att_name_.c_str(), &nprocs_);
    }
    if(ret == NC_NOERR){
      ret = ncmpi_get_att_int(ncid_, NC_GLOBAL, ndims_att_name_.c_str(), &ndims);
      if(ret == NC_NOERR){
        gdims_.resize(ndims);
        ret = ncmpi_get_att_int(ncid_, NC_GLOBAL, gdimlen_att_name_.c_str(), gdims_.data());
      }
    }
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Reading attributes (version/nprocs/ndims/gdimlen) from decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }

    //std::pair<int, int> proc_range = {comm_rank_, comm_rank_ + agg_comm_sz_};
    
    MPI_Offset start_var = comm_rank_;
    MPI_Offset count_var = (comm_rank_ + agg_comm_sz_ <= comm_sz_) ? agg_comm_sz_ : (comm_sz_ - comm_rank_);

    int tmp_varid = -1;
    ret = ncmpi_inq_varid(ncid_, counts_var_name_.c_str(), &tmp_varid);
    if(ret == NC_NOERR){
      ret = ncmpi_get_vara_int_all(ncid_, tmp_varid, &start_var, &count_var, agg_counts.data());
    }
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Reading counts array from decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }

    MPI_Offset agg_io_chunk_sz = 0;
    for(std::size_t i = 0; i < agg_counts.size(); i++){
      agg_io_chunk_sz += agg_counts[i];
    }

    MPI_Offset agg_io_chunk_start = agg_io_chunk_sz;
    ret = MPI_Exscan(MPI_IN_PLACE, &agg_io_chunk_start, 1, MPI_OFFSET, MPI_SUM, io_comm_); assert(ret == MPI_SUCCESS);
    if(io_comm_rank_ == 0){
      agg_io_chunk_start = 0;
    }

    agg_gmap_chunk.resize(agg_io_chunk_sz);
    ret = ncmpi_inq_varid(ncid_, gmap_var_name_.c_str(), &tmp_varid);
    if(ret == NC_NOERR){
      ret = ncmpi_get_vara_longlong_all(ncid_, tmp_varid, &agg_io_chunk_start,
              &agg_io_chunk_sz, agg_gmap_chunk.data());
    }
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Reading gmap array from decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
    }
  }

  ret = MPI_Bcast(&version_len, 1, MPI_OFFSET, 0, agg_comm_); assert(ret == MPI_SUCCESS);
  ret = MPI_Bcast(&nprocs_, 1, MPI_INT, 0, agg_comm_); assert(ret == MPI_SUCCESS);
  ret = MPI_Bcast(&ndims, 1, MPI_INT, 0, agg_comm_); assert(ret == MPI_SUCCESS);

  char tmp_str[version_len + 1];
  tmp_str[version_len] = '\0';
  std::copy(version_.cbegin(), version_.cend(), tmp_str);
  ret = MPI_Bcast(tmp_str, version_len, MPI_CHAR, 0, agg_comm_); assert(ret == MPI_SUCCESS);
  if(agg_comm_rank_ != 0){
    version_.assign(tmp_str);
    gdims_.resize(ndims);
  }
  ret = MPI_Bcast(gdims_.data(), ndims, MPI_INT, 0, agg_comm_); assert(ret == MPI_SUCCESS);

  int lcompmap_sz = 0;
  ret = MPI_Scatter(agg_counts.data(), 1, MPI_INT, &lcompmap_sz, 1, MPI_INT, 0, agg_comm_); assert(ret == MPI_SUCCESS);

  lcompmap_.resize(lcompmap_sz);
  std::vector<int> agg_starts(agg_counts.size());
  int cur_start = 0;
  for(std::size_t i = 0; i < agg_starts.size(); i++){
    agg_starts[i] = cur_start;
    cur_start += agg_counts[i];
  }
  ret = MPI_Scatterv(agg_gmap_chunk.data(), agg_counts.data(), agg_starts.data(), MPI_OFFSET,
          lcompmap_.data(), lcompmap_sz, MPI_OFFSET, 0, agg_comm_); assert(ret == MPI_SUCCESS);

  info_cached_ = true;

#endif
}

SPIO_Util::Decomp_Util::Decomp_logger &SPIO_Util::Decomp_Util::Decomp_nc_logger::put(io_desc_t *iodesc)
{
#ifdef _PNETCDF
  int ret = MPI_SUCCESS;

  assert(iodesc);

  /* Cache the iodesc info */
  gdims_.resize(iodesc->ndims);
  std::copy(iodesc->dimlen, iodesc->dimlen + iodesc->ndims, gdims_.begin());
  lcompmap_.resize(iodesc->maplen);
  std::copy(iodesc->map, iodesc->map + iodesc->maplen, lcompmap_.begin());
  info_cached_ = true;

  std::vector<int> agg_starts, agg_counts;
  MPI_Offset agg_io_chunk_sz = 0;

  /* Aggregation of local map lengths & map happens in agg_comm_, the
   * data is written out to the file using io_comm_. Each rank 0 proc in
   * agg_comm is used to create the io_comm_
   */
  /* Gather/Aggregate local map lengths - map lengths of each compute proc
   * should be available in agg_counts
   * The starts/displacements (on the global map) for map chunks in each
   * agg process should be available in agg_starts
   * agg_io_chunk_sz : Total size of aggregated gmap chunk on this I/O proc
   */
  agg_starts.resize(agg_comm_sz_);
  agg_counts.resize(agg_comm_sz_);
  gather_starts_counts(agg_starts, agg_counts, agg_io_chunk_sz, iodesc);

  std::vector<MPI_Offset> gmap_chunk;
  if(is_io_proc_){
    gmap_chunk.resize(agg_io_chunk_sz);
  }

  /* Gather the local compmaps from compute procs to the I/O processes */
  gather_gmap(agg_starts, agg_counts, gmap_chunk, iodesc);

  if(!is_io_proc_){
    return *this;
  }

  assert(sizeof(PIO_Offset) == sizeof(MPI_Offset));
  MPI_Offset agg_io_chunk_count = agg_io_chunk_sz;

  /* Find starts/Displacements for "counts" var and "gmap" var among I/O processes */
  std::array<MPI_Offset, 2> starts_for_counts_and_gmap = { static_cast<MPI_Offset>(agg_comm_sz_), agg_io_chunk_count };
  ret = MPI_Exscan(MPI_IN_PLACE, starts_for_counts_and_gmap.data(), starts_for_counts_and_gmap.size(), MPI_OFFSET, MPI_SUM, io_comm_); assert(ret == MPI_SUCCESS);
  if(io_comm_rank_ == 0){
    starts_for_counts_and_gmap = {0, 0};
  }

  MPI_Offset gmaplen = 0;
  ret = MPI_Allreduce(&agg_io_chunk_count, &gmaplen, 1, MPI_OFFSET, MPI_SUM, io_comm_); assert(ret == MPI_SUCCESS);

  assert(ncid_ != INVALID_ID);

  ret = ncmpi_redef(ncid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Redefine mode for decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  ret = ncmpi_put_att_int(ncid_, NC_GLOBAL, ioid_att_name_.c_str(), NC_INT, 1, &(iodesc->ioid));
  if(ret == NC_NOERR){
    ret = ncmpi_put_att_int(ncid_, NC_GLOBAL, ndims_att_name_.c_str(), NC_INT, 1, &(iodesc->ndims));
  }
  if(ret == NC_NOERR){
    ret = ncmpi_put_att_int(ncid_, NC_GLOBAL, gdimlen_att_name_.c_str(), NC_INT, iodesc->ndims, iodesc->dimlen);
  }
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Writing ioid/ndims/gdimlen to decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  assert(sizeof(MPI_Offset) == sizeof(int64_t));
  /* Global start offsets for reads/writes */
  int counts_varid = INVALID_ID;
  int gmaplen_dimid = INVALID_ID;
  int gmap_varid = INVALID_ID;

  ret = ncmpi_def_var(ncid_, counts_var_name_.c_str(), NC_INT, 1, &comm_sz_dimid_, &counts_varid);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Defining counts var in decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  ret = ncmpi_def_dim(ncid_, gmaplen_dim_name_.c_str(), gmaplen, &gmaplen_dimid);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Defining gmaplen dimension, size = ") + std::to_string(static_cast<long long int>(gmaplen)) + std::string(", in decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  /* The global compmap */
  ret = ncmpi_def_var(ncid_, gmap_var_name_.c_str(), NC_INT64, 1, &gmaplen_dimid, &gmap_varid);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Defining var to store global compmap in decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  ret = ncmpi_enddef(ncid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Ending redefine mode for decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  /* Write compmap counts */
  MPI_Offset counts_start = starts_for_counts_and_gmap[0];
  MPI_Offset counts_count = agg_comm_sz_;
  assert((counts_start < static_cast<MPI_Offset>(comm_sz_)) &&
    (counts_count == static_cast<MPI_Offset>(agg_counts.size())));
  ret = ncmpi_iput_vara_int(ncid_, counts_varid, &counts_start, &counts_count, agg_counts.data(), NULL);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Writing gmap process counts to decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  MPI_Offset agg_io_chunk_start = starts_for_counts_and_gmap[1];
  assert((agg_io_chunk_start < gmaplen) && (agg_io_chunk_start + agg_io_chunk_count <= gmaplen));
  /* Write compmap */
  ret = ncmpi_iput_vara_longlong(ncid_, gmap_varid, &agg_io_chunk_start, &agg_io_chunk_count, gmap_chunk.data(), NULL);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Writing gmap to decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  ret = ncmpi_wait_all(ncid_, NC_REQ_ALL, NULL, NULL);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Waiting on writes of gmap/counts arrays to decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

#endif
  return *this;
}

void SPIO_Util::Decomp_Util::Decomp_nc_logger::gather_starts_counts(std::vector<int> &agg_starts, std::vector<int> &agg_counts, MPI_Offset &agg_io_chunk_sz, io_desc_t *iodesc)
{
  int ret = MPI_SUCCESS;

  assert(iodesc && (agg_comm_sz_ > 0));
  agg_io_chunk_sz = 0;

  int lmap_sz = iodesc->maplen;

  ret = MPI_Gather(&lmap_sz, 1, MPI_INT, agg_counts.data(), 1, MPI_INT, 0, agg_comm_); assert(ret == MPI_SUCCESS);

  int cur_start = 0;
  for(std::size_t i = 0; i < agg_starts.size(); i++){
    agg_starts[i] = cur_start;
    cur_start += agg_counts[i];
    agg_io_chunk_sz += static_cast<MPI_Offset>(agg_counts[i]);
  }
}

void SPIO_Util::Decomp_Util::Decomp_nc_logger::gather_gmap(const std::vector<int> &starts, const std::vector<int> &counts, std::vector<MPI_Offset> &gmap_chunk, io_desc_t *iodesc)
{
  int ret = MPI_SUCCESS;

  assert(iodesc);
  assert((agg_comm_rank_ != 0) || ((gmap_chunk.size() > 0) && (counts.size() > 0) && (starts.size() > 0)));

  ret = MPI_Gatherv(iodesc->map, iodesc->maplen, MPI_OFFSET, gmap_chunk.data(),
          counts.data(), starts.data(), MPI_OFFSET, 0, agg_comm_); assert(ret == MPI_SUCCESS);
}

void SPIO_Util::Decomp_Util::Decomp_nc_logger::close(void )
{
  int ret = MPI_SUCCESS;

#ifdef _PNETCDF
  if(!is_io_proc_){
    return;
  }

  assert(ncid_ != INVALID_ID);
  ret = ncmpi_close(ncid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Closing decomp log file, \"") + log_fname_ + std::string("\", failed, ierr =") + std::to_string(ret) + std::string(" ( ") + std::string(ncmpi_strerror(ret)) + std::string(")"));
  }

  ncid_ = INVALID_ID;
#endif
}
