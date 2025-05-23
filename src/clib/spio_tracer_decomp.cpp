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
#include <stdexcept>

#include <unistd.h>

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "mpi.h"
#if PIO_USE_PNETCDF
  #include "pnetcdf.h"
#endif
#include "spio_tracer.hpp"
#include "spio_tracer_mdata.hpp"
#include "spio_tracer_decomp.hpp"

namespace SPIO_Util{
  namespace Tracer{
    /* FIXME: Move to a singleton */
    namespace GVars{
      static std::map<int, SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger > trace_decomp_loggers_;
    }

    /* Initialize static vars */
    namespace Decomp_Utils{
      int Decomp_nc_logger::uniq_dimid_idx_ = 0;
    }
  } // namespace Tracer
} // namespace SPIO_Util


std::string SPIO_Util::Tracer::get_trace_decomp_fname(int iosysid, int mpi_wrank)
{
  return get_trace_decomp_logger(iosysid, mpi_wrank).get_log_fname();
}

std::string SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger::get_trace_decomp_tmp_fname(int iosysid, MPI_Comm comm, int comm_rank, int mpi_wrank)
{
  int ret = MPI_SUCCESS;
  const std::string LOG_FILE_PREFIX = "spio_trace_decomp_";
  const std::string LOG_FILE_SUFFIX = ".nc";
  std::string iosys_str = std::string("_iosys_") + ((iosysid != PIO_DEFAULT) ? std::to_string(iosysid) : "PIO_DEFAULT") + std::string("_");

  /* Use the global world rank of root/rank0 process as a uniq id for this I/O system */
  int uniq_id = mpi_wrank;
  ret = MPI_Bcast(&uniq_id, 1, MPI_INT, 0, comm);
  assert(ret == MPI_SUCCESS);

  std::string decomp_fname = LOG_FILE_PREFIX + iosys_str + std::to_string(uniq_id) + LOG_FILE_SUFFIX;

  return decomp_fname;
}

SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger &SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger::set_iosysid(int iosysid)
{
  int ret = 0;

  iosysid_ = iosysid;

  //ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank_);
  //assert(ret == MPI_SUCCESS);
  iosystem_desc_t *iosys = pio_get_iosystem_from_id(iosysid_);
  if((iosysid_ == PIO_DEFAULT) || !iosys){
    iosysid_ = PIO_DEFAULT;
    comm_ = PIO_DEFAULT_COMM;
  }
  else{
    comm_ = iosys->union_comm;
  }

  ret = MPI_Comm_rank(comm_, &comm_rank_);
  assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm_, &comm_sz_);
  assert(ret == MPI_SUCCESS);

  return *this;
}

void SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger::init(void )
{
  int ret = 0;
  MPI_Info info;

  assert(iosysid_ != INVALID_IOSYSID);

  iosystem_desc_t *iosys = pio_get_iosystem_from_id(iosysid_);
  if((iosysid_ == PIO_DEFAULT) || !iosys){
    iosysid_ = PIO_DEFAULT;
    comm_ = PIO_DEFAULT_COMM;
  }
  else{
    comm_ = iosys->union_comm;
  }
  int wrank = -1;
  //int wsz = 0;

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  assert(ret == MPI_SUCCESS);
  //ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  //assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_rank(comm_, &comm_rank_);
  assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm_, &comm_sz_);
  assert(ret == MPI_SUCCESS);

  /* Create NetCDF file and write header in the global attributes */
  ret = MPI_Info_create(&info);
  assert(ret == MPI_SUCCESS);
  ret = MPI_Info_set(info, "nc_var_align_size", "1");
  assert(ret == MPI_SUCCESS);

  fname_ = get_trace_decomp_tmp_fname(iosysid_, comm_, comm_rank_, wrank);

#if PIO_USE_PNETCDF
  int flags = NC_CLOBBER | NC_64BIT_DATA;
  ret = ncmpi_create(comm_, fname_.c_str(), flags, info, &ncid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Unable to create NetCDF I/O decomposition log file, file = ") + fname_ +
            std::string(", iosysid = ") + std::to_string(iosysid_));
  }

  const std::string spio_version_name = std::string("SCORPIO_VERSION");
  const std::string spio_version = std::to_string(PIO_VERSION_MAJOR) + "." +
                                          std::to_string(PIO_VERSION_MINOR) + "." +
                                          std::to_string(PIO_VERSION_PATCH);
  ret = ncmpi_put_att_text(ncid_, NC_GLOBAL, spio_version_name.c_str(), spio_version.size() + 1, spio_version.c_str());
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Putting SCORPIO Version attribute in I/O decomposition log file, file = ") + fname_ +
            std::string(", failed. iosysid = ") + std::to_string(iosysid_));
  }

  const std::string spio_trace_log_aname = std::string("Trace_log_file");
  const std::string spio_trace_log_name = SPIO_Util::Tracer::get_trace_log_fname(iosysid_, wrank);
  ret = ncmpi_put_att_text(ncid_, NC_GLOBAL, spio_trace_log_aname.c_str(), spio_trace_log_name.size() + 1, spio_trace_log_name.c_str());
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Putting trace log file name/attribute in I/O decomposition log file, file = ") + fname_ +
            std::string(", failed. iosysid = ") + std::to_string(iosysid_));
  }

  const std::string spio_trace_mdata_aname = std::string("Trace_MData_file");
  const std::string spio_trace_mdata_name = SPIO_Util::Tracer::get_trace_mdata_fname(iosysid_, wrank);
  ret = ncmpi_put_att_text(ncid_, NC_GLOBAL, spio_trace_mdata_aname.c_str(), spio_trace_mdata_name.size() + 1, spio_trace_mdata_name.c_str());
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Putting trace meta-data file name/attribute in I/O decomposition log file, file = ") + fname_ +
            std::string(", failed. iosysid = ") + std::to_string(iosysid_));
  }

  ret = ncmpi_put_att_int(ncid_, NC_GLOBAL, "IOSystemID", NC_INT, 1, &iosysid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Putting I/O system id as an attribute in I/O decomposition log file, file = ") + fname_ +
            std::string(", failed. iosysid = ") + std::to_string(iosysid_));
  }

  ret = ncmpi_put_att_int(ncid_, NC_GLOBAL, "MPIWRank", NC_INT, 1, &wrank);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Putting MPI world rank as an attribute in I/O decomposition log file, file = ") + fname_ +
            std::string(", failed. iosysid = ") + std::to_string(iosysid_));
  }

  ret = ncmpi_enddef(ncid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Ending NetCDF define mode in I/O decomposition log file, file = ") + fname_ +
            std::string(", failed. iosysid = ") + std::to_string(iosysid_));
  }
#endif // PIO_USE_PNETCDF

  ret = MPI_Info_free(&info);
  assert(ret == MPI_SUCCESS);
}

void SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger::log_decomp(int ioid, const PIO_Offset *map, int mapsz)
{
  int ret = MPI_SUCCESS;

  /* FIXME: Remove this requirement and cast the map appropriately */
  assert(sizeof(PIO_Offset) == sizeof(long long int));
  assert(sizeof(PIO_Offset) == sizeof(MPI_Offset));
  assert(sizeof(PIO_Offset) == 8);

  assert(comm_sz_ > 0);
  assert(ncid_ != INVALID_NCID);

  MPI_Offset mapsz_off = static_cast<MPI_Offset>(mapsz);
  MPI_Offset *lmapsz = NULL;
  if(comm_rank_ == 0){
    lmapsz = (MPI_Offset *)malloc(sizeof(MPI_Offset) * comm_sz_ * 2);
    if(lmapsz == NULL){
      throw std::runtime_error(std::string("Out of memory allocating memory for gathering local map sizes on root, ") +
              std::to_string(static_cast<long long int>(sizeof(MPI_Offset) * comm_sz_ * 2)) + std::string(" bytes, ioid = ") +
              std::to_string(ioid));
    }
  }

  ret = MPI_Gather(&mapsz_off, 1, MPI_OFFSET, lmapsz, 1, MPI_OFFSET, 0, comm_);
  if(ret != MPI_SUCCESS){
    throw std::runtime_error(std::string("Unable to gaher local map sizes on root, MPI_Gather failed, ioid = ") + std::to_string(ioid));
  }

  /* Convert local map sizes from each proc to starting offsets for each proc */
  if(comm_rank_ == 0){
    /* Find starting offset for writing the map out, rank 0 writes at offset 0 */
    MPI_Offset cur_sz = lmapsz[0];
    lmapsz[0] = 0;
    for(int i=1; i < comm_sz_; i++){
      MPI_Offset tmp_sz = lmapsz[i];
      lmapsz[i] = lmapsz[i-1] + cur_sz;
      cur_sz = tmp_sz;
    }

    MPI_Offset gmapsz = lmapsz[comm_sz_ - 1] + cur_sz;

    for(int i=2 * comm_sz_ - 1; i > 0; i-=2){
      lmapsz[i] = lmapsz[i/2];
      lmapsz[i-1] = gmapsz;
    }
  }

  /* gmap_sz_start[] = {global map size, local start offset} */
  MPI_Offset gmap_sz_start[2];
  ret = MPI_Scatter(lmapsz, 2, MPI_OFFSET, gmap_sz_start, 2, MPI_OFFSET, 0, comm_);
  if(ret != MPI_SUCCESS){
    throw std::runtime_error(std::string("Scattering global map sizes/start offset failed, ioid = ") + std::to_string(ioid));
  }

  if(lmapsz){
    free(lmapsz);
  }

  /* FIXME: Do we need a separate variable for global map size (Instead of having it multiple
   * times in the decomp_X variable, along with start/count)? In that case the reader would have
   * to read 2 variables instead of 1 to get gsz/start/count for the decomps
   * The global map size could also be added as an attribute to the decomp variable, decomp_X
   */
  /* gsz_start_count_for_saved_decomps_[] = {global map size, local start offset, local map sz} */
  gsz_start_count_for_saved_decomps_.insert(gsz_start_count_for_saved_decomps_.end(),
    {static_cast<long long> (gmap_sz_start[0]), static_cast<long long>(gmap_sz_start[1]),
      static_cast<long long> (mapsz_off)});

  int decomp_var_dimid = get_dim_id(static_cast<PIO_Offset>(gmap_sz_start[0]));
  std::string decomp_vname = std::string("decomp_") + std::to_string(ioid);
  int decomp_varid = INVALID_NCID;

#if PIO_USE_PNETCDF
  ret = ncmpi_redef(ncid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Unable to enter redef mode to write decompositing map, ncid = ") + std::to_string(ncid_) +
            std::string(", vname = ") + decomp_vname);
  }

  ret = ncmpi_def_var(ncid_, decomp_vname.c_str(), NC_INT64, 1, &decomp_var_dimid, &decomp_varid);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Error while defining variable to write decomposition map, ncid = ") + std::to_string(ncid_) +
            std::string(", vname = ") + decomp_vname);
  }

  ret = ncmpi_enddef(ncid_);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Unable to end redef mode while writing decomposition map, ncid = ") + std::to_string(ncid_) +
            std::string(", vname = ") + decomp_vname);
  }

  ret = PIO_EINTERNAL;
  if(sizeof(PIO_Offset) == sizeof(long long)){
    ret = ncmpi_iput_vara_longlong(ncid_, decomp_varid, &(gmap_sz_start[1]), &mapsz_off, (const long long *)(map), NULL);
  }
  else if(sizeof(PIO_Offset) == sizeof(long)){
    ret = ncmpi_iput_vara_long(ncid_, decomp_varid, &(gmap_sz_start[1]), &mapsz_off, (const long *) (map), NULL);
  }

  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Unable to write decomposition map, ncid = ") + std::to_string(ncid_) +
            std::string(", vname = ") + decomp_vname);
  }

  /* FIXME: Ideally we just need to do one wait all during finalize. But PnetCDF, 1.12.3, crashes with too many pending reqs, so
   * to be safe wait after each write. We could also wait after some specific number of writes (or based on buffered data in
   * a process)
   */
  ret = ncmpi_wait_all(ncid_, NC_REQ_ALL, NULL, NULL);
  if(ret != NC_NOERR){
    throw std::runtime_error(std::string("Error while waiting after writing the decomposition map, ncid = ") + std::to_string(ncid_) +
            std::string(", vname = ") + decomp_vname);
  }
#endif // PIO_USE_PNETCDF
}

void SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger::finalize(void )
{
#if PIO_USE_PNETCDF
  if(ncid_ != INVALID_NCID){
    int ret = MPI_SUCCESS;
    int dim_comm_sz_dimid = INVALID_NCID, dim_ndecompsx3_dimid = INVALID_NCID;

    assert(comm_sz_ > 0);

    /* Create a var to store gsz/start/count for stored decomps */
    long long int ndecompsx3 = gsz_start_count_for_saved_decomps_.size();

    if(ndecompsx3 > 0){
      ret = ncmpi_redef(ncid_);
      if(ret != NC_NOERR){
        throw std::runtime_error(std::string("Unable to enter redef mode to write start/count for decomposition map, ncid = ") + std::to_string(ncid_));
      }

      ret = ncmpi_def_dim(ncid_, "dim_comm_sz", comm_sz_, &dim_comm_sz_dimid);
      if(ret != NC_NOERR){
        throw std::runtime_error(std::string("Error while defining comm size dimension, ncid = ") + std::to_string(ncid_) +
                std::string(", comm size = ") + std::to_string(comm_sz_));
      }

      dim_sz_to_id_[static_cast<PIO_Offset>(comm_sz_)] = dim_comm_sz_dimid;

      ret = ncmpi_def_dim(ncid_, "dim_ndecompsx3", static_cast<MPI_Offset>(ndecompsx3), &dim_ndecompsx3_dimid);
      if(ret != NC_NOERR){
        throw std::runtime_error(std::string("Error while defining number_of_decompsx3 dimension, ncid = ") + std::to_string(ncid_) +
                std::string(", number of decompisitions x 3 = ") + std::to_string(static_cast<long long int>(ndecompsx3)));
      }

      dim_sz_to_id_[static_cast<PIO_Offset>(gsz_start_count_for_saved_decomps_.size())] = dim_ndecompsx3_dimid;

      /* FIXME: Do we need a separate variable for global map size */
      int gsz_start_count_varid = INVALID_NCID;
      int gsz_start_count_var_dimids[] = {dim_comm_sz_dimid, dim_ndecompsx3_dimid};
      ret = ncmpi_def_var(ncid_, "gsz_start_count_var", NC_INT64, 2, gsz_start_count_var_dimids, &gsz_start_count_varid);
      if(ret != NC_NOERR){
        throw std::runtime_error(std::string("Error while defining variable to store global map sizes/starts/counts, ncid = ") + std::to_string(ncid_));
      }

      ret = ncmpi_enddef(ncid_);
      if(ret != NC_NOERR){
        throw std::runtime_error(std::string("Unable to end redef mode to write start/count for decomposition map, ncid = ") + std::to_string(ncid_));
      }

      MPI_Offset gsz_start_count_var_start[] = {comm_rank_, 0};
      MPI_Offset gsz_start_count_var_count[] = {1, static_cast<MPI_Offset>(ndecompsx3)};
      ret = PIO_EINTERNAL;
      if(sizeof(MPI_Offset) == sizeof(long long)){
        ret = ncmpi_iput_vara_longlong(ncid_, gsz_start_count_varid, gsz_start_count_var_start, gsz_start_count_var_count, (const long long *) gsz_start_count_for_saved_decomps_.data(), NULL);
      }
      else if(sizeof(MPI_Offset) == sizeof(long)){
        ret = ncmpi_iput_vara_long(ncid_, gsz_start_count_varid, gsz_start_count_var_start, gsz_start_count_var_count, (const long *) gsz_start_count_for_saved_decomps_.data(), NULL);
      }

      if(ret != NC_NOERR){
        throw std::runtime_error(std::string("Error writing variable to store global map sizes/starts/counts, ncid = ") + std::to_string(ncid_));
      }
    }

    ret = ncmpi_wait_all(ncid_, NC_REQ_ALL, NULL, NULL);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Error waiting after writing variable to store global map sizes/starts/counts, ncid = ") + std::to_string(ncid_));
    }

    ret = ncmpi_close(ncid_);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Error closing file after writing variable to store global map sizes/starts/counts, ncid = ") + std::to_string(ncid_));
    }

    ncid_ = INVALID_NCID;
  }
#endif // PIO_USE_PNETCDF
}

int SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger::get_dim_id(PIO_Offset dimsz)
{
  int dimid = INVALID_NCID, ret = 0;

#if PIO_USE_PNETCDF
  std::map<PIO_Offset, int>::iterator dim_id_iter = dim_sz_to_id_.find(dimsz);
  if(dim_id_iter == dim_sz_to_id_.end()){
    assert(ncid_ != INVALID_NCID);

    ret = ncmpi_redef(ncid_);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Unable to enter redef mode to define dimension, dim size = ") +
              std::to_string(static_cast<long long int>(dimsz)) + std::string(", ncid = ") + std::to_string(ncid_));
    }

    std::string dimid_name = std::string("dimid_") + std::to_string(uniq_dimid_idx_++);

    ret = ncmpi_def_dim(ncid_, dimid_name.c_str(), static_cast<MPI_Offset>(dimsz), &dimid);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Unable to define dimension, dim size = ") +
              std::to_string(static_cast<long long int>(dimsz)) + std::string(", dim name = ") + dimid_name +
              std::string(", ncid = ") + std::to_string(ncid_));
    }

    dim_sz_to_id_[dimsz] = dimid;

    ret = ncmpi_enddef(ncid_);
    if(ret != NC_NOERR){
      throw std::runtime_error(std::string("Unable to end redef mode to define dimension, dim size = ") +
              std::to_string(static_cast<long long int>(dimsz)) + std::string(", ncid = ") + std::to_string(ncid_));
    }
  }
  else{
    dimid = dim_id_iter->second;
  }
#endif // PIO_USE_PNETCDF
  return dimid;
}

SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger &SPIO_Util::Tracer::get_trace_decomp_logger(int iosysid, int mpi_wrank)
{
  std::map<int, SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger >::iterator iter = SPIO_Util::Tracer::GVars::trace_decomp_loggers_.find(iosysid);
  if(iter == SPIO_Util::Tracer::GVars::trace_decomp_loggers_.end()){
    //SPIO_Util::Tracer::GVars::trace_decomp_loggers_[iosysid].set_iosysid(iosysid).init();
    SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger logger(iosysid);
    logger.init();
    std::pair<std::map<int, SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger >::iterator, bool> res = 
      SPIO_Util::Tracer::GVars::trace_decomp_loggers_.insert({iosysid, logger});
    assert(res.second);
    iter = res.first;
  }

  return iter->second;
}

void SPIO_Util::Tracer::trace_decomp(int iosysid, int mpi_wrank, int ioid, const PIO_Offset *map, int mapsz)
{
  get_trace_decomp_logger(iosysid, mpi_wrank).log_decomp(ioid, map, mapsz);
}

void SPIO_Util::Tracer::finalize_trace_decomp(int iosysid)
{
  std::map<int, SPIO_Util::Tracer::Decomp_Utils::Decomp_nc_logger >::iterator iter = SPIO_Util::Tracer::GVars::trace_decomp_loggers_.find(iosysid);
  if(iter != SPIO_Util::Tracer::GVars::trace_decomp_loggers_.end()){
    iter->second.finalize();
    SPIO_Util::Tracer::GVars::trace_decomp_loggers_.erase(iter);
  }
}
