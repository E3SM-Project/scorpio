#include <string>
#include <vector>
#include <map>
#include <utility>
#include <cassert>
#include <fstream>

#include <unistd.h>

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "mpi.h"
#include "spio_tracer.hpp"
#include "spio_logger.hpp"
#include "spio_tracer_mdata.hpp"
#include "spio_tracer_decomp.hpp"

namespace SPIO_Util{
  namespace Tracer{
    /* FIXME: Move to a singleton */
    namespace GVars{
      static std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> > trace_mdata_loggers_;
    }
  } // namespace Tracer
} // namespace SPIO_Util

std::string SPIO_Util::Tracer::get_mpi_comm_info(MPI_Comm comm)
{
  int wrank = 0, wsz = 0, comm_sz = 0, comm_rank = 0, ret = MPI_SUCCESS;

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz);
  assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_rank(comm, &comm_rank);
  assert(ret == MPI_SUCCESS);

  int comm_lmap[2] = {comm_rank, wrank};
  int comm_wmap[comm_sz * 2];
  int comm_color[wsz];
  int comm_key[wsz];

  ret = MPI_Gather(comm_lmap, 2, MPI_INT, comm_wmap, 2, MPI_INT, 0, comm);
  assert(ret == MPI_SUCCESS);

  std::string info = std::string("MPI_COMM_WORLD : [") + std::to_string(wrank) + std::string("/") + std::to_string(wsz) + std::string("]\n");
  info += std::string("MPI_COMM : [") + std::to_string(comm_rank) + std::string("/") + std::to_string(comm_sz) + std::string("]\n");

  if(comm_rank == 0){
    for(int i=0; i < wsz; i++){
      comm_color[i] = MPI_UNDEFINED;
      comm_key[i] = 0;
    }
    for(int i=0; i < comm_sz * 2; i+=2){
      comm_color[comm_wmap[i+1]] = 0;
      comm_key[comm_wmap[i+1]] = comm_wmap[i];
    }
    info += std::string("MPI_PROC_MAP :") + SPIO_Util::Tracer::Timed_func_call_tracer::arr_to_string(comm_wmap, comm_sz * 2) + "\n";
    info += std::string("MPI_PROC_MAP COLOR :") + SPIO_Util::Tracer::Timed_func_call_tracer::arr_to_string(comm_color, wsz) + "\n";
    info += std::string("MPI_PROC_MAP KEY :") + SPIO_Util::Tracer::Timed_func_call_tracer::arr_to_string(comm_key, wsz) + "\n";
  }

  return info;
}

std::string SPIO_Util::Tracer::get_trace_mdata_fname(int iosysid, int mpi_wrank)
{
  const std::string LOG_FILE_PREFIX = "spio_trace_mdata_";
  const std::string LOG_FILE_SUFFIX = ".log";
  std::string iosys_str = std::string("_iosys_") + ((iosysid != PIO_DEFAULT) ? std::to_string(iosysid) : "PIO_DEFAULT") + std::string("_");

  long long int pid = static_cast<long long int>(getpid());
  std::string log_fname = LOG_FILE_PREFIX + iosys_str + std::to_string(pid) + std::string("_") + std::to_string(mpi_wrank) + LOG_FILE_SUFFIX;

  return log_fname;
}

static inline std::string get_trace_mdata_header(int iosysid, int mpi_wrank)
{
  static const std::string spio_version = std::string("SCORPIO VERSION : ") +
                                          std::to_string(PIO_VERSION_MAJOR) + "." +
                                          std::to_string(PIO_VERSION_MINOR) + "." +
                                          std::to_string(PIO_VERSION_PATCH) + "\n";
  static const std::string banner =
                                  "=================================================================\n";
  static const std::string spio_trace_mdata_info = "\tSCORPIO TRACE META DATA\n" + spio_version;

  std::string iosys_info = std::string("I/O System ID : ") + std::to_string(iosysid) + "\n";

  std::string rank_info = std::string("MPI World rank : ") + std::to_string(mpi_wrank) + "\n";

  std::string log_info = std::string("Trace log file : ") + SPIO_Util::Tracer::get_trace_log_fname(iosysid, mpi_wrank) + "\n";

  std::string hdr = banner + spio_trace_mdata_info + iosys_info + rank_info + log_info + banner;

  return hdr;
}

static inline std::string get_trace_mdata_footer(void )
{
  static const std::string banner =
                                  "=================================================================\n";

  return banner;
}

SPIO_Util::Logger::MPI_logger<std::ofstream> &SPIO_Util::Tracer::get_iosys_trace_mdata_logger(int iosysid, int mpi_wrank)
{
  /* FIXME: We might need to trace both I/O and compute procs for async I/O */
  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_mdata_loggers_.find(std::to_string(iosysid));
  if(iter == SPIO_Util::Tracer::GVars::trace_mdata_loggers_.end()){
    const int MPI_ROOT_PROC = 0;
    int ret = MPI_SUCCESS, rank = -1;

    MPI_Comm comm = SPIO_Util::Tracer::PIO_DEFAULT_COMM;

    if(iosysid != PIO_DEFAULT){
      iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
      assert(ios);
      comm = ios->union_comm;
    }

    ret = MPI_Comm_rank(comm, &rank);
    assert(ret == MPI_SUCCESS);

    // FIXME: use unique_ptr
    std::ofstream *fstr = new std::ofstream();
    const std::string DEV_NULL = "/dev/null";
    std::string mdata_fname = get_trace_mdata_fname(iosysid, mpi_wrank);

    /* FIXME: We always log from the root proc, 0, and in the case of PIO_DEFAULT its the root process. This can cause issues when we use
     * PIO_DEFAULT as the I/O system, to handle user error cases etc where we cannot determine the I/O system, to log calls from a component
     * that does not have rank 0 of the MPI_COMM_WORLD/PIO_DEFAULT_COMM */
    fstr->open((rank == MPI_ROOT_PROC) ? mdata_fname.c_str() : DEV_NULL.c_str(), std::ofstream::out | std::ofstream::trunc);

    // FIXME: use insert() and get the iterator rather than insert and then find
    SPIO_Util::Logger::MPI_logger<std::ofstream> lstr(comm, fstr);
    lstr.log(get_trace_mdata_header(iosysid, mpi_wrank));
    SPIO_Util::Tracer::GVars::trace_mdata_loggers_[std::to_string(iosysid)] = lstr;
    iter = SPIO_Util::Tracer::GVars::trace_mdata_loggers_.find(std::to_string(iosysid));
    //std::pair<std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator, bool> res = SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //assert(res.second);
    //iter = res.first;
  }
  return iter->second;
}

void SPIO_Util::Tracer::finalize_iosys_trace_mdata_logger(std::string iosys_key)
{
  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_mdata_loggers_.find(iosys_key);
  if(iter != SPIO_Util::Tracer::GVars::trace_mdata_loggers_.end()){
    (*iter).second.log(get_trace_mdata_footer());
    std::ofstream *fstr = (*iter).second.get_log_stream();
    assert(fstr);
    fstr->close();
    delete fstr;

    SPIO_Util::Tracer::GVars::trace_mdata_loggers_.erase(iter);
  }
}
