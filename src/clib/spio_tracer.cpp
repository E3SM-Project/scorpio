#include <string>
#include <vector>
#include <map>
#include <utility>
#include <cassert>
#include <fstream>
#include <iostream>

#include <unistd.h>

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "mpi.h"
#include "spio_tracer.hpp"
#include "spio_tracer_mdata.hpp"
#include "spio_logger.hpp"

namespace SPIO_Util{
  namespace Tracer{
    /* FIXME: Move to a singleton */
    namespace GVars{
      static std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> > trace_loggers_;
      static Tracer_mdata tracer_mdata_;
    }

    /* static member defn */
    const MPI_Comm Timed_func_call_tracer::PIO_DEFAULT_COMM = MPI_COMM_WORLD;
    const std::string Timed_func_call_tracer::NULL_PTR = "NULL";
    int Timed_func_call_tracer::gfunc_id_ = 0;
  } // namespace Tracer
} // namespace SPIO_Util

SPIO_Util::Tracer::Timed_func_call_tracer::Timed_func_call_tracer(const std::string &func_name) : func_id_(gfunc_id_), func_name_(func_name), mpi_comm_(MPI_COMM_NULL), wrank_(INVALID_RANK), iosysid_(INVALID_IOSYSID), fh_(INVALID_FH), is_io_proc_(false), needs_finalize_(false)
{
  gfunc_id_++;
  timer_.start();
}

SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_iosys_id(int iosysid)
{
  iosysid_ = iosysid;
  if(iosysid != PIO_DEFAULT){
    iosystem_desc_t *iosys = pio_get_iosystem_from_id(iosysid);
    /* FIXME: Throw an exception instead */
    if(iosys){
      mpi_comm_ = iosys->union_comm;
    }
    else{
      /* User error - invalid iosysid */
      mpi_comm_ = PIO_DEFAULT_COMM;
      iosysid_ = PIO_DEFAULT;
    }
    /* For compute comps in async I/O, the I/O system ids corresponding
      to other compute comms are mostly just placeholders, all comms are
      NULL here - just ignore these */
    assert( (mpi_comm_ != MPI_COMM_NULL) ||
            ( (iosys->io_comm == MPI_COMM_NULL) &&
              (iosys->comp_comm == MPI_COMM_NULL) &&
              (iosys->async)  ) );
  }
  else{
    mpi_comm_ = PIO_DEFAULT_COMM;
  }

  if(mpi_comm_ != MPI_COMM_NULL){
    iosys_trace_key_ = std::to_string(iosysid_);

    int ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank_);
    assert(ret == MPI_SUCCESS);
  }
  else{
    /* For compute comps in async I/O, the I/O system ids corresponding
      to other compute comms are mostly just placeholders, all comms are
      NULL here - just ignore these */
    /* Ignore logging in this process */
    iosysid_ = INVALID_IOSYSID;
  }

  if(iosysid_ != INVALID_IOSYSID){
    GVars::tracer_mdata_.add_iosysid(iosysid_);
  }

  return *this;
}

SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_file_id(int fh)
{
  int ret = PIO_NOERR;
  file_desc_t *file = NULL;

  fh_ = fh;
  ret = pio_get_file(fh_, &file);
  /* FIXME: Throw an exception instead */
  if(ret == PIO_NOERR){
    assert(file && file->iosystem);
    iosysid_ = file->iosystem->iosysid;
    mpi_comm_ = file->iosystem->union_comm;

    GVars::tracer_mdata_.add_ncid(iosysid_, fh);
  }
  else{
    /* Most likely a user error, specifying an invalid file id, log the call to default logger */
    iosysid_ = PIO_DEFAULT;
    mpi_comm_ = PIO_DEFAULT_COMM;
  }

  iosys_trace_key_ = std::to_string(iosysid_);

  return *this;
}

SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_dim_id(int fh, int dimid)
{
  assert(iosysid_ != INVALID_IOSYSID);
  GVars::tracer_mdata_.add_dimid(iosysid_, fh, dimid);

  return *this;
}

SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_var_id(int fh, int varid)
{
  assert(iosysid_ != INVALID_IOSYSID);
  GVars::tracer_mdata_.add_varid(iosysid_, fh, varid);

  return *this;
}

SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_decomp_info(int decomp_id, const PIO_Offset *map, int sz)
{
  if(iosysid_ != INVALID_IOSYSID){
    GVars::tracer_mdata_.add_ioid(iosysid_);
  }

  return *this;
}

void SPIO_Util::Tracer::Timed_func_call_tracer::flush(void )
{
  /* Serialize the function call */
  std::string ser_fcall(  std::to_string(func_id_) + FUNC_ID_SEP +
                          std::to_string(timer_.get_start_time()) + FUNC_TIME_SEP +
                          FUNC_ENTER + func_name_ + FUNC_CALL_PREFIX);
  if(args_.size() > 0){
    std::vector<std::pair<std::string, std::string> >::const_iterator iter = args_.cbegin();
    ser_fcall += (*iter).first + ARG_EQUAL + (*iter).second;
    for(++iter ;iter != args_.cend(); ++iter){
      ser_fcall += ARG_SEP + (*iter).first + ARG_EQUAL + (*iter).second;
    }
  }
  ser_fcall += FUNC_CALL_SUFFIX;

  if(iosysid_ != INVALID_IOSYSID){
    SPIO_Util::Logger::MPI_logger<std::ofstream> &logger = SPIO_Util::Tracer::get_iosys_trace_logger(iosysid_, wrank_);
    logger.log(ser_fcall);
    logger.flush();
  }

  args_.clear();
}

void SPIO_Util::Tracer::Timed_func_call_tracer::finalize(void )
{
  if(iosysid_ != INVALID_IOSYSID){
    SPIO_Util::Logger::MPI_logger<std::ofstream> &logger = SPIO_Util::Tracer::get_iosys_trace_mdata_logger(iosysid_, wrank_);
    logger.slog(get_mpi_comm_info(mpi_comm_));
    logger.log(std::string("I/O System Info: ") + GVars::tracer_mdata_.get_mdata(iosysid_));
    logger.flush();
  }
  needs_finalize_ = true;
}

SPIO_Util::Tracer::Timed_func_call_tracer::~Timed_func_call_tracer()
{
  timer_.stop();
  log_func_call_exit();
  if(needs_finalize_){
    SPIO_Util::Tracer::finalize_iosys_trace_logger(iosys_trace_key_);

    /* FIXME: We only need to finalize the PIO_DEFAULT trace log after all trace loggers are finalized.
        So trace the init/finalize of all loggers and finalize the trace logger for PIO_DEFAULT when
        finalizing the last iosys logger
    */
    SPIO_Util::Tracer::finalize_iosys_trace_logger(std::to_string(PIO_DEFAULT));

    SPIO_Util::Tracer::finalize_iosys_trace_mdata_logger(iosys_trace_key_);
    needs_finalize_ = false;
  }
}

void SPIO_Util::Tracer::Timed_func_call_tracer::log_func_call_exit(void )
{
  std::string ser_fcall(  std::to_string(func_id_) + FUNC_ID_SEP +
                          std::to_string(timer_.get_stop_time()) + FUNC_TIME_SEP +
                          FUNC_EXIT + func_name_ + FUNC_CALL_PREFIX);

  if(rvals_.size() > 0){
    std::vector<std::pair<std::string, std::string> >::const_iterator iter = rvals_.cbegin();
    ser_fcall += (*iter).first + ARG_EQUAL + (*iter).second;
    for(++iter ;iter != rvals_.cend(); ++iter){
      ser_fcall += ARG_SEP + (*iter).first + ARG_EQUAL + (*iter).second;
    }
  }
  ser_fcall += FUNC_CALL_SUFFIX;
  if(iosysid_ != INVALID_IOSYSID){
    SPIO_Util::Logger::MPI_logger<std::ofstream> &logger = SPIO_Util::Tracer::get_iosys_trace_logger(iosysid_, wrank_);
    logger.log(ser_fcall);
    logger.flush();
  }
}

std::string SPIO_Util::Tracer::get_trace_log_fname(int iosysid, int mpi_wrank)
{
  const std::string LOG_FILE_PREFIX = "spio_trace_log_";
  const std::string LOG_FILE_SUFFIX = ".log";
  std::string iosys_str = std::string("_iosys_") + ((iosysid != PIO_DEFAULT) ? std::to_string(iosysid) : "PIO_DEFAULT") + std::string("_");

  long long int pid = static_cast<long long int>(getpid());
  std::string log_fname = LOG_FILE_PREFIX + iosys_str + std::to_string(pid) + std::string("_") + std::to_string(mpi_wrank) + LOG_FILE_SUFFIX;

  return log_fname;
}

static inline std::string get_trace_log_header(int iosysid, int mpi_rank)
{
  static const std::string spio_version = std::string("SCORPIO VERSION : ") +
                                          std::to_string(PIO_VERSION_MAJOR) + "." +
                                          std::to_string(PIO_VERSION_MINOR) + "." +
                                          std::to_string(PIO_VERSION_PATCH) + "\n";
  static const std::string banner =
                                  "=================================================================\n";
  static const std::string spio_trace_log_info = "\tSCORPIO TRACE LOG\n" + spio_version;

  std::string iosys_info = std::string("I/O System ID : ") + std::to_string(iosysid) + "\n";

  std::string rank_info = std::string("MPI World rank : ") + std::to_string(mpi_rank) + "\n";

  std::string mdata_info = std::string("Trace Mdata file : ") + SPIO_Util::Tracer::get_trace_mdata_fname(iosysid, mpi_rank) + "\n";

  std::string hdr = banner + spio_trace_log_info + iosys_info + rank_info + mdata_info + banner;

  return hdr;
}

static inline std::string get_trace_log_footer(void )
{
  static const std::string banner =
                                  "=================================================================\n";

  return banner;
}


SPIO_Util::Logger::MPI_logger<std::ofstream> &SPIO_Util::Tracer::get_iosys_trace_logger(int iosysid, int mpi_wrank)
{
  /* FIXME: We might need to trace both I/O and compute procs for async I/O */
  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(std::to_string(iosysid));
  if(iter == SPIO_Util::Tracer::GVars::trace_loggers_.end()){
    const int MPI_ROOT_PROC = 0;
    int ret = MPI_SUCCESS, rank = -1;

    MPI_Comm comm = SPIO_Util::Tracer::Timed_func_call_tracer::PIO_DEFAULT_COMM;

    if(iosysid != PIO_DEFAULT){
      iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
      /* FIXME: Throw an exception instead */
      assert(ios);
      comm = ios->union_comm;
    }

    ret = MPI_Comm_rank(comm, &rank);
    assert(ret == MPI_SUCCESS);

    // FIXME: use unique_ptr
    std::ofstream *fstr = new std::ofstream();
    const std::string DEV_NULL = "/dev/null";
    std::string log_fname = get_trace_log_fname(iosysid, mpi_wrank);

    /* FIXME: We always log from the root proc, 0, and in the case of PIO_DEFAULT its the root process. This can cause issues when we use
     * PIO_DEFAULT as the I/O system, to handle user error cases etc where we cannot determine the I/O system, to log calls from a component
     * that does not have rank 0 of the MPI_COMM_WORLD/PIO_DEFAULT_COMM */
    fstr->open((rank == MPI_ROOT_PROC) ? log_fname.c_str() : DEV_NULL.c_str(), std::ofstream::out | std::ofstream::trunc);

    // FIXME: use insert() and get the iterator rather than insert and then find
    SPIO_Util::Logger::MPI_logger<std::ofstream> lstr(comm, fstr);
    lstr.log(get_trace_log_header(iosysid, mpi_wrank));
    SPIO_Util::Tracer::GVars::trace_loggers_[std::to_string(iosysid)] = lstr;
    iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(std::to_string(iosysid));
    //std::pair<std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator, bool> res = SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //assert(res.second);
    //iter = res.first;
  }
  return iter->second;
}

SPIO_Util::Logger::MPI_logger<std::ofstream> &SPIO_Util::Tracer::get_file_trace_logger(int fh, int mpi_rank)
{
  file_desc_t *file = NULL;
  int ret = PIO_NOERR;

  ret = pio_get_file(fh, &file);  
  /* FIXME: Throw an exception instead */
  assert(ret == PIO_NOERR);
  assert(file->iosystem);

  /* FIXME: We might need to trace both I/O and compute procs for async I/O */
  return get_iosys_trace_logger(file->iosystem->iosysid, mpi_rank);
}

void SPIO_Util::Tracer::finalize_iosys_trace_logger(std::string iosys_key)
{
  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(iosys_key);
  if(iter != SPIO_Util::Tracer::GVars::trace_loggers_.end()){
    (*iter).second.log(get_trace_log_footer());
    std::ofstream *fstr = (*iter).second.get_log_stream();
    assert(fstr);
    fstr->close();
    delete fstr;

    SPIO_Util::Tracer::GVars::trace_loggers_.erase(iter);
  }
}

/*
void SPIO_Util::Tracer::finalize_file_trace_logger(int fh)
{
  file_desc_t *file = NULL;
  int ret = PIO_NOERR;

  ret = pio_get_file(fh, &file);  
  // FIXME: Throw an exception instead
  assert(ret == PIO_NOERR);
  assert(file && file->iosystem);

  finalize_iosys_trace_logger(file->iosystem->iosysid);
}
*/

