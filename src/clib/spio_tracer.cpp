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

namespace SPIO_Util{
  namespace Tracer{
    /* FIXME: Move to a singleton */
    namespace GVars{
      static std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> > trace_loggers_;
    }
  } // namespace Tracer
} // namespace SPIO_Util

SPIO_Util::Tracer::Timed_func_call_tracer::Timed_func_call_tracer(const std::string &func_name) : func_name_(func_name), mpi_comm_(MPI_COMM_NULL), iosysid_(INVALID_IOSYSID), fh_(INVALID_FH), is_io_proc_(false), needs_finalize_(false)
{
}

/*
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_mpi_comm(MPI_Comm comm)
{
  mpi_comm_ = comm;
  return *this;
}
*/

SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_iosys_id(int iosysid)
{
  iosysid_ = iosysid;
  iosystem_desc_t *iosys = pio_get_iosystem_from_id(iosysid);
  /* FIXME: Throw an exception instead */
  assert(iosys);
  mpi_comm_ = iosys->union_comm;

  iosys_trace_key_ = std::to_string(iosysid);

  return *this;
}

SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::set_file_id(int fh)
{
  int ret = PIO_NOERR;
  file_desc_t *file = NULL;

  fh_ = fh;
  ret = pio_get_file(fh_, &file);
  /* FIXME: Throw an exception instead */
  assert((ret == PIO_NOERR) && file && file->iosystem);
  iosysid_ = file->iosystem->iosysid;
  mpi_comm_ = file->iosystem->union_comm;

  iosys_trace_key_ = std::to_string(iosysid_);

  return *this;
}

void SPIO_Util::Tracer::Timed_func_call_tracer::flush(void )
{
  /* Serialize the function call */
  std::string ser_fcall(FUNC_ENTER + func_name_ + FUNC_CALL_PREFIX);
  if(args_.size() > 0){
    std::vector<std::pair<std::string, std::string> >::const_iterator iter = args_.cbegin();
    ser_fcall += (*iter).first + ARG_EQUAL + (*iter).second;
    for(++iter ;iter != args_.cend(); ++iter){
      ser_fcall += ARG_SEP + (*iter).first + ARG_EQUAL + (*iter).second;
    }
  }
  ser_fcall += FUNC_CALL_SUFFIX;

  if(iosysid_ != INVALID_IOSYSID){
    SPIO_Util::Logger::MPI_logger<std::ofstream> &logger = SPIO_Util::Tracer::get_iosys_trace_logger(iosysid_);
    logger.log(ser_fcall);
    logger.flush();
  }
}

void SPIO_Util::Tracer::Timed_func_call_tracer::finalize(void )
{
  needs_finalize_ = true;
}

SPIO_Util::Tracer::Timed_func_call_tracer::~Timed_func_call_tracer()
{
  log_func_call_exit();
  if(needs_finalize_){
    SPIO_Util::Tracer::finalize_iosys_trace_logger(iosys_trace_key_);
    needs_finalize_ = false;
  }
}

void SPIO_Util::Tracer::Timed_func_call_tracer::log_func_call_exit(void )
{
  std::string ser_fcall(FUNC_EXIT + func_name_ + FUNC_CALL_PREFIX + FUNC_CALL_SUFFIX);
  if(iosysid_ != INVALID_IOSYSID){
    SPIO_Util::Logger::MPI_logger<std::ofstream> &logger = SPIO_Util::Tracer::get_iosys_trace_logger(iosysid_);
    logger.log(ser_fcall);
    logger.flush();
  }
}

/*
SPIO_Util::Logger::MPI_logger<std::ofstream> &SPIO_Util::Tracer::get_mpi_trace_logger(MPI_Comm comm)
{
  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(std::to_string(comm));
  if(iter == SPIO_Util::Tracer::GVars::trace_loggers_.end()){
    // FIXME: use unique_ptr
    std::ofstream *fstr = new std::ofstream();
    const std::string LOG_FILE_PREFIX = "spio_trace_log_";
    const std::string LOG_FILE_SUFFIX = ".log";
    static int comm_idx = 0;
    std::string comm_idx_str = std::string("_comm_") + std::to_string(comm_idx) + std::string("_");
    comm_idx++;

    long long int pid = static_cast<long long int>(getpid());
    std::string log_fname = LOG_FILE_PREFIX + comm_idx_str + std::to_string(pid) + LOG_FILE_SUFFIX;
    fstr->open(log_fname.c_str(), std::ofstream::out | std::ofstream::trunc);

    // FIXME: use insert() and get the iterator rather than insert and then find
    SPIO_Util::Logger::MPI_logger<std::ofstream> lstr(comm, fstr);
    SPIO_Util::Tracer::GVars::trace_loggers_[std::to_string(comm)] = lstr;
    iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(std::to_string(comm));
    //std::pair<std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator, bool> res = SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //assert(res.second);
    //iter = res.first;
  }
  return iter->second;
}
*/

SPIO_Util::Logger::MPI_logger<std::ofstream> &SPIO_Util::Tracer::get_iosys_trace_logger(int iosysid)
{
  /* FIXME: We might need to trace both I/O and compute procs for async I/O */
  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(std::to_string(iosysid));
  if(iter == SPIO_Util::Tracer::GVars::trace_loggers_.end()){
    const int MPI_ROOT_PROC = 0;
    int ret = MPI_SUCCESS, rank = -1;

    iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);  
    /* FIXME: Throw an exception instead */
    assert(ios);

    ret = MPI_Comm_rank(ios->union_comm, &rank);
    assert(ret == MPI_SUCCESS);

    // FIXME: use unique_ptr
    std::ofstream *fstr = new std::ofstream();
    const std::string DEV_NULL = "/dev/null";
    const std::string LOG_FILE_PREFIX = "spio_trace_log_";
    const std::string LOG_FILE_SUFFIX = ".log";
    std::string iosys_str = std::string("_iosys_") + std::to_string(iosysid) + std::string("_");

    long long int pid = static_cast<long long int>(getpid());
    std::string log_fname = LOG_FILE_PREFIX + iosys_str + std::to_string(pid) + LOG_FILE_SUFFIX;
    fstr->open((rank == MPI_ROOT_PROC) ? log_fname.c_str() : DEV_NULL.c_str(), std::ofstream::out | std::ofstream::trunc);

    // FIXME: use insert() and get the iterator rather than insert and then find
    SPIO_Util::Logger::MPI_logger<std::ofstream> lstr(ios->union_comm, fstr);
    SPIO_Util::Tracer::GVars::trace_loggers_[std::to_string(iosysid)] = lstr;
    iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(std::to_string(iosysid));
    //std::pair<std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator, bool> res = SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //SPIO_Util::Tracer::GVars::trace_loggers_.insert({std::to_string(comm), lstr});
    //assert(res.second);
    //iter = res.first;
  }
  return iter->second;
}

SPIO_Util::Logger::MPI_logger<std::ofstream> &SPIO_Util::Tracer::get_file_trace_logger(int fh)
{
  file_desc_t *file = NULL;
  int ret = PIO_NOERR;

  ret = pio_get_file(fh, &file);  
  /* FIXME: Throw an exception instead */
  assert(ret == PIO_NOERR);
  assert(file->iosystem);

  /* FIXME: We might need to trace both I/O and compute procs for async I/O */
  return get_iosys_trace_logger(file->iosystem->iosysid);
}

/*
void SPIO_Util::Tracer::finalize_mpi_trace_logger(MPI_Comm comm)
{
  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(std::to_string(comm));
  if(iter != SPIO_Util::Tracer::GVars::trace_loggers_.end()){
    std::ofstream *fstr = (*iter).second.get_log_stream();
    assert(fstr);
    fstr->close();
    delete fstr;

    SPIO_Util::Tracer::GVars::trace_loggers_.erase(iter);
  }
}
*/

void SPIO_Util::Tracer::finalize_iosys_trace_logger(std::string iosys_key)
{
  //iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);  
  /* FIXME: Throw an exception instead */
  //assert(ios);

  std::map<std::string, SPIO_Util::Logger::MPI_logger<std::ofstream> >::iterator iter = SPIO_Util::Tracer::GVars::trace_loggers_.find(iosys_key);
  if(iter != SPIO_Util::Tracer::GVars::trace_loggers_.end()){
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

