#ifndef __SPIO_LOGGER_HPP__
#define __SPIO_LOGGER_HPP__

#include <string>

#include "mpi.h"

namespace SPIO_Util{
  namespace Logger{
    /* FIXME: Add a generic logger class */
    /* FIXME : Allow logging from multiple classes */
    /* An MPI logger */
    template<typename TStream>
    class MPI_logger{
      public:
        MPI_logger() : mpi_comm_(MPI_COMM_NULL), ostr_(NULL), is_io_proc_(false) {}

        MPI_logger(MPI_Comm comm, TStream *ostr);

        MPI_logger(MPI_logger &other) = default;

        MPI_logger &operator=(const MPI_logger &other) = default;

        /* Log message */
        void log(const std::string &log_msg);

        /* Explicitly flush the contents of the logger */
        void flush(void );

        /* Get associated stream */
        TStream *get_log_stream(void ) const;

        ~MPI_logger(){}
      private:
        MPI_Comm mpi_comm_;
        TStream *ostr_;
        bool is_io_proc_;

        static const int MPI_RANK_ROOT = 0;
        static const char LOG_SEP = '\n';
    };
  } // namespace Logger
} // namespace SPIO_Util

template<typename TStream>
SPIO_Util::Logger::MPI_logger<TStream>::MPI_logger(MPI_Comm comm, TStream *ostr):mpi_comm_(comm), ostr_(ostr), is_io_proc_(false)
{
  int rank;
  int ret;

  ret = MPI_Comm_rank(mpi_comm_, &rank);
  assert(ret == MPI_SUCCESS);
  if(rank == MPI_RANK_ROOT){
    is_io_proc_ = true;
  }
}

template<typename TStream>
void SPIO_Util::Logger::MPI_logger<TStream>::log(const std::string &log_msg)
{
  if(is_io_proc_){
    (*ostr_) << log_msg.c_str() << LOG_SEP;
  }
}

template<typename TStream>
void SPIO_Util::Logger::MPI_logger<TStream>::flush()
{
  if(is_io_proc_){
    ostr_->flush();
  }
}

template<typename TStream>
TStream *SPIO_Util::Logger::MPI_logger<TStream>::get_log_stream(void ) const
{
  return ostr_;
}
#endif /* __SPIO_LOGGER_HPP__ */
