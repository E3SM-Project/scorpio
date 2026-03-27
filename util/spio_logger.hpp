#ifndef __SPIO_LOGGER_HPP__
#define __SPIO_LOGGER_HPP__

#include <string>
#include <memory>
#include <algorithm>
#include <cctype>

#include "mpi.h"

namespace SPIO_Util{
  namespace Logger{
    enum class Log_level : int{
      INVALID = 0,
      TRACE,
      DEBUG,
      INFO,
      WARN,
      ERROR,
      FATAL
    };

    inline std::string log_level_to_string(const Log_level &lvl){
      switch(lvl){
        case Logger::Log_level::INVALID : return "INVALID";
        case Logger::Log_level::TRACE : return "TRACE";
        case Logger::Log_level::DEBUG : return "DEBUG";
        case Logger::Log_level::INFO : return "INFO";
        case Logger::Log_level::WARN : return "WARN";
        case Logger::Log_level::ERROR : return "ERROR";
        case Logger::Log_level::FATAL : return "FATAL";
        default : return "INVALID";
      }
    }

    inline Log_level string_to_log_level(const std::string &lvl){
      std::string ulvl(lvl);

      std::transform(ulvl.begin(), ulvl.end(), ulvl.begin(),
        [](unsigned char c) { return std::toupper(c); });

      if(ulvl == "INVALID") { return Logger::Log_level::INVALID; }
      if(ulvl == "TRACE") { return Logger::Log_level::TRACE; }
      if(ulvl == "DEBUG") { return Logger::Log_level::DEBUG; }
      if(ulvl == "INFO") { return Logger::Log_level::INFO; }
      if(ulvl == "WARN") { return Logger::Log_level::WARN; }
      if(ulvl == "ERROR") { return Logger::Log_level::ERROR; }
      if(ulvl == "FATAL") { return Logger::Log_level::FATAL; }

      return Logger::Log_level::INVALID;
    }

    inline std::string get_available_log_levels(void ){
      std::vector<Log_level> log_levels = {Log_level::INVALID,
        Log_level::TRACE, Log_level::DEBUG, Log_level::INFO,
        Log_level::WARN, Log_level::ERROR, Log_level::FATAL};

      std::string str_lvls("[");
      std::for_each(log_levels.cbegin(), log_levels.cend(),
        [&str_lvls](const Log_level &lvl) { str_lvls += log_level_to_string(lvl) + ", "; });

      return str_lvls + "]";
    }


    /* FIXME: Add a generic logger class */
    /* FIXME : Allow logging from multiple classes */
    /* An MPI logger */
    template<typename TStream>
    class MPI_logger{
      public:
        MPI_logger() : mpi_comm_(MPI_COMM_NULL), ostr_(NULL), is_io_proc_(false), slog_done_(false), log_lvl_(Log_level::INFO) {}

        MPI_logger(MPI_Comm comm, std::shared_ptr<TStream> &ostr);

        MPI_logger(const MPI_logger &other) = default;

        MPI_logger &operator=(const MPI_logger &other) = default;

        /* Enable logging on this proc. By default logging is only enabled on rank 0 */
        MPI_logger &enable_logging(void ) { is_io_proc_ = true;  return *this; }

        /* Log message */
        void log(const std::string &log_msg);

        /* Log message, with log level */
        void log(Log_level lvl, const std::string &log_msg);

        /* Singleton log : Logging is only done once - e.g. writing headers etc */
        void slog(const std::string &slog_msg);

        /* Singleton log with log level : Logging is only done once - e.g. writing headers etc */
        void slog(Log_level lvl, const std::string &slog_msg);

        /* Explicitly flush the contents of the logger */
        void flush(void );

        /* Get associated stream */
        std::shared_ptr<TStream> get_log_stream(void ) const;

        ~MPI_logger(){}
      private:
        MPI_Comm mpi_comm_;
        std::shared_ptr<TStream> ostr_;
        bool is_io_proc_;
        bool slog_done_;
        Log_level log_lvl_;

        static const int MPI_DEFAULT_ROOT = 0;
        static const char LOG_SEP = '\n';
    };
  } // namespace Logger
} // namespace SPIO_Util

template<typename TStream>
SPIO_Util::Logger::MPI_logger<TStream>::MPI_logger(MPI_Comm comm, std::shared_ptr<TStream> &ostr):
  mpi_comm_(comm), ostr_(ostr), is_io_proc_(false), slog_done_(false), log_lvl_(Log_level::INFO)
{
  int rank;
  int ret;

  ret = MPI_Comm_rank(mpi_comm_, &rank);
  assert(ret == MPI_SUCCESS);
  if(rank == MPI_DEFAULT_ROOT){
    is_io_proc_ = true;
  }
}

template<typename TStream>
void SPIO_Util::Logger::MPI_logger<TStream>::log(const std::string &log_msg)
{
  log(Log_level::INFO, log_msg);
}

template<typename TStream>
void SPIO_Util::Logger::MPI_logger<TStream>::log(Log_level lvl, const std::string &log_msg)
{
  if(is_io_proc_ && (log_lvl_ <= lvl)){
    (*ostr_) << log_msg.c_str() << LOG_SEP;
  }
}

template<typename TStream>
void SPIO_Util::Logger::MPI_logger<TStream>::slog(const std::string &slog_msg)
{
  slog(Log_level::INFO, slog_msg);
}

template<typename TStream>
void SPIO_Util::Logger::MPI_logger<TStream>::slog(Log_level lvl, const std::string &slog_msg)
{
  if(!slog_done_){
    if(is_io_proc_ && (log_lvl_ <= lvl)){
      log(slog_msg);
    }
    slog_done_ = true;
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
std::shared_ptr<TStream> SPIO_Util::Logger::MPI_logger<TStream>::get_log_stream(void ) const
{
  return ostr_;
}
#endif /* __SPIO_LOGGER_HPP__ */
