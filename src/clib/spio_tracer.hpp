#ifndef __SPIO_TRACER_HPP__
#define __SPIO_TRACER_HPP__

#include <string>
#include <sstream>
#include <vector>
#include <utility>

#include "mpi.h"
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_logger.hpp"

namespace SPIO_Util{
  namespace Tracer{
    /* FIXME: Add a generic/non-timed tracer class */
    /* A Timed single function call tracer */
    class Timed_func_call_tracer{
      public:
        Timed_func_call_tracer(const std::string &func_name);
        /* Set the MPI comm associated with this tracer */
        //Timed_func_call_tracer &set_mpi_comm(MPI_Comm comm);
        /* Set the id of PIO I/O system associated with this tracer */
        Timed_func_call_tracer &set_iosys_id(int iosysid);
        /* Set the id of PIO file associated with this tracer */
        Timed_func_call_tracer &set_file_id(int fh);

        /* Set the args of the function that needs to be traced */
        template<typename T>
        Timed_func_call_tracer &add_arg(const std::string &arg_name, const T &arg);
        template<typename T>
        Timed_func_call_tracer &add_arg(const std::string &arg_name, T *arg);
        template<typename T>
        Timed_func_call_tracer &add_arg(const std::string &arg_name, const T *arg, std::size_t arg_sz);

        /* Flush the contents of the tracer */
        void flush(void );

        /* Finalize the trace */
        void finalize(void );

        ~Timed_func_call_tracer();
      private:
        /* The name of the function being traced */
        const std::string func_name_;
        MPI_Comm mpi_comm_;
        int iosysid_;
        int fh_;
        bool is_io_proc_;
        bool needs_finalize_;
        std::string iosys_trace_key_;

        static const int INVALID_IOSYSID = -1;
        static const int INVALID_FH = -1;

        static const char ARRAY_ARG_PREFIX = '[';
        static const char ARRAY_ARG_SUFFIX = ']';
        static const char ARG_SEP = ',';
        static const char ARG_EQUAL = '=';
        static const char FUNC_CALL_PREFIX = '(';
        static const char FUNC_CALL_SUFFIX = ')';
        static const char FUNC_ENTER = '\\';
        static const char FUNC_EXIT = '/';
        
        /* FIXME: Use a named aggregate or tagged tuples */
        /* Array of <argument name, argument> pairs */
        std::vector<std::pair<std::string, std::string> > args_;

        void log_func_call_exit(void );
    };
    
    //SPIO_Util::Logger::MPI_logger<std::ofstream> &get_mpi_trace_logger(MPI_Comm comm);
    SPIO_Util::Logger::MPI_logger<std::ofstream> &get_iosys_trace_logger(int iosysid);
    SPIO_Util::Logger::MPI_logger<std::ofstream> &get_file_trace_logger(int fh);
    //void finalize_mpi_trace_logger(MPI_Comm comm);
    void finalize_iosys_trace_logger(std::string iosys_key);
    //void finalize_file_trace_logger(int fh);
  } // namespace Tracer
} // namespace SPIO_Util

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_arg(const std::string &arg_name, const T &arg)
{
  args_.push_back(std::pair<std::string, std::string>(arg_name, std::to_string(arg)));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_arg(const std::string &arg_name, T *arg)
{
  std::stringstream ss;
  ss << arg;
  args_.push_back(std::pair<std::string, std::string>(arg_name, ss.str()));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_arg(const std::string &arg_name, const T *arg, std::size_t arg_sz)
{
  std::string arr_arg(1, ARRAY_ARG_PREFIX);

  if(arg_sz > 0){
    arr_arg += std::to_string(arg[0]);
    for(std::size_t i = 1; i < arg_sz; i++){
      arr_arg += ARG_SEP;
      arr_arg += std::to_string(arg[i]);  
    }
  }

  arr_arg += ARRAY_ARG_SUFFIX;
  args_.push_back(std::pair<std::string, std::string>(arg_name, arr_arg));
  return *this;
}

#endif /* __SPIO_TRACER_HPP__ */
