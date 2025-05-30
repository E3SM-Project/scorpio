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
#include "spio_ltimer.hpp"

namespace SPIO_Util{
  namespace Tracer{
    /* FIXME: Add a generic/non-timed tracer class */
    /* A Timed single function call tracer */
    class Timed_func_call_tracer{
      public:
        Timed_func_call_tracer(const std::string &func_name);
        /* Set the id of PIO I/O system associated with this tracer */
        Timed_func_call_tracer &set_iosys_id(int iosysid);
        /* Set the id of PIO file associated with this tracer */
        Timed_func_call_tracer &set_file_id(int fh);
        /* Set the id of PIO dim associated with this tracer */
        Timed_func_call_tracer &set_dim_id(int fh, int dimid);
        /* Set the id of PIO var associated with this tracer */
        Timed_func_call_tracer &set_var_id(int fh, int varid);
        /* Set the decomp info */
        Timed_func_call_tracer &set_decomp_info(int decomp_id, const PIO_Offset *map, int sz);

        /* Set the args of the function that need to be traced */
        template<typename T>
        Timed_func_call_tracer &add_arg(const std::string &arg_name, const T &arg);
        template<typename T>
        Timed_func_call_tracer &add_arg(const std::string &arg_name, T *arg);
        template<typename T>
        Timed_func_call_tracer &add_arg(const std::string &arg_name, const T *arg, std::size_t arg_sz);

        /* Set the MPI args of the function that need to be traced - MPI opaque consts need special treatment */
        template<typename T>
        Timed_func_call_tracer &add_mpi_arg(const std::string &arg_name, const T &arg);
        template<typename T>
        Timed_func_call_tracer &add_mpi_arg(const std::string &arg_name, const T *arg, std::size_t arg_sz);

        /* Add the return values that need to be traced */
        template<typename T>
        Timed_func_call_tracer &add_rval(const std::string &name, const T &rval);
        template<typename T>
        Timed_func_call_tracer &add_rval(const std::string &name, T *rval);
        template<typename T>
        Timed_func_call_tracer &add_rval(const std::string &name, const T *rval, std::size_t rval_sz);

        /* Flush the contents of the tracer */
        void flush(void );

        /* Finalize the trace */
        void finalize(void );

        ~Timed_func_call_tracer();

        template<typename T>
        static std::string arr_to_string(const T *arr, std::size_t arr_sz);

        template<typename T>
        static std::string mpi_arr_to_string(const T *arr, std::size_t arr_sz);
      private:
        /* The name of the function being traced */
        int func_id_;
        const std::string func_name_;
        MPI_Comm mpi_comm_;
        int wrank_;
        int iosysid_;
        int fh_;
        bool is_io_proc_;
        bool needs_finalize_;
        std::string iosys_trace_key_;

        /* Global function id : */
        static int gfunc_id_;

        PIO_Util::SPIO_Ltimer_Utils::SPIO_ltimer timer_;

        /* Note: PIO_DEFAULT, the default I/O system id is -1 */
        static const int INVALID_IOSYSID = -2;
        static const int INVALID_FH = -1;
        static const int INVALID_RANK = -1;

        static const std::string NULL_PTR;

        static const char ARRAY_ARG_PREFIX = '[';
        static const char ARRAY_ARG_SUFFIX = ']';
        static const char ARG_SEP = ',';
        static const char ARG_EQUAL = '=';
        static const char FUNC_CALL_PREFIX = '(';
        static const char FUNC_CALL_SUFFIX = ')';
        static const char FUNC_ENTER = '\\';
        static const char FUNC_EXIT = '/';
        static const char FUNC_TIME_SEP = ':';
        static const char FUNC_ID_SEP = ':';
        
        /* FIXME: Use a named aggregate or tagged tuples */
        /* Array of <argument name, argument> pairs */
        std::vector<std::pair<std::string, std::string> > args_;
        std::vector<std::pair<std::string, std::string> > rvals_;

        void log_func_call_exit(void );
    };

    /* MPI Comm for default I/O system, PIO_DEFAULT */
    static const MPI_Comm PIO_DEFAULT_COMM = MPI_COMM_WORLD;

    std::string get_trace_log_fname(int iosysid, int mpi_wrank);

    SPIO_Util::Logger::MPI_logger<std::ofstream> &get_iosys_trace_logger(int iosysid, int mpi_wrank);
    SPIO_Util::Logger::MPI_logger<std::ofstream> &get_file_trace_logger(int fh, int mpi_rank);
    void finalize_iosys_trace_logger(std::string iosys_key);

    template<typename T>
    std::string mpi_type_to_string(const T& t);
    //void finalize_file_trace_logger(int fh);
  } // namespace Tracer
} // namespace SPIO_Util

template<typename T>
std::string SPIO_Util::Tracer::mpi_type_to_string(const T &t)
{
  /* FIXME: No easy/portable way to convert MPI_Datatype to string. Add mapping for all datatypes and use it later */
  static int i = 0;
  return std::string("MPI_Opaque_Datatype_") + std::to_string(i++);
}

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
  if(arg){
    ss << arg;
  }
  else{
    ss << NULL_PTR;
  }

  args_.push_back(std::pair<std::string, std::string>(arg_name, ss.str()));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_arg(const std::string &arg_name, const T *arg, std::size_t arg_sz)
{
  std::string arr_arg = arr_to_string(arg, arg_sz);
  args_.push_back(std::pair<std::string, std::string>(arg_name, arr_arg));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_mpi_arg(const std::string &arg_name, const T &arg)
{
  args_.push_back(std::pair<std::string, std::string>(arg_name, SPIO_Util::Tracer::mpi_type_to_string(arg)));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_mpi_arg(const std::string &arg_name, const T *arg, std::size_t arg_sz)
{
  std::string arr_arg = mpi_arr_to_string(arg, arg_sz);
  args_.push_back(std::pair<std::string, std::string>(arg_name, arr_arg));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_rval(const std::string &name, const T &rval)
{
  rvals_.push_back(std::pair<std::string, std::string>(name, std::to_string(rval)));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_rval(const std::string &name, T *rval)
{
  std::stringstream ss;
  if(rval){
    ss << *rval;
  }
  else{
    ss << NULL_PTR;
  }

  rvals_.push_back(std::pair<std::string, std::string>(name, ss.str()));
  return *this;
}

template<typename T>
SPIO_Util::Tracer::Timed_func_call_tracer &SPIO_Util::Tracer::Timed_func_call_tracer::add_rval(const std::string &name, const T *rval, std::size_t rval_sz)
{
  std::string arr_rval = arr_to_string(rval, rval_sz);
  rvals_.push_back(std::pair<std::string, std::string>(name, arr_rval));
  return *this;
}

template<typename T>
std::string SPIO_Util::Tracer::Timed_func_call_tracer::arr_to_string(const T *arr, std::size_t arr_sz)
{
  std::string arr_str(1, ARRAY_ARG_PREFIX);

  if(arr_sz > 0){
    arr_str += std::to_string(arr[0]);
    for(std::size_t i = 1; i < arr_sz; i++){
      arr_str += ARG_SEP;
      arr_str += std::to_string(arr[i]);
    }
  }

  arr_str += ARRAY_ARG_SUFFIX;
  return arr_str;
}

template<typename T>
std::string SPIO_Util::Tracer::Timed_func_call_tracer::mpi_arr_to_string(const T *arr, std::size_t arr_sz)
{
  std::string arr_str(1, ARRAY_ARG_PREFIX);

  if(arr_sz > 0){
    arr_str += SPIO_Util::Tracer::mpi_type_to_string(arr[0]);
    for(std::size_t i = 1; i < arr_sz; i++){
      arr_str += ARG_SEP;
      arr_str += SPIO_Util::Tracer::mpi_type_to_string(arr[i]);
    }
  }

  arr_str += ARRAY_ARG_SUFFIX;
  return arr_str;
}

#endif /* __SPIO_TRACER_HPP__ */
