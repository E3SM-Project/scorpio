#ifndef __E3SM_UTILS_FGI_HPP__
#define __E3SM_UTILS_FGI_HPP__

#include <mpi.h>
#ifdef SPIO_ENABLE_GPTL_TIMING
#include <gptl.h>
#endif
#include "pio.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <memory>
#include <cctype>
#include <algorithm>
#include <numeric>
#include <iterator>
#include <cassert>
#include <unordered_map>

namespace E3SM_FGI{
  enum class Case_Type{
    E3SM_F_CASE,
    E3SM_G_CASE,
    E3SM_I_CASE
  };
}

/* FIXME: Move logging related classes to a util */
namespace Util{
  namespace String{
    /* Util to convert a vactor of C++ types to a single string */
    template<typename T>
    inline std::string vec_to_string(const std::vector<T> &vec)
    {
      std::string str("[");
      str += std::accumulate(vec.cbegin(), vec.cend(), std::string(""),
              [](std::string str, const T &t){
                return std::to_string(t) + ", " + str;
              });
      str += "]";
      return str;
    }

    /* Util to convert a vactor of C++ strings to a single string */
    template<>
    inline std::string vec_to_string<std::string>(const std::vector<std::string> &vec)
    {
      std::string str("[");
      str += std::accumulate(vec.cbegin(), vec.cend(), std::string(""),
              [](std::string res, const std::string &str){
                return "\"" + str + "\", " + res;
              });
      str += "]";
      return str;
    }

    /* toupper() for a string */
    static inline std::string toupper(const std::string &str)
    {
      std::string res = str;
      std::transform(str.cbegin(), str.cend(), res.begin(),
              [](unsigned char c) { return std::toupper(c); });
      return res;
    }
  }

  namespace Logging{
    /* Different log levels supported */
    enum class LogLevel{ STATUS, VERBOSE, WARNING, ERROR };
    static inline std::string llevel2str(LogLevel lvl){
      std::string str;
      switch(lvl){
        case LogLevel::STATUS : str = "STATUS"; break;
        case LogLevel::VERBOSE : str = "VERBOSE"; break;
        case LogLevel::WARNING : str = "WARNING"; break;
        case LogLevel::ERROR : str = "ERROR"; break;
      }
      return str;
    }

    /* A message logger : To log/print to stdout, use "STDOUT" for the logger name */
    class Logger{
      public:
        static std::shared_ptr<Logger> get_logger(MPI_Comm comm, const std::string &log_name){
          static std::shared_ptr<Logger> logger_instance;
          return logger_instance ? logger_instance : std::shared_ptr<Logger>(is_std_logname(log_name) ? new Logger(comm) : new Logger(comm, log_name));
        }

        /* Log message (at level lvl) */
        void log(LogLevel lvl, const std::string &str){
          if(!need_to_log(lvl)) return;

          if((log_rank_ == LOG_ALL_RANKS) || (rank_ == log_rank_)){
            if(log_fname_.empty()){
              std::cout << llevel2str(lvl).c_str() << ":" << str.c_str() << std::flush; 
            }
            else{
              if(!log_fstr_.is_open()){
                log_fstr_.open(log_fname_);
              }
              log_fstr_ << llevel2str(lvl).c_str() << ":" << str.c_str();
            }
          }
        }

        /* Set the MPI process rank - that logs the message */
        Logger &set_log_rank(int rank){
          log_rank_ = rank;
          if(!log_fname_.empty()){
            log_fname_ = get_log_fname();
          }
          return *this;
        }

        /* Set log level */
        Logger &set_log_level(LogLevel lvl) { log_lvl_ = lvl; return *this; }

        ~Logger(){
          if(log_fstr_.is_open()){
            log_fstr_.close();
          }
        }
      private:
        Logger(MPI_Comm comm) : rank_(0), log_rank_(LOG_ALL_RANKS){
          int ret = MPI_Comm_rank(comm, &rank_); assert(ret == MPI_SUCCESS);
        }

        Logger(MPI_Comm comm, const std::string &log_fname) : rank_(0), log_rank_(LOG_ALL_RANKS), log_fname_prefix_(log_fname){
          int ret = MPI_Comm_rank(comm, &rank_); assert(ret == MPI_SUCCESS);
          log_fname_ = get_log_fname();
        }

        /* Check if the user wants to log to stdout */
        static bool is_std_logname(const std::string &log_name) { return (Util::String::toupper(log_name) == "STDOUT"); }
        std::string get_log_fname(void ) const { return log_fname_prefix_ + "_" + std::to_string(log_rank_) + ".log"; }

        bool need_to_log(LogLevel msg_lvl) const {
          switch(log_lvl_){
            case LogLevel::VERBOSE : return true;
            case LogLevel::STATUS : return (msg_lvl != LogLevel::VERBOSE) ? true : false;
            case LogLevel::WARNING : return ((msg_lvl == LogLevel::ERROR) || (msg_lvl == LogLevel::WARNING)) ? true : false;
            case LogLevel::ERROR : return (msg_lvl == LogLevel::ERROR) ? true : false;
          }
          return false;
        }

        const int LOG_ALL_RANKS = -1;
        int rank_;
        int log_rank_;
        std::string log_fname_prefix_;
        std::string log_fname_;
        std::ofstream log_fstr_;
        LogLevel log_lvl_;
    };
  } // namespace Logging
} // namespace Util

namespace Util{
  /* Global variable declarations (see e3sm_fgi.cpp) and
   * functions to modify them
   */
  namespace GVars{
    /* Available I/O types - for parsing user args*/
    extern std::unordered_map<std::string, int> iotypes;
    /* Available I/O rearrangers - for parsing user args */
    extern std::unordered_map<std::string, int> rearrs;
    /* E3SM pseudo cases to test - for parsing user args */
    extern std::unordered_map<std::string, E3SM_FGI::Case_Type> cases;
    /* Available log levels - for parsing user args */
    extern std::unordered_map<std::string, Util::Logging::LogLevel> llevels;

    /* Convert user option to string using the provided "option map", opt_map */
    template<typename T>
    std::string opt_map_to_str(const std::unordered_map<std::string, T> &opt_map)
    {
      std::string str("[");
      str += std::accumulate(opt_map.cbegin(), opt_map.cend(), std::string(""),
              [](std::string str, const std::pair<std::string, T> &t){
                return t.first + ", " + str;
              });
      str += "]";
      return str;
    }

    /* Convert option string to option type using provided "option map" */
    template<typename T>
    std::string opt_type_to_str(T opt_type,
                  const std::unordered_map<std::string, T> &opt_map)
    {
      typename std::unordered_map<std::string, T>::const_iterator iter =
        std::find_if(opt_map.cbegin(), opt_map.cend(),
          [opt_type](const std::pair<std::string, T> &a) { return a.second == opt_type; });
      if(iter != opt_map.cend()){
        return iter->first;
      }
      else{
        throw std::runtime_error("Unable to find key in option map");
      }
    }

    /* Copy the available options, the option names, to the destination */
    template<typename T, typename InsertIter>
    void copy_opt_map(const std::unordered_map<std::string, T> &opt_map,
      InsertIter dest)
    {
      std::transform(opt_map.cbegin(), opt_map.cend(), dest,
        [](const std::pair<std::string, T> &a) { return a.second; });
    }

    /* Helper utils for parsing user options, converting between types and names */
    static inline std::string iotypes2str(void ){ return opt_map_to_str(iotypes); }
    static inline std::string rearrs2str(void ){ return opt_map_to_str(rearrs); }
    static inline std::string cases2str(void ){ return opt_map_to_str(cases); }
    static inline std::string llevels2str(void ){ return opt_map_to_str(llevels); }

    static inline std::string iotype2str(int iotype){ return opt_type_to_str(iotype, iotypes); }
    static inline int str2iotype(const std::string &iotype_str){ return iotypes.at(iotype_str); }
    static inline std::string rearr2str(int rearr){ return opt_type_to_str(rearr, rearrs); }
    static inline int str2rearr(const std::string &rearr_str){ return rearrs.at(rearr_str); }
    static inline std::string case2str(E3SM_FGI::Case_Type c){ return opt_type_to_str(c, cases); }
    static inline E3SM_FGI::Case_Type str2case(const std::string &case_str){ return cases.at(case_str); }
    static inline std::string llevel2str(Util::Logging::LogLevel llevel){ return opt_type_to_str(llevel, llevels); }
    static inline Util::Logging::LogLevel str2llevel(const std::string &llevel_str){ return llevels.at(llevel_str); }

    /* Decl for the global logger */
    extern std::shared_ptr<Util::Logging::Logger> logger;
  } // namespace GVars
} //namespace Util

namespace Util{
  /* Run function f for each pair from the input iterators */
  template<typename InputIterator1, typename InputIterator2, typename Func>
  void zip_for_each(InputIterator1 iter1_begin, InputIterator1 iter1_end, InputIterator2 iter2_begin, InputIterator2 iter2_end, Func f)
  {
    for(InputIterator1 iter1 = iter1_begin; iter1 != iter1_end; ++iter1){
      for(InputIterator2 iter2 = iter2_begin; iter2 != iter2_end; ++iter2){
        f(*iter1, *iter2);
      }
    }
  }

  /* Util to check return value from the lib */
  static inline int check_spio_err(int ret, const std::string &err_msg, const char *fname, int line_num)
  {
    if(ret != PIO_NOERR){
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR,
        err_msg + ", file = " + std::string(fname) + ", line no : " + std::to_string(line_num));
      exit(-1);
    }

    return ret;
  }
} // namespace Util

namespace E3SM_FGI{
  /* Decls of the pseudo E3SM case functions */
  int test_e3sm_fcase(MPI_Comm comm, const std::vector<int> &iotypes,
        const std::vector<int> &rearrs, int nioprocs);
  int test_e3sm_gcase(MPI_Comm comm, const std::vector<int> &iotypes,
        const std::vector<int> &rearrs, int nioprocs);
  int test_e3sm_icase(MPI_Comm comm, const std::vector<int> &iotypes,
        const std::vector<int> &rearrs, int nioprocs);
}
#endif // __E3SM_UTILS_FGI_HPP__
