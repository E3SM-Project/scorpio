#ifndef __SPIO_TEST_FRAMEWORK__
#define __SPIO_TEST_FRAMEWORK__

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <cassert>
#include <type_traits>
#include <utility>
#include <functional>
#include <algorithm>

#include "pio_config.h"
#include "pio.h"
#include "argparser.h"
#include "spio_logger.hpp"

namespace SPIO_TF{

class Test_framework{
  public:
    Test_framework(const std::string &name, MPI_Comm comm, std::vector<std::pair<std::string, std::function<int (Test_framework&)> > > &run_funcs) :
      name_(name), log_fname_(tf_name_to_log_fname(name)),
      comm_(comm), comm_rank_(0), comm_root_(DEFAULT_ROOT), comm_sz_(0),
      run_funcs_(run_funcs) {}

    void init(int argc, char *argv[]){
      int ret = MPI_SUCCESS;

      ret = MPI_Comm_rank(comm_, &comm_rank_); assert(ret == MPI_SUCCESS);
      ret = MPI_Comm_size(comm_, &comm_sz_); assert(ret == MPI_SUCCESS);

      /* Get command line user options */
      spio_tool_utils::ArgParser ap(comm_);
      init_user_options(ap);

      std::string log_fname;
      SPIO_Util::Logger::Log_level log_lvl = SPIO_Util::Logger::Log_level::INVALID;
      bool verbose = false;

      ret = get_user_options(ap, argc, argv, comm_rank_, comm_root_,
              log_fname, log_lvl, verbose);
      if(ret == PIO_NOERR){
        if(verbose) { log_lvl = SPIO_Util::Logger::Log_level::TRACE; }
      }
      else{
        std::cerr << "ERROR: Getting user options failed\n";
      }

      /* Set up logger  - only log from comm_root_ */
      if(!log_fname.empty()){
        logger_fstr_ = std::make_shared<std::ofstream>();
        const std::string DEV_NULL = "/dev/null";
        logger_fstr_->open((comm_rank_ == comm_root_) ? log_fname_.c_str() : DEV_NULL.c_str(),
          std::ofstream::out | std::ofstream::trunc);
        flogger_ = SPIO_Util::Logger::MPI_logger<std::ofstream>(comm_, logger_fstr_);
      }
      if(comm_rank_ == comm_root_) { flogger_.enable_logging(); }


      get_logger().log(SPIO_Util::Logger::Log_level::DEBUG, "Testing framework initialized");
#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
      ret = GPTLinitialize();
      if(ret != 0){
        get_logger().log(SPIO_Util::Logger::Log_level::ERROR, std::string("GPTLinitialize() FAILED, ret = ") + std::to_string(ret) + ")");
        return ret;
      }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */
    }

    void register_run_funcs(const std::vector<std::pair<std::string, std::function<int (Test_framework &)> > > &run_funcs){
      run_funcs_.insert(run_funcs.end(), run_funcs.cbegin(), run_funcs.cend());
    }

    int run(void ){
      int nerrs = 0;

      std::for_each(run_funcs_.begin(), run_funcs_.end(),
        [this, &nerrs](std::pair<std::string, std::function<int (Test_framework &)> > &f){
          int ret = PIO_NOERR;

          ret = run(f.first, f.second);
          if(ret != 0) { nerrs++; }

        });

      if(nerrs == 0){
        get_logger().log(SPIO_Util::Logger::Log_level::INFO, "All Tests PASSED");
      }
      else{
        const std::string fail_msg = std::string("[") + std::to_string(nerrs) + "] Tests FAILED";
        get_logger().log(SPIO_Util::Logger::Log_level::INFO, fail_msg);
      }
      get_logger().flush();

      return nerrs;
    }

    template<typename... Args>
    int run(const std::string &name, std::function<int(Test_framework&, Args&&...)> &f, Args&&... args){
      int ret = PIO_NOERR, mpierr = MPI_SUCCESS;
      try{
        ret = f(*this, std::forward<Args>(args)...);
      } catch(...){
        ret = PIO_EINTERNAL;
      }

      /* Reduce the number of fails/errors across all processes */
      int num_lfail = (ret == PIO_NOERR) ? 0 : 1;
      mpierr = MPI_Reduce((comm_rank_ != comm_root_) ? &num_lfail : MPI_IN_PLACE, &num_lfail, 1, MPI_INT, MPI_SUM, comm_root_, comm_);
      assert(mpierr == MPI_SUCCESS);

      if(num_lfail == 0){
        get_logger().log(SPIO_Util::Logger::Log_level::INFO, name + " PASSED\n");
      }
      else{
        const std::string all_proc_fail_msg("failed on all processes");
        const std::string some_proc_including_root_fail_msg = std::string("failed on ") + std::to_string(num_lfail) + " processes, including root";
        const std::string some_proc_excluding_root_fail_msg = std::string("failed on ") + std::to_string(num_lfail) + " non-root processes, passed on root";


        get_logger().log(SPIO_Util::Logger::Log_level::INFO, name + " FAILED (ret = " + std::to_string(ret) + ((num_lfail == comm_sz_) ? all_proc_fail_msg : ((ret != PIO_NOERR) ? some_proc_including_root_fail_msg : some_proc_excluding_root_fail_msg) + ")"));

      }
      get_logger().flush();

      return (num_lfail == 0) ? 0 : 1;
    }

    SPIO_Util::Logger::MPI_logger<std::ofstream> get_logger(void ) const {
      return flogger_;
    }

    MPI_Comm get_comm(void ) const { return comm_; }
    int get_comm_rank(void ) const { return comm_rank_; }
    int get_comm_size(void ) const { return comm_sz_; }

    void finalize(void ){
      get_logger().log(SPIO_Util::Logger::Log_level::DEBUG, "Testing framework finalizing...");
#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
      ret = GPTLfinalize();
      if(ret != 0){
        get_logger().log(SPIO_Util::Logger::Log_level::ERROR, std::string("GPTLfinalize() FAILED, ret = ") + std::to_string(ret) + ")");
        return ret;
      }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

      if(logger_fstr_) { logger_fstr_->close(); }
    }

    ~Test_framework() = default;

  private:

    static inline std::string tf_name_to_log_fname(const std::string &tf_name){
      return tf_name + "_log.txt";
    }

    inline bool is_comm_root(void ) const { return comm_rank_ == comm_root_; }

    static void init_user_options(spio_tool_utils::ArgParser &ap){
      ap.add_opt("log-file", "Output log file (default output is stdout)")
        .add_opt("log-level", "Output log level, Valid values = " + SPIO_Util::Logger::get_available_log_levels())
        .add_opt("verbose", "Turn on verbose messages (log level = TRACE)");
    }

    static int get_user_options(spio_tool_utils::ArgParser &ap,
                int argc, char *argv[],
                int comm_rank, int comm_root,
                std::string &log_fname, SPIO_Util::Logger::Log_level &log_lvl,
                bool &verbose){
#ifdef SPIO_NO_CXX_REGEX
      ap.no_regex_parse(argc, argv);
#else
      ap.parse(argc, argv);
#endif
      log_fname = "";
      if(ap.has_arg("log-file")){
        log_fname = ap.get_arg<std::string>("log-file");
      }

      log_lvl = SPIO_Util::Logger::Log_level::INFO;
      if(ap.has_arg("log-level")){
        log_lvl = SPIO_Util::Logger::string_to_log_level(ap.get_arg<std::string>("log-level"));
      }

      verbose = false;
      if(ap.has_arg("verbose")){
        verbose = true;    
      }

      return PIO_NOERR;
    }

    const int DEFAULT_ROOT = 0;

    const std::string name_;
    const std::string log_fname_;
    MPI_Comm comm_;
    int comm_rank_;
    int comm_root_;
    int comm_sz_;
    std::vector<std::pair<std::string, std::function<int (Test_framework &)> > > run_funcs_;
    std::shared_ptr<std::ofstream> logger_fstr_;
    SPIO_Util::Logger::MPI_logger<std::ofstream> flogger_;
};

} // namespace SPIO_TF

#endif // __SPIO_TEST_FRAMEWORK__
