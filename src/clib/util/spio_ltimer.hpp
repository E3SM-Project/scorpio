#ifndef __SPIO_LTIMER_HPP__
#define __SPIO_LTIMER_HPP__

#include <iostream>
#include <vector>
#include <string>
#include "spio_dbg_utils.hpp"

namespace PIO_Util{
  namespace SPIO_Ltimer_Utils{
    /* A simple timer class */
    class SPIO_ltimer{
      public:
        SPIO_ltimer() : wtime_(0.0), lvl_(0) { start_ = MPI_Wtime(); }
        /* Start the timer */
        void start(void );
        /* Stop the timer */
        void stop(void );
        /* Get latest start time */
        inline double get_start_time(void ) const { return start_; }
        /* Get latest stop time */
        inline double get_stop_time(void ) const { return stop_; }
        /* Get elapsed wallclock time */
        double get_wtime(void ) const;

        void sanity_check(const std::string &msg) const {
          if(lvl_ != 0){
            std::cerr << "WARNING: Sanity check failed, trying to get timer before its stopped, level = " << lvl_ << "(" << msg.c_str() << ")\n";
            if(st_.size() > 0){
              std::cerr << "Printing first (of " << st_.size() << ") stack traces:\n";
              std::cerr << SPIO_Util::Dbg_Util::stack_trace_to_string(st_[0]).c_str() << "\n";
            }
          }
        }
      private:
        /* Start time for the most recent start() call */
        double start_;
        /* Stop time for the most recent stop() call */
        double stop_;
        /* Elapsed wallclock time, recorded on stop() */
        double wtime_;
        /* The current recursive depth/level to keep track of
         * recursive calls to this timer
         */
        int lvl_;

        /* Stack traces of calls to timer - for debugging */
        std::vector<std::vector<std::string> > st_;

        void push_stack_trace(void ){
          std::vector<std::string> st;
          SPIO_Util::Dbg_Util::get_stack_trace(st);
          st_.push_back(st);
        }

        void pop_stack_trace(void ){
          assert(st_.size() > 0);
          st_.pop_back();
        }
    };
  } // namespace SPIO_Ltimer_Utils
} // namespace PIO_Util


#endif /* __SPIO_LTIMER_HPP__ */
