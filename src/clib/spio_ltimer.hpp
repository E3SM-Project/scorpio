#ifndef __SPIO_LTIMER_HPP__
#define __SPIO_LTIMER_HPP__

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
    };
  } // namespace SPIO_Ltimer_Utils
} // namespace PIO_Util


#endif /* __SPIO_LTIMER_HPP__ */
