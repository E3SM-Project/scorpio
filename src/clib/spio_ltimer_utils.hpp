#ifndef __SPIO_LTIMER_UTILS_HPP__
#define __SPIO_LTIMER_UTILS_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"

#include "spio_ltimer.h"

#include <string>

namespace SPIO_Util{
  namespace SPIO_Ltimer_Utils{
    class SPIO_ltimer_wrapper{
      public:
        SPIO_ltimer_wrapper(const char *timer_name):timer_name_(timer_name){
          spio_ltimer_start(timer_name_.c_str());
        }
        ~SPIO_ltimer_wrapper(){
          spio_ltimer_stop(timer_name_.c_str());
        }
      private:
        const std::string timer_name_;
    };
  } // namespace SPIO_Ltimer_Utils
} // namespace SPIO_Util
#endif // __SPIO_LTIMER_UTILS_HPP__
