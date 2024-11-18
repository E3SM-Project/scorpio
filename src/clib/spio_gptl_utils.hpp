#ifndef __SPIO_GPTL_UTILS_HPP__
#define __SPIO_GPTL_UTILS_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"

#include <string>

namespace SPIO_Util{
  namespace GPTL_Util{
    class GPTL_wrapper{
      public:
        GPTL_wrapper(const char *gptl_timer_name):gptl_timer_name_(gptl_timer_name){ GPTLstart(gptl_timer_name); }
        ~GPTL_wrapper(){ GPTLstop(gptl_timer_name_.c_str()); }
      private:
        const std::string gptl_timer_name_;
    };
  } // namespace GPTL_Util
} // namespace SPIO_Util
#endif // __SPIO_GPTL_UTILS_HPP__
