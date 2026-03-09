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
        GPTL_wrapper(const std::string &gptl_timer_name):gptl_timer_name_(gptl_timer_name), is_valid_(false){
          if(!gptl_timer_name_.empty()){
            GPTLstart(gptl_timer_name_.c_str());
            is_valid_ = true;
          }
        }

        ~GPTL_wrapper(){
          if(is_valid_) { GPTLstop(gptl_timer_name_.c_str()); }
        }

      private:
        const std::string gptl_timer_name_;
        /* Is a valid GPTL timer - has a valid timer name */
        bool is_valid_;
    };

    class GPTL_timer : public GPTL_wrapper{
      public:
        GPTL_timer(const std::string &name) : GPTL_wrapper(name) {}
    };

    class GPTL_cond_timer : public GPTL_timer{
      public:
        GPTL_cond_timer(const std::string &name, bool cond) : GPTL_timer((cond) ? name : "") {}
    };

  } // namespace GPTL_Util
} // namespace SPIO_Util
#endif // __SPIO_GPTL_UTILS_HPP__
