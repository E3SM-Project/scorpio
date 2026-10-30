#ifndef __SPIO_TEST_CHECK__
#define __SPIO_TEST_CHECK__

#include <vector>
#include <string>
#include <sstream>
#include "pio.h"
#include "spio_test_framework.hpp"

template<typename T> struct Pio_type_map;
template<> struct Pio_type_map<int>    { static const int value = PIO_INT; };
template<> struct Pio_type_map<float>  { static const int value = PIO_REAL; };
template<> struct Pio_type_map<double> { static const int value = PIO_DOUBLE; };

template<typename T>
bool check_val(SPIO_TF::Test_framework &tf, const std::vector<T> &res,
    const std::vector<T> &exp, const std::string &ctx_msg)
{
  if(res.size() != exp.size()){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        ctx_msg + " : ERROR: The result and expected vectors are of different sizes\n");
    return false;
  }

  for(std::size_t i = 0; i < res.size(); i++){
    if(res[i] != exp[i]){
      std::ostringstream oss;
      oss << ctx_msg << " : ERROR: Invalid/Unexpected value, array[" << i << "] = " << res[i]
          << " (Expected array[" << i << "] = " << exp[i] << ")\n";
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, oss.str());
      return false;
    }
  }

  return true;
}

#endif // __SPIO_TEST_CHECK__
