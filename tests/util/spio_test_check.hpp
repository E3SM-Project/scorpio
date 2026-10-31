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
  bool lpass = true;

  if(res.size() != exp.size()){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        ctx_msg + " : ERROR: The result and expected vectors are of different sizes\n");
    lpass = false;
  }
  else{
    for(std::size_t i = 0; i < res.size(); i++){
      if(res[i] != exp[i]){
        std::ostringstream oss;
        oss << ctx_msg << " : ERROR: Invalid/Unexpected value, array[" << i << "] = " << res[i]
            << " (Expected array[" << i << "] = " << exp[i] << ")\n";
        tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, oss.str());
        lpass = false;
        break;
      }
    }
  }

  /* Every rank must agree on the pass/fail verdict before returning since
   * callers return early on failure, and other PIO calls further down are
   * MPI-collective - a rank-divergent result here would deadlock those.
   */
  int lpass_i = lpass ? 1 : 0;
  int gpass_i = 0;
  MPI_Allreduce(&lpass_i, &gpass_i, 1, MPI_INT, MPI_LAND, tf.get_comm());

  return (gpass_i != 0);
}

inline bool passert(SPIO_TF::Test_framework &tf, MPI_Comm comm, bool local_cond,
    const std::string &ctx_msg)
{
  if(!local_cond){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, ctx_msg + "\n");
  }

  int lpass_i = local_cond ? 1 : 0;
  int gpass_i = 0;
  MPI_Allreduce(&lpass_i, &gpass_i, 1, MPI_INT, MPI_LAND, comm);

  return (gpass_i != 0);
}

inline bool passert(SPIO_TF::Test_framework &tf, bool local_cond, const std::string &ctx_msg)
{
  return passert(tf, tf.get_comm(), local_cond, ctx_msg);
}

inline bool check_ret(SPIO_TF::Test_framework &tf, MPI_Comm comm, int ierr, const std::string &ctx_msg)
{
  if(ierr == PIO_NOERR){
    return passert(tf, comm, true, ctx_msg);
  }
  return passert(tf, comm, false, ctx_msg + " : ERROR: ret = " + std::to_string(ierr));
}

inline bool check_ret(SPIO_TF::Test_framework &tf, int ierr, const std::string &ctx_msg)
{
  return check_ret(tf, tf.get_comm(), ierr, ctx_msg);
}

#endif // __SPIO_TEST_CHECK__
