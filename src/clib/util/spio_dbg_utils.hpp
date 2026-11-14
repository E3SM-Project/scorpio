#ifndef __SPIO_DBG_UTILS_HPP__
#define __SPIO_DBG_UTILS_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_types.hpp"

#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <iterator>
#include <algorithm>
#include <initializer_list>

namespace SPIO_Util{
  namespace Dbg_Util{

    /* Debug print a vector */
    template<typename T>
    void print_1dvec(const std::vector<T> &v)
    {
      std::ostringstream ostr;
      ostr << "[";
      std::copy(v.cbegin(), v.cend(), std::ostream_iterator<T>(ostr, ","));
      ostr << "]";
      std::cout << ostr.str().c_str() << "\n" << std::flush;
    }

    /* Debug print a vector of vectors */
    template<typename T>
    void print_1dvec(std::initializer_list<std::vector<T> > vecs)
    {
      std::ostringstream ostr;
      for(typename std::initializer_list<std::vector<T> >::iterator citer = vecs.begin();
          citer != vecs.end(); ++citer){
        ostr << "[";
        std::copy((*citer).cbegin(), (*citer).cend(), std::ostream_iterator<T>(ostr, ","));
        ostr << "] ";
      }
      std::cout << ostr.str().c_str() << "\n" << std::flush;
    }

    /* Debug print an array */
    template<typename T>
    void print_1dvec(const T *vbegin, const T *vend)
    {
      std::ostringstream ostr;
      ostr << "[";
      std::copy(vbegin, vend, std::ostream_iterator<T>(ostr, ","));
      ostr << "]";
      std::cout << ostr.str().c_str() << "\n" << std::flush;
    }

    /* Convert vector to string */
    template<typename T>
    std::string vec1d_to_string(const T *vbegin, const T *vend)
    {
      std::ostringstream ostr;
      ostr << "[";
      std::copy(vbegin, vend, std::ostream_iterator<T>(ostr, ","));
      ostr << "]";
      return ostr.str();
    }

    class Stdout_logger{
      public:
        Stdout_logger(const std::string &info) : info_(info) { std::cout << "Entering " << info_.c_str() << "\n" << std::flush; }
        ~Stdout_logger() { std::cout << "Leaving " << info_.c_str() << "\n" << std::flush; }

      private:
        const std::string info_;
    };

    std::string get_iodesc_info(io_desc_t *ios);

    void get_stack_trace(std::vector<std::string> &st);
    std::string stack_trace_to_string(const std::vector<std::string> &st);
  } // namespace Dbg_Util
} // namespace SPIO_Util

#endif // __SPIO_DBG_UTILS_HPP__
