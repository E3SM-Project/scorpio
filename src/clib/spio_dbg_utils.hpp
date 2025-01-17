#ifndef __SPIO_DBG_UTILS_HPP__
#define __SPIO_DBG_UTILS_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_types.hpp"

#include <vector>
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

    std::string get_iodesc_info(io_desc_t *ios);
  } // namespace Dbg_Util
} // namespace SPIO_Util

#endif // __SPIO_DBG_UTILS_HPP__
