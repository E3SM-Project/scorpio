#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_types.hpp"
#include "spio_dbg_utils.hpp"
#include "execinfo.h"

#include <vector>
#include <array>
#include <iostream>
#include <sstream>
#include <iterator>
#include <algorithm>
#include <initializer_list>

std::string SPIO_Util::Dbg_Util::get_iodesc_info(io_desc_t *iodesc)
{
  std::ostringstream ostr;
  if(iodesc){
    ostr << "iodesc{";
    ostr << "ioid = " << iodesc->ioid << ", maplen =" << iodesc->maplen;
    ostr << ", map = [";
    //  std::copy(iodesc->map, iodesc->map + iodesc->maplen, std::ostream_iterator<long>(ostr, ","));
      ostr << "...";
    ostr << "], ndof = " << iodesc->ndof << ", ndims = " << iodesc->ndims;
    ostr << ", dimlen = [";
      std::copy(iodesc->dimlen, iodesc->dimlen + iodesc->ndims, std::ostream_iterator<int>(ostr, ","));
    ostr << "], rearranger = " << iodesc->rearranger;
    ostr << ", maxregions =" << iodesc->maxregions;
    ostr << ", llen = " << iodesc->llen;
    io_region *cur_region = iodesc->firstregion;
    for(int i = 0; i < iodesc->maxregions; i++){
      if(cur_region == NULL){
        ostr << ", region " << i << "{NULL}"; continue;
      }
      ostr << ", region " << i << " {";
        ostr << "loffset = " << cur_region->loffset;
        ostr << ", start = [";
          std::copy(cur_region->start, cur_region->start + iodesc->ndims, std::ostream_iterator<long>(ostr, ","));
        ostr << "], ";
        ostr << "count = [";
          std::copy(cur_region->count, cur_region->count + iodesc->ndims, std::ostream_iterator<long>(ostr, ","));
        ostr << "]";
      ostr << " }";
      cur_region = cur_region->next;
    }
    ostr << "}";
  }
  else{
    ostr<< "iodesc{NULL}";
  }

  return ostr.str();
}

void SPIO_Util::Dbg_Util::get_stack_trace(std::vector<std::string> &st)
{
  const std::size_t MAX_STACK_DEPTH = 20;
  std::array<void *, MAX_STACK_DEPTH> st_addrs;

  std::size_t st_sz = backtrace(st_addrs.data(), st_addrs.size());
  char **st_syms = backtrace_symbols(st_addrs.data(), st_sz);

  if(!st_syms){ return; }

  std::for_each(st_syms, st_syms + st_sz, [&st](char *sname) { st.push_back(sname); });

  free(st_syms);
}

std::string SPIO_Util::Dbg_Util::stack_trace_to_string(const std::vector<std::string> &st)
{
  std::ostringstream ostr;
  std::for_each(st.cbegin(), st.cend(), [&ostr](const std::string &str) { ostr << str << "\n"; });
  return ostr.str();
}
