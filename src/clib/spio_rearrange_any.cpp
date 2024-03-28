#include <iostream>
#include <stdexcept>
#include <cassert>
#include <string>
#include <vector>
#include <utility>
#include <limits>

#include "pio_config.h"
#include "pio.h"
//#include "pio_internal.h"

extern "C"{
#include "spio_rearrange_any.h"
}

namespace SPIO_Util{

  /* FIXME: Handle malformed string pairs */
  /* Convert "(intx, inty)" to std::pair<int, int> */
  std::pair<int, int> str2pair(const std::string &spair)
  {
    std::pair<int, int> res;
    const char PAIR_BBEGIN = '(';
    const char PAIR_BEND = ')';
    const char PAIR_SEP = ',';

    res.first = stoi(spair.substr(spair.find(PAIR_BBEGIN) + 1, spair.find(PAIR_SEP)));
    res.second = stoi(spair.substr(spair.find(PAIR_SEP) + 1, spair.find(PAIR_BEND)));

    return res;
  }

  namespace GVars{
    /* Collection of I/O decomposition length (local) ranges for different rearrangers */
    static std::vector<std::pair<std::pair<int, int>, int> > opt_pio_rearr_ranges;
  } // namespace GVars

} // namespace SPIO_Util

void spio_init_pio_rearr_any(void )
{
  static bool initialized = false;
  if(initialized){
    return;
  }

  /* FIXME: Once we have more ranges here, we need to sort the coll */
  /* Add the range for SUBSET rearranger - from configure */
  std::pair<int, int> subset_range = SPIO_Util::str2pair(PIO_REARR_ANY_SUBSET_RANGE);
  if(subset_range.first == -1){
    subset_range.first = std::numeric_limits<int>::max();
  }
  if(subset_range.second == -1){
    subset_range.second = std::numeric_limits<int>::max();
  }
  SPIO_Util::GVars::opt_pio_rearr_ranges.push_back(std::make_pair(subset_range, PIO_REARR_SUBSET));

  initialized = true;
}

int spio_get_opt_pio_rearr(iosystem_desc_t *iosys, int local_decomp_maplen)
{
  int mpierr = MPI_SUCCESS;

  int opt_rearr = PIO_REARR_BOX;
  int max_maplen = 0;

  assert(iosys);
  
  mpierr = MPI_Allreduce(&local_decomp_maplen, &max_maplen,
                          1, MPI_INT, MPI_MAX, iosys->union_comm);
  if(mpierr != MPI_SUCCESS){
    if(iosys->union_rank == 0){
      fprintf(stderr,
        "Finding max I/O decomposition len to decide on rearranger failed (iosysid = %d). Defaulting to BOX rearranger",
        iosys->iosysid);
    }
    return PIO_REARR_BOX;
  }

  /* FIXME: Do more sophisticated searches once we have multiple ranges here */
  for(std::vector<std::pair<std::pair<int, int>, int> >::const_iterator
        citer = SPIO_Util::GVars::opt_pio_rearr_ranges.cbegin();
        citer != SPIO_Util::GVars::opt_pio_rearr_ranges.cend(); ++citer){
    if(citer->first.first > max_maplen){
      break;
    }
    if((max_maplen >= citer->first.first) && (max_maplen < citer->first.second)){
      opt_rearr = citer->second;
      break;
    }
  }

  return opt_rearr;
}
