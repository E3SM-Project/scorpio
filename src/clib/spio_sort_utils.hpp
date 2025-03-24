#ifndef _SPIO_SORT_UTILS_HPP_
#define _SPIO_SORT_UTILS_HPP_

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>

#include <vector>
#include <algorithm>
#include <utility>
#include <queue>

namespace SPIO_Util{
  template<typename T>
  void vec_kway_merge_sort(std::vector<T> &v, const std::vector<std::pair<std::size_t, std::size_t> > &ranges)
  {
    std::vector<T> vtmp(v);

    /* Each pair in the priority queue :
        (idx of range in ranges, idx of next element in v in this range) */
    auto range_comp = [&vtmp](const std::pair<std::size_t, std::size_t> &a, const std::pair<std::size_t, std::size_t> &b){ return vtmp[a.second] > vtmp[b.second]; };
    std::priority_queue<std::pair<std::size_t, std::size_t>, std::vector<std::pair<std::size_t, std::size_t> >, decltype(range_comp)> range_idx_info(range_comp); 

    std::vector<std::size_t> cur_range_idx(ranges.size());
    /* Add range info from each range to the priority queue */
    for(std::size_t i = 0; i < ranges.size(); i++){
      cur_range_idx[i] = ranges[i].first;

      if(cur_range_idx[i] < ranges[i].second){
        range_idx_info.push(std::make_pair(i, cur_range_idx[i]));
      }
    }

    std::size_t v_idx = 0;
    while(!range_idx_info.empty()){
      const std::pair<std::size_t, std::size_t> &range_info = range_idx_info.top();

      std::size_t current_range = range_info.first;
      std::size_t r_idx = range_info.second;

      if(r_idx < ranges[current_range].second){
        v[v_idx++] = vtmp[r_idx++];
      }

      while(r_idx < ranges[current_range].second){
        if(vtmp[r_idx] == vtmp[r_idx - 1] + 1){
          v[v_idx++] = vtmp[r_idx++];
        }
        else{
          break;
        }
      }

      range_idx_info.pop();

      if(r_idx < ranges[current_range].second){
        range_idx_info.push(std::make_pair(current_range, r_idx));
      }
    }
  }
} // namespace SPIO_Util

#endif // _SPIO_SORT_UTILS_HPP_
