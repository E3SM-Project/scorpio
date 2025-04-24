#ifndef _SPIO_SORT_UTILS_HPP_
#define _SPIO_SORT_UTILS_HPP_

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>

#include <vector>
#include <algorithm>
#include <utility>
#include <queue>
#include <functional>

namespace SPIO_Util{
  /* K-way merge sort a vector with multiple sorted ranges, where ranges are in {start_idx, end_idx + 1} format
      e.g. vec_kway_merge_sort({2, 3, 4, 6, 7}, {{0, 3}, {3, 5}})
   */
  template<typename T, typename ContigRangeChecker, typename Comp = std::less<T>>
  void vec_kway_merge_sort(std::vector<T> &v, const std::vector<std::pair<std::size_t, std::size_t> > &ranges, ContigRangeChecker &&crc, Comp cmp = Comp{})
  {
    std::vector<T> vtmp(v);

    /* Each pair in the priority queue :
        (idx of range in ranges, idx of next element in v in this range) */
    auto range_comp = [&vtmp, &cmp](const std::pair<std::size_t, std::size_t> &a, const std::pair<std::size_t, std::size_t> &b){ return cmp(vtmp[b.second], vtmp[a.second]); };
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

      /* current_range : Index of current range in ranges[] */
      std::size_t current_range = range_info.first;
      /* r_idx : Index in v[] of the next element to be sorted in this range */
      std::size_t r_idx = range_info.second;

      if(r_idx < ranges[current_range].second){
        /* This element, vtmp[r_idx], is the next element in the sorted result, v[]  */
        v[v_idx++] = vtmp[r_idx++];
      }

      /* Add consecutive elements in this sorted range to the final sorted list, v[] */
      while(r_idx < ranges[current_range].second){
        /* If vtmp[r_idx] is consecutive in range, after vtmp[r_idx - 1], add it to the sorted list */
        if(crc(vtmp[r_idx - 1], vtmp[r_idx])){
          v[v_idx++] = vtmp[r_idx++];
        }
        else{
          break;
        }
      }

      range_idx_info.pop();

      /* If there are still elements in this range (after adding all consecutive elems), add it in to
       * the priority queue - for comparison across ranges
       */
      if(r_idx < ranges[current_range].second){
        /* Add the next range info,
            {idx of current range in ranges[], Next index - in vtmp[] - of element in this range},
          for next element in this range to the priority queue
         */
        range_idx_info.push(std::make_pair(current_range, r_idx));
      }
    }
  }

  /* K-way merge sort a vector with multiple sorted ranges, where ranges are in {start_idx, end_idx + 1} format
      e.g. vec_kway_merge_sort({2, 3, 4, 6, 7}, {{0, 3}, {3, 5}})
   */
  template<typename T>
  void vec_kway_merge_sort(std::vector<T> &v, const std::vector<std::pair<std::size_t, std::size_t> > &ranges)
  {
    return vec_kway_merge_sort(v, ranges, [](const T &a, const T &b) { return (a + 1) == b; });
  }
} // namespace SPIO_Util

#endif // _SPIO_SORT_UTILS_HPP_
