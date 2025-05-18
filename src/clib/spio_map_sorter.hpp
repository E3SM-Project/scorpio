#ifndef __SPIO_MAP_SORTER_HPP__
#define __SPIO_MAP_SORTER_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_types.hpp"

#include <vector>
#include <numeric>
#include <cassert>
#include <algorithm>

namespace SPIO_Util{

  class Map_sorter{
    public:
      Map_sorter(){}

      /* Create sorter map based on values in vector v, the comparator passed
         should be for comparing values in v
       */
      template<typename T, typename Comp=std::less<T> >
      Map_sorter(const std::vector<T> &v, Comp cmp = Comp{}):sort_map_(v.size())
      {
        auto sort_map_cmp = [&v, &cmp](T a, T b){ return cmp(v[a], v[b]); };
        init_sort_map(sort_map_cmp);
      }

      /* Create sorter based on the given comparator, the comparator passed
         should be for comparing sort map values
       */
      template<typename Comp>
      Map_sorter(std::size_t sz, Comp cmp):sort_map_(sz)
      {
        init_sort_map(cmp);
      }

      /* Init sorter map based on values in vector v, the comparator passed
         should be for comparing values in v
       */
      template<typename T, typename Comp=std::less<T> >
      void init(const std::vector<T> &v, Comp cmp = Comp{})
      {
        /* Don't allow reinits */
        assert(sort_map_.size() == 0);

        sort_map_.resize(v.size());
        auto sort_map_cmp = [&v, &cmp](T a, T b){ return cmp(v[a], v[b]); };
        init_sort_map(sort_map_cmp);
      }

      /* Init sorter based on the given comparator, the comparator passed
         should be for comparing sort map values
       */
      template<typename Comp>
      void init(std::size_t sz, Comp cmp)
      {
        /* Don't allow reinits */
        assert(sort_map_.size() == 0);

        sort_map_.resize(sz);
        init_sort_map(cmp);
      }

      template<typename T>
      void sort(std::vector<T> &v)
      {
        std::vector<T> vtmp(v);
        assert(v.size() == sort_map_.size());

        for(std::size_t i = 0; i < vtmp.size(); i++){
          v[sort_map_[i]] = vtmp[i];
        }
      }

      /* FIXME: Get rid of this function that exposes the sort map
         Current code needs this map for creating recv types. We need to find
         an alternate way to create these types
       */
      const std::vector<std::size_t> &get_sort_map(void ) { return sort_map_; }

      std::size_t size(void ) const { return sort_map_.size(); }
    private:
      std::vector<std::size_t> sort_map_;

      template<typename Comp>
      void init_sort_map(Comp cmp)
      {
        /* FIXME: Do we need a push sort map - isn't pull map enough ? */
        /* pull_sort_map => mapping for pulling values from source to sorted */
        std::vector<std::size_t> pull_sort_map(sort_map_.size());
        std::iota(pull_sort_map.begin(), pull_sort_map.end(), 0);
        std::sort(pull_sort_map.begin(), pull_sort_map.end(), cmp);
        /* sort_map_ is push map => mapping for pushing values from source to sorted */
        for(std::size_t i = 0; i < pull_sort_map.size(); i++){
          sort_map_[pull_sort_map[i]] = i;
        }
      }
  };

} // namespace SPIO_Util

#endif // __SPIO_MAP_SORTER_HPP__
