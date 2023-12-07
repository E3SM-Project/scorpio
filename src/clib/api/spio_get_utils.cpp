#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_get_utils.hpp"
#include <cassert>

namespace PIO_Util{
  namespace PIO_Get_Utils{

    /* Get the size of the variable slice from count[] array */
    std::size_t get_vslice_sz_from_count(int ncid, int varid, const PIO_Offset *count)
    {
      std::size_t sz = 0;
      int ndims = 0;
      int ret;

      ret = PIOc_inq_varndims_impl(ncid, varid, &ndims);
      if((ret == PIO_NOERR) && count){
        assert(ndims >= 0);

        sz = static_cast<std::size_t>(count[0]);
        for(int i = 1; i < ndims; i++){
          if(count[i] <= 0){
            break;
          }
          sz *= static_cast<std::size_t>(count[i]);
        }
      }

      return sz;
    }

    /* Get the size of the variable slice from start[] array */
    std::size_t get_vslice_sz_from_sidx(int ncid, int varid, const PIO_Offset *sidx)
    {
      std::size_t sz = 0;
      int ndims = 0;
      int ret;

      ret = PIOc_inq_varndims_impl(ncid, varid, &ndims);
      if((ret == PIO_NOERR) && sidx){
        sz = 1;
        if(ndims > 0){
          int dimids[ndims];
          
          ret = PIOc_inq_vardimid_impl(ncid, varid, dimids);
          if(ret == PIO_NOERR){
            for(int i = 0; i < ndims; i++){
              if(sidx[i] < 0){
                break;
              }
              
              PIO_Offset dlen;
              ret = PIOc_inq_dimlen_impl(ncid, dimids[i], &dlen);
              if(ret != PIO_NOERR){
                /* Ignore errors */
                sz = 0;
                break;
              }
              sz *= static_cast<std::size_t>(dlen - sidx[i]);
            }
          }
        }
      }

      return sz;
    }

    /* Get the size of the variable */
    std::size_t get_var_sz(int ncid, int varid)
    {
      std::size_t sz = 0;
      int ndims = 0;
      int ret;

      ret = PIOc_inq_varndims_impl(ncid, varid, &ndims);
      if(ret == PIO_NOERR){
        sz = 1;
        if(ndims > 0){
          int dimids[ndims];
          
          ret = PIOc_inq_vardimid_impl(ncid, varid, dimids);
          if(ret == PIO_NOERR){
            for(int i = 0; i < ndims; i++){
              PIO_Offset dlen;
              ret = PIOc_inq_dimlen_impl(ncid, dimids[i], &dlen);
              if(ret != PIO_NOERR){
                /* Ignore errors */
                sz = 0;
                break;
              }
              sz *= static_cast<std::size_t>(dlen);
            }
          }
        }
      }

      return sz;
    }

  } // namespace PIO_Get_Utils
} // namespace PIO_Util

