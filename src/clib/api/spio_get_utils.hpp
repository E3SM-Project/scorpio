#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"

namespace PIO_Util{
  namespace PIO_Get_Utils{

    /* Get the size of the variable slice from count[] array */
    std::size_t get_vslice_sz_from_count(int ncid, int varid, const PIO_Offset *count);

    /* Get the size of the variable slice from start[] array */
    std::size_t get_vslice_sz_from_sidx(int ncid, int varid, const PIO_Offset *sidx);

    /* Get the size of the variable */
    std::size_t get_var_sz(int ncid, int varid);

  } // namespace PIO_Get_Utils
} // namespace PIO_Util
