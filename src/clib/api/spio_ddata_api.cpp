#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* ================= Read/Write APIs for distributed data ================ */
int PIOc_advanceframe(int ncid, int varid)
{
  return PIOc_advanceframe_impl(ncid, varid);
}

int PIOc_setframe(int ncid, int varid, int frame)
{
  return PIOc_setframe_impl(ncid, varid, frame);
}

int PIOc_write_darray(int ncid, int varid, int ioid, PIO_Offset arraylen, const void *array,
                      const void *fillvalue)
{
  return PIOc_write_darray_impl(ncid, varid, ioid, arraylen, array, fillvalue);
}

int PIOc_write_darray_multi(int ncid, const int *varids, int ioid, int nvars, PIO_Offset arraylen,
                            const void *array, const int *frame, const void **fillvalue, bool flushtodisk)
{
  return PIOc_write_darray_multi_impl(ncid, varids, ioid, nvars, arraylen,
                                      array, frame, fillvalue, flushtodisk);
}

int PIOc_read_darray(int ncid, int varid, int ioid, PIO_Offset arraylen, void *array)
{
  return PIOc_read_darray_impl(ncid, varid, ioid, arraylen, array);
}

int PIOc_get_local_array_size(int ioid)
{
  return PIOc_get_local_array_size_impl(ioid);
}
