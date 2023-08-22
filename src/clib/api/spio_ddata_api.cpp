#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* ================= Read/Write APIs for distributed data ================ */
int PIOc_advanceframe(int ncid, int varid)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_advanceframe");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).flush();
  return PIOc_advanceframe_impl(ncid, varid);
}

int PIOc_setframe(int ncid, int varid, int frame)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_setframe");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("frame", frame).flush();
  return PIOc_setframe_impl(ncid, varid, frame);
}

int PIOc_write_darray(int ncid, int varid, int ioid, PIO_Offset arraylen, const void *array,
                      const void *fillvalue)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_write_darray");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("ioid", ioid).add_arg("arraylen", static_cast<long long int>(arraylen)).
    add_arg("*array", array).add_arg("*fillvalue", fillvalue).flush();
  return PIOc_write_darray_impl(ncid, varid, ioid, arraylen, array, fillvalue);
}

int PIOc_write_darray_multi(int ncid, const int *varids, int ioid, int nvars, PIO_Offset arraylen,
                            const void *array, const int *frame, const void **fillvalue, bool flushtodisk)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_write_darray_multi");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("*varids", varids).
    add_arg("ioid", ioid).add_arg("nvars", nvars).
    add_arg("arraylen", static_cast<long long int>(arraylen)).
    add_arg("*array", array).add_arg("*frame", frame).
    add_arg("*fillvalue", fillvalue).
    add_arg("flushtodisk", flushtodisk).flush();
  return PIOc_write_darray_multi_impl(ncid, varids, ioid, nvars, arraylen,
                                      array, frame, fillvalue, flushtodisk);
}

int PIOc_read_darray(int ncid, int varid, int ioid, PIO_Offset arraylen, void *array)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_read_darray");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("ioid", ioid).add_arg("arraylen", static_cast<long long int>(arraylen)).
    add_arg("*array", array).flush();
  return PIOc_read_darray_impl(ncid, varid, ioid, arraylen, array);
}

int PIOc_get_local_array_size(int ioid)
{
  /* FIXME: How should we trace these non I/O system specific functions? */
  return PIOc_get_local_array_size_impl(ioid);
}
