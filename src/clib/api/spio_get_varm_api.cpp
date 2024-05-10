#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"
#include "spio_get_utils.hpp"

/* APIs for reading/writing a hyperslab of strided non-distributed data/variable
 * with a mapped array. The mapped array maps between memory and variable data
 */
/* Varm functions are deprecated and should be used with extreme
 * caution or not at all. Varm functions are not supported in
 * async mode. */
int PIOc_get_varm_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, signed char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_schar_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_short_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_ulonglong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                            const PIO_Offset *stride, const PIO_Offset *imap, unsigned long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_ulonglong_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, unsigned short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_ushort_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const PIO_Offset *imap, long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_longlong_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, double *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_double_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_text_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, const PIO_Offset *imap, int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_int_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, unsigned int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_uint_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const PIO_Offset *imap, void *buf,
                  PIO_Offset bufcount, MPI_Datatype buftype)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).
    add_arg("bufcount", static_cast<long long int>(bufcount)).
    add_arg("buftype", static_cast<long long int>(buftype)).flush();
#endif
  return PIOc_get_varm_impl(ncid, varid, start, count, stride, imap, buf, bufcount, buftype);
}

int PIOc_get_varm_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, float *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_float_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_varm_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_varm_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_varm_long_impl(ncid, varid, start, count, stride, imap, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}
