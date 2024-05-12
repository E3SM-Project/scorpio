#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"
#include "spio_get_utils.hpp"

/* APIs for reading a hyperslab of strided non-distributed data/variable */
int PIOc_get_vars(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, void *buf)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  return PIOc_get_vars_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_text_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, signed char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_schar_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_short_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_int_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_long_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, float *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_float_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, double *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_double_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, unsigned char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_uchar_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, unsigned short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_ushort_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, unsigned int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_uint_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_longlong_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vars_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            unsigned long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vars_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*start", start).add_arg("*count", count).add_arg("*stride", stride).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vars_ulonglong_impl(ncid, varid, start, count, stride, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}
