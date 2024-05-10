#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

int PIOc_put_varm(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const PIO_Offset *imap, const void *buf,
                  PIO_Offset bufcount, MPI_Datatype buftype)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).add_arg("*buf", buf).
    add_arg("bufcount", static_cast<long long int>(bufcount)).
    add_arg("buftype", static_cast<long long int>(buftype)).flush();
#endif
  return PIOc_put_varm_impl(ncid, varid, start, count, stride, imap, buf, bufcount, buftype);
}

int PIOc_put_varm_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap,
                        const unsigned char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_uchar_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, const short *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_short_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride,
                       const PIO_Offset *imap, const char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_text_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, const unsigned short *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_ushort_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            const PIO_Offset *imap, const unsigned long long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_ulonglong_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_int(int ncid, int varid, const PIO_Offset *start,
                      const PIO_Offset *count, const PIO_Offset *stride,
                      const PIO_Offset *imap, const int *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_int_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_float(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride,
                        const PIO_Offset *imap, const float *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_float_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_long(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride,
                       const PIO_Offset *imap, const long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_long_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_uint(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride,
                       const PIO_Offset *imap, const unsigned int *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_uint_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, const double *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_double_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, const signed char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_schar_impl(ncid, varid, start, count, stride, imap, op);
}

int PIOc_put_varm_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const PIO_Offset *imap, const long long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_varm_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*imap", imap).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_varm_longlong_impl(ncid, varid, start, count, stride, imap, op);
}
