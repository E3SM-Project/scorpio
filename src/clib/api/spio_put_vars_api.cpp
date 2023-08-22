#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* APIs for writing a hyperslab of strided non-distributed data/variable */
int PIOc_put_vars(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const void *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*buf", buf).flush();
  return PIOc_put_vars_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_put_vars_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride, const char *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_text_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_schar(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride,
                        const signed char *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_shcar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_schar_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_short(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride, const short *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_short_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, const int *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_int_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const float *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_float_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_double(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, const PIO_Offset *stride, const double *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_double_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const long *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_long_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const unsigned char *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_uchar_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const unsigned short *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_ushort_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const unsigned int *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_uint_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const long long *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_longlong_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            const unsigned long long *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vars_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).add_arg("*count", count).
    add_arg("*stride", stride).add_arg("*op", op).flush();
  return PIOc_put_vars_ulonglong_impl(ncid, varid, start, count, stride, op);
}
