#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* APIs for writing a hyperslab of non-distributed data/variable */
int PIOc_put_vara(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const void *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_put_vara_impl(ncid, varid, start, count, buf);
}

int PIOc_put_vara_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const char *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", static_cast<const void *>(op)).flush();
  return PIOc_put_vara_text_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_schar(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const signed char *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_schar_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const short *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_short_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const int *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_int_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const long *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_long_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_float(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const float *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_float_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const double *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_double_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const unsigned char *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_uchar_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_ushort(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, const unsigned short *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_ushort_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_uint(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const unsigned int *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_uint_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_longlong(int ncid, int varid, const PIO_Offset *start,
                           const PIO_Offset *count, const long long *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_longlong_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const unsigned long long *op)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_vara_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*op", op).flush();
  return PIOc_put_vara_ulonglong_impl(ncid, varid, start, count, op);
}
