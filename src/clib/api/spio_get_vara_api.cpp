#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* APIs for reading a hyperslab of non-distributed data/variable */
int PIOc_get_vara(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count, void *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       char *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_text_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        signed char *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_schar_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        short *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_short_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      int *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_int_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        float *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_float_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       long *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_long_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_double(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, double *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_double_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        unsigned char *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_uchar_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         unsigned short *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_ushort_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       unsigned int *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_uint_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           long long *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_longlong_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_ulonglong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                            unsigned long long *buf)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
  return PIOc_get_vara_ulonglong_impl(ncid, varid, start, count, buf);
}
