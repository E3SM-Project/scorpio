#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* APIs for writing non-distributed data/variable at a specified index */
int PIOc_put_var1(int ncid, int varid, const PIO_Offset *index, const void *buf)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  return PIOc_put_var1_impl(ncid, varid, index, buf);
}

int PIOc_put_var1_text(int ncid, int varid, const PIO_Offset *index, const char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_text_impl(ncid, varid, index, op);
}

int PIOc_put_var1_schar(int ncid, int varid, const PIO_Offset *index, const signed char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_schar_impl(ncid, varid, index, op);
}

int PIOc_put_var1_short(int ncid, int varid, const PIO_Offset *index, const short *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_short_impl(ncid, varid, index, op);
}

int PIOc_put_var1_int(int ncid, int varid, const PIO_Offset *index, const int *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_int_impl(ncid, varid, index, op);
}

int PIOc_put_var1_long(int ncid, int varid, const PIO_Offset *index, const long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_long_impl(ncid, varid, index, op);
}

int PIOc_put_var1_float(int ncid, int varid, const PIO_Offset *index, const float *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_float_impl(ncid, varid, index, op);
}

int PIOc_put_var1_double(int ncid, int varid, const PIO_Offset *index, const double *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_double_impl(ncid, varid, index, op);
}

int PIOc_put_var1_uchar(int ncid, int varid, const PIO_Offset *index,
                        const unsigned char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_uchar_impl(ncid, varid, index, op);
}

int PIOc_put_var1_ushort(int ncid, int varid, const PIO_Offset *index,
                         const unsigned short *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_ushort_impl(ncid, varid, index, op);
}

int PIOc_put_var1_uint(int ncid, int varid, const PIO_Offset *index,
                       const unsigned int *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_uint_impl(ncid, varid, index, op);
}

int PIOc_put_var1_longlong(int ncid, int varid, const PIO_Offset *index, const long long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_longlong_impl(ncid, varid, index, op);
}

int PIOc_put_var1_ulonglong(int ncid, int varid, const PIO_Offset *index,
                            const unsigned long long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var1_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_var1_ulonglong_impl(ncid, varid, index, op);
}

