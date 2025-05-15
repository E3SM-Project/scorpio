#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"
#include "spio_gptl_utils.hpp"

/* APIs for writing entire non-distributed data/variable */
int PIOc_put_var(int ncid, int varid, const void *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  return PIOc_put_var_impl(ncid, varid, buf);
}

int PIOc_put_var_text(int ncid, int varid, const char *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", static_cast<const void *>(op)).flush();
#endif
  return PIOc_put_var_text_impl(ncid, varid, op);
}

int PIOc_put_var_schar(int ncid, int varid, const signed char *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_schar");
  /* Both bytes and chars are written using this interface, trying to trace/log
      a (byte *) as a (char *), a NUL terminated string can cause issues.
      So explicitly type-cast pointer to (void *) when tracing it */
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", static_cast<const void *>(op)).flush();
#endif
  return PIOc_put_var_schar_impl(ncid, varid, op);
}

int PIOc_put_var_short(int ncid, int varid, const short *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_short_impl(ncid, varid, op);
}

int PIOc_put_var_int(int ncid, int varid, const int *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_int_impl(ncid, varid, op);
}

int PIOc_put_var_long(int ncid, int varid, const long *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_long_impl(ncid, varid, op);
}

int PIOc_put_var_float(int ncid, int varid, const float *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_float_impl(ncid, varid, op);
}

int PIOc_put_var_double(int ncid, int varid, const double *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_double_impl(ncid, varid, op);
}

int PIOc_put_var_uchar(int ncid, int varid, const unsigned char *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_uchar");
  /* Both bytes and chars are written using this interface, trying to trace/log
      a (byte *) as a (char *), a NUL terminated string can cause issues.
      So explicitly type-cast pointer to (void *) when tracing it */
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", static_cast<const void *>(op)).flush();
#endif
  return PIOc_put_var_uchar_impl(ncid, varid, op);
}

int PIOc_put_var_ushort(int ncid, int varid, const unsigned short *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_ushort_impl(ncid, varid, op);
}

int PIOc_put_var_uint(int ncid, int varid, const unsigned int *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_uint_impl(ncid, varid, op);
}

int PIOc_put_var_longlong(int ncid, int varid, const long long *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_longlong_impl(ncid, varid, op);
}

int PIOc_put_var_ulonglong(int ncid, int varid, const unsigned long long *op)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_put_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_var_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*op", op).flush();
#endif
  return PIOc_put_var_ulonglong_impl(ncid, varid, op);
}
