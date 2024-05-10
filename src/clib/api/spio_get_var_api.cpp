#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"
#include "spio_get_utils.hpp"

/* APIs for reading entire non-distributed data/variable */
int PIOc_get_var(int ncid, int varid, void *buf)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  return PIOc_get_var_impl(ncid, varid, buf);
}

int PIOc_get_var_text(int ncid, int varid, char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", static_cast<void *>(buf)).flush();
#endif
  ret = PIOc_get_var_text_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  //tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
  tr.add_rval("*buf", (buf) ? buf : "");
#endif
  return ret;
}

int PIOc_get_var_schar(int ncid, int varid, signed char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_schar_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_short(int ncid, int varid, short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_short_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_int(int ncid, int varid, int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_int_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_long(int ncid, int varid, long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_long_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_float(int ncid, int varid, float *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_float_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_double(int ncid, int varid, double *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_double_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_uchar(int ncid, int varid, unsigned char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_uchar_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_ushort(int ncid, int varid, unsigned short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_ushort_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_uint(int ncid, int varid, unsigned int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_uint_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_longlong(int ncid, int varid, long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_longlong_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}

int PIOc_get_var_ulonglong(int ncid, int varid, unsigned long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var_ulonglong_impl(ncid, varid, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_var_sz(ncid, varid));
#endif
  return ret;
}
