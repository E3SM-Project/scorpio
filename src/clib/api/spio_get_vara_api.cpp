#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"
#include "spio_get_utils.hpp"
#include "spio_gptl_utils.hpp"
#include <cassert>

/* APIs for reading a hyperslab of non-distributed data/variable */
int PIOc_get_vara(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count, void *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  /* FIXME: Ignoring tracing void* bufs, this requires more code to find the type of the variable etc */
  return PIOc_get_vara_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       char *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", static_cast<void *>(buf)).flush();
#endif
  ret = PIOc_get_vara_text_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        signed char *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_schar_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        short *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_short_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      int *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_int_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        float *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_float_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       long *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_long_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_double(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, double *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_double_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        unsigned char *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_uchar_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         unsigned short *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_ushort_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       unsigned int *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_uint_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           long long *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_longlong_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}

int PIOc_get_vara_ulonglong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                            unsigned long long *buf)
{
  SPIO_Util::GPTL_Util::GPTL_wrapper func_timer("SPIO:PIOc_get_varx");
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_vara_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*start", start).
    add_arg("*count", count).add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_vara_ulonglong_impl(ncid, varid, start, count, buf);

#if (SPIO_ENABLE_API_TRACING) && (SPIO_ENABLE_API_VAR_TRACING)
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_count(ncid, varid, count));
#endif
  return ret;
}
