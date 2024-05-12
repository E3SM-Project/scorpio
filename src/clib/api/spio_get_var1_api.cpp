#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"
#include "spio_get_utils.hpp"

/* APIs for reading non-distributed data/variable at a specified index */
int PIOc_get_var1(int ncid, int varid, const PIO_Offset *index, void *buf)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  return PIOc_get_var1_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_text(int ncid, int varid, const PIO_Offset *index, char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_text_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_schar(int ncid, int varid, const PIO_Offset *index, signed char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_schar_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_short(int ncid, int varid, const PIO_Offset *index, short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_short_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_int(int ncid, int varid, const PIO_Offset *index, int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_int_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_long(int ncid, int varid, const PIO_Offset *index, long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_long_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_float(int ncid, int varid, const PIO_Offset *index, float *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_float_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_double(int ncid, int varid, const PIO_Offset *index, double *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_double_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_uchar(int ncid, int varid, const PIO_Offset *index, unsigned char *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_uchar_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_ushort(int ncid, int varid, const PIO_Offset *index, unsigned short *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_ushort_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_uint(int ncid, int varid, const PIO_Offset *index, unsigned int *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_uint_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_longlong(int ncid, int varid, const PIO_Offset *index, long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_longlong_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}

int PIOc_get_var1_ulonglong(int ncid, int varid, const PIO_Offset *index, unsigned long long *buf)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var1_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*index", index).
    add_arg("*buf", buf).flush();
#endif
  ret = PIOc_get_var1_ulonglong_impl(ncid, varid, index, buf);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*buf", buf, PIO_Util::PIO_Get_Utils::get_vslice_sz_from_sidx(ncid, varid, index));
#endif
  return ret;
}
