#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* APIs for reading file/variable attributes */
int PIOc_get_att(int ncid, int varid, const char *name, void *ip)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  return PIOc_get_att_impl(ncid, varid, name, ip);
}

int PIOc_get_att_text(int ncid, int varid, const char *name, char *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_text_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_schar(int ncid, int varid, const char *name, signed char *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_schar_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_short(int ncid, int varid, const char *name, short *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_short_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_int(int ncid, int varid, const char *name, int *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_int_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_long(int ncid, int varid, const char *name, long *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_long_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_float(int ncid, int varid, const char *name, float *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_float_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_double(int ncid, int varid, const char *name, double *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_double_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_uchar(int ncid, int varid, const char *name, unsigned char *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  return PIOc_get_att_uchar_impl(ncid, varid, name, ip);
}

int PIOc_get_att_ushort(int ncid, int varid, const char *name, unsigned short *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_ushort_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_uint(int ncid, int varid, const char *name, unsigned int *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_uint_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_longlong(int ncid, int varid, const char *name, long long *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_longlong_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}

int PIOc_get_att_ulonglong(int ncid, int varid, const char *name, unsigned long long *ip)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_att_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*ip", ip).flush();
  ret = PIOc_get_att_ulonglong_impl(ncid, varid, name, ip);

  tr.add_rval("*ip", ip);
  return ret;
}
