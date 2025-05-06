#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* Write APIs for file/variable attributes */
int PIOc_put_att(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len, const void *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_text(int ncid, int varid, const char *name, PIO_Offset len, const char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_text");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_text_impl(ncid, varid, name, len, op);
}

int PIOc_put_att_schar(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const signed char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_schar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op, len).flush();
#endif
  return PIOc_put_att_schar_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_short(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const short *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_short");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_short_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_int(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                     const int *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_int");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_int_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_long(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                      const long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_long");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_long_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_float(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const float *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_float");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_float_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_double(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                        const double *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_double");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_double_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_uchar(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const unsigned char *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_uchar");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op, len).flush();
#endif
  return PIOc_put_att_uchar_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_ushort(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                        const unsigned short *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_ushort");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_ushort_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_uint(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                      const unsigned int *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_uint");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_uint_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_longlong(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                          const long long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_longlong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_longlong_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_ulonglong(int ncid, int varid, const char *name, nc_type xtype,
                           PIO_Offset len, const unsigned long long *op)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_put_att_ulonglong");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*op", op).flush();
#endif
  return PIOc_put_att_ulonglong_impl(ncid, varid, name, xtype, len, op);
}
