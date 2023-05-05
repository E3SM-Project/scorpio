#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for reading file/variable attributes */
int PIOc_get_att(int ncid, int varid, const char *name, void *ip)
{
  return PIOc_get_att_impl(ncid, varid, name, ip);
}

int PIOc_get_att_text(int ncid, int varid, const char *name, char *ip)
{
  return PIOc_get_att_text_impl(ncid, varid, name, ip);
}

int PIOc_get_att_schar(int ncid, int varid, const char *name, signed char *ip)
{
  return PIOc_get_att_schar_impl(ncid, varid, name, ip);
}

int PIOc_get_att_short(int ncid, int varid, const char *name, short *ip)
{
  return PIOc_get_att_short_impl(ncid, varid, name, ip);
}

int PIOc_get_att_int(int ncid, int varid, const char *name, int *ip)
{
  return PIOc_get_att_int_impl(ncid, varid, name, ip);
}

int PIOc_get_att_long(int ncid, int varid, const char *name, long *ip)
{
  return PIOc_get_att_long_impl(ncid, varid, name, ip);
}

int PIOc_get_att_float(int ncid, int varid, const char *name, float *ip)
{
  return PIOc_get_att_float_impl(ncid, varid, name, ip);
}

int PIOc_get_att_double(int ncid, int varid, const char *name, double *ip)
{
  return PIOc_get_att_double_impl(ncid, varid, name, ip);
}

int PIOc_get_att_uchar(int ncid, int varid, const char *name, unsigned char *ip)
{
  return PIOc_get_att_uchar_impl(ncid, varid, name, ip);
}

int PIOc_get_att_ushort(int ncid, int varid, const char *name, unsigned short *ip)
{
  return PIOc_get_att_ushort_impl(ncid, varid, name, ip);
}

int PIOc_get_att_uint(int ncid, int varid, const char *name, unsigned int *ip)
{
  return PIOc_get_att_uint_impl(ncid, varid, name, ip);
}

int PIOc_get_att_longlong(int ncid, int varid, const char *name, long long *ip)
{
  return PIOc_get_att_longlong_impl(ncid, varid, name, ip);
}

int PIOc_get_att_ulonglong(int ncid, int varid, const char *name, unsigned long long *ip)
{
  return PIOc_get_att_ulonglong_impl(ncid, varid, name, ip);
}
