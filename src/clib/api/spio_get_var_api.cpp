#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for reading entire non-distributed data/variable */
int PIOc_get_var(int ncid, int varid, void *buf)
{
  return PIOc_get_var_impl(ncid, varid, buf);
}

int PIOc_get_var_text(int ncid, int varid, char *buf)
{
  return PIOc_get_var_text_impl(ncid, varid, buf);
}

int PIOc_get_var_schar(int ncid, int varid, signed char *buf)
{
  return PIOc_get_var_schar_impl(ncid, varid, buf);
}

int PIOc_get_var_short(int ncid, int varid, short *buf)
{
  return PIOc_get_var_short_impl(ncid, varid, buf);
}

int PIOc_get_var_int(int ncid, int varid, int *buf)
{
  return PIOc_get_var_int_impl(ncid, varid, buf);
}

int PIOc_get_var_long(int ncid, int varid, long *buf)
{
  return PIOc_get_var_long_impl(ncid, varid, buf);
}

int PIOc_get_var_float(int ncid, int varid, float *buf)
{
  return PIOc_get_var_float_impl(ncid, varid, buf);
}

int PIOc_get_var_double(int ncid, int varid, double *buf)
{
  return PIOc_get_var_double_impl(ncid, varid, buf);
}

int PIOc_get_var_uchar(int ncid, int varid, unsigned char *buf)
{
  return PIOc_get_var_uchar_impl(ncid, varid, buf);
}

int PIOc_get_var_ushort(int ncid, int varid, unsigned short *buf)
{
  return PIOc_get_var_ushort_impl(ncid, varid, buf);
}

int PIOc_get_var_uint(int ncid, int varid, unsigned int *buf)
{
  return PIOc_get_var_uint_impl(ncid, varid, buf);
}

int PIOc_get_var_longlong(int ncid, int varid, long long *buf)
{
  return PIOc_get_var_longlong_impl(ncid, varid, buf);
}

int PIOc_get_var_ulonglong(int ncid, int varid, unsigned long long *buf)
{
  return PIOc_get_var_ulonglong_impl(ncid, varid, buf);
}
