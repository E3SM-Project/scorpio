#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for writing entire non-distributed data/variable */
int PIOc_put_var(int ncid, int varid, const void *buf)
{
  return PIOc_put_var_impl(ncid, varid, buf);
}

int PIOc_put_var_text(int ncid, int varid, const char *op)
{
  return PIOc_put_var_text_impl(ncid, varid, op);
}

int PIOc_put_var_schar(int ncid, int varid, const signed char *op)
{
  return PIOc_put_var_schar_impl(ncid, varid, op);
}

int PIOc_put_var_short(int ncid, int varid, const short *op)
{
  return PIOc_put_var_short_impl(ncid, varid, op);
}

int PIOc_put_var_int(int ncid, int varid, const int *op)
{
  return PIOc_put_var_int_impl(ncid, varid, op);
}

int PIOc_put_var_long(int ncid, int varid, const long *op)
{
  return PIOc_put_var_long_impl(ncid, varid, op);
}

int PIOc_put_var_float(int ncid, int varid, const float *op)
{
  return PIOc_put_var_float_impl(ncid, varid, op);
}

int PIOc_put_var_double(int ncid, int varid, const double *op)
{
  return PIOc_put_var_double_impl(ncid, varid, op);
}

int PIOc_put_var_uchar(int ncid, int varid, const unsigned char *op)
{
  return PIOc_put_var_uchar_impl(ncid, varid, op);
}

int PIOc_put_var_ushort(int ncid, int varid, const unsigned short *op)
{
  return PIOc_put_var_ushort_impl(ncid, varid, op);
}

int PIOc_put_var_uint(int ncid, int varid, const unsigned int *op)
{
  return PIOc_put_var_uint_impl(ncid, varid, op);
}

int PIOc_put_var_longlong(int ncid, int varid, const long long *op)
{
  return PIOc_put_var_longlong_impl(ncid, varid, op);
}

int PIOc_put_var_ulonglong(int ncid, int varid, const unsigned long long *op)
{
  return PIOc_put_var_ulonglong_impl(ncid, varid, op);
}
