#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for reading non-distributed data/variable at a specified index */
int PIOc_get_var1(int ncid, int varid, const PIO_Offset *index, void *buf)
{
  return PIOc_get_var1_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_text(int ncid, int varid, const PIO_Offset *index, char *buf)
{
  return PIOc_get_var1_text_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_schar(int ncid, int varid, const PIO_Offset *index, signed char *buf)
{
  return PIOc_get_var1_schar_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_short(int ncid, int varid, const PIO_Offset *index, short *buf)
{
  return PIOc_get_var1_short_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_int(int ncid, int varid, const PIO_Offset *index, int *buf)
{
  return PIOc_get_var1_int_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_long(int ncid, int varid, const PIO_Offset *index, long *buf)
{
  return PIOc_get_var1_long_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_float(int ncid, int varid, const PIO_Offset *index, float *buf)
{
  return PIOc_get_var1_float_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_double(int ncid, int varid, const PIO_Offset *index, double *buf)
{
  return PIOc_get_var1_double_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_uchar(int ncid, int varid, const PIO_Offset *index, unsigned char *buf)
{
  return PIOc_get_var1_uchar_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_ushort(int ncid, int varid, const PIO_Offset *index, unsigned short *buf)
{
  return PIOc_get_var1_ushort_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_uint(int ncid, int varid, const PIO_Offset *index, unsigned int *buf)
{
  return PIOc_get_var1_uint_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_longlong(int ncid, int varid, const PIO_Offset *index, long long *buf)
{
  return PIOc_get_var1_longlong_impl(ncid, varid, index, buf);
}

int PIOc_get_var1_ulonglong(int ncid, int varid, const PIO_Offset *index, unsigned long long *buf)
{
  return PIOc_get_var1_ulonglong_impl(ncid, varid, index, buf);
}
