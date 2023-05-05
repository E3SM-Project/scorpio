#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for writing non-distributed data/variable at a specified index */
int PIOc_put_var1(int ncid, int varid, const PIO_Offset *index, const void *buf)
{
  return PIOc_put_var1_impl(ncid, varid, index, buf);
}

int PIOc_put_var1_text(int ncid, int varid, const PIO_Offset *index, const char *op)
{
  return PIOc_put_var1_text_impl(ncid, varid, index, op);
}

int PIOc_put_var1_schar(int ncid, int varid, const PIO_Offset *index, const signed char *op)
{
  return PIOc_put_var1_schar_impl(ncid, varid, index, op);
}

int PIOc_put_var1_short(int ncid, int varid, const PIO_Offset *index, const short *op)
{
  return PIOc_put_var1_short_impl(ncid, varid, index, op);
}

int PIOc_put_var1_int(int ncid, int varid, const PIO_Offset *index, const int *op)
{
  return PIOc_put_var1_int_impl(ncid, varid, index, op);
}

int PIOc_put_var1_long(int ncid, int varid, const PIO_Offset *index, const long *ip)
{
  return PIOc_put_var1_long_impl(ncid, varid, index, ip);
}

int PIOc_put_var1_float(int ncid, int varid, const PIO_Offset *index, const float *op)
{
  return PIOc_put_var1_float_impl(ncid, varid, index, op);
}

int PIOc_put_var1_double(int ncid, int varid, const PIO_Offset *index, const double *op)
{
  return PIOc_put_var1_double_impl(ncid, varid, index, op);
}

int PIOc_put_var1_uchar(int ncid, int varid, const PIO_Offset *index,
                        const unsigned char *op)
{
  return PIOc_put_var1_uchar_impl(ncid, varid, index, op);
}

int PIOc_put_var1_ushort(int ncid, int varid, const PIO_Offset *index,
                         const unsigned short *op)
{
  return PIOc_put_var1_ushort_impl(ncid, varid, index, op);
}

int PIOc_put_var1_uint(int ncid, int varid, const PIO_Offset *index,
                       const unsigned int *op)
{
  return PIOc_put_var1_uint_impl(ncid, varid, index, op);
}

int PIOc_put_var1_longlong(int ncid, int varid, const PIO_Offset *index, const long long *op)
{
  return PIOc_put_var1_longlong_impl(ncid, varid, index, op);
}

int PIOc_put_var1_ulonglong(int ncid, int varid, const PIO_Offset *index,
                            const unsigned long long *op)
{
  return PIOc_put_var1_ulonglong_impl(ncid, varid, index, op);
}

