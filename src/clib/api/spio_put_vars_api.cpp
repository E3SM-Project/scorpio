#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for writing a hyperslab of strided non-distributed data/variable */
int PIOc_put_vars(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const void *buf)
{
  return PIOc_put_vars_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_put_vars_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride, const char *op)
{
  return PIOc_put_vars_text_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_schar(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride,
                        const signed char *op)
{
  return PIOc_put_vars_schar_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_short(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride, const short *op)
{
  return PIOc_put_vars_short_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, const int *op)
{
  return PIOc_put_vars_int_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const float *op)
{
  return PIOc_put_vars_float_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_double(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, const PIO_Offset *stride, const double *op)
{
  return PIOc_put_vars_double_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const long *op)
{
  return PIOc_put_vars_long_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const unsigned char *op)
{
  return PIOc_put_vars_uchar_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const unsigned short *op)
{
  return PIOc_put_vars_ushort_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const unsigned int *op)
{
  return PIOc_put_vars_uint_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const long long *op)
{
  return PIOc_put_vars_longlong_impl(ncid, varid, start, count, stride, op);
}

int PIOc_put_vars_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            const unsigned long long *op)
{
  return PIOc_put_vars_ulonglong_impl(ncid, varid, start, count, stride, op);
}
