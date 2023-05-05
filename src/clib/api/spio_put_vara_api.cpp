#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for writing a hyperslab of non-distributed data/variable */
int PIOc_put_vara(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const void *buf)
{
  return PIOc_put_vara_impl(ncid, varid, start, count, buf);
}

int PIOc_put_vara_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const char *op)
{
  return PIOc_put_vara_text_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_schar(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const signed char *op)
{
  return PIOc_put_vara_schar_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const short *op)
{
  return PIOc_put_vara_short_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const int *op)
{
  return PIOc_put_vara_int_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const long *op)
{
  return PIOc_put_vara_long_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_float(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const float *op)
{
  return PIOc_put_vara_float_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const double *op)
{
  return PIOc_put_vara_double_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const unsigned char *op)
{
  return PIOc_put_vara_uchar_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_ushort(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, const unsigned short *op)
{
  return PIOc_put_vara_ushort_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_uint(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const unsigned int *op)
{
  return PIOc_put_vara_uint_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_longlong(int ncid, int varid, const PIO_Offset *start,
                           const PIO_Offset *count, const long long *op)
{
  return PIOc_put_vara_longlong_impl(ncid, varid, start, count, op);
}

int PIOc_put_vara_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const unsigned long long *op)
{
  return PIOc_put_vara_ulonglong_impl(ncid, varid, start, count, op);
}
