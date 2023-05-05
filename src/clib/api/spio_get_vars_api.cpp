#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for reading a hyperslab of strided non-distributed data/variable */
int PIOc_get_vars(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, void *buf)
{
  return PIOc_get_vars_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, char *buf)
{
  return PIOc_get_vars_text_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, signed char *buf)
{
  return PIOc_get_vars_schar_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, short *buf)
{
  return PIOc_get_vars_short_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, int *buf)
{
  return PIOc_get_vars_int_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, long *buf)
{
  return PIOc_get_vars_long_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, float *buf)
{
  return PIOc_get_vars_float_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, double *buf)
{
  return PIOc_get_vars_double_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, unsigned char *buf)
{
  return PIOc_get_vars_uchar_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, unsigned short *buf)
{
  return PIOc_get_vars_ushort_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, unsigned int *buf)
{
  return PIOc_get_vars_uint_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, long long *buf)
{
  return PIOc_get_vars_longlong_impl(ncid, varid, start, count, stride, buf);
}

int PIOc_get_vars_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            unsigned long long *buf)
{
  return PIOc_get_vars_ulonglong_impl(ncid, varid, start, count, stride, buf);
}
