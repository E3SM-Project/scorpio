#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for reading a hyperslab of non-distributed data/variable */
int PIOc_get_vara(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count, void *buf)
{
  return PIOc_get_vara_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       char *buf)
{
  return PIOc_get_vara_text_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        signed char *buf)
{
  return PIOc_get_vara_schar_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        short *buf)
{
  return PIOc_get_vara_short_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      int *buf)
{
  return PIOc_get_vara_int_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        float *buf)
{
  return PIOc_get_vara_float_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       long *buf)
{
  return PIOc_get_vara_long_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_double(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, double *buf)
{
  return PIOc_get_vara_double_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        unsigned char *buf)
{
  return PIOc_get_vara_uchar_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         unsigned short *buf)
{
  return PIOc_get_vara_ushort_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       unsigned int *buf)
{
  return PIOc_get_vara_uint_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           long long *buf)
{
  return PIOc_get_vara_longlong_impl(ncid, varid, start, count, buf);
}

int PIOc_get_vara_ulonglong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                            unsigned long long *buf)
{
  return PIOc_get_vara_ulonglong_impl(ncid, varid, start, count, buf);
}
