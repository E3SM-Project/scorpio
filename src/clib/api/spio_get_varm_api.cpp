#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for reading/writing a hyperslab of strided non-distributed data/variable
 * with a mapped array. The mapped array maps between memory and variable data
 */
/* Varm functions are deprecated and should be used with extreme
 * caution or not at all. Varm functions are not supported in
 * async mode. */
int PIOc_get_varm_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, signed char *buf)
{
  return PIOc_get_varm_schar_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, short *buf)
{
  return PIOc_get_varm_short_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_ulonglong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                            const PIO_Offset *stride, const PIO_Offset *imap, unsigned long long *buf)
{
  return PIOc_get_varm_ulonglong_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, unsigned short *buf)
{
  return PIOc_get_varm_ushort_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const PIO_Offset *imap, long long *buf)
{
  return PIOc_get_varm_longlong_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, double *buf)
{
  return PIOc_get_varm_double_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, char *buf)
{
  return PIOc_get_varm_text_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, const PIO_Offset *imap, int *buf)
{
  return PIOc_get_varm_int_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, unsigned int *buf)
{
  return PIOc_get_varm_uint_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const PIO_Offset *imap, void *buf,
                  PIO_Offset bufcount, MPI_Datatype buftype)
{
  return PIOc_get_varm_impl(ncid, varid, start, count, stride, imap, buf, bufcount, buftype);
}

int PIOc_get_varm_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, float *buf)
{
  return PIOc_get_varm_float_impl(ncid, varid, start, count, stride, imap, buf);
}

int PIOc_get_varm_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, long *buf)
{
  return PIOc_get_varm_long_impl(ncid, varid, start, count, stride, imap, buf);
}
