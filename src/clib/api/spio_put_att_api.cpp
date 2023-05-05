#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* Write APIs for file/variable attributes */
int PIOc_put_att(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len, const void *op)
{
  return PIOc_put_att_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_text(int ncid, int varid, const char *name, PIO_Offset len, const char *op)
{
  return PIOc_put_att_text_impl(ncid, varid, name, len, op);
}

int PIOc_put_att_schar(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const signed char *op)
{
  return PIOc_put_att_schar_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_short(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const short *op)
{
  return PIOc_put_att_short_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_int(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                     const int *op)
{
  return PIOc_put_att_int_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_long(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                      const long *op)
{
  return PIOc_put_att_long_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_float(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const float *op)
{
  return PIOc_put_att_float_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_double(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                        const double *op)
{
  return PIOc_put_att_double_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_uchar(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const unsigned char *op)
{
  return PIOc_put_att_uchar_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_ushort(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                        const unsigned short *op)
{
  return PIOc_put_att_ushort_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_uint(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                      const unsigned int *op)
{
  return PIOc_put_att_uint_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_longlong(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                          const long long *op)
{
  return PIOc_put_att_longlong_impl(ncid, varid, name, xtype, len, op);
}

int PIOc_put_att_ulonglong(int ncid, int varid, const char *name, nc_type xtype,
                           PIO_Offset len, const unsigned long long *op)
{
  return PIOc_put_att_ulonglong_impl(ncid, varid, name, xtype, len, op);
}
