#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* ================= File meta-data APIs ============== */
/* APIs for file/variable attributes */
int PIOc_rename_att(int ncid, int varid, const char *name, const char *newname)
{
  return PIOc_rename_att_impl(ncid, varid, name, newname);
}

int PIOc_del_att(int ncid, int varid, const char *name)
{
  return PIOc_del_att_impl(ncid, varid, name);
}

int PIOc_inq_att(int ncid, int varid, const char *name, nc_type *xtypep,
                 PIO_Offset *lenp)
{
  return PIOc_inq_att_impl(ncid, varid, name, xtypep, lenp);
}

int PIOc_inq_attid(int ncid, int varid, const char *name, int *idp)
{
  return PIOc_inq_attid_impl(ncid, varid, name, idp);
}

int PIOc_inq_attlen(int ncid, int varid, const char *name, PIO_Offset *lenp)
{
  return PIOc_inq_attlen_impl(ncid, varid, name, lenp);
}

int PIOc_inq_atttype(int ncid, int varid, const char *name, nc_type *xtypep)
{
  return PIOc_inq_atttype_impl(ncid, varid, name, xtypep);
}

int PIOc_inq_attname(int ncid, int varid, int attnum, char *name)
{
  return PIOc_inq_attname_impl(ncid, varid, attnum, name);
}

int PIOc_copy_att(int incid, int ivarid, const char *name, int oncid, int ovarid)
{
  return PIOc_copy_att_impl(incid, ivarid, name, oncid, ovarid);
}
