#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* ================= File meta-data APIs ============== */
/* APIs for variable dimensions */
int PIOc_inq_dim(int ncid, int dimid, char *name, PIO_Offset *lenp)
{
  return PIOc_inq_dim_impl(ncid, dimid, name, lenp);
}

int PIOc_inq_dimid(int ncid, const char *name, int *idp)
{
  return PIOc_inq_dimid_impl(ncid, name, idp);
}

int PIOc_inq_dimname(int ncid, int dimid, char *name)
{
  return PIOc_inq_dimname_impl(ncid, dimid, name);
}

int PIOc_inq_dimlen(int ncid, int dimid, PIO_Offset *lenp)
{
  return PIOc_inq_dimlen_impl(ncid, dimid, lenp);
}

int PIOc_rename_dim(int ncid, int dimid, const char *name)
{
  return PIOc_rename_dim_impl(ncid, dimid, name);
}

int PIOc_def_dim(int ncid, const char *name, PIO_Offset len, int *idp)
{
  return PIOc_def_dim_impl(ncid, name, len, idp);
}
