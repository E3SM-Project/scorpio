#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* ================= File meta-data APIs ============== */
int PIOc_redef(int ncid)
{
  return PIOc_redef_impl(ncid);
}

int PIOc_enddef(int ncid)
{
  return PIOc_enddef_impl(ncid);
}

int PIOc_inq_format(int ncid, int *formatp)
{
  return PIOc_inq_format_impl(ncid, formatp);
}

int PIOc_inq(int ncid, int *ndimsp, int *nvarsp, int *ngattsp, int *unlimdimidp)
{
  return PIOc_inq_impl(ncid, ndimsp, nvarsp, ngattsp, unlimdimidp);
}

int PIOc_inq_ndims(int ncid, int *ndimsp)
{
  return PIOc_inq_ndims_impl(ncid, ndimsp);
}

int PIOc_inq_nvars(int ncid, int *nvarsp)
{
  return PIOc_inq_nvars_impl(ncid, nvarsp);
}

int PIOc_inq_natts(int ncid, int *ngattsp)
{
  return PIOc_inq_natts_impl(ncid, ngattsp);
}

int PIOc_inq_unlimdim(int ncid, int *unlimdimidp)
{
  return PIOc_inq_unlimdim_impl(ncid, unlimdimidp);
}

int PIOc_inq_unlimdims(int ncid, int *nunlimdimsp, int *unlimdimidsp)
{
  return PIOc_inq_unlimdims_impl(ncid, nunlimdimsp, unlimdimidsp);
}

int PIOc_inq_type(int ncid, nc_type xtype, char *name, PIO_Offset *sizep)
{
  return PIOc_inq_type_impl(ncid, xtype, name, sizep);
}
