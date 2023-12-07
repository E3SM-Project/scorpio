#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* ================= File meta-data APIs ============== */
/* APIs for file/variable attributes */
int PIOc_rename_att(int ncid, int varid, const char *name, const char *newname)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_rename_att");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*newname", newname).flush();
  return PIOc_rename_att_impl(ncid, varid, name, newname);
}

int PIOc_del_att(int ncid, int varid, const char *name)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_del_att");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).flush();
  return PIOc_del_att_impl(ncid, varid, name);
}

int PIOc_inq_att(int ncid, int varid, const char *name, nc_type *xtypep,
                 PIO_Offset *lenp)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_att");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*xtypep", xtypep).add_arg("*lenp", lenp).flush();
  ret = PIOc_inq_att_impl(ncid, varid, name, xtypep, lenp);

  tr.add_rval("*xtypep", (xtypep) ? (static_cast<int>(*xtypep)) : -1).
    add_rval("*lenp", (lenp) ? (static_cast<long long int>(*lenp)) : -1);
  return ret;
}

int PIOc_inq_attid(int ncid, int varid, const char *name, int *idp)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_attid");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*idp", idp).flush();
  ret = PIOc_inq_attid_impl(ncid, varid, name, idp);

  tr.add_rval("*idp", (idp) ? (*idp) : -1);
  return ret;
}

int PIOc_inq_attlen(int ncid, int varid, const char *name, PIO_Offset *lenp)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_attlen");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*lenp", lenp).flush();
  ret = PIOc_inq_attlen_impl(ncid, varid, name, lenp);

  tr.add_rval("*lenp", (lenp) ? (static_cast<long long int>(*lenp)) : -1);
  return ret;
}

int PIOc_inq_atttype(int ncid, int varid, const char *name, nc_type *xtypep)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_atttype");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("*name", name).
    add_arg("*xtypep", xtypep).flush();
  ret = PIOc_inq_atttype_impl(ncid, varid, name, xtypep);

  tr.add_rval("*xtypep", (xtypep) ? (static_cast<int>(*xtypep)) : -1);
  return ret;
}

int PIOc_inq_attname(int ncid, int varid, int attnum, char *name)
{
  int ret = PIO_NOERR;
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_attname");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("varid", varid).add_arg("attnum", attnum).
    add_arg("*name", static_cast<void *>(name)).flush();
  ret = PIOc_inq_attname_impl(ncid, varid, attnum, name);

  tr.add_rval("*name", name);
  return ret;
}

int PIOc_copy_att(int incid, int ivarid, const char *name, int oncid, int ovarid)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_copy_att");
  tr.set_file_id(incid).add_arg("incid", incid).
    add_arg("ivarid", ivarid).add_arg("*name", name).
    add_arg("oncid", oncid).add_arg("ovarid", ovarid).flush();
  return PIOc_copy_att_impl(incid, ivarid, name, oncid, ovarid);
}
