#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* ================= File meta-data APIs ============== */
int PIOc_redef(int ncid)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_redef");
  tr.set_file_id(ncid).add_arg("ncid", ncid).flush();
  return PIOc_redef_impl(ncid);
}

int PIOc_enddef(int ncid)
{
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_enddef");
  tr.set_file_id(ncid).add_arg("ncid", ncid).flush();
  return PIOc_enddef_impl(ncid);
}

int PIOc_inq_format(int ncid, int *formatp)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_format");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*formatp", formatp).flush();
  ret = PIOc_inq_format_impl(ncid, formatp);

  tr.add_rval("*formatp", formatp);
  return ret;
}

int PIOc_inq(int ncid, int *ndimsp, int *nvarsp, int *ngattsp, int *unlimdimidp)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*ndimsp", ndimsp).add_arg("*nvarsp", nvarsp).
    add_arg("*ngattsp", ngattsp).add_arg("*unlimdimidp", unlimdimidp).flush();
  ret = PIOc_inq_impl(ncid, ndimsp, nvarsp, ngattsp, unlimdimidp);

  tr.add_rval("*ndimsp", ndimsp).add_rval("*nvarsp", nvarsp).
    add_rval("*ngattsp", ngattsp).add_rval("*unlimdimidp", unlimdimidp);
  return ret;
}

int PIOc_inq_ndims(int ncid, int *ndimsp)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_ndims");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*ndimsp", ndimsp).flush();
  ret = PIOc_inq_ndims_impl(ncid, ndimsp);

  tr.add_rval("*ndimsp", (ndimsp) ? (*ndimsp) : 0);
  return ret;
}

int PIOc_inq_nvars(int ncid, int *nvarsp)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_nvars");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*nvarsp", nvarsp).flush();
  ret = PIOc_inq_nvars_impl(ncid, nvarsp);

  tr.add_rval("*nvarsp", (nvarsp) ? (*nvarsp) : 0);
  return ret;
}

int PIOc_inq_natts(int ncid, int *ngattsp)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_natts");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*ngattsp", ngattsp).flush();
  ret = PIOc_inq_natts_impl(ncid, ngattsp);

  tr.add_rval("*ngattsp", (ngattsp) ? (*ngattsp) : 0);
  return ret;
}

int PIOc_inq_unlimdim(int ncid, int *unlimdimidp)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_unlimdim");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*unlimdimidp", unlimdimidp).flush();
  ret = PIOc_inq_unlimdim_impl(ncid, unlimdimidp);

  tr.add_rval("*unlimdimidp", (unlimdimidp) ? (*unlimdimidp) : -1);
  return ret;
}

int PIOc_inq_unlimdims(int ncid, int *nunlimdimsp, int *unlimdimidsp)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_unlimdims");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*nunlimdimsp", nunlimdimsp).
    add_arg("*unlimdimidsp", unlimdimidsp).flush();
  ret = PIOc_inq_unlimdims_impl(ncid, nunlimdimsp, unlimdimidsp);

  tr.add_rval("*nunlimdimsp", (nunlimdimsp) ? (*nunlimdimsp) : 0);
  if(nunlimdimsp && (*nunlimdimsp > 0)){
    tr.add_rval("*unlimdimidp", unlimdimidsp, *nunlimdimsp);
  }
  return ret;
}

int PIOc_inq_type(int ncid, nc_type xtype, char *name, PIO_Offset *sizep)
{
  int ret = PIO_NOERR;

  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_type");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("xtype", static_cast<int>(xtype)).
    add_arg("*name", name).add_arg("*sizep", sizep).flush();
  ret = PIOc_inq_type_impl(ncid, xtype, name, sizep);

  tr.add_rval("*name", name).add_rval("*sizep", sizep);
  return ret;
}
