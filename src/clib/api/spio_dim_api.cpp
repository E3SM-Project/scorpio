#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* ================= File meta-data APIs ============== */
/* APIs for variable dimensions */
int PIOc_inq_dim(int ncid, int dimid, char *name, PIO_Offset *lenp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_dim");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("dimid", dimid).
    add_arg("name", name).add_arg("*lenp", lenp).flush();
#endif
  ret = PIOc_inq_dim_impl(ncid, dimid, name, lenp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*name", name).add_rval("*lenp", lenp);
#endif
  return ret;
}

int PIOc_inq_dimid(int ncid, const char *name, int *idp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_dimid");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("name", name).add_arg("*idp", idp).flush();
#endif
  ret = PIOc_inq_dimid_impl(ncid, name, idp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*idp", idp);
#endif
  return ret;
}

int PIOc_inq_dimname(int ncid, int dimid, char *name)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_dimname");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("dimid", dimid).
    add_arg("name", static_cast<void *>(name)).flush();
#endif
  ret = PIOc_inq_dimname_impl(ncid, dimid, name);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*name", name);
#endif
  return ret;
}

int PIOc_inq_dimlen(int ncid, int dimid, PIO_Offset *lenp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_dimlen");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("dimid", dimid).
    add_arg("*lenp", lenp).flush();
#endif
  ret = PIOc_inq_dimlen_impl(ncid, dimid, lenp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*lenp", static_cast<long long int>(*lenp));
#endif
  return ret;
}

int PIOc_rename_dim(int ncid, int dimid, const char *name)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_rename_dim");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("dimid", dimid).
    add_arg("*name", name).flush();
#endif
  return PIOc_rename_dim_impl(ncid, dimid, name);
}

int PIOc_def_dim(int ncid, const char *name, PIO_Offset len, int *idp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_def_dim");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("*name", name).
    add_arg("len", static_cast<long long int>(len)).
    add_arg("*idp", idp).flush();
#endif
  ret = PIOc_def_dim_impl(ncid, name, len, idp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*idp", idp);
#endif
  return ret;
}
