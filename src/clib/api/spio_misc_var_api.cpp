#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* APIs for file variables */
int PIOc_inq_varid(int ncid, const char *name, int *varidp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_varid");
  tr.set_file_id(ncid).add_arg("ncid", ncid).
    add_arg("*name", name).add_arg("*varidp", varidp).flush();
#endif
  ret = PIOc_inq_varid_impl(ncid, name, varidp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*varidp", (varidp) ? (*varidp) : -1);
#endif
  return ret;
}

int PIOc_inq_var(int ncid, int varid, char *name, int namelen, nc_type *xtypep, int *ndimsp,
                 int *dimidsp, int *nattsp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_var");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*name", name).add_arg("namelen", namelen).
    add_arg("*xtypep", xtypep).add_arg("*ndimsp", ndimsp).
    add_arg("*dimidsp", dimidsp).add_arg("*nattsp", nattsp).flush();
#endif
  ret = PIOc_inq_var_impl(ncid, varid, name, namelen, xtypep, ndimsp,
                           dimidsp, nattsp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*name", (namelen > 0) ? name : "").
    add_rval("*xtypep", (xtypep) ? (static_cast<int>(*xtypep)) : 0).
    add_rval("*ndimsp", (ndimsp) ? (*ndimsp) : 0).
    add_rval("*nattsp", (nattsp) ? (*nattsp) : 0);

  if(ndimsp && (*ndimsp > 0) && dimidsp){
    tr.add_rval("*dimidsp", dimidsp, *ndimsp);
  }
#endif

  return ret;
}

int PIOc_inq_varname(int ncid, int varid, char *name, int namelen)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_varname");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*name", static_cast<void *>(name)).add_arg("namelen", namelen).flush();
#endif
  ret = PIOc_inq_varname_impl(ncid, varid, name, namelen);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*name", name);
#endif
  return ret;
}

int PIOc_inq_vartype(int ncid, int varid, nc_type *xtypep)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_vartype");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*xtypep", xtypep).flush();
#endif
  ret = PIOc_inq_vartype_impl(ncid, varid, xtypep);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*xtypep", (xtypep) ? (static_cast<int>(*xtypep)) : 0);
#endif
  return ret;
}

int PIOc_inq_varndims(int ncid, int varid, int *ndimsp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_varndims");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*ndimsp", ndimsp).flush();
#endif
  ret = PIOc_inq_varndims_impl(ncid, varid, ndimsp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ndimsp", (ndimsp) ? (*ndimsp) : 0);
#endif
  return ret;
}

int PIOc_inq_vardimid(int ncid, int varid, int *dimidsp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_vardimid");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*dimidsp", dimidsp).flush();
#endif
  ret = PIOc_inq_vardimid_impl(ncid, varid, dimidsp);

#if SPIO_ENABLE_API_TRACING
  int ndims = 0;
  int rval = PIOc_inq_varndims(ncid, varid, &ndims);
  if((rval == PIO_NOERR) && dimidsp){
    tr.add_rval("*dimidsp", dimidsp, ndims);
  }
#endif
  return ret;
}

int PIOc_inq_varnatts(int ncid, int varid, int *nattsp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_varnatts");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*nattsp", nattsp).flush();
#endif
  ret = PIOc_inq_varnatts_impl(ncid, varid, nattsp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*nattsp", (nattsp) ? (*nattsp) : 0);
#endif
  return ret;
}

int PIOc_def_var(int ncid, const char *name, nc_type xtype,  int ndims,
                 const int *dimidsp, int *varidp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  assert(ndims >= 0);
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_def_var");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("*name", name).
    add_arg("xtype", static_cast<long long int>(xtype)).
    add_arg("ndims", ndims).add_arg("*dimidsp", dimidsp, ndims).
    add_arg("*varidp", varidp).flush();
#endif
  ret = PIOc_def_var_impl(ncid, name, xtype,  ndims, dimidsp, varidp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*varidp", (varidp) ? (*varidp) : -1);
#endif
  return ret;
}

int PIOc_set_fill(int ncid, int fillmode, int *old_modep)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_set_fill");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("fillmode", fillmode).
    add_arg("*old_modep", old_modep).flush();
#endif
  ret = PIOc_set_fill_impl(ncid, fillmode, old_modep);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*old_modep", (old_modep) ? (*old_modep) : -1);
#endif
  return ret;
}

int PIOc_def_var_fill(int ncid, int varid, int no_fill, const void *fill_value)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_def_var_fill");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("no_fill", no_fill).add_arg("*fill_value", fill_value).flush();
#endif
  return PIOc_def_var_fill_impl(ncid, varid, no_fill, fill_value);
}

int PIOc_inq_var_fill(int ncid, int varid, int *no_fill, void *fill_valuep)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_var_fill");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*no_fill", no_fill).add_arg("*fill_valuep", fill_valuep).flush();
#endif
  return PIOc_inq_var_fill_impl(ncid, varid, no_fill, fill_valuep);
}

int PIOc_rename_var(int ncid, int varid, const char *name)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_rename_var");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*name", name).flush();
#endif
  return PIOc_rename_var_impl(ncid, varid, name);
}

/* APIs for data/variable compression. These settings only apply to netCDF-4 files for now */
int PIOc_def_var_deflate(int ncid, int varid, int shuffle, int deflate,
                         int deflate_level)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_def_var_deflate");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("shuffle", shuffle).add_arg("deflate", deflate).
    add_arg("deflate_level", deflate_level).flush();
#endif
  return PIOc_def_var_deflate_impl(ncid, varid, shuffle, deflate, deflate_level);
}

int PIOc_inq_var_deflate(int ncid, int varid, int *shufflep, int *deflatep,
                         int *deflate_levelp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_var_deflate");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*shufflep", shufflep).add_arg("*deflatep", deflatep).
    add_arg("*deflate_levelp", deflate_levelp).flush();
#endif
  ret = PIOc_inq_var_deflate_impl(ncid, varid, shufflep, deflatep, deflate_levelp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*shufflep", (shufflep) ? (*shufflep) : -1).
    add_rval("*deflatep", (deflatep) ? (*deflatep) : -1).
    add_rval("*deflate_levelp", (deflate_levelp) ? (*deflate_levelp) : -1);
#endif
  return ret;
}

/* PIOc_inq_var_szip() is not implemented
int PIOc_inq_var_szip(int ncid, int varid, int *options_maskp, int *pixels_per_blockp)
{
  return PIOc_inq_var_szip_impl(ncid, varid, options_maskp, pixels_per_blockp);
}
*/

int PIOc_def_var_chunking(int ncid, int varid, int storage, const PIO_Offset *chunksizesp)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_def_var_chunking");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("storage", storage).add_arg("*chunksizesp", chunksizesp).flush();
#endif
  return PIOc_def_var_chunking_impl(ncid, varid, storage, chunksizesp);
}

int PIOc_inq_var_chunking(int ncid, int varid, int *storagep, PIO_Offset *chunksizesp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_var_chunking");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*storagep", storagep).add_arg("*chunksizesp", chunksizesp).flush();
#endif
  ret = PIOc_inq_var_chunking_impl(ncid, varid, storagep, chunksizesp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*storagep", (storagep) ? (*storagep) : -1).
    add_rval("*chunksizesp", (chunksizesp) ? (*chunksizesp) : -1);
#endif
  return ret;
}

int PIOc_def_var_endian(int ncid, int varid, int endian)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_def_var_endian");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("endian", endian).flush();
#endif
  return PIOc_def_var_endian_impl(ncid, varid, endian);
}

int PIOc_inq_var_endian(int ncid, int varid, int *endianp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_inq_var_endian");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*endianp", endianp).flush();
#endif
  ret = PIOc_inq_var_endian_impl(ncid, varid, endianp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*endianp", (endianp) ? (*endianp) : -1);
#endif
  return ret;
}

int PIOc_set_var_chunk_cache(int ncid, int varid, PIO_Offset size, PIO_Offset nelems,
                             float preemption)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_set_var_chunk_cache");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("size", static_cast<long long int>(size)).
    add_arg("nelems", static_cast<long long int>(nelems)).
    add_arg("preemption", preemption).flush();
#endif
  return PIOc_set_var_chunk_cache_impl(ncid, varid, size, nelems, preemption);
}

int PIOc_get_var_chunk_cache(int ncid, int varid, PIO_Offset *sizep, PIO_Offset *nelemsp,
                             float *preemptionp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_get_var_chunk_cache");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("varid", varid).
    add_arg("*sizep", sizep).add_arg("*nelemsp", nelemsp).
    add_arg("*preemptionp", preemptionp).flush();
#endif
  ret = PIOc_get_var_chunk_cache_impl(ncid, varid, sizep, nelemsp, preemptionp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*sizep", (sizep) ? (static_cast<long long int>(*sizep)) : 0).
    add_rval("*nelemsp", (nelemsp) ? (static_cast<long long int>(*nelemsp)) : 0).
    add_rval("*preemptionp", (preemptionp) ? (*preemptionp) : 0);
#endif
  return ret;
}
