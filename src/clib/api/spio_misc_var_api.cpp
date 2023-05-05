#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* APIs for file variables */
int PIOc_inq_varid(int ncid, const char *name, int *varidp)
{
  return PIOc_inq_varid_impl(ncid, name, varidp);
}

int PIOc_inq_var(int ncid, int varid, char *name, int namelen, nc_type *xtypep, int *ndimsp,
                 int *dimidsp, int *nattsp)
{
  return PIOc_inq_var_impl(ncid, varid, name, namelen, xtypep, ndimsp,
                           dimidsp, nattsp);
}

int PIOc_inq_varname(int ncid, int varid, char *name, int namelen)
{
  return PIOc_inq_varname_impl(ncid, varid, name, namelen);
}

int PIOc_inq_vartype(int ncid, int varid, nc_type *xtypep)
{
  return PIOc_inq_vartype_impl(ncid, varid, xtypep);
}

int PIOc_inq_varndims(int ncid, int varid, int *ndimsp)
{
  return PIOc_inq_varndims_impl(ncid, varid, ndimsp);
}

int PIOc_inq_vardimid(int ncid, int varid, int *dimidsp)
{
  return PIOc_inq_vardimid_impl(ncid, varid, dimidsp);
}

int PIOc_inq_varnatts(int ncid, int varid, int *nattsp)
{
  return PIOc_inq_varnatts_impl(ncid, varid, nattsp);
}

int PIOc_def_var(int ncid, const char *name, nc_type xtype,  int ndims,
                 const int *dimidsp, int *varidp)
{
  return PIOc_def_var_impl(ncid, name, xtype,  ndims, dimidsp, varidp);
}

int PIOc_set_fill(int ncid, int fillmode, int *old_modep)
{
  return PIOc_set_fill_impl(ncid, fillmode, old_modep);
}

int PIOc_def_var_fill(int ncid, int varid, int no_fill, const void *fill_value)
{
  return PIOc_def_var_fill_impl(ncid, varid, no_fill, fill_value);
}

int PIOc_inq_var_fill(int ncid, int varid, int *no_fill, void *fill_valuep)
{
  return PIOc_inq_var_fill_impl(ncid, varid, no_fill, fill_valuep);
}

int PIOc_rename_var(int ncid, int varid, const char *name)
{
  return PIOc_rename_var_impl(ncid, varid, name);
}

/* APIs for data/variable compression. These settings only apply to netCDF-4 files for now */
int PIOc_def_var_deflate(int ncid, int varid, int shuffle, int deflate,
                         int deflate_level)
{
  return PIOc_def_var_deflate_impl(ncid, varid, shuffle, deflate, deflate_level);
}

int PIOc_inq_var_deflate(int ncid, int varid, int *shufflep, int *deflatep,
                         int *deflate_levelp)
{
  return PIOc_inq_var_deflate_impl(ncid, varid, shufflep, deflatep, deflate_levelp);
}

/* PIOc_inq_var_szip() is not implemented
int PIOc_inq_var_szip(int ncid, int varid, int *options_maskp, int *pixels_per_blockp)
{
  return PIOc_inq_var_szip_impl(ncid, varid, options_maskp, pixels_per_blockp);
}
*/

int PIOc_def_var_chunking(int ncid, int varid, int storage, const PIO_Offset *chunksizesp)
{
  return PIOc_def_var_chunking_impl(ncid, varid, storage, chunksizesp);
}

int PIOc_inq_var_chunking(int ncid, int varid, int *storagep, PIO_Offset *chunksizesp)
{
  return PIOc_inq_var_chunking_impl(ncid, varid, storagep, chunksizesp);
}

int PIOc_def_var_endian(int ncid, int varid, int endian)
{
  return PIOc_def_var_endian_impl(ncid, varid, endian);
}

int PIOc_inq_var_endian(int ncid, int varid, int *endianp)
{
  return PIOc_inq_var_endian_impl(ncid, varid, endianp);
}

int PIOc_set_var_chunk_cache(int ncid, int varid, PIO_Offset size, PIO_Offset nelems,
                             float preemption)
{
  return PIOc_set_var_chunk_cache_impl(ncid, varid, size, nelems, preemption);
}

int PIOc_get_var_chunk_cache(int ncid, int varid, PIO_Offset *sizep, PIO_Offset *nelemsp,
                             float *preemptionp)
{
  return PIOc_get_var_chunk_cache_impl(ncid, varid, sizep, nelemsp, preemptionp);
}
