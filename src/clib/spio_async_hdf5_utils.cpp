/** @file
 * Utility functions for Asynchronous I/O using HDF5
 */

#include <iostream>
#include <cassert>
#include <cstring>
#include <string>
#include <numeric>
#include <functional>

extern "C"{

#include <pio_config.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#if PIO_ENABLE_LOGGING
#include <unistd.h>
#endif /* PIO_ENABLE_LOGGING */
#include <pio.h>
} // extern "C"
#if PIO_USE_ASYNC_WR_THREAD
#include "spio_async_tpool_cint.h"
#endif
#include "pio_timer.h"
#include <pio_internal.h>
#include "spio_async_utils.hpp"
#include "spio_async_tpool.hpp"
#include "spio_file_mvcache.h"
#include "spio_dbg_utils.hpp"
#include "spio_dt_converter.hpp"
#include "spio_hdf5_utils.hpp"
#include "spio_async_tpool_cint.h"

struct Hdf5_create_info{
  file_desc_t *file;
  std::string fname;
};

struct Hdf5_def_var_info{
  file_desc_t *file;
  std::string vname;
  nc_type xtype;
  int ndims;
  std::vector<int> dimids;
  int varid;
};

struct Hdf5_wcache{
  file_desc_t *file;
  int nvars;
  int fndims;
  std::vector<int> varids;
  io_desc_t *iodesc;
  std::vector<int> frame;

  bool wr_fillbuf;
  void *iobuf;
  std::size_t iobuf_sz;
  void *fillbuf;
  std::size_t fillbuf_sz;
};

/* Global vars */
std::atomic<int> SPIO_Util::GVars::npend_hdf5_async_ops;

#ifdef _HDF5
namespace Util{
  /* Each variable writes out one or more regions of data. However all variables have the
   * same I/O decomposition and write the same regions (within the variable) of data. The
   * region info is the same across all variables
   * Note: Different variables (among nvars variables) could be writing out different
   * timesteps/records (while still writing the same region - within the record)
   */
  struct RInfo{
    /* The starts(:) for this region - local to this process */
    std::vector<hsize_t> starts;
    /* The counts(:) for this region - local to this process */
    std::vector<hsize_t> counts;
    /* Total number of elements written out, locally, for this region */
    std::size_t nelems;
  };
} // namespace Util

/* Update the the start frame for all regions in reg_infos, based on the variable's current frame */
static inline void update_reg_infos_start_frame(std::vector<Util::RInfo> &reg_infos, const var_desc_t *vdesc, const int frame)
{
  /* If the variable has records/frames then update the starts(:) to the provided frame/record number */
  if(vdesc->record >= 0){
    for(std::vector<Util::RInfo>::iterator iter = reg_infos.begin(); iter != reg_infos.end(); ++iter){
      assert(iter->starts.size() > 0);
      iter->starts[0] = frame;
    }
  }
}

void spio_iosys_async_op_hdf5_create_free(void *pdata)
{
  if(pdata){
    delete(static_cast<Hdf5_create_info *>(pdata));
  }
}

int spio_iosys_async_op_hdf5_create(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_create_info *cinfo = static_cast<Hdf5_create_info *>(pdata);

  assert(cinfo && cinfo->file && cinfo->file->iosystem);

  ret = spio_hdf5_create(cinfo->file->iosystem, cinfo->file, cinfo->fname.c_str());

  cinfo->file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return ret;
}

int spio_iosys_async_hdf5_create_op_add(file_desc_t *file, const char *filename)
{
  int ret = PIO_NOERR;

  assert(file && file->iosystem && filename);

  iosystem_desc_t *ios = file->iosystem;

  if(!ios->ioproc){
    return PIO_NOERR;
  }

  Hdf5_create_info *cinfo = new Hdf5_create_info{file, filename};

  /* Create async task */
  pio_async_op_t *pnew = static_cast<pio_async_op_t *>(calloc(1, sizeof(pio_async_op_t)));
  if(pnew == NULL){
    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                      "Queuing asynchronous op/task for creating file (%s) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for async task internal struct", filename, static_cast<long long int>(sizeof(pio_async_op_t)));
  }

  pnew->op_type = PIO_ASYNC_HDF5_CREATE_OP;
  pnew->pdata = static_cast<void *>(cinfo);
  pnew->wait = spio_iosys_async_op_hdf5_create;
  pnew->poke = pio_async_poke_func_unavail;
  pnew->free = spio_iosys_async_op_hdf5_create_free;

  /* One more pending op using this iodesc & file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(pnew);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation to the thread pool (iosystem = %d). Adding the asynchronous operation failed", ios->iosysid);
  }

  return PIO_NOERR;
}

void spio_iosys_async_op_hdf5_def_var_free(void *pdata)
{
  if(pdata){
    delete(static_cast<Hdf5_def_var_info *>(pdata));
  }
}

int spio_iosys_async_op_hdf5_def_var(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_def_var_info *def_var_info = static_cast<Hdf5_def_var_info *>(pdata);

  assert(def_var_info && def_var_info->file && def_var_info->file->iosystem);

  ret = spio_hdf5_def_var(def_var_info->file->iosystem, def_var_info->file,
          def_var_info->vname.c_str(), def_var_info->xtype, def_var_info->ndims,
          def_var_info->dimids.data(), def_var_info->varid);

  def_var_info->file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return ret;
}

int spio_iosys_async_hdf5_def_var_op_add(file_desc_t *file, const char *name,
      nc_type xtype, int ndims, const int *dimidsp, int varid)
{
  int ret = PIO_NOERR;

  assert(file && file->iosystem && name);

  iosystem_desc_t *ios = file->iosystem;

  if(!ios->ioproc){
    return PIO_NOERR;
  }

  Hdf5_def_var_info *def_var_info = new Hdf5_def_var_info{file, name, xtype, ndims,
                                  {dimidsp, dimidsp + ndims}, varid};

  /* Create async task */
  pio_async_op_t *pnew = static_cast<pio_async_op_t *>(calloc(1, sizeof(pio_async_op_t)));
  if(pnew == NULL){
    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                      "Queuing asynchronous op/task for defining variable (%s, file=%s) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for async task internal struct", name, pio_get_fname_from_file(file), static_cast<long long int>(sizeof(pio_async_op_t)));
  }

  pnew->op_type = PIO_ASYNC_HDF5_DEF_VAR_OP;
  pnew->pdata = static_cast<void *>(def_var_info);
  pnew->wait = spio_iosys_async_op_hdf5_def_var;
  pnew->poke = pio_async_poke_func_unavail;
  pnew->free = spio_iosys_async_op_hdf5_def_var_free;

  /* One more pending op using this iodesc & file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(pnew);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation to the thread pool (iosystem = %d). Adding the asynchronous operation failed", ios->iosysid);
  }

  return PIO_NOERR;
}
#endif // _HDF5

int spio_wait_all_hdf5_async_ops(int iosysid)
{
  const int SLEEP_TIME_IN_MILLISECONDS = 500;
  while(SPIO_Util::GVars::npend_hdf5_async_ops > 0){
    std::this_thread::sleep_for(std::chrono::milliseconds(SLEEP_TIME_IN_MILLISECONDS));
    if(SPIO_Util::GVars::npend_hdf5_async_ops > 0){
      PIOc_warn(iosysid, PIO_DEFAULT, __FILE__, __LINE__, "Continuing to wait on all HDF5 async ops...");
    }
  }

  return PIO_NOERR;
}

int pio_iosys_async_op_hdf5_write(void *pdata)
{
#ifdef _HDF5
  /* FIXME: Add futures */
  int ret = PIO_NOERR;
  Hdf5_wcache *wcache = static_cast<struct Hdf5_wcache *>(pdata);
  assert(wcache);

  file_desc_t *file = wcache->file;
  int nvars = wcache->nvars;
  int fndims = wcache->fndims;
  io_desc_t *iodesc = wcache->iodesc;

  assert(file && (nvars > 0) && (fndims > 0) && iodesc);
  assert((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C));

  iosystem_desc_t *ios = file->iosystem;
  var_desc_t *v1desc = file->varlist + wcache->varids[0];

  assert(ios && v1desc && ios->ioproc);

  bool wr_fillbuf = wcache->wr_fillbuf;

  //int num_regions = (wr_fillbuf) ? iodesc->maxfillregions : iodesc->maxregions;
  io_region *region = (wr_fillbuf) ? iodesc->fillregion : iodesc->firstregion;
  PIO_Offset llen = (wr_fillbuf) ? iodesc->holegridsize : iodesc->llen;
  void *iobuf = (wr_fillbuf) ? (wcache->fillbuf) : (wcache->iobuf);
  std::size_t iobuf_sz = (wr_fillbuf) ? (wcache->fillbuf_sz) : (wcache->iobuf_sz);

  assert(region && (nvars * llen * iodesc->mpitype_size == static_cast<PIO_Offset>(iobuf_sz)));
  assert(((iobuf_sz == 0) && !iobuf) || ((iobuf_sz != 0) && iobuf));

  /* Info on all regions */
  std::vector<Util::RInfo> vreg_infos;
  /* Total number of elements written out for all the regions. Since all variables write
   * out the same regions - since they use the same I/O decomposition - the total number
   * of elements written out for each variable will be the same (reg_nelems)
   */
  hsize_t reg_nelems = 0;

  /* Collect region info, starts/counts etc, for all regions */
  while(region){
    std::size_t start[fndims], count[fndims];

    ret = spio_find_start_count(iodesc->ndims, iodesc->dimlen, fndims, v1desc, region, start, count);
    if(ret != PIO_NOERR){
      return pio_err(ios, file, ret, __FILE__, __LINE__,
                      "Writing variables (number of variables = %d) to file (%s, ncid=%d) failed. Internal error finding start/count for the I/O regions written out from the I/O process", nvars, pio_get_fname_from_file(file), file->pio_ncid);
    }

    std::size_t nelems = std::accumulate(count, count + fndims, 1, std::multiplies<std::size_t>());

    /* Note: The region info is tied to the first variable, we need to update the variable specific
     * sections, start(:) based on the record/frame being written out, later
     */
    if(nelems > 0){
      std::vector<hsize_t> tmp_start(start, start + fndims);
      std::vector<hsize_t> tmp_count(count, count + fndims);
      vreg_infos.push_back({tmp_start, tmp_count, nelems});
      reg_nelems += static_cast<hsize_t>(nelems);
    }
    region = region->next;
  }

  std::size_t num_regions = vreg_infos.size();

  /* Write data - one variable at a time (all regions for a variable written out in a single call) */
  var_desc_t *vdesc = NULL;
  void *bufptr = NULL;
  for(int vidx = 0; vidx < nvars; vidx++){
    hid_t file_space_id = H5Dget_space(file->hdf5_vars[wcache->varids[vidx]].hdf5_dataset_id);
    if(file_space_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to make a copy of the dataspace of the dataset associated with variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
    }

    hid_t mem_space_id = H5I_INVALID_HID;
    if(reg_nelems > 0){
      /* Get the var info. */
      vdesc = file->varlist + wcache->varids[vidx];

      /* Update the generic region info with variable-specific info */
      /* If this is a record (or quasi-record) var, set the start for
       * the record dimension. */
      if((fndims > 1) && (vdesc->record > 0)){
        assert(static_cast<int>(wcache->frame.size()) == nvars);
        update_reg_infos_start_frame(vreg_infos, vdesc, wcache->frame[vidx]);
      }
      

      /* Create a hyperslab of all the regions written out for a variable */
      H5S_seloper_t op = H5S_SELECT_SET;
      for(std::size_t ireg = 0; ireg < num_regions; ireg++){
        /* Union hyperslabs of all regions */
        if(H5Sselect_hyperslab(file_space_id, op, static_cast<hsize_t *>(vreg_infos[ireg].starts.data()), NULL, static_cast<hsize_t *>(vreg_infos[ireg].counts.data()), NULL) < 0){
          return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                         "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                         "The low level (HDF5) I/O library call failed to select a hyperslab region for a dataspace copied from the dataset associated with variable (%s, varid=%d)",
                         nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
        }

        op = H5S_SELECT_OR;
      }

      /* Total number of elements across all regions = 
       *  total number of elements written out with this hyperslab = reg_nelems
       */
      mem_space_id = H5Screate_simple(1, &reg_nelems, NULL);
      if(mem_space_id == H5I_INVALID_HID){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new simple dataspace for variable (%s, varid=%d)",
                       nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
      }

      /* Get a pointer to the data. This buffer points to data from all the regions written out for this variable */
      bufptr = (void *)((char *)iobuf + vidx * iodesc->mpitype_size * llen);
    }
    else{
      /* No data, across all regions, to write on this IO task */
      if(H5Sselect_none(file_space_id) < 0){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to reset the selection region for a dataspace copied from the dataset associated with variable (%s, varid=%d)",
                       nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
      }

      mem_space_id = H5Screate(H5S_NULL);
      if(mem_space_id == H5I_INVALID_HID){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new NULL dataspace for variable (%s, varid=%d)",
                       nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
      }

      bufptr = NULL;
    }

    /* Collective write - for all regions in a single variable */
    hid_t mem_type_id = spio_nc_type_to_hdf5_type(iodesc->piotype);
    if(mem_type_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "Unsupported memory type (type=%x) for variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, iodesc->piotype, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
    }

    hid_t file_var_type_id = H5Dget_type(file->hdf5_vars[wcache->varids[vidx]].hdf5_dataset_id);
    if(file_var_type_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "Unable to query the type of variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
    }

    hid_t file_var_ntype_id = H5Tget_native_type(file_var_type_id, H5T_DIR_DEFAULT);
    assert(file_var_ntype_id != H5I_INVALID_HID);

    /* When HDF5 filters (e.g. data compression) are enabled collective writes fail when datatype conversion is required for writing user data.
     * So we manually perform the data conversion here before passing it to HDF5. When filters are not enabled the write might succeed but HDF5
     * might be switching off collective writes (hurts performance) when datatype conversion is required
     * FIXME: Disable datatype conversion when filters are not enabled on the dataset
     */
    void *wbuf = bufptr;
    if((reg_nelems > 0) && !H5Tequal(mem_type_id, file_var_ntype_id)){
      assert(file->dt_converter);
      wbuf = static_cast<SPIO_Util::File_Util::DTConverter *>(file->dt_converter)->convert(iodesc->ioid, bufptr, iodesc->mpitype_size * reg_nelems,
              iodesc->piotype, spio_hdf5_type_to_pio_type(file_var_ntype_id));
      if(wbuf == NULL){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "Unable to convert the type (from %d to %d) of variable (%s, varid=%d)",
                       nvars, pio_get_fname_from_file(file), file->pio_ncid, iodesc->piotype,
                       spio_hdf5_type_to_pio_type(file_var_ntype_id), pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
      }
    }

    if(H5Dwrite(file->hdf5_vars[wcache->varids[vidx]].hdf5_dataset_id, file_var_ntype_id, mem_space_id, file_space_id,
                 file->dxplid_coll, wbuf) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to write the dataset associated with variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
    }

#if SPIO_HDF5_FLUSH_AFTER_COLL_WR
    if(H5Fflush(file->hdf5_file_id, H5F_SCOPE_LOCAL) < 0){
      H5Eprint2(H5E_DEFAULT, stderr);
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to flush the dataset associated with variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
    }
#endif

    if(H5Sclose(file_space_id) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to release a dataspace copied from the dataset associated with variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
    }

    if(H5Sclose(mem_space_id) < 0){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "The low level (HDF5) I/O library call failed to release a simple (or NULL) dataspace for variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
    }
  }

  /* FIXME: Is this barrier needed ? */
  MPI_Barrier(ios->io_comm);

  iodesc->nasync_pend_ops--;
  file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return PIO_NOERR;
#else // _HDF5
  assert(0);
  return PIO_EINTERNAL;
#endif // _HDF5
}

void pio_iosys_async_op_hdf5_write_free(void *pdata)
{
#ifdef _HDF5
  Hdf5_wcache *wcache = static_cast<struct Hdf5_wcache *>(pdata);
  assert(wcache);

  /* Using swap trick to free vectors
   * - swap vector with an empty local/temp vector that gets deallocated when func exits
   */
  //wcache->varids.clear();
  std::vector<int>().swap(wcache->varids);
  //wcache->frame.clear();
  std::vector<int>().swap(wcache->frame);

  if(wcache->iobuf){ brel(wcache->iobuf); }
  if(wcache->fillbuf){ brel(wcache->fillbuf); }

  free(wcache);
#else // _HDF5
  assert(0);
#endif // _HDF5
}

int pio_iosys_async_hdf5_write_op_add(file_desc_t *file, int nvars, int fndims,
      const int *varids, io_desc_t *iodesc, int fill, const int *frame)
{
#ifdef _HDF5
  int ret = PIO_NOERR;

  assert(file && (nvars > 0) && (fndims > 0) && varids && iodesc);

  iosystem_desc_t *ios = file->iosystem;
  assert(ios);

  if(!ios->ioproc){
    return PIO_NOERR;
  }

  std::vector<int> vids(varids, varids + nvars);
  std::vector<int> frms;
  if(frame){
    frms.resize(nvars);
    std::copy(frame, frame + nvars, frms.begin());
  }

  Hdf5_wcache *wcache = static_cast<Hdf5_wcache *>(calloc(1, sizeof(Hdf5_wcache)));
  *wcache = {file, nvars, fndims, vids, iodesc, frms, (fill) ? true : false, NULL, 0, NULL, 0};

  /* We need to copy the iobuf/fillbuf since the mvcache gets reused for future writes */
  /* Copy iobuf/fillvalue */
  var_desc_t *v1desc = file->varlist + varids[0];
  //PIO_Offset llen = fill ? iodesc->holegridsize : iodesc->llen;
  if(fill){
    std::size_t fillbuf_sz = iodesc->holegridsize * iodesc->mpitype_size;
    /* On I/O processes with no data (after rearrangement) to write the fillbuf_sz will be 0 */
    if(fillbuf_sz > 0){
      wcache->fillbuf = bget(fillbuf_sz);
      if(!wcache->fillbuf){
        return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                        "Queuing asynchronous op/task for writing fillvalue for variables (number of variables = %d) to file (%s, ncid=%d) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for caching variable fillvalue", nvars, pio_get_fname_from_file(file), file->pio_ncid, static_cast<long long int>(fillbuf_sz));
      }

      std::memcpy(wcache->fillbuf, v1desc->fillbuf, fillbuf_sz);
      wcache->fillbuf_sz = fillbuf_sz;
    }
  }
  else{
    /* Copy buffer, with rearranged data, for nvars */
    std::size_t iobuf_sz = nvars * iodesc->llen * iodesc->mpitype_size;
    /* On I/O processes with no data (after rearrangement) to write the iobuf_sz will be 0 */
    if(iobuf_sz > 0){
      wcache->iobuf = bget(iobuf_sz);
      if(!wcache->iobuf){
        return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                        "Queuing asynchronous op/task for writing fillvalue for variables (number of variables = %d) to file (%s, ncid=%d) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for caching variable data for all the variables", nvars, pio_get_fname_from_file(file), file->pio_ncid, static_cast<long long int>(iobuf_sz));
      }

      void *iobuf = spio_file_mvcache_get(file, iodesc->ioid);
      assert(iobuf);
      std::memcpy(wcache->iobuf, iobuf, iobuf_sz);
      wcache->iobuf_sz = iobuf_sz;
    }
  }

  /* Create async task */
  pio_async_op_t *pnew = static_cast<pio_async_op_t *>(calloc(1, sizeof(pio_async_op_t)));
  if(pnew == NULL){
    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                      "Queuing asynchronous op/task for writing fillvalue for variables (number of variables = %d) to file (%s, ncid=%d) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for async task internal struct", nvars, pio_get_fname_from_file(file), file->pio_ncid, static_cast<long long int>(sizeof(pio_async_op_t)));
  }

  pnew->op_type = PIO_ASYNC_HDF5_WRITE_OP;
  pnew->pdata = static_cast<void *>(wcache);
  pnew->wait = pio_iosys_async_op_hdf5_write;
  pnew->poke = pio_async_poke_func_unavail;
  pnew->free = pio_iosys_async_op_hdf5_write_free;

  /* One more pending op using this iodesc & file */
  iodesc->nasync_pend_ops++;
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(pnew);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation to the thread pool (iosystem = %d). Adding the asynchronous operation failed", ios->iosysid);
  }

  return PIO_NOERR;
#else // _HDF5
  assert(0);
  return PIO_EINTERNAL;
#endif // _HDF5
}

