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
#include "pio_timer.h"
#include <pio_internal.h>
#include "spio_async_op.hpp"
#include "spio_async_utils.hpp"
#include "spio_async_tpool.hpp"
#include "spio_file_mvcache.h"
#include "spio_dbg_utils.hpp"
#include "spio_dt_converter.hpp"
#include "spio_hdf5_utils.hpp"
#include "spio_async_tcomm.hpp"
#include "spio_iosys_utils.hpp"

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

struct Hdf5_put_att_info{
  file_desc_t *file;
  int varid;
  std::string aname;
  nc_type atype;
  PIO_Offset alen;
  void *abuf;
};

struct Hdf5_enddef_info{
  file_desc_t *file;
};

struct Hdf5_put_var_info{
  file_desc_t *file;
  int varid;
  std::vector<PIO_Offset> start;
  std::vector<PIO_Offset> count;
  std::vector<PIO_Offset> stride;
  nc_type xtype;
  PIO_Offset vbuf_sz;
  void *vbuf;
};

struct Hdf5_set_frame_info{
  file_desc_t *file;
  int varid;
  int frame;
};

struct Hdf5_wcache{
  file_desc_t *file;
  int nvars;
  int fndims;
  std::vector<int> varids;
  std::shared_ptr<io_desc_t> iodesc;
  std::vector<int> frame;

  /* FIXME: Since wcache is only used by async write ops we can
   * directly use the async I/O desc info from iodesc, without
   * caching io_desc_info from the caller
   */
  /* Cache the io_desc_info from the caller, i.e., sync/async I/O desc
   * info returned by  iodesc->select_io_desc_info(), to determine if
   * the call is sync/async. The async write with async I/O desc is
   * currently only implemented for HDF5[C] + PIO_REARR_CONTIG
   *
   * io_desc_info is not owned by wcache
   */
  io_desc_info_t *io_desc_info;

  bool wr_fillbuf;
  void *iobuf;
  std::size_t iobuf_sz;
  void *fillbuf;
  std::size_t fillbuf_sz;

  Hdf5_wcache(file_desc_t *file, int nvars, int fndims,
    const int *varids, std::shared_ptr<io_desc_t> iodesc, bool wr_fillbuf, const int *frame);
  ~Hdf5_wcache();
};

Hdf5_wcache::Hdf5_wcache(file_desc_t *file, int nvars, int fndims,
  const int *varids, std::shared_ptr<io_desc_t> iodesc, bool wr_fillbuf, const int *frame):
    file(file), nvars(nvars), fndims(fndims), iodesc(iodesc),
    io_desc_info(NULL),
    wr_fillbuf(wr_fillbuf), iobuf(NULL), iobuf_sz(0),
    fillbuf(NULL), fillbuf_sz(0)
{
  assert(file && (nvars > 0) && (fndims > 0) && varids && iodesc);

  /* Cache varids and frames (one frame, the frame being written,
   * for each varid)
   */
  this->varids.resize(nvars);
  std::copy(varids, varids + nvars, this->varids.begin());

  if(frame){
    this->frame.resize(nvars);
    std::copy(frame, frame + nvars, this->frame.begin());
  }

  /* FIXME: Copy/init iobuf and fillbuf here */
}

Hdf5_wcache::~Hdf5_wcache()
{
  /* Don't delete cached iodesc ptr (we don't own it) */

  if(iobuf) { brel(iobuf); iobuf = NULL; }
  if(fillbuf) { brel(fillbuf); fillbuf = NULL; }
}

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
  //SPIO_Util::Dbg_Util::Stdout_logger(__func__);
  if(pdata){
    delete(static_cast<Hdf5_create_info *>(pdata));
  }
}

int spio_iosys_async_op_hdf5_create(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_create_info *cinfo = static_cast<Hdf5_create_info *>(pdata);

  assert(cinfo && cinfo->file && cinfo->file->iosystem);
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + cinfo->fname);

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

  if(!ios->io_comm_info->is_sync_io_proc()){
    return PIO_NOERR;
  }

  Hdf5_create_info *cinfo = new Hdf5_create_info{file, filename};

  /* Create async task */
  SPIO_Util::Async_op op = {SPIO_Util::Async_op::Op_type::SPIO_ASYNC_HDF5_CREATE_OP,
                            static_cast<void *>(cinfo),
                            spio_iosys_async_op_hdf5_create,
                            pio_async_poke_func_unavail,
                            spio_iosys_async_op_hdf5_create_free};

  /* One more pending op using this iodesc & file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(op);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation (to create file = %s) to the thread pool (iosystem = %d). Adding the asynchronous operation failed", filename, ios->iosysid);
  }

  return PIO_NOERR;
}

void spio_iosys_async_op_hdf5_def_var_free(void *pdata)
{
  //SPIO_Util::Dbg_Util::Stdout_logger(__func__);
  if(pdata){
    delete(static_cast<Hdf5_def_var_info *>(pdata));
  }
}

int spio_iosys_async_op_hdf5_def_var(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_def_var_info *def_var_info = static_cast<Hdf5_def_var_info *>(pdata);
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + ": fname = " + def_var_info->file->fname + ", vname: " + def_var_info->vname);

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

  if(!ios->io_comm_info->is_sync_io_proc()){
    return PIO_NOERR;
  }

  Hdf5_def_var_info *def_var_info = new Hdf5_def_var_info{file, name, xtype, ndims,
                                  {dimidsp, dimidsp + ndims}, varid};

  /* Create async task */
  SPIO_Util::Async_op op = {SPIO_Util::Async_op::Op_type::SPIO_ASYNC_HDF5_DEF_VAR_OP,
                            static_cast<void *>(def_var_info),
                            spio_iosys_async_op_hdf5_def_var,
                            pio_async_poke_func_unavail,
                            spio_iosys_async_op_hdf5_def_var_free};

  /* One more pending op using this iodesc & file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(op);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation (to define var = %s, in file = %s) to the thread pool (iosystem = %d). Adding the asynchronous operation failed", name, pio_get_fname_from_file(file), ios->iosysid);
  }

  return PIO_NOERR;
}

void spio_iosys_async_op_hdf5_put_att_free(void *pdata)
{
  //SPIO_Util::Dbg_Util::Stdout_logger(__func__);
  if(pdata){
    Hdf5_put_att_info *info = static_cast<Hdf5_put_att_info *>(pdata);
    free(info->abuf);
    delete(info);
  }
}

int spio_iosys_async_op_hdf5_put_att(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_put_att_info *info = static_cast<Hdf5_put_att_info *>(pdata);

  assert(info && info->file && info->file->iosystem);
  assert((info->varid >= 0) || (info->varid == PIO_GLOBAL));
  assert(info->alen >= 0);
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + ": fname = " + info->file->fname + ", aname: " + info->aname);

  ret = spio_hdf5_put_att(info->file->iosystem, info->file,
          info->varid, info->aname.c_str(), info->atype,
          info->alen, info->abuf);

  info->file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return ret;
}

int spio_iosys_async_hdf5_put_att_op_add(file_desc_t *file, int varid,
      const char *aname, nc_type atype, PIO_Offset alen, const void *abuf)
{
  int ret = PIO_NOERR;

  assert(file && file->iosystem && aname && (alen >= 0) && ((alen == 0) || abuf));
  assert((varid >= 0) || (varid = PIO_GLOBAL));

  iosystem_desc_t *ios = file->iosystem;

  if(!ios->io_comm_info->is_sync_io_proc()){
    return PIO_NOERR;
  }

  void *buf = malloc(alen * spio_get_nc_type_size(atype));
  if(buf == NULL){
    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                      "Queuing asynchronous op/task for writing attribute (%s, variable=%s, file=%s) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for caching attribute value", aname, pio_get_vname_from_file(file, varid), pio_get_fname_from_file(file), static_cast<long long int>(alen));
  }

  memcpy(buf, abuf, alen * spio_get_nc_type_size(atype));

  Hdf5_put_att_info *info = new Hdf5_put_att_info{file, varid, aname, atype, alen, buf};

  /* Create async task */
  SPIO_Util::Async_op op = {SPIO_Util::Async_op::Op_type::SPIO_ASYNC_HDF5_PUT_ATT_OP,
                            static_cast<void *>(info),
                            spio_iosys_async_op_hdf5_put_att,
                            pio_async_poke_func_unavail,
                            spio_iosys_async_op_hdf5_put_att_free};

  /* One more pending op using this iodesc & file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(op);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation (to define attribute = %s, for var = %s in file = %s) to the thread pool (iosystem = %d). Adding the asynchronous operation failed", aname, pio_get_vname_from_file(file, varid), pio_get_fname_from_file(file), ios->iosysid);
  }

  return PIO_NOERR;
}

void spio_iosys_async_op_hdf5_enddef_free(void *pdata)
{
  //SPIO_Util::Dbg_Util::Stdout_logger(__func__);
  if(pdata){
    delete(static_cast<Hdf5_enddef_info *>(pdata));
  }
}

int spio_iosys_async_op_hdf5_enddef(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_enddef_info *info = static_cast<Hdf5_enddef_info *>(pdata);

  assert(info && info->file && info->file->iosystem);
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + ": fname = " + info->file->fname);

  ret = spio_hdf5_enddef(info->file->iosystem, info->file);

  info->file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return ret;
}

int spio_iosys_async_hdf5_enddef_op_add(file_desc_t *file)
{
  int ret = PIO_NOERR;

  assert(file && file->iosystem);

  iosystem_desc_t *ios = file->iosystem;

  if(!ios->io_comm_info->is_sync_io_proc()){
    return PIO_NOERR;
  }

  Hdf5_enddef_info *info = new Hdf5_enddef_info{file};

  /* Create async task */
  SPIO_Util::Async_op op = {SPIO_Util::Async_op::Op_type::SPIO_ASYNC_HDF5_ENDDEF_OP,
                            static_cast<void *>(info),
                            spio_iosys_async_op_hdf5_enddef,
                            pio_async_poke_func_unavail,
                            spio_iosys_async_op_hdf5_enddef_free};

  /* One more pending op using this file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(op);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation (to end define mode in file = %s) to the thread pool (iosystem = %d). Adding the asynchronous operation failed", pio_get_fname_from_file(file), ios->iosysid);
  }

  return PIO_NOERR;
}

void spio_iosys_async_op_hdf5_put_var_free(void *pdata)
{
  //SPIO_Util::Dbg_Util::Stdout_logger(__func__);
  if(pdata){
    Hdf5_put_var_info *info = static_cast<Hdf5_put_var_info *>(pdata);
    free(info->vbuf);
    delete(info);
  }
}

int spio_iosys_async_op_hdf5_put_var(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_put_var_info *info = static_cast<Hdf5_put_var_info *>(pdata);

  assert(info && info->file && info->file->iosystem);
  assert((info->varid >= 0) || (info->varid == PIO_GLOBAL));
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + ": fname = " + info->file->fname + ", varid=" + std::to_string(info->varid));

  ret = spio_hdf5_put_var(info->file->iosystem, info->file,
          info->varid,
          (!info->start.empty()) ? (info->start.data()) : NULL,
          (!info->count.empty()) ? (info->count.data()) : NULL,
          (!info->stride.empty()) ? (info->stride.data()) : NULL,
          info->xtype, info->vbuf);

  info->file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return ret;
}

int spio_iosys_async_hdf5_put_var_op_add(file_desc_t *file, int varid,
      const PIO_Offset *start, const PIO_Offset *count, const PIO_Offset *stride,
      nc_type xtype, const void *vbuf)
{
  int ret = PIO_NOERR;

  assert(file && file->iosystem && (varid >= 0) && vbuf);

  iosystem_desc_t *ios = file->iosystem;

  if(!ios->io_comm_info->is_sync_io_proc()){
    return PIO_NOERR;
  }

  int ndims = file->hdf5_vars[varid].ndims;
  PIO_Offset vbuf_sz = 0;
  if(count){
    vbuf_sz = std::accumulate(count, count + ndims, 1, std::multiplies<PIO_Offset>());
  }
  else{
    int *dimids = file->hdf5_vars[varid].hdf5_dimids;
    vbuf_sz = std::accumulate(dimids, dimids + ndims, 1,
                [file](PIO_Offset acc_sz, const int &dimid){
                  return acc_sz * file->hdf5_dims[dimid].len;
                });
  }

  vbuf_sz *= spio_get_nc_type_size(xtype);

  void *buf = malloc(vbuf_sz);
  if(buf == NULL){
    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                      "Queuing asynchronous op/task for writing variable(%s, varid=%d, file=%s) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for caching variable value", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), static_cast<long long int>(vbuf_sz));
  }

  memcpy(buf, vbuf, vbuf_sz);

  Hdf5_put_var_info *info = new Hdf5_put_var_info{file, varid,
                                  (start) ? std::vector<PIO_Offset>{start, start + ndims} : std::vector<PIO_Offset>{},
                                  (count) ? std::vector<PIO_Offset>{count, count + ndims} : std::vector<PIO_Offset>{},
                                  (stride) ? std::vector<PIO_Offset>{stride, stride + ndims} : std::vector<PIO_Offset>{},
                                  xtype, vbuf_sz, buf};

  /* Create async task */
  SPIO_Util::Async_op op = {SPIO_Util::Async_op::Op_type::SPIO_ASYNC_HDF5_PUT_VAR_OP,
                            static_cast<void *>(info),
                            spio_iosys_async_op_hdf5_put_var,
                            pio_async_poke_func_unavail,
                            spio_iosys_async_op_hdf5_put_var_free};

  /* One more pending op using this file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(op);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation (to write/put var = %s in file = %s) to the thread pool (iosystem = %d). Adding the asynchronous operation failed", pio_get_vname_from_file(file, varid), pio_get_fname_from_file(file), ios->iosysid);
  }

  return PIO_NOERR;
}

void spio_iosys_async_op_hdf5_set_frame_free(void *pdata)
{
  //SPIO_Util::Dbg_Util::Stdout_logger(__func__);
  if(pdata){
    delete(static_cast<Hdf5_set_frame_info *>(pdata));
  }
}

int spio_iosys_async_op_hdf5_set_frame(void *pdata)
{
  int ret = PIO_NOERR;

  Hdf5_set_frame_info *info = static_cast<Hdf5_set_frame_info *>(pdata);

  assert(info && info->file && info->file->iosystem);
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + ": fname = " + info->file->fname + ", frame=" + std::to_string(info->frame));

  ret = spio_hdf5_set_frame(info->file, info->varid, info->frame);

  info->file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return ret;
}

int spio_iosys_async_hdf5_set_frame_op_add(file_desc_t *file, int varid, int frame)
{
  int ret = PIO_NOERR;

  assert(file && file->iosystem && (varid >= 0) && (frame >= 0));

  iosystem_desc_t *ios = file->iosystem;

  if(!ios->io_comm_info->is_sync_io_proc()){
    return PIO_NOERR;
  }

  Hdf5_set_frame_info *info = new Hdf5_set_frame_info{file, varid, frame};

  /* Create async task */
  SPIO_Util::Async_op op = {SPIO_Util::Async_op::Op_type::SPIO_ASYNC_HDF5_SET_FRAME_OP,
                            static_cast<void *>(info),
                            spio_iosys_async_op_hdf5_set_frame,
                            pio_async_poke_func_unavail,
                            spio_iosys_async_op_hdf5_set_frame_free};

  /* One more pending op using this iodesc & file */
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(op);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation (to set frame to %d for variable = %s in file = %s) to the thread pool (iosystem = %d). Adding the asynchronous operation failed", frame, pio_get_vname_from_file(file, varid), pio_get_fname_from_file(file), ios->iosysid);
  }

  return PIO_NOERR;
}

#endif // _HDF5

int spio_wait_all_hdf5_async_ops(int iosysid)
{
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + ", iosysid=" + std::to_string(iosysid));
  unsigned long long int sleep_time = 0;
  /* Sleep for 0.5 seconds */
  const int SLEEP_TIME_IN_MILLISECONDS = 500;
  /* Warn every 2 seconds */
  const int WARN_TIME_IN_MILLISECONDS = 500;
  while(SPIO_Util::GVars::npend_hdf5_async_ops > 0){
    std::this_thread::sleep_for(std::chrono::milliseconds(SLEEP_TIME_IN_MILLISECONDS));
    sleep_time += SLEEP_TIME_IN_MILLISECONDS;
    if((SPIO_Util::GVars::npend_hdf5_async_ops > 0) && (sleep_time % WARN_TIME_IN_MILLISECONDS == 0)){
      std::string warn_msg = std::string("Continuing to wait on all HDF5 async ops... ") +
                              "(Elapsed time = " + std::to_string(sleep_time) + " ms)";
      PIOc_warn(iosysid, PIO_DEFAULT, __FILE__, __LINE__, warn_msg.c_str());
    }
  }

  return PIO_NOERR;
}

/* The body of pio_iosys_async_op_hdf5_write(), separated out so that the
 * pending-op counters (iodesc->nasync_pend_ops, file->npend_ops and
 * SPIO_Util::GVars::npend_hdf5_async_ops) can be decremented on *every* exit
 * path, including the error returns below. Leaking a pending-op count on an
 * error used to be harmless because nothing ever waited on those counters;
 * now that close/sync actually drain file->npend_ops (see
 * spio_wait_file_async_pend_ops() in pio_file.cpp) a leaked count would stall
 * the application until the async-op timeout expires. */
static int pio_iosys_async_op_hdf5_write_impl(void *pdata)
{
#ifdef _HDF5
  /* FIXME: Add futures */
  int ret = PIO_NOERR;
  Hdf5_wcache *wcache = static_cast<struct Hdf5_wcache *>(pdata);
  assert(wcache);
  //SPIO_Util::Dbg_Util::Stdout_logger(std::string(__func__) + ", fname=" + wcache->file->fname + ", nvars=" + std::to_string(wcache->nvars));

  file_desc_t *file = wcache->file;
  int nvars = wcache->nvars;
  int fndims = wcache->fndims;
  io_desc_t *iodesc = wcache->iodesc.get();

  assert(file && (nvars > 0) && (fndims > 0) && iodesc);
  assert((file->iotype == PIO_IOTYPE_HDF5) || (file->iotype == PIO_IOTYPE_HDF5C));

  iosystem_desc_t *ios = file->iosystem;
  var_desc_t *v1desc = file->varlist + wcache->varids[0];

  /* io_desc_info selected (sync or async) when this op was queued - use it,
   * rather than iodesc->sync_io_desc_info directly, so an async-dispatched
   * write uses the async rearranger's region/communicator info. */
  assert(wcache->io_desc_info);
  io_desc_info_t &io_desc_info = *(wcache->io_desc_info);
  bool is_async = (wcache->io_desc_info == iodesc->async_io_desc_info);

  assert(ios && v1desc);
  assert(is_async ? ios->io_comm_info->is_async_io_proc() : ios->io_comm_info->is_sync_io_proc());

  bool wr_fillbuf = wcache->wr_fillbuf;

  //int num_regions = (wr_fillbuf) ? io_desc_info.maxfillregions : io_desc_info.maxregions;
  io_region *region = (wr_fillbuf) ? io_desc_info.fillregion : io_desc_info.firstregion;
  PIO_Offset llen = (wr_fillbuf) ? io_desc_info.holegridsize : io_desc_info.llen;
  void *iobuf = (wr_fillbuf) ? (wcache->fillbuf) : (wcache->iobuf);
  std::size_t iobuf_sz = (wr_fillbuf) ? (wcache->fillbuf_sz) : (wcache->iobuf_sz);

  /* FIXME: for wr_fillbuf with nvars > 1 this fires - see the FIXME on
   * fillbuf_sz's computation in pio_iosys_async_hdf5_write_op_add() above.
   * Pre-existing (identical in master, modulo the iodesc -> io_desc_info
   * rename); not fixed here. */
  assert(region && (nvars * llen * io_desc_info.mpitype_size == static_cast<PIO_Offset>(iobuf_sz)));
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

    ret = spio_find_start_count(io_desc_info.ndims, io_desc_info.dimlen, fndims, v1desc, region, start, count);
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
      bufptr = (void *)((char *)iobuf + vidx * io_desc_info.mpitype_size * llen);
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
    hid_t mem_type_id = spio_nc_type_to_hdf5_type(io_desc_info.piotype);
    if(mem_type_id == H5I_INVALID_HID){
      return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                     "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                     "Unsupported memory type (type=%x) for variable (%s, varid=%d)",
                     nvars, pio_get_fname_from_file(file), file->pio_ncid, io_desc_info.piotype, pio_get_vname_from_file(file, wcache->varids[vidx]), wcache->varids[vidx]);
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
      wbuf = static_cast<SPIO_Util::File_Util::DTConverter *>(file->dt_converter)->convert(iodesc->ioid, bufptr, io_desc_info.mpitype_size * reg_nelems,
              io_desc_info.piotype, spio_hdf5_type_to_pio_type(file_var_ntype_id));
      if(wbuf == NULL){
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variables (number of variables = %d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "Unable to convert the type (from %d to %d) of variable (%s, varid=%d)",
                       nvars, pio_get_fname_from_file(file), file->pio_ncid, io_desc_info.piotype,
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

  /* FIXME: Is this barrier needed ? TComm_info caches a single io_comm with
   * no sync/async role distinction - it is built from the *sync* I/O comm
   * (see TComm_info's construction from io_comm_info->get_sync_io_comm()), so
   * it is MPI_COMM_NULL on async-only I/O ranks. An async-dispatched write
   * (CONTIG/BOX/SUBSET + HDF5/HDF5C + PIO_USE_ASYNC_WR_THREAD) would
   * therefore barrier on the wrong rank set here if it ever ran on async I/O
   * ranks disjoint from the sync ones. That configuration is currently
   * prevented upstream by io_desc_t::select_io_desc_info(), which falls any
   * of CONTIG/BOX/SUBSET back to sync_io_desc_info unless the file uses async
   * I/O (file->uses_async_io, computed once at file create/open time - see
   * spio_createfile_int()/PIOc_openfile_retry_impl()), so this barrier is
   * always reached by the full sync-I/O rank set today. It still needs a
   * TComm_info audit (a sync/async role-aware io_comm) before HDF5
   * file/dataset creation is extended to span the async comm. */
  MPI_Barrier(ios->tcomm_info->get_io_comm());

  return PIO_NOERR;
#else // _HDF5
  assert(0);
  return PIO_EINTERNAL;
#endif // _HDF5
}

int pio_iosys_async_op_hdf5_write(void *pdata)
{
#ifdef _HDF5
  Hdf5_wcache *wcache = static_cast<struct Hdf5_wcache *>(pdata);
  assert(wcache);

  int ret = pio_iosys_async_op_hdf5_write_impl(pdata);

  /* One less pending op using this iodesc & file - decremented here, rather
   * than at the end of the body above, so that it also happens when the body
   * returns early with an error (see the comment on
   * pio_iosys_async_op_hdf5_write_impl()) */
  wcache->iodesc->nasync_pend_ops--;
  wcache->file->npend_ops--;
  SPIO_Util::GVars::npend_hdf5_async_ops--;

  return ret;
#else // _HDF5
  assert(0);
  return PIO_EINTERNAL;
#endif // _HDF5
}

void pio_iosys_async_op_hdf5_write_free(void *pdata)
{
  //SPIO_Util::Dbg_Util::Stdout_logger(__func__);
#ifdef _HDF5
  Hdf5_wcache *wcache = static_cast<struct Hdf5_wcache *>(pdata);
  assert(wcache);


  delete(wcache);
#else // _HDF5
  assert(0);
#endif // _HDF5
}

int pio_iosys_async_hdf5_write_op_add(file_desc_t *file, int nvars, int fndims,
      const int *varids, std::shared_ptr<io_desc_t> iodesc, int fill, const int *frame)
{
#ifdef _HDF5
  int ret = PIO_NOERR;

  assert(file && (nvars > 0) && (fndims > 0) && varids && iodesc);

  iosystem_desc_t *ios = file->iosystem;
  assert(ios);

  /* Select the io_desc_info_t (sync or async) that this write targets, and
   * gate on the role-correct membership check: an async-dispatched write
   * (CONTIG/BOX/SUBSET rearranger, PIO_USE_ASYNC_WR_THREAD on) is only
   * queued/executed on async I/O procs, everything else (including the flag
   * compiled off, where select_io_desc_info() always returns
   * sync_io_desc_info) keeps the original sync I/O proc gate. */
  io_desc_info_t &sel_desc = iodesc->select_io_desc_info(file->iotype, file);
  bool is_async_write = (&sel_desc == iodesc->async_io_desc_info);

  if(!(is_async_write ? ios->io_comm_info->is_async_io_proc() : ios->io_comm_info->is_sync_io_proc())){
    return PIO_NOERR;
  }

  Hdf5_wcache *wcache = new Hdf5_wcache(file, nvars, fndims, varids, iodesc,
    (fill) ? true : false, frame);
  wcache->io_desc_info = &sel_desc;

  /* We need to copy the iobuf/fillbuf since the mvcache gets reused for future writes */
  /* Copy iobuf/fillvalue */
  var_desc_t *v1desc = file->varlist + varids[0];
  //PIO_Offset llen = fill ? sel_desc.holegridsize : sel_desc.llen;
  if(fill){
    /* FIXME: fillbuf_sz is not multiplied by nvars, but the write loop in
     * pio_iosys_async_op_hdf5_write_impl() (see the assert at the top of
     * that function, and the per-variable "bufptr = iobuf + vidx *
     * mpitype_size * llen" offset below it) indexes wcache->fillbuf as if it
     * held nvars copies of a single variable's fill region. For nvars > 1
     * this undersizes/underfills fillbuf: the assert
     * "nvars * llen * mpitype_size == iobuf_sz" then fails (confirmed via
     * test pio_decomp_fillval2_np4_nio2_st2, PIO_USE_ASYNC_WR_THREAD=ON,
     * which crashes on this assert), and without that assert vidx >= 1
     * would read past the end of the undersized fillbuf. Pre-existing bug -
     * this computation is an unchanged, mechanical port of master's
     * "iodesc->holegridsize * iodesc->mpitype_size" (same missing nvars
     * factor there); never exercised by CI since CI never sets
     * PIO_USE_ASYNC_WR_THREAD=ON. Not fixed here - tracked as a follow-up. */
    std::size_t fillbuf_sz = sel_desc.holegridsize * sel_desc.mpitype_size;
    /* On I/O processes with no data (after rearrangement) to write the fillbuf_sz will be 0 */
    if(fillbuf_sz > 0){
      wcache->fillbuf = bget(fillbuf_sz);
      if(!wcache->fillbuf){
        return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                        "Queuing asynchronous op/task for writing fillvalue for variables (number of variables = %d) to file (%s, ncid=%d) using PIO_IOTYPE_HDF5x failed. Unable to allocate memory (%lld bytes) for caching variable fillvalue", nvars, pio_get_fname_from_file(file), file->pio_ncid, static_cast<long long int>(fillbuf_sz));
      }

      /* FIXME: v1desc->fillbuf is NULL whenever no fill value was ever set for
       * this variable (its default), but fillbuf_sz > 0 can still hold (e.g.
       * holes from rearrangement) - memcpy() from a NULL source here is a
       * NULL-pointer dereference. Confirmed reproducible: a var with no fill
       * value set, PIO_USE_ASYNC_WR_THREAD=ON, crashes here with a SEGV
       * (test_darray_hdf5_contig's test_write_read_hdf5_dtconv, once
       * file->uses_async_io correctly resolves true - see commit
       * 969b502e1a). Pre-existing bug, never exercised by CI since CI never
       * sets PIO_USE_ASYNC_WR_THREAD=ON. Not fixed here - tracked as a
       * follow-up alongside the nvars-sizing FIXME above. */
      std::memcpy(wcache->fillbuf, v1desc->fillbuf, fillbuf_sz);
      wcache->fillbuf_sz = fillbuf_sz;
    }
  }
  else{
    /* Copy buffer, with rearranged data, for nvars */
    std::size_t iobuf_sz = nvars * sel_desc.llen * sel_desc.mpitype_size;
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
  SPIO_Util::Async_op op = {SPIO_Util::Async_op::Op_type::SPIO_ASYNC_HDF5_WRITE_OP,
                            static_cast<void *>(wcache),
                            pio_iosys_async_op_hdf5_write,
                            pio_async_poke_func_unavail,
                            pio_iosys_async_op_hdf5_write_free};

  /* One more pending op using this iodesc & file */
  iodesc->nasync_pend_ops++;
  file->npend_ops++;
  SPIO_Util::GVars::npend_hdf5_async_ops++;

  /* Get the mt queue and queue the async task */
  ret = pio_async_tpool_op_add(op);
  if(ret != PIO_NOERR){
    LOG((1, "Adding file pending ops to tpool failed, ret = %d", ret));
    return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                    "Internal error while adding asynchronous pending operation (to write %d variables in file = %s) to the thread pool (iosystem = %d). Adding the asynchronous operation failed", nvars, pio_get_fname_from_file(file), ios->iosysid);
  }

  return PIO_NOERR;
#else // _HDF5
  assert(0);
  return PIO_EINTERNAL;
#endif // _HDF5
}

