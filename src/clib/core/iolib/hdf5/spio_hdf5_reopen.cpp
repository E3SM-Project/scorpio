/** @file
 * Native HDF5 reopen support.
 *
 * PIOc_openfile_retry_impl() has historically handled every HDF5-iotype
 * reopen by switching file->iotype away to NETCDF4 (or, for a file that is
 * not even valid HDF5, PNETCDF/NETCDF) - see the H5Fis_hdf5() block in
 * pioc_support.cpp. That is a safe fallback, but it means a file SCORPIO
 * itself wrote with the HDF5 iotype can never be read back with the HDF5
 * iotype's own (eventual) read support.
 *
 * This file adds the detection half of fixing that: spio_hdf5_is_scorpio_written()
 * recognizes a file written by spio_hdf5_create()/spio_hdf5_def_var()/
 * spio_hdf5_enddef() (see spio_hdf5_utils.cpp), and spio_hdf5_reopen() is the
 * entry point PIOc_openfile_retry_impl() calls before falling back to the
 * historical switch-away behavior. The dimension/variable/attribute
 * introspection that lets a recognized file's reopen actually succeed
 * (returning PIO_NOERR) is added by the task that immediately follows this
 * one in the same plan - until then, spio_hdf5_reopen() always reports
 * PIO_EBADIOTYPE (see the TODO below), so observable behavior is unchanged.
 */
#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>

#include "spio_hdf5_reopen.hpp"

#ifdef _HDF5

namespace {

struct spio_hdf5_dimscale_scan_ctx_t
{
  bool found_dimscale;
};

/* H5Literate2() callback: stop at the first immediate child of the
 * iterated group that H5DSis_scale() recognizes as an HDF5 dimension-scale
 * dataset - the marker spio_hdf5_utils.cpp's spio_hdf5_enddef() writes, via
 * H5DSset_scale(), on every SCORPIO dimension dataset (coordinate or
 * pure-dimension).
 */
extern "C" herr_t spio_hdf5_find_dimscale_cb(hid_t loc_id, const char *name,
                                              const H5L_info2_t *info, void *op_data)
{
  (void) info;
  spio_hdf5_dimscale_scan_ctx_t *ctx = static_cast<spio_hdf5_dimscale_scan_ctx_t *>(op_data);

  /* Check the link target's type before trying to open it as a dataset -
   * H5Dopen2() on a non-dataset (e.g. a group) would push an avoidable
   * error onto the HDF5 error stack (and, with the default auto-printing
   * error handler, print it) on every ordinary group link we encounter.
   */
  H5O_info2_t oinfo;
  if(H5Oget_info_by_name3(loc_id, name, &oinfo, H5O_INFO_BASIC, H5P_DEFAULT) < 0){
    /* Could not stat this link's target - skip it, do not fail the scan */
    return 0;
  }

  if(oinfo.type != H5O_TYPE_DATASET){
    return 0;
  }

  hid_t did = H5Dopen2(loc_id, name, H5P_DEFAULT);
  if(did == H5I_INVALID_HID){
    return 0;
  }

  htri_t is_ds = H5DSis_scale(did);
  H5Dclose(did);

  if(is_ds > 0){
    ctx->found_dimscale = true;
    return 1; /* Found one - stop iterating */
  }

  return 0;
}

} // anonymous namespace

bool spio_hdf5_is_scorpio_written(hid_t file_id)
{
  if(file_id == H5I_INVALID_HID){
    return false;
  }

  /* (a) The root group's creation property list must have link creation
   * order both tracked and indexed - see spio_hdf5_create()'s
   * H5Pset_link_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED |
   * H5P_CRT_ORDER_INDEXED) call, made on the file creation property list at
   * file-creation time. H5Pget_link_creation_order() itself, however, only
   * accepts a *group* creation property list (it calls
   * H5P_object_verify(plist_id, H5P_GROUP_CREATE) internally) - so reading
   * the flags back requires going through the root group's GCPL
   * (H5Gget_create_plist()), not H5Fget_create_plist()'s FCPL, even though
   * the property was originally set on the FCPL. This alone is not
   * sufficient (nothing stops some other HDF5 writer from setting the same
   * property), so it is always combined with the dimension-scale marker
   * check below.
   */
  hid_t root_id = H5Gopen2(file_id, "/", H5P_DEFAULT);
  if(root_id == H5I_INVALID_HID){
    return false;
  }

  hid_t gcpl_id = H5Gget_create_plist(root_id);
  if(gcpl_id == H5I_INVALID_HID){
    H5Gclose(root_id);
    return false;
  }

  unsigned crt_order_flags = 0;
  herr_t pret = H5Pget_link_creation_order(gcpl_id, &crt_order_flags);
  H5Pclose(gcpl_id);
  if(pret < 0){
    H5Gclose(root_id);
    return false;
  }

  const unsigned expected_flags =
    static_cast<unsigned>(H5P_CRT_ORDER_TRACKED) | static_cast<unsigned>(H5P_CRT_ORDER_INDEXED);
  if((crt_order_flags & expected_flags) != expected_flags){
    H5Gclose(root_id);
    return false;
  }

  /* (b) The root group must contain at least one HDF5 dimension-scale
   * dataset (H5DSis_scale()) - every dimension SCORPIO defines gets one
   * (see spio_hdf5_utils.cpp's spio_hdf5_enddef(), which calls
   * H5DSset_scale() on every hdf5_dims[]/coordinate-variable dataset). An
   * empty file, or a file with no datasets at all, correctly falls through
   * to "false" here (H5Literate2() simply finds nothing to stop on,
   * leaving found_dimscale at its initial value).
   */
  spio_hdf5_dimscale_scan_ctx_t ctx;
  ctx.found_dimscale = false;

  herr_t iret = H5Literate2(root_id, H5_INDEX_NAME, H5_ITER_NATIVE, NULL,
                             spio_hdf5_find_dimscale_cb, &ctx);
  H5Gclose(root_id);

  if(iret < 0){
    return false;
  }

  return ctx.found_dimscale;
}

int spio_hdf5_reopen(iosystem_desc_t *ios, file_desc_t *file, const char *filename, int mode)
{
  assert(ios && file && filename);

  /* Cheap pre-check, using the exact same local/non-collective call
   * PIOc_openfile_retry_impl() itself uses right next to this function to
   * decide whether a file is HDF5 at all: if H5Fis_hdf5() does not confirm
   * this file is valid HDF5 *right now*, do not even attempt the heavier
   * H5Fopen() probe below - just report "not recognized" immediately. Aside
   * from avoiding needless work on a clearly-non-HDF5 file, this also keeps
   * this function's own view of the file consistent with the caller's: the
   * caller only reaches its own switch-iotype-away H5Fis_hdf5() check after
   * this function returns, so checking the identical thing first, here,
   * means both checks are evaluated back-to-back rather than one being
   * separated from the other by this function's (comparatively heavier)
   * H5Fopen()/H5Literate2() work - which matters because this probe is not
   * an MPI collective call, and a freshly-closed file on a shared
   * filesystem is exactly the kind of thing whose visibility can otherwise
   * flip between two closely-spaced, independent local opens.
   */
  if(H5Fis_hdf5(filename) <= 0){
    return PIO_EBADIOTYPE;
  }

  /* A plain, local (not MPI-IO collective) probe open, purely to read the
   * file-level/dataset-level metadata spio_hdf5_is_scorpio_written() needs.
   * A full reopen's own parallel/collective open (if one turns out to be
   * needed) is this function's responsibility to set up below, once
   * recognition succeeds.
   *
   * A failure here is treated the same as "not recognized" (PIO_EBADIOTYPE),
   * not as a genuine, propagated error: since this probe is not an MPI
   * collective call, different ranks can observe different outcomes for the
   * exact same file at the exact same point in time. Treating a transient
   * H5Fopen() failure as a hard error here, instead, would let one rank take
   * the "genuine error, return immediately" path in
   * PIOc_openfile_retry_impl() while other ranks fall through normally - a
   * divergence that can deadlock any collective call later in that
   * function. Falling back uniformly avoids that regardless of why the open
   * failed.
   */
  hid_t fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
  if(fid == H5I_INVALID_HID){
    return PIO_EBADIOTYPE;
  }

  if(!spio_hdf5_is_scorpio_written(fid)){
    /* Not a SCORPIO-written HDF5 file (or indeterminate) - close the probe
     * handle and let the caller fall back to the existing (safe, if lossy)
     * switch-iotype-away behavior. This is an expected outcome, not an
     * error: every file that predates this feature, or that was written by
     * some other HDF5 writer, takes this path.
     */
    H5Fclose(fid);
    return PIO_EBADIOTYPE;
  }

  /* Recognized as a SCORPIO-written HDF5 file. The dimension/variable/
   * attribute introspection that would populate file->hdf5_dims/hdf5_vars/
   * hdf5_attrs from fid, and let this function return PIO_NOERR with
   * file->hdf5_file_id = fid and file->iotype left as HDF5/HDF5C, is not
   * implemented yet - it lands in the very next task of this plan, in the
   * same branch/PR, as a direct continuation of this function.
   *
   * Until then, report PIO_EBADIOTYPE (the same "not recognized, fall
   * back" sentinel used above) so PIOc_openfile_retry_impl() falls back to
   * the existing switch-to-NETCDF4 behavior instead of returning PIO_NOERR
   * over a file with zero populated dims/vars/attrs - which would break
   * every existing caller that reads back what it just wrote. This keeps
   * this task's change behavior-neutral for every file, recognized or not,
   * until the next task gives this branch something real to do.
   *
   * TODO: Task 11 replaces this block with a call to the real
   * introspection routine and returns PIO_NOERR on its success, or a
   * genuine PIO error code - not PIO_EBADIOTYPE - if that introspection
   * itself fails.
   */
  H5Fclose(fid);
  return PIO_EBADIOTYPE;
}

#endif /* _HDF5 */
