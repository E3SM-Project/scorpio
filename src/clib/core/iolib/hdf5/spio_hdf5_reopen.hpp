#ifndef __SPIO_HDF5_REOPEN_HPP__
#define __SPIO_HDF5_REOPEN_HPP__

#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#if PIO_USE_HDF5
#include <hdf5.h>
#endif // PIO_USE_HDF5

#ifdef _HDF5

/* Returns true iff file_id identifies an HDF5 file that was written by
 * SCORPIO's native HDF5 writer (spio_hdf5_create()/spio_hdf5_def_var()/
 * spio_hdf5_enddef(), see spio_hdf5_utils.cpp), as opposed to some other
 * HDF5-compatible writer (including a plain HDF5 file with none of
 * SCORPIO's markers at all). Used by spio_hdf5_reopen() to decide whether a
 * native (no switch-away-to-NETCDF4) reopen is even worth attempting.
 *
 * Returns false (not SCORPIO-written, or indeterminate) on any missing
 * marker, any iteration/attribute-read error, or an empty file. A "false"
 * result is an ordinary, expected outcome for most HDF5 files SCORPIO might
 * be asked to open - it never raises a PIO/HDF5 error itself.
 *
 * Does not take ownership of, or close, file_id.
 */
bool spio_hdf5_is_scorpio_written(hid_t file_id);

/* Attempt a native HDF5 reopen of filename, without switching file->iotype
 * away to NETCDF4/PNETCDF/NETCDF the way the historical (lossy) fallback in
 * PIOc_openfile_retry_impl() does for every HDF5-iotype file.
 *
 * Returns:
 *  - PIO_NOERR on a successful native reopen, with file->hdf5_file_id and
 *    file->hdf5_dims/hdf5_vars/hdf5_attrs (and their counts) populated, and
 *    file->iotype left unchanged (HDF5/HDF5C).
 *  - PIO_EBADIOTYPE if filename is not recognized as a SCORPIO-written
 *    HDF5 file (see spio_hdf5_is_scorpio_written()) - the caller should
 *    fall back to the existing switch-iotype-away behavior. This is an
 *    expected, non-error outcome for any file this feature does not (yet)
 *    handle, not a sign that anything went wrong.
 *  - any other PIO error code on a genuine failure while reopening a file
 *    that *was* recognized as SCORPIO-written - callers must propagate
 *    this like any other error, not silently fall back, since it indicates
 *    a real bug rather than "this feature doesn't apply to this file".
 *
 * Any H5Fopen()'d handle used purely to probe recognition is always closed
 * before returning on the PIO_EBADIOTYPE path - callers never need to clean
 * up a probe handle themselves.
 */
int spio_hdf5_reopen(iosystem_desc_t *ios, file_desc_t *file, const char *filename, int mode);

#endif // _HDF5

#endif // __SPIO_HDF5_REOPEN_HPP__
