#ifndef __SPIO_ASYNC_HDF5_UTILS_HPP__
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include <memory>

namespace SPIO_Util{
  namespace GVars{
    extern std::atomic<int> npend_hdf5_async_ops;
  } // namespace GVars
} // namespace SPIO_Util

int spio_iosys_async_hdf5_create_op_add(file_desc_t *file, const char *filename);
int spio_iosys_async_hdf5_def_var_op_add(file_desc_t *file, const char *name,
      nc_type xtype, int ndims, const int *dimidsp, int varid);
int spio_iosys_async_hdf5_put_att_op_add(file_desc_t *file, int varid,
      const char *aname, nc_type atype, PIO_Offset alen, const void *abuf);
int spio_iosys_async_hdf5_enddef_op_add(file_desc_t *file);
int spio_iosys_async_hdf5_put_var_op_add(file_desc_t *file, int varid,
      const PIO_Offset *start, const PIO_Offset *count, const PIO_Offset *stride,
      nc_type xtype, const void *vbuf);

int spio_wait_all_hdf5_async_ops(int iosysid);
int pio_iosys_async_op_hdf5_write(void *pdata);
void pio_iosys_async_op_hdf5_write_free(void *pdata);
int pio_iosys_async_hdf5_write_op_add(file_desc_t *file, int nvars, int fndims,
      const int *varids, std::shared_ptr<io_desc_t> iodesc, int fill, const int *frame);
int spio_iosys_async_hdf5_set_frame_op_add(file_desc_t *file, int varid, int frame);

#if PIO_USE_ASYNC_WR_REARR
/* Combined async rearrange (CONTIG only)+HDF5 write op - see the long
 * comment above Hdf5_rearr_wcache (spio_async_hdf5_utils.cpp) for the
 * design. Only ever called on ranks where
 * Contig_rearr::aggregate_comp2io() (pio_rearr_contig.hpp) returned a
 * non-NULL agg_buf - takes ownership of agg_buf unconditionally, including
 * on its own failure paths. */
int pio_iosys_async_hdf5_rearr_write_op_add(file_desc_t *file, int nvars, int fndims,
      const int *varids, std::shared_ptr<io_desc_t> iodesc, const int *frame,
      void *agg_buf, std::size_t agg_buf_sz, const void *fillvalue);

/* Async CONTIG rearrange_io2comp() op - implemented for symmetry with the
 * write side above and unit-tested directly (test_spio_rearr_contig_async.cpp,
 * calling pio_iosys_async_op_hdf5_rearr_read_impl() inline), but
 * DELIBERATELY NEVER QUEUED by any production code path. See the design
 * note above pio_iosys_async_hdf5_rearr_read_op_add() (spio_async_hdf5_utils.cpp)
 * for why: wiring it into PIOc_read_darray() would put an agg_comm_
 * collective (disperse_data(), inside rearrange_io2comp()) on a worker
 * thread, breaking the invariant - relied on by Tasks 7-9 of the plan this
 * op was added for - that only I/O-owning ranks ever hold a pending
 * asynchronous rearrange op.
 *
 * Unlike Hdf5_rearr_wcache (the write side's cache, private to
 * spio_async_hdf5_utils.cpp), this struct is declared here, in the header,
 * so that a unit test can construct one "by hand" - set its fields directly
 * - without going through an op-add function, per the plan's Step 4
 * ("construct an Hdf5_rearr_rcache by hand ... call the _impl on the
 * test's own thread"). It also does not need an embedded Hdf5_wcache: this
 * op never touches HDF5 at all, so unlike the write op's _impl, this one's
 * functions carry no #ifdef _HDF5 guard. */
struct Hdf5_rearr_rcache{
  file_desc_t *file; /* not owned; may be NULL (never dereferenced by this op's _impl - only used for error-message context) */
  int varid; /* not used by this op's _impl; kept for parity/future use */
  std::shared_ptr<io_desc_t> iodesc;
  io_desc_info_t *io_desc_info; /* not owned */

  /* The local, on-disk-order I/O-side source data that rearrange_io2comp()
   * disperses to compute ranks - already read from the file by whatever
   * populates this cache (outside this op's responsibility; a real HDF5
   * read, or, in the unit test, a hand-filled pattern). Owned here:
   * malloc()'d by whoever constructs this cache, std::free()d by
   * ~Hdf5_rearr_rcache(). NULL (with iobuf_sz == 0) on a rank with no local
   * I/O data to disperse. */
  void *iobuf;
  std::size_t iobuf_sz;

  /* The CALLER's destination buffer - exactly what PIOc_read_darray()'s own
   * `array` parameter would be. NOT owned: never allocated or freed here. */
  void *array;
  std::size_t array_sz;

  int nvars;

  Hdf5_rearr_rcache(void ):
    file(NULL), varid(-1), io_desc_info(NULL),
    iobuf(NULL), iobuf_sz(0), array(NULL), array_sz(0), nvars(0) {}
  ~Hdf5_rearr_rcache();
};

int pio_iosys_async_op_hdf5_rearr_read_impl(void *pdata);
int pio_iosys_async_op_hdf5_rearr_read(void *pdata);
void pio_iosys_async_op_hdf5_rearr_read_free(void *pdata);
int pio_iosys_async_hdf5_rearr_read_op_add(file_desc_t *file, int varid, int nvars,
      std::shared_ptr<io_desc_t> iodesc, void *iobuf, std::size_t iobuf_sz,
      void *array, std::size_t array_sz);
#endif /* PIO_USE_ASYNC_WR_REARR */

#define __SPIO_ASYNC_HDF5_UTILS_HPP__
#endif // __SPIO_ASYNC_HDF5_UTILS_HPP__
