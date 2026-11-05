# HDF5 Darray Reads + CONTIG Async Writes via `async_io_desc_info` — Design Spec

**Status:** Approved by user, pending implementation plan.
**Sub-project 1 of 2.** Sub-project 2 (BOX/SUBSET rearranger support for async
writes) is a separate, later spec — see "Out of Scope" below.

## Background

`io_desc_t` (`src/clib/pio_types.hpp`) was split in a prior commit into a thin
handle holding `sync_io_desc_info` (fully populated, always used today) and
`async_io_desc_info` (a pointer, always `NULL` — pure scaffolding, never
allocated). Separately, `iosystem_desc_t::io_comm_info` (an `IO_comm_info`,
`src/clib/core/util/spio_iosys_utils.hpp`) already derives a real, distinct
asynchronous I/O communicator via `get_async_io_comm()` whenever
`PIO_USE_ASYNC_WR_THREAD` is on, but it has exactly one call site today (a
trivial liveness check in `pioc.cpp`) — nothing rearranges or moves data over
it yet.

Today, "async write" (for `PIO_IOTYPE_HDF5`/`PIO_IOTYPE_HDF5C` with
`PIO_USE_ASYNC_WR_THREAD` on) means only that an already-rearranged buffer is
written to disk by a background thread. The compute→IO rearrangement that
produces that buffer always happens synchronously, over the sync I/O
communicator, via `sync_io_desc_info` — regardless of iotype or the async
flag. `PIOc_read_darray_impl` additionally has no case for
`PIO_IOTYPE_HDF5`/`PIO_IOTYPE_HDF5C` at all; HDF5 darray reads are entirely
unimplemented (they fall through to a `PIO_EBADIOTYPE` error).

## Goal

1. Implement HDF5/HDF5C darray reads (currently nonexistent), always
   synchronous, using `sync_io_desc_info` and the sync I/O communicator —
   matching the pattern every other iotype's read path already uses.
2. For `PIO_IOTYPE_HDF5`/`PIO_IOTYPE_HDF5C` writes with
   `PIO_USE_ASYNC_WR_THREAD` on **and a CONTIG-rearranger decomposition**,
   rearrange compute→IO data over the asynchronous I/O communicator
   (`io_comm_info->get_async_io_comm()`) using `async_io_desc_info`, instead
   of the sync comm/`sync_io_desc_info`. The background thread then writes
   the already-async-rearranged buffer out, as today.
3. Leave every other combination (all non-HDF5 iotypes; HDF5 without the
   async flag; HDF5 with the async flag but a BOX/SUBSET decomposition)
   behaviorally unchanged.

## Out of Scope

- **BOX/SUBSET rearranger support for async writes.** `box_rearrange_create`,
  `box_rearrange_create_with_holes`, `subset_rearrange_create`, and the free
  functions `rearrange_comp2io`/`rearrange_io2comp` (`pio_rearrange.cpp`) are
  deeply hardwired to `iodesc->sync_io_desc_info` fields and sync-comm
  getters throughout (not just at their call sites — internally, in field
  writes and loop bounds). Making them usable against a second comm/
  io_desc_info_t is a large, separate reworking effort, planned as
  Sub-project 2. Until that lands, HDF5+async writes for BOX/SUBSET decomps
  keep today's exact behavior: synchronous rearrange via
  `sync_io_desc_info`, then the already-rearranged buffer is queued to the
  async thread pool for writing (same as every other HDF5+async write today).
- **HDF5 metadata operations** (create/def_var/put_att/enddef/set_frame).
  These are already queued to the async thread pool under
  `PIO_USE_ASYNC_WR_THREAD` but are RPC-like calls to the I/O tasks, not
  rearrangement. They stay on the sync comm; this spec is scoped strictly to
  darray data reads/writes.
- **`needs_sync_btw_sync_and_async_ops_`.** When `IO_comm_info` can't find a
  disjoint async I/O process set, it falls back to
  `MPI_Comm_dup(sync_comm)` for the async comm and sets this flag. This
  design builds `async_io_desc_info` the same way regardless — the flag is
  left for future synchronization work and is not consulted here.
- **CI changes.** The existing GitHub Actions workflow
  (`.github/workflows/spio_pnetcdf_testing_workflow.yaml`) only builds with
  PnetCDF; it has never exercised HDF5 or `PIO_USE_ASYNC_WR_THREAD` and this
  spec does not change that. Testing relies on this repo's locally
  configured HDF5+`PIO_USE_ASYNC_WR_THREAD` build tree.

## Architecture

### The selector

`io_desc_t` gains one new method:

```cpp
io_desc_info_t &select_io_desc_info(int iotype) const;
```

Declared in `src/clib/pio_types.hpp`, defined in
`src/clib/core/spio_iodesc_utils.cpp` (alongside the existing `io_desc_t`
destructor). Its rule, and the only rule this design adds:

> `(iotype == PIO_IOTYPE_HDF5 || iotype == PIO_IOTYPE_HDF5C) &&`
> `PIO_USE_ASYNC_WR_THREAD && sync_io_desc_info.rearranger == PIO_REARR_CONTIG`
> → return `*async_io_desc_info`. Otherwise → return `sync_io_desc_info`.

A `pioassert(async_io_desc_info != NULL, ...)` guards the async branch
defensively, even though the allocation rule below guarantees it is non-NULL
whenever the selector's condition is true.

This one method is the single source of truth for "which `io_desc_info_t`
and which comm does this write use" and is called at every site that
currently hardcodes `sync_io_desc_info` for write-path rearrangement or
HDF5-async execution.

### Allocating `async_io_desc_info`

`async_io_desc_info` is allocated and populated eagerly, at decomposition
init time, but **only when `PIO_USE_ASYNC_WR_THREAD` is on and the
decomposition's rearranger is `PIO_REARR_CONTIG`** — not for every
decomposition unconditionally, since for BOX/SUBSET there is no setup logic
yet to populate it with (that is Sub-project 2's job). `contig_rearranger_init`
(`pioc.cpp`), after building `sync_io_desc_info` exactly as today, additionally:

1. `new`s an `io_desc_info_t` into `iodesc->async_io_desc_info`.
2. Populates its descriptor fields (`piotype`, `map`, `maplen`, `dimlen`,
   `ndims`, etc.) with the same logical values as `sync_io_desc_info`, since
   the data decomposition itself does not change — only which I/O tasks
   receive the data changes. `io_desc_info_t` owns malloc'ed arrays and is
   non-copyable by design (shallow-copying would double-free on
   destruction), so `map`/`dimlen`/etc. must each be freshly malloc'ed and
   populated for `async_io_desc_info` — never pointer-shared with
   `sync_io_desc_info`'s arrays. Use the same allocation/population logic
   `init_iodesc_contig_rearr_fields` already uses for `sync_io_desc_info`,
   applied a second time into `async_io_desc_info`.
3. Constructs its `Contig_rearr` with the "targets async comm" flag (see
   below) set, and calls `init()` the same way `sync_io_desc_info.rearr` is
   initialized today.

If `async_io_desc_info` allocation or init fails, this is treated as a decomp
init error (same error path as a `sync_io_desc_info` rearranger init
failure today) — decomposition creation fails rather than silently falling
back to sync-only behavior, so callers never get an iodesc that silently
provides less concurrency than requested.

### `Contig_rearr` generalization

`Contig_rearr` (`src/clib/core/rearr/pio_rearr_contig.hpp/.cpp`) has ~10 call
sites hardcoded to sync-comm getters (`is_sync_io_proc()`,
`get_sync_io_comm()`, `get_sync_io_comm_sz()`, `get_sync_io_comm_rank()`),
inside `init()`/`rearr_comm_` setup and `create_agg_comm()`. It gains:

- A constructor parameter, e.g. `bool targets_async_comm` (default `false`,
  preserving today's behavior for every existing call site).
- Four small private dispatch helpers used in place of the hardcoded calls:
  `role_io_comm()`, `role_is_io_proc()`, `role_io_comm_sz()`,
  `role_io_comm_rank()` — each calls the sync or async `io_comm_info` getter
  based on the stored flag.
- The ~10 existing call sites are replaced with calls through these helpers.

No other `Contig_rearr` behavior changes. When `targets_async_comm` is
`false`, every helper calls exactly the sync getter it replaced, so existing
sync-only behavior is unchanged bit-for-bit.

## Write-path data flow

`PIOc_write_darray_multi_impl` (`pio_darray.cpp`) calls
`iodesc->select_io_desc_info(file->iotype)` once, early, getting a reference
`wdesc`. Every subsequent reference to `iodesc->sync_io_desc_info` in the
rearrangement portion of this function (the `rlen` computation, the
`spio_file_mvcache_alloc` sizing, fill-value insertion, and — for CONTIG —
the `wdesc.rearr->rearrange_comp2io(...)` call) uses `wdesc` instead. For
BOX/SUBSET decomps, `wdesc` is always `sync_io_desc_info` by the selector's
rule, so the free-function `rearrange_comp2io(ios, iodesc, file, ...)` call
is untouched and keeps reading `iodesc->sync_io_desc_info` directly as it
does today.

The existing `switch(file->iotype)` that follows (deciding whether the
already-rearranged `mv_iobuf` is written inline or queued to the async
thread pool) is unchanged in structure. Inside the HDF5+async branch:

- `pio_iosys_async_hdf5_write_op_add` calls `select_io_desc_info` itself
  (same `file->iotype`) to get `wdesc`, and gates participation on
  `wdesc`'s corresponding `is_sync_io_proc()`/`is_async_io_proc()` instead
  of unconditionally calling `is_sync_io_proc()`. It copies the rearranged
  data into its private `Hdf5_wcache` buffer and stores `&wdesc` in the new
  `Hdf5_wcache::io_desc_info` field (type `io_desc_info_t *`).
- `pio_iosys_async_op_hdf5_write` (the thread-pool execute function) reads
  `wcache->io_desc_info->{llen, mpitype_size, ndims, dimlen, piotype,
  region/fillregion, maxregions/maxfillregions}` instead of hardcoding
  `iodesc->sync_io_desc_info.*` at its ~12 existing read sites.

Because only true I/O processes under the selected comm ever enqueue (the
updated gate above) or execute (thread-pool workers only run on ranks that
enqueued), the thread-pool execution naturally happens on the correct rank
set — sync I/O ranks for everything that selects sync, async I/O ranks for
CONTIG+HDF5+async.

The `DARRAY_FILL` path (~line 521 in `pio_darray.cpp`, structurally
identical to the `DARRAY_DATA` path above) is updated the same way.

## Read-path data flow

A new case for `PIO_IOTYPE_HDF5`/`PIO_IOTYPE_HDF5C` is added to
`PIOc_read_darray_impl`'s iotype switch, dispatching to a new function
(structurally modeled on `pio_iosys_async_op_hdf5_write`'s region/shape/
datatype-conversion logic, but reading via `H5Dread`-equivalent calls
instead of writing, and fully synchronous — no thread pool, no wcache).
Sync I/O tasks read each variable's region directly from the file into a
local buffer; then exactly the same dispatch the existing non-HDF5 read
paths use applies: `sync_io_desc_info.rearr->rearrange_io2comp(...)` for
CONTIG, or the free `rearrange_io2comp(ios, iodesc, ...)` for BOX/SUBSET —
both always against the sync comm. No selector is involved on the read
side: reads always use `sync_io_desc_info`, matching the original request.

## Error Handling

- `select_io_desc_info` can only return `*async_io_desc_info` when the
  condition that gates its allocation (`PIO_USE_ASYNC_WR_THREAD` on,
  rearranger `PIO_REARR_CONTIG`) also holds, so the defensive
  `pioassert` can never fire in correct code; it exists to catch a future
  change that breaks this invariant, not an expected runtime condition.
- When `PIO_USE_ASYNC_WR_THREAD` is off (the CMake default), the selector's
  condition is always false, so it always returns `sync_io_desc_info` and
  every touched function executes exactly the same code path, over exactly
  the same comm, as it does today. This is a regression-safety property,
  not just an implementation detail, and the implementation plan's tests
  must pin it down explicitly (byte-identical behavior with the flag off).
- `async_io_desc_info` allocation/init failure during decomp creation fails
  decomposition creation outright (same severity as a `sync_io_desc_info`
  rearranger init failure today) — see "Allocating `async_io_desc_info`"
  above.
- The new HDF5 read path reuses the existing error-reporting helpers
  (`pio_err`, `check_mpi`) the same way the existing HDF5 write path does;
  no new error codes are introduced.

## Testing

- **C (tests/cunit):** extend `test_darray_async`, `test_darray_async_simple`,
  and `test_darray_async_many` to round-trip: write a CONTIG decomposition
  via HDF5 with `PIO_USE_ASYNC_WR_THREAD` on, then read it back via the new
  HDF5 read path, and assert the data matches what was written. Add
  regression coverage confirming sync-build behavior (flag off) is
  unchanged.
- **Fortran (tests/general):** `tests/general/util/pio_tutil.F90` already
  enumerates `PIO_IOTYPE_HDF5`/`HDF5C` as iotypes for the generic decomp
  tests (`pio_decomp_tests*.F90`, `pio_sync_tests.F90`, etc.) whenever built
  with `_HDF5` — these tests already attempt HDF5 writes today but cannot
  currently verify them via read-back, since the read case doesn't exist.
  The implementation plan must first determine exactly how these tests
  currently handle the missing HDF5 read case (silently skipped assertion,
  or a tolerated/ignored error) before relying on them, since adding the
  read case will make a previously-dormant assertion active; any test that
  turns out to depend on the old failure behavior needs updating alongside
  the fix.
- All testing runs against this repo's locally configured
  HDF5+`PIO_USE_ASYNC_WR_THREAD`-enabled `build/` tree; no CI workflow
  changes.

## Summary of Scope Decisions

| Question | Decision |
|---|---|
| HDF5 darray reads (currently nonexistent) | Implement now, sync-only |
| Rearranger scope for async writes | CONTIG only in this spec; BOX/SUBSET is Sub-project 2 |
| `async_io_desc_info` init timing | Eager at decomp init, but only for CONTIG decomps with the async flag on |
| `needs_sync_btw_sync_and_async_ops_` fallback | Not specially handled; async_io_desc_info built the same way regardless |
| HDF5 metadata ops (create/def_var/enddef/...) | Out of scope; stay on sync comm |
| Test coverage | Extend existing local HDF5+async test suite; no CI changes |
