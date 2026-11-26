# HDF5 Async Writes for BOX/SUBSET Rearrangers — Design Spec

**Status:** Approved by user, pending implementation plan.
**Sub-project 2 of 2.** Sub-project 1
(`docs/superpowers/specs/2026-10-07-hdf5-async-contig-write-design.md`)
implemented HDF5 darray reads (sync-only, all rearrangers) and HDF5 async
writes for `PIO_REARR_CONTIG` only. This spec extends async writes to
`PIO_REARR_BOX` and `PIO_REARR_SUBSET`.

## Background

Sub-project 1 built `async_io_desc_info` and the whole sync/async selection
machinery (`io_desc_t::select_io_desc_info()`, `IO_comm_info`'s symmetric
sync/async comm accessors, the HDF5 async write-op dispatch), but gated it to
`PIO_REARR_CONTIG` only, because `Contig_rearr` already had a clean,
object-based "which comm do I target" seam (a constructor flag), while the
BOX/SUBSET rearrange machinery does not. That machinery
(`src/clib/core/rearr/pio_rearrange.cpp`) is a set of free functions —
`compute_counts`, `define_iodesc_datatypes`, `box_rearrange_create`,
`box_rearrange_create_with_holes`, `default_subset_partition`,
`subset_rearrange_create`, `rearrange_comp2io`, `rearrange_io2comp` — that
read and write `iodesc->sync_io_desc_info` fields directly, by name, at
dozens of internal call sites (not just at their own call sites), and call
`ios->io_comm_info`'s sync-only accessors (`get_sync_io_comm()`,
`get_sync_io_comm_sz()`, `get_sync_io_comm_rank()`, `get_sync_io_ranks()`,
`is_sync_io_proc()`) directly throughout. `box_rearranger_init()`/
`subset_rearranger_init()` (`src/clib/core/pioc.cpp:536-604`) call these
functions exactly once each, always targeting sync, with no async
counterpart.

Confirmed unchanged from Sub-project 1 and out of scope for further work
here:
- **Read path.** `rearrange_io2comp` and `PIOc_read_darray_impl`'s HDF5 case
  are already fully generic across rearrangers and always synchronous, via
  `sync_io_desc_info` and the sync I/O comm. No code changes.
- **The selector, comm-accessor API, and HDF5 async write-op dispatch.**
  `select_io_desc_info()`, `IO_comm_info`, and `Hdf5_wcache`/
  `pio_iosys_async_hdf5_write_op_add`/`pio_iosys_async_op_hdf5_write` consume
  an `io_desc_info_t *` purely by pointer identity (`== iodesc->async_io_desc_info`
  or not) — never by rearranger type. They need no changes to support
  BOX/SUBSET once those rearrangers populate `async_io_desc_info`.

## Goal

For `PIO_IOTYPE_HDF5`/`PIO_IOTYPE_HDF5C` writes with
`PIO_USE_ASYNC_WR_THREAD` on and a `PIO_REARR_BOX` or `PIO_REARR_SUBSET`
decomposition: rearrange compute→IO data over the asynchronous I/O
communicator (`io_comm_info->get_async_io_comm()`) into a populated
`async_io_desc_info`, instead of falling through to the sync-only rearrange
today's code always uses. The background thread then writes the
already-async-rearranged buffer out, exactly as it does today for CONTIG.

`PIO_REARR_ANY` requires no code changes: it resolves to `PIO_REARR_BOX` or
`PIO_REARR_SUBSET` at `pioc.cpp:866`, before decomposition-specific
rearranger init runs, so it inherits whichever behavior its resolved
rearranger has.

## Out of Scope

- **Shared-procs optimization.** `box_rearranger_init()`/
  `subset_rearranger_init()` always build a full, separate async rearrange
  state when `PIO_USE_ASYNC_WR_THREAD` is on, regardless of whether
  `sync_and_async_io_share_procs()` is true — matching `contig_rearranger_init()`'s
  existing unconditional-dual-build precedent. No aliasing/reuse
  optimization for the shared-procs case is attempted here, and none is
  planned.
- **`needs_sync_btw_sync_and_async_ops_`.** Same treatment as Sub-project 1:
  not specially handled; `async_io_desc_info` is built the same way
  regardless of whether `IO_comm_info` fell back to `MPI_Comm_dup(sync_comm)`
  for the async comm.
- **HDF5 metadata operations** (create/def_var/put_att/enddef/set_frame).
  Unchanged; out of scope for both sub-projects.
- **CI changes.** No changes to `.github/workflows/`; testing relies on this
  repo's locally configured HDF5+`PIO_USE_ASYNC_WR_THREAD` build tree, as in
  Sub-project 1.

## Architecture

### 1. Dispatch changes (`spio_iodesc_utils.cpp`, `pio_darray.cpp`)

**`select_io_desc_info()`** (`src/clib/core/spio_iodesc_utils.cpp:240-280`)
drops its `sync_io_desc_info.rearranger == PIO_REARR_CONTIG` clause. The sole
remaining gate is `async_io_desc_info != NULL`, which is `false` for any
decomposition whose rearranger hasn't had its async init path populate it —
so this change is a safe no-op until the init-time changes below run. The
existing disjoint-procs fallback (returns `sync_io_desc_info` when
`!sync_and_async_io_share_procs()`, since HDF5 file/dataset handles exist
only on sync IO ranks) is unchanged — it was already rearranger-agnostic.

**Write-dispatch gap fix** (`src/clib/core/pio_darray.cpp:354-364`). Today,
only CONTIG's write path honors `select_io_desc_info()`'s choice (via
`Contig_rearr`'s own stored comm-target flag); BOX/SUBSET always call the
sync-only free function `rearrange_comp2io()` regardless of what was
selected. Fix: at this call site, derive
`bool targets_async = (&write_desc_info == iodesc->async_io_desc_info)` and
pass it into `rearrange_comp2io()` as a new parameter (folded into the
`Io_comm_target`/`io_desc_info_t&` parameterization below, not a bare bool
threaded further).

**`PIO_REARR_ANY`** needs no special-casing anywhere in this design: it is
resolved to `PIO_REARR_BOX` or `PIO_REARR_SUBSET` at `pioc.cpp:866`, strictly
before any rearranger-init or dispatch code described here runs.

### 2. Rearrange-function generalization (`pio_rearrange.cpp`, `pioc.cpp`)

**New helper type**, added alongside the free functions it parameterizes:

```cpp
struct Io_comm_target {
    MPI_Comm comm;
    int comm_sz;
    int rank;
    int root;                          // comm-local rank of the IO root
    const std::vector<int> &io_ranks;  // positions within union_comm
    bool is_io_proc;
};
```

Two small factory helpers, `make_sync_io_comm_target(ios)` and
`make_async_io_comm_target(ios)`, build one from `ios->io_comm_info`'s
existing sync/async accessors. Callers resolve a target once per call site
and pass it down — an explicit tuple, not a stored/threaded bool flag,
matching `Contig_rearr`'s role but without requiring an object to hold the
flag.

**Mechanical signature change.** Every BOX/SUBSET free function that
hardcodes `iodesc->sync_io_desc_info` and `ios->io_comm_info->get_sync_io_*()`
gains two parameters: `io_desc_info_t &desc_info` and
`const Io_comm_target &io_target`. Internally, every `iodesc->sync_io_desc_info`
reference becomes `desc_info`, and every `ios->io_comm_info->get_sync_io_comm()`/
`_sz()`/`_rank()`/`get_sync_io_ranks()`/`is_sync_io_proc()` call becomes the
corresponding `io_target.*` field access. Functions changed:
`define_iodesc_datatypes`, `compute_counts`, `rearrange_comp2io`,
`box_rearrange_create`, `box_rearrange_create_with_holes`,
`default_subset_partition`, `subset_rearrange_create`. **`rearrange_io2comp`
is not touched** — per Background, it stays hardcoded to sync, since reads
are confirmed always-synchronous regardless of rearranger.

At every existing (sync) call site, callers pass
`make_sync_io_comm_target(ios)` and `iodesc->sync_io_desc_info` — reproducing
today's exact behavior bit-for-bit.

**`box_rearranger_init()`/`subset_rearranger_init()`**
(`src/clib/core/pioc.cpp:536-604`) each gain a second pass under
`#if PIO_USE_ASYNC_WR_THREAD`, structurally mirroring
`contig_rearranger_init()`'s existing CONTIG pattern (`pioc.cpp:681-703`):

1. Allocate `iodesc->async_io_desc_info` via one shared helper used by all
   three `*_rearranger_init()` functions (the allocation shape — `new
   io_desc_info_t(ios, piotype, ndims, dimlen, maplen, map, rearranger,
   /*map_zero_based=*/true)` plus copying the same descriptor fields
   CONTIG's `init_iodesc_contig_rearr_fields` copies — is identical across
   all three rearrangers; `io_desc_info_t` is non-copyable and owns
   malloc'ed arrays, so these are freshly allocated, never pointer-shared
   with `sync_io_desc_info`'s arrays).
2. Call the now-generalized create function(s) with
   `make_async_io_comm_target(ios)` and `*iodesc->async_io_desc_info`,
   **unconditionally** — no `sync_and_async_io_share_procs()` gating (see Out
   of Scope).

If this async build fails, decomposition creation fails outright — same
severity as a `sync_io_desc_info` rearranger-init failure today, and the
same policy Sub-project 1 adopted for CONTIG.

**SUBSET's `subset_comm`.** `default_subset_partition()` builds its
communicator via `MPI_Comm_split()` keyed on sync-IO-proc membership. The
async pass calls the generalized version keyed on async-IO-proc membership
instead (`io_target.is_io_proc`), writing into a distinct
`async_io_desc_info->subset_comm` — a separate `MPI_Comm`, split once at init
time, never aliased with the sync one.

### 3. HDF5 async write-op compatibility

No changes needed in `spio_async_hdf5_utils.cpp`. `Hdf5_wcache` stores a
generic `io_desc_info_t *io_desc_info`, selected once via
`select_io_desc_info()` at op-construction time, and is distinguished
downstream only by pointer identity (`== iodesc->async_io_desc_info`), never
by rearranger. Once Section 2's init-time changes populate
`async_io_desc_info` for BOX/SUBSET, the existing write-op consumer picks it
up with no modification.

## Write-path data flow

`PIOc_write_darray_multi_impl` (`src/clib/core/pio_darray.cpp:122`) already
resolves `iodesc->select_io_desc_info(file->iotype)` into `write_desc_info` generically
for every rearranger. With Section 1's selector fix, this now correctly
yields `*async_io_desc_info` for HDF5/HDF5C + async + BOX/SUBSET too. The
dispatch at `pio_darray.cpp:354-364` becomes:

```cpp
if(write_desc_info.rearranger == PIO_REARR_CONTIG){
  ierr = write_desc_info.rearr->rearrange_comp2io(...);
}
else{
  bool targets_async = (&write_desc_info == iodesc->async_io_desc_info);
  const Io_comm_target io_target = targets_async
      ? make_async_io_comm_target(ios) : make_sync_io_comm_target(ios);
  ierr = rearrange_comp2io(ios, iodesc, file, array, mv_iobuf, nvars,
                            write_desc_info, io_target);
}
```

From there, `pio_iosys_async_hdf5_write_op_add` and the thread-pool execute
function behave exactly as Sub-project 1 built them (Section 3 above) — they
already operate generically on whichever `io_desc_info_t *` was selected.

## Read-path data flow

No changes. `rearrange_io2comp` and `PIOc_read_darray_impl`'s HDF5 case
remain fully generic across rearrangers and always synchronous — confirmed
unchanged in Background.

## Error Handling

- The disjoint-procs fallback in `select_io_desc_info()` means that for
  disjoint sync/async IO proc sets, HDF5/HDF5C writes always select
  `sync_io_desc_info` regardless of rearranger — so a disjoint-procs
  `async_io_desc_info` for BOX/SUBSET is built (per the "always build both"
  decision) but never selected for HDF5. This mirrors CONTIG's existing,
  already-accepted behavior exactly; it is a known characteristic, not a new
  gap introduced here.
- Async rearrange-build failure during decomposition creation fails
  decomposition creation outright, matching Sub-project 1's policy for
  CONTIG (see Architecture, Section 2).
- When `PIO_USE_ASYNC_WR_THREAD` is off, none of this code compiles in
  (`#if PIO_USE_ASYNC_WR_THREAD` guards the async-build passes), and
  `select_io_desc_info()`'s `async_io_desc_info != NULL` check is always
  false, so every touched function executes exactly the same code path, over
  exactly the same comm, as it does today.

## Testing

- **New example tests**, mirroring `examples/c/test_hdf5_async_contig.c`
  (which hardcodes `const int rearranger = PIO_REARR_CONTIG;`):
  - `test_hdf5_async_box.c` — same structure, `rearranger = PIO_REARR_BOX`.
  - `test_hdf5_async_subset.c` — same structure,
    `rearranger = PIO_REARR_SUBSET`, run with `NUMPROCS` ≥ 4 so the subset
    partition is non-trivial, matching existing sync SUBSET test
    conventions.
  Both registered in `examples/c/CMakeLists.txt` under the same
  `WITH_HDF5 AND PIO_USE_ASYNC_WR_THREAD` gate as the existing CONTIG test
  (`CMakeLists.txt:118-119`). A short comment in one of these files notes
  that `PIO_REARR_ANY` needs no dedicated test, since it resolves to
  BOX/SUBSET before any of this design's code runs.
- **Unit-level coverage.** Add cases verifying `iodesc->async_io_desc_info`
  is non-NULL and correctly populated after `box_rearranger_init()`/
  `subset_rearranger_init()` when `PIO_USE_ASYNC_WR_THREAD` is on, for both
  rearrangers, in both shared-procs and disjoint-procs IO configurations.
- **Regression check.** Re-run the existing sync BOX/SUBSET HDF5 tests, with
  `PIO_USE_ASYNC_WR_THREAD` both `OFF` and `ON`, to confirm the sync write
  path is byte-identical to today's behavior — the refactor only adds
  parameters and an additional async pass; every existing (sync) call site
  passes `make_sync_io_comm_target(ios)` and `iodesc->sync_io_desc_info`.
- All testing runs against this repo's locally configured
  HDF5+`PIO_USE_ASYNC_WR_THREAD`-enabled `build/` tree; no CI workflow
  changes.

## Summary of Scope Decisions

| Question | Decision |
|---|---|
| Rearranger scope | `PIO_REARR_BOX` and `PIO_REARR_SUBSET`; `PIO_REARR_ANY` needs no changes (resolves to BOX/SUBSET before init) |
| How to generalize the ~15 BOX/SUBSET free-function call sites | Explicit `Io_comm_target` tuple parameter + `io_desc_info_t&` parameter, resolved once per call site — not a stored/threaded bool flag |
| Async rearrange state when sync/async IO procs are shared vs. disjoint | Always build both unconditionally, regardless of `sync_and_async_io_share_procs()` — matches CONTIG precedent, including its accepted "disjoint-case async state is built but never selected for HDF5" characteristic |
| Read path | No changes; already generic and sync-only across all rearrangers |
| HDF5 async write-op dispatch (`spio_async_hdf5_utils.cpp`) | No changes; already generic by `io_desc_info_t` pointer identity |
| Test coverage | New `test_hdf5_async_box.c`/`test_hdf5_async_subset.c` examples mirroring the CONTIG one; unit coverage for the init-time async build; sync-path regression check |
