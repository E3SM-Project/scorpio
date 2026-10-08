# HDF5 Async CONTIG Write: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `async_io_desc_info` a real, populated `io_desc_info_t` for CONTIG-rearranger decompositions when `PIO_USE_ASYNC_WR_THREAD` is enabled, and route HDF5/HDF5C writes through it when appropriate, while every other I/O path (all reads, all non-CONTIG rearrangers, all non-HDF5 iotypes) continues to use `sync_io_desc_info` exactly as today.

**Architecture:** `io_desc_t` gains a `select_io_desc_info(int iotype)` method that is the single source of truth for which `io_desc_info_t` a write should target. At decomposition-init time, if the rearranger is `PIO_REARR_CONTIG` and `PIO_USE_ASYNC_WR_THREAD` is on, a second `io_desc_info_t` plus a second `Contig_rearr` (constructed in an "async" role) are eagerly built alongside the existing synchronous ones — eagerly because the iotype of the file(s) that will eventually use this decomposition is not known until write time. `Contig_rearr` is extended so a single class can serve either the sync IO communicator or the async IO communicator, selected by a constructor flag and resolved through 4 small dispatch helpers. The write path (`PIOc_write_darray_multi_impl`'s DARRAY_DATA and DARRAY_FILL switches) is converted from a compile-time `#if/#else` split to a runtime call to `select_io_desc_info` plus a pointer-identity check, so the decision of sync-vs-async is made once, centrally, and reused via `==` comparison wherever downstream code needs to know which path it is on. `Hdf5_wcache` is given a new field caching the result so the async write thread sees the same decision made at enqueue time. The HDF5 read path is untouched in spirit — it keeps using `sync_io_desc_info` — but is extended to actually support `PIO_IOTYPE_HDF5`/`HDF5C`, which today fall through to `PIO_EBADIOTYPE`, by adding a case to the existing shared `pio_read_darray_nc` function (already used by NETCDF4P/NCZARR/PNETCDF) and to `PIOc_read_darray_impl`'s outer switch.

**Tech Stack:** C++ (library core), C (public API, test harness), CMake/CTest, MPI, HDF5 (`WITH_HDF5`), CUnit.

**Spec:** `docs/superpowers/specs/2026-10-07-hdf5-async-contig-write-design.md`

## Global Constraints

- Scope is strictly `PIO_REARR_CONTIG` + `PIO_IOTYPE_HDF5`/`PIO_IOTYPE_HDF5C` writes. No other rearranger, no other iotype, and no read path changes behavior.
- All HDF5/HDF5C **reads** remain synchronous always — adding read support for these iotypes is a correctness fix (they currently return `PIO_EBADIOTYPE`), not a new async read feature.
- No CI workflow changes. No changes to `.github/workflows`.
- Metadata operations (`def_var`, `put_att`, `enddef`, etc.) are out of scope — only darray (`write_darray_multi`/`read_darray`) is touched.
- No explicit cross-communicator synchronization is introduced for `needs_sync_btw_sync_and_async_ops_`-style concerns beyond what `nasync_pend_ops` and the existing async thread-pool barrier already provide.
- `ios->async` IO-service (compute/IO process split) messaging is untouched; this work is orthogonal to that mechanism.
- When `PIO_USE_ASYNC_WR_THREAD` is off, `async_io_desc_info` must remain `NULL` and behavior must be bit-for-bit identical to before this change (regression safety).
- All work is committed directly to `jayeshkrishna/fix_num_threads`. Do not merge to `develop` or `master`. Do not push.

## Review Focus

- `maplen == 0` edge case for the eagerly-constructed async `io_desc_info_t` — must not crash or misbehave when a task has no local data.
- Leak/exception safety if async `Contig_rearr::init()` throws partway through decomposition init — confirmed safe today because `io_desc_t` is held in a `std::shared_ptr` and its destructor unconditionally does `if(async_io_desc_info != NULL){ delete async_io_desc_info; }`; any change to construction order must preserve this.
- Flag-off (`PIO_USE_ASYNC_WR_THREAD == 0`) regression safety — every new runtime branch must collapse to exactly today's behavior when the flag is off.
- `io_desc_info_t &wdesc` reference lifetime at call sites — it must not outlive the `io_desc_t` it was selected from, and must not be captured across an async boundary without going through the `Hdf5_wcache` field.
- Concurrent sync + async `nasync_pend_ops` counting — a file could in principle have synchronous writes (non-CONTIG variables) and asynchronous writes (CONTIG variables) interleaved; the counter and `PIOc_freedecomp_impl`'s "let I/O desc be freed during finalize" deferral must still be correct.
- The pointer-identity idiom `is_async = (&wdesc == iodesc->async_io_desc_info)` — verify it is computed from the exact same `iodesc` the write call operates on (shared_ptr vs raw pointer mismatches would break `==`).
- Native-type-sized read-buffer correctness in the new HDF5 read case (`H5Tget_native_type` + `DTConverter::size_of`) — must match the write path's `file_var_ntype_id` derivation exactly, or round-trip data will be sized/interpreted incorrectly.
- Sharing `startlist`/`countlist`/`rrlen`-shaped locals between the existing PNETCDF branch and the new HDF5 branch inside `pio_read_darray_nc` — ensure no aliasing or double-free, since `bget`/`brel` ownership must stay scoped to whichever branch allocated them.
- `create_agg_comm()` (`pio_rearr_contig.cpp`) needs special attention in Task 2: unlike the other 8 call sites, its `color`/`key` derivation depends on `ios_->num_uniontasks` divided by the IO communicator size. This is not a pure 1:1 getter substitution — the async-role variant must derive `agg_range_sz` from the **async** IO comm size when `targets_async_comm_` is true, and the reviewer should double check the resulting split communicator is still correct for the async IO comm's rank space (not the sync comm's).

---

### Task 1: `select_io_desc_info` selector method

**Context:** `io_desc_t` currently has only `sync_io_desc_info` (always valid) and `async_io_desc_info` (always `NULL` today). We need one well-defined place that decides, given an iotype, which `io_desc_info_t` a write should use. Every later task (write-path integration, `Hdf5_wcache` population) calls through this method rather than re-deriving the condition.

**Files:**
- `src/clib/pio_types.hpp` (declare on `io_desc_t`, after the existing `~io_desc_t()` declaration at line 394)
- `src/clib/core/spio_iodesc_utils.cpp` (define, near the `io_desc_t` constructor/destructor at lines 224-238)
- New: `tests/cunit/test_spio_select_io_desc_info.cpp`
- `tests/cunit/CMakeLists.txt` (register the new test — see Task 6)

**Steps:**
- [ ] In `src/clib/pio_types.hpp`, add to `struct io_desc_t` (after `~io_desc_t();`):
  ```cpp
  /* Returns the io_desc_info_t to use for a write/read of the given iotype:
   * async_io_desc_info for CONTIG-rearranger HDF5/HDF5C writes when async
   * writes are enabled and have been allocated, sync_io_desc_info otherwise. */
  io_desc_info_t &select_io_desc_info(int iotype);
  ```
- [ ] In `src/clib/core/spio_iodesc_utils.cpp`, add the definition directly after the `io_desc_t` destructor (after line 238):
  ```cpp
  io_desc_info_t &io_desc_t::select_io_desc_info(int iotype)
  {
  #if PIO_USE_ASYNC_WR_THREAD
    if(((iotype == PIO_IOTYPE_HDF5) || (iotype == PIO_IOTYPE_HDF5C)) &&
        (async_io_desc_info != NULL) &&
        (sync_io_desc_info.rearranger == PIO_REARR_CONTIG)){
      return *async_io_desc_info;
    }
  #endif
    return sync_io_desc_info;
  }
  ```
  Note the `async_io_desc_info != NULL` guard is defensive: it is always true for CONTIG decomps once Task 3 lands and the flag is on, but keeps this method safe to call even before Task 3's allocation exists or if allocation is ever skipped (e.g. construction failure paths).
- [ ] Add `#include "pio_rearr_contig.hpp"` to `spio_iodesc_utils.cpp` if not already present (it is, per existing includes) — no action needed, just confirm during implementation.
- [ ] Write `tests/cunit/test_spio_select_io_desc_info.cpp`, gated by `#if PIO_USE_ASYNC_WR_THREAD` for the async-returning assertions (always buildable, but async-specific checks only run when the flag is on). Construct an `io_desc_t` for `PIO_REARR_CONTIG` and one for `PIO_REARR_SUBSET` (small maplen, 1-2 MPI ranks) via the mock-friendly real constructor, and assert:
  - For `PIO_REARR_SUBSET`: `select_io_desc_info(PIO_IOTYPE_HDF5)` and `select_io_desc_info(PIO_IOTYPE_NETCDF4P)` both return `&sync_io_desc_info`.
  - For `PIO_REARR_CONTIG` with the flag off (or `async_io_desc_info == NULL`): `select_io_desc_info(PIO_IOTYPE_HDF5)` returns `&sync_io_desc_info`.
  - For `PIO_REARR_CONTIG` with the flag on and `async_io_desc_info` manually set to a non-NULL stub: `select_io_desc_info(PIO_IOTYPE_HDF5)` and `select_io_desc_info(PIO_IOTYPE_HDF5C)` return `&*async_io_desc_info`; `select_io_desc_info(PIO_IOTYPE_NETCDF4P)` still returns `&sync_io_desc_info` (non-HDF5 iotype).

**Verify:** `make test_spio_select_io_desc_info && ctest -R test_spio_select_io_desc_info`.

---

### Task 2: `Contig_rearr` sync/async role support

**Context:** `Contig_rearr` currently hardcodes `ios_->io_comm_info->get_sync_io_comm*()` at 9 call sites. To let an async-role `Contig_rearr` drive a second, async-targeted `io_desc_info_t`, it needs a constructor flag and 4 small dispatch helpers that pick sync or async IO-comm accessors based on that flag.

**Files:**
- `src/clib/core/rearr/pio_rearr_contig.hpp`
- `src/clib/core/rearr/pio_rearr_contig.cpp`
- `src/clib/core/util/spio_iosys_utils.hpp` / `.cpp` (new async-aware mock constructor overload for tests)
- New: `tests/cunit/test_spio_rearr_contig_async.cpp`
- `tests/cunit/CMakeLists.txt` (register — see Task 6)

**Steps:**
- [ ] In `pio_rearr_contig.hpp`, add a private member `bool targets_async_comm_;` and change the constructor to:
  ```cpp
  Contig_rearr(iosystem_desc_t *ios, bool targets_async_comm = false);
  ```
  Add 4 private dispatch helper declarations:
  ```cpp
  MPI_Comm role_io_comm(void ) const;
  bool role_is_io_proc(void ) const;
  int role_io_comm_sz(void ) const;
  int role_io_comm_rank(void ) const;
  ```
- [ ] In `pio_rearr_contig.cpp`, implement the constructor to store `targets_async_comm_`, and implement the 4 helpers:
  ```cpp
  MPI_Comm Contig_rearr::role_io_comm(void ) const
  {
    return targets_async_comm_ ? ios_->io_comm_info->get_async_io_comm()
                                : ios_->io_comm_info->get_sync_io_comm();
  }

  bool Contig_rearr::role_is_io_proc(void ) const
  {
    return targets_async_comm_ ? ios_->io_comm_info->is_async_io_proc()
                                : ios_->io_comm_info->is_sync_io_proc();
  }

  int Contig_rearr::role_io_comm_sz(void ) const
  {
    return targets_async_comm_ ? ios_->io_comm_info->get_async_io_comm_sz()
                                : ios_->io_comm_info->get_sync_io_comm_sz();
  }

  int Contig_rearr::role_io_comm_rank(void ) const
  {
    return targets_async_comm_ ? ios_->io_comm_info->get_async_io_comm_rank()
                                : ios_->io_comm_info->get_sync_io_comm_rank();
  }
  ```
  (Exact `is_async_io_proc()`/`is_sync_io_proc()` naming must match `IO_comm_info`'s actual method names — confirm against `spio_iosys_utils.hpp` during implementation; the class exposes `is_sync_io_proc()` and the mirrored async equivalent.)
- [ ] Replace the 8 straightforward call sites with the matching helper (1:1 substitution, same semantics, just sync→async-aware):
  - `.hpp` line 86 (`get_rearrange_buf_sz`): `ios_->io_comm_info->get_sync_io_comm_sz()` → `role_io_comm_sz()`.
  - `.cpp` `create_rearr_comm` lines 962, 966: → `role_io_comm()` / `role_io_comm_sz()` as appropriate.
  - `.cpp` `set_rearr_comm_iochunk_sz` line 989: → `role_io_comm_sz()`.
  - `.cpp` `get_rearr_toproc_map` line 1038: → `role_io_comm_sz()`.
  - `.cpp` `setup_data_rearr_info` line 1072: → `role_is_io_proc()`.
  - `.cpp` `init_rearr_send_types` line 1205: → `role_io_comm()` (or whichever accessor matches the original call).
  - `.cpp` `init_rearr_recvd_types` line 1266: → matching helper.
  - `.cpp` `rearrange_data` line 1338: → `role_io_comm()`.
  (During implementation, re-read each exact line to match the original accessor — `get_sync_io_comm()` vs `get_sync_io_comm_sz()` vs `is_sync_io_proc()` — with its corresponding `role_*` helper; do not change which quantity is fetched, only its sync/async source.)
- [ ] Handle `create_agg_comm()` (lines 624/626/628/631) carefully per the Review Focus note above: replace `ios_->io_comm_info->get_sync_io_comm_sz()` in the `agg_range_sz` derivation with `role_io_comm_sz()`, and the `get_sync_io_comm()`/sync-rank references used for `MPI_Comm_split` color/key with `role_io_comm()`/`role_io_comm_rank()`. Because this function's color/key math is bespoke (not a pure getter call), re-read the full function body during implementation and verify the resulting communicator split is dimensionally consistent when `targets_async_comm_` is true (i.e. `ios_->num_uniontasks` relates correctly to the async IO comm's size, not the sync one).
- [ ] In `spio_iosys_utils.hpp`/`.cpp`, add a second mock-constructor overload to `IO_comm_info` that accepts async comm parameters in addition to the existing sync-only mock parameters, modeled directly on the existing sync-only mock constructor:
  ```cpp
  /* Test-only mock constructor: sync comm only, async comm left unset/invalid */
  IO_comm_info(MPI_Comm sync_io_comm, int sync_io_comm_rank, int sync_io_comm_sz,
               MPI_Group sync_io_comm_group, std::vector<int> sync_io_ranks);

  /* Test-only mock constructor: both sync and async comms explicitly provided */
  IO_comm_info(MPI_Comm sync_io_comm, int sync_io_comm_rank, int sync_io_comm_sz,
               MPI_Group sync_io_comm_group, std::vector<int> sync_io_ranks,
               MPI_Comm async_io_comm, int async_io_comm_rank, int async_io_comm_sz,
               MPI_Group async_io_comm_group, std::vector<int> async_io_ranks);
  ```
  Implement the new overload by calling `sync_comm_info_.set(...)` and `async_comm_info_.set(...)` with `owns_resources = false` (consistent with how the existing mock constructor avoids taking ownership of caller-managed test communicators/groups).
- [ ] Write `tests/cunit/test_spio_rearr_contig_async.cpp`, gated under `#if PIO_USE_ASYNC_WR_THREAD` (the whole file compiles only when the flag is on, matching the `test_mtq` precedent in `tests/cunit/CMakeLists.txt`). Use `MPI_Comm_dup`/`MPI_Comm_split` to create two distinct communicators simulating sync and async IO comms (they can both be subsets of `MPI_COMM_WORLD` for the test), construct a mock `iosystem_desc_t` with the new async-aware `IO_comm_info` mock constructor, then:
  - Construct `Contig_rearr(ios, false)` and `Contig_rearr(ios, true)` from the same `ios`.
  - Call `get_rearrange_buf_sz()` (or another cheap public accessor that routes through `role_io_comm_sz()`) on both and assert the sync-role instance's result matches the sync comm's size and the async-role instance's result matches the async comm's size (when they differ, e.g. construct the two comms with different sizes/subsets).
  - This exercises the dispatch helpers without requiring a full `init()`/rearrange round trip.

**Verify:** `make test_spio_rearr_contig_async && ctest -R test_spio_rearr_contig_async` (and the pre-existing `test_spio_rearr_contig${nproc}` tests must still pass unmodified, confirming the default `targets_async_comm = false` path is unchanged).

---

### Task 3: Eager `async_io_desc_info` allocation for CONTIG decomps

**Context:** `async_io_desc_info` must be populated at decomposition-init time (not write time) because by write time we only have `iodesc`, not knowledge of which iotype will eventually be used — and because allocating communicators/rearranger state per-write would be wasteful. This only happens for `PIO_REARR_CONTIG` when `PIO_USE_ASYNC_WR_THREAD` is on; the iotype restriction (HDF5/HDF5C) is applied later, in `select_io_desc_info` (Task 1), not here.

**Files:**
- `src/clib/core/pioc.cpp` (`init_iodesc_contig_rearr_fields`, lines 606-651; `contig_rearranger_init`, lines 653-674)

**Steps:**
- [ ] Generalize `init_iodesc_contig_rearr_fields` to operate on an explicit `io_desc_info_t &desc_info` and a `bool targets_async_comm` parameter instead of assuming `iodesc->sync_io_desc_info`:
  ```cpp
  static int init_iodesc_contig_rearr_fields(iosystem_desc_t *ios, io_desc_t *iodesc,
                                              io_desc_info_t &desc_info,
                                              bool targets_async_comm)
  ```
  Update its body (currently reading/writing fields on `iodesc->sync_io_desc_info` directly) to use `desc_info` throughout, and pass `targets_async_comm` through to wherever it constructs or queries the `Contig_rearr` role (if any role-dependent logic lives directly in this function rather than inside `Contig_rearr` itself — confirm exact body during implementation since it was last fully read in a prior session).
- [ ] Update `contig_rearranger_init` so it:
  1. Keeps building `iodesc->sync_io_desc_info.rearr = new SPIO::DataRearr::Contig_rearr(ios);` and calling `init_iodesc_contig_rearr_fields(ios, iodesc, iodesc->sync_io_desc_info, false)` exactly as today (no behavior change for the sync path).
  2. Adds, immediately after, a new block:
     ```cpp
     #if PIO_USE_ASYNC_WR_THREAD
       iodesc->async_io_desc_info = new io_desc_info_t(ios, iodesc->sync_io_desc_info.piotype,
         iodesc->sync_io_desc_info.ndims, iodesc->sync_io_desc_info.dimlen,
         iodesc->sync_io_desc_info.maplen, iodesc->sync_io_desc_info.map,
         iodesc->sync_io_desc_info.rearranger, /*map_zero_based=*/true);
       iodesc->async_io_desc_info->rearr = new SPIO::DataRearr::Contig_rearr(ios, /*targets_async_comm=*/true);
       ret = iodesc->async_io_desc_info->rearr->init(iodesc->async_io_desc_info, ...);
       if(ret != PIO_NOERR){ return ret; }
       ret = init_iodesc_contig_rearr_fields(ios, iodesc, *(iodesc->async_io_desc_info), true);
       if(ret != PIO_NOERR){ return ret; }
     #endif
     ```
     (`...` stands for whatever additional arguments `Contig_rearr::init()` takes today for the sync path — mirror them exactly, substituting the async `io_desc_info_t` where the sync one is passed.)
  3. Passing `map_zero_based=true` here is deliberate: `iodesc->sync_io_desc_info.map` has already been transformed by `init_map()` during the sync `io_desc_info_t`'s construction (0-based for CONTIG). Passing it again with `map_zero_based=true` makes the async `io_desc_info_t`'s own `init_map()` take the "already zero-based" branch (a straight `std::copy`, no re-transformation), giving the async copy an identical map without re-deriving it from the original 1-based `compmap`.
  4. If the async block fails (`ret != PIO_NOERR`), simply `return ret;` — no manual cleanup of `iodesc->async_io_desc_info` is needed because the caller holds `iodesc` in a `std::shared_ptr` which will invoke `~io_desc_t()` on the error path, and that destructor already unconditionally handles a non-NULL `async_io_desc_info`.
- [ ] Confirm (re-read during implementation) that `contig_rearranger_init`'s signature and call sites do not need to change — this is purely additive inside the function body.

**Verify:** A new or extended CUnit/general test that creates a `PIO_REARR_CONTIG` decomposition with `PIO_USE_ASYNC_WR_THREAD` on and asserts `iodesc->async_io_desc_info != NULL`, with `maplen`/`ndims`/`dimlen` matching `sync_io_desc_info`'s. Also test the `maplen == 0` edge case (a rank with no local data) does not crash. Also verify with the flag off that `async_io_desc_info` stays `NULL` (existing behavior, should already pass without changes — include as a regression check).

---

### Task 4: Write-path integration

**Context:** This is where the decision made by `select_io_desc_info` actually changes which code path a write takes. Two call sites in `pio_darray.cpp` currently use `#if PIO_USE_ASYNC_WR_THREAD`/`#else` to compile-time-select between the async enqueue function and the synchronous collective write function; both become a single runtime branch. `Hdf5_wcache` and the async write/enqueue functions need to know which `io_desc_info_t` the write targets.

**Files:**
- `src/clib/core/pio_darray.cpp` (DARRAY_DATA switch, lines 417-456; DARRAY_FILL switch, lines 489-563)
- `src/clib/core/iolib/hdf5/spio_async_hdf5_utils.cpp` (`Hdf5_wcache` struct lines 79-96; `pio_iosys_async_hdf5_write_op_add` lines 848-925; `pio_iosys_async_op_hdf5_write` lines 607-831)

**Steps:**
- [ ] In `Hdf5_wcache` (lines 79-96), add a new field:
  ```cpp
  io_desc_info_t *io_desc_info;
  ```
  populated once at construction in `pio_iosys_async_hdf5_write_op_add`.
- [ ] In `pio_darray.cpp`'s DARRAY_DATA switch (lines 417-456), replace the `#if PIO_USE_ASYNC_WR_THREAD` / `#else` HDF5/HDF5C case with:
  ```cpp
  case PIO_IOTYPE_HDF5:
  case PIO_IOTYPE_HDF5C:
    {
      io_desc_info_t &wdesc = iodesc->select_io_desc_info(file->iotype);
      bool is_async_write = (&wdesc == iodesc->async_io_desc_info);
      if(is_async_write){
        if((ierr = pio_iosys_async_hdf5_write_op_add(file, nvars, fndims, varids,
                                                       sp_iodesc, DARRAY_DATA, frame))){
          /* existing error handling */
        }
      }
      else{
        if((ierr = write_darray_multi_par(file, nvars, fndims, varids, iodesc,
                                           DARRAY_DATA, frame))){
          /* existing error handling */
        }
      }
    }
    break;
  ```
  (Re-read the exact existing error-handling statements inside each branch during implementation and preserve them verbatim — only the dispatch condition changes, from `#if`/`#else` to `if(is_async_write)`/`else`.) Confirm `sp_iodesc` (the `std::shared_ptr<io_desc_t>`) is in scope at this call site — the async enqueue function takes the shared_ptr, not the raw `iodesc` used elsewhere in the switch; if only the raw pointer is in scope, use whatever shared_ptr accessor the surrounding function already uses to obtain it for other iotypes' async paths, if any, or confirm how `iodesc`/`sp_iodesc` relate in this function's existing signature.
- [ ] Apply the identical transformation to the DARRAY_FILL switch (lines 489-563). This path is reached only when `iodesc->sync_io_desc_info.rearranger == PIO_REARR_SUBSET && iodesc->sync_io_desc_info.needsfill` (checked by the caller at line 483), so in practice `select_io_desc_info` will always return `sync_io_desc_info` here today (it only returns async for CONTIG) and `is_async_write` will always be `false`. This is a mechanical consistency rename with no functional change — call it out as such in the PR description so reviewers don't look for behavior change here.
- [ ] In `pio_iosys_async_hdf5_write_op_add` (lines 848-925), when constructing the `Hdf5_wcache`, set:
  ```cpp
  wcache->io_desc_info = &(iodesc->select_io_desc_info(file->iotype));
  ```
  using whatever form of `iodesc` (shared_ptr or raw) is already available in this function's parameter list.
- [ ] In `pio_iosys_async_op_hdf5_write` (lines 607-831), replace the current unconditional `assert(ios->io_comm_info->is_sync_io_proc())` (line 627) with a role-correct check based on whether this op is operating on the async `io_desc_info_t`:
  ```cpp
  bool is_async = (wcache->io_desc_info == iodesc->async_io_desc_info);
  assert(is_async ? ios->io_comm_info->is_async_io_proc() : ios->io_comm_info->is_sync_io_proc());
  ```
  (Confirm exact variable names for `iodesc`/`wcache` in this function's local scope during implementation — it receives `Hdf5_wcache *wcache` from `void *pdata` per prior research; `iodesc` must be reachable from `wcache` or from another field already present in the struct, e.g. via `file`/`varids`/an existing iodesc lookup. If `iodesc` is not already reachable from this function, that is a materially different situation from what was assumed during planning and should be flagged to the user before proceeding, rather than guessed.) Everywhere else in this function that currently reads `iodesc->sync_io_desc_info` directly (per prior research: building per-variable hyperslabs from `vreg_infos`, type conversion sizing, etc.) must be updated to read `*(wcache->io_desc_info)` instead, so an async-dispatched write actually uses the async rearranger's region/communicator info rather than the sync one.
- [ ] Update any other direct `iodesc->sync_io_desc_info` references inside the async write/enqueue functions that logically should follow the selected `io_desc_info_t` rather than being hardcoded to sync (re-identify during implementation by grepping `sync_io_desc_info` within `spio_async_hdf5_utils.cpp` after the above changes land, to catch anything missed).

**Verify:** Build with `PIO_USE_ASYNC_WR_THREAD` on and off. With it on, a CONTIG+HDF5 write test should exercise `is_async_write == true` and complete without the `is_sync_io_proc()` assert firing incorrectly. With it off, behavior must be identical to pre-change (sync path only, `write_darray_multi_par` always taken). A SUBSET+HDF5 write test (DARRAY_FILL path, or any non-CONTIG HDF5 write) should always take `is_async_write == false` regardless of the flag.

---

### Task 5: HDF5 darray read path (PIO_IOTYPE_HDF5/HDF5C)

**Context:** HDF5/HDF5C reads are not just "kept synchronous" — they currently don't exist at all. `PIOc_read_darray_impl`'s switch has no `PIO_IOTYPE_HDF5`/`HDF5C` case and falls to `default: → PIO_EBADIOTYPE`. This task adds real, always-synchronous HDF5 darray read support by extending the existing shared `pio_read_darray_nc` function (already used by `PIO_IOTYPE_NETCDF4P`/`_NCZARR`/`PIO_IOTYPE_PNETCDF`), mirroring `write_darray_multi_par`'s existing HDF5 write branch in reverse. No new function and no new file are created.

**Files:**
- `src/clib/core/pio_darray_int.cpp` (`pio_read_darray_nc`, lines 1175-1408; its internal switch at lines 1286-1384)
- `src/clib/core/pio_darray.cpp` (`PIOc_read_darray_impl`'s outer switch, lines 3618-3651)

**Steps:**
- [ ] In `pio_darray_int.cpp`, inside `pio_read_darray_nc`'s existing per-region loop and switch (lines 1286-1384), add a new case alongside the existing `#ifdef _NETCDF4`/`#ifdef _PNETCDF` branches:
  ```cpp
  #ifdef _HDF5
    case PIO_IOTYPE_HDF5:
    case PIO_IOTYPE_HDF5C:
      {
        /* Accumulate this region into startlist/countlist, reusing the
         * function's existing tmp_bufsize/startlist/countlist/rrlen locals */
        tmp_bufsize = 1;
        for(int i = 0; i < ndims; i++){ tmp_bufsize *= count[i]; }
        if(tmp_bufsize > 0){
          startlist[rrlen] = (PIO_Offset *) bget(ndims * sizeof(PIO_Offset));
          countlist[rrlen] = (PIO_Offset *) bget(ndims * sizeof(PIO_Offset));
          memcpy(startlist[rrlen], start, ndims * sizeof(PIO_Offset));
          memcpy(countlist[rrlen], count, ndims * sizeof(PIO_Offset));
          dsize_all += tmp_bufsize;
          rrlen++;
        }

        if(regioncnt == iodesc->sync_io_desc_info.maxregions - 1){
          hid_t file_space_id = H5Dget_space(file->hdf5_vars[vid].hdf5_dataset_id);
          hid_t mem_space_id;
          for(int i = 0; i < rrlen; i++){
            H5Sselect_hyperslab(file_space_id, (i == 0) ? H5S_SELECT_SET : H5S_SELECT_OR,
                                 (hsize_t *) startlist[i], NULL, (hsize_t *) countlist[i], NULL);
          }
          if(dsize_all > 0){
            mem_space_id = H5Screate_simple(1, &dsize_all, NULL);
          }
          else{
            H5Sselect_none(file_space_id);
            mem_space_id = H5Screate(H5S_NULL);
          }

          hid_t mem_type_id = spio_nc_type_to_hdf5_type(iodesc->sync_io_desc_info.piotype);
          hid_t file_var_type_id = H5Dget_type(file->hdf5_vars[vid].hdf5_dataset_id);
          hid_t file_var_ntype_id = H5Tget_native_type(file_var_type_id, H5T_DIR_DEFAULT);

          bool dt_conv_reqd = (dsize_all > 0) && !H5Tequal(mem_type_id, file_var_ntype_id);
          void *read_buf = iobuf;
          void *native_buf = NULL;
          if(dt_conv_reqd){
            native_buf = bget(dsize_all * DTConverter::size_of(spio_hdf5_type_to_pio_type(file_var_ntype_id)));
            read_buf = native_buf;
          }

          H5Dread(file->hdf5_vars[vid].hdf5_dataset_id, file_var_ntype_id,
                  mem_space_id, file_space_id, file->dxplid_coll, read_buf);

          if(dt_conv_reqd){
            void *converted_buf = NULL;
            DTConverter::convert(native_buf, iodesc->sync_io_desc_info.mpitype_size * dsize_all,
                                  spio_hdf5_type_to_pio_type(file_var_ntype_id),
                                  iodesc->sync_io_desc_info.piotype, &converted_buf);
            memcpy(iobuf, converted_buf, iodesc->sync_io_desc_info.mpitype_size * dsize_all);
            brel(converted_buf);
            brel(native_buf);
          }

          H5Sclose(mem_space_id);
          H5Sclose(file_space_id);
          for(int i = 0; i < rrlen; i++){
            brel(startlist[i]);
            brel(countlist[i]);
          }
        }
      }
      break;
  #endif /* _HDF5 */
  ```
  This is a design sketch to transcribe and adapt to the function's *actual* local variable names/types (`tmp_bufsize`, `rrlen`, `startlist`/`countlist` array element types, `vid`/`varid` naming, `ndims`/`start`/`count` exact identifiers) exactly as they appear in the real function body at implementation time — re-read lines 1175-1408 immediately before editing to match real names precisely, since this plan's sketch uses working names based on prior research rather than guaranteed-exact identifiers. In particular, confirm the exact signature of `DTConverter::convert`'s caller-owned overload (whether it returns the buffer or takes an out-parameter) and adjust the call accordingly.
  Add a new local `hsize_t dsize_all = 0;` near the top of `pio_read_darray_nc` (reset appropriately if the region loop can run more than once per call — confirm against the function's actual loop structure).
- [ ] Add `#include "spio_hdf5_utils.hpp"` and `#include "spio_dt_converter.hpp"` to `pio_darray_int.cpp` if not already present (needed for `spio_nc_type_to_hdf5_type`, `spio_hdf5_type_to_pio_type`, `DTConverter`) — check existing includes first since `write_darray_multi_par` in the same file already uses these symbols for the write path, so they are likely already included.
- [ ] In `pio_darray.cpp`'s `PIOc_read_darray_impl` outer switch (lines 3618-3651), add, immediately after the existing `PIO_IOTYPE_NETCDF4P`/`PIO_IOTYPE_PNETCDF`-handling case and before any `#ifdef _ADIOS2` block:
  ```cpp
  #ifdef _HDF5
    case PIO_IOTYPE_HDF5:
    case PIO_IOTYPE_HDF5C:
  #endif
  ```
  added to the same case label group that already calls `pio_read_darray_nc(...)` — i.e. HDF5/HDF5C fall through to the exact same function call already used for NETCDF4P/NCZARR/PNETCDF. No new function, no new declaration (`pio_read_darray_nc` is already declared in `pio_internal.h:334`), no new file.

**Verify:** A round-trip test (write via CONTIG+HDF5 — exercising either the sync or async write path from Task 4 — then read back via `PIOc_read_darray` on `PIO_IOTYPE_HDF5`) must reproduce the original data exactly, including at least one case where `dt_conv_reqd` is true (mem type != file native type) to exercise the `DTConverter` path. See Task 6 for the integration test that performs this.

---

### Task 6: Test infrastructure and CMakeLists wiring

**Context:** New unit tests (Tasks 1 and 2) and a new HDF5 round-trip integration test (Task 5) need to be registered so `make` builds them and `ctest` runs them, following this repo's existing precedents exactly.

**Files:**
- `tests/cunit/CMakeLists.txt`
- `examples/c/CMakeLists.txt`
- New: an HDF5 CONTIG async round-trip test under `examples/c/` (working name `test_hdf5_async_contig.c`, modeled on the existing `examples/c/test_hdf5.c`)

**Steps:**
- [ ] In `tests/cunit/CMakeLists.txt`, near the existing `PIO_USE_ASYNC_WR_THREAD`-gated block (lines 129-134, the `test_mtq`/`test_mtq_signal`/`test_mtq_pool` precedent), add:
  ```cmake
  if(PIO_USE_ASYNC_WR_THREAD)
    add_spio_executable (test_spio_select_io_desc_info TRUE "" test_spio_select_io_desc_info.cpp OBJ_LIBS test_common_obj)
    add_spio_executable (test_spio_rearr_contig_async TRUE "" test_spio_rearr_contig_async.cpp OBJ_LIBS test_common_obj)
    add_dependencies(tests test_spio_select_io_desc_info test_spio_rearr_contig_async)
  endif()
  ```
  (Match the exact `add_spio_executable`/`add_dependencies` argument style used by the neighboring `test_mtq` block — re-read lines 129-134 during implementation to copy the precedent precisely, including whether `TRUE ""` or different flags are used for similarly-shaped tests, e.g. `test_spio_rearr_contig` at line 115 for a non-flag-gated precedent.)
- [ ] In the ctest-registration section (lines 160-164 precedent for `test_mtq`), add matching `add_test(NAME test_spio_select_io_desc_info COMMAND test_spio_select_io_desc_info)` style entries, and for `test_spio_rearr_contig_async` — since it needs multiple MPI ranks to exercise distinct sync/async comm sizes meaningfully — use the `add_mpi_test`/`foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})` pattern from lines 204-209 (the `test_spio_rearr_contig${nproc}` precedent) instead of a plain `add_test`, still inside the `if(PIO_USE_ASYNC_WR_THREAD)` guard.
- [ ] Create `examples/c/test_hdf5_async_contig.c`, based on the structure of the existing `examples/c/test_hdf5.c`: create a `PIO_REARR_CONTIG` decomposition, write a darray variable to an HDF5 file, close/reopen (or keep open, matching whatever `test_hdf5.c` already does for its round trip), read the darray back via `PIOc_read_darray`, and compare against the original data buffer element-by-element, failing with a clear message on mismatch. Include at least one variable whose PIO type forces `dt_conv_reqd == true` on the target platform's native HDF5 type mapping, if `test_hdf5.c` already has a precedent for this; otherwise add a second variable/type specifically to exercise the conversion path.
- [ ] In `examples/c/CMakeLists.txt`, mirror the existing `WITH_HDF5`-gated `test_hdf5` precedent (lines 83-85, 112-114) but gate on both flags:
  ```cmake
  if (WITH_HDF5 AND PIO_USE_ASYNC_WR_THREAD)
    add_spio_executable(test_hdf5_async_contig TRUE "" test_hdf5_async_contig.c)
  endif ()
  ```
  and, in the test-registration section:
  ```cmake
  if (WITH_HDF5 AND PIO_USE_ASYNC_WR_THREAD)
    add_mpi_test(test_hdf5_async_contig EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_hdf5_async_contig NUMPROCS 4 TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endif ()
  ```

**Verify:** Full reconfigure with `-DPIO_USE_ASYNC_WR_THREAD=ON -DWITH_HDF5=ON` (actual CMake option names for these may differ — confirm exact option names used by this build during implementation, since `PIO_USE_ASYNC_WR_THREAD` is a `pio_config.h.in`-substituted macro and may be driven by a differently-named CMake cache variable such as `USE_ASYNC_WR_THREAD`). `make && ctest` should build and pass all new tests, plus all pre-existing tests unmodified. A build with the flag(s) off should skip the new tests entirely and show no change in pass/fail counts versus `master`.

- [ ] **Investigate `tests/general` HDF5 handling before closing this task.** `tests/general/util/pio_tutil.F90` already enumerates `PIO_IOTYPE_HDF5`/`HDF5C` as iotypes for the generic decomposition tests (`pio_decomp_tests*.F90`, `pio_sync_tests.F90`, etc.) whenever built `_HDF5`, and those generic tests already perform HDF5 writes today. Because `PIOc_read_darray_impl` currently returns `PIO_EBADIOTYPE` for HDF5/HDF5C, determine exactly how these pre-existing Fortran tests currently handle (or avoid) the HDF5 read-back/verification step — e.g. whether the read call is skipped for HDF5 today, its error is tolerated/ignored, or the read assertion is simply dormant. Task 5 makes that read call succeed for the first time, which can turn a previously-dormant assertion into an active (and possibly failing, if test data/expectations were never exercised against it) one. Any `tests/general` test found to depend on the old failing/skipped behavior must be updated alongside Task 5, as a follow-up to this task.

---

## Deviation from Spec: Testing Approach

The design spec's "Testing" section proposed extending the existing `test_darray_async`, `test_darray_async_simple`, and `test_darray_async_many` C tests in place. This plan deliberately deviates from that: Tasks 2, 5, and 6 above add **new** test files (`test_spio_select_io_desc_info.cpp`, `test_spio_rearr_contig_async.cpp`, `test_hdf5_async_contig.c`) instead of modifying `test_darray_async*`. Rationale: the `test_darray_async*` family exercises the pre-existing async write-op infrastructure for other iotypes/rearrangers, and interleaving new CONTIG/HDF5-specific assertions into those shared tests risks coupling unrelated test scenarios and making failures harder to attribute. New, narrowly-scoped test files keep this feature's coverage isolated and easy to run/bisect independently, at the cost of not reusing the existing tests' setup boilerplate.

## Notable Open Risks (for reviewer attention)

1. **`create_agg_comm()` is not a mechanical substitution.** Its color/key derivation depends on `ios_->num_uniontasks` divided by an IO comm size; the async-role variant must be verified to compute a dimensionally correct split against the async IO comm, not just textually swapped.
2. **HDF5 reads were previously unsupported, not merely "forced synchronous."** `PIOc_read_darray_impl` has no HDF5/HDF5C case today (falls to `PIO_EBADIOTYPE`). Task 5 adds genuinely new functionality (reusing the existing shared `pio_read_darray_nc`), not a no-op passthrough — this should be scoped and reviewed as a distinct, real feature addition, with its own round-trip test coverage, not waved through as "no behavior change."
3. **Enabling HDF5 reads may activate dormant assertions in `tests/general`'s existing Fortran decomp tests**, which already perform HDF5 writes today but have never been able to verify them via read-back. See Task 6's investigation step.
