# Fortran -> C++ test port roadmap

Tracks the ongoing effort to port all Fortran tests in `tests/general` to
C++ functional tests in `tests/functional`, one file (or small bundle of
tiny files) at a time, each via `superpowers:writing-plans` +
`superpowers:subagent-driven-development`, following the pattern
established by `2026-10-30-port-pio-decomp-tests-to-cxx.md`.

**Non-negotiable constraints (apply to every file below, carried from the
first plan):**
- Fortran tests in `tests/general` are NEVER removed or modified, only read.
- 1:1 file naming: `tests/general/X.F90.in` -> `tests/functional/X.cpp`
  (for bundles, name the bundle's `.cpp` after the cluster, see below).
- Reuse `tests/util/spio_test_framework.hpp`, `spio_test_common.hpp`
  (`get_iotypes`/`get_iotype_name`), `spio_test_check.hpp` (`check_val`,
  `Pio_type_map`) — extend them only when a new file needs something they
  don't yet provide, never duplicate.
- C++ style pinned to `tests/cunit/test_spio_rearr_contig.cpp`'s
  conventions (2-space indent, no space before `(` after keywords,
  same-line opening braces, `std::size_t` for vector-index loops,
  `PIO_NOERR`/`PIO_EINTERNAL` + `logger.log(...)`, no `std::cerr`/exceptions,
  `const int` not `#define`), as established and reviewed in the first plan.
- No external `.in`-to-`.cpp` generation scripts.
- Each file's CMake target name gets a `_cxx` suffix if it would otherwise
  collide with the existing Fortran target of the same name (per the
  Task-1 ruling in the first plan) — check for collisions before naming.

**Ordering:** smallest-first by line count. **Batching:** files under
~100 lines are bundled into one plan/SDD run each; everything else gets
its own plan. **Check-ins:** a brief status message after each file/bundle's
SDD run (including final review + any fix round) completes and the branch
is green — the user asked for this (not continuous silent execution).

## Status legend
`[ ]` pending · `[~]` in progress · `[x]` done

## Roadmap (smallest-first, as inventoried 2026-10-30)

### Bundle 1: tiny files (~100 lines or less), one shared plan
- [x] `pio_decomp_large_tests.F90.in` (23 lines)
- [x] `pio_fail.F90.in` (25 lines)
- [x] `pio_init_finalize.F90.in` (40 lines)
- [x] `pio_file_fail.F90.in` (51 lines)
- [x] `pio_file_simple_tests.F90.in` (68 lines)
- [x] `ncdf_eh_one_test.F90.in` (100 lines)

### Individual files, smallest to largest
- [x] `pio_set_hint.F90.in` (117 lines)
- [x] `pio_copy.F90.in` (146 lines)
- [x] `pio_async_decomp_tests.F90.in` (155 lines)
- [x] `ncdf_inq.F90.in` (191 lines)
- [x] `pio_async_file_tests.F90.in` (193 lines)
- [x] `ncdf_fail.F90.in` (221 lines)
- [x] `pio_decomp_tests_3d.F90.in` (221 lines)
- [x] `pio_iosystem_tests2.F90.in` (246 lines)
- [x] `pio_rearr.F90.in` (260 lines)
- [x] `pio_iosystem_tests3.F90.in` (292 lines)
- [x] `ncdf_simple_tests.F90.in` (310 lines)
- [x] `pio_async_init_finalize.F90.in` (334 lines)
- [x] `pio_decomp_tests2_1d.F90.in` (354 lines)
- [x] `pio_async_copy_tests.F90.in` (357 lines)
- [x] `pio_iosystem_tests.F90.in` (372 lines)
- [x] `pio_decomp_tests_2d.F90.in` (391 lines)
- [x] `pio_async_decomp_tests_1d.F90.in` (420 lines)
- [x] `pio_rearr_opts2.F90.in` (424 lines)
- [x] `pio_large_file_tests.F90.in` (433 lines)
- [x] `pio_rearr_opts.F90.in` (487 lines)
- [x] `pio_buf_lim_tests.F90.in` (506 lines)
- [x] `pio_decomp_fillval.F90.in` (540 lines)
- [x] `pio_iodesc_tests.F90.in` (634 lines)
- [x] `pio_decomp_fillval2.F90.in` (645 lines)
- [x] `pio_decomp_frame_tests.F90.in` (661 lines)
- [ ] `pio_decomp_tests_1d.F90.in` (727 lines)
- [ ] `pio_sync_tests.F90.in` (802 lines)
- [ ] `pio_decomp_extra_dims.F90.in` (896 lines)
- [ ] `ncdf_get_put.F90.in` (3283 lines)

## Already done (before this roadmap)
- [x] `pio_decomp_tests.F90.in` (539 lines) -> `tests/functional/pio_decomp_tests.cpp`
  — see `2026-10-30-port-pio-decomp-tests-to-cxx.md`.

## Per-file log

(Append one entry per completed file/bundle: plan file path, commit
range, any ruling made, final review verdict.)

### Bundle 1 — `pio_decomp_large_tests`, `pio_fail`, `pio_init_finalize`, `pio_file_fail`, `pio_file_simple_tests`, `ncdf_eh_one_test`
- Plan: `docs/superpowers/plans/2026-10-30-port-fortran-bundle1-to-cxx.md`
- Commit range: `c3c0e0ba0e..f71b3e3eff` on `jayeshkrishna/port_fortran_tests_to_cxx`
- Task 2 (`pio_fail.cpp`) ruling: plan text itself had a bug (`tf.run();`
  instead of `nerrs += tf.run();`), causing all 9 WILLFAIL ctest entries
  to pass vacuously (exit 0 always). Fixed at the plan-text source and
  one fix round on the same implementer; re-review confirmed 9/9 correct
  (binary now exits nonzero as intended).
- Task 5 (`pio_file_fail.cpp`) ruling: implementer added
  `PIOc_set_iosystem_error_handling(..., PIO_BCAST_ERROR, &old_handler)`
  in `main()`, not in the brief's literal snippet but verified necessary
  — `Test_framework::init_iosystem()` doesn't default to `PIO_BCAST_ERROR`
  the way the Fortran framework does, so the C library's real default
  (`PIO_INTERNAL_ERROR`) would abort on the first undefined-iotype call.
  Judged correct and file-local; not pushed into shared `tests/util`.
- Task 6 (`ncdf_eh_one_test.cpp`) scope ruling: ported only the 3
  non-aborting error handlers (`PIO_BCAST_ERROR`, `PIO_REDUCE_ERROR`,
  `PIO_RETURN_ERROR`), looped internally across all iotypes in one
  binary. The 5 Fortran `WILLFAIL`/`PIO_INTERNAL_ERROR`-per-iotype ctest
  variants are **not ported** — would need generic `--pio-tf-targ-*` CLI
  passthrough added to `Test_framework` (doesn't exist today) plus a way
  to mark one iotype's run as an expected-abort inside a multi-iotype
  loop. Recorded here as an explicit, known gap for a future bundle, not
  silently dropped.
- Final whole-branch review found 2 real pre-existing defects (both
  invisible in this PnetCDF-only dev build, so missed by every per-task
  review): (1) Critical — `get_undef_iotypes()` classified
  ADIOS/ADIOSC/HDF5/HDF5C via `PIOc_iotype_available()`, which has no
  case for any of the four and always reports them unavailable; the real
  gate `PIOc_createfile` uses is `iotype_is_valid()`
  (`src/clib/core/pioc_support.cpp`), gating ADIOS/ADIOSC under
  `_ADIOS2` and HDF5/HDF5C under `_HDF5` (HDF5C additionally needs
  `_SPIO_HDF5_USE_COMPRESSION`); (2) Important — `pio_file_simple_tests`'s
  netcdf-family `enddef` guard was inverted relative to the Fortran
  `PIO_TF_Is_netcdf` semantics. Plus 2 minor hardening items (file handle
  leak, uninitialized buffer). Fixed in one round (commit `0c9e5a7fa9`).
  A scoped re-review of that fix caught a second-order defect
  self-introduced in the fix brief (ADIOSC given an incorrect compression
  sub-guard that doesn't match `iotype_is_valid()`'s real logic, and
  which also mirrors a latent quirk in the Fortran reference itself,
  not something to blindly copy) — corrected in a second fix round
  (commit `f71b3e3eff`), independently re-verified against
  `iotype_is_valid()` directly rather than trusting either prior report.
- Final review verdict: **Approved**. `tests/general` untouched
  throughout (zero diff lines in that directory across the whole bundle).

### `pio_set_hint`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-set-hint-to-cxx.md`
- Commit: `30d30b6149` on `jayeshkrishna/port_fortran_tests_to_cxx`
  (base `f71b3e3eff`).
- Scope ruling: the Fortran reference's `PIO_TF_Get_nc_iotypes` is, despite
  its name, not limited to the netcdf family (includes adios/adiosc/hdf5/
  hdf5c too). This port uses the existing `get_iotypes()` 5-flavor helper
  instead, consistent with the rest of `tests/functional`, rather than add
  a new helper replicating that broader scope. Documented as low-cost if
  wrong — a future file can add a dedicated helper if it genuinely needs
  the wider set.
- One deviation reported and verified: implementer added the standard GPTL
  header-guard block (missing from the plan's code block, present in every
  other ported file) — required for the build; task reviewer confirmed it
  byte-for-byte matches `pio_file_fail.cpp`/`pio_file_simple_tests.cpp`/
  `ncdf_eh_one_test.cpp`.
- Single-task plan: the task review doubled as the whole-branch final
  review (ruling recorded in this plan's now-deleted SDD ledger).
- Final review verdict: **Approved**. `tests/general` untouched.

### `pio_copy`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-copy-to-cxx.md`
- Commit: `aa7f1ca6b8` on `jayeshkrishna/port_fortran_tests_to_cxx`
  (base `30d30b6149`).
- Templated-type pattern: `template<typename T> test_copy_att<T>`, explicitly
  instantiated for int/float/double and registered as 3 named `rfuncs`
  entries, mirroring `pio_decomp_tests.cpp`'s precedent for the Fortran
  `PIO_TF_TEMPLATE` 3-way expansion. Uses generic (non-type-suffixed)
  `PIOc_put_att`/`PIOc_get_att` to keep the template uniform over `T`.
- One deviation reported and verified: the plan's own draft had the GPTL
  guard block placed after the `spio_test_framework.hpp` include (a plan
  defect, not an implementer error); implementer reordered it to match
  `pio_set_hint.cpp`'s real, working order.
- Task reviewer independently verified every Review Focus item (per-type
  numeric round-trip, independent char-attribute round-trip, all 4
  global+variable `PIOc_copy_att` calls, complete error-path ncid cleanup
  across 22 branches, consistent iotype for input/output). Two Minor,
  non-blocking notes parked (missing explicit `<algorithm>` include for
  `std::fill`; reused test filenames across iotype/type combos, safe due
  to `PIO_CLOBBER`, matching sibling-file convention).
- Single-task plan: the task review doubled as the whole-branch final
  review.
- Final review verdict: **Approved**. `tests/general` untouched.

### `pio_async_decomp_tests`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-async-decomp-tests-to-cxx.md`
- Commit: `a990c5000f` on `jayeshkrishna/port_fortran_tests_to_cxx`
  (base `aa7f1ca6b8`).
- First C++ port of SCORPIO's asynchronous (multi-component intercomm) I/O
  mode — architecturally different from every prior ported file. Confirmed
  via reading `PIOc_init_intercomm_impl()`
  (`src/clib/core/pioc.cpp:2620-3171`) that it's collective over the full
  `peer_comm` for every component index regardless of local ownership, and
  that `iosysidps[i]` is populated with a valid id for every index on
  every process — so every compute rank correctly finalizes all
  `NUM_COMPONENTS` iosysids, not just its own, matching the Fortran
  reference.
- Architectural ruling: `Test_framework::init_iosystem()`/`get_iosysid()`/
  `finalize_iosystem()` only support the single synchronous iosystem case
  and have no intercomm support, so this file bypasses them entirely —
  `main()` uses only `tf.init()`/`tf.run()`/`tf.finalize()`, and the
  registered test function manages its own comm-split + per-component
  `PIOc_init_intercomm`/`PIOc_finalize` lifecycle directly. This pattern
  is expected to be reused by the other `pio_async_*` files later in the
  roadmap.
- Critical safety ruling: `check_ret()`/`passert()`/`check_val()`
  (`tests/util/spio_test_check.hpp`) do an `MPI_Allreduce` over the full
  world comm, but once ranks diverge into I/O vs. compute role, the I/O
  rank is blocked inside its own `PIOc_init_intercomm()` call until every
  compute rank finalizes — so any of those helpers reaching the diverged
  region would deadlock. The port uses purely local `nerrs`/
  `tf.get_logger().log()` (confirmed non-collective) in that region
  instead, relying on `Test_framework::run()`'s own post-return
  `MPI_Reduce` for the final tally.
- Task reviewer independently re-verified the no-deadlock property from
  source (not from the implementer's report), re-ran ctest (2/2 pass) plus
  direct `mpirun` at np=1..6 (all exit 0, no hang), and confirmed
  `tests/general` untouched branch-wide, `_cxx` target naming, both
  rearrangers, and all 3 type instantiations.
- 4 Minor findings parked, none blocking: unreachable compute-rank error
  logs (only rank 0, always the I/O rank, emits logger output — a
  `Test_framework`-level limitation, out of this task's scope);
  unchecked `MPI_Comm_split`/`size`/`rank`/`free` return codes (small
  fidelity gap vs. the Fortran, which checks `MPI_Comm_free`'s `ierr`);
  latent OOB `comp_comms[-2]` if `split_world_ncomms()`'s early-return
  path were ever reachable (unreachable today since `MIN_NUM_PROCS ==
  NCOMMS`); theoretical hang if `PIOc_init_intercomm()` failed on a
  compute rank only (same shape as the Fortran reference, which checks
  even less). All judged low-cost-if-wrong, pre-existing-shape, not
  regressions introduced by this port.
- Single-task plan: the task review doubled as the whole-branch final
  review.
- Final review verdict: **Approved**. `tests/general` untouched.

### `pio_async_file_tests`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-async-file-tests-to-cxx.md`
- Commit: `dd39ac9f64` on `jayeshkrishna/port_fortran_tests_to_cxx`
  (base `7234868e80`).
- Reuses the async-I/O architecture established in
  `pio_async_decomp_tests.cpp` (2 compute components + 1 I/O comm,
  `PIOc_init_intercomm`/`PIOc_finalize`, `init_iosystem()`/
  `finalize_iosystem()` bypassed), but goes further: the Fortran reference
  actually performs real file I/O (metadata only, no data payload) inside
  the diverged compute-only region and checks return codes there via the
  3-argument `PIO_TF_CHECK_ERR(ret, new_comm, msg)` form — confirmed via
  `tests/general/util/pio_tf_f90gen.pl:415-427` to be a genuine
  `MPI_Allreduce`-style reduction scoped to the passed-in comm (not a
  no-op, not the full test comm). The port reproduces this faithfully with
  a new local helper, `check_ret_over_comm()`, doing a real
  `MPI_Allreduce(MPI_LAND)` over the compute component's own `new_comm`
  (which structurally excludes the blocked I/O rank, so no deadlock risk)
  — a stronger, more faithful check than the predecessor file's
  local-only accumulation, since here the Fortran itself performs a real
  collective check, not a vacuous/absent one.
- Faithfulness ruling: the Fortran's `PIO_TF_ASSERT` (used for the
  var name/type/ndims inquiry checks) was confirmed via
  `tests/general/util/pio_tf_f90gen.pl:404-413` to perform **no**
  cross-rank communication at all (a pure local `IF (.NOT. (cond)) THEN
  ... RETURN`) — so the port's corresponding checks are local-only
  (`nerrs++`), not routed through any collective. The Fortran's literal,
  apparently copy-pasted file names (`pio_async_decomp_tests_file_comp1
  /2.nc`, reused from the sibling test rather than this file's own name)
  are preserved verbatim rather than "fixed." No data write/read
  (`PIOc_put_var`/`PIOc_get_var`) appears anywhere, matching the Fortran's
  metadata-only reference.
- Task reviewer independently re-verified every Global Constraint and
  Review Focus item against the diff (not the implementer's report):
  confirmed byte-identical transcription against the brief, re-ran a
  forced clean rebuild (0 warnings) and `ctest` itself (2/2 pass,
  covering all 3 type instantiations x 2 rearrangers), and additionally
  verified (beyond what the implementer's own report established) that
  neither `MPI_logger::log()` nor `get_iotypes()` make any blocking MPI
  calls that could interact with the diverged I/O rank.
- 1 Important finding, non-blocking: all compute-side diagnostic log
  lines are silently swallowed, since `MPI_logger` only emits on world
  rank 0, which is always the I/O rank in this test and never calls the
  logging helper — pass/fail correctness is unaffected (`nerrs` still
  propagates to a framework-level FAILED), and this is the identical
  pre-existing pattern already accepted in the sibling
  `pio_async_decomp_tests.cpp`, not a regression introduced by this port.
  Logged as a known limitation of the whole `pio_async_*` family; a
  proper fix (enabling logging on a compute-component root, or falling
  back to `std::cerr` on the failure path) is left as a future
  whole-family follow-up, out of scope for a single-file port.
- 6 Minor findings parked, none blocking: (M1) a latent
  `gcomm_idx == 0` out-of-bounds risk in `comp_idx = gcomm_idx - 2`,
  identical to a latent bug already present in the Fortran, currently
  unreachable given the `MIN_NUM_PROCS`/`NCOMMS` coupling; (M2) the
  Fortran's trailing post-`MPI_Comm_free` `PIO_TF_CHECK_ERR` not
  transcribed, confirmed harmless; (M3) a deliberate, reviewer-endorsed
  control-flow deviation (`continue`/`nerrs++` instead of the Fortran's
  `RETURN`), since copying the Fortran's `RETURN` here would actually
  deadlock this async test (an early-returning rank would skip
  `PIOc_finalize()` while the I/O rank waits forever); (M4) the port
  correctly declines to copy a Fortran logging-only indexing bug
  (`iotype_descs(i)` instead of `(j)`), log-text only; (M5) an unused
  `new_rank` variable, mirroring the Fortran exactly; (M6)
  `split_world_ncomms()` duplicated verbatim from
  `pio_async_decomp_tests.cpp` rather than shared, mirroring the
  Fortran's own precedent of duplicating this subroutine across 5
  `.F90.in` files — hoisting to `tests/util` is a reasonable future
  whole-family follow-up, not required now.
- Single-task plan: the task review doubled as the whole-branch final
  review.
- Final review verdict: **Approved**. `tests/general` untouched.

### `ncdf_inq`
- Plan: `docs/superpowers/plans/2026-10-30-port-ncdf-inq-to-cxx.md`
- Commit: `7234868e80` on `jayeshkrishna/port_fortran_tests_to_cxx`
  (base `a990c5000f`).
- Not templated over a data type (the Fortran reference is a plain
  `PIO_TF_AUTO_TEST_SUB_BEGIN` block, not `PIO_TF_TEMPLATE`), so the port
  registers exactly one driver function, unlike `pio_copy.cpp`/
  `pio_decomp_tests.cpp`'s 3-way int/float/double pattern. Confirmed via
  reading `pio_api.h:140,149` — `PIOc_inq_dimname` takes no `namelen` arg
  while `PIOc_inq_varname` does.
- Faithfulness ruling: the character attribute's length is asserted using
  the trimmed string length (9, i.e. `strlen("DUMMY_STR")`) rather than
  the Fortran's 100-byte fixed-length buffer, since the Fortran's own
  `PIO_put_att` wrapper trims trailing blanks before the underlying write
  — confirmed by the Fortran reference's own assertion,
  `att_len == len(trim(tgv_catt_val))`. The integer attribute's length is
  inquired but deliberately never asserted (only `check_ret` on the
  return code), matching the Fortran reference's own dead, commented-out
  `PIO_TF_PASSERT` for that value (which referenced an undeclared
  variable) — the port does not "fix" this by adding an assertion the
  Fortran never enabled.
- Task reviewer independently re-verified every Global Constraint and
  Review Focus item against the diff (not the implementer's report),
  re-ran the build and `ctest` itself (3/3 pass), and confirmed
  `tests/general` untouched.
- 3 Minor findings parked, none blocking: (1) the port reuses `DIM_LEN`
  (100) as the int-attribute's element count where the Fortran has a
  separate (coincidentally also 100-valued) constant — originates in the
  plan's own mandated code, not an implementer deviation; ruled
  acceptable since the values are numerically identical in the spec, pure
  naming/coupling nit, 2-line fix if it ever needs to diverge; (2) and (3)
  were explicitly "no change recommended" observations (file-scope
  constants; leaving a test file on disk on an open failure, mirroring
  the Fortran exactly).
- Single-task plan: the task review doubled as the whole-branch final
  review.
- Final review verdict: **Approved**. `tests/general` untouched.

### `ncdf_fail`
- Plan: `docs/superpowers/plans/2026-10-30-port-ncdf-fail-to-cxx.md`
- Commit: `3575d5612a` on `jayeshkrishna/port_fortran_tests_to_cxx`
  (base `dd39ac9f64`).
- Not templated over a data type; registers 5 non-templated rfuncs, one
  per `PIO_TF_AUTO_TEST_SUB_BEGIN` block in the Fortran reference
  (`test_clob_then_no_clob`, `test_redef_with_no_write`,
  `test_redef_twice`, `test_inq_missing`, `test_get_missing`), following
  the same "one rfunc per non-templated sub-test block" precedent
  established by `pio_decomp_tests.cpp`'s `free_before_close`.
- NC4-iotype filtering: the Fortran's `PIO_TF_Get_nc4_iotypes` (confirmed
  via `tests/general/util/pio_tutil.F90:428-448` to return exactly the
  netcdf4p/netcdf4c/netcdf4p_nczarr subset) has no equivalent helper in
  `tests/util/spio_test_common.hpp`. Resolved with a small file-local
  `static bool is_nc4_iotype(int)` comparing against
  `PIO_IOTYPE_NETCDF4C`/`PIO_IOTYPE_NETCDF4P`/
  `PIO_IOTYPE_NETCDF4P_NCZARR` — no change to shared headers.
- **Error-handler-matrix scope ruling**: the Fortran registers 3
  `add_pio_test` ctest variants for one executable
  (`tests/general/CMakeLists.txt:343-374`) — default (BCAST),
  `--pio-tf-err-handler=PIO_REDUCE_ERROR`, and
  `--pio-tf-err-handler=PIO_INTERNAL_ERROR` (`WILLFAIL`) — driven by a
  global CLI flag (`pio_tutil.F90:1526-1528`) with no equivalent in
  `tests/util/spio_test_framework.hpp`. Ruled out of scope for this port:
  adding CLI/iosystem-wide-error-handler infrastructure to the shared
  C++ framework would be new shared-infra work no other ported file has
  needed, and handler-matrix coverage (BCAST/REDUCE/RETURN) already
  exists independently via the already-ported
  `tests/functional/ncdf_eh_one_test.cpp`, which loops over all three
  handlers internally for its own sub-test. The C++ port therefore
  registers exactly one `ncdf_fail_cxx` ctest target, dropping the
  REDUCE/INTERNAL variants as an accepted, documented scope reduction.
- **Error-handler-default correction**: discovered that the SCORPIO C
  library's actual runtime default iosystem error handler is
  `PIO_INTERNAL_ERROR` (`src/clib/core/pioc.cpp:48`, copied into every
  iosystem-init path), not `PIO_BCAST_ERROR` as this port's plan
  incorrectly assumed in its Global Constraints — the Fortran framework
  only appears to default to BCAST because `pio_tutil.F90` sets it
  itself, independent of the C library default. Without an explicit
  override, every intentional-failure call in this file would abort via
  `MPI_Abort` instead of returning an error code. Fixed by adding
  `PIOc_set_iosystem_error_handling(tf.get_iosysid(), PIO_BCAST_ERROR,
  &old_handler)` immediately after `init_iosystem()` succeeds in
  `main()` — the identical, pre-existing pattern already used in
  `tests/functional/pio_file_fail.cpp:81` and
  `tests/functional/ncdf_eh_one_test.cpp:28-30`. This is now the third
  ported file needing this exact override; a future refactor could hoist
  it into `Test_framework::init_iosystem()` itself, but that is left as
  a follow-up, not actioned here.
- Independent per-function file provisioning: since the C++ framework has
  no equivalent to the Fortran driver's shared cross-sub-test state
  (`tgv_fname` created once per iotype, reused by multiple sub-tests),
  `test_redef_with_no_write` and `test_redef_twice` each independently
  create, use, and delete their own copy of `TGV_FNAME =
  "pio_ncdf_test_file.nc"` per iotype iteration.
- Faithfulness ruling: `test_clob_then_no_clob`'s `PIOc_strerror` log
  line is only reached when `PIOc_strerror` itself succeeds (gated via
  `check_ret`), mirroring the Fortran's `PIO_TF_CHECK_ERR`-induced
  `RETURN` before reaching the equivalent log line on failure — confirmed
  correct via cross-reference to `pio_tf_f90gen.pl:388`.
- Task reviewer independently re-verified every Global Constraint and
  Review Focus item against the diff (not the implementer's report);
  independently confirmed the `PIO_INTERNAL_ERROR` default and the
  `WILLFAIL` marker on the Fortran's `ncdf_fail_intrnl` variant as
  corroborating evidence for the error-handler-default fix; re-ran a
  clean build (0 warnings) and `ctest -R ncdf_fail_cxx` (3/3 Passed);
  confirmed `tests/general` untouched.
- 1 Important finding, non-blocking: this roadmap entry and checkbox were
  not yet updated at review time — pure bookkeeping, not implementer
  rework; actioned by this very log entry.
- 4 Minor findings parked, none blocking: narrower iotype coverage vs.
  the Fortran (pre-existing repo-wide via `get_iotypes()`); `old_handler`
  captured but never restored (matches `pio_file_fail.cpp`'s existing
  pattern); two dead initializers at `ncdf_fail.cpp:239`; `passert` not
  early-returning like the Fortran macro (intentional repo-wide design).
- Single-task plan: the task review doubled as the whole-branch final
  review.
- Final review verdict: **Approved**. `tests/general` untouched.

### `pio_decomp_tests_3d`

- Commit `74284f7061` (base `3575d5612a`). Created
  `tests/functional/pio_decomp_tests_3d.cpp`; appended a
  `pio_decomp_tests_3d_cxx` block to `tests/functional/CMakeLists.txt`
  (knobs copied verbatim from `tests/general/CMakeLists.txt:666-674`).
- Architecture: one non-templated helper `get_3d_col_decomp_info` (ported
  line-for-line from the Fortran subroutine, 1-based arithmetic preserved)
  computing a 3D column decomposition in two modes (`force_rearrange`
  true/false); one function template `nc_write_read_3d_col_decomp<T>`
  (instantiated for `int`/`float`/`double`) that writes with the
  forced-rearrange decomp and reads back with the natural decomp over
  every registered iotype, verifying via `check_val`; `main()` follows
  `pio_decomp_tests.cpp`'s exact rearranger-loop precedent
  (`PIO_REARR_BOX`/`SUBSET`/`CONTIG`).
- **Fortran-to-C dimension-order reversal** (the key new pattern this
  file introduces, reusable by every later multi-dimensional-decomposition
  port): confirmed via direct inspection of `src/flib/spio_decomp.F90.in`
  (`cgdims(i) = gdims(ndims-i+1)` before `PIOc_InitDecomp`) and
  `src/flib/spio_def_var.F90` (`cvdimids(i) = vdimids(ndims-i+1) - 1`
  before `PIOc_def_var`), cross-checked against
  `src/clib/core/rearr/pio_rearrange.cpp`'s `idx_to_dim_list` (confirming
  the C API's own offset-decoding treats the last `gdimlen` entry as
  fastest-varying). Since this port calls the raw C API directly, it
  reproduces both reversals itself: `wr_gdims`/`rd_gdims = {dims[2],
  dims[1], dims[0]}` passed to `PIOc_InitDecomp`, and `dimids =
  {hgt_dimid, col_dimid, row_dimid}` passed to `PIOc_def_var`, while
  `dims[]`/`start[]`/`count[]` and the compdof/value arithmetic keep the
  Fortran's own (unreversed) order and 1-based values throughout.
- Task reviewer independently re-derived the reversal rule from the same
  three source files (not just trusting the plan's claim) and closed the
  loop arithmetically: showed the compdof formula reduces exactly to the
  1-based rightmost-fastest linear index that `idx_to_dim_list` consumes.
  Also independently verified: `dims[]` is mode-independent between the
  two `get_3d_col_decomp_info` calls (only `start`/`count` for the column
  dimension vary) per the Fortran source; the `wbuf`/`compdof` and
  `rbuf`/`exp_val`/`compdof` flattening loops are each internally
  self-consistent (buffer index `n` <-> `compdof[n]`); the write-value and
  expected-value formulas are character-identical (mirroring the
  Fortran's duplicated formula); `get_iotypes()` is queried fresh inside
  the templated function (not hoisted/shared); both `PIOc_freedecomp`
  calls happen once, after the iotype loop, in `rd`-then-`wr` order
  (matching the Fortran). Also confirmed the brief's code block was
  committed byte-for-byte unmodified, and that `check_val` is
  `MPI_Allreduce`-backed so early-return-on-mismatch cannot desync ranks.
- 0 Critical, 0 Important, 3 Minor findings parked, none blocking:
  error paths inside the iotype loop leak `ncid`/`wr_iodesc`/`rd_iodesc`
  on an already-failing test (matches `pio_decomp_tests.cpp`'s existing
  convention and the Fortran `PIO_TF_CHECK_ERR` semantics being ported —
  explicitly "no change recommended"); `get_iotypes()`'s return value is
  discarded (matches every existing caller repo-wide); the shared literal
  test filename `"test_pio_decomp_simple_tests.testfile"` is reused
  across ~8 Fortran decomp tests (plan-mandated verbatim preservation,
  pre-existing property, not introduced by this task).
- Single-task plan: the task review doubled as the final review — no
  separate final review dispatch needed.
- Final review verdict: **Approved**. `tests/general` untouched.

### pio_iosystem_tests2

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-iosystem-tests2-to-cxx.md`.
  Commit `e22dfe08ea7084bf93376dd49a9666a54a3efefe` (base `74284f7061`).
- Architecture: `tests/functional/pio_iosystem_tests2.cpp` creates and manages
  a SECOND, independent PIO iosystem on an odd/even-split MPI sub-communicator
  (raw C API `PIOc_Init_Intracomm`), alongside the test framework's own
  (primary) iosystem — the first port in this series to do so. Helper
  functions: `split_world_odd_even` (both parities get a valid, non-null
  comm via distinct colors, unlike `MPI_UNDEFINED`-based splitting),
  `create_file`/`check_file`/`open_and_check_file` (text-attribute
  write/read, no `PIOc_enddef`, following `pio_copy.cpp`'s precedent). No
  custom driver block in the Fortran source, so `main()` loops over all 3
  rearrangers for the primary iosystem (established convention).
- Deliberate, documented design ruling: the Fortran file defines
  `split_world_only_even` but never calls it in this file (confirmed via
  grep — its only call site and only other definition live in the
  still-unported `pio_iosystem_tests.F90.in`, which carries its own
  duplicate copy). Ruled to OMIT a C++ equivalent here; the real port is
  deferred to when `pio_iosystem_tests.F90.in` is itself ported. Task
  reviewer independently re-examined (not rubber-stamped) and agreed:
  omitting it is additive-only (no behavior lost, Fortran file untouched),
  and a ported unused `static` function would have triggered
  `-Wunused-function`, conflicting with the branch's zero-warning goal.
- Collective-safety argument for reusing `check_ret`/`passert`/`check_val`
  (which hardcode `tf.get_comm()`, the full world comm, for their internal
  `MPI_Allreduce`) without a new comm-scoped helper: every world rank calls
  every checked operation in this test exactly once, regardless of which
  iosystem/parity group is logically involved, so the full-world reduction
  is safe. Task reviewer independently re-derived this and noted it is
  actually *safer* than the Fortran original, which reduces over the
  sub-comm for second-iosystem checks and would deadlock on a
  single-parity failure.
- `PIOc_Init_Intracomm` parameter mapping independently re-derived by the
  reviewer against `src/clib/pio_api.h` and `src/flib/spio_init.F90`'s
  `pio_init_intracomm` wrapper: `num_iotasks` is the sub-comm's own size
  (`odd_even_size`, from `MPI_Comm_size` on the split comm), not any
  world-scoped value; `naggprocs` has no raw-C-API equivalent and is
  correctly dropped. Per-parity file selection (`is_even ? fname1 :
  fname2`) verified to match the Fortran's *code* (the Fortran source's
  own adjacent comment is backwards/stale; the C++ correctly follows the
  code, not the misleading comment). Open/close and
  `PIOc_finalize`/`MPI_Comm_free` ordering verified balanced across all 3
  rearranger iterations.
- CMake target uses `_C1_` knobs (`PIO_TF_C1_MAXNUMIOPROCS`/
  `PIO_TF_C1_MAXSTRIDE`) and `MINNUMPROCS 4`, copied verbatim from this
  file's own Fortran registration — confirmed to correctly diverge from
  sibling files' `_C2_` knobs (per-file, not a fixed repo-wide pattern).
- Implementer self-identified and fixed one brief gap: the plan's code
  block omitted the standard `#ifdef SPIO_ENABLE_GPTL_TIMING` guard present
  in all 14 prior `tests/functional/*.cpp` files; a hard build break
  without it (this build has GPTL enabled). Added the identical guard in
  the identical position. Task reviewer confirmed this was a plan-writing
  oversight (not implementer error), verified it is byte-identical to all
  13 prior files, and recommended future plan/brief templates in this
  series bake the guard in directly.
- 4/4 ctest variants passing (np4 × nio{1,2,2,4} × st{1,1,2,1}), build
  clean, 0 warnings. Reviewer independently corroborated via
  `ctest -R pio_iosystem_tests2_cxx`.
- 0 Critical, 0 Important, 3 Minor findings parked, none blocking: a code
  comment referencing "this task's plan" that is untracked and dangles for
  future readers (suggest self-contained wording next time); `odd_even_rank`
  written but never read (harmless, mirrors the Fortran helper's signature,
  address-taken so no `-Wunused`); error-path resource leaks
  (`odd_even_iosysid`/`odd_even_comm`/open ncids) on early-return paths,
  matching the established convention from `pio_decomp_tests_3d`/
  `ncdf_fail` (the harness aborts the whole process on a check failure, so
  inert in practice).
- Single-task plan: the task review doubled as the final review — no
  separate final review dispatch needed.
- Final review verdict: **Approved**. `tests/general` untouched.

### pio_rearr

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-rearr-to-cxx.md`.
  Commit `8bc0afb550dc026ddc89f2d1bb4069fbdabf47dd` (base `e22dfe08ea`).
- Architecture: `tests/functional/pio_rearr.cpp` ports two
  `PIO_TF_AUTO_TEST_SUB` tests (`test_rearrs_base`, `test_rearrs_combs`)
  and their two shared file-scope helpers (`create_file`,
  `open_and_check_rdwr`), 1:1 with the Fortran source. Each test creates a
  file via the primary (tf-managed) iosystem, then repeatedly builds a
  SECOND iosystem on an `MPI_Comm_dup`'d copy of the world comm (not a
  split — first port in this series to vary `base` across 0..comm_sz-1)
  to exercise read/write through that second iosystem. No custom driver
  block in the Fortran source, so `main()` loops over all 3 rearrangers for
  the primary iosystem (established convention).
- Non-templated, `float`/`PIO_REAL`-only (the Fortran source only ever uses
  `real`), diverging from the templated `pio_decomp_tests.cpp`/
  `pio_decomp_tests_3d.cpp` siblings — correct 1:1 fidelity choice, not an
  oversight.
- Error-handling style deliberately chosen as the ad-hoc
  `if(ret != PIO_NOERR){ logger.log(...); return PIO_EINTERNAL; }` +
  `check_val` pattern (matching `pio_decomp_tests.cpp`/
  `pio_decomp_tests_3d.cpp`), not `check_ret`/`passert`. Confirmed during
  planning that the AGENTS.md-cited style exemplar
  (`tests/cunit/test_spio_rearr_contig.cpp`) actually uses neither helper
  (zero grep hits) — there is no single repo-wide error-handling style, so
  style was matched to the closest architectural sibling instead.
- `rearrs_perms` 4x2 table: task reviewer independently re-decoded the
  Fortran's actual column-major `reshape` call (not its stale/backwards
  comment) and confirmed the C++ `[MAX_PERMS][NUM_REARRANGERS]` row-major
  table, indexed `[k][j]`, reproduces it exactly — the implementer followed
  the Fortran's code, correctly ignoring its misleading comment (same
  stale-comment pattern as `pio_iosystem_tests2`'s `split_world` helpers).
- `base` parameter: `test_rearrs_base` varies `base = k` for
  `k = 0..dup_comm_sz-1` (0-based, matching the Fortran's 1-based `k-1`);
  `test_rearrs_combs` always passes `base = 0`. Both independently verified
  by the reviewer against the Fortran source line numbers.
- `PIOc_Init_Intracomm`'s `base` parameter confirmed (via
  `src/clib/api/spio_io_sys_api.cpp` and reviewer's independent read of
  `src/clib/core/pioc.cpp`'s validation) to pass straight through with
  range validation `base ∈ [0, num_comptasks)` — no clamping surprises.
- CMake target uses `_C3_` knobs (`PIO_TF_C3_MAXNUMIOPROCS`/
  `PIO_TF_C3_MAXSTRIDE`) and `MINNUMPROCS 4`, copied verbatim from this
  file's own Fortran registration — a new variant vs. prior ports' `_C1_`/
  `_C2_` knobs (per-file, not a fixed repo-wide pattern). The per-test
  Fortran CMake comment `# Costly test` was NOT carried forward (reviewer
  separately noted that comment actually belongs to `pio_rearr_opts2` in
  the Fortran CMakeLists, not `pio_rearr` — a pre-existing misattribution
  in the Fortran file, irrelevant to this port either way).
- Implementer hit a local build-environment issue unrelated to the new
  code: an earlier interrupted build attempt corrupted the pre-existing
  `build/src/clib/libpioc.a` (truncated to an empty archive), breaking
  linking of every target depending on it. Fixed by removing the corrupted
  (untracked, git-ignored) archive and rebuilding the `pioc` target; no
  repository source was touched. Reviewer independently confirmed via
  `git diff --name-only` that the diff touches only
  `tests/functional/pio_rearr.cpp` and `tests/functional/CMakeLists.txt`,
  and that `build/` is untracked, so none of this bled into the commit.
- 2/2 ctest variants passing (`pio_rearr_cxx_np4_nio2_st1`,
  `_st2`), build clean. Reviewer independently re-ran `ctest -R
  pio_rearr_cxx` (2/2 passed) and additionally ran the binary directly on 4
  ranks, counting log lines to confirm every nested loop iteration actually
  executes as designed (36 `Base =` lines; rearranger counts matching the
  perm table's prediction).
- 0 Critical, 0 Important, 6 Minor findings parked, none blocking: a
  128-char vs. actual-length attribute write (benign, attribute never read
  back); unused `dup_comm_rank` in `test_rearrs_base` (faithful to the
  Fortran, which is equally unused; address-taken so no `-Wunused`);
  `check_val`'s internal `MPI_Allreduce` uses `tf.get_comm()` rather than
  the `comm` argument threaded through `open_and_check_rdwr` (harmless here
  since `dup_comm` is a dup with an identical rank set — worth knowing if
  this helper is ever reused with a genuinely different communicator);
  unused `old_handler` initializer (matches Fortran, never restored); only
  the `pnetcdf` iotype was exercised in the local build environment (same
  limitation as every prior port); the stale `# Costly test` CMake
  attribution noted above.
- Single-task plan: the task review doubled as the final review — no
  separate final review dispatch needed.
- Final review verdict: **Approved**. `tests/general` untouched.

### pio_iosystem_tests3

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-iosystem-tests3-to-cxx.md`.
  Two-task plan — Task 1 commit `2bbcd96186` (base `8bc0afb550`), Task 2
  commit `901602f487` (base `2bbcd96186`).
- Architecture: `tests/functional/pio_iosystem_tests3.cpp` ports the single
  `PIO_TF_AUTO_TEST_SUB` `three_files_two_iosystems_with_overlap` plus its
  4 shared helpers (`split_world_two_with_overlap`, `create_file`,
  `check_file`, `open_and_check_file`). Unlike `pio_iosystem_tests2`'s
  disjoint odd/even split, this file splits world into 2 sub-communicators
  built via `MPI_Group_range_incl` that **overlap** at rank 0 and are
  otherwise disjoint — a real correctness hazard, not just an API
  difference: checking a per-sub-comm code path with a world-scoped
  `MPI_Allreduce` would mismatch collective-call counts across ranks
  (hang), since non-rank-0 world ranks only ever enter one of the two
  sub-comm paths.
- Task 1 added backward-compatible comm-scoped overloads
  `passert(tf, MPI_Comm, bool, string)` / `check_ret(tf, MPI_Comm, int,
  string)` to the shared `tests/util/spio_test_check.hpp`, with the
  existing 3-arg overloads rewritten as one-line delegations via
  `tf.get_comm()`. Verified (both task review and final review) that all
  13 real call sites across the test suite (not just the 4 originally
  named in the plan) resolve correctly by arity alone — including under
  MPICH 4.1.2, where `MPI_Comm` is an `int` typedef, so overload
  resolution could not rely on a distinct handle type. `check_val<T>()`
  deliberately left untouched (unused by this port, out of scope per the
  plan — no scope creep).
- Task 2 ported `split_world_two_with_overlap`'s two-pass
  `MPI_Group_range_incl` group-building exactly, including the Fortran's
  array-layout translation (column-major `dimension(3,NUM_GROUP_RANGES)`
  → row-major `int[NUM_GROUP_RANGES][3]`, both contiguous-triple layouts
  that `MPI_Group_range_incl` expects identically) and its asymmetric
  `pio_base_rank`/`pio_num_iotasks` computation (`base=1,
  iotasks=size-1` when a sub-comm's size > 1, since rank 0 is always
  shared; else `base=0, iotasks=size`). Both task and final reviewers
  independently hand-traced the group arithmetic for world_sz=4 and
  world_sz=6 against the Fortran and confirmed exact membership match
  (evens vs. `{0}` ∪ odds, overlapping only at rank 0).
- The Fortran's pointer-swap-before-use filename selection (1-based `j=1`
  → `fname2`, `j=2` → `fname1`) was ported as a 0-based pointer toggle
  (`j=0` → `fname2`, `j=1` → `fname1`) — confirmed correct both by hand
  and empirically via a direct 4-rank `mpiexec` run showing the expected
  per-`j` file sequence in the log output.
- Every check inside a per-overlapped-comm code path uses the new 4-arg
  comm-scoped `check_ret`/`passert` with the correct sub-communicator
  (`overlapped_comms[i]`/`[j]`); only genuinely all-rank operations (the
  three `create_file` calls, the world-visible `ncid0` open/close) use the
  pre-existing 2-arg world-scoped form. No world-scoped collective was
  found on any partial-participation path.
- Cleanup (`PIOc_finalize` + `MPI_Comm_free`) happens once per non-null
  `overlapped_comms[i]`, in a loop after the full iotype loop completes,
  matching the Fortran; `MPI_COMM_NULL` comms are correctly skipped.
- CMake registration uses the `_C1_` knob family and `MINNUMPROCS 4`,
  copied verbatim from this file's own Fortran registration; no "Costly
  test" comment (none exists on the Fortran side — that comment belongs to
  `pio_rearr_opts2`, per the `pio_rearr` log entry above). Empirically
  confirmed the C++ target expands to the same 4 ctest variants as the
  Fortran one.
- Test verification: `ctest -R pio_iosystem_tests3_cxx` 4/4 passed. Final
  review additionally forced fresh rebuilds (not cached) of
  `pio_iosystem_tests3_cxx` and the 4 other `spio_test_check.hpp` consumers
  (`pio_iosystem_tests2_cxx`, `ncdf_fail_cxx`, `pio_copy_cxx`,
  `pio_file_fail_cxx` — 18/18 passed), ran the full ported-C++-suite
  regression (`ctest -R '_cxx_'`, 80/80 passed), and re-ran the original
  unmodified Fortran `pio_iosystem_tests3` ctest variants (4/4 passed) for
  a behavioral cross-check. Only `pnetcdf` iotype was exercised (same
  local-environment limitation as every prior port).
- 0 Critical, 0 Important findings across both task reviews and the final
  review. Minor findings parked, none blocking: a cosmetic line-length
  asymmetry at the Task 1/Task 2 seam (threading `MPI_Comm comm` through
  helper signatures widened a couple of lines past this port series'
  usual width, though within existing codebase precedent); unused
  `overlapped_comm_ranks[]` output (faithful to the Fortran helper's
  signature, harmless); an inherited (not introduced) partial-failure hang
  risk identical to the Fortran macro's own semantics; one fewer redundant
  allreduce than the Fortran performs (safe, matches the `pio_iosystem_tests2`
  convention); rank-0-only vs. all-ranks failure logging (cosmetic,
  established convention across this port series); `pio_iosystem_tests3_cxx`
  shares output file basenames with `pio_iosystem_tests2_cxx` (inherited
  verbatim from the Fortran originals, harmless under serial ctest, means
  the two targets can't run concurrently).
- Two-task plan: the final whole-branch review was a separate dispatch,
  not folded into Task 2's own review, since cross-task interface
  integration (Task 2 against Task 1's new overloads, in the combined
  tree, from a forced-fresh build) genuinely needed its own check.

### ncdf_simple_tests

- Single-task plan: one new file, `tests/functional/ncdf_simple_tests.cpp`
  (674 lines), plus one appended `tests/functional/CMakeLists.txt` block.
  No shared header changed.
- Architecture: the Fortran file's custom `PIO_TF_TEST_DRIVER_BEGIN/END`
  block (looping `PIO_TF_Get_nc_iotypes()`, pre-creating a shared probe
  file `tgv_fname` per iotype, running every auto-test against it) was
  flattened using the exact pattern already established and approved in
  `ncdf_fail.cpp`: each of the 7 Fortran sub-tests became a self-contained
  C++ function that loops `get_iotypes()` itself and delegates to a
  `static <name>_one_iotype` helper. The three sub-tests that depend on
  the driver's pre-created `tgv_fname` (`test_redef_enddef`, `test_def_dim`,
  `test_def_var`) each create, use, and delete that file themselves per
  iotype iteration, matching `ncdf_fail.cpp`'s `test_redef_with_no_write`
  precedent.
- The one `PIO_TF_TEMPLATE`-templated sub-test, `test_data_conversion`
  (confirmed via the generated `build/tests/general/ncdf_simple_tests.F90`
  to instantiate exactly 3 times: int/real/double), was ported as
  `template<typename T> test_data_conversion(...)` using the existing
  `Pio_type_map<T>` struct and `check_val<T>()` helper from
  `tests/util/spio_test_check.hpp` — the exact pattern already established
  in `pio_decomp_tests.cpp`'s `nc_write_1d_darray<T>`. Registered as 3
  separate `rfuncs` entries (`test_data_conversion_int`/`_float`/`_double`).
- Pre-plan research resolved three open questions before any code was
  written:
  1. **Iotype set ruling**: Fortran's `PIO_TF_Get_nc_iotypes` (full read of
     `tests/general/util/pio_tutil.F90:315-417`) actually enumerates ALL
     defined iotypes (pnetcdf, adios/adiosc, hdf5/hdf5c, netcdf family) —
     not just the "nc" family its name implies. The already-approved
     `ncdf_fail.cpp` (same driver macro) had already deliberately narrowed
     this to the existing `get_iotypes()` helper's 5 NC-family flavors.
     This port followed that established precedent for architectural
     consistency with its direct sibling file, explicitly documenting the
     fidelity gap as inherited from `ncdf_fail.cpp`'s prior, separate port
     rather than a new regression — fixing it would mean touching a
     different, already-merged file, out of this task's scope.
  2. **`#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC` reachability**: confirmed via
     `CMakeLists.txt:71-80` and `tests/functional/CMakeLists.txt:14-16`
     that this is a real, reachable build option (forced `ON` under
     `WITH_HDF5`) already propagated to the C++ functional-test directory
     — both branches are live code and were both ported verbatim, not
     collapsed to one.
  3. **`test_redef_twice` cleanup**: confirmed by reading the Fortran sub
     body that it creates `pio_test_create_redef_twice.nc` but never
     deletes it — the port intentionally matches this (no added delete
     call), documented inline with a comment.
- The task brief itself had one bug, caught by the implementer at compile
  time: its `test_data_conversion` pseudocode called a nonexistent
  `PIOc_syncfile`. Fixed to `PIOc_sync` (the correct API, confirmed by the
  reviewer three independent ways: `src/clib/pio_api.h:123`'s actual
  declaration, a repo-wide zero-hit grep for `PIOc_syncfile`, and 8 other
  `tests/functional/*.cpp` files already using `PIOc_sync` in the same
  post-write position).
- The implementer also self-caught and non-destructively corrected a git
  hygiene slip: the first commit attempt swept in an unrelated,
  pre-staged file (this roadmap doc) because the brief's commit command
  had no pathspec. Fixed via `git reset --soft HEAD~1` +
  `git restore --staged <file>` and recommitted with only the 2 intended
  files — verified by the reviewer via `git show --stat` and `git reflog`
  (abandoned commit still recoverable, nothing force-pushed or discarded).
- Build/test: `ncdf_simple_tests_cxx` builds with zero warnings; both the
  new C++ binary and the original (untouched) Fortran `ncdf_simple_tests`
  binary exit 0 under `mpiexec -n 4`, all 9 registered sub-tests PASSED.
  The reviewer independently re-ran the binary rather than trusting the
  report (binary mtime matched source mtime, confirming the artifact
  genuinely corresponds to the committed source). Only `pnetcdf` iotype
  was exercised (same local-environment limitation as every prior port).
  `tests/general/ncdf_simple_tests.F90.in` confirmed untouched
  (`git diff` empty, no commit on this branch touches it).
- CMake block verified copied verbatim from the Fortran file's own block
  (`tests/general/CMakeLists.txt:317-325`: `MINNUMPROCS 4`,
  `MINNUMIOPROCS 2`/`MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}`,
  `MINSTRIDE 2`/`MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}`), explicitly NOT
  inheriting `ncdf_fail_cxx`'s different `MINSTRIDE 1` values.
- Single-task plan: the task review doubled as the final whole-branch
  review (0 Critical, 0 Important findings). 3 Minor findings parked, none
  blocking: the roadmap doc ended up re-stageable-but-unstaged after the
  implementer's git-hygiene fix (re-staged as part of this close-out);
  a few `int` loop counters over vector indices where house style prefers
  `std::size_t` (cosmetic, no warning risk since the loop bound is a
  `const int`); and an internally-inconsistent error-path cleanup inside
  `test_data_conversion_one_iotype` (some failure paths free the two
  iodescs, most don't) — confirmed to match the identical bare-return
  style already present in `pio_decomp_tests.cpp`, so not a new deviation.

### pio_async_init_finalize

- Single-task plan: one new file,
  `tests/functional/pio_async_init_finalize.cpp` (369 lines), plus one
  appended `tests/functional/CMakeLists.txt` block. Commit:
  `b7de52232010a56aa3588269dd63fd8f5f427305` (base
  `98db39da4a810c70d654f41b6c5bf187d66378a9`).
- No file I/O, no decompositions — this file exercises only the
  asynchronous (multi-component intercomm) I/O initialization/finalization
  lifecycle. Reused the async-I/O architecture already established in
  `pio_async_decomp_tests.cpp`/`pio_async_file_tests.cpp`: `main()`
  bypasses `init_iosystem()`/`finalize_iosystem()` entirely, and each of
  the 4 registered functions manages its own comm-split +
  `PIOc_init_intercomm()`/`PIOc_finalize()` lifecycle directly. Two helper
  subroutines were duplicated as file-local `static` functions rather than
  shared via `tests/util`, mirroring both the C++ precedent
  (`split_world_ncomms` already duplicated per-file across the
  `pio_async_*` family) and the Fortran's own precedent
  (`split_world_odd_even` is independently defined in 3 separate
  `tests/general/*.F90.in` files, confirmed via grep).
- Not templated — no `PIO_TF_TEMPLATE` anywhere in the Fortran reference —
  so each of the 4 `PIO_TF_AUTO_TEST_SUB_BEGIN/END` blocks became exactly
  one plain (non-templated) function: `async_1comp_init_finalize`,
  `async_1comp_init_finalize_v2`, `async_loop_init_finalize`,
  `async_2comp_init_finalize`.
- Two rulings made and documented in the plan before dispatch, both
  independently re-verified by the task reviewer from the diff itself
  (not taken on faith from the implementer's report):
  1. **`PIO_init`/`PIO_init_v2` C++ identity**: confirmed by reading
     `src/flib/spio_init.F90:45-49,219-271` that both Fortran overloads
     (`pio_init_intercomm`, `pio_init_intercomm_v2`) call the exact same
     underlying C function, and the C API (`src/clib/pio_api.h:58-60`)
     exposes only one entry point, `PIOc_init_intercomm()`. Both
     `async_1comp_init_finalize` and `_v2` were ported as separate
     functions (preserving 1:1 Fortran test-name coverage) but are
     structurally identical C++ code — confirmed byte-identical by the
     reviewer via a direct diff of the two function bodies.
  2. **Collective-check safety**: unlike the sibling async files (where
     error checks happen *during* active I/O-vs-compute divergence,
     forcing local-only `nerrs` accumulation), every `PIO_TF_CHECK_ERR` in
     this Fortran file appears *after* the unconditional
     `MPI_Comm_free(<comm>, ret)` that every rank reaches at the end of
     each iteration — by which point the I/O rank has already returned
     from its blocking `PIO_init()` call. Confirmed via
     `tests/general/util/pio_tf_f90gen.pl:429-441` that the plain 2-arg
     `PIO_TF_CHECK_ERR` form itself reduces over the full `pio_tf_comm_`.
     The port therefore uses `check_ret()`/`passert()`
     (`tests/util/spio_test_check.hpp`, full-comm `MPI_Allreduce`)
     directly at all 4 check sites, rather than the sibling files' local
     `nerrs` pattern. The reviewer independently verified, line-by-line
     from the diff, that every one of the 4 `check_ret()` call sites
     strictly follows that iteration's unconditional `MPI_Comm_free`, and
     traced the no-deadlock argument from source rather than trusting the
     report.
- Review Focus items (world_sz>1 guard in the 1-component tests;
  odd/even role *inversion* across `async_loop_init_finalize`'s 5
  iterations; `async_2comp_init_finalize`'s `>= MIN_NUM_PROCS` guard
  — not `>` — and its 0-based `comp_idx = gcomm_idx - 2` indexing;
  unconditional `MPI_Comm_free` on every path) were each independently
  re-derived and verified by the reviewer against the Fortran source,
  including hand-tracing the 2-component rank-to-component mapping at
  np=3, 4, and 7, and confirming (by running `mpiexec` directly at np=1
  and np=2) that the size guards silently no-op rather than fail.
- The implementer again self-caught and non-destructively corrected the
  same git-hygiene slip seen in the prior task (`ncdf_simple_tests`): a
  first commit attempt swept in the pre-staged roadmap doc because the
  `git commit` call carried no pathspec; fixed via `git reset --soft
  HEAD~1` + `git restore --staged`, recommitted with only the 2 intended
  files. Verified clean by the reviewer via `git show --stat` (exactly 2
  files, 382 insertions, 0 deletions). Unlike the prior task, the
  roadmap doc's staging state was actually preserved this time (reviewer
  found it still `A`-staged, correcting a minor inaccuracy in the
  implementer's own concern note).
- Build/test: zero warnings on a forced clean rebuild; `ctest -R
  pio_async_init_finalize_cxx -V` 3/3 pass (np=2,3,4); reviewer also ran
  direct `mpiexec` at np=1, 2, 5, 7, all exiting 0 with every sub-test
  PASSED. `tests/general/pio_async_init_finalize.F90.in` confirmed
  untouched (`git diff --stat` empty for `tests/general`).
- CMake block verified copied verbatim from this file's own Fortran block
  (`tests/general/CMakeLists.txt:246-254`: `MINNUMPROCS 2`,
  `MINNUMIOPROCS 1`/`MAXNUMIOPROCS 1`, `MINSTRIDE 1`/`MAXSTRIDE 1`),
  target/test named `pio_async_init_finalize_cxx` per the established
  `_cxx` convention — deliberately NOT copying the Fortran's own
  inconsistent `add_pio_test` shortname (`async_init_finalize`, dropping
  the `pio_` prefix), which is unique to this one Fortran block and not
  followed by any prior C++ port.
- Single-task plan: the task review doubled as the final whole-branch
  review (0 Critical, 0 Important findings). 6 Minor findings parked, all
  cosmetic or spec-conformant-as-written, none blocking: (1) the C++
  check validates `PIOc_init_intercomm`/`PIOc_finalize`'s return rather
  than the Fortran's literal `ret` (which the Fortran's own
  `MPI_Comm_free(..., ret)` clobbers before the check) — a strictly
  stronger check, explicitly what the brief directed; (2) uninitialized
  `iosysids[]`/`comp_comms[]` plain arrays in the 3 one-component
  functions (vs. `async_2comp_init_finalize`'s initialized
  `std::vector`s) — brief-prescribed, harmless since both branches always
  assign the live slots; (3) `split_world_odd_even` discards MPI return
  codes, matching both the brief's mandated `void` signature and the
  Fortran's own discarded `ierr`; (4) the CMake comment drops the
  Fortran's "Note: " prefix — the plan's own code block already dropped
  it, so the implementer correctly followed the plan text as written;
  (5) dead `rank`/`sz`/`new_rank`/`new_size` locals, mirroring the
  Fortran's own unused variables; (6) a report-accuracy-only note (not a
  code issue) that the roadmap doc's staging state was actually preserved
  through the implementer's git-hygiene fix, contrary to what the
  implementer's own concern note claimed.

### pio_decomp_tests2_1d

- Single-task plan: one new file, `tests/functional/pio_decomp_tests2_1d.cpp`
  (~460 lines), plus one appended `tests/functional/CMakeLists.txt` block.
  Commit: `35fef55fc8695dc24c7967fd50ab6837ddc79394` (base
  `b7de52232010a56aa3588269dd63fd8f5f427305`).
- No async behavior — this file uses the standard synchronous-iosystem
  `main()` pattern (`init_iosystem()`/`run()`/`finalize_iosystem()` over the
  3 rearrangers), modeled directly on `tests/functional/pio_decomp_tests.cpp`
  (closest sibling: templated `Pio_type_map<T>`-based write/read-darray
  tests, `get_iotypes()`/`get_iotype_name()` iotype enumeration,
  `check_val<T>()` verification, both `PIO_TEST_CLOSE_OPEN_FOR_SYNC`
  branches live).
- Two file-local helpers (`get_1d_a2a_info`, `get_1d_bc_info`, both with
  `VEC_BLOCK_SZ = 7`) and two templated test functions (`nc_wr_rd_a2a<T>`,
  `nc_wr_rev_rd<T>`), each instantiated for int/float/double (6 total
  `rfuncs` registrations), matching the Fortran's 2 `PIO_TF_TEMPLATE`
  auto-tests.
- **Ruling made and documented in the plan before dispatch**: the Fortran's
  explicit per-iotype skip of `PIO_IOTYPE_ADIOS`/`PIO_IOTYPE_ADIOSC`
  ("Read/Writes with different I/O decomps is not supported") is omitted
  entirely in the C++ port, since `get_iotypes()` (the established iotype
  helper used by every prior port) never returns those two flavors in the
  first place — confirmed via grep that no prior `.cpp` port has ever
  needed to replicate this skip. The reviewer independently re-verified
  `get_iotypes()` usage at both call sites and confirmed no coverage
  regression beyond the already-established precedent.
- Review Focus items, all independently re-verified by the reviewer against
  the Fortran source rather than trusted from the implementer's report:
  1. `get_1d_a2a_info`'s `force_all2all=true` branch (write decomposition:
     `sz` discontiguous `VEC_BLOCK_SZ`-sized regions strided by `lsz`) is
     structurally distinct from the `force_all2all=false` branch (read
     decomposition: one contiguous region of size `lsz`) — exact 1-based
     Fortran→0-based C++ arithmetic conversion confirmed
     (`start[i] = (rank*VEC_BLOCK_SZ + 1) + i*lsz`).
  2. `nc_wr_rev_rd`'s read decomposition is a cyclic LEFT-SHIFT by 1 of the
     block (`[start+1, start+2, ..., start+count-1, start]`), not a second
     reversal of the write buffer — verified element-by-element against the
     Fortran's `exp_val(i-1) = start(1) + i - 1` (`i=2..count(1)`) plus
     `exp_val(count(1)) = start(1)`.
  3. Both `PIO_TEST_CLOSE_OPEN_FOR_SYNC`/`#else` branches present and
     reachable in both templated functions (4 occurrences total).
  4. `tot_count` in `nc_wr_rd_a2a` confirmed to be the SUM over all
     `count[i]` entries, not a single region's count.
  5. 1-based Fortran index arithmetic preserved throughout (no "fix" to
     0-based global positions) — every `compdof` entry derives from
     `wbuf`/`exp_val` via `static_cast<PIO_Offset>`, mirroring the
     Fortran's `int(wbuf(k))` round-trip.
- CMake block deviated from the plan's literal placeholder snippet: the
  implementer used the real repo convention found in all 19 existing
  `tests/functional/CMakeLists.txt` sibling blocks
  (`add_spio_executable(<name> TRUE "" <file>.cpp
  ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)`) instead of the brief's
  `FALSE ${PIO_LINKER_LANGUAGE}` snippet (which was actually the Fortran
  block's own style, `tests/general/CMakeLists.txt:624`, mistakenly carried
  into the plan's example). This was explicitly sanctioned by the plan's
  own instruction to confirm against sibling blocks and not invent new
  macro arguments; the reviewer independently read the file and confirmed
  the match against all 19 siblings. `add_pio_test` knobs
  (`MINNUMPROCS 1`, `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`,
  `MINNUMIOPROCS 1`, `MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}`,
  `MINSTRIDE 1`, `MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}`,
  `TIMEOUT ${DEFAULT_TEST_TIMEOUT}`) copied verbatim from
  `tests/general/CMakeLists.txt:626-634`, target/test named
  `pio_decomp_tests2_1d_cxx` per the established `_cxx` convention.
- Build/test: `ctest -R pio_decomp_tests2_1d_cxx -V` 10/10 sub-cases pass
  (np1-4, various nio/stride combos, all 3 rearrangers, all 6 `rfuncs`);
  reviewer independently re-ran a np1/np3 subset (100% pass) and a direct
  `mpiexec -n 3` run, confirming via output that the iotype loop body is
  actually entered (non-vacuous test). Only the pnetcdf iotype was
  available in this build environment (same limitation as
  `pio_decomp_tests.cpp`), so the `PIO_TEST_CLOSE_OPEN_FOR_SYNC` branch
  wasn't runtime-exercised, though both branches are present as reachable
  code. `tests/general/pio_decomp_tests2_1d.F90.in` confirmed untouched
  (`git diff --stat` empty for `tests/general`).
- Single-task plan: the task review doubled as the final whole-branch
  review (0 Critical, 0 Important findings). 4 Minor findings parked, all
  cosmetic, none blocking: (1) iodesc leak on early-return error paths (no
  `PIOc_freedecomp` before `return PIO_EINTERNAL`) — identical to the
  established pattern in `pio_decomp_tests.cpp`, fixing here alone would
  break consistency; (2) `int k` used as a flattened vector index rather
  than `std::size_t` in `nc_wr_rd_a2a`, mirroring the Fortran's own
  `integer :: k`, no correctness impact; (3) the plan's suggested CMake
  comment header (`#===== pio_decomp_tests2_1d_cxx =====`) was correctly
  omitted since no existing block in `tests/functional/CMakeLists.txt` has
  one (those headers live only in `tests/general/CMakeLists.txt`); (4)
  `compdof` vectors held for the whole function rather than freed
  immediately after `PIOc_InitDecomp` (the Fortran deallocates eagerly) —
  trivial memory, matches sibling pattern.
- Final review verdict: **Approved**. `tests/general` untouched.

### pio_async_copy_tests

- Single-task plan: one new file, `tests/functional/pio_async_copy_tests.cpp`
  (455 lines, +1 line in a follow-up fix), plus one appended
  `tests/functional/CMakeLists.txt` block. Commits:
  `d9990e5f4dc5e51a4cdbf4210b5dd2bff1d7f6fe` (base
  `35fef55fc8695dc24c7967fd50ab6837ddc79394`) and a follow-up fix
  `09cbf6fb26e972bb887e32807e573943c089c283`.
- Architecture: reuses the async-I/O (multi-component intercomm) pattern
  from `pio_async_decomp_tests.cpp`/`pio_async_file_tests.cpp` (2 compute
  components + 1 I/O comm, `main()` bypassing `init_iosystem()`/
  `finalize_iosystem()`). Adds one new helper, `create_ucomm` (a faithful
  port of the Fortran's `MPI_Allgather`+`MPI_Comm_group`+`MPI_Group_incl`+
  `MPI_Comm_create`+`MPI_Group_free`×2 sequence, `MPI_LOGICAL`→`MPI_INT`),
  building a union communicator of all non-I/O world ranks once per
  rearranger iteration; `split_world_ncomms` reused verbatim from
  `pio_async_file_tests.cpp`. One `template<typename T>` auto-test,
  `test_async_copy_att<T>` (int/float/double), copies 4 attributes
  (global scalar, global char, var scalar, var char) from an input file to
  an output file via `PIOc_copy_att` in the compute branch, then
  cross-verifies them by having each compute component reopen the *other*
  component's output file (`vidx = NUM_COMPONENTS - comp_idx - 1`,
  confirmed algebraically equivalent to the Fortran's 1-based
  `ofnames(NUM_COMPONENTS - gcomp_idx + 1)`).
- **Key ruling, resolved during planning, before any code was written**:
  `tests/util/spio_test_check.hpp` already gained comm-scoped
  `check_ret(tf, comm, ...)`/`passert(tf, comm, ...)` overloads in commit
  `2bbcd96186` (the `pio_iosystem_tests3` Task 1 work), which postdates
  `pio_async_file_tests.cpp` — that older sibling had to duplicate a
  file-local `check_ret_over_comm()` helper because the shared overloads
  didn't exist yet. This port uses the shared overloads directly instead
  of duplicating that now-superseded helper; confirmed during planning by
  reading the header in full and cross-checking that `pio_iosystem_tests3.cpp`
  already relies on them directly.
- **Check-scope ruling**: comm-scoped `check_ret(tf, new_comm, ...)`/
  `passert(tf, new_comm, ...)` used for every check made while ranks are
  diverged into I/O vs. compute roles; the single check after both
  branches rejoin (post `MPI_Comm_free(comp_ucomm)`/`MPI_Comm_free(new_comm)`)
  uses the default full-comm `check_ret(tf, ...)` — independently
  re-verified by the reviewer: 24 comm-scoped calls inside the diverged
  region, exactly one full-comm call at `pio_async_copy_tests.cpp:421`,
  correctly placed after both frees.
- **Documented, inherited (not introduced) deadlock-risk note**: the
  Fortran's `PIO_TF_CHECK_ERR` macro actually expands to an immediate
  `RETURN` from the whole subroutine on failure (confirmed via
  `tests/general/util/pio_tf_f90gen.pl:415-437`), not a loop `continue` —
  meaning a single compute component's check failure causes that
  component's ranks to skip the remaining `comp_ucomm` barriers while the
  other, healthy component's ranks still reach them and hang. The C++ port
  uses the established house `nerrs++; continue;` convention instead
  (closing any already-open files first), which carries the identical
  hazard shape as the Fortran original — not a porting defect, accepted as
  inherited spec behavior and documented in the plan's Review Focus so the
  reviewer wouldn't mistake it for a new regression.
- CMake block matches the real `add_spio_executable(... TRUE "" ...
  argparser.cxx)` convention and all 7 knobs verbatim from
  `tests/general/CMakeLists.txt:540-552` (`MINNUMPROCS 3`,
  `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`, `MINNUMIOPROCS 2`,
  `MAXNUMIOPROCS 2`, `MINSTRIDE 2`, `MAXSTRIDE 2`,
  `TIMEOUT ${DEFAULT_TEST_TIMEOUT}`), placed immediately after the
  `pio_decomp_tests2_1d_cxx` block.
- Task reviewer independently verified (not trusting the implementer's
  report): `create_ucomm` is a correct translation with both groups freed
  and no comm leak; all 3 barriers use `comp_ucomm` (zero occurrences of
  `MPI_Barrier(new_comm)`); all 15 early-exit `continue` paths in the
  iotype loop close every already-open handle; the finalize loop covers
  all `NUM_COMPONENTS` entries, not just the caller's own; no file-local
  `check_ret_over_comm` duplicate was introduced. Forced a clean rebuild
  (zero warnings) and re-ran `ctest -R pio_async_copy_tests_cxx` (2/2
  passed) plus `mpiexec -n 6` directly (exit 0, all 3 type instantiations
  × 2 rearrangers PASSED). Proved non-vacuity empirically (per-iotype log
  lines never print, since rank 0 is always the I/O rank and never enters
  the compute branch — same as every prior async port) by polling the CWD
  during a live run and catching `pio_async_copy_ifile_comp1.nc`
  materialize and then get cleaned up, confirming the compute branch
  genuinely executes.
- The implementer self-caught and non-destructively corrected the same
  git-hygiene slip seen in `ncdf_simple_tests`/`pio_async_init_finalize`: a
  first commit attempt (run without a pathspec, per the brief's literal
  Step 10 command) swept in two unrelated pre-staged files (this roadmap
  doc and `2026-10-30-port-pio-async-init-finalize-to-cxx.md`). Fixed via
  `git reset --soft HEAD~1` + recommit with explicit pathspecs, verified
  via `git log -1 --stat` that only the 2 intended files landed. The same
  slip recurred once more during this close-out while committing the
  review's one fix (an untargeted `git commit` again swept in the same two
  staged docs) and was caught and fixed the identical way before
  finalizing.
- 1 Minor finding, fixed immediately (not parked): missing
  `#include <algorithm>` for `std::fill` at
  `pio_async_copy_tests.cpp:383` — compiled today only via transitive
  libstdc++ inclusion, not standard-guaranteed. Fixed in a follow-up
  commit (`09cbf6fb26`), rebuilt clean, `ctest` re-confirmed 2/2 pass.
- 2 further Minor/informational findings parked, non-blocking: the brief's
  claim that 0-based `i + 1` filename numbering "reproduces the same
  literal filenames" as the Fortran is inaccurate (Fortran zero-pads to
  4 digits via `I4.4`; the C++ emits unpadded `comp1.nc` — harmless, purely
  test-internal, and the divergence avoids a filename collision with the
  Fortran test under parallel `ctest -j`; noted so the discrepancy isn't
  copy-pasted into a future brief); `nerrs` double-counted when
  `PIOc_init_intercomm()` fails (checked once at the point of failure, once
  more at the post-rejoin check) — inert since `nerrs` is only ever
  compared against zero, present in the brief's own reference code and
  sibling ports.
- Single-task plan: the task review doubled as the final whole-branch
  review. Final review verdict: **Approved with one fixed nit**.
  `tests/general` untouched throughout.

### pio_iosystem_tests

- Ported `tests/general/pio_iosystem_tests.F90.in` (373 lines) to
  `tests/functional/pio_iosystem_tests.cpp` (514 lines). Three auto-tests
  (`two_iosystems_odd_even`, `two_iosystems_even_all`,
  `one_iosystem_odd_even`), each managing its own second
  `PIOc_Init_Intracomm` iosystem lifecycle on an odd/even (or even-only)
  sub-communicator alongside the primary framework-managed iosystem; two
  split helpers (`split_world_odd_even` reused verbatim from
  `pio_iosystem_tests2.cpp`, `split_world_only_even` new — an
  `MPI_UNDEFINED`-based variant producing `MPI_COMM_NULL` for excluded
  odd ranks).
- Key architectural ruling: `create_file`/`check_file`/
  `open_and_check_file` take an explicit `MPI_Comm comm` parameter and use
  the comm-scoped `check_ret(tf, comm, ...)`/`passert(tf, comm, ...)`
  overloads throughout, diverging from `pio_iosystem_tests2.cpp`'s simpler
  hardcoded-full-comm helpers — required because `two_iosystems_even_all`
  and `one_iosystem_odd_even` have `is_even`-gated call sites where only a
  subset of world ranks reach the helper; a hardcoded full-comm reduction
  there would deadlock/collective-mismatch.
- `ret`-initialization ruling for `two_iosystems_even_all`: gave the C++
  local a safe `PIO_NOERR` default (the Fortran leaves it uninitialized)
  to avoid true C++ UB. The reviewer independently traced that this is
  not just safe but exactly faithful — the Fortran's `ret` is in fact
  always written by `create_file` before the first unconditional check
  on every rank, so the port's behavior is unchanged, not
  tightened/loosened.
- Review Focus (5 items, all independently verified PASS by the
  reviewer): comm-scope correctness at every call site across all three
  tests; `split_world_only_even`'s `MPI_COMM_NULL` branch never touched
  by `PIOc_Init_Intracomm`/`PIOc_finalize`/`MPI_Comm_free` (stays
  `is_even`-gated); the `ret`-initialization ruling above; no handle
  leak across an iotype-loop `continue`; the cross-iosystem
  `PIOc_deletefile` call in `one_iosystem_odd_even` — issued via the
  *primary* iosystem even though the file was created/opened via the
  secondary odd/even iosystem, unconditional, and deliberately
  **unchecked**, exactly matching the Fortran's own unchecked call (no
  check was added).
- CMake knobs copied verbatim from `tests/general/CMakeLists.txt:773-781`
  (`MINNUMPROCS 4`, `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`, `MINNUMIOPROCS 1`,
  `MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}`, `MINSTRIDE 1`,
  `MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}`, `TIMEOUT ${DEFAULT_TEST_TIMEOUT}`).
- Build/test: `ctest -R pio_iosystem_tests_cxx` 4/4 passed; direct
  `mpiexec -n 4` and `-n 5` runs both 12/12 passed (3 tests x 3
  rearrangers), independently reproduced by the reviewer with identical
  numbers. The `-n 5` run (3 even / 2 odd) is a non-vacuous exercise of
  the odd-rank and `MPI_UNDEFINED` code paths.
- Git-staging mishap recurred a third time in this series (bare
  `git commit -m` after `git add` sweeping in an unrelated pre-staged
  file) — but for the first time, the implementer itself caught and
  fixed it via `git reset --soft HEAD~1` + pathspec'd recommit before
  ever reporting back, rather than it surfacing at review or controller
  level. Final commit (`ed652cd71b3bc1ce97ced585c1a1ee598d2f5920`)
  verified clean by both the controller and the reviewer.
- 3 Minor/cosmetic findings parked, non-blocking, no code changes made:
  `PIO_WRITE` used instead of the Fortran's `PIO_nowrite` in
  `open_and_check_file` (matches explicit brief instruction and
  `pio_iosystem_tests2.cpp` precedent, behaviorally inert here);
  `ncid1`/`ncid2` in `two_iosystems_even_all` not reset to `-1` after
  close with unguarded `PIOc_closefile` calls on error paths (verified no
  actual double-close risk — both are always freshly reassigned by
  `open_and_check_file` before any subsequent close; error-path-only, no
  collective-mismatch risk); `odd_even_rank` written but never read in
  all three tests (matches existing convention in the sibling
  `pio_iosystem_tests2.cpp`, cosmetic).
- Single-task plan: the task review doubled as the final whole-branch
  review. Final review verdict: **Approved with nits parked (no code
  changes needed)**. `tests/general` untouched throughout.

### pio_decomp_tests_2d

- Ported `tests/general/pio_decomp_tests_2d.F90.in` (391 lines) to
  `tests/functional/pio_decomp_tests_2d.cpp` (582 lines), mirroring the
  existing `pio_decomp_tests_3d.cpp` sibling with one fewer dimension. Two
  decomposition helpers (`get_2d_col_decomp_info`, `get_2d_row_decomp_info`)
  and two `template<typename T>` auto-tests (`nc_write_read_2d_col_decomp`,
  `nc_write_read_2d_row_decomp`), each instantiated for int/float/double —
  write using a forced-rearrangement layout, read back using a different
  natural layout, verify the round-trip value.
- Key fidelity point (Review Focus item 1): the col-decomp value formula
  uses the *local* row count (`nrows`/`count[0]`), while the row-decomp
  formula uses the *global* row count (`dims[0]`) plus the global row
  offset (`start[0]`) — these look similar but are not interchangeable,
  since rows are split unevenly across ranks only in the row decomposition.
  The reviewer validated this by direct line-by-line comparison against
  the exact Fortran source lines (F90:186/206 and F90:305/325), not by
  test execution alone — see note below.
- Review Focus (5 items, all independently verified PASS by the
  reviewer): the col/row value-formula distinction above; the mirror-image
  `force_rearrange`-gating between the two helpers (col gates
  columns/`start[1]`/`count[1]`, row gates rows/`start[0]`/`count[0]`);
  dimension-order reversal for the C API (`gdims = {dims[1], dims[0]}`,
  `dimids = {col_dimid, row_dimid}`); the `rank + 1 < sz` trailing-
  unpaired-even-rank edge case preserved verbatim in both helpers
  (exercised at `sz` = 1, 2, 3, 4); the literal filename
  `"test_pio_decomp_simple_tests.testfile"` reused verbatim across
  iotype-loop iterations with a `PIOc_deletefile` each iteration (matches
  the Fortran and the `pio_decomp_tests_3d.cpp`/`pio_decomp_tests.cpp`
  siblings — intentional reuse, not a bug).
- `VEC_ROW_SZ`/`VEC_COL_SZ` correctly kept at this file's own value of 7
  (not copy-pasted from the 3D sibling's `= 2`). CMake knobs copied
  verbatim from `tests/general/CMakeLists.txt:653-661`
  (`MINNUMPROCS 1`, `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`,
  `MINNUMIOPROCS 1`, `MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}`,
  `MINSTRIDE 1`, `MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}`,
  `TIMEOUT ${DEFAULT_TEST_TIMEOUT}`).
- Build/test: `ctest -R pio_decomp_tests_2d_cxx` 10/10 passed; direct
  `mpiexec -n 2` and `-n 3` runs both passed (6 auto-tests x 3 rearrangers
  each), independently reproduced by the reviewer with identical numbers,
  confirming non-vacuous exercise of both the "has a higher-ranked odd
  partner" and "trailing unpaired even rank" branches.
- Informational note from the reviewer (not a defect, no action taken):
  a mutation check showed the test suite cannot actually distinguish a
  local-vs-global row-count swap in the row-decomp formula at the process
  counts exercised here, because the written value equals the global
  index and the mutated index sets still cover each other within bounds.
  This weakness is inherited verbatim from the Fortran original (not
  introduced by the port) — recorded here so it isn't rediscovered as a
  surprise by a future reviewer of a similar decomp-test file; the
  reviewer compensated by validating the formula against the Fortran
  source directly rather than relying on test execution to catch a
  formula swap.
- Zero findings (no blocking, no nits) — the cleanest review in this
  series so far.
- Single-task plan: the task review doubled as the final whole-branch
  review. Final review verdict: **Approved, zero findings**.
  `tests/general` untouched throughout (confirmed via
  `git diff --stat master..HEAD -- tests/general/` being empty across the
  whole branch, not just this one commit).

### `pio_async_decomp_tests_1d` -> `tests/functional/pio_async_decomp_tests_1d.cpp`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-async-decomp-tests-1d-to-cxx.md`
- Commit: `f4e109992d` on `jayeshkrishna/port_fortran_tests_to_cxx`
  (`tests/functional/pio_async_decomp_tests_1d.cpp` new, 633 lines;
  `tests/functional/CMakeLists.txt` +13).
- Architecture: combines the async (intercomm) I/O lifecycle already
  established in `pio_async_decomp_tests.cpp`/`pio_async_file_tests.cpp`/
  `pio_async_copy_tests.cpp` (`split_world_ncomms()` splits `tf.get_comm()`
  into 1 I/O + 2 compute sub-comms; `PIOc_init_intercomm()`/
  `PIOc_finalize()` manage async iosystems directly, bypassing
  `Test_framework::init_iosystem()`) with full decomposed file
  write/read round-trip checks across every discovered iotype, in two
  templated (`int`/`float`/`double`) auto-tests: an evenly-divided 1D
  decomposition, and a "with holes" variant where even ranks (within
  their own compute sub-comm) absorb the next odd rank's share, leaving
  odd ranks with 0 local elements.
- Key fidelity decisions (all confirmed correct by the reviewer):
  the final per-rearrangement check uses `MPI_Comm_free()`'s own return
  code via a dedicated `mpi_free_ret` variable, not `PIOc_finalize()`'s
  — the Fortran overwrites `ret` with `MPI_Comm_free`'s result
  immediately before its only reachable-by-all-ranks check, silently
  discarding whatever `PIO_finalize` returned; `check_ret_over_comm()`
  (Allreduce scoped to the compute sub-comm, copied verbatim from
  `pio_async_file_tests.cpp`) used for every PIO-call check after I/O
  role divergence to avoid deadlocking the I/O rank blocked inside
  `PIOc_init_intercomm()`; `get_1d_bc_with_holes_info()` called
  unconditionally for every compute rank, producing a legitimate
  `count == 0` for odd ranks that is passed through to
  `PIOc_InitDecomp`/`PIOc_write_darray`/`PIOc_read_darray` unguarded, never
  special-cased; 1-based `gcomm_idx` (1=I/O, 2..3=compute) mapped to
  0-based `comp_idx = gcomm_idx - 2`; the two test functions'
  `wbuf` formulas kept distinct — the no-holes version is a *constant*
  array (every element = this rank's *world* rank, matching Fortran's
  scalar-broadcast `wbuf = pio_tf_world_rank_`), the with-holes version
  varies per index (`wbuf[k] = start + k`) — with both "Got wrong val"
  comparisons using plain local `nerrs++` accumulation (no Allreduce),
  since the Fortran's `PIO_TF_ASSERT` performs no cross-rank
  communication at all. `PIOc_sync()`/`PIOc_finalize()` returns checked
  explicitly (a deliberate strengthening beyond the Fortran, which checks
  neither) per established sibling precedent — confirmed deadlock-safe.
  CMake knobs copied verbatim from `tests/general/CMakeLists.txt:636-648`.
- Build/test: implementer built `pio_async_decomp_tests_1d_cxx` clean,
  `ctest -R pio_async_decomp_tests_1d_cxx` passed (np3, np4), manual
  `mpiexec -n 6` run passed all 6 functions. Reviewer independently
  re-ran `ctest` (2/2 pass) and additionally ran `mpiexec -n 7` (not
  covered by the implementer), exercising both the `count == 0`
  odd-rank-hole branch and the `rank + 1 == sz` trailing-even-rank
  half-count branch in the same run — all 6 functions passed.
- Review findings: no CRITICAL/MAJOR. Several MINOR/NIT findings, all
  either inherited precedent shared with the three already-merged
  `pio_async_*` siblings (compute-side diagnostic logging is a no-op
  since the logger only emits on rank 0 of `tf.get_comm()`, which the
  I/O rank never reaches in the diverged region — flagged as a
  cross-file follow-up candidate, out of scope for this single-file
  port) or intentional, brief-sanctioned strengthenings
  (`PIOc_sync`/`PIOc_finalize` checked where the Fortran checks
  neither). No fixes applied; nits parked in the SDD ledger.
- Single-task plan: the task review doubled as the final whole-branch
  review. Final review verdict: **APPROVE_WITH_NITS**. `tests/general`
  confirmed untouched (`pio_async_decomp_tests_1d.F90.in` has no diff
  against `master`).

### `pio_rearr_opts2.F90.in` → `tests/functional/pio_rearr_opts2.cpp`

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-rearr-opts2-to-cxx.md`.
  Commit: `f1c6724b55` ("test: port pio_rearr_opts2 to
  tests/functional/pio_rearr_opts2.cpp").
- Architecture: synchronous (non-async) test, unlike the four
  recently-ported `pio_async_*` files — uses the existing shared
  `tests/util/spio_test_check.hpp` helpers (`check_val`, `check_ret`,
  `passert`) directly, no new file-local helper. Confirmed via
  `tests/general/util/pio_tf_f90gen.pl`'s macro expansions
  (`PIO_TF_CHECK_ERR`/`PIO_TF_CHECK_VAL`/`PIO_TF_ASSERT` all expand to
  Fortran `RETURN`) that the correct C++ control flow is
  `return PIO_EINTERNAL;` immediately on any check failure — a materially
  different pattern from the async files' "nerrs++; continue to next
  combo." Reviewer confirmed (grep for `nerrs`/`continue;`) no leftover
  async-style pattern leaked in from the recent async-file work.
- `get_1d_bc_info(rank, sz, dim, start, count, force_rearrange)` ported
  with full fidelity, including the `force_rearrange == false` branch,
  which is dead code in this file (both call sites pass `true`) but kept
  per house convention. This was the plan's highest flagged risk (a
  sign/branch error could pass at small process counts and fail only at
  larger ones); reviewer independently re-derived both the Fortran and
  C++ formulas and exhaustively verified them equal for every rank at
  every `sz = 1..32`, for both `force_rearrange` values (zero mismatches),
  additionally confirming `[start, start+count)` tiles `1..dim` exactly
  (no gaps/overlaps) in all 64 configurations.
- `set_rearr_opts_and_write`: single, non-templated `float`/`PIO_REAL`
  test function (matches the Fortran's `real`/`PIO_real`-only usage, no
  `Pio_type_map` template left over). 8-level nested loop exercises
  `PIOc_set_rearr_opts()` across `comm_type` (`PIO_REARR_COMM_P2P`/
  `_COLL`) × `fcd` (`FC_2D_DISABLE`/`FC_1D_COMP2IO`/`FC_1D_IO2COMP`/
  `FC_2D_ENABLE`, exact Fortran array order) × the 6 flow-control axes
  (`enable_hs`/`enable_isend`/`max_pend_req`, separately for c2i and
  i2c). `comm_type == COLL` correctly restricts the fcd loop to 1 value
  (`FC_2D_DISABLE`); the 6 inner axes' iteration counts are correctly
  gated by the current `fcd` (full counts only on the axes that `fcd`
  enables, forced to a single default value otherwise) — 170 combos per
  iotype for `P2P` (1+12+12+144), 1 for `COLL`, matching the Fortran.
  File is created/opened once per iotype and stays open across the whole
  nested loop, closed+deleted once at the end of that iotype's
  processing; the only in-loop close/reopen is the
  `#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC` branch, which legitimately
  mirrors the Fortran's own `#ifdef` (default path uses `PIOc_sync`
  instead). `main()` registers this single function via the same
  `rfuncs`/`tf.run()` idiom as sibling files, with one
  `tf.init_iosystem(PIO_REARR_BOX)` call (no rearranger loop — the
  Fortran source has none), per Global Constraints.
- Implementer deviated from the brief in three places, all judged
  reasonable by the reviewer: used the real
  `get_iotype_name(int, char *)` signature (the brief's description of a
  `std::string`-returning helper was simply wrong) — matches
  `pio_decomp_tests2_1d.cpp`'s idiom, with an added `iotype_name_buf[0] =
  '\0'` safety init not present in the sibling; interpreted the brief's
  otherwise-unused `iotype_name` parameter on `create_file_and_var` as
  the source of a "Testing : <iotype>" log line (matches the Fortran's
  log content/order, though siblings log at the call site instead of in
  the callee — cosmetic); single-rfunc/single-rearranger reconciliation
  as above, explicitly mandated by the plan text.
- CMake block appended after `pio_async_decomp_tests_1d_cxx`, knobs
  copied verbatim from `tests/general/CMakeLists.txt:568-580`
  (`MINNUMPROCS 1`, `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`,
  `MINNUMIOPROCS 1`, `MAXNUMIOPROCS ${PIO_TF_C3_MAXNUMIOPROCS}`,
  `MINSTRIDE 2`, `MAXSTRIDE ${PIO_TF_C3_MAXSTRIDE}`, including the
  "# This is a costly test" comment). `_C3_` tier variables confirmed
  already defined locally in `tests/functional/CMakeLists.txt`.
- Build/test: implementer built `pio_rearr_opts2_cxx` clean, `ctest -R
  pio_rearr_opts2_cxx` 2/2 passed (np3/np4, ~11-35s). Reviewer
  independently re-ran the same ctest target (2/2 passed, ~15-45s) rather
  than trusting the report.
- Review findings: no CRITICAL/blocking. Nits: `PIOc_sync`'s return is
  checked though the Fortran ignores it (benign strengthening);
  `create_decomp_and_init_buf`'s `PIOc_InitDecomp` failure path uses a
  bare `if`+return rather than the collective `check_ret`/`passert`
  helpers used everywhere else in the file — as prescribed by the brief,
  not an implementer error, but a latent non-collective-failure deadlock
  risk if `PIOc_InitDecomp` ever failed on a strict subset of ranks
  (flagged as a follow-up candidate, out of scope for this port); the
  Fortran's explicit `else`/`PIO_TF_ERROR("Unexpected ...")` fallbacks for
  unrecognized `fcd`/`comm_type` values were dropped in favor of relying
  on the local `const` option arrays making those branches provably
  unreachable (readability preference only); "Testing : <iotype>" logged
  by the callee rather than the call site (cosmetic, see deviation above).
  No fixes applied; nits parked in the ledger.
- Single-task plan: the task review doubled as the final whole-branch
  review. Final review verdict: **APPROVE_WITH_NITS**. `tests/general`
  confirmed untouched (`git diff f02418a46f..f1c6724b55 -- tests/general/pio_rearr_opts2.F90.in`
  empty; `git log --follow` shows its last commit, `fb8c15c7a4`, is far
  outside the port range).

### `pio_large_file_tests.F90.in` → `tests/functional/pio_large_file_tests.cpp`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-large-file-tests-to-cxx.md`
- Commit range: `f7df2cc61d..d3ee769038` (port) + one follow-up fix commit
  on `jayeshkrishna/port_fortran_tests_to_cxx`.
- Architecture: templated over `int`/`float`/`double`, 3 functions mirroring
  the Fortran's 3 `PIO_TF_AUTO_TEST_SUB` blocks, gated end-to-end by the
  `PIO_ENABLE_LARGE_TESTS` CMake option (OFF by default, same as Fortran).
- Scope ruling: `nc_wr_rd_large_1d_bc<T>` ported as a 2-line log+return
  stub only, since the Fortran source disables this test via an
  unconditional early `RETURN` (with a `! REMOVE THIS RETURN STMT...`
  comment) — the ~95 lines of decomposition/write/read logic below it,
  and its sole helper `get_1d_bc_info`, are unreachable dead code in
  Fortran and were deliberately not ported. The unused `ONE_G` constant
  in `init_file_fmts` was likewise omitted.
- `nc_def_large_1d_bc<T>` and `nc_write_large_1d_fval<T>` ported in full,
  preserving their deliberately divergent behavior: only the latter calls
  `PIOc_deletefile` (the former's Fortran source leaves that call
  commented out, relying on `PIO_CLOBBER` overwrite), and each uses its
  own distinct filename (`test_pio_large_file_tests.testfile` vs.
  `test_pio_decomp_fillval_tests.testfile`).
- **Known pre-existing failure (not a porting defect):** with
  `PIO_ENABLE_LARGE_TESTS=ON`, `nc_def_large_1d_bc_{int,float,double}`
  FAIL on the CDF1+pnetcdf iotype combination with netCDF error -62
  ("variable sizes violate format constraints") — 6 of 9 sub-tests pass,
  no crash, `ctest` reports the 3 affected invocations as failed. Both
  the implementer and the reviewer independently confirmed this is a
  **pre-existing upstream fragility in the Fortran original**, not
  something introduced by the port: (1) the exact comment "NetCDF
  documentation says max_rec_sz should be 2**31 - 4, but it no longer
  works... So trying out 2**31 - 2**10 instead" already exists verbatim
  in `tests/general/pio_large_file_tests.F90.in:69-72`, i.e. the original
  Fortran authors already knew this file-format sizing is fragile across
  netCDF/PnetCDF library versions; (2) the reviewer built and ran the
  actual Fortran `pio_large_file_tests` binary from the same checkout and
  it fails identically (same -62 error, same stage, same 3 sub-tests,
  `STOP 99`); (3) the reviewer independently re-derived every
  `init_file_fmts()` constant (`ONE_K`/`TWO_G`/`FOUR_G`, all three
  `Pio_file_fmt_info` entries, `pio_flag` bitwise-OR vs. Fortran's `IOR`)
  byte-for-byte against the Fortran source and found no drift. This
  should be filed as a separate upstream issue against the Fortran test;
  it is explicitly out of scope for this porting task (`tests/general`
  must never be modified).
- Deviation from brief (accepted): added
  `PIOc_set_iosystem_error_handling(tf.get_iosysid(), PIO_BCAST_ERROR, &old_handler)`
  in `main()`, matching Fortran's `PIO_TF` driver default error handler
  (`pio_tf_err_handler_` in `tests/general/util/pio_tutil.F90`, confirmed
  set to `PIO_BCAST_ERROR`). Without it, the C++ port's default handler
  (`PIO_INTERNAL_ERROR`) would `MPI_Abort()` the whole process on the
  CDF1 failure above instead of returning a checkable error code — a
  strictly worse-fidelity outcome than Fortran's actual (non-crashing,
  but failing) behavior. Exact precedent already in this codebase:
  `tests/functional/ncdf_fail.cpp:383-389` uses the identical idiom with
  an identical justifying comment. Reviewer independently verified this
  is non-masking: every `PIOc_*` call the Fortran checks via
  `PIO_TF_CHECK_ERR` is checked here via `check_ret`, so a non-fatal
  error still correctly becomes a reported sub-test failure, and BCAST's
  identical-return-on-all-ranks property avoids a collective-call
  deadlock/divergence risk.
- Review findings: no CRITICAL/blocking. One fidelity nit fixed as a
  follow-up: the `PIO_TEST_CLOSE_OPEN_FOR_SYNC` reopen branch used
  `PIO_WRITE` where the Fortran uses `PIO_nowrite` (and the sibling port
  `pio_decomp_tests2_1d.cpp` uses `PIO_NOWRITE` for the identical
  pattern) — corrected to `PIO_NOWRITE`; this branch is not compiled in
  the default build config (`PIO_TEST_CLOSE_OPEN_FOR_SYNC` is only forced
  on for ADIOS/HDF5), so it was untested either way. Remaining nits
  (stronger `check_ret`-wrapping of 3 calls the Fortran leaves unchecked;
  a second standalone `if (PIO_ENABLE_LARGE_TESTS)` block in
  `tests/functional/CMakeLists.txt` instead of folding into the existing
  one) are cosmetic/benign, parked without action.
- Single-task plan: the task review doubled as the final whole-branch
  review. Final review verdict: **APPROVE_WITH_NITS**. `tests/general`
  confirmed untouched (`git diff f7df2cc61d..d3ee769038 -- tests/general/pio_large_file_tests.F90.in`
  empty).

### `pio_rearr_opts.F90.in` → `tests/functional/pio_rearr_opts.cpp`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-rearr-opts-to-cxx.md`
- Commit range: `5b33b3e948..d6fcf18c85` (initial port) + one fix-round
  commit `2f83c754e1` on `jayeshkrishna/port_fortran_tests_to_cxx`.
- Architecture: two test functions mirroring the Fortran's two
  `PIO_TF_AUTO_TEST_SUB` blocks. `init_fin_with_rearr_opts` is a simple
  init/finalize round-trip over 2 rearrangers. `write_with_rearr_opts` is
  the main test: for every iotype, a file+var is created once on the
  framework's main iosystem, then for every rearranger x flow-control
  combination a **fresh, separate iosystem** is created, used for one
  write/read round-trip, and torn down. This is architecturally distinct
  from the sibling `pio_rearr_opts2.cpp`, which sets rearranger options on
  one long-lived iosystem via `PIOc_set_rearr_opts()`. Fortran's
  `PIO_init(..., rearr_opts=...)` has no single-call C API equivalent —
  `PIOc_Init_Intracomm_from_F90_impl` (`src/clib/core/pioc.cpp:1570-1601`)
  is literally `PIOc_Init_Intracomm(...)` followed by a conditional
  `PIOc_set_rearr_opts(...)` — so the port replicates that exact two-call
  sequence on a duplicated communicator each time via a shared helper
  `init_dup_iosys_with_rearr_opts`.
- `NUM_MAX_PEND_REQ_OPTS = 2` (`{PIO_REARR_COMM_UNLIMITED_PEND_REQ, 2}`),
  deliberately not the sibling's 3-element array. CMake knobs mirror the
  Fortran test's own registration (`MINNUMPROCS 4`, `MINNUMIOPROCS 2`,
  `MINSTRIDE 2`), not the sibling's (`1`/`1`/`2`).
- **Fix round 1 (load-bearing, caught by task review, not the
  implementer):** the initial port and its plan both mis-read the
  Fortran call
  `PIO_init(pio_tf_world_rank_, dup_comm, pio_tf_world_sz_, 1, 1, rearrs(i), dup_iosys, base=0, rearr_opts=...)`,
  treating the two literal `1`s as `num_iotasks`/`stride` and hardcoding
  `PIOc_Init_Intracomm(dup_comm, 1, 1, ...)`. `pio_init_intracomm`'s real
  signature (`src/flib/spio_init.F90:101-103`:
  `(comm_rank, comm, nioprocs, naggprocs, ioprocs_stride, rearr, iosys, base, rearr_opts, ierr)`,
  forwarding `nioprocs` to `PIOc_Init_Intracomm_from_F90`'s `num_iotasks`
  at line 140) shows the 3rd positional arg `pio_tf_world_sz_` is
  `nioprocs`/`num_iotasks`, not either literal `1` (the first is
  `naggprocs`, dropped entirely by the C API; the second is
  `ioprocs_stride`/`stride`). `pio_tf_world_sz_` =
  `MPI_COMM_SIZE(pio_tf_comm_)` (`tests/general/util/pio_tutil.F90:147`)
  and `dup_comm = MPI_Comm_dup(pio_tf_comm_)` in this test, so `dup_comm`'s
  size always equals `pio_tf_world_sz_`. Coordinator independently
  verified all three citations (the Fortran interface signature, its
  forwarding call, and the `pio_tf_world_sz_`/`dup_comm` definitions)
  before ruling and correcting the plan. Fix: derive `num_iotasks` via
  `MPI_Comm_size(dup_comm, &nioprocs)` inside
  `init_dup_iosys_with_rearr_opts` — a single-site change, since both
  test functions route through this one helper. Fix dispatched to the
  same implementer agent (round 1 of 5), independently verified by the
  coordinator (single-site, 3 insertions/1 deletion, Fortran source
  untouched), then confirmed ADDRESSED by a scoped re-review with no new
  breakage; the re-reviewer also confirmed the 4-rank test run genuinely
  exercises the changed path (`nioprocs` now resolves to 4, not a
  no-op on an already-1-sized comm).
- Other review findings, no action needed: a cosmetic dropped
  per-rearranger log line; plan-sanctioned extra checks relative to
  Fortran; cosmetic dropped unreachable `PIO_TF_ERROR` else-branches
  matching the sibling's precedent.
- Build/test: `ctest -R pio_rearr_opts_cxx` 1/1 passed both before
  (`pio_rearr_opts_cxx_np4_nio2_st2`, 57.48s) and after the fix (86.85s).
- Single-task plan: the task review + fix round + scoped re-review
  doubled as the final whole-branch review. `tests/general` confirmed
  untouched throughout by the implementer, coordinator, task reviewer,
  and re-reviewer independently
  (`git diff 5b33b3e948..2f83c754e1 -- tests/general/pio_rearr_opts.F90.in`
  empty).

### `pio_buf_lim_tests.F90.in` → `tests/functional/pio_buf_lim_tests.cpp`
- Plan: `docs/superpowers/plans/2026-10-30-port-pio-buf-lim-tests-to-cxx.md`
- Commit: `c932803c7a` on `jayeshkrishna/port_fortran_tests_to_cxx` (single
  commit, no fix round needed).
- Architecture: a free `get_iobuf_sz()` helper (literal transcription of
  the Fortran's `iobuf_sz = lbuf_sz*lbuf_type_sz*nbufs - 1`, clamped to 0)
  plus three templated test functions, each instantiated for
  `int`/`float`/`double` (9 total `rfuncs` registrations):
  `nc_wr_buf_limit_zero<T>` (one var, forces the write-buffer limit to 0
  via `PIOc_set_buffer_size_limit(0)` then queries it back with a negative
  value — a direct single-call substitution for the Fortran's two-call
  `pio_set_buffer_size_limit` query trick, unlike `pio_rearr_opts`'s
  two-call pattern), `nc_wr_mvar_impl_flush<T>` (four vars, three
  increasing-flush-threshold scenarios run in sequence, each with its own
  immediate write→sync/reopen→read+check block), and
  `nc_wr_after_impl_flush<T>` (three vars, one scenario demonstrating that
  data written after an implicit flush is only guaranteed visible after an
  explicit sync/close). Decomposition reuses the `VEC_LOCAL_SZ=7` /
  `dims[0] = VEC_LOCAL_SZ * comm_size` / `compdof[i] = VEC_LOCAL_SZ *
  comm_rank + (i+1)` pattern already established in
  `tests/functional/pio_decomp_large_tests.cpp`; `PIOc_InitDecomp`/
  `PIOc_freedecomp` bracket the iotype loop once per function (matching
  the Fortran exactly), not once per iotype.
- One self-corrected deviation from the plan's literal code snippet (not a
  fix round — caught and fixed by the implementer during implementation,
  confirmed correct by the coordinator and the reviewer): the brief showed
  `PIO_Offset dims[1]`, but `PIOc_InitDecomp`'s `gdimlen` parameter is
  `const int *` (`src/clib/pio_api.h:16`), so `int dims[1]` was used
  instead in all three functions, matching the actual precedent file
  `pio_decomp_large_tests.cpp`.
- The plan's Global Constraints deliberately preserved three verbatim
  Fortran inconsistencies rather than "fixing" them, all confirmed present
  in the diff by both the coordinator and the task reviewer: (1) sub 1
  reopens with `PIO_NOWRITE`, subs 2/3 reopen with `PIO_WRITE`; (2) subs 1
  and 2 share the filename `"test_pio_decomp_simple_tests.testfile"`, sub
  3 uses `"test_pio_wr_aftr_impl_flush_tests.testfile"`; (3) in
  `nc_wr_mvar_impl_flush`'s first scenario (flush-every-1-var), the
  `PIO_TEST_CLOSE_OPEN_FOR_SYNC` reopen path calls `PIOc_inq_varid` for
  `var1`, `var2`, AND `var3`, even though `var3` isn't written or read
  back until the next scenario — an asymmetry present in the original
  Fortran (`tests/general/pio_buf_lim_tests.F90.in:218-225`) and preserved
  exactly, not normalized away.
- Review: task reviewer (opus) found full spec compliance against the
  Fortran source, verified line-by-line for all 8 called-out risk areas
  (decomp bracketing, per-scenario immediate read-and-check ordering, the
  scenario-1 `var3`-inq asymmetry, the NOWRITE/WRITE reopen split, the
  filename reuse, the `get_iobuf_sz` clamp across `sizeof(int)=4`/
  `sizeof(float)=4`/`sizeof(double)=8`, the 9 rfuncs registrations, and the
  CMake knobs). Also independently cross-checked PIO API call counts
  between the Fortran and the port (`PIOc_inq_varid` 15, `PIOc_write_darray`
  13, `PIOc_read_darray` 13, `PIOc_set_buffer_size_limit` 6 — all matching).
  No Critical or Important findings. Minor notes (a dropped
  unreachable-in-practice log line on the `get_iobuf_sz` clamp branch;
  inherited ~700-line duplication across the three functions, accepted
  because the Fortran itself has this shape and every sibling
  `tests/functional` port follows the same 1:1-diffable verbose style;
  inherited leaked-`ncid`/`ioid`-on-error-abort pattern, identical to the
  precedent file; inherited filename-collision risk with
  `pio_decomp_tests2_1d.cpp` if ever run concurrently) — all accepted
  as-is, no fix round needed.
- Build/test: `ctest -R pio_buf_lim_tests_cxx` 6/6 passed, both by the
  implementer (77.40s) and independently re-run by the coordinator
  (66.21s) before dispatching the task reviewer.
- Single-task plan: the task review doubled as the final whole-branch
  review (no findings to fix, so no fix round or re-review was needed).
  `tests/general/pio_buf_lim_tests.F90.in` confirmed untouched throughout
  by the implementer, the coordinator
  (`git diff --stat 236c2fbd50..c932803c7a --
  tests/general/pio_buf_lim_tests.F90.in` empty), and the task reviewer
  independently.

### `pio_decomp_fillval.F90.in` (540 lines) -> `tests/functional/pio_decomp_fillval.cpp` (864 lines)

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-decomp-fillval-to-cxx.md`,
  single task (5 templated subroutines, each instantiated for
  `int`/`float`/`double`, 15 `rfuncs` total).
- Architecture: 4 decomposition-based functions
  (`nc_write_1d_explicit_fval`, `nc_write_1d_implicit_fval`,
  `nc_read_1d_explicit_fval`, `nc_read_1d_implicit_fval`) exercising
  `PIOc_InitDecomp`/`PIOc_write_darray`/`PIOc_read_darray` with
  caller-supplied fill values, plus one non-decomposition function
  (`test_set_fill`) exercising `PIOc_set_fill`/`PIOc_redef`/`PIOc_put_var`/
  `PIOc_get_var` directly on whole numeric and `PIO_CHAR` variables.
- Two genuinely different hole-creation mechanisms, both preserved exactly:
  "explicit" variants keep full-length compdof/buffers and mark holes with
  a `PIO_COMPDOF_FILLVAL = 0` compdof sentinel (odd 1-based Fortran
  positions = 0-based C++ indices 0,2,4,6 — the parity flip was the single
  highest-risk translation point and was gotten right); "implicit"
  variants instead allocate a genuinely shorter, rank-dependent-length
  compdof/buffer (`wcompdof_sz = min(rank+1, VEC_LOCAL_SZ)`) and never
  reference the remaining global indices at all, from either side.
- `nc_read_1d_implicit_fval` has two deliberate deviations from its three
  siblings, both preserved and called out in a code comment: `BUF_FILLVAL`
  is declared `const int` (the Fortran source declares it as a plain
  `integer`, not the templated `PIO_TF_FC_DATA_TYPE` used everywhere else
  in this file), and its `PIOc_write_darray` call passes `NULL` for the
  fillvalue argument (the Fortran's corresponding `PIO_write_darray` call
  omits the fillvalue actual argument entirely, unlike the other three
  decomposition functions).
- `test_set_fill`'s fillmode ordering preserved exactly: define all 4 vars
  (2 numeric via `Pio_type_map<T>::value`, 2 `PIO_CHAR` regardless of
  `T`) -> `PIOc_set_fill(PIO_NOFILL)` -> `enddef` -> put the two "nofill"
  vars -> `PIOc_redef` -> `PIOc_set_fill(PIO_FILL)` -> `enddef` -> put the
  two "fill" vars. Char-variable buffers use `std::string::resize(DIM_LEN,
  ' ')` to reproduce Fortran's blank-padded fixed-length character
  assignment (`CHARACTER(len=100)` pads with spaces, not null bytes) so
  the round-trip comparison is faithful.
- One plan-authoring slip, self-corrected by the implementer: the plan
  text named the sync-branch API `PIOc_syncfile` (an incorrect
  back-formation from the Fortran's `PIO_syncfile`); the implementer
  recognized the real C API is `PIOc_sync` (`src/clib/pio_api.h:123`,
  also the name used throughout the `pio_buf_lim_tests.cpp` precedent) and
  used it correctly without flagging it as a deviation requiring a
  ruling — confirmed correct by both the coordinator and the task
  reviewer independently.
- `#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC` / `#else PIOc_sync` branches
  preserved verbatim in all 5 functions; decomposition bracketing
  (`PIOc_InitDecomp`/`PIOc_freedecomp` once per function, outside the
  iotype loop) preserved in all 4 decomposition functions.
- Filename patterns preserved exactly: `nc_write_1d_explicit_fval` uses a
  per-iotype-numbered suffix (`"test_pio_decomp_fillval_tests.testfile" +
  std::to_string(i+1)`, matching the Fortran's `write(filename,'(a,i1)')`
  with a 1-based iotype index); the other three decomposition functions
  use a fixed filename; `test_set_fill` reuses
  `"test_pio_ncdf_get_put.testfile"` verbatim from the Fortran (that name
  belongs to the not-yet-ported `ncdf_get_put` test — safe since files are
  deleted via `PIOc_deletefile` at the end of every iotype iteration).
- Review: task reviewer (opus) found full spec compliance against the
  actual Fortran source (read in full, not just the brief), verified
  line-by-line against every risk area called out in the plan's Review
  Focus section (explicit/implicit hole semantics not conflated, correct
  side punched in each of the 4 explicit/implicit variants, index-parity
  translation, per-function `rbuf` pre-read initialization value —
  `0` vs `BUF_FILLVAL` — correct in each of the 4 decomposition
  functions, both `nc_read_1d_implicit_fval` deviations preserved,
  `test_set_fill` fillmode ordering and char blank-padding, `#ifdef`
  branches, decomposition bracketing, filename patterns). Also
  independently verified every `PIOc_*` signature used against
  `src/clib/pio_api.h`, forced a clean rebuild, and re-ran `ctest`
  independently. No Critical or Important findings. 3 Minor notes (`gcval`
  buffers null-filled rather than blank-filled before the char-var read —
  no behavioral difference since `PIOc_get_var` overwrites every byte
  before the comparison; `wcompdof_sz`/`rcompdof_sz` could be `const int`;
  two `PIOc_def_var` lines in `test_set_fill` exceed 100 columns, within
  existing practice for this port series) — all accepted as-is, no fix
  round needed.
- Build/test: `ctest -R pio_decomp_fillval_cxx` passed, both by the
  implementer and independently re-run by the coordinator (13.17s) and
  again by the task reviewer after a forced clean rebuild (17.02s, all 15
  rfuncs x 3 rearrangers PASSED).
- Single-task plan: the task review doubled as the final whole-branch
  review (no findings to fix, so no fix round or re-review was needed).
  `tests/general/pio_decomp_fillval.F90.in` confirmed untouched throughout
  by the implementer, the coordinator (`git diff --stat
  7908138b5f..9b387d5e2d -- tests/general/pio_decomp_fillval.F90.in`
  empty), and the task reviewer independently.

### `pio_iodesc_tests.F90.in` (634 lines) -> `tests/functional/pio_iodesc_tests.cpp` (706 lines)

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-iodesc-tests-to-cxx.md`,
  single task (3 static helper functions + 4 templated subroutines, each
  of the latter instantiated for `int`/`float`/`double`, 12 `rfuncs`
  total).
- Architecture: 3 free `static` helpers translate the Fortran's local
  (non-auto-test) SUBROUTINEs that build block-cyclic decomposition
  `start`/`count` arrays (`get_1d_bc_shift_decomp_info` — cyclic-left-shift
  single/two-region split; `get_1d_bc_mureg_info` — uniform 3-region
  decomp; `get_1d_bc_mnureg_info` — non-uniform odd/even-rank region
  counts, 3 vs 2). The 4 templated test functions exercise
  `PIOc_InitDecomp`/`PIOc_get_local_array_size` (`init_decomp_1d_get_loc_sz`,
  no runtime type loop, just 3 instantiations), a two-decomposition
  even/odd-frame record-variable write/read (`nc_wr_2d_two_iodescs`), and
  two structurally identical multi-region two-variable write/read tests
  differing only in which helper/var/dim names they use
  (`nc_mureg_wr_rd_1d_bc`, `nc_mnureg_wr_rd_1d_bc`).
- Two highest-risk translation points, both independently confirmed
  correct by the coordinator before dispatch and again by the task
  reviewer against the actual code: (1) `PIOc_setframe`'s 0-based frame
  index inverts the even/odd parity used by the Fortran's 1-based `f` to
  pick between the unshifted and left-shifted decomposition for writing —
  correctly translated as `bool is_even_frame = (((cf + 1) % 2) == 0);`,
  not the naive `(cf % 2) == 0`; (2) the C API's record/unlimited
  dimension goes FIRST in the `dimids[]` array passed to `PIOc_def_var`
  (`{time_dimid, row_dimid}`), the reverse of the Fortran's
  row-then-time `pio_dims` order.
- `exp_val` in `nc_wr_2d_two_iodescs` is computed only from the unshifted
  compdof/dims formula for every frame — intentional (both decomps write
  the same global value pattern via different local-to-global row
  mappings), not a bug; preserved as specified, with no
  `exp_val_shifted` introduced.
- `get_1d_bc_mnureg_info`'s region loop preserved as two independent,
  simultaneously-firing `if` statements (`i < NUM_REGIONS_ODD` and
  `i < NUM_REGIONS_EVEN`, both can fire since `NUM_REGIONS_EVEN=2 <=
  NUM_REGIONS_ODD=3`), never collapsed to `if`/`else`.
- CMake registration copies the existing Fortran `pio_iodesc_tests`
  entry's own "C2" tier knobs verbatim (`MINNUMPROCS 1`,
  `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`, `MINNUMIOPROCS 1`,
  `MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}`, `MINSTRIDE 1`,
  `MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}`) rather than the heavier "C3" tier
  used by `pio_decomp_fillval`/`pio_rearr_opts`.
- Review: task reviewer (opus) found full spec compliance (PASS) after
  comparing all 7 routines/helpers line-by-line against the actual
  Fortran source (not just the brief), confirming both highest-risk
  points above in the actual code, confirming the CMake knobs are
  byte-identical to the existing Fortran registration, confirming diff
  scope touched only the two intended files, confirming the Fortran
  source byte-identical to master, and independently forcing a clean
  rebuild (zero warnings) and re-running `ctest` itself
  (10/10 passed, 86.03s) rather than trusting the implementer's report.
  Code quality: APPROVED WITH MINOR FINDINGS (no Critical/Major) — (1)
  `rbuf` wasn't re-zeroed per iotype-flavor iteration in
  `nc_wr_2d_two_iodescs` (harmless since `PIOc_read_darray` overwrites
  every element, but a test-strength gap vs. the Fortran and the file's
  other two routines, which do re-zero); (2) `idx = i * count[i] + j` in
  the two multi-region functions relies on an implicit
  uniform-region-count invariant — reviewer verified it actually holds
  in both callers (not a bug), suggested a guard comment; (3) a nit about
  dropped worked-example diagrams in helper doc comments, left as-is.
- Ruling: findings (1) and (2) were both cheap, mechanical, zero-risk
  fixes (a buffer-reset loop; two one-line comments) — the coordinator
  applied them directly rather than dispatching another
  implementer+reviewer round trip for a single-digit-line follow-up,
  then independently rebuilt (clean) and re-ran `ctest`
  (10/10 passed, 64.24s) before committing. Finding (3) left unaddressed
  (optional, no behavioral or review-scope impact).
- Build/test: `ctest -R pio_iodesc_tests_cxx` passed at every
  verification point — implementer (10/10, 56.96s), coordinator
  pre-review (10/10, 54.14s), task reviewer post-forced-rebuild (10/10,
  86.03s), coordinator post-fix (10/10, 64.24s).
- Single-task plan: the task review doubled as the final whole-branch
  review. `tests/general/pio_iodesc_tests.F90.in` confirmed untouched
  throughout — by the implementer, by the coordinator
  (`git diff --stat ee9485be60..37d566c56d --
  tests/general/pio_iodesc_tests.F90.in` empty, and again against master
  after the fix-up commit), and by the task reviewer independently.

### `pio_decomp_fillval2.F90.in` (645 lines) -> `tests/functional/pio_decomp_fillval2.cpp` (954 lines)

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-decomp-fillval2-to-cxx.md`,
  single task (5 templated test subroutines, each instantiated for
  `int`/`float`/`double`, 15 `rfuncs` total).
- Architecture: 5 fillvalue-hole variants on the 1D write/read-darray
  pattern already established by the sibling `pio_decomp_fillval.cpp`:
  `nc_wr_1d_mreg_exp_fval` (dynamically-sized decomposition, explicit
  `PIO_COMPDOF_FILLVAL=0` sentinel holes strided by `HSTRIDE`, two vars
  written from the same buffer), `nc_wr_1d_mreg_imp_fval` (implicit holes
  via a genuinely shorter write-compdof array built from a two-loop
  construction with a single running output index, full-size read
  decomposition), `nc_wr_1d_one_val` (every rank punches all slots, only
  the last global rank restores one real value), `nc_wr_1d_odd_fval`
  (odd-ranked processes punch all slots, even ranks keep full valid data),
  and `test_data_conv_with_fval` (single `T`-typed decomposition driving
  three fixed-type `PIO_INT`/`PIO_REAL`/`PIO_DOUBLE` variables, exercising
  PIO's internal I/O type conversion regardless of `T`).
- Five Review Focus items, all independently confirmed correct by both the
  coordinator and the task reviewer against the actual Fortran source
  line-by-line: (1) `nc_wr_1d_mreg_exp_fval`'s 1-based
  `(i-1)*HSTRIDE+1` -> 0-based `i*HSTRIDE` hole-index translation; (2)
  `nc_wr_1d_mreg_imp_fval`'s two-loop implicit write-compdof construction
  preserved exactly (not collapsed into one formula) — verified the
  element count closes exactly at `vec_local_wsz`; (3)
  `nc_wr_1d_one_val`'s `is_last_proc = (rank == size - 1)` branch,
  including the nprocs=1 case where rank 0 is also the last rank; (4)
  `test_data_conv_with_fval`'s fixed `PIO_INT`/`PIO_REAL`/`PIO_DOUBLE` var
  types never "corrected" to match the templated `T`, preserving the
  forced-conversion intent; (5) every one of the file's 8
  `PIOc_write_darray` calls passes a non-NULL `&BUF_FILLVAL` (unlike the
  sibling file's `nc_read_1d_implicit_fval`, which passes `NULL`).
- `PIOc_freedecomp` call order preserved per function exactly as in the
  Fortran: read-then-write in 4 of the 5 functions, but reversed to
  write-then-read in `test_data_conv_with_fval` specifically — confirmed
  via direct grep against the Fortran source by both the coordinator and
  the reviewer.
- One brief-vs-C-API spelling deviation, flagged by the implementer and
  confirmed by the reviewer as purely cosmetic: the brief used the
  Fortran-style `PIO_int`/`PIO_real`/`PIO_double`, but the C API only
  exposes `PIO_INT`/`PIO_REAL`/`PIO_DOUBLE` (uppercase); reviewer checked
  both `src/flib/pio_types.F90:246-248` and `src/clib/pio.h:180-183` and
  confirmed identical underlying NetCDF type codes, no semantic change.
- CMake registration copies the existing Fortran `pio_decomp_fillval2`
  entry's own "C3" tier knobs verbatim (`MINNUMPROCS 4`,
  `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`, `MINNUMIOPROCS 2`,
  `MAXNUMIOPROCS ${PIO_TF_C3_MAXNUMIOPROCS}`, `MINSTRIDE 2`,
  `MAXSTRIDE ${PIO_TF_C3_MAXSTRIDE}`) — the same tier
  `pio_decomp_fillval_cxx` already uses.
- Review: task reviewer (opus) found full spec compliance (PASS) after
  tracing all 5 subroutines line-by-line against the actual 645-line
  Fortran source (not just the brief), confirming all 5 Review Focus
  items in the actual code, confirming the `PIOc_freedecomp`
  ordering per function, confirming the CMake knobs are byte-identical to
  the existing Fortran registration, confirming the Fortran source
  untouched, and independently forcing a clean rebuild (zero warnings)
  and re-running `ctest` itself (1/1 passed, 12.84s) rather than trusting
  the implementer's report. Code quality: APPROVED WITH MINOR FINDINGS
  (no Critical/Major) — (A) `nc_wr_1d_mreg_imp_fval` used a fixed
  filename where the Fortran numbers it per-iotype, identical to the
  other 3 numbered functions in this file; root-caused to an incorrect
  claim in the plan's Step 3 (copied from the sibling file's pattern,
  which doesn't apply to this Fortran source); behaviorally benign
  (create+delete within each iotype iteration) but an internal
  inconsistency vs. the file's other 3 numbered functions; (B) roadmap
  not yet updated at review time — expected follow-up, not a plan
  violation; (C) nit, no action — the C3 tier's `MINNUMPROCS=4` means the
  nprocs=1 "rank 0 is also the last rank" case in Review Focus item 3 is
  never exercised by CI, inherited from the verbatim-copied Fortran tier.
- Ruling: finding (A) was a cheap, mechanical, zero-risk one-line fix
  (moved the filename construction inside the iotype loop, numbered via
  `std::to_string(i + 1)`, matching the other 3 functions) — the
  coordinator applied it directly rather than dispatching another
  implementer+reviewer round trip, then independently rebuilt (clean) and
  re-ran `ctest` (1/1 passed, 23.62s) before committing as `e4f6f56639`.
  Finding (B) addressed by this roadmap update. Finding (C) left as-is
  per the reviewer's own no-action recommendation.
- Build/test: `ctest -R pio_decomp_fillval2_cxx` passed at every
  verification point — implementer (1/1, 17.93s), coordinator pre-review
  (1/1, 14.84s), task reviewer post-forced-rebuild (1/1, 12.84s),
  coordinator post-fix (1/1, 23.62s).
- Single-task plan: the task review doubled as the final whole-branch
  review. `tests/general/pio_decomp_fillval2.F90.in` confirmed untouched
  throughout — by the implementer, by the coordinator
  (`git diff --stat 151f4a1b22..8c366aa69d --
  tests/general/pio_decomp_fillval2.F90.in` empty), and by the task
  reviewer independently against the same base commit.

### `pio_decomp_frame_tests.F90.in` (661 lines) -> `tests/functional/pio_decomp_frame_tests.cpp` (~865 lines)

- Plan: `docs/superpowers/plans/2026-10-30-port-pio-decomp-frame-tests-to-cxx.md`,
  2 tasks: Task 1 (non-decomposition vara-based 1d frame test + a
  decomposition-based 2d frame test), Task 2 (a reusable 3D-column-decomp
  helper + a 4d-frame darray test + a 3d/4d-decomposition-reuse test +
  `main()` + CMake registration).
- Architecture: 4 `PIO_TF_AUTO_TEST_SUB` subroutines ported 1:1 —
  `nc_wr_rd_1d_unlim_dim` (no decomposition; per-frame
  `PIOc_put_vara`/`PIOc_get_vara` against an unlimited dim), `nc_wr_rd_2d_unlim_dim`
  (a row-dim decomposition with `PIOc_setframe` selecting an unlimited
  time dim per frame), `nc_write_read_4d_col_decomp` (the existing 3D
  column decomposition from `pio_decomp_tests_3d.cpp` extended with a
  time/frame dimension, `NFRAMES=6`), and `nc_reuse_3d_decomp` (a single
  `wr_iodesc`/`rd_iodesc` pair reused across a 3D var with no frame
  dimension and a 4D var with `NFRAMES=3`, confirming decomposition reuse
  works across variables with different ranks).
- `get_3d_col_decomp_info` copied verbatim from the already-reviewed
  `pio_decomp_tests_3d.cpp:34-89` — confirmed byte-identical via direct
  `diff` both at Task 2 implementation time and again at final review.
- **Real bug #1 (Task 1, found by task review, fixed):** `nc_wr_rd_1d_unlim_dim`'s
  `PIOc_put_vara`/`PIOc_get_vara` calls used a 1-based `start[1] = {f+1}`,
  following the plan's Global Constraint that dims/start/count are
  1-based matching the Fortran — but `PIOc_put_vara`/`PIOc_get_vara`'s
  `start` is 0-based in C (unlike `PIOc_InitDecomp`'s `compdof`, which
  genuinely is 1-based). Confirmed independently against
  `src/flib/spio_put_var.F90.in:225,347,349,477,479`
  (`cstart(i) = INT(start(...) - 1, ...)`). Fixed to `start[1] = {f}`
  (0-based loop variable). This is a project-wide gotcha worth flagging
  forward to any future port that uses the vara API directly: the
  plan-level "1-based, matching Fortran" convention applies to
  `compdof`/`PIOc_InitDecomp`, not to `PIOc_put_vara`/`PIOc_get_vara`.
- **Real bug #2 (Task 2, found by task review, fixed):** the read buffers
  (`rbuf`, `rbuf3d`, `rbuf4d`) were zeroed once at declaration instead of
  at the top of each iotype iteration, unlike the Fortran (F90.in:427,
  624-625) and the sibling `pio_decomp_tests_3d.cpp:262-264`. Since every
  iotype writes/expects identical values, a silent read failure that
  didn't fill the buffer could have compared equal to `exp_val` on
  iotypes 2..N — exactly the per-iotype read bug class this test exists
  to catch. Fixed by adding a per-iotype-iteration zero-fill matching the
  sibling idiom exactly. (Reviewer noted this specific fix is
  unobservable in a pnetcdf-only build since `num_flavors == 1` there,
  but is still correct and will matter in NetCDF-enabled builds.)
- **Port-fidelity restoration (final whole-branch review, applied):**
  `nc_wr_rd_2d_unlim_dim` originally consolidated the Fortran's two
  identical decompositions (`wr_iodesc`/`rd_iodesc`, both built from the
  same `compdof`/`dims`, F90.in:143/145/208-209) into one shared
  `iodesc`, per the Task 1 brief's explicit instruction. The final
  reviewer independently re-derived the Fortran and found the brief's
  premise (that the Fortran "uses the same iodesc") was factually wrong
  about the source; ruled to restore the literal two-iodesc structure
  for full port fidelity (cheap, zero-risk, already-proven pattern used
  by the other two functions in the same file) rather than keep the
  documented deviation.
- CMake registration uses the C2 tier (`MINNUMPROCS 1`,
  `MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}`, `MINNUMIOPROCS 1`,
  `MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}`, `MINSTRIDE 1`,
  `MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}`), confirmed to match the Fortran
  `pio_decomp_frame_tests` registration's own tier
  (`tests/general/CMakeLists.txt:716-727`) field-for-field; the final
  reviewer empirically confirmed `ctest -N` expands the C++ target to
  the identical 10 `np*/nio*/st*` invocations as the Fortran target.
- Review: Task 1 and Task 2 each went through one review round with a
  confirmed bug, a fix round, and a scoped re-review before being marked
  complete (see the two real bugs above). The final whole-branch review
  (opus) independently re-derived every formula/filename/ordering/NFRAMES
  value against the full 662-line Fortran source rather than trusting
  either task's report or the ledger, confirmed single coherent `main()`
  wiring (12 `rfuncs` entries, no dead code from Task 1's
  deliberately-deferred `main()`), ran a REAL rebuild plus
  `-Wall -Wextra -Wshadow` recompile (clean) plus REAL `ctest` (10/10),
  and ran a **mutation test** (redirected one `PIOc_setframe` call to the
  wrong frame and confirmed the test failed with a specific wrong-value
  error) proving per-frame coverage is real rather than vacuous.
  Verdict: APPROVED, with one additional fix round (the two-iodesc
  restoration above plus two trivial consistency nits — a flavor-loop
  variable rename `i`->`fl`, and making `vec_local_sz` `const`) applied
  and re-reviewed clean before closing the plan.
- Build/test: `ctest -R pio_decomp_frame_tests_cxx` passed 10/10 at every
  verification checkpoint across both tasks and all three fix rounds
  (coordinator-run, not implementer-self-reported, at each checkpoint).
- Final commit: `97094f1d87`. `tests/general/pio_decomp_frame_tests.F90.in`
  confirmed untouched at every checkpoint throughout (implementer,
  coordinator, both task reviewers, and the final whole-branch reviewer,
  via repeated independent `git diff --stat` checks).
