# Port pio_large_file_tests to C++ Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port `tests/general/pio_large_file_tests.F90.in` (433 lines) to a new C++ functional test `tests/functional/pio_large_file_tests.cpp`, templated over `int`/`float`/`double`, guarded by the same `PIO_ENABLE_LARGE_TESTS` CMake option as the Fortran original and the existing `pio_decomp_large_tests.cpp`.

**Architecture:** Three templated (`int`/`float`/`double`, via `Pio_type_map<T>`) test functions, matching the Fortran's three `PIO_TF_AUTO_TEST_SUB` blocks: `nc_wr_rd_large_1d_bc<T>` (currently test-disabled upstream — see Global Constraints), `nc_def_large_1d_bc<T>` (defines large dims/vars across 3 file formats × iotypes, no I/O), and `nc_write_large_1d_fval<T>` (writes/reads a 1M-element array with a user-specified fill value for implicit holes). All three are synchronous (non-async) tests following the `return PIO_EINTERNAL;`-on-failure convention established for `pio_rearr_opts2.cpp`/`pio_decomp_tests2_1d.cpp`, using the shared `tests/util/spio_test_check.hpp` helpers.

**Tech Stack:** C++14, SCORPIO C API (`PIOc_*`), `SPIO_TF::Test_framework`, MPI, existing `tests/util` helpers (`get_iotypes`, `get_iotype_name`, `Pio_type_map<T>`, `check_val`, `check_ret`, `passert`).

**Spec:** `tests/general/pio_large_file_tests.F90.in` (the Fortran source being ported — read-only, must never be modified).

## Global Constraints

- `tests/general/pio_large_file_tests.F90.in` must NEVER be removed, modified, or deprecated — read-only reference.
- C++14 only. 2-space indent, no space before `(` after keywords, opening braces on the same line.
- `std::size_t` for `.size()`-bounded loop counters; plain `int` for loop counters bounded by a fixed-size `const int`.
- Error codes: `PIO_NOERR` / `PIO_EINTERNAL` only; `return PIO_EINTERNAL;` immediately on any check failure (this is a synchronous test — no "continue to next combo" async pattern).
- `const int`/`const std::string`/`const PIO_Offset` for constants, not `#define`.
- File name: `tests/functional/pio_large_file_tests.cpp` (1:1 with `pio_large_file_tests.F90.in`).
- Reuse existing `tests/util` helpers (`get_iotypes`, `get_iotype_name`, `Pio_type_map<T>`, `check_val`, `passert`, `check_ret`) — do not duplicate their logic.
- GPTL guard immediately after `#include "pio.h"`, before `spio_test_*.hpp` includes, exactly as in existing functional tests.
- **`nc_wr_rd_large_1d_bc` is currently disabled in the Fortran source**: its body is `PIO_TF_LOG(0,*) "Skipping the write test..."` followed immediately by an unconditional `RETURN`, with a comment `! REMOVE THIS RETURN STMT TO RUN THE TEST` — everything below that `RETURN` (including the call to `get_1d_bc_info`) is unreachable dead code in the Fortran today. Port ONLY the behavior that currently executes: log the message and `return PIO_NOERR;` immediately. Do NOT port the unreachable write/read logic below it, and do NOT port the Fortran `get_1d_bc_info` subroutine at all — its only caller is this disabled function, so it would be dead/unused code in the C++ port (triggering unused-function warnings and violating the project's no-dead-code convention). This is a deliberate, documented scope decision, not an oversight — call it out explicitly in review.
- `init_file_fmts`'s Fortran source declares an unused local parameter `ONE_G = INT(2,pio_offset_kind)**30` that is never referenced anywhere in the file. Omit it from the C++ port (do not declare an unused constant) — this changes no behavior.
- `nc_def_large_1d_bc`'s Fortran source never calls `PIO_deletefile` (the call is commented out) — do NOT call `PIOc_deletefile` in the C++ port either; each iteration's `PIOc_createfile` uses `PIO_CLOBBER` and overwrites the previous file, matching the Fortran exactly.
- `nc_write_large_1d_fval`'s Fortran source DOES call `PIO_deletefile` (uncommented) after each iotype's write/read/close cycle — the C++ port must call `PIOc_deletefile` there. Do not confuse the two functions' differing delete-file behavior.
- The two live functions use different hardcoded filenames — copy verbatim, do not unify them: `nc_def_large_1d_bc` uses `"test_pio_large_file_tests.testfile"`; `nc_write_large_1d_fval` uses `"test_pio_decomp_fillval_tests.testfile"`.
- This whole file is a "large" test: wrap both the new `add_spio_executable`/`add_pio_test` CMake block in `if (PIO_ENABLE_LARGE_TESTS) ... endif ()`, matching the existing `pio_decomp_large_tests_cxx` block already present in `tests/functional/CMakeLists.txt` and the Fortran's own `if (PIO_ENABLE_LARGE_TESTS)` guard in `tests/general/CMakeLists.txt`.
- Git commit messages end with `Co-Authored-By: Claude Code <noreply@anthropic.com>`.

## Review Focus

- **The disabled-test scope decision is followed consistently:** `nc_wr_rd_large_1d_bc<T>` must contain ONLY a log call and `return PIO_NOERR;` — verify no decomposition/file-I/O logic or `get_1d_bc_info` helper was accidentally ported alongside it (that would be dead code, since nothing can reach it before the early return).
- **`init_file_fmts` numeric fidelity:** `max_rec_sz`/`max_large_rec_sz` for all three formats (CDF1 `TWO_G - ONE_K` / `ONE_K/2 - 1`; CDF2 `TWO_G - 1` / `FOUR_G - 4`; CDF5 `TWO_G - 1` / `FOUR_G + 1`) must be transcribed as exact 64-bit (`PIO_Offset`) arithmetic — several of these values (e.g. `FOUR_G + 1` = 4294967297) exceed 32-bit `int` range, so an implementer who narrows to `int` anywhere in this chain would silently truncate/wrap a constant that no small-process-count test run would ever expose as a visible failure.
- **`pio_flag` bitwise-OR correctness:** CDF2 uses `PIO_CLOBBER | PIO_64BIT_OFFSET` and CDF5 uses `PIO_CLOBBER | PIO_64BIT_DATA` (Fortran `IOR(...)`) — verify `|` is used, not accidentally `+` or a single flag.
- **Two functions' divergent `PIOc_deletefile` behavior is preserved:** `nc_def_large_1d_bc` must NOT delete its file between iterations (relies on `PIO_CLOBBER` to overwrite); `nc_write_large_1d_fval` MUST delete its file after each iotype — a reviewer should specifically diff these two functions' end-of-loop-body code against each other, since copy-pasting one function's loop tail into the other is an easy, silent mistake.
- **`nc_write_large_1d_fval`'s implicit-hole expected-value split:** elements `0..wcompdof_sz-1` of `exp_val` must equal the just-written `wbuf` values; elements `wcompdof_sz..VEC_LOCAL_SZ-1` must equal `BUF_FILLVAL` (not `0` or uninitialized) — this is the one function in this port that actually exercises a fill-value code path, and an off-by-one in the `wcompdof_sz` boundary would make the test spuriously pass for rank 0 (`wcompdof_sz == 1`) while being wrong for every other rank.

---

### Task 1: Port pio_large_file_tests.F90.in to tests/functional/pio_large_file_tests.cpp

**Files:**
- Create: `tests/functional/pio_large_file_tests.cpp`
- Modify: `tests/functional/CMakeLists.txt` (append a new `if (PIO_ENABLE_LARGE_TESTS) ... endif ()` block, after the existing `pio_decomp_large_tests_cxx` block)

**Interfaces:**
- Consumes (from `tests/util`, already existing — do not redefine): `get_iotypes`, `get_iotype_name(int, char*)`, `Pio_type_map<T>`, `check_val<T>`, `passert`, `check_ret`, `SPIO_TF::Test_framework` (same lifecycle API as `pio_decomp_large_tests.cpp`'s `main()`: `tf.init(argc, argv)`, `tf.init_iosystem(PIO_REARR_BOX)`, `tf.run()`, `tf.finalize_iosystem()`, `tf.finalize()`).
- Produces: a standalone executable `pio_large_file_tests_cxx`, built and registered only when `PIO_ENABLE_LARGE_TESTS` is ON; no other file depends on this task's output.

- [ ] **Step 1: Write file header, includes, and the `Pio_file_fmt_info` struct + `init_file_fmts()`**

  Follow `tests/functional/pio_decomp_tests2_1d.cpp`'s include order (GPTL guard right after `#include "pio.h"`, before `spio_test_framework.hpp`/`spio_test_check.hpp`/`spio_test_common.hpp`).

  ```cpp
  struct Pio_file_fmt_info {
    std::string name;
    int pio_flag;
    PIO_Offset max_rec_sz;
    PIO_Offset max_large_rec_sz;
  };

  std::vector<Pio_file_fmt_info> init_file_fmts()
  ```

  Body: declare `const PIO_Offset ONE_K = static_cast<PIO_Offset>(1) << 10;`, `const PIO_Offset TWO_G = static_cast<PIO_Offset>(1) << 31;`, `const PIO_Offset FOUR_G = static_cast<PIO_Offset>(1) << 32;` (do NOT declare `ONE_G` — unused, see Global Constraints). Return a 3-element `std::vector<Pio_file_fmt_info>`:
  - `{"CDF1", PIO_CLOBBER, TWO_G - ONE_K, ONE_K/2 - 1}`
  - `{"CDF2", PIO_CLOBBER | PIO_64BIT_OFFSET, TWO_G - 1, FOUR_G - 4}`
  - `{"CDF5", PIO_CLOBBER | PIO_64BIT_DATA, TWO_G - 1, FOUR_G + 1}`

- [ ] **Step 2: Implement `nc_wr_rd_large_1d_bc<T>` (disabled-test stub)**

  Signature: `template<typename T> int nc_wr_rd_large_1d_bc(SPIO_TF::Test_framework &tf)`

  Body is exactly:
  ```cpp
  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "Skipping the write test...\n");
  return PIO_NOERR;
  ```
  Add a one-line comment above the function citing that this mirrors the Fortran source's own disabling early `RETURN` (see Global Constraints) — do not implement anything else in this function.

- [ ] **Step 3: Implement `nc_def_large_1d_bc<T>`**

  Signature: `template<typename T> int nc_def_large_1d_bc(SPIO_TF::Test_framework &tf)`

  - `std::vector<Pio_file_fmt_info> pio_file_fmts = init_file_fmts();`
  - Discover iotypes via `get_iotypes`/`SPIO_TEST_NUM_FLAVORS` (same idiom as `pio_decomp_tests2_1d.cpp`).
  - `const std::string filename = "test_pio_large_file_tests.testfile";`
  - `const int DUMMY_DIM_SZ = 10;`
  - For each file format (outer loop) × each iotype (inner loop):
    - Log `"Testing : PIO file format : " + fmt.name + " : PIO file format : " + iotype_name` (matches the Fortran's literal log text, including its duplicated "PIO file format" wording — copy verbatim, do not "fix" it).
    - `PIO_Offset nbytes = static_cast<PIO_Offset>(sizeof(T));`
    - `int dim = static_cast<int>(fmt.max_rec_sz / nbytes);`
    - `PIO_Offset large_dim = fmt.max_large_rec_sz / nbytes;`
    - `int last_var_dim_sz = DUMMY_DIM_SZ;`
    - `PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), fmt.pio_flag)`, checked via `check_ret` → `return PIO_EINTERNAL;` on failure.
    - `PIOc_def_dim(ncid, "PIO_TF_test_dim", (PIO_Offset)dim, &pio_dim)`, checked.
    - `PIOc_def_dim(ncid, "PIO_TF_test_large_dim", large_dim, &pio_large_dim)`, checked.
    - `PIOc_def_dim(ncid, "PIO_TF_test_last_var_dim", (PIO_Offset)last_var_dim_sz, &pio_last_var_dim)`, checked.
    - `PIOc_def_var(ncid, "PIO_TF_test_var", Pio_type_map<T>::value, 1, &pio_dim, &pio_var)`, checked.
    - `PIOc_def_var(ncid, "PIO_TF_test_large_var", Pio_type_map<T>::value, 1, &pio_large_dim, &pio_large_var)`, checked.
    - `PIOc_def_var(ncid, "PIO_TF_test_last_var", Pio_type_map<T>::value, 1, &pio_last_var_dim, &pio_last_var)`, checked.
    - `PIOc_enddef(ncid)`, checked.
    - `PIOc_closefile(ncid)`.
    - Do NOT call `PIOc_deletefile` (see Global Constraints).
  - `return PIO_NOERR;` after both loops complete.

- [ ] **Step 4: Implement `nc_write_large_1d_fval<T>`**

  Signature: `template<typename T> int nc_write_large_1d_fval(SPIO_TF::Test_framework &tf)`

  - `const int VEC_LOCAL_SZ = 1000000;`
  - `const T BUF_FILLVAL = static_cast<T>(-2);`
  - `int wcompdof_sz = std::min(tf.get_comm_rank() + 1, VEC_LOCAL_SZ);`
  - `std::vector<PIO_Offset> wcompdof(wcompdof_sz);` filled by `wcompdof[i] = static_cast<PIO_Offset>(VEC_LOCAL_SZ) * tf.get_comm_rank() + (i + 1);` for `i = 0 .. wcompdof_sz-1`.
  - `std::vector<PIO_Offset> rcompdof(VEC_LOCAL_SZ);` filled by the same formula over the full range `i = 0 .. VEC_LOCAL_SZ-1` (this is the "read everything, including fillvalue holes" decomposition).
  - `std::vector<T> wbuf(wcompdof_sz);` and `std::vector<T> exp_val(VEC_LOCAL_SZ);`: for `i = 0 .. wcompdof_sz-1`, `wbuf[i] = static_cast<T>(wcompdof[i]); exp_val[i] = wbuf[i];`; for `i = wcompdof_sz .. VEC_LOCAL_SZ-1`, `exp_val[i] = BUF_FILLVAL;`.
  - `std::vector<T> rbuf(VEC_LOCAL_SZ, static_cast<T>(0));`
  - `int dim = VEC_LOCAL_SZ * tf.get_comm_size();`
  - `PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim, wcompdof_sz, wcompdof.data(), &wioid, NULL, NULL, NULL)`, checked via `check_ret` → `return PIO_EINTERNAL;` on failure.
  - `PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim, VEC_LOCAL_SZ, rcompdof.data(), &rioid, NULL, NULL, NULL)`, checked the same way.
  - Discover iotypes via `get_iotypes`; `const std::string filename = "test_pio_decomp_fillval_tests.testfile";` (note: different from Step 3's filename).
  - For each iotype:
    - Log `"Testing : PIO_TF_DATA_TYPE : " + iotype_name`.
    - `PIOc_createfile(..., PIO_CLOBBER)`, checked.
    - `PIOc_def_dim(ncid, "PIO_TF_test_dim", (PIO_Offset)dim, &pio_dim)`, checked.
    - `PIOc_def_var(ncid, "PIO_TF_test_var", Pio_type_map<T>::value, 1, &pio_dim, &pio_var)`, checked.
    - `PIOc_enddef(ncid)`, checked.
    - `PIOc_write_darray(ncid, pio_var, wioid, wcompdof_sz, wbuf.data(), &BUF_FILLVAL)`, checked.
    - `#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC`: close + reopen (`PIO_WRITE`) + `PIOc_inq_varid`; `#else`: `PIOc_sync(ncid)`, checked via `check_ret`; `#endif` (same structure as `pio_rearr_opts2.cpp`'s sync branch).
    - Reset `rbuf` to all zero.
    - `PIOc_read_darray(ncid, pio_var, rioid, VEC_LOCAL_SZ, rbuf.data())`, checked.
    - `check_val(tf, rbuf, exp_val, "Got wrong val")` → `return PIO_EINTERNAL;` on mismatch.
    - `PIOc_closefile(ncid)`.
    - `PIOc_deletefile(tf.get_iosysid(), filename.c_str())` (note: DOES delete here, unlike Step 3).
  - `PIOc_freedecomp(tf.get_iosysid(), rioid);` and `PIOc_freedecomp(tf.get_iosysid(), wioid);`.
  - `return PIO_NOERR;`.

- [ ] **Step 5: Implement `main()`**

  Follow `tests/functional/pio_decomp_large_tests.cpp`'s `main()` structure exactly: `MPI_Init` (with the one permitted `std::cerr`-on-failure line), build the `rfuncs` vector registering all 9 instantiations by name (`"nc_wr_rd_large_1d_bc_int"`/`"_float"`/`"_double"`, `"nc_def_large_1d_bc_int"`/`"_float"`/`"_double"`, `"nc_write_large_1d_fval_int"`/`"_float"`/`"_double"`, each mapped to the corresponding `<int>`/`<float>`/`<double>` instantiation — same naming idiom as `pio_decomp_tests2_1d.cpp`'s `rfuncs`), construct `SPIO_TF::Test_framework tf("pio_large_file_tests_cxx", MPI_COMM_WORLD, rfuncs)`, `tf.init(argc, argv)`, `if(tf.init_iosystem(PIO_REARR_BOX) == PIO_NOERR){ nerrs += tf.run(); tf.finalize_iosystem(); } else { nerrs++; }`, `tf.finalize()`, `MPI_Finalize()`, `return nerrs;`.

- [ ] **Step 6: Build the test**

  Run: `cd build && cmake -DPIO_ENABLE_LARGE_TESTS=ON . && make pio_large_file_tests_cxx`
  Expected: builds with no errors or warnings. (If the existing `build/` cache was not configured with `PIO_ENABLE_LARGE_TESTS=ON`, re-running `cmake` with that flag in the existing build directory is sufficient — no need to reconfigure from scratch.)

- [ ] **Step 7: Append the CMake test block to `tests/functional/CMakeLists.txt`**

  Append immediately after the existing `pio_decomp_large_tests_cxx` block's `endif ()` (inside a new `if (PIO_ENABLE_LARGE_TESTS) ... endif ()` block), copying the knobs verbatim from `tests/general/CMakeLists.txt:271-282`:

  ```cmake
  if (PIO_ENABLE_LARGE_TESTS)
    add_spio_executable(pio_large_file_tests_cxx TRUE "" pio_large_file_tests.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

    # Costly test only test with 2 I/O procs
    add_pio_test(pio_large_file_tests_cxx
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_large_file_tests_cxx
      MINNUMPROCS 2
      MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
      MINNUMIOPROCS 2
      MAXNUMIOPROCS 2
      MINSTRIDE 1
      MAXSTRIDE 1
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endif ()
  ```

- [ ] **Step 8: Rebuild and run the test**

  Run: `cd build && make pio_large_file_tests_cxx && ctest -R pio_large_file_tests_cxx --output-on-failure`
  Expected: all invocations PASS, 0 failures.

- [ ] **Step 9: Verify the Fortran source was not touched**

  Run: `git status --porcelain tests/general/pio_large_file_tests.F90.in`
  Expected: no output (file unchanged).

- [ ] **Step 10: Commit**

  ```bash
  git add tests/functional/pio_large_file_tests.cpp tests/functional/CMakeLists.txt
  git commit -m "$(cat <<'EOF'
  test: port pio_large_file_tests to tests/functional/pio_large_file_tests.cpp

  Co-Authored-By: Claude Code <noreply@anthropic.com>
  EOF
  )"
  ```
