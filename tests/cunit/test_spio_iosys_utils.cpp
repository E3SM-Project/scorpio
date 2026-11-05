#include <cstdio>
#include <cstdint>
#include <cassert>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"
#include "spio_iosys_utils.hpp"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#define LOG_RANK0(rank, ...)                      \
            do{                                   \
              if(rank == 0){                      \
                fprintf(stderr, __VA_ARGS__);     \
              }                                   \
            }while(0);

static const int FAIL = -1;

/* Case (a): every rank in comm must receive the identical combined 64-bit
 * value - the whole point of generate_and_bcast_lock_id() is that only
 * "root" draws fresh entropy and everyone else learns it via MPI_Bcast().
 * Checked by splitting the 64-bit value back into its two 32-bit halves (the
 * same halves the implementation itself broadcasts) and MPI_Allreduce()'ing
 * MIN/MAX over each half separately - if any rank disagrees, MIN != MAX for
 * that half. This avoids assuming MPI_UINT64_T works (the implementation
 * itself avoids it for the same reason, see spio_iosys_utils.cpp).
 */
int test_lock_id_identical_on_all_ranks(MPI_Comm comm, int wrank, int wsz, int root)
{
  int ret = PIO_NOERR;

  uint64_t id = SPIO_Util::Iosys_Util::generate_and_bcast_lock_id(comm, root);

  uint32_t hi = static_cast<uint32_t>(id >> 32);
  uint32_t lo = static_cast<uint32_t>(id & 0xFFFFFFFFu);

  uint32_t hi_min = 0, hi_max = 0, lo_min = 0, lo_max = 0;
  int mpierr = MPI_SUCCESS;
  mpierr = MPI_Allreduce(&hi, &hi_min, 1, MPI_UNSIGNED, MPI_MIN, comm);
  assert(mpierr == MPI_SUCCESS);
  mpierr = MPI_Allreduce(&hi, &hi_max, 1, MPI_UNSIGNED, MPI_MAX, comm);
  assert(mpierr == MPI_SUCCESS);
  mpierr = MPI_Allreduce(&lo, &lo_min, 1, MPI_UNSIGNED, MPI_MIN, comm);
  assert(mpierr == MPI_SUCCESS);
  mpierr = MPI_Allreduce(&lo, &lo_max, 1, MPI_UNSIGNED, MPI_MAX, comm);
  assert(mpierr == MPI_SUCCESS);

  if((hi_min != hi_max) || (lo_min != lo_max)){
    LOG_RANK0(wrank, "ERROR: generate_and_bcast_lock_id() returned different "
      "values across ranks in comm (root = %d): hi in [%u, %u], lo in [%u, %u] "
      "(this rank's id = 0x%016llx)\n",
      root, hi_min, hi_max, lo_min, lo_max,
      static_cast<unsigned long long>(id));
    ret = PIO_EINTERNAL;
  }

  return ret;
}

/* Case (b): two back-to-back calls (simulating two distinct iosystems being
 * initialized one after another in the same process) must produce different
 * values - this is the only way to catch a would-be "forgot to actually draw
 * fresh entropy" bug (e.g. hardcoding a value, or reusing a static/once-only
 * draw) without being able to compare against another process's run.
 *
 * NOTE: since generate_and_bcast_lock_id() draws from std::random_device
 * (true entropy, not a seeded/reproducible PRNG, per this task's explicit
 * requirement), a collision across two draws is possible in principle but is
 * vanishingly unlikely (~1 in 2^64) - this assertion is probabilistic, not a
 * mathematical guarantee, and is the best available check for "did this
 * actually call into fresh entropy twice" within a single test process.
 */
int test_lock_id_differs_across_calls(MPI_Comm comm, int wrank, int root)
{
  int ret = PIO_NOERR;

  uint64_t id1 = SPIO_Util::Iosys_Util::generate_and_bcast_lock_id(comm, root);
  uint64_t id2 = SPIO_Util::Iosys_Util::generate_and_bcast_lock_id(comm, root);

  if(id1 == id2){
    LOG_RANK0(wrank, "ERROR: generate_and_bcast_lock_id() returned the same "
      "value (0x%016llx) on two back-to-back calls - expected two independent "
      "std::random_device draws to differ (a collision is possible but "
      "vanishingly unlikely)\n", static_cast<unsigned long long>(id1));
    ret = PIO_EINTERNAL;
  }

  return ret;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);

  ret = test_lock_id_identical_on_all_ranks(comm, wrank, wsz, 0);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_id_identical_on_all_ranks(root=0) FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_id_identical_on_all_ranks(root=0) PASSED\n");
  }

  /* Exercise a non-zero root too (when there are enough ranks), since
   * generate_and_bcast_lock_id() takes an arbitrary root and the real call
   * site always uses root = 0 on union_comm - but the function itself must
   * not assume that. */
  int other_root = (wsz > 1) ? (wsz - 1) : 0;
  ret = test_lock_id_identical_on_all_ranks(comm, wrank, wsz, other_root);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_id_identical_on_all_ranks(root=%d) FAILED, ret = %d\n", other_root, ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_id_identical_on_all_ranks(root=%d) PASSED\n", other_root);
  }

  ret = test_lock_id_differs_across_calls(comm, wrank, 0);
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_lock_id_differs_across_calls() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_lock_id_differs_across_calls() PASSED\n");
  }

  *num_errors += nerrs;
  return nerrs;
}

int main(int argc, char *argv[])
{
  int ret;
  int wrank, wsz;
  int num_errors;
#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLinitialize();
  if(ret != 0){
    fprintf(stderr, "GPTLinitialize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  ret = MPI_Init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Init() FAILED, ret = %d\n", ret);
    return ret;
  }

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
    return ret;
  }
  ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  if(ret != MPI_SUCCESS){
    fprintf(stderr, "MPI_Comm_size() FAILED, ret = %d\n", ret);
    return ret;
  }

  num_errors = 0;
  ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
  if(ret != 0){
    LOG_RANK0(wrank, "Test driver FAILED\n");
  }
  else{
    LOG_RANK0(wrank, "All tests PASSED\n");
  }

  MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLfinalize();
  if(ret != 0){
    fprintf(stderr, "GPTLfinalize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  if(num_errors != 0){
    LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
    return FAIL;
  }
  return 0;
}
