#include "pio_config.h"

#if PIO_USE_ASYNC_WR_THREAD

#include <iostream>
#include <vector>
#include <string>
#include <cassert>
#include <cstring>
#include <numeric>
#include <sstream>
#include <functional>
#include <utility>

#include "pio.h"
#include "pio_tests.h"
#include "pio_rearr_contig.hpp"
#include "spio_test_framework.hpp"
#include "spio_iosys_utils.hpp"

static const int FAIL = -1;

/* Tests for Contig_rearr's sync/async role dispatch helpers
 * (role_io_comm()/role_is_io_proc()/role_io_comm_sz()/role_io_comm_rank())
 *
 * Two MPI communicators are created for the sync and async cases:
 *  - sync_io_comm : a dup of MPI_COMM_WORLD (size == wsz for every rank)
 *  - async_io_comm: MPI_COMM_WORLD split into two comms: {rank 0} (size 1) and
 *                   {ranks 1..wsz-1} (size wsz-1).
 */

iosystem_desc_t *get_sync_async_iosystem(SPIO_TF::Test_framework &tf, MPI_Comm comm,
    int wrank, int wsz, MPI_Comm sync_io_comm, MPI_Comm async_io_comm)
{
  iosystem_desc_t *ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t));
  if(!ios){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "Unable to allocate memory for I/O system");
    return NULL;
  }

  static int iosysid = 1;
  ios->iosysid = iosysid++;
  ios->union_comm = comm;
  ios->num_uniontasks = wsz;
  ios->union_rank = wrank;
  /* Every proc is a compute proc */
  ios->comp_comm = comm;
  ios->num_comptasks = wsz;
  ios->comp_rank = wrank;
  ios->compproc = true;

  /* Both sync_io_comm and async_io_comm are really owned/created by this test, so using the
   * default IO_comm_info::owns_resources = true so that it gets freed when ios->io_comm_info
   * is deleted
   */
  ios->io_comm_info = new SPIO_Util::Iosys_Util::IO_comm_info(
      sync_io_comm, wrank, wsz, MPI_GROUP_NULL, std::vector<int>(),
      async_io_comm, wrank, wsz, MPI_GROUP_NULL, std::vector<int>());

  return ios;
}

void free_sync_async_iosystem(iosystem_desc_t *ios)
{
  if(!ios){
    return;
  }

  delete(ios->io_comm_info);
  free(ios);
}

/* Compute the expected get_rearrange_buf_sz()
 * (mirrors the formula in Contig_rearr::get_rearrange_buf_sz()/set_rearr_comm_iochunk_sz())
 */
static PIO_Offset expect_rearrange_buf_sz(PIO_Offset gdecomp_sz, int comm_sz, int comm_rank)
{
  PIO_Offset iochunk = gdecomp_sz / comm_sz;
  if(comm_rank != (comm_sz - 1)){
    return iochunk;
  }
  else{
    return gdecomp_sz - comm_rank * iochunk;
  }
}

int test_contig_rearr_sync_async_role_dispatch(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();

  int ret = PIO_NOERR;

  assert(wsz >= 2);

  MPI_Comm sync_io_comm = MPI_COMM_NULL, async_io_comm = MPI_COMM_NULL;
  ret = MPI_Comm_dup(comm, &sync_io_comm);
  if(ret != MPI_SUCCESS){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to dup comm for sync I/O comm");
    return PIO_EINTERNAL;
  }

  /* async_io_comm: Two comms : {rank 0} (size 1) & {ranks 1..wsz-1} (size wsz - 1) */
  int async_color = (wrank == 0) ? 0 : 1;
  ret = MPI_Comm_split(comm, async_color, wrank, &async_io_comm);
  if(ret != MPI_SUCCESS){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to split comm for async I/O comm");
    MPI_Comm_free(&sync_io_comm);
    return PIO_EINTERNAL;
  }

  int real_async_sz = -1, real_async_rank = -1;
  MPI_Comm_size(async_io_comm, &real_async_sz);
  MPI_Comm_rank(async_io_comm, &real_async_rank);

  iosystem_desc_t *ios = get_sync_async_iosystem(tf, comm, wrank, wsz, sync_io_comm, async_io_comm);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to get I/O system");
    MPI_Comm_free(&sync_io_comm);
    MPI_Comm_free(&async_io_comm);
    return PIO_EINTERNAL;
  }

  try{
    const int LOCAL_COMPMAP_SZ = wsz - 1;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);

    /* Default role (targets_async_comm = false) : targets the sync I/O comm */
    SPIO::DataRearr::Contig_rearr sync_rearr(ios);
    ret = sync_rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing sync-role contig rearranger failed");
      free_sync_async_iosystem(ios);
      return PIO_EINTERNAL;
    }

    /* Async role (targets_async_comm = true) : targets the async I/O comm */
    SPIO::DataRearr::Contig_rearr async_rearr(ios, true);
    ret = async_rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing async-role contig rearranger failed");
      sync_rearr.finalize();
      free_sync_async_iosystem(ios);
      return PIO_EINTERNAL;
    }

    PIO_Offset gdecomp_sz = static_cast<PIO_Offset>(gdimlen);
    PIO_Offset exp_sync_buf_sz = expect_rearrange_buf_sz(gdecomp_sz, wsz, wrank);
    PIO_Offset exp_async_buf_sz = expect_rearrange_buf_sz(gdecomp_sz, real_async_sz, real_async_rank);

    std::size_t sync_buf_sz = sync_rearr.get_rearrange_buf_sz();
    std::size_t async_buf_sz = async_rearr.get_rearrange_buf_sz();

    bool test_success = true;
    if(sync_buf_sz != static_cast<std::size_t>(exp_sync_buf_sz)){
      std::ostringstream oss;
      oss << "ERROR: sync-role get_rearrange_buf_sz() = " << sync_buf_sz
          << " (expected " << exp_sync_buf_sz << ", sync comm sz = " << wsz << ")\n";
      logger.log(SPIO_Util::Logger::Log_level::INFO, oss.str());
      test_success = false;
    }

    if(async_buf_sz != static_cast<std::size_t>(exp_async_buf_sz)){
      std::ostringstream oss;
      oss << "ERROR: async-role get_rearrange_buf_sz() = " << async_buf_sz
          << " (expected " << exp_async_buf_sz << ", async comm sz = " << real_async_sz << ")\n";
      logger.log(SPIO_Util::Logger::Log_level::INFO, oss.str());
      test_success = false;
    }

    /* The sync and async roles is being tested on different, differently-sized, communicators */
    if(wsz == real_async_sz){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Test setup error - sync and async comms unexpectedly have the same size\n");
      test_success = false;
    }

    ret = sync_rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing sync-role contig rearranger failed");
      test_success = false;
    }

    ret = async_rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing async-role contig rearranger failed");
      test_success = false;
    }

    if(!test_success){
      free_sync_async_iosystem(ios);
      return PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Testing contig rearranger sync/async role dispatch failed");
    free_sync_async_iosystem(ios);
    return PIO_EINTERNAL;
  }

  free_sync_async_iosystem(ios);
  return PIO_NOERR;
}

/* Tests that rearrange_comp2io() (the fused, original entry point) and
 * aggregate_comp2io() -> rearrange_aggregated_to_io() (the split phases
 * added for PIO_USE_ASYNC_WR_REARR) produce byte-identical results, for a
 * single-variable and a multi-variable write, on an imbalanced topology
 * (the "async" role setup below has exactly one I/O/agg-root rank and
 * wsz-1 compute ranks, matching the real disjoint-topology shape this
 * split exists for).
 */
static int run_aggregate_rearrange_equivalence_case(SPIO_TF::Test_framework &tf,
    SPIO::DataRearr::Contig_rearr &rearr, int nvars, PIO_Offset lcompmap_sz, int wrank)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;

  /* Rank- and var-distinguishable input: sbuf holds nvars contiguous blocks,
   * each lcompmap_sz elements, matching the layout PIOc_write_darray_multi_
   * impl() passes (pio_darray.cpp:349's sbuf_sz = arraylen * nvars * mpitype_size). */
  std::vector<double> sbuf(lcompmap_sz * nvars);
  for(int v = 0; v < nvars; v++){
    for(PIO_Offset i = 0; i < lcompmap_sz; i++){
      sbuf[v * lcompmap_sz + i] = 1000.0 * v + 100.0 * wrank + static_cast<double>(i);
    }
  }
  std::size_t sbuf_sz = sbuf.size() * sizeof(double);

  std::size_t rlen = rearr.get_rearrange_buf_sz();
  std::size_t rbuf_sz = rlen * nvars * sizeof(double);
  /* Same sentinel for both buffers: this test checks fused-vs-split
   * EQUIVALENCE, not absolute correctness of what CONTIG writes (that is
   * covered elsewhere, e.g. test_darray_hdf5_contig) - using different
   * sentinels would flag an untouched position as a "difference" even
   * when both paths behave identically. */
  const double UNWRITTEN_SENTINEL = -999.0;
  std::vector<double> rbuf_fused(rlen * nvars, UNWRITTEN_SENTINEL);
  std::vector<double> rbuf_split(rlen * nvars, UNWRITTEN_SENTINEL);

  /* (1) The fused, original call. */
  ret = rearr.rearrange_comp2io(sbuf.data(), sbuf_sz, rbuf_fused.data(), rbuf_sz, nvars);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "rearrange_comp2io() (fused) failed");
    return PIO_EINTERNAL;
  }

  /* (2) The split call. */
  void *agg_buf = NULL;
  std::size_t agg_buf_sz = 0;
  ret = rearr.aggregate_comp2io(sbuf.data(), sbuf_sz, agg_buf, agg_buf_sz, nvars);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "aggregate_comp2io() failed");
    return PIO_EINTERNAL;
  }

  bool test_success = true;

  if((agg_buf != NULL) != rearr.is_agg_root()){
    std::ostringstream oss;
    oss << "ERROR: (agg_buf != NULL) = " << (agg_buf != NULL)
        << " does not match is_agg_root() = " << rearr.is_agg_root() << "\n";
    logger.log(SPIO_Util::Logger::Log_level::INFO, oss.str());
    test_success = false;
  }

  /* agg_buf_sz must be identical on every rank (the fused implementation
   * always computed it the same way regardless of is_agg_root_). */
  unsigned long long local_sz = static_cast<unsigned long long>(agg_buf_sz);
  unsigned long long min_sz = 0, max_sz = 0;
  MPI_Allreduce(&local_sz, &min_sz, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, tf.get_comm());
  MPI_Allreduce(&local_sz, &max_sz, 1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, tf.get_comm());
  if(min_sz != max_sz){
    std::ostringstream oss;
    oss << "ERROR: agg_buf_sz is not identical across ranks (min=" << min_sz
        << ", max=" << max_sz << ")\n";
    logger.log(SPIO_Util::Logger::Log_level::INFO, oss.str());
    test_success = false;
  }

  ret = rearr.rearrange_aggregated_to_io(agg_buf, agg_buf_sz, rbuf_split.data(), rbuf_sz, nvars);
  if(agg_buf){ free(agg_buf); }
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "rearrange_aggregated_to_io() failed");
    return PIO_EINTERNAL;
  }

  if(std::memcmp(rbuf_fused.data(), rbuf_split.data(), rbuf_sz) != 0){
    std::ostringstream oss;
    oss << "ERROR: rearrange_comp2io() (fused) and aggregate_comp2io()+"
        << "rearrange_aggregated_to_io() (split) produced different data (nvars=" << nvars
        << ", rank=" << wrank << ")\n";
    logger.log(SPIO_Util::Logger::Log_level::INFO, oss.str());
    test_success = false;
  }

  return test_success ? PIO_NOERR : PIO_EINTERNAL;
}

int test_contig_rearr_aggregate_rearrange_equivalence(SPIO_TF::Test_framework &tf)
{
  MPI_Comm comm = tf.get_comm();
  int wrank = tf.get_comm_rank();
  int wsz = tf.get_comm_size();
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();

  int ret = PIO_NOERR;

  assert(wsz >= 2);

  MPI_Comm sync_io_comm = MPI_COMM_NULL, async_io_comm = MPI_COMM_NULL;
  ret = MPI_Comm_dup(comm, &sync_io_comm);
  if(ret != MPI_SUCCESS){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to dup comm for sync I/O comm");
    return PIO_EINTERNAL;
  }

  /* One I/O/agg-root rank (rank 0), wsz-1 compute ranks - the imbalanced
   * shape that exercises is_agg_root() == false. */
  int async_color = (wrank == 0) ? 0 : 1;
  ret = MPI_Comm_split(comm, async_color, wrank, &async_io_comm);
  if(ret != MPI_SUCCESS){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to split comm for async I/O comm");
    MPI_Comm_free(&sync_io_comm);
    return PIO_EINTERNAL;
  }

  iosystem_desc_t *ios = get_sync_async_iosystem(tf, comm, wrank, wsz, sync_io_comm, async_io_comm);
  if(!ios){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Unable to get I/O system");
    MPI_Comm_free(&sync_io_comm);
    MPI_Comm_free(&async_io_comm);
    return PIO_EINTERNAL;
  }

  int test_ret = PIO_NOERR;
  try{
    const int LOCAL_COMPMAP_SZ = wsz - 1;
    int gdimlen = LOCAL_COMPMAP_SZ * wsz;
    std::vector<PIO_Offset> compmap(LOCAL_COMPMAP_SZ);
    std::iota(compmap.begin(), compmap.end(), wrank * LOCAL_COMPMAP_SZ);

    SPIO::DataRearr::Contig_rearr rearr(ios, true);
    ret = rearr.init(PIO_DOUBLE, compmap.data(), compmap.size(), &gdimlen, 1, NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Initializing contig rearranger failed");
      free_sync_async_iosystem(ios);
      return PIO_EINTERNAL;
    }

    for(int nvars : {1, 3}){
      int case_ret = run_aggregate_rearrange_equivalence_case(tf, rearr, nvars, LOCAL_COMPMAP_SZ, wrank);
      if(case_ret != PIO_NOERR){
        test_ret = case_ret;
      }
    }

    ret = rearr.finalize();
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO, "Finalizing contig rearranger failed");
      test_ret = PIO_EINTERNAL;
    }
  }
  catch(...){
    logger.log(SPIO_Util::Logger::Log_level::INFO, "Testing aggregate/rearrange equivalence failed");
    free_sync_async_iosystem(ios);
    return PIO_EINTERNAL;
  }

  free_sync_async_iosystem(ios);
  return test_ret;
}

int main(int argc, char *argv[])
{
  int ret;

  ret = pio_test_mpi_init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    std::cerr << "MPI_Init() FAILED, ret = " << ret << "\n";
    return ret;
  }

  std::vector<std::pair<std::string, std::function<int(SPIO_TF::Test_framework &)> > > rfuncs = {
    {"test_contig_rearr_sync_async_role_dispatch", test_contig_rearr_sync_async_role_dispatch},
    {"test_contig_rearr_aggregate_rearrange_equivalence", test_contig_rearr_aggregate_rearrange_equivalence}
  };

  SPIO_TF::Test_framework tf("test_spio_rearr_contig_async", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);
  ret = tf.run();
  tf.finalize();

  MPI_Finalize();

  return ret;
}

#else /* PIO_USE_ASYNC_WR_THREAD */

/* Contig_rearr sync/async role dispatch is only used/relevant when async I/O
 * (PIO_USE_ASYNC_WR_THREAD) is enabled - nothing to test otherwise.
 */
int main(int argc, char *argv[])
{
  return 0;
}

#endif /* PIO_USE_ASYNC_WR_THREAD */
