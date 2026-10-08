#include "pio_config.h"

#if PIO_USE_ASYNC_WR_THREAD

#include <iostream>
#include <vector>
#include <string>
#include <cassert>
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

int main(int argc, char *argv[])
{
  int ret;

  ret = MPI_Init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    std::cerr << "MPI_Init() FAILED, ret = " << ret << "\n";
    return ret;
  }

  std::vector<std::pair<std::string, std::function<int(SPIO_TF::Test_framework &)> > > rfuncs = {
    {"test_contig_rearr_sync_async_role_dispatch", test_contig_rearr_sync_async_role_dispatch}
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
