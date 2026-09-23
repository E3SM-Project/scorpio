/** @file
 * Utils related to I/O System
 */
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_iosys_utils.hpp"
#include <vector>
#include <algorithm>
#include <exception>

SPIO_Util::Iosys_Util::IO_comm_info::IO_comm_info(MPI_Comm union_comm,
  MPI_Group union_group,
  int sync_io_root, int sync_io_stride, int sync_io_nprocs):
    is_sync_io_proc_(false), sync_io_master_(MPI_PROC_NULL), sync_io_root_(sync_io_root),
    is_async_io_proc_(false), async_io_master_(MPI_PROC_NULL), async_io_root_(-1),
    needs_sync_btw_sync_and_async_ops_(false)
{
  int ret = MPI_SUCCESS;
  assert((union_comm != MPI_COMM_NULL) &&
    (sync_io_root >= 0) && (sync_io_stride > 0) && (sync_io_nprocs > 0));

  /* Make sure that we have enough procs in union_comm that can be I/O procs */

  /* Determine the I/O processes in the union comm
   * io_ranks[] => (io_root + io_stride * i) % union_comm_sz
   */
  int union_comm_sz = -1, union_comm_rank = -1;
  ret = MPI_Comm_size(union_comm, &union_comm_sz); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_rank(union_comm, &union_comm_rank); assert(ret == MPI_SUCCESS);

  assert((union_comm_sz > 0) && (union_comm_rank >= 0));

  std::vector<int> tmp_sync_io_ranks;
  /* Set to true for union ranks that is an I/O rank - either sync or async I/O rank */
  std::vector<bool> union_rank_is_io_rank(union_comm_sz, false);
  tmp_sync_io_ranks.reserve(sync_io_nprocs);
  std::generate_n(std::back_inserter(tmp_sync_io_ranks), sync_io_nprocs,
    [sync_io_root, sync_io_stride,
      union_comm_rank, union_comm_sz, &union_rank_is_io_rank,
      sync_io_nprocs, this, i=0]() mutable{
        int io_proc = ((sync_io_root + i++ * sync_io_stride) % union_comm_sz);
        if(union_comm_rank == io_proc) { is_sync_io_proc_ = true; }
        if(union_rank_is_io_rank[io_proc]){
          /* The union rank is already an I/O proc */
          std::string err_msg = std::string("Creating I/O comm failed.") +
            std::string("An I/O process (") + std::to_string(io_proc) + std::string(") was selected multiple times.") +
            std::string("io root = ") + std::to_string(sync_io_root) +
            std::string(", io stride = ") + std::to_string(sync_io_stride) +
            std::string(", number of io procs = ") + std::to_string(sync_io_nprocs);
          throw std::runtime_error(err_msg.c_str());
        }
        union_rank_is_io_rank[io_proc] = true;
        return io_proc;
    });

  assert(!tmp_sync_io_ranks.empty());
  sync_io_master_ = (union_comm_rank == sync_io_root) ? MPI_ROOT : MPI_PROC_NULL;

  /* FIXME: Explore if we can use MPI_Group_range_incl() instead */
  /* Create I/O process group from union group */
  MPI_Group sync_io_group = MPI_GROUP_NULL;
  ret = MPI_Group_incl(union_group, sync_io_nprocs, tmp_sync_io_ranks.data(), &sync_io_group);
  if(ret != MPI_SUCCESS){
    throw std::runtime_error("Creating MPI process group for I/O processes failed");
  }

  /* Create I/O comm */
  MPI_Comm sync_io_comm = MPI_COMM_NULL;
  ret = MPI_Comm_create(union_comm, sync_io_group, &sync_io_comm);
  if(ret != MPI_SUCCESS){
    throw std::runtime_error("Creating MPI communicator for I/O processes failed");
  }

  int sync_io_rank = -1;
  if(is_sync_io_proc_){
    ret = MPI_Comm_rank(sync_io_comm, &sync_io_rank); assert(ret == MPI_SUCCESS);
  }

#if PIO_USE_ASYNC_WR_THREAD
  /* Set up the async communicator. The async communicator is used for all async ops
   * 1) First try to create an async communicator with the same size but disjoint set
   *    of processes in the sync communicator. If the size of the async communicator
   *    is smaller than the sync communicator warn the user.
   * 2) If there are no processes disjoint from the set of I/O processes, e.g. all
   *    processes in the union communicator are I/O processes, then warn the user
   *    and enable "Synchronize sync tasks with async tasks" flag
   */

  bool dup_sync_comm = (sync_io_nprocs == union_comm_sz) ? true : false;
  MPI_Comm async_io_comm = MPI_COMM_NULL;
  int async_io_rank = -1, async_io_nprocs = -1;
  MPI_Group async_io_group = MPI_GROUP_NULL;
  bool is_async_io_proc = false;
  std::vector<int> tmp_async_io_ranks;

  if(!dup_sync_comm){
    /* Try to find async io proc corresponding to each sync io proc
     * 1) Use the sync io proc stride to divide the union comm world procs into 
     *    "local ranges" of union comm procs. The "local range" corresponds to
     *    procs in the local comm when strides are set so that we have 1 I/O proc
     *    per node (the typical setting)
     * 2) Start searching for async io proc in this "local range", starting from
     *    the process after the current io proc and looping back to the start of
     *    of the "local range"
     * 2.1) If the search fails to find a candidate async io  proc in the "local range"
     *      warn the user and move to the next process in the union comm
     */
    int nprocs_in_local_range = sync_io_stride;

    std::for_each(tmp_sync_io_ranks.cbegin(), tmp_sync_io_ranks.cend(),
      [nprocs_in_local_range, union_comm_rank, union_comm_sz, &union_rank_is_io_rank,
        &tmp_async_io_ranks, &is_async_io_proc](int io_rank) mutable {
        /* First search through "local range" to find candidate async io proc. Then
         * search the rest of the procs in union comm. Start from the next "local range"
         * since the previous ranges have a higher chance to be filled up
         * Make sure to loop back once we reach the end - since there are potentially
         * many "holes"/"procs not considered" in the list
         */
        bool found_candidate = false;

        int io_rank_local_range = io_rank / nprocs_in_local_range;
        int local_proc_range_end = (io_rank_local_range + 1) * nprocs_in_local_range;
        if(local_proc_range_end > union_comm_sz) { local_proc_range_end = union_comm_sz; }
        assert(io_rank < local_proc_range_end);

        int local_proc_range_start = (io_rank + 1 != local_proc_range_end) ? (io_rank + 1) :
          (io_rank_local_range * nprocs_in_local_range);

        std::vector<std::pair<int, int> > search_ranges = {
          /* "local" range */
          {local_proc_range_start, local_proc_range_end},
          /* "global" ranges */
          {local_proc_range_end, union_comm_sz},
          {0, io_rank} };

        for(std::vector<std::pair<int, int> >::const_iterator riter = search_ranges.cbegin();
              !found_candidate && (riter != search_ranges.cend()); ++riter){
          for(int i = riter->first; i < riter->second; i++){
            if(!union_rank_is_io_rank[i]){
              found_candidate = true;
              tmp_async_io_ranks.push_back(i);
              union_rank_is_io_rank[i] = true;
              if(i == union_comm_rank) { is_async_io_proc = true; }
              break;
            }
          }
        }
      });

    if(!tmp_async_io_ranks.empty()){
      if(tmp_async_io_ranks.size() < tmp_sync_io_ranks.size()){
        std::string warn_msg = std::string("Unable to allocate the same number of async I/O procs (") +
          std::to_string(tmp_async_io_ranks.size()) + std::string(" procs) as the sync I/O procs (") +
          std::to_string(tmp_sync_io_ranks.size()) + std::string(" procs). This may impact write I/O throughput when using async I/O");
        PIOc_warn(union_comm, __FILE__, __LINE__, warn_msg.c_str());
      }

      async_io_root_ = tmp_async_io_ranks.front();
      async_io_nprocs = static_cast<int>(tmp_async_io_ranks.size());
      /* Create async I/O process group and comm */
      ret = MPI_Group_incl(union_group, static_cast<int>(tmp_async_io_ranks.size()), tmp_async_io_ranks.data(), &async_io_group);
      if(ret != MPI_SUCCESS){
        throw std::runtime_error("Creating MPI process group for async I/O processes failed");
      }

      ret = MPI_Comm_create(union_comm, async_io_group, &async_io_comm);
      if(ret != MPI_SUCCESS){
        throw std::runtime_error("Creating MPI communicator for async I/O processes failed");
      }
  
      if(is_async_io_proc){
        assert(async_io_comm != MPI_COMM_NULL);
        ret = MPI_Comm_rank(async_io_comm, &async_io_rank); assert(ret == MPI_SUCCESS);
      }
    }
    else{
      PIOc_warn(union_comm, __FILE__, __LINE__, "Unable to allocate dedicated processes (no dedicated procs available) for async I/O. Will use the same set of processes for sync and async I/O. This may impact read I/O throughput when using async I/O");
      dup_sync_comm = true;
    }
  }

  if(dup_sync_comm){
    /* Since we use the same set of procs for sync and async ops, we need to sync
     * between sync (e.g. read) and async (e.g. write) ops
     */
    needs_sync_btw_sync_and_async_ops_ = true;

    ret = MPI_Comm_dup(sync_io_comm, &async_io_comm);
    if(ret != MPI_SUCCESS){
      throw std::runtime_error("Creating MPI communicator for async I/O processes failed. Dup of MPI communicator for sync I/O processes failed");
    }

    ret = MPI_Comm_group(async_io_comm, &async_io_group);
    if(ret != MPI_SUCCESS){
      throw std::runtime_error("Creating MPI process group for async I/O processes failed. Accessing MPI group from async MPI communicator failed");
    }

    async_io_root_ = sync_io_root;
    async_io_rank = sync_io_rank;
    async_io_nprocs = sync_io_nprocs;
    is_async_io_proc = is_sync_io_proc_;
    tmp_async_io_ranks = tmp_sync_io_ranks;
  }

  is_async_io_proc_ = is_async_io_proc;

  async_comm_info_.set(async_io_comm, async_io_rank, async_io_nprocs,
    async_io_group, std::move(tmp_async_io_ranks));
#endif

  sync_comm_info_.set(sync_io_comm, sync_io_rank, sync_io_nprocs,
    sync_io_group, std::move(tmp_sync_io_ranks));

}

SPIO_Util::Iosys_Util::IO_comm_info::IO_comm_info(MPI_Comm sync_io_comm, int sync_io_comm_rank, int sync_io_comm_sz,
  MPI_Group sync_io_group, std::vector<int> sync_io_ranks,
  bool is_sync_io_proc, int sync_io_master, int sync_io_root,
  bool owns_resources):
    is_sync_io_proc_(is_sync_io_proc), sync_io_master_(sync_io_master), sync_io_root_(sync_io_root),
    is_async_io_proc_(false), async_io_master_(MPI_PROC_NULL), async_io_root_(-1),
    needs_sync_btw_sync_and_async_ops_(false)
{
  sync_comm_info_.set(sync_io_comm, sync_io_comm_rank, sync_io_comm_sz, sync_io_group,
    std::move(sync_io_ranks), owns_resources);
}

bool SPIO_Util::Iosys_Util::iosys_uses_async_io_service(iosystem_desc_t *iosys)
{
  assert(iosys);
  return (iosys->intercomm != MPI_COMM_NULL);
}
