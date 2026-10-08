/** @file
 * Utils related to I/O System
 */
#ifndef __SPIO_IOSYS_UTILS_HPP__
#define __SPIO_IOSYS_UTILS_HPP__

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include <string>
#include <vector>
#include <exception>
#include <stdexcept>
#include <algorithm>

namespace SPIO_Util{
  namespace Iosys_Util{
    /* Contains MPI communicator info,
     *  Note: The ownership of the comm/group is passed to this class from the caller
     */
    class Comm_info{
      public:
        Comm_info() : comm(MPI_COMM_NULL), rank(0), sz(0), group(MPI_GROUP_NULL), owns_resources(true) {}
        Comm_info(MPI_Comm comm) : comm(comm), rank(0), sz(0), group(MPI_GROUP_NULL), owns_resources(true) {
          int ret = MPI_SUCCESS;
          if(comm == MPI_COMM_NULL) { return; }

          ret = MPI_Comm_size(comm, &sz);
          if(ret == MPI_SUCCESS) { ret = MPI_Comm_rank(comm, &rank); }

          if(ret != MPI_SUCCESS) { throw std::runtime_error("Creating comm_info failed, Unable to get rank/size from the MPI Communicator"); }
        }

        Comm_info(MPI_Comm comm, int rank, int sz) : comm(comm), rank(rank), sz(sz), group(MPI_GROUP_NULL), owns_resources(true) {}
        Comm_info(MPI_Comm comm, int rank, int sz, MPI_Group group, std::vector<int> &&io_ranks) :
          comm(comm), rank(rank), sz(sz), group(group), io_ranks(std::move(io_ranks)), owns_resources(true) {}
        Comm_info(MPI_Comm comm, int rank, int sz, MPI_Group group, int *io_ranks, int nio_ranks) :
          comm(comm), rank(rank), sz(sz), group(group), io_ranks(io_ranks, io_ranks + nio_ranks), owns_resources(true) {}
        /* Used when the comm/group are owned/freed elsewhere (e.g. test mocks that alias
         * an externally managed communicator) - owns_resources controls whether the
         * destructor frees comm/group. */
        Comm_info(MPI_Comm comm, int rank, int sz, MPI_Group group, std::vector<int> io_ranks, bool owns_resources) :
          comm(comm), rank(rank), sz(sz), group(group), io_ranks(std::move(io_ranks)), owns_resources(owns_resources) {}

        /* Not copyable: owns_resources would copy as true, making a copy's
         * destructor double-free the original's comm/group. */
        Comm_info(const Comm_info &) = delete;
        Comm_info &operator=(const Comm_info &) = delete;

        inline void set(MPI_Comm comm, int rank, int sz, MPI_Group group, std::vector<int> &&io_ranks, bool owns_resources = true){
          this->comm = comm; this->rank = rank; this->sz = sz; this->group = group;
          this->io_ranks = std::move(io_ranks);
          this->owns_resources = owns_resources;
        }

        ~Comm_info(){
          if(owns_resources){
            if(comm != MPI_COMM_NULL) { MPI_Comm_free(&comm); }
            if(group != MPI_GROUP_NULL) { MPI_Group_free(&group); }
          }
        }

        MPI_Comm comm;
        int rank;
        int sz;
        MPI_Group group;
        std::vector<int> io_ranks;
        bool owns_resources;
    };

    class IO_comm_info{
      public:
        IO_comm_info(MPI_Comm union_comm, MPI_Group union_group, int sync_io_root, int sync_io_stride, int sync_io_nprocs);

        /* Construct directly from already-known sync I/O comm info, without deriving
         * it from a union comm - used by test mocks. owns_resources controls whether
         * the destructor frees sync_io_comm/sync_io_group (default false: assume the
         * caller owns/frees them, matching how existing test mocks alias comms). */
        IO_comm_info(MPI_Comm sync_io_comm, int sync_io_comm_rank, int sync_io_comm_sz,
          MPI_Group sync_io_group, std::vector<int> sync_io_ranks,
          bool is_sync_io_proc, int sync_io_master, int sync_io_root,
          bool owns_resources = false);

        /* Construct directly from specified sync and async I/O comm info, without deriving
         * it from union_comm - used by unit tests
         */
        IO_comm_info(MPI_Comm sync_io_comm, int sync_io_comm_rank, int sync_io_comm_sz,
          MPI_Group sync_io_comm_group, std::vector<int> sync_io_ranks,
          MPI_Comm async_io_comm, int async_io_comm_rank, int async_io_comm_sz,
          MPI_Group async_io_comm_group, std::vector<int> async_io_ranks);

        /* Not copyable: Comm_info members own MPI resources whose
         * destructor would double-free on a shallow copy. */
        IO_comm_info(const IO_comm_info &) = delete;
        IO_comm_info &operator=(const IO_comm_info &) = delete;

        /* Query sync I/O comm info */
        MPI_Comm get_sync_io_comm(void ) const { return sync_comm_info_.comm; }
        MPI_Group get_sync_io_group(void ) const { return sync_comm_info_.group; }
        int get_sync_io_comm_sz(void ) const { return sync_comm_info_.sz; }
        int get_sync_io_comm_rank(void ) const { return sync_comm_info_.rank; }
        int get_sync_io_master(void ) const { return sync_io_master_; }
        int get_sync_io_root(void ) const { return sync_io_root_; }
        std::vector<int> get_sync_io_ranks(void ) const { return sync_comm_info_.io_ranks; }
        void get_sync_io_ranks(int *io_ranks, std::size_t sz) const {
          assert(sz >= 0);
          if(sz == 0) return;
          assert(io_ranks);
          if(sz < sync_comm_info_.io_ranks.size()){
            throw std::runtime_error("Insufficient space (" + std::to_string(sz) +
                    " elements) in user buffer to copy I/O ranks (" +
                    std::to_string(sync_comm_info_.io_ranks.size()) + " elements)");
          }

          std::copy(sync_comm_info_.io_ranks.cbegin(), sync_comm_info_.io_ranks.cend(), io_ranks);
        }
        bool is_sync_io_proc(void ) const { return is_sync_io_proc_; }
        void get_sync_io_comm_info(MPI_Comm &sync_comm, int &sync_comm_sz, int &sync_comm_rank, int &sync_io_master, int &sync_io_root, bool &is_sync_io_proc) const {
          sync_comm = sync_comm_info_.comm;
          sync_comm_sz = sync_comm_info_.sz;
          sync_comm_rank = sync_comm_info_.rank;
          sync_io_master = sync_io_master_;
          sync_io_root = sync_io_root_;
          is_sync_io_proc = is_sync_io_proc_;
        }

        /* Query async I/O comm info */
        MPI_Comm get_async_io_comm(void ) const { return async_comm_info_.comm; }
        MPI_Group get_async_io_group(void ) const { return async_comm_info_.group; }
        int get_async_io_comm_sz(void ) const { return async_comm_info_.sz; }
        int get_async_io_comm_rank(void ) const { return async_comm_info_.rank; }
        int get_async_io_master(void ) const { return async_io_master_; }
        int get_async_io_root(void ) const { return async_io_root_; }
        std::vector<int> get_async_io_ranks(void ) const { return async_comm_info_.io_ranks; }
        void get_async_io_ranks(int *io_ranks, std::size_t sz) const {
          assert(sz >= 0);
          if(sz == 0) return;
          assert(io_ranks);
          if(sz < async_comm_info_.io_ranks.size()){
            throw std::runtime_error("Insufficient space (" + std::to_string(sz) +
                    " elements) in user buffer to copy I/O ranks (" +
                    std::to_string(async_comm_info_.io_ranks.size()) + " elements)");
          }

          std::copy(async_comm_info_.io_ranks.cbegin(), async_comm_info_.io_ranks.cend(), io_ranks);
        }
        bool is_async_io_proc(void ) const { return is_async_io_proc_; }
        void get_async_io_comm_info(MPI_Comm &async_comm, int &async_comm_sz, int &async_comm_rank, int &async_io_master, int &async_io_root, bool &is_async_io_proc) const {
          async_comm = async_comm_info_.comm;
          async_comm_sz = async_comm_info_.sz;
          async_comm_rank = async_comm_info_.rank;
          async_io_master = async_io_master_;
          async_io_root = async_io_root_;
          is_async_io_proc = is_async_io_proc_;
        }

        bool is_sync_or_async_io_proc(void ) const { return is_sync_io_proc() || is_async_io_proc(); }

      private:
        Comm_info sync_comm_info_;
        /* Async comm info is only valid if async is enabled */
        Comm_info async_comm_info_;
        bool is_sync_io_proc_;
        /* Sync io master is only valid on sync I/O procs */
        int sync_io_master_;
        /* Sync io root is valid on all processes - compute/io procs */
        int sync_io_root_;
        bool is_async_io_proc_;
        /* Async io master is only valid on async I/O procs - if async is enabled */
        int async_io_master_;
        /* Async io root is valid on all processes - compute/io procs - if async is enabled*/
        int async_io_root_;
        /* If true, all sync procs need to wait for pending async ops */
        bool needs_sync_btw_sync_and_async_ops_;
    };

    bool iosys_uses_async_io_service(iosystem_desc_t *iosys);
  } // namespace Iosys_Util
} // namespace SPIO_Util

#endif // __SPIO_IOSYS_UTILS_HPP__
