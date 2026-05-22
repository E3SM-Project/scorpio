#ifndef __SPIO_ASYNC_TCOMM_HPP__
#define __SPIO_ASYNC_TCOMM_HPP__

#include <iostream>
#include <thread>
#include <chrono>
#include <list>
#include <vector>
#include "pio_config.h"
#ifdef MPI_SERIAL
extern "C" {
#endif

#include "mpi.h"

#ifdef MPI_SERIAL
}
#endif
#include "spio_async_tpool.hpp"

namespace SPIO_Util{

/* Keep track of thread-specific MPI communicators */
class TComm_info{
  public:
    /* See iosystem_desc_t{} for info on the communicators cached here */
    TComm_info(MPI_Comm union_comm, int union_comm_rank, int union_comm_io_root, int union_comm_comp_root, MPI_Comm io_comm, int io_comm_rank, bool is_io_master, MPI_Comm comp_comm, int comp_comm_rank, bool is_comp_master, MPI_Comm intercomm, MPI_Comm my_comm, MPI_Comm node_comm);
    MPI_Comm get_union_comm(void );
    int get_union_comm_rank(void ) const { return union_comm_rank_; }
    int get_union_comm_io_root(void ) const { return union_comm_io_root_; }
    int get_union_comm_comp_root(void ) const { return union_comm_comp_root_; }

    MPI_Comm get_io_comm(void );
    int get_io_comm_rank(void ) const { return io_comm_rank_; }
    bool is_io_master(void ) const { return is_io_master_; }

    MPI_Comm get_comp_comm(void );
    int get_comp_comm_rank(void ) const { return comp_comm_rank_; }
    bool is_comp_master(void ) const { return is_comp_master_; }

    MPI_Comm get_intercomm(void );
    MPI_Comm get_my_comm(void );
    MPI_Comm get_node_comm(void );

    /* The MPI_Info object is owned by this class. The user can use it
     * but its a "weak pointer"
     */
    MPI_Info *create_mpi_info(void );

    ~TComm_info();
  private:
    const std::size_t INVALID_IDX = -1;
    /* Index to thread-specific comm in the comm vectors. Index 0
     * is reserved for the main thread
     * e.g. union_comms_[tidx_] is the union comm for current thread
    */
    static thread_local std::size_t tidx_;
    static thread_local bool is_thread_init_;

    int union_comm_rank_;
    int union_comm_io_root_;
    int union_comm_comp_root_;
    int io_comm_rank_;
    bool is_io_master_;
    int comp_comm_rank_;
    bool is_comp_master_;

    std::vector<std::size_t> tids_;
    std::vector<MPI_Comm> union_comms_;
    std::vector<MPI_Comm> io_comms_;
    std::vector<MPI_Comm> comp_comms_;
    std::vector<MPI_Comm> intercomms_;
    std::vector<MPI_Comm> my_comms_;
    std::vector<MPI_Comm> node_comms_;

    /* Note: We return pointers to elements of this list back to user */
    std::list<MPI_Info> comm_infos_;

    void init_thread_info(void );
    std::size_t get_tidx(void );
};

} // namespace SPIO_Util

#endif // __SPIO_ASYNC_TCOMM_HPP__
