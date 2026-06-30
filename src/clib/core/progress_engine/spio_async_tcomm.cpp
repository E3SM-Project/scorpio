#include <vector>
#include <thread>
#include <mutex>
#include <cassert>
#include <algorithm>
#include "spio_async_tcomm.hpp"
#include "spio_async_tpool.hpp"

/* Static variables */
thread_local std::size_t SPIO_Util::TComm_info::tidx_ = -1;
thread_local bool SPIO_Util::TComm_info::is_thread_init_ = false;

SPIO_Util::TComm_info::TComm_info(MPI_Comm union_comm, int union_comm_rank, int union_comm_io_root, int union_comm_comp_root, MPI_Comm io_comm, int io_comm_rank, bool is_io_master, MPI_Comm comp_comm, int comp_comm_rank, bool is_comp_master, MPI_Comm intercomm, MPI_Comm my_comm, MPI_Comm node_comm): union_comm_rank_(union_comm_rank), union_comm_io_root_(union_comm_io_root), union_comm_comp_root_(union_comm_comp_root), io_comm_rank_(io_comm_rank), is_io_master_(is_io_master), comp_comm_rank_(comp_comm_rank), is_comp_master_(is_comp_master)
{
  int nthreads = 1;
#if PIO_USE_ASYNC_WR_THREAD
  nthreads += PIO_Util::PIO_async_tpool_manager::get_num_threads();
  if(io_comm != MPI_COMM_NULL){
    tids_ = PIO_Util::PIO_async_tpool_manager::get_tpool_instance()->get_thread_ids();
    assert(static_cast<int>(tids_.size()) == nthreads - 1);
  }
#endif

  /* Total number of comms = Main/Default thread + Number of threads in thread pool
   * Main thread info is the first in the list
   */
  tidx_ = 0; is_thread_init_ = true;

  union_comms_.push_back(union_comm);
  io_comms_.push_back(io_comm);
  comp_comms_.push_back(comp_comm);
  intercomms_.push_back(intercomm);
  my_comms_.push_back(my_comm);
  node_comms_.push_back(node_comm);

  int ret = MPI_SUCCESS;
  for(int i = 0; i < nthreads - 1; i++){
    /* Since the I/O threads no longer have access (or should be using)
     * to global communicators, except the I/O communicator (io_comm),
     * all other communicators are assigned to COMM_NULL (not duped)
     */
    union_comms_.push_back(MPI_COMM_NULL);

    MPI_Comm tmp_io_comm = MPI_COMM_NULL;
    if(io_comm != MPI_COMM_NULL){
      ret = MPI_Comm_dup(io_comm, &tmp_io_comm); assert(ret == MPI_SUCCESS);
    }
    io_comms_.push_back(tmp_io_comm);

    comp_comms_.push_back(MPI_COMM_NULL);
    intercomms_.push_back(MPI_COMM_NULL);
    /* my_comm, see iosystem_desc_t for details, is just a copy (not dup)
     * of union_comm or comp_comm
     */
    if(my_comm == io_comm){
      my_comms_.push_back(tmp_io_comm);
    }
    else{
      my_comms_.push_back(MPI_COMM_NULL);
    }

    /* FIXME: To start using node_comm we need to create a node local
     * comm with just the I/O procs and dup it
     */
    node_comms_.push_back(MPI_COMM_NULL);
  }
}

MPI_Comm SPIO_Util::TComm_info::get_union_comm(void )
{
  return union_comms_[get_tidx()];
}

MPI_Comm SPIO_Util::TComm_info::get_io_comm(void )
{
  return io_comms_[get_tidx()];
}

MPI_Comm SPIO_Util::TComm_info::get_comp_comm(void )
{
  return comp_comms_[get_tidx()];
}

MPI_Comm SPIO_Util::TComm_info::get_intercomm(void )
{
  return intercomms_[get_tidx()];
}

MPI_Comm SPIO_Util::TComm_info::get_my_comm(void )
{
  return my_comms_[get_tidx()];
}

MPI_Comm SPIO_Util::TComm_info::get_node_comm(void )
{
  return node_comms_[get_tidx()];
}

MPI_Info *SPIO_Util::TComm_info::create_mpi_info(void )
{
  int ret = MPI_SUCCESS;
  MPI_Info info;
  std::mutex mtx;
  std::lock_guard<std::mutex> lg(mtx);

  ret = MPI_Info_create(&info); assert(ret == MPI_SUCCESS);

  comm_infos_.push_back(info);
  return &(comm_infos_.back());
}

SPIO_Util::TComm_info::~TComm_info()
{
  /* The main thread comms, in tidx == 0, was just copied over (not duped)
   * Similarly my_comms are also just copied over
   * Free other comms
   */

  int nthreads = 1;
#if PIO_USE_ASYNC_WR_THREAD
  /* Number of threads including the main thread */
  nthreads += PIO_Util::PIO_async_tpool_manager::get_num_threads();
#endif
  assert(static_cast<int>(union_comms_.size()) == nthreads);
  for(int tidx = 1; tidx < nthreads; tidx++){
    assert(union_comms_[tidx] == MPI_COMM_NULL);
    if(io_comms_[tidx] != MPI_COMM_NULL) { MPI_Comm_free(&io_comms_[tidx]); }
    assert(comp_comms_[tidx] == MPI_COMM_NULL);
    assert(intercomms_[tidx] == MPI_COMM_NULL);
    assert(node_comms_[tidx] == MPI_COMM_NULL);
  }

  std::for_each(comm_infos_.begin(), comm_infos_.end(), [](MPI_Info &info) { MPI_Info_free(&info); });
}

void SPIO_Util::TComm_info::init_thread_info(void )
{
  tidx_ = INVALID_IDX;
  /* Since we do this once per thread, and have a small number of threads a linear
   * search should be ok
   */
  std::hash<std::thread::id> tid_hasher;
  std::size_t tid = tid_hasher(std::this_thread::get_id());

  for(std::size_t i = 0; i < tids_.size(); i++){
    if(tid == tids_[i]){
      /* First tidx_ (tidx_ == 0) corresponds to main thread info */
      tidx_ = i + 1;
      assert(tidx_ < union_comms_.size());
      break;
    }
  }

  assert(tidx_ != INVALID_IDX);
  is_thread_init_ = true;
}

std::size_t SPIO_Util::TComm_info::get_tidx(void )
{
  if(!is_thread_init_) { init_thread_info(); }
  return tidx_;
}
