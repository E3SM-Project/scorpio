#include <vector>
#include <thread>
#include <cassert>
#include "spio_async_tcomm.hpp"
#include "spio_async_tpool.hpp"

/* Static variables */
thread_local std::size_t SPIO_Util::TComm_info::tidx_ = -1;
thread_local bool SPIO_Util::TComm_info::is_thread_init_ = false;

SPIO_Util::TComm_info::TComm_info(MPI_Comm union_comm, int union_comm_rank, int union_comm_io_root, int union_comm_comp_root, MPI_Comm io_comm, int io_comm_rank, bool is_io_master, MPI_Comm comp_comm, int comp_comm_rank, bool is_comp_master, MPI_Comm intercomm, MPI_Comm my_comm, MPI_Comm node_comm): union_comm_rank_(union_comm_rank), union_comm_io_root_(union_comm_io_root), union_comm_comp_root_(union_comm_comp_root), io_comm_rank_(io_comm_rank), is_io_master_(is_io_master), comp_comm_rank_(comp_comm_rank), is_comp_master_(is_comp_master)
{
  tids_ = PIO_Util::PIO_async_tpool_manager::get_tpool_instance()->get_thread_ids();  

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
  for(std::size_t i = 0; i < tids_.size(); i++){
    MPI_Comm tmp_union_comm = MPI_COMM_NULL;
    if(union_comm != MPI_COMM_NULL){
      ret = MPI_Comm_dup(union_comm, &tmp_union_comm); assert(ret == MPI_SUCCESS);
    }
    union_comms_.push_back(tmp_union_comm);

    MPI_Comm tmp_io_comm = MPI_COMM_NULL;
    if(io_comm != MPI_COMM_NULL){
      ret = MPI_Comm_dup(io_comm, &tmp_io_comm); assert(ret == MPI_SUCCESS);
    }
    io_comms_.push_back(tmp_io_comm);

    MPI_Comm tmp_comp_comm = MPI_COMM_NULL;
    if(comp_comm != MPI_COMM_NULL){
      ret = MPI_Comm_dup(comp_comm, &tmp_comp_comm); assert(ret == MPI_SUCCESS);
    }
    comp_comms_.push_back(tmp_comp_comm);

    MPI_Comm tmp_comm = MPI_COMM_NULL;
    if(intercomm != MPI_COMM_NULL){
      ret = MPI_Comm_dup(intercomm, &tmp_comm); assert(ret == MPI_SUCCESS);
    }
    intercomms_.push_back(tmp_comm);

    /* my_comm, see iosystem_desc_t for details, is just a copy (not dup)
     * of union_comm or comp_comm
     */
    if(my_comm == union_comm){
      my_comms_.push_back(tmp_union_comm);
    }
    else if(my_comm == comp_comm){
      my_comms_.push_back(tmp_comp_comm);
    }
    else if(my_comm == io_comm){
      my_comms_.push_back(tmp_io_comm);
    }
    else{
      assert(0);
    }

    tmp_comm = MPI_COMM_NULL;
    if(node_comm != MPI_COMM_NULL){
      ret = MPI_Comm_dup(node_comm, &tmp_comm); assert(ret == MPI_SUCCESS);
    }
    node_comms_.push_back(tmp_comm);
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

SPIO_Util::TComm_info::~TComm_info()
{
  /* The main thread comms, in tidx == 0, was just copied over (not duped)
   * Similarly my_comms are also just copied over
   * Free other comms
   */

  /* Number of threads including the main thread */
  std::size_t nthreads = tids_.size() + 1;
  assert(union_comms_.size() == nthreads);
  for(std::size_t tidx = 1; tidx < nthreads; tidx++){
    if(union_comms_[tidx] != MPI_COMM_NULL) { MPI_Comm_free(&union_comms_[tidx]); }
    if(io_comms_[tidx] != MPI_COMM_NULL) { MPI_Comm_free(&io_comms_[tidx]); }
    if(comp_comms_[tidx] != MPI_COMM_NULL) { MPI_Comm_free(&comp_comms_[tidx]); }
    if(intercomms_[tidx] != MPI_COMM_NULL) { MPI_Comm_free(&intercomms_[tidx]); }
    if(node_comms_[tidx] != MPI_COMM_NULL) { MPI_Comm_free(&node_comms_[tidx]); }
  }
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
