#include "spio_decomp_map_info_pool.hpp"
#include <iostream>
#include <fstream>
#include "mpi.h"

SPIO_Util::Decomp_Util::Decomp_map_info_pool *SPIO_Util::Decomp_Util::Decomp_map_info_pool_manager::dpool_ = NULL;

static SPIO_Util::Decomp_Util::Decomp_map_info_pool_manager gdpool_mgr;

SPIO_Util::Decomp_Util::Decomp_map_info_pool *
  SPIO_Util::Decomp_Util::Decomp_map_info_pool_manager::get_decomp_map_info_pool(void )
{
  if(dpool_ == NULL){
    dpool_ = new SPIO_Util::Decomp_Util::Decomp_map_info_pool();
  }

  return dpool_;
}

SPIO_Util::Decomp_Util::Decomp_map_info_pool_manager::~Decomp_map_info_pool_manager()
{
  if(dpool_){
    delete(dpool_);
    dpool_ = NULL;
  }
}

void SPIO_Util::Decomp_Util::serialize_decomp_map_info_pool(MPI_Comm comm)
{
  static bool decomp_info_serialized = false;

  if(decomp_info_serialized) return;

  int rank = -1;
  MPI_Comm_rank(comm, &rank);
  if(rank != 0) return;

  SPIO_Util::Decomp_Util::Decomp_map_info_pool *dpool = SPIO_Util::Decomp_Util::gdpool_mgr.get_decomp_map_info_pool();
  std::string decomp_map_pool_info = dpool->to_string();
  
  std::ofstream ostr("pio_decomp_map_info.txt");
  if(ostr.is_open()){
    ostr << decomp_map_pool_info.c_str() << "\n";
    ostr.close();
    decomp_info_serialized = true;
  }
}
