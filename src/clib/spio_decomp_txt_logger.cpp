#include "spio_decomp_logger.hpp"

SPIO_Util::Decomp_Util::Decomp_logger& SPIO_Util::Decomp_Util::Decomp_txt_logger::open(void )
{
  return *this;
}

void SPIO_Util::Decomp_Util::Decomp_txt_logger::get_info(std::string &version, int &nprocs, int &ngdims, PIO_Offset &lcompmap_sz)
{
}

void SPIO_Util::Decomp_Util::Decomp_txt_logger::get_gdims(int *gdims, std::size_t gdims_sz)
{
}

void SPIO_Util::Decomp_Util::Decomp_txt_logger::get_lcompmap(PIO_Offset *lcompmap, std::size_t lcompmap_sz)
{
}

SPIO_Util::Decomp_Util::Decomp_logger& SPIO_Util::Decomp_Util::Decomp_txt_logger::get(std::string &version, int &nprocs, std::vector<int> &gdims, std::vector<PIO_Offset> &lcompmap)
{
  return *this;
}

SPIO_Util::Decomp_Util::Decomp_logger& SPIO_Util::Decomp_Util::Decomp_txt_logger::put(io_desc_t *iodesc)
{
  return *this;
}

void SPIO_Util::Decomp_Util::Decomp_txt_logger::close(void )
{
}
