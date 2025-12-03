#include "spio_dt_converter.hpp"

void *SPIO_Util::File_Util::DTConverter::convert(int ncid, void *buf, std::size_t sz, int from_pio_type, int to_pio_type)
{
  if(from_pio_type == to_pio_type){
    /* No conversion required, return the buffer as is */
    return buf;
  }

  std::size_t nelems = sz / size_of(from_pio_type);
  
  Cachebuf cbuf = { bget(nelems * size_of(to_pio_type)), to_pio_type };

  copy_to(buf, from_pio_type, cbuf.buf, to_pio_type, nelems);

  std::map<int, std::vector<Cachebuf> >::iterator cbufs_iter = cbufs_.find(ncid);
  if(cbufs_iter != cbufs_.end()){
    cbufs_iter->second.push_back(cbuf);
  }
  else{
    cbufs_.insert(std::make_pair(ncid, std::vector<Cachebuf>({cbuf})));
  }

  return cbuf.buf;
}

void SPIO_Util::File_Util::DTConverter::free(int ncid)
{
  std::map<int, std::vector<Cachebuf> >::iterator cbufs_iter = cbufs_.find(ncid);
  if(cbufs_iter != cbufs_.end()){
    for(std::vector<Cachebuf>::iterator iter = cbufs_iter->second.begin();
          iter != cbufs_iter->second.end(); ++iter){
      brel(iter->buf);
    }
    cbufs_.erase(cbufs_iter);
  }
}

void SPIO_Util::File_Util::DTConverter::clear(void)
{
  for(std::map<int, std::vector<Cachebuf> >::iterator cbufs_iter = cbufs_.begin();
        cbufs_iter != cbufs_.end(); ++cbufs_iter){
    for(std::vector<Cachebuf>::iterator iter = cbufs_iter->second.begin();
          iter != cbufs_iter->second.end(); ++iter){
      brel(iter->buf);
    }
  }
  cbufs_.clear();
}
