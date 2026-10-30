#ifndef __SPIO_TEST_COMMON_HPP__
#define __SPIO_TEST_COMMON_HPP__

#include <cstring>
#include "pio.h"

const int SPIO_TEST_NUM_FLAVORS = 4;

inline int get_iotypes(int *num_flavors, int *flavors)
{
  static const int all_flavors[SPIO_TEST_NUM_FLAVORS] =
      {PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C, PIO_IOTYPE_NETCDF4P};
  int num = 0;

  for(int i = 0; i < SPIO_TEST_NUM_FLAVORS; i++){
    if(PIOc_iotype_available(all_flavors[i])){
      if(flavors){
        flavors[num] = all_flavors[i];
      }
      num++;
    }
  }

  if(num_flavors){
    *num_flavors = num;
  }

  return PIO_NOERR;
}

inline int get_iotype_name(int iotype, char *name)
{
  static const char *flavor_name[SPIO_TEST_NUM_FLAVORS] =
      {"pnetcdf", "classic", "serial4", "parallel4"};

  if(!name || iotype < PIO_IOTYPE_PNETCDF || iotype > PIO_IOTYPE_NETCDF4P){
    return PIO_EINVAL;
  }

  strcpy(name, flavor_name[iotype - 1]);
  return PIO_NOERR;
}

#endif
