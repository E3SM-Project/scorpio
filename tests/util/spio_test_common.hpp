#ifndef __SPIO_TEST_COMMON_HPP__
#define __SPIO_TEST_COMMON_HPP__

#include <cstring>
#include "pio.h"

// Covers PNETCDF/NETCDF/NETCDF4C/NETCDF4P/NETCDF4P_NCZARR. ADIOS/ADIOSC/
// HDF5/HDF5C are not included: PIOc_iotype_available_impl() has no case
// for them, so they cannot be queried via the public PIOc_iotype_available()
// API this file is built on without changing src/clib (out of scope here).
const int SPIO_TEST_NUM_FLAVORS = 5;

inline int get_iotypes(int *num_flavors, int *flavors)
{
  static const int all_flavors[SPIO_TEST_NUM_FLAVORS] =
      {PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C, PIO_IOTYPE_NETCDF4P,
       PIO_IOTYPE_NETCDF4P_NCZARR};
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
      {"pnetcdf", "classic", "serial4", "parallel4", "nczarr"};

  if(!name || iotype < PIO_IOTYPE_PNETCDF || iotype > PIO_IOTYPE_NETCDF4P_NCZARR){
    return PIO_EINVAL;
  }

  strcpy(name, flavor_name[iotype - 1]);
  return PIO_NOERR;
}

#endif
