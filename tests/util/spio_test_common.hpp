#ifndef __SPIO_TEST_COMMON_HPP__
#define __SPIO_TEST_COMMON_HPP__

#include <cstring>
#include "pio.h"

// Covers PNETCDF/NETCDF/NETCDF4C/NETCDF4P/NETCDF4P_NCZARR. ADIOS/ADIOSC/
// HDF5/HDF5C are not included here: PIOc_iotype_available_impl() has no
// case for them, so PIOc_iotype_available() always reports them
// unavailable regardless of build config. get_undef_iotypes() below
// queries them correctly via the _ADIOS2/_HDF5/_SPIO_*_USE_COMPRESSION
// compile-time macros instead (mirroring tests/general/util/pio_tutil.F90's
// PIO_TF_Get_undef_iotypes), since those are the actual gate
// PIOc_createfile uses (iotype_is_valid() in pioc_support.cpp).
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

const int SPIO_TEST_NUM_ALL_IOTYPES = 9;

inline bool is_iotype_available_for_tests(int iotype)
{
  switch(iotype){
    case PIO_IOTYPE_ADIOS:
#ifdef _ADIOS2
      return true;
#else
      return false;
#endif
    case PIO_IOTYPE_ADIOSC:
#if defined(_ADIOS2) && defined(_SPIO_ADIOS_USE_COMPRESSION)
      return true;
#else
      return false;
#endif
    case PIO_IOTYPE_HDF5:
#ifdef _HDF5
      return true;
#else
      return false;
#endif
    case PIO_IOTYPE_HDF5C:
#if defined(_HDF5) && defined(_SPIO_HDF5_USE_COMPRESSION)
      return true;
#else
      return false;
#endif
    default:
      return (PIOc_iotype_available(iotype) != 0);
  }
}

inline int get_undef_iotypes(int *num_undef, int *undef_iotypes)
{
  static const int all_iotypes[SPIO_TEST_NUM_ALL_IOTYPES] =
      {PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C, PIO_IOTYPE_NETCDF4P,
       PIO_IOTYPE_NETCDF4P_NCZARR, PIO_IOTYPE_ADIOS, PIO_IOTYPE_ADIOSC,
       PIO_IOTYPE_HDF5, PIO_IOTYPE_HDF5C};
  int num = 0;

  for(int i = 0; i < SPIO_TEST_NUM_ALL_IOTYPES; i++){
    if(!is_iotype_available_for_tests(all_iotypes[i])){
      if(undef_iotypes){
        undef_iotypes[num] = all_iotypes[i];
      }
      num++;
    }
  }

  if(num_undef){
    *num_undef = num;
  }

  return PIO_NOERR;
}

inline int get_undef_iotype_name(int iotype, char *name)
{
  static const int all_iotypes[SPIO_TEST_NUM_ALL_IOTYPES] =
      {PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C, PIO_IOTYPE_NETCDF4P,
       PIO_IOTYPE_NETCDF4P_NCZARR, PIO_IOTYPE_ADIOS, PIO_IOTYPE_ADIOSC,
       PIO_IOTYPE_HDF5, PIO_IOTYPE_HDF5C};
  static const char *all_iotype_names[SPIO_TEST_NUM_ALL_IOTYPES] =
      {"pnetcdf", "classic", "serial4", "parallel4", "nczarr",
       "adios", "adiosc", "hdf5", "hdf5c"};

  if(!name){
    return PIO_EINVAL;
  }
  for(int i = 0; i < SPIO_TEST_NUM_ALL_IOTYPES; i++){
    if(all_iotypes[i] == iotype){
      strcpy(name, all_iotype_names[i]);
      return PIO_NOERR;
    }
  }
  return PIO_EINVAL;
}

#endif
