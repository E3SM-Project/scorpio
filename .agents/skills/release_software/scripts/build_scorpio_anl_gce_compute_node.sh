#!/bin/bash

module purge
module load gcc/12.1.0

export CC=/nfs/gce/projects/climate/software/linux-ubuntu22.04-x86_64/mpich/4.1.2/gcc-12.1.0/bin/mpicc
export CXX=/nfs/gce/projects/climate/software/linux-ubuntu22.04-x86_64/mpich/4.1.2/gcc-12.1.0/bin/mpicxx
export FC=/nfs/gce/projects/climate/software/linux-ubuntu22.04-x86_64/mpich/4.1.2/gcc-12.1.0/bin/mpif90
export F77=/nfs/gce/projects/climate/software/linux-ubuntu22.04-x86_64/mpich/4.1.2/gcc-12.1.0/bin/mpif90

export PATH=/nfs/gce/projects/climate/software/linux-ubuntu22.04-x86_64/mpich/4.1.2/gcc-12.1.0/bin:$PATH
export LD_LIBRARY_PATH=/nfs/gce/projects/climate/software/linux-ubuntu22.04-x86_64/mpich/4.1.2/gcc-12.1.0/lib:$LD_LIBRARY_PATH

export PNETCDF_PATH=/nfs/gce/projects/climate/software/linux-ubuntu22.04-x86_64/pnetcdf/1.12.3/mpich-4.1.2/gcc-12.1.0

# Use address sanitizer
export CFLAGS="-g -O0 -Wall -std=c99 -fsanitize=address -fno-omit-frame-pointer"
export CXXFLAGS="-g -O0 -Wall -fsanitize=address -fno-omit-frame-pointer"
export FFLAGS="-ffixed-line-length-none -ffree-line-length-none -g -O0 -Wall -fsanitize=address -fno-omit-frame-pointer"
export FCFLAGS="-ffixed-line-length-none -ffree-line-length-none -g -O0 -Wall -fsanitize=address -fno-omit-frame-pointer"

# Create/Cleanup build directory
mkdir -p scorpio_build
cd scorpio_build
rm -rf *

# Configure with PnetCDF
cmake \
-DPIO_BUILD_STATIC_LIBS=TRUE \
-DWITH_PNETCDF:BOOL=TRUE \
-DBUILD_SHARED_LIBS:BOOL=OFF \
-DPIO_ENABLE_FORTRAN:BOOL=ON \
-DCMAKE_BUILD_TYPE=Release \
-DPIO_BUILD_TESTS:BOOL=ON \
-DPIO_ENABLE_EXAMPLES:BOOL=ON \
-DPIO_ENABLE_TESTS:BOOL=ON \
-DPIO_ENABLE_TIMING:BOOL=ON \
-DPIO_ENABLE_INTERNAL_TIMING:BOOL=ON \
-DPIO_MICRO_TIMING:BOOL=ON \
-DPLATFORM:STRING=linux-gnu \
-DPnetCDF_PATH=$PNETCDF_PATH \
-LH \
.. |& tee configure.log

# Build library, examples and tests
make -j4 |& tee make.log
make -j4 examples |& tee example.log
make -j4  tests |& tee test.log

# Run the tests
make test |& tee test_out.log
