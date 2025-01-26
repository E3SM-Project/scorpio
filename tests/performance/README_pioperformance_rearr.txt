============ EXAMPLE ==================

#!/bin/bash
for nioprocs in 2 4 8 12 16
do
  echo "Running with $nioprocs I/O procs"
  mpiexec -n 16 ../scorpio_build/tests/performance/pioperf_rearr --pio-decompfiles='piodecomp002049id16tasks04io01dims0253.dat, piodecomp002049id16tasks04io02dims0247.dat, piodecomp002049id16tasks04io01dims0252.dat, piodecomp002049id16tasks04io02dims0248.dat' --pio-types='pnetcdf' --pio-rearrangers='1,2,3,4' --pio-nvars=1 --pio-niotasks=$nioprocs
done

