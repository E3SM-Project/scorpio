# CMake generated Testfile for 
# Source directory: /home/tkurc/codar/acme/ParallelIO/tests/unit
# Build directory: /home/tkurc/codar/acme/ParallelIO/build/tests/unit
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(pio_unit_test "/home/tkurc/codar/spack/opt/spack/linux-centos7-x86_64/gcc-4.8.5/mpich-3.2-7ebkszxwry7gtzk47p2ygd6n235rohhi/bin/mpiexec" "-np" "4" "/home/tkurc/codar/acme/ParallelIO/build/tests/unit/pio_unit_test")
set_tests_properties(pio_unit_test PROPERTIES  TIMEOUT "60")
