# CMake generated Testfile for 
# Source directory: /home/tkurc/codar/acme/ParallelIO/examples/c
# Build directory: /home/tkurc/codar/acme/ParallelIO/build/examples/c
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(examplePio "/home/tkurc/codar/spack/opt/spack/linux-centos7-x86_64/gcc-4.8.5/mpich-3.2-7ebkszxwry7gtzk47p2ygd6n235rohhi/bin/mpiexec" "-np" "4" "/home/tkurc/codar/acme/ParallelIO/build/examples/c/examplePio")
set_tests_properties(examplePio PROPERTIES  TIMEOUT "60")
add_test(example1 "/home/tkurc/codar/spack/opt/spack/linux-centos7-x86_64/gcc-4.8.5/mpich-3.2-7ebkszxwry7gtzk47p2ygd6n235rohhi/bin/mpiexec" "-np" "4" "/home/tkurc/codar/acme/ParallelIO/build/examples/c/example1")
set_tests_properties(example1 PROPERTIES  TIMEOUT "60")
add_test(put_var "/home/tkurc/codar/spack/opt/spack/linux-centos7-x86_64/gcc-4.8.5/mpich-3.2-7ebkszxwry7gtzk47p2ygd6n235rohhi/bin/mpiexec" "-np" "4" "/home/tkurc/codar/acme/ParallelIO/build/examples/c/put_var")
set_tests_properties(put_var PROPERTIES  TIMEOUT "60")
add_test(darray_no_async "/home/tkurc/codar/spack/opt/spack/linux-centos7-x86_64/gcc-4.8.5/mpich-3.2-7ebkszxwry7gtzk47p2ygd6n235rohhi/bin/mpiexec" "-np" "4" "/home/tkurc/codar/acme/ParallelIO/build/examples/c/darray_no_async")
set_tests_properties(darray_no_async PROPERTIES  TIMEOUT "60")
