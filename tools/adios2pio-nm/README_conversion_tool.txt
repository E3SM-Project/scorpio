This file contains some basic information on how to use the ADIOS to NetCDF conversion tool for converting SCORPIO output files in the ADIOS BP file format to NetCDF. The conversion tool reads the ADIOS BP files in parallel (using SCORPIO and ADIOS) and converts the file using SCORPIO and PnetCDF or NetCDF libraries.

Running the application
------------------------
If SCORPIO is enabled with support for ADIOS (see the configure log of SCORPIO to see if ADIOS support is enabled), using PIO_IOTYPE_ADIOS (for uncompressed output) or PIO_IOTYPE_ADIOSC (for compressed output) will result in all SCORPIO output being written in the ADIOS BP file format. The output files in the ADIOS BP file format can be converted to NetCDF using the conversion tool in a separate batch job.

The conversion tool is an MPI program and can be run with different number (i.e., fewer) of MPI processes compared to the application that generated the output.


---------------------------------------------------------------------------------------
EXAMPLES:
---------------------------------------------------------------------------------------

# Program options

jayesh@compute-386-03:~/scorpio_build2$ ./tools/adios2pio-nm/adios2pio-nm.exe --help
Usage : ./tools/adios2pio-nm/adios2pio-nm.exe --[OPTIONAL ARG1 NAME]=[OPTIONAL ARG1 VALUE] --[OPTIONAL ARG2 NAME]=[OPTIONAL ARG2 VALUE] ... 
Optional Arguments :
--bp-file:	input file (or regex of basename for multiple files) in ADIOS BP format
--idir:	Directory containing the input files (in ADIOS BP format)
--nc-file:	output file name after conversion (will be used as prefix when converting multiple files)
--pio-format:	output SCORPIO I/O type. Supported parameters: "pnetcdf",  "netcdf",  "netcdf4c",  "netcdf4p", "nczarr"
--rearr:	SCORPIO rearranger. Supported parameters: "subset", "box", "any". Default "any".
--rm-bp-file-after-conv:	remove the ADIOS BP file after conversion (or regex to specify which files to delete after conversion)
--verbose:	Turn on verbose info messages

# Delete all ADIOS BP files after conversion
jayesh@compute-386-03:~/scorpio_build2$ ./tools/adios2pio-nm/adios2pio-nm.exe --bp-file=/home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp --rm-bp-file-after-conv
Converting BP file : "/home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp"
Deleting BP file : /home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp

# Delete all ADIOS BP files with name "(.*)(test_pio_file_simple)(.*)" after conversion
jayesh@compute-386-03:~/scorpio_build2$ ./tools/adios2pio-nm/adios2pio-nm.exe --bp-file=/home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp --rm-bp-file-after-conv="(.*)(test_pio_file_simple)(.*)"
Converting BP file : "/home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp"
Deleting BP file : /home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp

jayesh@compute-386-03:~/scorpio_build2$ ./tools/adios2pio-nm/adios2pio-nm.exe --bp-file=/home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp --rm-bp-file-after-conv="(.*)(DONT_DELETE_test_pio_file_simple)(.*)"
Converting BP file : "/home/jayesh/scorpio_build2/tests/general/test_pio_file_simple_tests.testfile.bp"

# Convert ADIOS BP output files in directory, /scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run, and only delete the EAM ADIOS BP files after conversion
jayesh@compute-386-03:~/scorpio_build2$ mpiexec -n 8 ./tools/adios2pio-nm/adios2pio-nm.exe --idir=/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run --rm-bp-file-after-conv="(.*)(.eam.)(.*)"
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.eam.rs.0001-01-02-00000.nc.bp"
Deleting BP file : /scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.eam.rs.0001-01-02-00000.nc.bp
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.mosart.rh0.0001-01-02-00000.nc.bp"
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.elm.rh0.0001-01-02-00000.nc.bp"
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.cpl.r.0001-01-02-00000.nc.bp"
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.elm.r.0001-01-02-00000.nc.bp"
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.eam.h0.0001-01-01-00000.nc.bp"
Deleting BP file : /scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.eam.h0.0001-01-01-00000.nc.bp
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.eam.r.0001-01-02-00000.nc.bp"
Deleting BP file : /scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.eam.r.0001-01-02-00000.nc.bp
Converting BP file : "/scratch/jayesh/e3sm/scratch/test_F2010_ne4_oQU240/run/test_F2010_ne4_oQU240.mosart.r.0001-01-02-00000.nc.bp"

# Convert ONLY coupler output in ADIOS BP format : Specify the regex for files to convert via "--bp-file" option
jayesh@compute-386-03:~/scorpio_build2$ ./tools/adios2pio-nm/adios2pio-nm.exe --idir=/scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir --bp-file=".*.cpl.*"
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.mpassi.rst.0001-01-03_00000.nc.bp
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.mosart.r.0001-01-03-00000.nc.bp
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.elm.r.0001-01-03-00000.nc.bp
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.eam.rh0.0001-01-03-00000.nc.bp
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.eam.r.0001-01-03-00000.nc.bp
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.mosart.rh0.0001-01-03-00000.nc.bp
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.elm.rh0.0001-01-03-00000.nc.bp
WARNING: Skipping BP file (did not match specified regex): /scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.eam.rs.0001-01-03-00000.nc.bp
Converting BP file : "/scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.cpl.hi.0001-01-04-00000.nc.bp"
Converting BP file : "/scratch/jayesh/e3sm/scratch/tmp_adios_bp_run_dir/ERP_Ld3.ne4pg2_oQU480.F2010.anlgce-ub22_gnu.io-force_adios.20240717_210751_7we0ju.cpl.r.0001-01-03-00000.nc.bp"

