This file contains some basic information on how to generate SCORPIO API traces and using the trace files to replay the I/O data patterns.

The following API trace files are generated (when SCORPIO API tracing is enabled),

* spio_trace_log_*.log : Generated for each I/O system and contains the API trace
* spio_trace_mdata_*.log : Generated for each I/O system and contains some meta-data regarding the API trace
* spio_trace_decomp_*.nc : Generated for each I/O system and contains the I/O decompositions used by the I/O system.
* SCORPIO_CMakeCache.txt : Contains info on the SCORPIO build

The above trace files can be used to generate a replay program, spio_replay.exe, that will replay the I/O patterns captured by the trace files.

Generating API trace
---------------------
1) Configure SCORPIO to generate API traces

cmake -DPIO_ENABLE_API_TRACING:BOOL=ON ...

2) Build/Install SCORPIO, Build user app using SCORPIO
3) Run the user app. The API trace files will be output in the run directory.

Creating replay program (generation, build) from the API trace
---------------------------------------------------------------
1) Use the spio_replay.py script to generate and build the replay tool, spio_replay.exe, from the API trace files

Assuming the SCORPIO source is in ~/scorpio, the build/run directory is ~/scorpio_build (trace logs are here), scorpio install directory is ~/scorpio_install,

----------------------------------------------------------------------------
$ ~/scorpio/tools/replay/spio_replay.py --help
=================================================
	SCORPIO REPLAY TOOL GENERATOR
=================================================

usage: spio_replay.py [-h] [--scorpio-src-dir scorpio_src_dir] [--scorpio-build-dir scorpio_build_dir] [--scorpio-install-dir scorpio_install_dir] [--trace-log-dir trace_log_dir]
                      [--replay-tool-dir replay_tool_dir] [-v] [-q]

Script to generate and build spio_replay.exe from SCORPIO trace logs. This script reads the SCORPIO trace logs & trace meta data logs to generate source files to replay the I/O from the trace. These
sources are built, using the same configuration used to build SCORPIO, to create spio_replay.exe . At runtime spio_replay.exe requires the I/O decomposition files, spio_trace_decomp_*.nc, to replay the
I/O data pattern. The script requires the I/O trace logs, spio_trace_log*.log, and the I/O trace meta-data logs, spio_trace_mdata*.log, to generate the executable, spio_replay.exe, that can be used to
replay the I/O data patterns in an application (e.g. E3SM) run

options:
  -h, --help            show this help message and exit
  --scorpio-src-dir scorpio_src_dir
                        Source directory for the SCORPIO library (default: /home/jayesh/scorpio_build3/tmp)
  --scorpio-build-dir scorpio_build_dir
                        Build directory for the SCORPIO library (default: /home/jayesh/scorpio_build3/tmp)
  --scorpio-install-dir scorpio_install_dir
                        Install directory for the SCORPIO library (default: /home/jayesh/scorpio_build3/tmp)
  --trace-log-dir trace_log_dir
                        Directory containing SCORPIO trace log files (default: /home/jayesh/scorpio_build3/tmp)
  --replay-tool-dir replay_tool_dir
                        Directory to store replay tool source/exe (default: /home/jayesh/scorpio_build3/tmp)
  -v, --verbose         Turn on verbose mode (default: False)
  -q, --quiet           Turn on quiet mode (no output to stdout) (default: False)

$ mkdir scorpio_replay_tool_build
$ cd scorpio_replay_tool_build
$ ~/scorpio/tools/replay/spio_replay.py --scorpio-src-dir=~/scorpio --scorpio-build-dir=~/scorpio_build --scorpio-install-dir=~/scorpio_install --trace-log-dir=~/scorpio_build --verbose
----------------------------------------------------------------------------

The above script (spio_replay.py) generates some template source files and builds the scorpio replay tool, spio_replay.exe .

Running the replay program
---------------------------
* Run the replay tool, spio_replay.exe, using the same number of MPI processes 

