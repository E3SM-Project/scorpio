#!/usr/bin/env python3

"""
Class used for building the SPIO Trace replay tool.

The SPIOTraceLogParser class is used to parse the SCORPIO I/O trace logs
and generate the replay tool source. This class is used for building this
source using compiler settings used for building the SCORPIO library

"""

import os, sys, logging
import re, subprocess

logger = logging.getLogger(__name__)

class SPIOReplayToolBuilder:
    """
    Class used to build the SCORPIO replay tool, spio_replay.exe

    """
    def __init__(self, spio_src_dir, spio_build_dir, spio_install_dir, spio_trace_dir,
                  spio_replay_tool_src_dir, spio_replay_tool_build_dir):
        self.spio_src_dir = spio_src_dir
        self.spio_build_dir = spio_build_dir
        self.spio_install_dir = spio_install_dir
        self.spio_trace_dir = spio_trace_dir
        self.spio_replay_tool_src_dir = spio_replay_tool_src_dir
        self.spio_replay_tool_build_dir = spio_replay_tool_build_dir

    def _is_scorpio_cmake_build_cache(self, scorpio_build_cache_file):
        # Verify that its a SCORPIO build CMake cache
        is_scorpio_build_cache = False
        with open(scorpio_build_cache_file, "r") as fh:
            line = fh.readline()
            while line:
                if re.match(r"^#CMAKE_PROJECT_NAME.+$", line):
                    toks = re.split("=", line)
                    if (len(toks) == 2) and (toks[1].strip() == "SCORPIO"):
                        is_scorpio_build_cache = True
                    break
                line = fh.readline()
        return is_scorpio_build_cache


    def build(self):
        """
        Builds the replay tool
        """
        banner = "===================================================="
        ret = 0
        logger.info("Building SCORPIO Replay tool: \n\
                    SCORPIO Source directory:\"{}\"\n\
                    SCORPIO Build directory:\"{}\"\n\
                    SCORPIO Install directory:\"{}\"\n\
                    SCORPIO Trace directory:\"{}\"\n\
                    SCORPIO Replay tool source directory:\"{}\"\n\
                    SCORPIO Replay tool Build directory:\"{}\"".\
                    format(self.spio_src_dir, self.spio_build_dir, self.spio_install_dir, self.spio_trace_dir,\
                        self.spio_replay_tool_src_dir, self.spio_replay_tool_build_dir))

        scorpio_build_cache_file = self.spio_build_dir + "/" + "SCORPIO_CMakeCache.txt"
        if os.path.exists(scorpio_build_cache_file):
            is_scorpio_build_cache = self._is_scorpio_cmake_build_cache(scorpio_build_cache_file)
            if not is_scorpio_build_cache:
                logger.error("Found CMake cache file, \"{}\", but it is not from a SCORPIO build".format(scorpio_build_cache_file))
                raise RuntimeError("Found CMake cache file, \"{}\", but it is not from a SCORPIO build".format(scorpio_build_cache_file))
        else:
            logger.error("Unable to find the SCORPIO Build CMake cache file in SCORPIO build directory (\"{}\")".format(self.spio_build_dir))
            raise RuntimeError("Unable to find the SCORPIO Build CMake cache file in SCORPIO build directory (\"{}\")".format(self.spio_build_dir))

        os.chdir(self.spio_replay_tool_build_dir)
        # Configure replay tool
        os.environ["SCORPIO_DIR"] = self.spio_install_dir
        cmake_cmd = "cmake -C " + scorpio_build_cache_file +\
                    " -S " + self.spio_replay_tool_src_dir + " -B " + self.spio_replay_tool_build_dir

        logger.info("CONFIGURE: Running cmake :\"{}\"".format(cmake_cmd))
        with open(self.spio_replay_tool_build_dir + "/configure_replay_tool.log", "w") as fh:
            cmake_ret = subprocess.run(cmake_cmd.split(), stdout=fh, stderr=fh)
            if cmake_ret.returncode == 0:
                logger.info("Configure SUCCESS...")
                logger.info(banner)
                logger.info(cmake_ret.stdout)
                logger.info(banner)
            else:
                logger.error("Configure FAILED... see configure_replay_tool.log for more information")
                logger.error(banner)
                logger.error(cmake_ret.stdout)
                logger.error(cmake_ret.stderr)
                logger.error(banner)
            ret = cmake_ret.returncode

        # Build replay tool
        if cmake_ret.returncode == 0:
            with open(self.spio_replay_tool_build_dir + "/make_replay_tool.log", "w") as fh:
                make_cmd = "make spio_replay.exe"
                logger.info("BUILD: Running make :\"{}\"".format(make_cmd))
                make_ret = subprocess.run(make_cmd.split(), stdout=fh, stderr=fh)
                if make_ret.returncode == 0:
                    logger.info("Build SUCCESS...")
                    logger.info(banner)
                    logger.info(make_ret.stdout)
                    logger.info(banner)
                else:
                    logger.error("Build FAILED... see make_replay_tool.log for more information")
                    logger.error(banner)
                    logger.error(make_ret.stdout)
                    logger.error(make_ret.stderr)
                    logger.error(banner)
                ret = make_ret.returncode

        return ret
