#!/usr/bin/env python3

"""
Class used for building the SPIO Trace replay tool.

The SPIOTraceLogParser class is used to parse the SCORPIO I/O trace logs
and generate the replay tool source. This class is used for building this
source using compiler settings used for building the SCORPIO library

"""

import os, sys, logging

logger = logging.getLogger(__name__)

class SPIOReplayToolBuilder:
    """
    Class used to build the SCORPIO replay tool, spio_replay.exe

    """
    def __init__(self, spio_src_dir, spio_install_dir, spio_trace_dir,
                  spio_replay_tool_src_dir, spio_replay_tool_build_dir):
        self.spio_src_dir = spio_src_dir
        self.spio_install_dir = spio_install_dir
        self.spio_trace_dir = spio_trace_dir
        self.spio_replay_tool_src_dir = spio_replay_tool_src_dir
        self.spio_replay_tool_build_dir = spio_replay_tool_build_dir

    def build(self):
        """
        Builds the replay tool
        """
        ret = 0
        logger.info("Parsing trace logs in \"{}\"".format(self.spio_trace_dir))

        return ret
