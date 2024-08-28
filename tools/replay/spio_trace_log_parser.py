#!/usr/bin/env python3

"""
Class used to parse the SCORPIO I/O trace logs

I/O trace logs : spio_trace_log*.log : contains the trace of all SCORPIO APIs
I/O trace meta-data logs : spio_trace_mdata*.log : contains the meta-data for the trace logs
I/O Decomposition logs : spio_trace_decomp*.nc : contains the I/O decompositions used by the app

Note: All trace logs in a directory are assumed to belong to a single run of the application.

"""

import os, sys, logging

logger = logging.getLogger(__name__)

class SPIOTraceLogParser:
    """
    Class used to parse the SCORPIO I/O trace logs

    """
    def __init__(self, spio_src_dir, spio_install_dir, spio_trace_dir, spio_replay_tool_src_dir):
        self.spio_src_dir = spio_src_dir
        self.spio_install_dir = spio_install_dir
        self.spio_trace_dir = spio_trace_dir
        self.spio_replay_tool_src_dir = spio_replay_tool_src_dir

    def parse_log_and_generate_tool(self):
        """
        Parses the spio_trace_dir to look for I/O trace and I/O trace meta-data logs and generates
        the replay tool sources from source templates
        """
        logger.info("Parsing trace logs in \"{}\"".format(self.spio_trace_dir))
