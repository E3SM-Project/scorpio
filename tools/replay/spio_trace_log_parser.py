#!/usr/bin/env python3

"""
Class used to parse the SCORPIO I/O trace logs

I/O trace logs : spio_trace_log*.log : contains the trace of all SCORPIO APIs
I/O trace meta-data logs : spio_trace_mdata*.log : contains the meta-data for the trace logs
I/O Decomposition logs : spio_trace_decomp*.nc : contains the I/O decompositions used by the app

Note: All trace logs in a directory are assumed to belong to a single run of the application.

"""

import os, sys, logging
import glob, re

import spio_trace_mdata_parser

logger = logging.getLogger(__name__)

class SPIOTraceLogHeaderInfo:
    """
    Stores the header info from a SCORPIO Trace log file
    """
    def __init__(self, ver, iosysid, wrank, trace_mdata_fname, trace_decomp_fname):
        self.ver = ver.strip()
        self.iosysid = iosysid
        self.wrank = wrank
        self.trace_mdata_fname = trace_mdata_fname.strip()
        self.trace_decomp_fname = trace_decomp_fname.strip()

    def __str__(self):
        return "SPIOTraceLogHeaderInfo(" + \
                "ver =\"{}\",".format(self.ver) + \
                " iosysid = {},".format(self.iosysid) + \
                " wrank = {},".format(self.wrank) + \
                " trace_mdata_fname =\"{}\",".format(self.trace_mdata_fname) + \
                " trace_decomp_fname =\"{}\"".format(self.trace_decom_fname) + ")"

class SPIOTraceLogParser:
    """
    Class used to parse the SCORPIO I/O trace logs

    """
    def __init__(self, spio_src_dir, spio_install_dir, spio_trace_dir, spio_replay_tool_src_dir):
        self.spio_src_dir = spio_src_dir.strip()
        self.spio_install_dir = spio_install_dir.strip()
        self.spio_trace_dir = spio_trace_dir.strip()
        self.spio_replay_tool_src_dir = spio_replay_tool_src_dir.strip()

    def _parse_log_header(self, fname, file):
        """
        Reads the I/O trace log header. The file needs to be opened and passed as an arg to
        this function

        =================================================================
          SCORPIO TRACE LOG
        SCORPIO VERSION : 1.6.3
        I/O System ID : 2048
        MPI World rank : 0
        Trace Mdata file : spio_trace_mdata__iosys_2048_4131009_0.log
        I/O Decomp file : spio_trace_decomp__iosys_2048.nc
        =================================================================
        """
        logger.info("Reading header of I/O trace file : {}".format(fname))

        # Reading "================================================================="
        line = file.readline()
        if (not line) or (not re.match(r"^[=]+$", line)):
            logger.error("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))

        # Reading "  SCORPIO TRACE LOG"
        line = file.readline()
        if (not line) or (not re.match(r"^[ \t]+SCORPIO TRACE LOG[ \t]*$", line)):
            logger.error("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))

        # Reading "SCORPIO VERSION : 1.6.3"
        line = file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace file \"{}\", missing SCORPIO version, corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace file \"{}\", line = \"{}\", corrupted I/O trace file".format(fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        ver = toks[1].strip()

        # Reading "I/O System ID : 2048"
        line = file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace file \"{}\", missing I/O system id, corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace file \"{}\", line = \"{}\", corrupted I/O trace file".format(fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        iosysid = int(toks[1].strip())

        # Reading "MPI World rank : 0"
        line = file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace file \"{}\", missing world rank, corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace file \"{}\", line = \"{}\", corrupted I/O trace file".format(fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        wrank = int(toks[1].strip())

        # Reading "Trace Mdata file : spio_trace_mdata__iosys_2048_4131009_0.log"
        line = file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace file \"{}\", missing trace meta-data filename, corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace file \"{}\", line = \"{}\", corrupted I/O trace file".format(fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        trace_mdata_fname = toks[1].strip()

        # Reading "I/O Decomp file : spio_trace_decomp__iosys_2048.nc"
        line = file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace file \"{}\", missing I/O decomposition filename, corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace file \"{}\", line = \"{}\", corrupted I/O trace file".format(fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
        trace_decomp_fname = toks[1].strip()

        # Reading "================================================================="
        line = file.readline()
        if (not line) or (not re.match(r"^[=]+$", line)):
            logger.error("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))
            raise RuntimeError("Unable to parse the header of I/O trace file \"{}\", corrupted I/O trace file".format(fname))

        logger.debug(  "Finished reading header file :\n" +\
                      "\tSCORPIO VERSION = {}\n".format(ver) +\
                      "\tI/O System ID = {}\n".format(iosysid) +\
                      "\tMPI World rank = {}\n".format(wrank) +\
                      "\tTrace Meta-data file = {}\n".format(trace_mdata_fname) +\
                      "\tI/O Decomp file = {}\n".format(trace_decomp_fname) ) 

        return SPIOTraceLogHeaderInfo(ver, iosysid, wrank, trace_mdata_fname, trace_decomp_fname)

    def _parse_log(self, fname, file):
        """
        Parses the I/O trace log. The log file needs to be opened and the header needs to be
        processed using _parse_log_header() before passing the file handle to this function
        """

    def parse_log_and_generate_tool(self):
        """
        Parses the spio_trace_dir to look for I/O trace and I/O trace meta-data logs and generates
        the replay tool sources from source templates
        """
        logger.info("Parsing trace logs in \"{}\"".format(self.spio_trace_dir))
        trace_log_regex = "spio_trace_log_*.log"
        # FIXME: Do we need to recursively search the user trace log dir
        trace_logs = glob.glob("{}/{}".format(self.spio_trace_dir,trace_log_regex), recursive=False)
        logger.info("Found {} trace logs in \"{}\"".format(len(trace_logs), self.spio_trace_dir))
        for trace_log in trace_logs:
            trace_log_file = open(trace_log, "r")
            hdr_info = self._parse_log_header(trace_log, trace_log_file)
            mdata_fname = "{}/{}".format(self.spio_trace_dir, hdr_info.trace_mdata_fname)
            mdata_parser = spio_trace_mdata_parser.SPIOTraceMDataParser(mdata_fname)
            trace_mdata = mdata_parser.get_mdata()
            print("DBG: {}".format(trace_mdata))
            self._parse_log(trace_log, trace_log_file)
