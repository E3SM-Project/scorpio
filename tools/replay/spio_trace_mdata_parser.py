#!/usr/bin/env python3

"""
Class used to parse the SCORPIO I/O trace meta-data logs

I/O trace logs : spio_trace_log*.log : contains the trace of all SCORPIO APIs
I/O trace meta-data logs : spio_trace_mdata*.log : contains the meta-data for the trace logs
I/O Decomposition logs : spio_trace_decomp*.nc : contains the I/O decompositions used by the app

Note: All trace logs in a directory are assumed to belong to a single run of the application.

"""

import os, sys, logging
import glob, re

logger = logging.getLogger(__name__)

class SPIOTraceMData:
    """
    The SCORPIO I/O trace meta-data read from trace meta-data log
    """
    def __init__(self, ver=None, iosysid=-1,
                  wrank=-1, wsz=0, comm_rank=-1, comm_sz=0,
                  comm_map_color=None, comm_map_key=None,
                  nioids=0, nncids=0, ndimids=0, nvarids=0,
                  trace_log_fname=None, trace_decomp_fname=None):
        self.ver = ver
        self.iosysid = iosysid
        self.wrank = wrank
        self.wsz = wsz
        self.comm_rank = comm_rank
        self.comm_sz = comm_sz
        self.comm_map_color = comm_map_color
        self.comm_map_key = comm_map_key
        self.nioids = nioids
        self.nncids = nncids
        self.ndimids = ndimids
        self.nvarids = nvarids
        self.trace_log_fname = trace_log_fname
        self.trace_decomp_fname = trace_decomp_fname

    def __str__(self):
        return "SPIOTraceMData(" + \
                "ver=\"{}\",".format(self.ver) + \
                " iosysid={},".format(self.iosysid) + \
                " wrank={},".format(self.wrank) + \
                " wsz={},".format(self.wsz) + \
                " comm_rank={},".format(self.comm_rank) + \
                " comm_sz={},".format(self.comm_sz) + \
                " comm_map_color=\"{}\",".format(self.comm_map_color) + \
                " comm_map_key=\"{}\",".format(self.comm_map_key) + \
                " nioids={},".format(self.nioids) + \
                " nncids={},".format(self.nncids) + \
                " nvarids={},".format(self.nvarids) + \
                " trace_log_fname=\"{}\",".format(self.trace_log_fname) + \
                " trace_decomp_fname=\"{}\"".format(self.trace_decomp_fname) + ")"

    """
    Init comm info from a string like "[COMM_RANK/COMM_SIZE]" e.g. "[0/3]"
    """
    def set_wcomm_info(self, comm_info_str):
        self.wrank, self.wsz = re.findall(r"\d+", comm_info_str.strip())

    """
    Init comm info from a string like "[COMM_RANK/COMM_SIZE]" e.g. "[0/3]"
    """
    def set_comm_info(self, comm_info_str):
        self.comm_rank, self.comm_sz = re.findall(r"\d+", comm_info_str.strip())

    """
    Set comm map color to a string, ignoring all other info (like brackets)
    e.g. "[0,1,2]" is stored as "0,1,2"
    """
    def set_comm_map_color(self, comm_map_color):
        self.comm_map_color = re.match(r"^[ \t]*\[([0-9,]+)\][ \t]*$", comm_map_color.strip()).group(1)

    """
    Set comm map key to a string, ignoring all other info (like brackets)
    e.g. "[0,1,2]" is stored as "0,1,2"
    """
    def set_comm_map_key(self, comm_map_key):
        self.comm_map_key = re.match(r"^[ \t]*\[([0-9,]+)\][ \t]*$", comm_map_key.strip()).group(1)

    def set_iosys_info(self, iosys_info):
        None

class SPIOTraceMDataParser:
    """
    Class used to parse the SCORPIO I/O trace meta-data logs

    """
    def __init__(self, trace_mdata_log_fname):
        self.trace_mdata_log_fname = trace_mdata_log_fname
        self._is_header_parsed = False
        self._is_mdata_parsed = False
        self._trace_mdata = SPIOTraceMData()
        self._iosys_info = None

    def _parse_log_header(self):
        """
        Reads the I/O trace meta-data log header.

        =================================================================
          SCORPIO TRACE META DATA
        SCORPIO VERSION : 1.6.3
        I/O System ID : 2048
        MPI World rank : 0
        Trace log file : spio_trace_log__iosys_2048_4131009_0.log
        I/O Decomp file : spio_trace_decomp__iosys_2048.nc
        =================================================================
        """
        self._trace_mdata_log_file = open(self.trace_mdata_log_fname, "r")
        logger.info("Reading header of I/O trace file : {}".format(self.trace_mdata_log_fname))

        # Reading "================================================================="
        line = self._trace_mdata_log_file.readline()
        if (not line) or (not re.match(r"^[=]+$", line)):
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))

        # Reading "  SCORPIO TRACE META DATA"
        line = self._trace_mdata_log_file.readline()
        if (not line) or (not re.match(r"^[ \t]+SCORPIO TRACE META DATA[ \t]*$", line)):
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))

        # Reading "SCORPIO VERSION : 1.6.3"
        line = self._trace_mdata_log_file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", missing SCORPIO version, corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", line = \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        self._trace_mdata.ver = toks[1].strip()

        # Reading "I/O System ID : 2048"
        line = self._trace_mdata_log_file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", missing I/O system id, corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", line = \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        self._trace_mdata.iosysid = int(toks[1].strip())

        # Reading "MPI World rank : 0"
        line = self._trace_mdata_log_file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", missing world rank, corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", line = \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        self._trace_mdata.wrank = int(toks[1].strip())

        # Reading "Trace log file : spio_trace_log__iosys_2048_4131009_0.log"
        line = self._trace_mdata_log_file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", missing trace log filename, corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", line = \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        self._trace_mdata.trace_log_fname = toks[1].strip()

        # Reading "I/O Decomp file : spio_trace_decomp__iosys_2048.nc"
        line = self._trace_mdata_log_file.readline()
        if not line:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", missing I/O decomposition filename, corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        toks = re.split(r":", line)
        if len(toks) != 2:
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", line = \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname, line))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
        self._trace_mdata.trace_decomp_fname = toks[1].strip()

        # Reading "================================================================="
        line = self._trace_mdata_log_file.readline()
        if (not line) or (not re.match(r"^[=]+$", line)):
            logger.error("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))
            raise RuntimeError("Unable to parse the header of I/O trace meta-data file \"{}\", corrupted I/O trace meta-data file".format(self.trace_mdata_log_fname))

        logger.debug(  "Finished reading header of the I/O trace meta-data file :\n" +\
                      "\tSCORPIO VERSION = {}\n".format(self._trace_mdata.ver) +\
                      "\tI/O System ID = {}\n".format(self._trace_mdata.iosysid) +\
                      "\tMPI World rank = {}\n".format(self._trace_mdata.wrank) +\
                      "\tTrace log file = {}\n".format(self._trace_mdata.trace_log_fname) +\
                      "\tI/O Decomp file = {}\n".format(self._trace_mdata.trace_decomp_fname) ) 

        self._is_header_parsed = True

    def _parse_mdata(self):
        """
        Parses the meta-data (past the header) in the I/O trace meta-data file
        """
        if self._is_mdata_parsed:
            return

        if not self._is_header_parsed:
            logger.info("Parsing trace meta-data log file :\"{}\"".format(self.trace_mdata_log_fname))
            self._parse_log_header()

        mdata_key_parsers = {
            "MPI_COMM_WORLD": (lambda self, val: self._trace_mdata.set_wcomm_info(val)),
            "MPI_COMM": (lambda self, val: self._trace_mdata.set_comm_info(val)),
            "MPI_PROC_MAP": (lambda self, val: None),
            "MPI_PROC_MAP COLOR": (lambda self, val: self._trace_mdata.set_comm_map_color(val)),
            "MPI_PROC_MAP KEY": (lambda self, val: self._trace_mdata.set_comm_map_key(val)),
            "I/O System Info": (lambda self, val: self._trace_mdata.set_iosys_info(val))
        }

        line = self._trace_mdata_log_file.readline()
        while line:
            if re.match(r"^[=]+$", line):
                break
            toks = re.split(r":", line)
            if toks[0].strip():
                mdata_key_parsers[toks[0].strip()](self, toks[1].strip())
            line = self._trace_mdata_log_file.readline()

        logger.info("Trace Meta-data: {}".format(self._trace_mdata))
        self._is_mdata_parsed = True
        self._trace_mdata_log_file.close()

    # FIXME: Direct initialization is not currently supported
    def _initialize(self):
        """
        Initializes the meta-data log parser by reading the meta-data log header
        """
        if(not self._is_header_parsed):
            logger.info("Parsing trace meta-data log file :\"{}\"".format(self.trace_mdata_log_fname))
            self._parse_log_header()

    def get_mdata(self):
        """
        Get the meta-data from the trace meta-data file
        """
        if self._is_mdata_parsed:
            return self._trace_mdata

        if not self._is_header_parsed:
            logger.info("Parsing trace meta-data log file :\"{}\"".format(self.trace_mdata_log_fname))
            self._parse_log_header()

        self._parse_mdata()
        return self._trace_mdata
