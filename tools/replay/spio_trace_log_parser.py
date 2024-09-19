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
import spio_trace_log_transform
import spio_vm
import spio_iosys

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
        self.spio_src_dir = os.path.expanduser(spio_src_dir.strip())
        self.spio_install_dir = os.path.expanduser(spio_install_dir.strip())
        self.spio_trace_dir = os.path.expanduser(spio_trace_dir.strip())
        self.spio_replay_tool_src_dir = os.path.expanduser(spio_replay_tool_src_dir.strip())

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

    # FIXME: Do we need this function to be part of the parser?
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

        # Read the template source file contents
        iosys_src_template_fname = "{}/{}".format(self.spio_src_dir, "tools/replay/src_templates/iosys_trace_template.cpp")
        iosys_src_template_file = open(iosys_src_template_fname, "r")
        iosys_src_template = iosys_src_template_file.read()
        iosys_src_template_file.close()

        # Read the template header file contents
        iosys_hdr_template_fname = "{}/{}".format(self.spio_src_dir, "tools/replay/src_templates/iosys_trace_template.hpp")
        iosys_hdr_template_file = open(iosys_hdr_template_fname, "r")
        iosys_hdr_template = iosys_hdr_template_file.read()
        iosys_hdr_template_file.close()

        # Driver source template transformer
        driver_log_to_src_transformer = spio_trace_log_transform.SPIODriverLogToSrcTransformer()

        iosys = []
        iosys_src_files = []
        iosys_hdr_files = []
        trace_log_files = []
        for trace_log in trace_logs:
            # Open trace log and read header
            trace_log_file = open(trace_log, "r")
            hdr_info = self._parse_log_header(trace_log, trace_log_file)
            mdata_fname = "{}/{}".format(self.spio_trace_dir, hdr_info.trace_mdata_fname)
            mdata_parser = spio_trace_mdata_parser.SPIOTraceMDataParser(mdata_fname)
            trace_mdata = mdata_parser.get_mdata()

            # Create log2src transformer and use it to create the src file (the initial contents)
            # from the soure templates
            log_to_src_transformer = spio_trace_log_transform.SPIOIOSysTraceLogToSrcTransformer(trace_mdata)
            iosys_src_fname = "{}/{}_{}.cpp".format(self.spio_replay_tool_src_dir, "spio_replay_iosys_trace", hdr_info.iosysid)
            iosys_hdr_bname = "{}_{}.hpp".format("spio_replay_iosys_trace", hdr_info.iosysid)
            iosys_hdr_fname = "{}/{}".format(self.spio_replay_tool_src_dir, iosys_hdr_bname)
            driver_log_to_src_transformer.append_transform_tok("__IOSYS_HEADER_INCLUDES__", log_to_src_transformer.get_include_decl(iosys_hdr_bname))

            # Open output cpp source file & hpp header file
            iosys_src_file = open(iosys_src_fname, "w")
            iosys_hdr_file = open(iosys_hdr_fname, "w")

            # Transform the source template and write to output cpp source file
            iosys_src_file.write(log_to_src_transformer.transform(iosys_src_template))

            iosys_src_files.append(iosys_src_file)
            iosys_hdr_files.append(iosys_hdr_file)
            trace_log_files.append(trace_log_file)

            iosys.append(spio_iosys.SPIOIOSys(hdr_info.iosysid, trace_log_file, iosys_src_file, iosys_hdr_file, trace_mdata))

        # Create a Virtual Machine to "execute" the instructions
        vm = spio_vm.SPIOVM(iosys)
        vm.execute()

        # Read the driver source & header file templates and create the driver sources
        # Create the driver source
        driver_src_template_fname = "{}/{}".format(self.spio_src_dir, "tools/replay/src_templates/driver_template.cpp")
        driver_src_template_file = open(driver_src_template_fname, "r")
        driver_src_template = driver_src_template_file.read()
        driver_src_template_file.close()

        driver_src_fname = "{}/{}.cpp".format(self.spio_replay_tool_src_dir, "spio_replay_driver")
        driver_src_file = open(driver_src_fname, "w")
        driver_src_file.write(driver_log_to_src_transformer.transform(driver_src_template))
        driver_src_file.close()

        # Create the driver header
        driver_hdr_template_fname = "{}/{}".format(self.spio_src_dir, "tools/replay/src_templates/driver_template.hpp")
        driver_hdr_template_file = open(driver_hdr_template_fname, "r")
        driver_hdr_template = driver_hdr_template_file.read()
        driver_hdr_template_file.close()

        driver_hdr_fname = "{}/{}.hpp".format(self.spio_replay_tool_src_dir, "spio_replay_driver")
        driver_hdr_file = open(driver_hdr_fname, "w")
        driver_hdr_file.write(driver_log_to_src_transformer.transform(driver_hdr_template))
        driver_hdr_file.close()

        # Write the I/O system header files
        for sys in iosys:
            sys.iosys_hdr_file.write(sys.log_to_src_transformer.transform(iosys_hdr_template))

        # Close source/header files
        for iosys_src_file in iosys_src_files:
            iosys_src_file.close()

        for iosys_hdr_file in iosys_hdr_files:
            iosys_hdr_file.close()

        for trace_log_file in trace_log_files:
            trace_log_file.close()
