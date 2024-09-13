#!/usr/bin/env python3

"""
SCORPIO I/O trace log to source transformer

The SPIOTraceLogToSrcTransformer class uses the meta-data info from
the SCORPIO trace meta-data logs (read using the SPIOTraceMDataParser class)
to transform logs to C++ source lines
"""
import os, sys, logging
import glob, re

import spio_trace_mdata_parser

logger = logging.getLogger(__name__)

class SPIOTraceLogToSrcTransformer:
    """
    SCORPIO I/O trace log to source transformer
    """
    def __init__(self, trace_mdata): 
        self._trace_mdata = trace_mdata
        self._is_initialized = False
        self._trace_mdata_rep_toks = []
        self._rep_toks = {}

    def initialize(self):
        self._trace_mdata_rep_toks.append(("__IOSYSID__", str(self._trace_mdata.iosysid)))
        self._trace_mdata_rep_toks.append(("__MPI_PROC_MAP_COLORS__", self._trace_mdata.comm_map_color))
        self._trace_mdata_rep_toks.append(("__MPI_PROC_MAP_KEYS__", self._trace_mdata.comm_map_key))

        self._rep_toks["__PHASE__"] = str(0)

        self._is_initialized = True

    def transform(self, log):
        if(not self._is_initialized):
            self.initialize()

        for rep_tok in self._trace_mdata_rep_toks:
            log = re.sub(rep_tok[0], rep_tok[1], log)

        for tok, val in self._rep_toks.items():
            log = re.sub(tok, val, log)

        return log

    def add_transform_tok(self, tok, val):
        self._rep_toks[tok] = val

    def get_iosys_run_method_prefix(self):
        return "\nvoid iosys_run_{}_phase{}(void )\n{{\n".format("__IOSYSID__", "__PHASE__")

    def get_iosys_run_method_suffix(self):
        return "\n}"
