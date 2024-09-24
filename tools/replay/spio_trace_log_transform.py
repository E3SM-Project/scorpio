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

class SPIOLogToSrcTransformer:
    """
    SCORPIO I/O trace log to source transformer
    """
    def __init__(self): 
        self._is_initialized = False
        self._trace_mdata_rep_toks = []
        self._rep_toks = {}

    def initialize(self):
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
        if(not self._is_initialized):
            self.initialize()

        self._rep_toks[tok] = val

    def append_transform_tok(self, tok, aval):
        if(not self._is_initialized):
            self.initialize()

        self._rep_toks[tok] += aval

class SPIODriverLogToSrcTransformer(SPIOLogToSrcTransformer):
    """
    SCORPIO I/O trace log to source transformer for I/O systems
    """
    def __init__(self): 
        SPIOLogToSrcTransformer.__init__(self)

    def initialize(self):
        SPIOLogToSrcTransformer.initialize(self)
        self._rep_toks["__IOSYS_HEADER_INCLUDES__"] = "\n"
        self._rep_toks["__DRIVER_RUN_SEQUENCE__"] = "\n"
        self._is_initialized = True

class SPIOIOSysTraceLogToSrcTransformer(SPIOLogToSrcTransformer):
    """
    SCORPIO I/O trace log to source transformer for I/O systems
    """
    def __init__(self, trace_mdata): 
        SPIOLogToSrcTransformer.__init__(self)
        self._trace_mdata = trace_mdata

    def initialize(self):
        SPIOLogToSrcTransformer.initialize(self)
        self._trace_mdata_rep_toks.append(("__IOSYSID__", str(self._trace_mdata.iosysid)))
        self._trace_mdata_rep_toks.append(("__MPI_PROC_MAP_COLORS__", self._trace_mdata.comm_map_color))
        self._trace_mdata_rep_toks.append(("__MPI_PROC_MAP_KEYS__", self._trace_mdata.comm_map_key))

        #self._trace_mdata_rep_toks.append(("__IOSYS_INIT_FUNC_DECLS__", ""))
        #self._trace_mdata_rep_toks.append(("__IOSYS_FINALIZE_FUNC_DECLS__", ""))
        self._rep_toks["__IOSYS_RUN_FUNC_DECLS__"] = ""
        self._rep_toks["__IOSYS_RUN_PHASE_FUNCS__"] = ""

        self._rep_toks["__PHASE__"] = str(0)

        self._is_initialized = True

    def get_iosys_init_method_name(self):
        return "iosys_init_{}".format("__IOSYSID__")

    def get_iosys_init_method_decl(self):
        return "\nint iosys_init_{}(void );\n".format("__IOSYSID__")

    def get_iosys_finalize_method_name(self):
        return "iosys_finalize_{}".format("__IOSYSID__")

    def get_iosys_finalizxe_method_decl(self):
        return "\nint iosys_finalize_{}(void );\n".format("__IOSYSID__")

    def get_iosys_run_method_name(self):
        return "iosys_run_{}_phase{}".format("__IOSYSID__", "__PHASE__")

    def get_iosys_run_method_decl(self):
        return "\nint iosys_run_{}_phase{}(void );".format("__IOSYSID__", "__PHASE__")

    def get_iosys_run_method_prefix(self):
        return "\nint iosys_run_{}_phase{}(void )\n{{\n".format("__IOSYSID__", "__PHASE__")

    def get_iosys_run_method_suffix(self):
        return "\nreturn 0;\n}"

    def get_iosys_run_method_with_phase_name(self):
        return "iosys_run_{}".format("__IOSYSID__")

    def get_iosys_run_method_with_phase(self):
        return \
"""
int {}(int phase){{
  static const std::vector<std::function<void (void)> > run_phases = {{ {} }};
  assert((phase >= 0) && (phase < run_phases.size()));
  if(gvars___IOSYSID__::is_proc_in_iosys){{
    return run_phases[phase]();
  }}
  return 0;
}}
""".format(self.get_iosys_run_method_with_phase_name(), "__IOSYS_RUN_PHASE_FUNCS__")

    def get_include_decl(self, fname):
        return "#include \"{}\"\n".format(fname)
