#!/usr/bin/env python3

"""
Classes to capture the details of an I/O system/MPI comm, get instructions from trace log
associated with the iosys & "run" instructions on it
"""
import os, sys, logging
import glob, re
from enum import Enum

import spio_trace_log_transform

logger = logging.getLogger(__name__)

class SPIOIOSys:
    """
    Class to capture details of the I/O system
    """
    def __init__(self, iosysid, trace_log_file, iosys_src_file, iosys_hdr_file, trace_mdata):
        self.iosysid = iosysid
        self.trace_mdata = trace_mdata
        self.comm_map_color = re.split(r"\D+", trace_mdata.comm_map_color)
        self.trace_log_file = trace_log_file
        self.iosys_src_file = iosys_src_file
        self.iosys_hdr_file = iosys_hdr_file
        self.log_to_src_transformer = spio_trace_log_transform.SPIOIOSysTraceLogToSrcTransformer(trace_mdata)
        self.exec_engine = SPIOIOSysExecEngine(self)
        self.instruction_stream = SPIOIOSysInstructionStream(trace_log_file, self.log_to_src_transformer)
        self.run_phase = 0

    def is_dependent(self, spio_iosys):
        if len(spio_iosys.comm_map_color) != len(self.comm_map_color):
            raise RuntimeError("Error comparing I/O systems. The two I/O systems (len(self.comm_map_key) = {} is not equal to len(other.comm_map_key) = {}) don't belong to the same parent communicator (MPI_COMM_WORLD)".format(len(self.comm_map_color), len(spio_iosys.comm_map_color)))
        for i in range(len(spio_iosys.comm_map_color)):
            if (int(spio_iosys.comm_map_color[i]) >= 0) and (int(self.comm_map_color[i]) >= 0):
                return True

        return False

"""
I/O System Execution Engine states:
INVALID : Invalid execution state
INIT : Exec engine is initializing (on the I/O system)
RUN : Exec engine is running (I/O system is "running" instructions)
QUIESCENT : Exec engine is temporarily in the quiescent state (I/O system is not "running" any instr)
FINALIZE : Exec engine is finalizing
"""
class SPIOIOSysExecEngineState(Enum):
    INVALID = 0
    INIT = 1
    READY = 2
    RUN = 3
    QUIESCENT = 4
    FINALIZE = 5

def iosys_default_state_transition_cb(spio_iosys):
    None

def iosys_ready_to_run_cb(spio_iosys):
    # Start writing the run phase method
    spio_iosys.iosys_src_file.write(spio_iosys.log_to_src_transformer.transform(spio_iosys.log_to_src_transformer.get_iosys_run_method_prefix()))
    spio_iosys.log_to_src_transformer.append_transform_tok("__IOSYS_RUN_FUNC_DECLS__", spio_iosys.log_to_src_transformer.transform(spio_iosys.log_to_src_transformer.get_iosys_run_method_decl()))

def iosys_run_to_quiescent_cb(spio_iosys):
    # End this run phase
    spio_iosys.iosys_src_file.write(spio_iosys.log_to_src_transformer.transform(spio_iosys.log_to_src_transformer.get_iosys_run_method_suffix()))
    spio_iosys.run_phase += 1
    spio_iosys.log_to_src_transformer.add_transform_tok("__PHASE__", str(spio_iosys.run_phase))

def iosys_quiescent_to_run_cb(spio_iosys):
    # FIXME: Do we need a quiescent to run cb?
    spio_iosys.iosys_src_file.write(spio_iosys.log_to_src_transformer.transform(spio_iosys.log_to_src_transformer.get_iosys_run_method_prefix()))

def iosys_run_to_finalize_cb(spio_iosys):
    spio_iosys.iosys_src_file.write(spio_iosys.log_to_src_transformer.transform(spio_iosys.log_to_src_transformer.get_iosys_run_method_suffix()))

def iosys_ready_to_finalize_cb(spio_iosys):
    iosys_run_to_finalize_cb(spio_iosys)

class SPIOIOSysExecEngine:
    """
    Allows virtually executing instructions on an I/O system
    """
    def __init__(self, my_iosys):
        self.my_iosys = my_iosys
        self._my_iosys_idx = -1
        self._iosys_deps = []
        self.state = SPIOIOSysExecEngineState.INVALID
        self._is_init = False

        # Set up the state transition callback matrix
        # Since function objects cannot be assigned to we have to do init this way (rather than assign defaults & set the needed funcs later)
        self.transition_cb = []
        for i in range(len(SPIOIOSysExecEngineState)):
            trans_cb_mat_row = []
            for j in range(len(SPIOIOSysExecEngineState)):
                if ((i == SPIOIOSysExecEngineState.READY.value) and (j == SPIOIOSysExecEngineState.RUN.value)): 
                    trans_cb_mat_row.append(iosys_ready_to_run_cb)
                elif ((i == SPIOIOSysExecEngineState.RUN.value) and (j == SPIOIOSysExecEngineState.QUIESCENT.value)):
                    trans_cb_mat_row.append(iosys_run_to_quiescent_cb)
                elif ((i == SPIOIOSysExecEngineState.QUIESCENT.value) and (j == SPIOIOSysExecEngineState.RUN.value)):
                    trans_cb_mat_row.append(iosys_quiescent_to_run_cb)
                elif ((i == SPIOIOSysExecEngineState.RUN.value) and (j == SPIOIOSysExecEngineState.FINALIZE.value)):
                    trans_cb_mat_row.append(iosys_run_to_finalize_cb)
                elif ((i == SPIOIOSysExecEngineState.READY.value) and (j == SPIOIOSysExecEngineState.FINALIZE.value)):
                    trans_cb_mat_row.append(iosys_ready_to_finalize_cb)
                else:
                    trans_cb_mat_row.append(iosys_default_state_transition_cb)

            self.transition_cb.append(trans_cb_mat_row)


    def init(self, iosys):
        # Find and save index to my I/O system
        for i in range(len(iosys)):
            if self.my_iosys.iosysid == iosys[i].iosysid:
                self._my_iosys_idx = i

        # Find and save dependencies between I/O systems
        for i in range(len(iosys)):
            self._iosys_deps.append([])
        for i in range(len(iosys)):
            for j in range(i+1, len(iosys)):
                if(iosys[i].is_dependent(iosys[j])):
                    self._iosys_deps[i].append(j)
                    self._iosys_deps[j].append(i)

        self._is_init = True

    def set_state(self, exec_engine_state):
        if not self._is_init:
            raise RuntimeError("I/O system execution engine needs to be initialized")

        if self.state != SPIOIOSysExecEngineState.FINALIZE:
            old_state = self.state
            self.state = exec_engine_state
            logger.debug("set_state({} -> {}), calling {}".format(old_state, self.state, self.transition_cb[old_state.value][self.state.value]))
            self.transition_cb[old_state.value][self.state.value](self.my_iosys)

    def is_ready(self, iosys):
        if not self._is_init:
            raise RuntimeError("I/O system execution engine needs to be initialized")

        if ((self.state == SPIOIOSysExecEngineState.INVALID) or
            (self.state == SPIOIOSysExecEngineState.FINALIZE)):
            return False

        # Check that no dependent I/O systems are currently running
        for iosys_dep in self._iosys_deps[self._my_iosys_idx]:
            if iosys[iosys_dep].exec_engine.state == SPIOIOSysExecEngineState.RUN:
                return False

        # Move back all dependent I/O systems in READY state to INIT
        for iosys_dep in self._iosys_deps[self._my_iosys_idx]:
            if iosys[iosys_dep].exec_engine.state == SPIOIOSysExecEngineState.READY:
                iosys[iosys_dep].exec_engine.set_state(SPIOIOSysExecEngineState.INIT)

        return True

    def execute(self, instr):
        if not self._is_init:
            raise RuntimeError("I/O system execution engine needs to be initialized")

        if self.state == SPIOIOSysExecEngineState.FINALIZE:
            raise RuntimeError("Trying to run instruction on an I/O system execution engine in the FINALIZE state")

        logger.debug("Executing instruction (on iosysid={}) : {}".format(self.my_iosys.iosysid, instr))
        self.my_iosys.iosys_src_file.write(self.my_iosys.log_to_src_transformer.transform(instr))
        self.my_iosys.instruction_stream.reset()

class SPIOIOSysInstructionStream:
    """
    Instruction stream (read from trace logs) associated with an I/O system
    """
    def __init__(self, trace_log_file, log_to_src_transformer):
        self.trace_log_file = trace_log_file
        self.log_to_src_transformer = log_to_src_transformer
        self._inst_cached = False
        self._func_enter_tstamp = 0.0
        self._func_exit_tstamp = 0.0
        self._func_instr = None

    def fetch(self):
        if self._inst_cached:
            return self._func_enter_tstamp, self._func_exit_tstamp, self._func_instr

        while True:
            func_enter_log = self.trace_log_file.readline()
            if not func_enter_log:
                # All function calls have been fetched
                return None
            if func_enter_log.strip():
                break

        # The trace logs end with a "====================================" string
        if re.match(r"^[=]+$", func_enter_log):
            return None

        # e.g: Function enter log : "1:2914.416464:\PIOc_Set_IOSystem_Error_Handling(iosysid=2048,method=-52)"
        toks = re.split(r":", func_enter_log)
        if len(toks) != 3:
            logger.error("Error parsing the trace log file, Unexpected format for function enter log :\"{}\")".format(func_enter_log))
            raise RuntimeError("Corrupted trace log. error parsing the trace log file. Unexpected format for function enter log:\"{}\"".format(func_enter_log))

        self._func_enter_tstamp = toks[1]
        self._func_instr = toks[2]

        func_exit_log = self.trace_log_file.readline()
        if not func_exit_log:
            logger.error("Error parsing the trace log file, missing function exit log (corresponding to function enter log : \"{}\")".format(func_enter_log))
            raise RuntimeError("Corrupted trace log. error parsing the trace log file. Missing function exit log (corresponding to function enter log : \"{}\")".format(func_enter_log))

        # e.g. Function exit log : "1:2914.416487:/PIOc_Set_IOSystem_Error_Handling()"
        toks = re.split(r":", func_exit_log)
        if len(toks) != 3:
            logger.error("Error parsing the trace log file, Unexpected format for function exit log :\"{}\")".format(func_exit_log))
            raise RuntimeError("Corrupted trace log. error parsing the trace log file. Unexpected format for function exit log:\"{}\"".format(func_exit_log))

        self._func_exit_tstamp = toks[1]

        return self._func_enter_tstamp, self._func_exit_tstamp, self._func_instr

    def decode(self):
        if not self._inst_cached:
            self._func_instr = "/*" + self._func_instr + "*/"
            self._inst_cached = True

        return self._func_enter_tstamp, self._func_exit_tstamp, self._func_instr

    def reset(self):
        self._inst_cached = False
