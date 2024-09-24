#!/usr/bin/env python3

"""
Virtual Machine used to "execute" instructions from the trace logs
"""
import os, sys, logging
import glob, re
import spio_iosys

logger = logging.getLogger(__name__)

class SPIOVM:
    """
    The Virtual machine stores and "executes" instructions from the I/O trace logs
    for the list of I/O systems passed to it
    """
    def __init__(self, iosys):
        self.iosys = iosys
        for sys in self.iosys:
            sys.exec_engine.init(self.iosys)
        self.iosys_instr_sequence = []
        self.iosys_run_sequence = []

    def execute(self):
        INF_QTIME = -1.0

        # Start execution by first fetching and decoding instructions from each log
        niosys = len(self.iosys)
        iosys_idx = list(range(len(self.iosys)))
        iosys_exec_tstamps = []
        iosys_exec_cur_instr = []
        iosys_exec_ninstr = [0] * len(self.iosys)
        iosys_exec_total_qtimes = [INF_QTIME] * len(self.iosys)
        clock_tic = 0

        for sys in self.iosys:
            logger.debug("Starting execution on iosys (iosysid={})".format(sys.iosysid))
            sys.exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.INIT, clock_tic)
            start_time, end_time, instr = 0.0, 0.0, ""
            if sys.instruction_stream.fetch():
                start_time, end_time, instr = sys.instruction_stream.decode()
            else:
                sys.exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.FINALIZE, clock_tic)
                niosys -= 1
            iosys_exec_tstamps.append((float(start_time), float(end_time)))
            iosys_exec_cur_instr.append(instr)

        iosys_sorted_idx = sorted(iosys_idx, key=lambda x: iosys_exec_tstamps[x][0], reverse=True)

        # Start running the 1st instruction on I/O systems that are ready
        for idx in iosys_sorted_idx:
            if self.iosys[idx].exec_engine.is_ready(self.iosys):
                self.iosys[idx].exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.READY, clock_tic)

        for idx in iosys_sorted_idx:
            if self.iosys[idx].exec_engine.state == spio_iosys.SPIOIOSysExecEngineState.READY:
                self.iosys[idx].exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.RUN, clock_tic)
                self.iosys_instr_sequence.append(idx)
                self.iosys_run_sequence.append(idx)
                self.iosys[idx].exec_engine.execute(iosys_exec_cur_instr[idx], clock_tic)
                clock_tic += 1
                iosys_exec_ninstr[idx] += 1

        # Execute the rest of the instructions
        while niosys > 0:
            iosys_exec_current_tstamps = []
            iosys_exec_cur_instr.clear()
            for sys in self.iosys:
                start_time, end_time, instr = 0.0, 0.0, ""
                if sys.exec_engine.state != spio_iosys.SPIOIOSysExecEngineState.FINALIZE:
                    if sys.instruction_stream.fetch():
                        start_time, end_time, instr = sys.instruction_stream.decode()
                    else:
                        sys.exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.FINALIZE, clock_tic)
                        niosys -= 1
                iosys_exec_current_tstamps.append((float(start_time), float(end_time)))
                iosys_exec_cur_instr.append(instr)

            if niosys == 0:
                break

            iosys_sorted_idx = sorted(iosys_idx, key=lambda x: iosys_exec_current_tstamps[x][0], reverse=True)

            # Check if any iosys needs to go to QUIESCENT state
            for idx in iosys_idx:
                if self.iosys[idx].exec_engine.state == spio_iosys.SPIOIOSysExecEngineState.RUN:
                    # Quiescent time : time between two instructions
                    qtime = iosys_exec_current_tstamps[idx][0] - iosys_exec_tstamps[idx][1]
                    if iosys_exec_total_qtimes[idx] == INF_QTIME:
                        iosys_exec_total_qtimes[idx] = qtime
                    else:
                        iosys_exec_total_qtimes[idx] += qtime

                    # Move to QUIESCENT state if current qtime > 10 x current_average_qtime
                    nqtimes = iosys_exec_ninstr[idx] - 1
                    if (nqtimes > 0) and (qtime > iosys_exec_total_qtimes[idx]/nqtimes * 10):
                        #print("DBG: Moving iosys (iosysid={}) to QUIESCENT (qtime = {}, avg_qtime={})".format(self.iosys[idx].iosysid, qtime, iosys_exec_total_qtimes[idx]/nqtimes * 10))
                        sys.exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.QUIESCENT, clock_tic)

            iosys_exec_tstamps = iosys_exec_current_tstamps

            # Start running the instruction on I/O system if its ready
            for idx in iosys_sorted_idx:
                if ((self.iosys[idx].exec_engine.state != spio_iosys.SPIOIOSysExecEngineState.RUN) and
                    self.iosys[idx].exec_engine.is_ready(self.iosys)):
                    self.iosys[idx].exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.READY, clock_tic)

            for idx in iosys_sorted_idx:
                if self.iosys[idx].exec_engine.state == spio_iosys.SPIOIOSysExecEngineState.READY:
                    self.iosys[idx].exec_engine.set_state(spio_iosys.SPIOIOSysExecEngineState.RUN, clock_tic)
                    self.iosys_run_sequence.append(idx)
                if self.iosys[idx].exec_engine.state == spio_iosys.SPIOIOSysExecEngineState.RUN:
                    self.iosys_instr_sequence.append(idx)
                    self.iosys[idx].exec_engine.execute(iosys_exec_cur_instr[idx], clock_tic)
                    clock_tic += 1
                    iosys_exec_ninstr[idx] += 1
