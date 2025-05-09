#!/usr/bin/env python3

"""
Script to generate and build spio_replay.exe from SCORPIO trace logs.

This script reads the SCORPIO trace logs & trace meta data logs to
generate source files to replay the I/O from the trace. These sources
are built, using the same configuration used to build SCORPIO, to
create spio_replay.exe . At runtime spio_replay.exe requires the
I/O decomposition files, spio_trace_decomp_*.nc, to replay the I/O
data pattern.

The script requires the I/O trace logs, spio_trace_log*.log, and the
I/O trace meta-data logs, spio_trace_mdata*.log, to generate the
executable, spio_replay.exe, that can be used to replay the I/O
data patterns in an application (e.g. E3SM) run

"""

import os, sys, argparse, logging

import spio_trace_log_parser
import spio_replay_tool_builder

logger = logging.getLogger(__name__)

spio_replay_tool_banner = "\n===================================================\n" \
                          "\tSCORPIO REPLAY TOOL\n" \
                          "===================================================\n"

###############################################################################
def parse_command_line(args, description):
###############################################################################
    example_usage_doc = '''example:

      python %(prog)s --scorpio-src-dir=/home/user/scorpio_src --scorpio-install-dir=/home/user/scorpio_install --trace-log-dir=/home/user/scorpio_rundir

    '''
    parser =  argparse.ArgumentParser(
                prog=os.path.basename(args[0]),
                description=description,
                epilog=example_usage_doc,
                formatter_class=argparse.ArgumentDefaultsHelpFormatter
              )

    cwd = os.getcwd()

    parser.add_argument("--scorpio-src-dir",
                        metavar="scorpio_src_dir",
                        default=cwd,
                        help="Source directory for the SCORPIO library")

    parser.add_argument("--scorpio-build-dir",
                        metavar="scorpio_build_dir",
                        default=cwd,
                        help="Build directory for the SCORPIO library")

    parser.add_argument("--scorpio-install-dir",
                        metavar="scorpio_install_dir",
                        default=cwd,
                        help="Install directory for the SCORPIO library")

    parser.add_argument("--trace-log-dir",
                        metavar="trace_log_dir",
                        default=cwd,
                        help="Directory containing SCORPIO trace log files")

    parser.add_argument("--replay-tool-dir",
                        metavar="replay_tool_dir",
                        default=cwd,
                        help="Directory to store replay tool source/exe")

    parser.add_argument("-v", "--verbose",
                        action="store_true",
                        help="Turn on verbose mode")

    parser.add_argument("-q", "--quiet",
                        action="store_true",
                        help="Turn on quiet mode (no output to stdout)")

    args = parser.parse_args(args[1:])

    if args.trace_log_dir.strip() is not None:
        os.chdir(os.path.expanduser(args.trace_log_dir.strip()))

    return os.path.expanduser(args.scorpio_src_dir.strip()),\
            os.path.expanduser(args.scorpio_build_dir.strip()),\
            os.path.expanduser(args.scorpio_install_dir.strip()),\
            os.path.expanduser(args.trace_log_dir.strip()),\
            os.path.expanduser(args.replay_tool_dir.strip())

###############################################################################
def _main_func(description):
###############################################################################

    ret = 0
    banner = "================================================="
    print("{}".format(banner + "\n" + "\tSCORPIO REPLAY TOOL GENERATOR\n" + banner + "\n"))

    # Setup logging
    logging.basicConfig(filename="spio_replay_py.log", level=logging.INFO)
    logger.info(spio_replay_tool_banner)

    # Parse command line args
    logger.info("Parsing command line args...")
    sys.argv.extend([] if "ARGS_FOR_SPIO_REPLAY_SCRIPT" not in os.environ else os.environ["ARGS_FOR_SPIO_REPLAY_SCRIPT"].split())

    spio_src_dir, spio_build_dir, spio_install_dir, spio_trace_dir, spio_replay_tool_dir = parse_command_line(sys.argv, description)
    spio_replay_tool_src_dir = spio_replay_tool_dir
    spio_replay_tool_build_dir = spio_replay_tool_dir
    
    # Parse the trace log files and generate replay tool source
    print("Parsing SCORPIO Trace log files in \"{}\"".format(spio_trace_dir));
    logger.info("Parsing SCORPIO Trace log files in \"{}\"".format(spio_trace_dir));
    parser = spio_trace_log_parser.SPIOTraceLogParser(spio_src_dir, spio_install_dir, spio_trace_dir, spio_replay_tool_src_dir)
    parser.parse_log_and_generate_tool()

    # Build replay tool
    print("Building SCORPIO Replay tool (source files at \"{}\")".format(spio_replay_tool_src_dir));
    logger.info("Building SCORPIO Replay tool (source files at \"{}\")".format(spio_replay_tool_src_dir));
    builder = spio_replay_tool_builder.SPIOReplayToolBuilder(spio_src_dir, spio_build_dir, spio_install_dir, spio_trace_dir, spio_replay_tool_src_dir, spio_replay_tool_build_dir)
    ret = builder.build()

    if ret == 0:
        print("Successfully finished parsing trace logs and building replay tool. Replay tool, spio_replay.exe, is available at \"{}\"".format(spio_replay_tool_build_dir))
        logger.info("Successfully finished parsing trace logs and building replay tool. Replay tool, spio_replay.exe, is available at \"{}\"".format(spio_replay_tool_build_dir))
    else:
        print("Building replay tool failed, see build logs in \"{}\"".format(spio_replay_tool_build_dir))
        logger.error("Building replay tool failed, see build logs in \"{}\"".format(spio_replay_tool_build_dir))

    print("{}".format(banner + "\n"))
    sys.exit(ret)

if __name__ == "__main__":
    _main_func(__doc__)
