#!/usr/bin/env python3
import sys
import argparse
import re
import logging

#logger = logging.getLogger(__name__)

component_names = ["eam", "cpl", "elm", "mosart", "mpaso", "mpassi"]
log_levels = {"DEBUG" : logging.DEBUG, "INFO" : logging.INFO,
              "WARNING" : logging.WARNING, "ERROR" : logging.ERROR,
              "CRITICAL" : logging.CRITICAL}

tool_hdr_str = "Decomposition map info parser tool\n=====================================================\n"

def set_log_level(log_level):
  logging.basicConfig(format='%(levelname)s:%(message)s', level=log_levels[log_level])

def parse_command_line(argv):
  parser = argparse.ArgumentParser(description ='Parse decomp info file : pio_decomp_map_info.txt')
  parser.add_argument('--filter-components=', dest = 'filter_components',
                      default ='eam',
                      metavar ='filter_component',
                      nargs ='+', action ='store',
                      help ='The name of components to filter (Supported component names = ' + str(component_names) + ')')
  parser.add_argument('--decomp-map-info-file', dest ='decomp_map_info_fname', 
                      default ='./pio_decomp_map_info.txt',
                      action ='store',
                      help ='The complete path of pio_decomp_map_info.txt (or the file containing the decomp map info)')
  parser.add_argument('--log-level', dest ='log_level',
                      default ='INFO',
                      action ='store',
                      help ='Specify the log level (Default is INFO, Available log levels are : ' + str(log_levels.keys()) + ')')
  args = parser.parse_args()
  return args.filter_components, args.decomp_map_info_fname, args.log_level

def parse_decomp_map_info(fname, filter_components):
  decomp_map_fnames = set()
  with open(fname, 'r') as file:
    component_begin_fname_rgx_str = r'^[{].*\sfname\s[=].*(' + r'|'.join(filter_components) + r').*$'
    logging.debug("Using regex str \"{}\" to match files to filter".format(component_begin_fname_rgx_str))
    component_begin_fname_rgx = re.compile(component_begin_fname_rgx_str)
    component_end_fname_rgx = re.compile(r'^[}]$')
    collect_decomp_map_fnames = False
    for line in file:
      line = line.strip()
      if component_begin_fname_rgx.match(line):
        collect_decomp_map_fnames = True
        logging.debug("Enabling collection of decomp map file names : Parsed string : {}".format(line))
      elif component_end_fname_rgx.match(line):
        collect_decomp_map_fnames = False
        logging.debug("Disabling collection of decomp map file names : Parsed string : {}".format(line))
      elif collect_decomp_map_fnames:
        decomp_map_info = line.split(',')
        decomp_map_fname = decomp_map_info[2].split('=')[1].strip()
        decomp_map_fnames.add(decomp_map_fname)
  return decomp_map_fnames

def _main(argv):
  #set_log_level('DEBUG')
  filter_components, decomp_map_info_fname, log_level = parse_command_line(argv)
  set_log_level(log_level)
  logging.info(tool_hdr_str)
  logging.debug("Parsing decomp info file : {}".format(decomp_map_info_fname))
  logging.debug("Filtering decomp file info of components : {}".format(str(filter_components)))
  filtered_decomp_map_fnames = parse_decomp_map_info(decomp_map_info_fname, filter_components)
  print(filtered_decomp_map_fnames)

if __name__ == "__main__":
  _main(sys.argv)


