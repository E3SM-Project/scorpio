Parse the pio_decomp_map_info.txt file (that contains the mapping between file/variable names and dumped decomposition file names) and filter out the decomposition files used by specified set of components.

================ EXAMPLE ====================================
$ ./parse_decomp_info.py --help
usage: parse_decomp_info.py [-h] [--filter-components= filter_component [filter_component ...]] [--decomp-map-info-file DECOMP_MAP_INFO_FNAME] [--log-level LOG_LEVEL]

Parse decomp info file : pio_decomp_map_info.txt

options:
  -h, --help            show this help message and exit
  --filter-components= filter_component [filter_component ...]
                        The name of components to filter (Supported component names = ['eam', 'cpl', 'elm', 'mosart', 'mpaso', 'mpassi'])
  --decomp-map-info-file DECOMP_MAP_INFO_FNAME
                        The complete path of pio_decomp_map_info.txt (or the file containing the decomp map info)
  --log-level LOG_LEVEL
                        Specify the log level (Default is INFO, Available log levels are : dict_keys(['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL']))


================ EXAMPLE ====================================

$ ./parse_decomp_info.py --filter-components eam cpl --decomp-map-info-file ./pio_decomp_map_info.txt --log-level=INFO |& tee out.log
INFO:Decomposition map info parser tool
=====================================================

{'piodecomp002049id16tasks04io01dims0242.dat', 'piodecomp002048id16tasks04io02dims0269.dat', 'piodecomp002049id16tasks04io01dims15.dat', 'piodecomp002048id16tasks04io02dims0267.dat', 'piodecomp002048id16tasks04io02dims0263.dat', 'piodecomp002049id16tasks04io01dims0256.dat', 'piodecomp002049id16tasks04io02dims0248.dat', 'piodecomp002048id16tasks04io02dims0268.dat', 'piodecomp002049id16tasks04io01dims0245.dat', 'piodecomp002048id16tasks04io02dims0259.dat', 'piodecomp002049id16tasks04io02dims0247.dat', 'piodecomp002048id16tasks04io02dims0270.dat', 'piodecomp002049id16tasks04io01dims0244.dat', 'piodecomp002049id16tasks04io01dims0252.dat', 'piodecomp002049id16tasks04io01dims0253.dat', 'piodecomp002049id16tasks04io01dims0254.dat', 'piodecomp002048id16tasks04io02dims0262.dat', 'piodecomp002049id16tasks04io02dims0249.dat', 'piodecomp002048id16tasks04io02dims0265.dat', 'piodecomp002048id16tasks04io02dims0261.dat', 'piodecomp002049id16tasks04io01dims0250.dat', 'piodecomp002049id16tasks04io02dims0243.dat', 'piodecomp002048id16tasks04io02dims0264.dat', 'piodecomp002048id16tasks04io02dims0260.dat', 'piodecomp002048id16tasks04io02dims0258.dat', 'piodecomp002049id16tasks04io01dims0246.dat', 'piodecomp002048id16tasks04io02dims0266.dat'}

================ EXAMPLE ====================================
./parse_decomp_info.py --filter-components mosart --decomp-map-info-file ./pio_decomp_map_info.txt --log-level=INFO | sed -e "s/'//g"
INFO:Decomposition map info parser tool
=====================================================

{piodecomp002054id16tasks04io02dims0202.dat, piodecomp002054id16tasks04io02dims0201.dat}
