# enform_aqmeii4

This is the set of FORTRAN routines and Perl scripts to postprocess at client's side the model output files for the ENSEMBLE non nuclear activities, before transferring them to the ENSEMBLE platform.

This package also includes the routines to produce ASCII (.dat) and netCDF (.nc) files starting from encoded grid files generated with enform_aq (files .ens). These codes are deform_aq and ens2nc_aq and their use is described below.

Additional information is in file [ENFORM_SOFTWARE_README.TXT](ENFORM_SOFTWARE_README.TXT)

## Usage of enform, deform and ens2nc

The `enform_aq`, `deform_aq` and `ens2nc_aq` programs usage is shown in the script `go_test_aq.sh` in folder `test_aq`. 

This script calls, for each of these programs, a Perl driver that could also be used as alternative to call the command line of each program.

Download (or clone) this archive:
```
$ wget https://github.com/enviroware/enform_aqmeii4/archive/master.zip
```
Unzip it:
```
$ unzip master.zip
```
Compile the executables:
```
$ cd enform_aqmeii4-master/client
$ ./compile
$ cd ../server
$ ./compile_aq
$ cd ../ens2nc
$ ./compile_ens2nc_aq
```
