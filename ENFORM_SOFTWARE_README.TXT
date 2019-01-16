********************************************
ENFORM Version: 20190101
PREVIOUS ENFORM Version: 20150930
This file  : ENFORM_SOFTWARE_README.TXT 
*********************************************

(C) 2000-2019 European Commission

ENSEMBLE - http ://ensemble.jrc.ec.europa.eu

Author: rbianconi@enviroware.com
Contact: stefano.galmarini@ec.europa.eu

This is the set of FORTRAN routines to postprocess at client's side the model output files
for the ENSEMBLE non nuclear activities, before transferring them to the ENSEMBLE platform.
For comments about compilation see the bottom of this file.

The package also includes the routines to produce ASCII (.dat) and netCDF (.nc) files starting from
encoded grid files generated with enform_aq (files .ens). These codes are deform_aq and ens2nc_aq and their
use is described below.

An Ensemble application can be on 2D/3D regular grids (referenced as AQ or aq in the following)
or on a set of sparse points at one or more vertical levels (refereneced as AQR or aqr in the 
following) as monitoring stations or ozonesondes. An high time-resolution version of AQR, referred
as AQRH or aqrh, is also available. In AQRH model output time resolution is 0.01 seconds. 

The enform_aq, enform_aqr and enform_aqrh are the programs here provided that allow to postprocess
the model results so that they can be submitted to Ensemble.

This postprocessing requires a metafile (and a related TSD, Technical Specification Document)
that you will receive. Both metafile contents and model output structure are described in the TSD.
The metafile (or "source file") has extension .src. Another metafile, with extension .cf, will be 
provided to generate the netCDF files.

The metafile indicates if the application is AQ, AQR or AQRH and the enform program to be used must
be consistent with that.

The program reduces the model file size by "optimisation" of data significance and structure
and it checks the data compliance to the simulation case before the submission to ENSEMBLE.
A check based on a random string is also made when uploading the file to the remote ENSEMBLE
platform.

*** Usage of enform, deform and ens2nc ***

The enform_aq, deform_aq and ens2nc_aq programs usage is shown in the script go_test_aq.sh
in folder test_aq. This script calls, for each iof these programs, a Perl driver that could also
be used as alternative to call the command line of each program.

Some details follow:

1. enform

The executable provided herewith (upon compilation, Linux/Unix/OSx or Windows, see note at bottom) 
for processing your model output into an ENSEMBLE-coded file is:

./client/enform_aq (or ./client/enform_aqr, or ./client/enform_aqrh)

The general syntax to execute enform_aq is:

./client/enform_aq metafile model_output_file, e.g.

./client/enform_aq 0001-001.src model_output.txt

The metafile has a standard name:

SSSS-CCC.src

The output file of enform_aq has a standard name:

MMMMM-SSSS-CCC-VV-II-YYYYMMDDHHSS.ens

where:

- MMMMM is the model code (assigned by the ENFORM_AQ program based on the
  value of MC in the model output file)
- SSSS-CCC is the simulation case number (assigned by the ENFORM_AQ program
  based on the random key and the corresponding source file content)
- VV is the variable code (assigned by the ENFORM_AQ program based on the
  variable name in the model output file and corresponding source file content)
- II is the item code (assigned by the ENFORM_AQ program based on the item
  name in the model output file and corresponding source file content)
- YYYYMMDDHHSS is the date and time of meteorological data used to run the
  dispersion forecast or the last date and time for analysis runs (assigned by 
  the ENFORM_AQ program based on meteo date in model output file)

Once the enform_aq output file is generated, it must be compressed with the 
bzip2 algorithm and then submitted as described in the TSD.
bzip2 is generally installed in Linux distributions and it is available
in most compression utilities under Windows. If it is not available it can be 
downloaded from http://www.bzip.org.

The same applies to enform_aqr and enform_aqrh.

---> enform_aq
The enform_aq program is provided with a sample metafile (3_3_8760.src) and a 
dummy output file (dummy_output_3_3_8760.dat) in the directory ./test_enform_aq

Upon changing to the directory ./test_enform_aq, the execution of the command:

../client/enform_aq 3_3_8760.src dummy_output_3_3_8760.dat

will produce the output file:

test_enform_aq/09999-0001-001-01-01-200601010000.ens

---> enform_aqr
The enform_aqr program is provided with a sample metafile (0999-001.src) and a
dummy output file (dummy_model_output_enform_aq_receptors.dat) in the directory
./test_enform_aqr

Upon changing to the directory ./test_enform_aqr, the execution of the command:

../client/enform_aqr 0999-001.src dummy_model_output_enform_aqr_receptors.dat

will produce the output file

09900-0999-001-02-01-200901010000.ens

---> enform_aqrh
The enform_aqrh program is provided with a sample metafile (0999-001.src) and a
dummy output file (dummy_model_output_enform_aqrh.dat) in the directory
./test_enform_aqrh

Upon changing to the directory ./test_enform_aqrh, the execution of the command:

../client/enform_aqrh 0999-001.src dummy_model_output_enform_aqrh.dat

will produce the output file:

09900-0999-001-01-01-200309261700.ens

2. deform_aq and ens2nc_aq

The deform_aq and ens2nc_aq programs usage is shown in the script go_test_aq.sh
in folder test_aq. This script calls, for each program, a Perl driver that could also
be used as alternative to call the command line of each program.

# Encode the model output (dat --> ens) using enform_aq:
perl ../scripts/encode_aq.pl -model_file=dummy_output_3_3_8760.dat -src_file=3_3_8760.src

The underlying command issued by the script is:
../client/enform_aq 3_3_8760.src dummy_output_3_3_8760.dat

# Decode the model output (ens --> dat) in a directory tree under ./) using deform_aq
perl ../scripts/decode_aq.pl -ens_file=09999-0001-001-01-01-200601010000.ens -src_file=3_3_8760.src -output_dir=./ -force=1

The underlying command issued by the script is:
../server/deform_aq ./ 09999-0001-001-01-01-200601010000.ens 3_3_8760.src DUMMY 201901161804 +0m +0m dummy 127.0.0.1 ./

# Create a netCDF file (ens --> nc) using ens2nc_aq
perl ../scripts/ens2nc_aq.pl -ens_file=09999-0001-001-01-01-200601010000.ens -src_file=3_3_8760.src -cf_file=3_3_8760.cf -output_dir=./ -log_dir=./ -force=1

The underlying command issued by the script is:
../ens2nc/ens2nc_aq ./ 09999-0001-001-01-01-200601010000.ens 3_3_8760.src 3_3_8760.cf 09999 ./

*** List of routines/files provided ************************************************

./

    ENFORM_SOFTWARE_README.TXT (this file)

./client/
    
    compile
    enform_aq.for
    enform_aqr.for
    enform_aqrh.for

./inc/

    aq_processing_block.for
    aqr_processing_block.for
    aqr_hires_processing_block.for
    enform_aq.cmn
    enform_aq.prm
    enform_aqr.cmn
    enform_aqr.prm
    enform_aqrh.cmn
    enform_aqrh.prm
    version.prm

./lib/

    day_number.for
    encodeg.for
    find_nto.for
    fix24_aqr.for
    fix24.for
    fixname.for
    generate_dates.for
    generate_dates_hires.for
    getarg.for
    get_date.for
    leap_year.for
    lstrim.for
    lstriml.for
    parse_averaging.for
    parse_item.for
    parse_metafile_aqr.for
    parse_metafile_aqr_hires.for
    parse_metafile.for
    readrec.for
    sort.for

Compilation parameters:

There are compile scripts in each directory where the main programs are.
For example:

$ cd client
$ ./compile 

will genreate the executables enform_aq and enform_aqr

The file ./inc/enform_aq.prm contains these sensible parameters

    NXM   - maximum number of grid nodes along X direction
    NYM   - maximum number of grid nodes along Y direction
    NTMAX - maximum number of time outputs
    NUC_AMX - maximum number of variables
    NITEMS_MAX - maximum number of items
    NPOSTS_MAX - maximum number of postprocessings

The file ./inc/enform_aqr.prm contains these sensible parameters

    NZM   - maximum number of vertical levels (e.g. measurements in profile)
    NTMAX - maximum number of time outputs
    NUC_AMX - maximum number of variables
    NITEMS_MAX - maximum number of items

    They must be consistent with the specifications in the  metafile

    LDEBUG = .TRUE.

    Set this variable to .FALSE. to suppress some verbosity

Compilers:
The codes were successfully compiled and run under Linux (gfortran compiler) and
Windows XP Professional (gfortran).

While the getarg command is intrinsic in g95/gfortran, in order to compile the programs
under Windows using compilers as Lahey LF90 it is necessary to uncomment the instruction

INCLUDE 'getarg.for' 

at the end of the enform_aq.for code to include a subrourine that mimicks the getarg
behavior for Lahey LF90.  Some other Fortran compilers under Windows do have getarg
as intrinsic, or you can get them to work by modifying the getarg.for routine
provided herewith.

Memory issues from users experience:
- using gfortran the flag -mcmodel=medium could resolve limitations in memory allocation (truncation in COMMON blocs)
- using ifort, this works: ifort -mcmodel=large -shared_intel -o enform_aq.x enform_aq.for

Speed issues from users experience:
- you can check with "top" Unix command what are the resources allocated for your run and set them as necessary.


*******************************
Last modification: 20190101 RBI
*******************************
