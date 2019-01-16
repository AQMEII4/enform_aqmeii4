# enform_aqmeii4

This is the set of FORTRAN routines and Perl scripts to postprocess at client's side the model output files for the ENSEMBLE non nuclear activities, before transferring them to the ENSEMBLE platform.

For comments about compilation see the bottom of this file.

This package also includes the routines to produce ASCII (.dat) and netCDF (.nc) files starting from encoded grid files generated with enform_aq (files .ens). These codes are deform_aq and ens2nc_aq and their use is described below.

An Ensemble application can be on 2D/3D regular grids (referenced as AQ or aq in the following) or on a set of sparse points at one or more vertical levels (refereneced as AQR or aqr in the following) as monitoring stations or ozonesondes. An high time-resolution version of AQR, referred as AQRH or aqrh, is also available. In AQRH model output time resolution is 0.01 seconds.

The enform_aq, enform_aqr and enform_aqrh are the programs here provided that allow to postprocess the model results so that they can be submitted to Ensemble.

This postprocessing requires a metafile (and a related TSD, Technical Specification Document) that you will receive. Both metafile contents and model output structure are described in the TSD. The metafile (or "source file") has extension .src. Another metafile, with extension .cf, will be provided to generate the netCDF files.

The metafile indicates if the application is AQ, AQR or AQRH and the enform program to be used must be consistent with that.

The program reduces the model file size by "optimisation" of data significance and structure and it checks the data compliance to the simulation case before the submission to ENSEMBLE. A check based on a random string is also made when uploading the file to the remote ENSEMBLE platform.

*** Usage of enform, deform and ens2nc ***

The enform_aq, deform_aq and ens2nc_aq programs usage is shown in the script go_test_aq.sh
in folder test_aq. 

This script calls, for each iof these programs, a Perl driver that could also be used as alternative to call the command line of each program.

Clone and unzip this archive

```
unzip 

Additional information is in file [ENFORM_SOFTWARE_README.TXT] (ENFORM_SOFTWARE_README.TXT)
