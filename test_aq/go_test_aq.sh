#!/bin/sh

# Encode the model output (dat --> ens) using enform_aq
perl ../scripts/encode_aq.pl -model_file=dummy_output_3_3_8760.dat -src_file=3_3_8760.src

# Decode the model output (ens --> dat) in a directory tree under ./) using deform_aq
perl ../scripts/decode_aq.pl -ens_file=09999-0001-001-01-01-200601010000.ens -src_file=3_3_8760.src -output_dir=./ -force=1

# Create a netCDF file (ens --> nc) using ens2nc_aq
perl ../scripts/ens2nc_aq.pl -ens_file=09999-0001-001-01-01-200601010000.ens -src_file=3_3_8760.src -cf_file=3_3_8760.cf -output_dir=./ -log_dir=./ -force=1

