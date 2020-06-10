#!/bin/sh

# Encode the model output (dat --> ens) using enform_aq
perl ../scripts/encode_aq.pl -model_file=v01-24567-0251-262-59-201601010100.dat -src_file=0251-262_test_monthly_median.src

# Decode the model output (ens --> dat) in a directory tree under ./) using deform_aq
perl ../scripts/decode_aq.pl -ens_file=24567-0251-262-59-01-201601010100.ens -src_file=0251-262_test_monthly_median.src -output_dir=./ -force=1

# Create a netCDF file (ens --> nc) using ens2nc_aq
perl ../scripts/ens2nc_aq.pl -ens_file=24567-0251-262-59-01-201601010100.ens -src_file=0251-262_test_monthly_median.src -cf_file=0251-262_test_monthly_median.cf -output_dir=./ -log_dir=./ -force=1

