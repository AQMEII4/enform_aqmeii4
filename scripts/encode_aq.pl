use strict;

# (C) ENSEMBLE https://ensemble.jrc.europa.eu

# Author: rbianconi@enviroware.com
# Contact: stefano.galmarini@jrc.europa.eu

# This script encodes the model grid output to be uploaded to ENSEMBLE

use strict;
use Data::Dumper;
use FindBin qw($Bin);
use Getopt::Long;

my $VERSION = '20190115';

my ($model_file,$src_file,$ens_dir);

&GetOptions(
    "model_file=s", \$model_file,
    "src_file=s", \$src_file
);
unless ($model_file && $src_file) {
    die "\nUsage: perl $0 -model_file=/path/to/model_output.txt \\
                         -src_file=/path/to/sequence-case.src
                          
                          \n\n";
}

my $exe_file = "$Bin/../client/enform_aq";
my @cmd = ($exe_file,$src_file,$model_file);
system(@cmd);
print "\n\nCommand issued was:\n";
print join(" ",@cmd),"\n";
    
