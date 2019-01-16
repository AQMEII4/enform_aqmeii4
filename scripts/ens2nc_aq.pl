use strict;

# (C) ENSEMBLE https://ensemble.jrc.europa.eu

# Author: rbianconi@enviroware.com
# Contact: stefano.galmarini@jrc.europa.eu

# This script deecodes the model grid output in .ens format

use strict;
use Data::Dumper;
use FindBin qw($Bin);
use Getopt::Long;
use File::Basename;
use File::Path;
use POSIX qw (strftime);
my $VERSION = '20190116';

my ($ens_file,$src_file,$cf_file,$output_dir,$log_dir);
my $force = 0;

my $exe_file = "$Bin/../ens2nc/ens2nc_aq";

&GetOptions(
    "ens_file=s", \$ens_file,
    "src_file=s", \$src_file,
    "cf_file=s", \$cf_file,
    "output_dir=s", \$output_dir,
    "log_dir=s", \$log_dir,
    "force=i", \$force
);
unless ($ens_file && $src_file && $output_dir) {
    die "\nUsage: perl $0 -ens_file=/path/to/model_output.ens \\
                         -src_file=/path/to/sequence-case.src \\
                         -cf_file=/path/to/sequence-case.cf \\
                         -output_dir=/path/to/output_dir (add final slash) \\
                         -log_dir=/path/to/log_dir (add final slash) \\
                         -force=1 to create non-existing output folder (optional)
                          \n\n";
}

die "Output dir $output_dir does not exist (use -force=1 to create it).\n" unless (-e $output_dir || $force);

# --- Check that the name is ENSEMBLE-compatible (MMMMM-SSSS-CCC-VV-II-YYYYMMDDHHSS.ens)
die "$ens_file does not exist\." unless (-e $ens_file);
my ($input_name,$input_path,$suffix) = fileparse($ens_file,'.ens');
my $safe = $input_name=~/(\d\d\d\d\d-\d\d\d\d-\d\d\d-\d\d-\d\d-\d\d\d\d\d\d\d\d\d\d\d\d)/;
die "$input_name.ens is not a valid Ensemble .ens filename\n" unless ($safe);

# --- Break input_name into components
my ($md,$sq,$cs,$rl,$vr,$dt) = split (/-/,$input_name);

my $nc_base = $output_dir.'s'.$sq.'/c'.$cs.'/r'.$rl.'/v'.$vr.'/';
die "netCDF output dir $nc_base does not exist (use -force=1 to create it).\n" unless (-e $nc_base || $force);

#my @cmd = ($exe_file,$output_dir,$ens_file,$src_file,$cf_file,$md,$output_dir,">& $log_dir"."$ens_file.log");
my @cmd = ($exe_file,$output_dir,$ens_file,$src_file,$cf_file,$md,$output_dir);
system(@cmd);
print "\n\nCommand issued was:\n";
print join(" ",@cmd),"\n";
