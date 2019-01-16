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
my $VERSION = '20190115';

my ($ens_file,$src_file,$output_dir);
my $force = 0;

my $exe_file = "$Bin/../server/deform_aq";

&GetOptions(
    "ens_file=s", \$ens_file,
    "src_file=s", \$src_file,
    "output_dir=s", \$output_dir,
    "force=i", \$force
);
unless ($ens_file && $src_file && $output_dir) {
    die "\nUsage: perl $0 -ens_file=/path/to/model_output.ens \\
                         -src_file=/path/to/sequence-case.src \\
                         -output_dir=/path/to/output_dir (add final slash) \\
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

# --- Search number of variables in src file
open(SRC,"<$src_file") or die "$src_file: $!\n";
my @lines = <SRC>;
close(SRC);
my $line;
while(@lines) {
    $line = shift @lines;
    chomp $line;
    last if ($line eq 'Code                            $ '.$rl);
}
shift @lines;
shift @lines;
$line = shift @lines;
chomp $line;
my ($dummy,$np) = split(/\$/,$line);
$np  = $np * 1;

my $now = strftime("%Y%m%d%H%M", localtime(time));

my @paths;
push @paths, "$output_dir/s$sq/c$cs/status_c";
push @paths, "$output_dir/s$sq/c$cs/r$rl/status_r";
for my $ii (1..$np) {
    my $vr = sprintf "%2.2d", $ii;
    push @paths, "$output_dir/s$sq/c$cs/r$rl/v$vr";
}
foreach my $path (@paths) {
    next if (-e $path);
    die "Output folder $path does not exists. Use -force=1 in command line.\n" unless (-e $path || $force); 
    mkpath $path or die "Cannot create output folder $path.\n";
}

my $output_file = "$output_dir/s$sq/c$cs/r$rl/v$vr/$md-$sq-$cs-$rl-01-$dt.dat";
die "Output file $output_file already exists. Use -force=1 to recreate it.\n" unless (!-e $output_file || ($force == 1));

#  1,INPUT_PATH
#  2,INPUT_FILE
#  3,SOURCE_FILE
#  4,MODEL_NAME
#  5,UPL_DATE
#  6,DELTA_M
#  7,DELTA_U
#  8,UPL_USER
#  9,UPL_IP
# 10,OUTPUT_BASE_PATH
my @cmd = ($exe_file,$input_path,$input_name.'.ens',$src_file,'DUMMY',$now,'+0m','+0m','dummy','127.0.0.1',$output_dir);
#die Dumper \@cmd;
system(@cmd);
    
print "\n\nCommand issued was:\n";
print join(" ",@cmd),"\n";
