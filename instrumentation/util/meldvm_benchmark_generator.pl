#!/usr/bin/perl 

use strict;
use warnings;

my $line;
my $i;
my $path_to_singleop_files = "benchmark_single_instruction";

if ($#ARGV != 1 ) {

	print "\n usage is meldvm_benchmark_generator.pl operation_name number_of_iterations\n\n";
	print "This will generate a meldvm benchmark code file containing number_of_iterations x operation_name \n\n";
	exit;
}

# Opening the file containing the single operation
my $singleopfile =  "$path_to_singleop_files/"."$ARGV[0]\n";
open SINGLEOPFILE,$singleopfile or die $!;
# Reading the file containing the single operation
my @singleop=<SINGLEOPFILE>;

#opening header and footer files
open BENCHMARKHEADER,"benchmark_header.bb" or die $!;
open BENCHMARKFOOTER,"benchmark_footer.bb" or die $!;

# Filling the benchmark in that order:
# header
# number_of_iterations x single operation
# footer

while (<BENCHMARKHEADER>) {
	$line = $_;
	print $line;
	}
print "/* Writing $ARGV[1] Operations of type $ARGV[0] */ \n";

for ($i = 0; $i<$ARGV[1]; $i++) {
	print @singleop;
	}

print "/* End of $ARGV[1] Operations of type $ARGV[0] */ \n";

while (<BENCHMARKFOOTER>) {
	$line = $_;
	print $line
	}

close  BENCHMARKHEADER;
close BENCHMARKFOOTER;
close SINGLEOPFILE;

