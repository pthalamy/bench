#!/usr/bin/perl
use strict;
use warnings;

use lib ".";
use serial_log;

my $num_args = $#ARGV + 1;
#print "Number of Arguments: $num_args\n";
if ($num_args != 3) {
  print "\nUsage: generate_csv.pl serial_log_filename OPSNAME Comment\n";
  exit;
}

my $filename = $ARGV[0];
my $Comment = $ARGV[2];
my @Iter;
my  @Cycles;
my @Benchsize;
my $Iter;
my $Cycles;
my $Benchsize;
my @CyclesPerOps;


#my @OPS=("OPFLOATADD","OPFLOATDIV","OPFLOATEQ","OPFLOATGT","OPFLOATGTE","OPFLOATLT","OPFLOATLTE","OPFLOATMOD","OPFLOATMUL","OPFLOATNEQ","OPFLOATSUB","OPINTADD","OPINTDIV","OPINTEQ","OPINTGT","OPINTGTE","OPINTLT","OPINTLTE","OPINTMOD","OPINTMUL","OPINTNEQ","OPINTSUB");
my @OPS=($ARGV[1]);
my $OPS;

#reading serial log
read_serial_log (\@Iter,\@Cycles,\@Benchsize,\@OPS,\$filename);

my $numberOfRecords=@Iter;
my $index;

# Calculating Cycles per Ops

my $size=@Cycles;
for ($index=0; $index<$size; $index++) {
        $CyclesPerOps[$index] = $Cycles[$index]/$Iter[$index];
        }
# print CSV to stdout

#print header
#print "Iterations,Bench size,Cycles,Cycles per Ops,\n";
print "Iterations,Cycles,\n";
for ($index=0;$index<$numberOfRecords;$index++) {

	print $Iter[$index].",";
#	print $Benchsize[$index].",";
	print $Cycles[$index].",\n";
#	print $CyclesPerOps[$index]."\n"
}

my $max = $CyclesPerOps[0];                                                                                           
my $min = $CyclesPerOps[0];                                                                                           
                                                                                                                      
    foreach $index (@CyclesPerOps[1..$#CyclesPerOps])                                                                     
    {                                                                                                                 
        if ($index > $max)                                                                                                
        {                                                                                                             
            $max = $index;                                                                                                
        }                                                                                                             
        elsif ($index < $min)                                                                                             
        {                                                                                                             
            $min = $index;                                                                                                
        }                                                                                                             
    }       


print "Minimum: $min\n";


#	print "Iterations;";
#	foreach $Iter (@Iter) {
#		print "$Iter;";
#		}	
#	print "\n";
#	@Iter = ();
#
#	print "Bench Size;";
#	foreach $Benchsize (@Benchsize) {
#		print "$Benchsize;";
#		}
#	print "\n";
#	@Benchsize =();
#
#	print "Cycles;";
#	foreach  $Cycles (@Cycles) {
#		print "$Cycles;";
#	}
#	print "\n";
#	@Cycles = ();
#
#}
#close (FILE);
