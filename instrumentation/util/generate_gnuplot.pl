#!/usr/bin/perl

use strict;
use warnings;

use lib ".";
use serial_log;

use Chart::Graph::Gnuplot qw(gnuplot);
use List::Util qw[min max];

#my ($Iterref,$Cyclesref,$Benchsizeref,$OPSref,$filename) = @_ 

my $num_args = $#ARGV + 1;
#print "Number of Arguments: $num_args\n";
if ($num_args != 3) {
  print "\nUsage: generate_gnuplot.pl serial_log_filename OPSNAME Comment\n";
  exit;
}

my $filename = $ARGV[0];
my $Comment = $ARGV[2];
my @Iter;
my @Cycles;
my @Benchsize;
my @CyclesPerOps;
my $i;
my $size;
#my @OPS=("OPFLOATADD","OPFLOATDIV","OPFLOATEQ","OPFLOATGT","OPFLOATGTE","OPFLOATLT","OPFLOATLTE","OPFLOATMOD","OPFLOATMUL","OPFLOATNEQ" ,"OPFLOATSUB","OPINTADD","OPINTDIV","OPINTEQ","OPINTGT","OPINTGTE","OPINTLT","OPINTLTE","OPINTMOD","OPINTMUL","OPINTNEQ","OPINTSUB");
my @OPS=($ARGV[1]);

read_serial_log (\@Iter,\@Cycles,\@Benchsize,\@OPS,\$filename);

#calculating cycles per ops

$size=@Cycles;
for ($i=0; $i<$size; $i++) {
	$CyclesPerOps[$i] = $Cycles[$i]/$Iter[$i];
	print "$Iter[$i] $CyclesPerOps[$i]\n";
	}

my $max = $CyclesPerOps[0];
my $min = $CyclesPerOps[0];

    foreach $i (@CyclesPerOps[1..$#CyclesPerOps])
    {
        if ($i > $max)
        {
            $max = $i;
        }
        elsif ($i < $min)
        {
            $min = $i;
        }
    }

#print @Iter;
#print @Cycles;


gnuplot({'title' => "",
           'y-axis label' => "CPU Cycles",
	   'x-axis label' => "Iterations",
           'output type' => 'eps enhanced color size 4in, 3in "Helvetica, 20"',
           'output file' => "graphs/$OPS[0]_CyclesPerOps_$Comment.eps",
           #'xtics' => \@Iter,
           'ytics' => [$CyclesPerOps[$#CyclesPerOps]],
           #'xtics' => [1000,2000,3000,4000,5000,6000],
           'xtics' => [100,200,300,400,500,600,700,800,900,1000,1100],
           'extra_opts' => ["set size '1,1'","set key outside above font 'Helvetica,25'","set ytics font 'Helvetica,18' \n set ylabel font 'Helvetica,25' offset 8,0 \n set xtics font 'Helvetica,18' \n set xlabel font 'Helvetica,25' \n set style line 4 linetype 1 lw 10 pointtype 3 pointsize 10"]},
           [{ title => "Cycles per Operation for $Comment",
              'style' => 'lines lw 8 linecolor rgb "red"',
            'type' => 'columns', }, \@Iter, \@CyclesPerOps ]
);

gnuplot({'title' => $OPS[0],
           'y-axis label' => "Number of CPU Cycles for $OPS[0]",
	   'x-axis label' => "Number of Iterations",
           'output type' => 'eps',
           'output file' => "graphs/$OPS[0]_Cycles_$Comment.eps",
           'xtics' => \@Iter,
           'ytics' => \@Cycles,
           'extra_opts' => ''},
          [{'title' => "Number of Cycles for $OPS[0] $Comment",
            'style' => 'lines',
            'type' => 'columns'}, \@Iter,
                                  \@Cycles ]
);

#print "Generated graphs/$OPS[0]_Cycles_$Comment.eps\n";
#print "Generated graphs/$OPS[0]_CyclesPerOps_$Comment.eps\n";

