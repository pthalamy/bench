package serial_log;
use strict;
use warnings;
use Exporter;

Our @ISA= qw( Exporter );

# these CAN be exported.
our @EXPORT_OK = qw( read_serial_log );

# these are exported by default.
our @EXPORT = qw( read_serial_log );

sub  read_serial_log {
  
my ($Iterref,$Cyclesref,$Benchsizeref,$OPSref,$filename) = @_ ;
my @Iter = @$Iterref;
our @Cycles = @$Cyclesref;
our @Benchsize = @$Benchsizeref;
our @OPS = @$OPSref;
my $OPS;


open FILE, "<", ${$filename} or die $!;

# Storing values
foreach $OPS (@OPS) {
        print "$OPS\n";
        while (<FILE>) {
	my $line;
        $line = $_;
	$line =~ s/\x13//g;
	print $line;
                if ($line =~ /bench_($OPS)_((\d)+)\.bb:Cycles:((\d)+):Benchsize:((\d)+)/) {
                        print "number of $OPS iterations: $1\n";
                        push (@$Iterref,$2);
                        print "number of cycles: $4\n";
                        push (@$Cyclesref, $4);
                        push (@$Benchsizeref, $6);
                }

        }

        seek(FILE, 0, 0) || die "$0: error: cannot seek to the beginning of the file!\n";

	}

}

1;
