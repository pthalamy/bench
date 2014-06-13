#!/usr/bin/perl

use String::Random;
$foo = new String::Random;


while (<>)
{
	if ($_ =~ m/(#define)\s+.*\(x\)\s+.*/)
	{ @biniou = split /\s+/,$_;
	  $randomdigit = $foo->randregex('\d');
	  print "\"".substr($biniou[1],0,-3)."\"".",".$randomdigit.",";
	}
}
