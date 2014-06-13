#!/bin/bash -f
#
# this file sets up links to various .bb, .c, .h files for making a simulator/block executable.
#

if [ ! $?BBASE ]; then
  echo "You must set BBASE to the directory which is the base of the blinkyblock code"
  exit 1
fi

srcdir=$BBASE/src-bobby
dfile=$1
shift

#### routine to create list

createlist() {
    d=$1
    t=$2
    ll=$srcdir/$d/linkfile.list
    echo "creating for $d ($ll)"
    if [ ! -e $ll ]; then echo "$ll does not exist."; return 0; fi
    for i in `cat $ll`; do 
	base=$srcdir/$d
	if [ ${i/*./} == bb ]; then 
	    echo $t"BBSRC += $base/$i" >> $dfile
	    j=${i/.*/.o}
	    echo $t"O += \$(TARCH)/$d/$j" >> $dfile
	fi
	if [ ${i/*./} == bbh ]; then echo $t"BBINCLUDE += $base/$i" >> $dfile; fi
	if [ ${i/*./} == c ]; then 
	    echo $t"CSRC += $base/$i" >> $dfile
	    j=${i/.*/.o}
	    echo $t"O += \$(TARCH)/$d/$j" >> $dfile
	fi
    done
}

echo -n "# list of extern files generated by create-extern-Makefile.sh on " > $dfile
echo `date` >> $dfile
echo "#"  >> $dfile

for d in $* ; do
    if [ ! $d == meldinterp-runtime ]; then createlist $d "ALL"; else createlist meldinterp-runtime "MELD"; fi
done
echo "Done linking. rerun make."
