#!/bin/bash -f
#
# this file sets up links to various .bb, .c, .h files for makeing a simulator executable.
#
if [ ! $?BBASE ]; then
  echo "You must set BBASE to the directory which is the base of the blinkyblock code"
  exit 1
fi
if [ ! $?ARCH ]; then
  export ARCH=`uname -m`-`uname -s | sed -e 's+/+-+g'`
  echo "setting ARCH to $ARCH"
fi
if [ ! -e arch-$ARCH ]; then
  echo "There is no arch-$ARCH target directory, please make one."
  exit 1;
fi
cd arch-$ARCH
if [ $? != 0 ]; then echo "failed to enter arch-$ARCH"; exit 1; fi

echo -n "# list of extern files generated by linkfile4meld.sh on " > Makefile.externs
echo `date` >> Makefile.externs

archdir=`pwd`

#for d in sim system hw-api hw-sim meldinterp-runtime; do
for d in $*; do
  echo "linking from $d"
  mkdir -p $d
  cd $d
  for i in `cat $BBASE/src-bobby/$d/linkfile.list`; do 
    ln -s $BBASE/src-bobby/$d/$i .
    if [ ${i/*./} == bb ]; then echo "BBSRC += arch-$ARCH/$d/$i" >> $archdir/Makefile.externs; fi
    if [ ${i/*./} == bbh ]; then echo "BBINCLUDE += arch-$ARCH/$d/$i" >> $archdir/Makefile.externs; fi
    if [ ${i/*./} == c ]; then echo "CSRC += arch-$ARCH/$d/$i" >> $archdir/Makefile.externs; fi
  done
  cd $archdir
done
echo "Done linking. rerun make."

