#!/bin/bash

BENCHMARKDIR=benchmark_programs
BENCHMARKSINGLEINSTDIR=benchmark_single_instruction
UTILDIR=util
COMPILDIR=/Users/pthalamy/Studies/CMU/benchmark/build

usage () {
    echo usage is $0 INSTRUCTION_TYPE NUMBER_OF_ITERATIONS 
}

benchmark_size () {
    FILE_NAME=$1
    ITERATIONS=$2

    FILE_SIZE=`ls -l $BENCHMARKSINGLEINSTDIR/$FILE_NAME | awk '{print $5}'`
    # 4 is the size of one hex byte in number of caracters
    UNITARY_SIZE=`expr $FILE_SIZE / 4`
    TOTAL_SIZE=`expr $UNITARY_SIZE \* $ITERATIONS`

    echo "#define BENCHMARKSIZE \"$TOTAL_SIZE\"" > /Users/pthalamy/Studies/CMU/benchmark/build/src-bobby/meldinterp-runtime/benchmark_name.h

}

generate_benchmark () {
    if [ ! -f $BENCHMARKSINGLEINSTDIR/$1 ]
    then
	echo The single instruction benchmark for $1 does not exist
	exit 1
    fi 	

    if [ -f $BENCHMARKDIR/bench_$1_$2.bb ]
    then
	echo The benchmark program is already generated
	echo calculating size
	benchmark_size $1 $2
	echo Onto compilation phase
	$UTILDIR/cpbench_blocks $BENCHMARKDIR/bench_$1_$2.bb
	compil
	transfer_benchmark
    else
	echo Generating the benchmark programs for $2 iterations of $1
	$UTILDIR/meldvm_benchmark_generator.pl $1 $2 > $BENCHMARKDIR/bench_$1_$2.bb
	echo calculating size
        benchmark_size $1 $2
	$UTILDIR/cpbench_blocks $BENCHMARKDIR/bench_$1_$2.bb
	echo Onto compilation phase
	compil
	transfer_benchmark
    fi

}

compil() {
    cd $COMPILDIR
    . $COMPILDIR/initbb.sh
    cd $COMPILDIR/apps/sample-meld
    export BB=block && make
}

transfer_benchmark() {
    echo Transferring and running benchmark
    sudo avrdude -F -p x256a3 -c stk600  -P usb -U flash:w:$COMPILDIR/apps/sample-meld/arch-blocks/ends.hex -U eeprom:w:$COMPILDIR/apps/sample-meld/arch-blocks/ends.eep -V -v -e
}


if [ $# != 3 ] ; then
    usage
fi

INSTR=$1
ITER=$2

generate_benchmark $INSTR $ITER
#generate_gnuplot $INSTR $ITER $COMMENT

