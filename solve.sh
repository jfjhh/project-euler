#!/bin/bash

RUNDIR="`pwd`"
DIR="`dirname $(readlink -f $0)`"

cd $DIR
for d in `find . -maxdepth 1 -type d -print | sort -n`; do
	if [[ "$d" != "." ]]; then
		cd $d
		printf "\033[0;1mSolving Problem %s...\n\033[0;32m" \
			`basename $d`
		csi -q < "p.scm" | tee "solution.txt"
		printf "\033[0m\n"
		cd $DIR
	fi
done

cd $RUNDIR

