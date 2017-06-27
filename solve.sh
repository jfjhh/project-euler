#!/bin/bash
#
# Solves all of the Project Euler problems done so far.
# Alex Striff.
#
# Run like an executable, i.e. by typing `./solve.sh`.
# Do not source this script (`. solve.sh` or `source solve.sh`).
#

RUNDIR="`pwd`"
DIR="`dirname $(readlink -f $0)`"

SOL_FILE="p.lisp"
SOL_OUT="solution.txt"

cd $DIR
for d in [0-9]*; do
	if [[ -d "$d" ]]; then
		cd $d
		printf "\033[0;1mSolving Problem %s ... \033[0;32m" \
			`basename $d`
		if [[ -f "$SOL_FILE" ]]; then
			sbcl --script "$SOL_FILE" | tail -1 | tee "$SOL_OUT"
			echo >> "$SOL_OUT"
		else
			echo "No code in directory!"
		fi
		printf "\033[0m\n"
		cd $DIR
	fi
done

cd $RUNDIR

