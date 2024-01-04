#!/bin/bash
# compilation script for noisy - Linux
#

set -e

SRC="src/"
OBJ="obj/"
BIN="bin/"

flags="-O3 -ffpe-trap=invalid,zero,overflow -g -fbounds-check -fbacktrace"

[[ ! -d $OBJ ]] && mkdir $OBJ
[[ ! -d $BIN ]] && mkdir $BIN

declare -a names=()
names+=("noisy_modules.f90")
names+=("noisy.f90")

declare -a objs=()
for i in "${names[@]}"; do
	objs+=("${i/f90/o}")
done

declare -a srcs=()
for i in "${names[@]}"; do
	srcs+=("${SRC}${i}")
done

# clear .mod
rm -f ${OBJ}*.mod

echo "  Compiling noisy..."

gfortran ${flags} -J${OBJ} -c ${srcs[@]} 
gfortran ${flags} -o ${BIN}noisy ${objs[@]}

#chmod 775 ${BIN}noisy

rm -f ${objs[@]}

echo "  Compiled."
