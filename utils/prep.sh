#!/bin/bash

DIR=bin_cyclist
EXE=sl_prove.native

[ ! -e all.itarget ] && echo "Must be run from cyclist root dir" && exit 1 

cd ..
rm -rf $DIR
git clone https://github.com/ngorogiannis/cyclist $DIR
rm -rf $DIR/.git
cd $DIR
make $EXE
LNK=$(readlink $EXE)
rm $EXE
cp $LNK $EXE
strip $EXE
rm -rf _build examples/cctests.tgz
cd ..
tar zcf ${DIR}.tar.gz $DIR

