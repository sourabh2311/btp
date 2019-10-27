#!/bin/bash

cwd=$(pwd)
cp $1 $TIGER/$1
cd $TIGER
sml <<MY_QUERY
CM.make ("sources.cm");
Main.compile "$1";
MY_QUERY
cat $1.s >> $1.tmp
cat runtime.s >> $1.tmp
rm $1.s
rm $1
mv $1.tmp $1.s
mv $1.s $cwd/$1.s
cd $cwd
