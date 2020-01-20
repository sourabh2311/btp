#!/bin/bash

cwd=$(pwd)
cp $1 $TIGER/$1
cd $TIGER
sml <<MY_QUERY
CM.make ("sources.cm");
Main.compile "$1";
MY_QUERY
rm $1
mv $1.s $cwd/$1.s
cd $cwd
