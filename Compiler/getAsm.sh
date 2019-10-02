#!/bin/bash

cd TestFiles/
cp $1 ../$1
cd ..
sml <<MY_QUERY
CM.make ("sources.cm");
Main.compile "$1";
MY_QUERY
cat $1.s >> $1.tmp
cat runtime.s >> $1.tmp
rm $1.s
rm $1
mv $1.tmp $1.s
