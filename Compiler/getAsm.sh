#!/bin/bash

cd TestFiles/
cp $1 ../$1
cd ..
sml <<MY_QUERY
CM.make ("sources.cm");
Main.compile ("$1", []);
MY_QUERY
rm $1
