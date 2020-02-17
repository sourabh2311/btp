#!/bin/bash

sml <<MY_QUERY
CM.make ("sources.cm");
Main.compile("TestFiles/$1", $2);
MY_QUERY
