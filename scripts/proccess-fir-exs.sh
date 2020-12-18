#!/bin/bash
FIRRTL_FILES=./src/test/resources/chiselucl/examples/*.fir
for f in $FIRRTL_FILES
do 
  echo "\nGenerating UCLID5 for $f ..."
  time (sbt "runMain chiselucl.FirrtlToUclid $f" &>/dev/null)
done
rm ./*.v
