#!/bin/bash

# usage: 
#	runTests.sh        runs all files
#	runTests.sh 03     runs files >= 03
#	runTests.sh 03 06  runs files >= 03 && <= 06
#	runTests.sh 03 03  runs only file test03
# 

if (( $# == 0 )); then   # no parameters, run All
  START="00"
  STOP="99"
elif (( $# == 1 )); then
  START=$1
  STOP="99"
elif (( $# == 2 )); then
  START=$1
  STOP=$2
fi

# $1 filename with extension
runFile() {
    echo -n "Running $1 1, "
    F=`basename $1 .tex`
    lualatex $1 > /dev/null
    echo "2"
    lualatex $1 > /dev/null
    echo "Housekeeping ..."
    rm -fr $F.tmp $F.aux $F.toc $F.dvi $F.log 
    echo "done"
}

for FILE in `ls test??.tex  2>/dev/null`
do
#    F=`basename $FILE .tex`
    No="${FILE//[^[:digit:]]}"
    if (( No >= START && No <= STOP )); then
#       echo $FILE
      runFile $FILE 
    fi
done

#runFile test00.tex     # blendmode and transparency 
#runFile test01.tex     # grids
#runFile test02.tex     # liftpen
#runFile test03.tex     # transparency
#runFile test04.tex     # gridfont
#runFile test05.tex     # psdot
#runFile test06.tex     # arrows




exit 0




