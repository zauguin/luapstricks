#!/bin/bash

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

#for FILE in `ls */.tex  2>/dev/null`
#do
#    F=`basename $FILE`
#    ifnewer $SRC $DEST $F || compileLTX2PS $F
#done

runFile test00.tex     # blendmode and transparency 
runFile test01.tex     # grids
runFile test02.tex     # liftpen
runFile test03.tex     # transparency
runFile test04.tex     # gridfont
runFile test05.tex     # psdot




exit 0




