#! /bin/sh

# NOTE: THIS CRUDE SCRIPT SHOULD BE REPLACED BY A PROPER MAKE FILE !

#gnatmake -aPdir ../lib/m1.adb $1.adb
gnatmake -aPdir ../../lbr_ada/src/* $1.adb

#gnatmake $1.adb
[ $? -ne 0 ] && exit 1;

#echo "copying to /opt/m-1/bin"
#cp $1 /opt/m-1/bin
cp $1 ../bin
#echo done
#ls -l /opt/m-1/bin/$1
#ls -l ../../../bin/$1

exit