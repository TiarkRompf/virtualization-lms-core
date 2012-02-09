#!/bin/bash

# check args
if [ $# -ne 1 ] 
then
    echo "use: $0 <generated PDE1>"
    exit 1
fi

# check file
if [ ! -f $1 ]
then
    echo not found: $1
    exit 1
fi

# cleanup and copy files
echo -e "\n\n  PDE1 Experiment\n\n"
echo -e "Cleanup...\n"
rm -rf test
rm -rf scala
mkdir test

echo -e "Copying files...\n"
cp ../test-src/epfl/test7-mdarray/original-mdarray/*.scala test/
cp MainPDE.scala test/
cp $1 test/

echo -e "Compiling...\n"
scalac test/*.scala


# run benchmark
echo -e "Running...\n"
time scala -cp . scala.virtualization.lms.epfl.test7.original.Main
time scala -cp . scala.virtualization.lms.epfl.test7.original.Main
time scala -cp . scala.virtualization.lms.epfl.test7.original.Main
time scala -cp . scala.virtualization.lms.epfl.test7.original.Main
time scala -cp . scala.virtualization.lms.epfl.test7.original.Main

echo Done
