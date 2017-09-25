#!/bin/sh
#
#  Shell script to convert .mansrc files to .man files for nroff(1).
#

if [ $# != 4 ] ; then
   echo "usage: $0 awk_script mansrc_file man_dir section"
   exit 1
fi

# the input filename.
input=$2

# the output filename.
output=$3/man$4cx/`basename $2 mansrc`$4"cx"

awk -f $1 $input > $output

exit 0
