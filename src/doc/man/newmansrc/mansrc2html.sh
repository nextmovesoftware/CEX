#!/bin/sh
#
#  Shell script to convert MANSRC files to html format.
#
#

if [ $# != 4 ] ; then
        echo "usage: $0 awk_script mansrc_file man_dir section"
        exit 1
fi

# the input filename.
input=$2

# the output filename.
output=$3/html$4cx/`basename $2 .mansrc`.html

# Figure out which awk to use

nawk -f $1 $input > $output

exit 0
