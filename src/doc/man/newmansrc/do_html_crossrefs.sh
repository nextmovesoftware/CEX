#!/bin/sh
#
# Make cross references for one set of html pages against another.  This should
# work generally, but it is specifically meant to add cross-references for
# the section 3ob man pages (object descriptions), referring each object to
# the section 3 man pages (functions) that apply.
#
# An index file of all of the object-function pairs must be created before
# using this program.  It should be given as the first arguement (srcpath).
#
# Takes four parameters: the name of the cross-reference file which is
# searched for applicable functions ("srcpath") and section ("srcsect"), and 
# the name of the directory and section to which cross references should be 
# added.
#

if [ $# != 4 ] ; then
  echo "error - requires four parameters"
  echo "usage: $0 srcpath srcsect destdir destsect"
  echo "       e.g. $0 mansrc3/XREF-LIST 3 mansrc3ob 3"
  exit 1
fi

srcpath=$1
srcsect=$2
destdir=$3
destsect=$4

cutline="<!---------- automatically-generated cross references ----------!>"

#--------------------------------------------------
# First, chop off stuff added from previous
# invocations of this program.  Otherwise, we
# would just keep adding the same stuff over
# and over.
#--------------------------------------------------


( cd $destdir
  tmpfile=./tmpxref.$$
  for f in *.html ; do
    cat $f |
      (while read line ; do
	 if [ "$line" = $cutline ] ; then
	   break
	 fi
	 echo "$line"
       done
      ) >$tmpfile
    if [ -s $tmpfile ] ; then
      mv $tmpfile $f
    else
      rm $tmpfile
    fi
  done
)

#--------------------------------------------------
# Next, add the "cutline" and a paragraph break at
# the end of each destination file.  This is so
# that the "See Also" lines that are added will be
# in a separate paragraph from the ones that are
# already there.
#--------------------------------------------------

( cd $destdir
  for f in *.html ; do
    (echo "$cutline"; echo "<p>") >>$f
  done
)

#--------------------------------------------------
# Now do the cross referencing.  Search each src
# file for its object references.
#--------------------------------------------------

( cat $srcpath | (
    while true ; do
	read obj funct
 	if [ $? != 0 ] ; then
	  break;
	fi
	destfile=$destdir/$obj.$destsect
	if [ ! -w $destfile ] ; then
	  echo "Yikes! Unknown object name: $obj ($destfile)"
        else
	echo "<A href=../man${srcsect}/${funct}.html>${funct}(${srcsect})</A>"  >> $destfile
        fi
      done)
)

