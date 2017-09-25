#!/bin/sh
#
#  Cross references one set of man pages against another.  This is used to
#  add cross-references to the section 5 man pages (object descriptions),
#  referring each object to the section 3 man pages (functions) that apply.
#
#  Each section 3 .mansrc file has a non-printing <objects> section containing
#  a list of the objects to which the function applies.  This shell script
#  takes each section 5 man page, gets the object's name by striping off the
#  .mansrc suffix, and searches the section 3 man pages for functions that
#  mention the object's name.  When the list is complete, each function is
#  appended to the nroff version of the section 5 man page (NOT to .mansrc!).
#
#  Takes two parameters: the name of the directory that is to be searched
#  for applicable functions ("srcdir"), and the name of the directory to
#  which cross references should be added ("destdir").
#
#  Modified by Dave Weininger from the Daylight development utility
#  `do_man_crossrefs.sh' written by Craig James
#

if [ $# != 2 ] ; then
   echo "error - requires two parameters"
   echo "usage: $0 srcdir destdir"
   exit 1
fi

srcdir=$1
destdir=$2

#----------------------------------------------------------------------------
# Search each source file for its object references and crossreference
#----------------------------------------------------------------------------

(
   cd $srcdir
   for src in *.mansrc ; do
      name=`basename $src .mansrc`
      echo "looking for references to objects in "$name" ..."

      objects=`cat $src |
      (
         while true ; do
            read line
            if [ $? != 0 -o "$line" = "<objects>" ] ; then
               break;
            fi
         done
         cat
      )`

      for dest in $objects ; do
         destfile=$destdir/$dest.5cx
         if [ ! -w $destfile ] ; then
            echo "Unknown object name: $dest ($destfile)"
         else
            echo "${name}(3cx)" >>$destfile
         fi
      done
   done
)
