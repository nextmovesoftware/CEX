#!/bin/sh
#
#

if [ $# != 2 ] ; then
  echo "requires two parameters"
  echo "usage: $0 srcdir destpath"
  exit 1
fi

srcdir=$1
destpath=$2
myname=`basename $0`

#
# See if index is already up to date.  If so, we're done.
#

cd $srcdir

if [ -r $destpath ] ; then 
  newer=`find . -follow -name "*.mansrc" -newer $destpath -print`
  if [ -z "$newer" ] ; then
    echo "$myname: HTML 1 index \"$destpath\" is up to date."
    exit 0
  fi
fi

echo "$myname: HTML index 1 $destpath is out of date, rebuilding ..."

rm -f $destpath

cat <<EOF
<HTML><HEADER>
<TITLE>CEX Programs Reference Manual</TITLE>
</HEADER><BODY>
<H2 ALIGN=CENTER>CEX Programs<br>Reference Manual</H2><p>
EOF > $destpath
#
# Collect all of the program names
#
(
  find . -name "*.mansrc" -exec awk '
                   BEGIN { ntok = "" ; split(FILENAME, temparr, "/");
                           split(temparr[2], namearr, "."); }
                   /^<fn>/ { ntok = "FN" }
                   /^<un>/ { ntok = "" }
                   /^<op>/ { ntok = "" }
                   /^<fd>/ { ntok = "" }
                   /^[^<]/ { if (ntok == "FN") {
         printf("%s <A HREF=%s.html>%s</A> ", $1, namearr[1], $1);
         printf("%s<br>\n", substr($0, index($0, " "))) 
         } } ' {} \; 
)  | sort | (
#
#  Write out the sorted versions, without the first argument (the sort key).
#
  while true ; do
    read sortkey rest
    if [ $? != 0 ] ; then
      break
    fi
    echo $rest
  done) >> $destpath

cat <<EOF
</BODY></HTML>
EOF >> $destpath