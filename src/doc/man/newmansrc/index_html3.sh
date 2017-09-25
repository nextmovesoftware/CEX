#!/bin/sh
#
#  Collect the index information from the individual mansrc files and 
#  reformat them for HTML.  This is a brute-force approach.
#
if [ $# != 3 ] ; then
  echo "requires three parameters"
  echo "usage: $0 srcdir destpath"
  echo ""
  echo "          srcdir - where the section 3cx pages are"
  echo "          destpath - full path to the output file"
  exit 1
fi
#

myname=`basename $0`
destpath=$3
tmpfile1=/tmp/${myname}1-$$
tmpfile2=/tmp/${myname}2-$$

rm -f $destpath


#
cat <<EOF
<HTML><HEADER>
<TITLE>CEX Library Reference Manual</TITLE>
</HEADER><BODY>
<H2 ALIGN=CENTER>CEX Library<br>Reference Manual</H2>
<P>
EOF > $destpath

#
# Collect all of the prototypes.
#
(
  cd $1;
  find . -name "*.mansrc" -exec awk '
                   /^<fp>/ { ntok = "FC" ; first_one = "TRUE" }
                  /^<fc>/ { ntok = "" }
                 /#include/ { split($0, temparr, "\"")
                            printf("%s %s %s\n", temparr[2], f_line, s_line) }
                 /^[^<]/  { if (ntok == "FC") {
                            if (first_one == "TRUE") {
                            f_line = $0
                            first_one = "FALSE" }
                            else { s_line = $0 } } } ' {} \; 
) | sort | (
#
# Reformat the prototypes, and add headers when the sections change.
#
  prevfile=xxx
  while true ; do
    read incfile proto 
    if [ $? != 0 ] ; then
      break
    fi
#
#  Headers for when sections change.  Each heading is an anchor, and
#  we write out a link to the anchor that goes at the top of the index.
#
    if [ $incfile != $prevfile ] ; then
      if [ $prevfile != xxx ] ; then
        echo "<p>Back to <A HREF="#CONTENTS">Table of Contents.</A><p>"
      fi
      prevfile=$incfile
      toolkit=`basename $incfile .h \
	  | sed -e s/dt_// \
		-e s/finger/fingerprint/ \
		-e s/progob/program-object/\
          | tr [a-z] [A-Z]`
      echo "<p><H3><A NAME = \"$toolkit\"</A>$toolkit Toolkit</H3><p>"
      ( echo "<LI>"
	echo "<A HREF = \"#$toolkit\">$toolkit Toolkit</A>"
        echo "</LI>"
	) >> $tmpfile1
    fi
#
#  Write the prototype with a link.
#
    echo $proto | sed -e 's/^[^(]*/<A HREF=&.html>&<\/A>/'
    echo "<br>"
  done)  >> $tmpfile2


#
# Finish the header and body (see "Widgets" right below)
#
cat <<***EOF*** >>$tmpfile1
<LI>
<A HREF = "#Widgets">Widgets Toolkit</A>
</LI>
</UL>
***EOF***

#
# Add "hard coded" links to Widgets.  The Widget man pages don't follow
# the same format as the regular Toolkit man pages.
#

cat <<***EOF*** >>$tmpfile2
<H3><A NAME="Widgets">Widgets Toolkit</A></H3>
<A HREF="dw_3d.html">dw_3d</A> - 3d "Trackball" widget</br>
<A HREF="dw_depict.html">dw_depict</A> - Depict widget</br>
<A HREF="dw_edgar.html">dw_edgar</A> - Edit Graphics Attributes widget</br>
<A HREF="dw_file.html">dw_file</A> - File widget</br>
<A HREF="dw_font.html">dw_font</A> - Font utility</br>
<A HREF="dw_grins.html">dw_grins</A> - GRINS widget</br>
<A HREF="dw_help.html">dw_help</A> - Help widget</br>
<A HREF="dw_msg.html">dw_msg</A> - Message widget</br>
<A HREF="dw_status.html">dw_status</A> - Status widget</br>
<A HREF="dw_tdt.html">dw_tdt</A> - TDT widget</br>

***EOF***

#
# Add final link at end
#
echo "<p>Back to <A HREF="#CONTENTS">Table of Contents.</A><p>" >>$tmpfile2

#
# Combine the header with the body of the links.
#

cat $tmpfile1 $tmpfile2 >$destpath
rm $tmpfile1 $tmpfile2
