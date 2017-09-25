#======================================================================
# mansrc2html.awk -- Awk script to convert .mansrc files to .html files
#
# Adapted from Daylight utility "mansrc_to_html.awk" written by Jack
# Delaney.
#======================================================================


BEGIN	{  ntok = ""; in_block = "FALSE"; in_pre = "FALSE" }
END     {  
        print "</BODY></HTML>"
        }
#
#
#
/^<br>/ { print "<br>" }
#
#  <ti> defines the function name.
#
/^<ti>/ {
 	ntok = "TI"
	print "<HTML><HEAD><TITLE>"
	}
#
#  <fn> defines the function, object, or program name
#
/^<fn>/ {
        ntok = "FN"
	}
#
#  <un> defines the unix command line syntax.
#
/^<un>/ {
	ntok = "UN"
	print "<H3>Unix Synopsis</H3>"
	}
#
# <fi> for files used by a program.
#
/^<fi>/ {
	ntok = "PP"
	print "<H3>Files</H3>"
	}
#
# <op> defines options to a program.
#
/^<op>/ {
	ntok = "PP"
	print "<H3>Options</H3>"
	}
#
# <li> license required for a program.
#
/^<li>/ {
	ntok = "PP"
	print "<H3>Daylight License</H3>"
	}
#
#  <fp> defines a generic function prototype.  By convention, do not use 
#       arguement names, just the types.
#
/^<fp>/ {
	ntok = "FP"
	print "<H2>"
	}
#
#  <fc> defines the C-language prototype, with included file(s) and variable
#       type-declarations.  By convention, us arguement names and types but
#       only use the return type (no name).
#
/^<fc>/ {
	if (ntok == "FP") print "</H2>"
	ntok = "PP"
	print "<H3>C Prototype</H3>"
	}
#
#  <ff> defines the FORTRAN-language prototype, with included file(s) and
#       variable type-declarations.  Use arg names and return type with the
#       function, then args and arg-types in declaration lines.
#
/^<ff>/ {
	ntok = "PP"
	print "<H3>Fortran Prototype</H3>"
	}
#
#  <od> is for section 5 man pages -- the one-line object description.
#
/^<od>/ {
	ntok="OD"
	print "<H3>Object Synopsis</H3>"
	}
#
#  <fd> defines the function description text.  Note that a line cannot
#       begin with a single-quote or this script barfs.
#
/^<fd>/ {
	ntok = "PP"
	print "<H3>Description</H3>"
	}
#
#  <fr> defines the functions return value.  A text description of the
#       returned value under normal and exception conditions.
#
/^<fr>/ {
	ntok = "PP"
	print "<H3>Return Value</H3>"
	}
#
# <ex> examples.
#
/^<ex>/ {
	ntok = "PP"
	print "<H3>Examples</H3>"
	}
#
#  <seealso> defines the list of functions in the 'see also' section of the manual.
#       These should be entered one-per-line, with a trailing comma except for
#       the last one.
#
/<seealso>/ {
	ntok = "SA"
	print "<H3>Related topics</H3>"
	}
#
#  <bugs> defines the 'BUGS' section of the manual, which is a text section 
#         and can be absent.
#
/<bugs>/ {
	ntok="PP"
	print "<H3>Bugs</H3>"	
	}
#
#  <ob> is a list of the objects to which the function applies.  This
#       section is not printed, but rather is used to generate information
#       for the section 5 (objects) man pages.
/<ob>/ {
	ntok="OB"
       }
#
#  Begin and end of 'indented' sections, which are also set off from the rest
#  of the text as a separate paragraph.  Multiple '<bi>' are legal, the flag
#  in_block keeps track.
#
/^<bi>$/ { if (in_block == "FALSE") print "<blockquote>"
                   else print "<p>";
		   in_block = "TRUE" }
#
/^<ei>$/ { print "</blockquote>"; in_block = "FALSE" }
#
#  Begin and end of 'preformatted' sections, which are also set off from the 
#  rest of the text as a separate paragraph.  Multiple '<pre>' are legal,
#  the flag in_pre keeps track.
#
/^<bp>$/ { if (in_pre == "FALSE") print "<pre>"
                   else print "<p>";
		   in_pre = "TRUE" }
#
/^<ep>$/ { print "</pre>"; in_pre = "FALSE" }
#
#       Process the tokens.
#
/^[^<]/  {
	if (ntok == "TI")
		{
		print $1 "</TITLE></HEAD><BODY>"
		section_number = $2
		}
	if (ntok == "FN")
		{
		if (section_number != "3" && section_number != "3ob")
			{
			if (did_name != "TRUE")
				{
				print "<H3>Name</H3>"
				}
			did_name = "TRUE"
			print $0"<br>"
			}
		}		
	if (ntok == "FP")
		{
		if ($0 ~ /^$/) print "<p>"
		else if ($0 ~ /^.br$/) print "<br>"
		else print $0"<br>"
		}
	if (ntok == "PP")
		{
		if ($0 ~ /^$/) print "<p>"
		else if ($0 ~ /^<br>$/) print "<br>"
		else 
                  {
		     #  Substitute &, <, > in the html text.
 		     #
                     gsub( /\&/, "\\&amp;" );
                     gsub( /\</, "\\&lt;" );
                     gsub( /\>/, "\\&gt;" );
                     print $0;
                  }
		}
	if (ntok == "UN")
		{
		if ($0 ~ /^$/) print "<p>"
		else if ($0 ~ /^<br>$/) print "<br>"
		else print $0"<br>"
		}
	if (ntok == "OD")
		{
		#  Substitute &, <, > in the html text.
 		#
                gsub( /\&/, "\\&amp;" );
                gsub( /\</, "\\&lt;" );
                gsub( /\>/, "\\&gt;" );
                print $0;
		ntok = ""
		}
	if (ntok == "SA")
		{
		not_done = "TRUE"
		if (index($1, "()") || index($1, "(3)"))
			{
			split($1, arr, "(");
			print "<A href=../man3/"arr[1]".html>"arr[1]"(3)</A>";
			not_done = "FALSE"
			}
		if (index($1, "(1)"))
			{
			split($1, arr, "(");
			print "<A href=../man1/"arr[1]".html>"arr[1]"(1)</A>";
			not_done = "FALSE"
			}
		if (index($1, "(5)"))
			{
			split($1, arr, "(");
			print "<A href=../man5/"arr[1]".html>"arr[1]"(5)</A>";
			not_done = "FALSE"
			}
		if (not_done == "TRUE")
			{				
			print $0
#			split($1, arr, ",");
#			print "<A href=../"arr[1]".html>"arr[1]"</A>";
			}
		}
	}
#
#       Allow a blank line in a text section.
#
/^$/	{ if ((ntok == "PP") || (ntok == "SA")) print "<p>" }

