#  mansrc2man.awk -- Awk script file to convert .mansrc files to .man files
#
#  Adapted from Daylight utility `compile_mansrc.awk', written by Jack Delany.
#  This is generalized to handle programs(1), fucntions(3), and types(5)
# 
#----------------------------------------------------------------------------
#  Stuff for all sections
#----------------------------------------------------------------------------
BEGIN	{
	date    = "\"23 Oct 1995\""
	version = "\"CEX 0.33\""
	}
#
#  <description> defines the function description text.  Note that a line
#  cannot begin with a single-quote or this script barfs.
#
/<description>/ {
	ntok = "PP"
	print ".SH DESCRIPTION"
	}
#
#  <diagnostics> defines the 'DIAGNOSTICS' section of the manual which is
#  a text section and can be absent.
#
/<diagnostics>/ {
	ntok="PP"
	print ".SH DIAGNOSTICS"
	}
#
#  <seealso> defines the list of functions in the 'see also' section of the
#	manual.  These should be entered one-per-line, with a trailing comma
#	except for the last one.
#
/<seealso>/ {
	ntok = "PP"
	print ".SH \"SEE ALSO\""
	}
#
#  <bugs> defines the 'BUGS' section of the manual, which is a text section 
#         and can be absent.
#
/<bugs>/ {
	ntok="PP"
	print ".SH BUGS"
	}
#----------------------------------------------------------------------------
#  Stuff for section 1 (programs)
#----------------------------------------------------------------------------
#
#  <pn> defines the name and one-line whatis text
#
/<pn>/	{
	ntok    = "NAME"
	header  = "\"CX PROGRAMS\""
	section = "1cx"
	}
#
#  <ps> defines the program synopsis for section 1 man pages
#
/<ps>/ {
	ntok="PP"
	print ".SH SYNOPSIS"
	}
#
#  <options> is for section 1 man pages, the options section
#
/<options>/ {
	ntok="PP"
	print ".SH OPTIONS"
	}
#----------------------------------------------------------------------------
#  Stuff for section 3 (functions)
#----------------------------------------------------------------------------
#
#  <fn> defines the function name.
#
/<fn>/ {
 	ntok    = "NAME"
	header  = "\"CX LIBRARY FUNCTIONS\""
	section = "3cx"
	}
#
#  <index> defines an index field for the windex utility (man -k, apropos)
#
/<index>/ {
	ntok = "IN"
	}
#
#  <fp> defines a generic function prototype.  By convention, do not use 
#       arguement names, just the types.
#
/<fp>/ {
	ntok = "FP"
	}
#
#  <fc> defines the C-language prototype, with included file(s) and variable
#       type-declarations.  By convention, us arguement names and types but
#       only use the return type (no name).
#
/<fc>/ {
	ntok = "PP"
	print ".SH SYNOPSIS: ANSI-C USAGE"
	}
#
#  <ff> defines the FORTRAN-language prototype, with included file(s) and
#       variable type-declarations.  Use arg names and return type with the
#       function, then args and arg-types in declaration lines.
#
/<ff>/ {
	ntok = "PP"
	print ".PP"
	print ".SH SYNOPSIS: FORTRAN USAGE"
	}
#
#  <returns> defines the RETURNS section.  A text description of the
#       returned value under normal and exception conditions.
#
/<returns>/ {
	ntok = "PP"
	print ".SH RETURNS"
	}
#
#  <objects> is a list of the objects to which the function applies.  This
#       section is not printed, but rather is used to generate information
#       for the section 5 (objects) man pages.
/<objects>/	{
	ntok="OB"
	}
#----------------------------------------------------------------------------
#  Stuff for section 5 (objects == file formats)
#----------------------------------------------------------------------------
#
#  <pt> defines a primitive type name
#
/<pt>/ {
 	ntok    = "NAME"
	header  = "\"CX PRIMITIVE TYPES\""
	section = "5cx"
	}
#
#  <on> defines an object name
#
/<on>/ {
 	ntok    = "NAME"
	header  = "\"CX OBJECT TYPES\""
	section = "5cx"
	}
#
#  <os> is for section 5 man pages -- the one-line object description.
#
/<os>/ {
	ntok="OS"
	print ".SH SYNOPSIS"
	}
#----------------------------------------------------------------------------
#  Process the tokens.
#----------------------------------------------------------------------------
/^[^<]/  {
	if (ntok == "NAME")
		{
		print ".TH", $1, section, date, version, header
		print ".SH NAME"
		print $0
		}
	if (ntok == "IN")
		{
		print ".IX", $0
		}
	if (ntok == "OD")
		{
		print $0
		ntok = ""
		}
	if (ntok == "PP")
		{
		if ($0 ~ /^$/) print ".PP"
		else print $0
		}
	}
#
#       Allow a blank line in a text section.
#
/^$/	{
	if (ntok == "PP") print ".PP"
	}
#
#       Allow a line-separator in a text section.
#
/^.br$/ { print $0 }
