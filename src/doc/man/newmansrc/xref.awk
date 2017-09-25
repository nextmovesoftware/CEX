#======================================================================
# FILE:		xref.awk
# AUTHOR:	Jack Delany
# DESCRIPTION:
#       Collects all of the object references from the mansrc3 directory.
#       Used to build the cross references for the man pages for the 
#       object manuals.
#	
# Copyright (c) 1995, Daylight Chemical Information Systems, Inc.
#======================================================================

BEGIN	{
	ntok = ""
	split(FILENAME, temparr, "/");
	split(temparr[2], namearr, ".");
	}
#
/^<ob>/ { ntok = "OB" }
#
#       Process the tokens.
#
/^[^<]/  {
	if (ntok == "OB")
		{
		print $1, namearr[1]
		}
	}
