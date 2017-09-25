#----------------------------------------------------------------------------
#  $CX_ROOT/doc/Makefile -- just responds to make clean (cleans out docs)
#----------------------------------------------------------------------------
#  Usage:
#     make cleanall ...... cleans out everything (no sources)
#     make clean ...... cleans out everything (no sources)
#----------------------------------------------------------------------------
#  Contributing author and institution: Dave Weininger, Daylight CIS, Inc.
#
#  This source code is contributed to the public domain and may be freely
#  copied and redistributed for research, profit, fun or any other reason,
#  with these restrictions: (1) unmodified or functionally equivalent code
#  derived from CX code must contain this notice, (2) all derived code must
#  acknowledge the author and institution, and (3) the functional definition
#  of symbols starting CX_ or cx_ may not be changed (if you need to change
#  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
#----------------------------------------------------------------------------

SHELL      = /bin/sh
SUBDIRS    = html man postman text
ALLSUBDIRS = $(SUBDIRS)

default:
		@echo "oops: doc/Makefile only knows how to make clean"

cleanall:	clean
		@echo "Removing CX man pages..."
		@rm -rf man/man1cx/*.1cx man/man3cx/*.3cx man/man5cx/*.5cx
		@echo "Removing CX Postscript manuals..."
		@rm -rf postman/*.ps
		@echo "Removing CX HTML man pages..."
		@rm -rf html/man1cx/* html/man3cx/* html/man5cx/* html/*.html

clean:
		@echo "Removing CX document texts..."
		@rm -rf text/*
		@echo "Removing HTML documents..."
		@rm -f html/other/*.html html/index.html

