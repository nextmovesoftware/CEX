#----------------------------------------------------------------------------
#  $CX_ROOT/src/doc/man/Makefile -- make and install CX man pages
#----------------------------------------------------------------------------
#  Usage:
#     make ............ same as make install
#     make all ........ makes everything, including test programs
#     make install .... make & install  everything except test programs
#     make clean ...... cleans out everything except sources
#
#     make postman .... makes postscript man pages
#     make htmlman .... makes HTML man pages
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

#----------------------------------------------------------------------------
#  O/S specific definitions
#----------------------------------------------------------------------------

include $(CX_ROOT)/include/makefile.mdep

#----------------------------------------------------------------------------
#  Installation destinations and targets
#----------------------------------------------------------------------------

MANDIR  = $(CX_ROOT)/doc/man
POSTMAN = $(CX_ROOT)/doc/postman
HTMLDIR = $(CX_ROOT)/doc/html

MANCACHE = man3cx/Intro.3cx

#----------------------------------------------------------------------------
#  default -- compile mansrc to man pages, install in MANDIR
#----------------------------------------------------------------------------

install:	mansrc
		rm -rf $(MANDIR)/man1cx/*.1cx; cp man1cx/*.1cx $(MANDIR)/man1cx
		rm -rf $(MANDIR)/man3cx/*.3cx; cp man3cx/*.3cx $(MANDIR)/man3cx
		rm -rf $(MANDIR)/man5cx/*.5cx; cp man5cx/*.5cx $(MANDIR)/man5cx
		@echo "+-------------------------------------------------"
		@echo "| CX manual pages are up to date and installed."
		@echo "+-------------------------------------------------"

all:		install

#----------------------------------------------------------------------------
#  Manual source compilation
#----------------------------------------------------------------------------

$(MANCACHE):	mansrc/mansrc1/* mansrc/mansrc3/* mansrc/mansrc5/*
		@cd mansrc;                \
		echo installing `pwd`;     \
		if [ -r Makefile ] ; then  \
		   make mansrc;            \
		fi ;
		
mansrc:		$(MANCACHE)

#----------------------------------------------------------------------------
#  Special target converts installed man pages (not in src) to postscript
#----------------------------------------------------------------------------


postman:
		makepostman  $(POSTMAN)/man1cx.ps  $(MANDIR)/man1cx/*.1cx ; \
		makepostman  $(POSTMAN)/man3cx.ps  $(MANDIR)/man3cx/*.3cx ; \
		makepostman  $(POSTMAN)/man5cx.ps  $(MANDIR)/man5cx/*.5cx ;

htmlman:	mansrc/mansrc1/* mansrc/mansrc3/* mansrc/mansrc5/*
		@cd mansrc;                \
		echo installing `pwd`;     \
		if [ -r Makefile ] ; then  \
		   make htmlman;           \
		fi ;


#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:		


cleanall:	clean
	rm -f man1cx/*.1cx man3cx/*.3cx man5cx/*.5cx
