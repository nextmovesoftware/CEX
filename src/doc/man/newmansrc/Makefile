#----------------------------------------------------------------------------
#  $CX_ROOT/src/doc/man/mansrc/Makefile -- compile CX man pages
#
#----------------------------------------------------------------------------
#  Usage:
#     make mansrc ..... make & install UNIX man pages
#     make htmlman .... make & install HTML man pages
#
#     make ............ same as make install
#     make all ........ makes both UNIX and HTML man pages
#     make install .... make & install UNIX man pages
#     make clean ...... cleans out everything except sources
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

MANSRCDIR = $(CX_ROOT)/src/doc/man/mansrc
HTMLDIR   = $(CX_ROOT)/doc/html

#----------------------------------------------------------------------------
#  Subdirectories containing mansrc files
#----------------------------------------------------------------------------

SUBDIRS = mansrc1 mansrc3 mansrc5

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers, install applics
#----------------------------------------------------------------------------

install:	mansrc xrefs
		@echo "+-------------------------------------------------"
		@echo "| CX manual pages are compiled."
		@echo "+-------------------------------------------------"

all:		install htmlman

#----------------------------------------------------------------------------
#  
#----------------------------------------------------------------------------

MANSCRIPT  = $(MANSRCDIR)/mansrc2man.sh
AWKSCRIPT  = $(MANSRCDIR)/mansrc2man.awk
XREFSCRIPT = $(MANSRCDIR)/xrefmans.sh
HMANSCRIPT = $(MANSRCDIR)/mansrc2html.sh
HAWKSCRIPT = $(MANSRCDIR)/mansrc2html.awk
HXREFSCRIPT= $(MANSRCDIR)/xrefhtml.sh
INDEX1     = $(MANSRCDIR)/index_html1.sh
INDEX3     = $(MANSRCDIR)/index_html3.sh
INDEX5     = $(MANSRCDIR)/index_html5.sh

mansrc:		mansrc1 mansrc3 mansrc5

mansrc1:	mansrc1/*
		@cd mansrc1; echo "Compiling `pwd`"; \
		find . -name "*.mansrc" -print \
		   -exec $(MANSCRIPT) $(AWKSCRIPT) {} $(MANDIR) 1 \;

mansrc3:	mansrc3/*
		@cd mansrc3; echo "Compiling `pwd`"; \
		find . -name "*.mansrc" -print \
		   -exec $(MANSCRIPT) $(AWKSCRIPT) {} $(MANDIR) 3 \;

mansrc5:	mansrc5/*
		@cd mansrc5; echo "Compiling `pwd`"; \
		find . -name "*.mansrc" -print \
		   -exec $(MANSCRIPT) $(AWKSCRIPT) {} $(MANDIR) 5 \;

xrefs:	$(MANDIR)/man1cx/*.1* $(MANDIR)/man3cx/*.3* $(MANDIR)/man5cx/*.5*
	@echo "Crossreferencing man pages sections 5 vs 3"; \
	$(XREFSCRIPT) mansrc3 $(MANDIR)/man5cx


htmlman:	hmansrc1 hmansrc3 hmansrc5

hmansrc1:	mansrc1/*
		@cd mansrc1; echo "Compiling HTML `pwd`"; \
		find . -name "*.mansrc" -print \
		   -exec $(HMANSCRIPT) $(HAWKSCRIPT) {} $(MANDIR) 1 \;
		$(INDEX1) . $(HTMLDIR)/man1cx/index.html

hmansrc3:	mansrc3/*
		@cd mansrc3; echo "Compiling HTML `pwd`"; \
		find . -name "*.mansrc" -print \
		   -exec $(HMANSCRIPT) $(HAWKSCRIPT) {} $(MANDIR) 3 \;
		$(INDEX3) . $(HTMLDIR)/man3cx/index.html

hmansrc5:	mansrc5/*
		@cd mansrc5; echo "Compiling HTML `pwd`"; \
		find . -name "*.mansrc" -print \
		   -exec $(HMANSCRIPT) $(HAWKSCRIPT) {} $(MANDIR) 5 \;
		$(INDEX5) . $(HTMLDIR)/man5cx/index.html

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
