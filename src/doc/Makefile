#----------------------------------------------------------------------------
#  $CX_ROOT/src/doc/Makefile -- recursively make subdirectories
#----------------------------------------------------------------------------
#  Usage:
#     make ............ same as make install
#     make all ........ makes everything, including test programs
#     make install .... make & install  everything except test programs
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
SHELL  = /bin/sh

#----------------------------------------------------------------------------
#  Note: subdirectories are listed in the order that they must be built
#----------------------------------------------------------------------------

SUBDIRS  = man
TEXTDIR  = $(CX_ROOT)/doc/text
HTMLDIR  = $(CX_ROOT)/doc/html

#----------------------------------------------------------------------------
#  Default "make" is same as "make install"
#----------------------------------------------------------------------------

install:	$(TEXTDOCS) $(HTMLDOCS) html
		@for d in $(SUBDIRS) ;      \
		do                          \
	   	( cd $$d;                   \
	     	  echo installing `pwd`;    \
	     	  if [ -r Makefile ] ; then \
		    make install;           \
	     	  fi ;                      \
	   	) ;                         \
		done
		@echo "+---------------------------------------------------"
		@echo "| All CX docs are compiled and installed."
		@echo "+---------------------------------------------------"

all:		$(TEXTDOCS) $(HTMLDOCS)
		@for d in $(SUBDIRS) ;      \
		do                          \
	   	( cd $$d;                   \
	     	  echo making all of `pwd`; \
	     	  if [ -r Makefile ] ; then \
		    make all;               \
	     	  fi ;                      \
	   	) ;                         \
		done
		@echo "+---------------------------------------------------"
		@echo "| All CX docs are compiled."
		@echo "+---------------------------------------------------"

#----------------------------------------------------------------------------
#  html: copy all .html to HTMLDIR
#----------------------------------------------------------------------------

html:		html/index.html
		@echo "Installing HTML pages..."
		@cp html/*.html $(HTMLDIR)/other
		@rm -rf $(HTMLDIR)/other/index.html
		@cp html/index.html $(HTMLDIR)

#----------------------------------------------------------------------------
#  textdocs: copy all files in text subdir to TEXTDIR
#----------------------------------------------------------------------------

text:		
		cp text/* $(TEXTDIR)

#----------------------------------------------------------------------------
#  Clean up.
#----------------------------------------------------------------------------

clean:
	@for d in $(SUBDIRS) ;          \
	do                              \
	   ( cd $$d;                    \
	     echo cleaning `pwd`;       \
	     if [ -r Makefile ] ; then  \
		make clean;             \
	     fi ;                       \
	   ) ;                          \
	done
	@echo "+-----------------------------------------------------"
	@echo "| CX document source directories have been made clean."
	@echo "+-----------------------------------------------------"

cleanall:	clean
	@for d in $(SUBDIRS) ;          \
	do                              \
	   ( cd $$d;                    \
	     echo cleaning `pwd`;       \
	     if [ -r Makefile ] ; then  \
		make cleanall;          \
	     fi ;                       \
	   ) ;                          \
	done
	@echo "+-----------------------------------------------------"
	@echo "| CX document source directories have been purged."
	@echo "+-----------------------------------------------------"

