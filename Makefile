#----------------------------------------------------------------------------
#  $CX_ROOT/Makefile -- recursively make subdirectories
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
#
#  NOTE: CX_ROOT must be defined as the current directory (the place where
#        this file resides.  Everything else is relative to it.
#
#----------------------------------------------------------------------------

SHELL      = /bin/sh
SUBDIRS    = src
ALLSUBDIRS = $(SUBDIRS) doc


install:	
	sh $(CX_ROOT)/src/etc/cx_install
	@for d in $(SUBDIRS) ;          \
	do                              \
	   ( cd $$d;                    \
	     echo installing `pwd`;     \
	     if [ -r Makefile ] ; then  \
		make install;           \
	     fi ;                       \
	   ) ;                          \
	done
	@echo "+-------------------------------------------------"
	@echo "| CX library and applications are installed."
	@echo "+-------------------------------------------------"

all:	
	sh $(CX_ROOT)/src/etc/cx_install
	@for d in $(SUBDIRS) ;          \
	do                              \
	   ( cd $$d;                    \
	     echo making all of `pwd`;  \
	     if [ -r Makefile ] ; then  \
		make all;               \
	     fi ;                       \
	   ) ;                          \
	done
	@echo "+-------------------------------------------------"
	@echo "| CX library, applications, & test prog are built."
	@echo "+-------------------------------------------------"

clean:  
	@echo ""
	@echo "Cleaning CX source directories."
	@echo ""
	@for d in $(SUBDIRS) ;          \
	do                              \
	   ( cd $$d;                    \
	     echo cleaning `pwd`;       \
	     if [ -r Makefile ] ; then  \
		make clean;             \
	     fi ;                       \
	   ) ;                          \
	done
	@echo ""
	@echo "Removing CX binaries."
	@echo ""
	rm -f $(CX_ROOT)/bin/*
	@echo ""
	@echo "Removing CX libraries."
	@echo ""
	rm -rf $(CX_ROOT)/lib/*
	@echo ""
	@echo "Removing CX documents."
	@echo "";
	( cd doc; make clean; )
	@echo ""
	@echo "Removing CX headers."
	@echo ""
	rm -f $(CX_ROOT)/include/*.h
	rm -f $(CX_ROOT)/include/*.inc
	@echo "+-------------------------------------------------"
	@echo "| All CX directories have been made clean."
	@echo "+-------------------------------------------------"

cleanall:	clean
	@echo ""
	@echo "Purging CX source directories."
	@echo ""
	@for d in $(ALLSUBDIRS) ;       \
	do                              \
	   ( cd $$d;                    \
	     echo purging `pwd`;       \
	     if [ -r Makefile ] ; then  \
		make cleanall;          \
	     fi ;                       \
	   ) ;                          \
	done
	rm -f $(CX_ROOT)/include/*
	@echo "+---------------------------------------"
	@echo "| All CX directories have been purged."
	@echo "+---------------------------------------"

distclean:	cleanall
