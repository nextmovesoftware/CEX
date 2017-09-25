#----------------------------------------------------------------------------
#  $CX_ROOT/src/applics/Makefile -- recursively make applics subdirectories
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

SHELL      = /bin/sh

#----------------------------------------------------------------------------
#  Note: subdirectories are listed in the order that they must be built
#----------------------------------------------------------------------------

# no fortran
#SUBDIRS      = utils cex mol wrl pdb connect chains rasmol

# real stuff
SUBDIRS    = utils cex mol wrl pdb connect chains bondage rasmol

# sans rasmol
# SUBDIRS    = utils cex mol wrl pdb connect chains bondage

ALLSUBDIRS = $(SUBDIRS)

install:
	@for d in $(ALLSUBDIRS) ;       \
	do                              \
	   ( cd $$d;                    \
	     echo installing `pwd`;     \
	     if [ -r Makefile ] ; then  \
		make install;           \
	     fi ;                       \
	   ) ;                          \
	done
	@echo "+--------------------------------------------------"
	@echo "| CX applics are compiled, linked, and installed."
	@echo "+--------------------------------------------------"

all:
	@for d in $(ALLSUBDIRS) ;       \
	do                              \
	   ( cd $$d;                    \
	     echo making all of `pwd`;  \
	     if [ -r Makefile ] ; then  \
		make all;               \
	     fi ;                       \
	   ) ;                          \
	done
	@echo "+----------------------------------------------------"
	@echo "| All CX applics & testprogs are compiled and linked."
	@echo "+----------------------------------------------------"

clean:
	@for d in $(ALLSUBDIRS) ;       \
	do                              \
	   ( cd $$d;                    \
	     echo cleaning `pwd`;       \
	     if [ -r Makefile ] ; then  \
		make clean;             \
	     fi ;                       \
	   ) ;                          \
	done
	@echo "+--------------------------------------------------"
	@echo "| All CX applics directories have been made clean."
	@echo "+--------------------------------------------------"

cleanall:	clean
