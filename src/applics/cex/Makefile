#----------------------------------------------------------------------------
#  $CX_ROOT/src/applics/cex/Makefile -- make CX cex applications
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

BINDIR    = $(CX_ROOT)/bin
INC       = $(CX_ROOT)/include
LOCALINCS = -I$(INC)
APPLICS   = catcex printcex
TESTPROGS =

#----------------------------------------------------------------------------
#  Installable headers (none)
#----------------------------------------------------------------------------

HEADERS	  = 

#----------------------------------------------------------------------------
#  Libraries
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a
CULIB  = $(CX_ROOT)/lib/libcu.a
LIBS   = $(CXLIB) $(CULIB)

#----------------------------------------------------------------------------
#  default, install
#----------------------------------------------------------------------------

install:	$(APPLICS)
		@echo "Installing application programs ..."
		cp $(APPLICS) $(BINDIR); cd $(BINDIR); $(STRIP) $(APPLICS);
		@echo "+-------------------------------------------------"
		@echo "| CX cex applics are up to date and installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprogs
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+-------------------------------------------------"
		@echo "| CX cex applics testprogs are up to date.
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  man pages (none yet)
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
#  program dependencies
#----------------------------------------------------------------------------

catcex.o:	catcex.c

printcex.o:	printcex.c

#----------------------------------------------------------------------------
#  programs
#----------------------------------------------------------------------------

progs:		$(APPLICS)

catcex:		catcex.o $(LIBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) catcex.o $(LIBS) \
			$(SYSLIBS)

printcex:	printcex.o $(LIBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) printcex.o \
			$(LIBS) $(SYSLIBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core
