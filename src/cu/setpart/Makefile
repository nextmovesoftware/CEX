#----------------------------------------------------------------------------
#  $CX_ROOT/src/cu/Makefile -- make CU support
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

INC       = $(CX_ROOT)/include
LOCALINCS = -I../../include
TESTPROGS = testparts

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

HEADERS	  = $(INC)/cu.h \
	    $(INC)/cu_mol_setpart.h

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers
#----------------------------------------------------------------------------

install:	$(HEADERS) library
		@echo "+-------------------------------------------------"
		@echo "| CU SETPART support is up to date and installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprograms
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+-------------------------------------------------"
		@echo "| CU SETPART support and test programs up to date."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  CX library (for test programs)
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a

#----------------------------------------------------------------------------
#  CU library
#----------------------------------------------------------------------------

CULIB  = $(CX_ROOT)/lib/libcu.a

LFILES = cu_mol_setpart.o

library:	$(CULIB)

$(CULIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CULIB) $(LFILES)
		$(RANLIB) $(CULIB)
		@echo $(CULIB) " is up to date."

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

$(INC)/cu.h:			../cu.h
				cp ../cu.h $(INC)

$(INC)/cu_mol_setpart.h:	cu_mol_setpart.h
				cp cu_mol_setpart.h $(INC)

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cu_mol_setpart.o:	cu_mol_setpart.c cu_mol_setpart.h

#----------------------------------------------------------------------------
#  Test program dependencies
#----------------------------------------------------------------------------

testparts.o:	testparts.c $(HEADERS) $(CULIB) $(CXLIB)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	$(TESTPROGS)

TESTPARTS_OBS = testparts.o $(CXLIB) $(CULIB)

testparts:	$(TESTPARTS_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TESTPARTS_OBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) *pure* core
