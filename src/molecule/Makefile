#----------------------------------------------------------------------------
#  $CX_ROOT/src/molecule/Makefile -- make CX molecule support
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

#----------------------------------------------------------------------------
#  Installation destinations and targets
#----------------------------------------------------------------------------

CXBIND    = $(CX_ROOT)/bin
APPLICS   = 

CXINCD    = $(CX_ROOT)/include
LOCALINCS = -I../include
HEADERS   = $(CXINCD)/cx_molecule.h

#----------------------------------------------------------------------------
#  Non-installable targets
#----------------------------------------------------------------------------

TESTPROGS = trymol

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers, install applics
#----------------------------------------------------------------------------

install:	$(HEADERS) library $(APPLICS)
		@echo "+-------------------------------------------------"
		@echo "| CX molecule support is up to date and installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprograms
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+--------------------------------------------------"
		@echo "| CX molecule support and testprogs are up to date."
		@echo "+--------------------------------------------------"

#----------------------------------------------------------------------------
#  Installable headers (EXPORT_HEADERS)
#----------------------------------------------------------------------------

$(CXINCD)/cx_molecule.h:	cx_molecule.h
				cp cx_molecule.h $(CXINCD)

#----------------------------------------------------------------------------
#  CX library
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a

LFILES = cx_molecule.o       \
         cx_atomtuple.o      \
         cx_bondtuple.o      \
         cx_hydrogens.o      \
         cx_mol_copy.o

library:	$(CXLIB)

$(CXLIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CXLIB) $(LFILES)
		$(RANLIB) $(CXLIB)
		@echo $(CXLIB) " is up to date."

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cx_molecule.o:		cx_molecule.c cx_molecule.h

cx_atomtuple.o:		cx_atomtuple.c cx_molecule.h

cx_bondtuple.o:		cx_bondtuple.c cx_molecule.h

cx_hydrogens.o:		cx_hydrogens.c cx_molecule.h

cx_mol_copy.o:		cx_mol_copy.c cx_molecule.h

#----------------------------------------------------------------------------
#  Application program rules
#----------------------------------------------------------------------------

applics:	$(APPLICS)

PRINTCEX_OBS = xprintcex.o $(CXLIB)

xprintcex:	xprintcex.o $(HEADERS) library
		$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(PRINTCEX_OBS)

xprintcex.o:	xprintcex.c $(HEADERS) $(CXLIB)

#----------------------------------------------------------------------------
#  Test program dependencies
#----------------------------------------------------------------------------

trymol.o:	trymol.c cx_molecule.c cx_hydrogens.c $(HEADERS) $(CXLIB)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	$(TESTPROGS)

TRYMOL_OBS = trymol.o cx_molecule.o cx_hydrogens.o $(CXLIB)

trymol:		$(TRYMOL_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TRYMOL_OBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core

cleanall:	clean

