#----------------------------------------------------------------------------
#  $CX_ROOT/src/applics/pdb/Makefile -- make CX PDB conversion applics
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

INCDIR    = $(CX_ROOT)/include
BINDIR    = $(CX_ROOT)/bin
LOCALINCS = -I$(INCDIR)
TESTPROGS = testpdbin testpdbout
APPLICS   = pdb2cex cex2pdb

SUBDIRS    = pdb

ALLSUBDIRS = $(SUBDIRS)

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

HEADERS	  = $(INCDIR)/cx_pdb.h

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers
#----------------------------------------------------------------------------

install:	subdirs $(HEADERS) library $(APPLICS)
		cp $(APPLICS) $(BINDIR); cd $(BINDIR); $(STRIP) $(APPLICS);
		@echo "+-------------------------------------------------"
		@echo "| CX pdb applics and support are installed."
		@echo "+-------------------------------------------------"

subdirs:
		@for d in $(ALLSUBDIRS) ;       \
		do                              \
		   ( cd $$d;                    \
		     echo installing `pwd`;     \
		     if [ -r Makefile ] ; then  \
			make install;           \
		     fi ;                       \
		   ) ;                          \
		done

#----------------------------------------------------------------------------
#  all -- make install and testprograms
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+-------------------------------------------------"
		@echo "| CX pdb support and test programs are up to date."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  PDB library
#----------------------------------------------------------------------------

PDBLIB  = $(CX_ROOT)/lib/libpdb.a

#----------------------------------------------------------------------------
#  CU library
#----------------------------------------------------------------------------

CULIB  = $(CX_ROOT)/lib/libcu.a

ULFILES = cu_camera2pdb.o

$(CULIB):	$(ULFILES)
		$(AR) $(AR_PUTFLAGS) $(CULIB) $(ULFILES)
		$(RANLIB) $(CULIB)
		@echo $(CULIB) " is up to date."

#----------------------------------------------------------------------------
#  CX library
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a

LFILES = cx_pdb_read.o   \
	 cx_pdb_write.o

library:	$(CXLIB) $(CULIB)

$(CXLIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CXLIB) $(LFILES)
		$(RANLIB) $(CXLIB)
		@echo $(CXLIB) " is up to date."

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

$(INCDIR)/cx_pdb.h:	cx_pdb.h
			cp cx_pdb.h $(INCDIR)

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cx_pdb_read.o:	cx_pdb_read.c cx_pdb.h

cx_pdb_write.o:	cx_pdb_write.c cx_pdb.h

cu_camera2pdb.o:	cu_camera2pdb.c cx_pdb.h

#----------------------------------------------------------------------------
#  Program dependencies
#----------------------------------------------------------------------------

pdb2cex.o:	pdb2cex.c cx_pdb_read.c $(HEADERS) $(CXLIB) $(CULIB)

cex2pdb.o:	cex2pdb.c cx_pdb_write.c $(HEADERS) $(CXLIB) $(CULIB)

testpdbin.o:	testpdbin.c  cx_pdb_read.c  $(HEADERS) $(CXLIB) $(CULIB)

testpdbout.o:	testpdbout.c cx_pdb_write.c $(HEADERS) $(CXLIB) $(CULIB)

#----------------------------------------------------------------------------
#  Application programs
#----------------------------------------------------------------------------

P2C_OBS = pdb2cex.o cx_pdb_read.o $(CXLIB) $(CULIB) $(PDBLIB)

pdb2cex:	$(P2C_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(P2C_OBS) \
			$(SYSLIBS)

C2P_OBS = cex2pdb.o cx_pdb_write.o cu_camera2pdb.o $(CXLIB) $(CULIB) $(PDBLIB)

cex2pdb:	$(C2P_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(C2P_OBS) \
			$(SYSLIBS)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	testpdbin testpdbout

IN_OBS = testpdbin.o cx_pdb_read.o $(CXLIB) $(CULIB) $(PDBLIB)

testpdbin:	$(IN_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(IN_OBS) \
			$(SYSLIBS)

OUT_OBS = testpdbout.o cx_pdb_write.o $(CXLIB) $(CULIB) $(PDBLIB)

testpdbout:	$(OUT_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(OUT_OBS) \
			$(SYSLIBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

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
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core

