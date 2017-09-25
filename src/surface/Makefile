#----------------------------------------------------------------------------
#  $CX_ROOT/src/surface/Makefile -- make CX surface support
#----------------------------------------------------------------------------
#  Usage:
#     make ............ same as make install
#     make all ........ makes everything, including test programs
#     make install .... make & install  everything except test programs
#     make clean ...... cleans out everything except sources
#----------------------------------------------------------------------------
#  Contributing author and institution: Anthony Nicholls, Conrad Huang
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
HEADERS   = $(CXINCD)/cx_surface.h

#----------------------------------------------------------------------------
#  Non-installable targets
#----------------------------------------------------------------------------

TESTPROGS = trysurf testsurf

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers, install applics
#----------------------------------------------------------------------------

install:	$(HEADERS) library $(APPLICS)
		@echo "+-------------------------------------------------"
		@echo "| CX surface support is up to date and installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprograms
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+--------------------------------------------------"
		@echo "| CX surface support and testprogs are up to date."
		@echo "+--------------------------------------------------"

#----------------------------------------------------------------------------
#  Installable headers (EXPORT_HEADERS)
#----------------------------------------------------------------------------

$(CXINCD)/cx_surface.h:	cx_surface.h
			cp cx_surface.h $(CXINCD)

#----------------------------------------------------------------------------
#  CX library
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a

LFILES = cx_surface.o       \
         cx_vertextuple.o   \
         cx_facetuple.o

library:	$(CXLIB)

$(CXLIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CXLIB) $(LFILES)
		$(RANLIB) $(CXLIB)
		@echo $(CXLIB) " is up to date."

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cx_surface.o:		cx_surface.c cx_surface.h

cx_vertextuple.o:	cx_vertextuple.c cx_surface.h

cx_facetuple.o:		cx_facetuple.c cx_surface.h

#----------------------------------------------------------------------------
#  Application program rules
#----------------------------------------------------------------------------

applics:	$(APPLICS)

#----------------------------------------------------------------------------
#  Test program dependencies
#----------------------------------------------------------------------------

trysurf.o:	trysurf.c $(LFILES) $(HEADERS) $(CXLIB)

testsurf.o:	testsurf.c $(LFILES) $(HEADERS) $(CXLIB)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	$(TESTPROGS)

TRYSURF_OBS = trysurf.o cx_surface.o cx_facetuple.o cx_vertextuple.o $(CXLIB)

trysurf:	$(TRYSURF_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TRYSURF_OBS)

TESTSURF_OBS = testsurf.o cx_surface.o cx_facetuple.o cx_vertextuple.o $(CXLIB)

testsurf:	$(TESTSURF_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TESTSURF_OBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core

cleanall:	clean

