#----------------------------------------------------------------------------
#  $CX_ROOT/src/applics/wrl/Makefile -- make CX WRL conversion applics
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

INCDIR     = $(CX_ROOT)/include
BINDIR     = $(CX_ROOT)/bin
LOCALINCS  = -I$(INCDIR)
# TESTPROGS  = testwrlin testwrlout
APPLICS    = cex2wrl

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

HEADERS	  = $(INCDIR)/cx_wrl.h

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers
#----------------------------------------------------------------------------

install:	$(HEADERS) library $(APPLICS)
		cp $(APPLICS) $(BINDIR); cd $(BINDIR); $(STRIP) $(APPLICS);
		@echo "+-------------------------------------------------"
		@echo "| CX wrl applics and support are installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprograms
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+-------------------------------------------------"
		@echo "| CX wrl support and test programs are up to date."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  CX library
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a
LIBS   = $(CXLIB) $(SQRTLIB)

LFILES = cx_wrl_write.o

library:	$(CXLIB)

$(CXLIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CXLIB) $(LFILES)
		$(RANLIB) $(CXLIB)
		@echo $(CXLIB) " is up to date."

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

$(INCDIR)/cx_wrl.h:	cx_wrl.h
			cp cx_wrl.h $(INCDIR)

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cx_wrl_write.o:	cx_wrl_write.c cx_wrl.h

#----------------------------------------------------------------------------
#  Program dependencies
#----------------------------------------------------------------------------

cex2wrl.o:	cex2wrl.c cx_wrl_write.c $(HEADERS) $(CXLIB)

#----------------------------------------------------------------------------
#  Application programs
#----------------------------------------------------------------------------

C2W_OBS = cex2wrl.o cx_wrl_write.o

cex2wrl:	$(C2W_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(C2W_OBS) \
			$(LIBS) $(SYSLIBS)

#----------------------------------------------------------------------------
#  Test programs (none so far)
#----------------------------------------------------------------------------
# 
#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core
