#----------------------------------------------------------------------------
#  $CX_ROOT/src/cu/camera/Makefile -- make CU camera support
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
HEADERS   = $(CXINCD)/cu_camera.h

#----------------------------------------------------------------------------
#  Non-installable targets
#----------------------------------------------------------------------------

TESTPROGS = cameraone

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers, install applics
#----------------------------------------------------------------------------

install:	$(HEADERS) library $(APPLICS)
		@echo "+-----------------------------------------------"
		@echo "| CU camera support is up to date and installed."
		@echo "+-----------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprograms
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+------------------------------------------------"
		@echo "| CU camera support and testprogs are up to date."
		@echo "+------------------------------------------------"

#----------------------------------------------------------------------------
#  Installable headers (EXPORT_HEADERS)
#----------------------------------------------------------------------------

$(CXINCD)/cu_camera.h:	cu_camera.h
				cp cu_camera.h $(CXINCD)

#----------------------------------------------------------------------------
#  CX library
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a

#----------------------------------------------------------------------------
#  CU library
#----------------------------------------------------------------------------

CULIB  = $(CX_ROOT)/lib/libcu.a

LFILES = cu_camera.o

library:	$(CULIB)

$(CULIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CULIB) $(LFILES)
		$(RANLIB) $(CULIB)
		@echo $(CULIB) " is up to date."

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cu_camera.o:		cu_camera.c cu_camera.h

#----------------------------------------------------------------------------
#  Application program rules
#----------------------------------------------------------------------------

applics:	$(APPLICS)

#----------------------------------------------------------------------------
#  Test program dependencies
#----------------------------------------------------------------------------

cameraone.o:	cameraone.c cu_camera.c $(HEADERS) $(CXLIB)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	$(TESTPROGS)

TRYCAMERA_OBS = cameraone.o cu_camera.o $(CULIB) $(CXLIB)

cameraone:	$(TRYCAMERA_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TRYCAMERA_OBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core
