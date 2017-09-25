#----------------------------------------------------------------------------
#  $CX_ROOT/src/cobweb/Makefile -- make cobweb (chemical object web) support
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
LOCALINCS = -I../include
TESTPROGS = helloworlds testcexrw testcextd testcexdt \
	    helloworld testbin2cex testcex2bin testcexrw2

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

HEADERS	  = $(INC)/cx_cobweb.h       \
	    $(INC)/cx_basics.h       \
	    $(INC)/cx_cex_io.h       \
	    $(INC)/cx_datatype.h     \
	    $(INC)/cx_errorqueue.h   \
            $(INC)/cx_iostream.h     \
	    $(INC)/cx_property.h     \
	    $(INC)/cx_spewob.h       \
	    $(INC)/cx_stream.h       \
	    $(INC)/cx_string.h       \
	    $(INC)/cx_binary.h       \
	    $(INC)/cx_utils.h

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers files
#----------------------------------------------------------------------------

install:	$(HEADERS) library
		@echo "+-------------------------------------------------"
		@echo "| Basic CX library up to date and installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and make test programs
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+-------------------------------------------------"
		@echo "| Basic CX library and test programs up to date."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  CX library
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a

LFILES = cx_basics.o     \
         cx_version.o    \
         cx_cex_io.o     \
         cx_iostream.o   \
         cx_utils.o      \
         cx_stream.o     \
         cx_string.o     \
         cx_binary.o     \
         cx_property.o   \
         cx_errorqueue.o \
         cx_datatype.o   \
         cx_spewob.o

$(CXLIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CXLIB) $(LFILES)
		$(RANLIB) $(CXLIB)
		@echo $(CXLIB) " is up to date."

library:	 $(CXLIB)
#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

$(INC)/cx_cobweb.h:	cx_cobweb.h
			cp cx_cobweb.h $(INC)

$(INC)/cx_basics.h:	cx_basics.h
			cp cx_basics.h $(INC)

$(INC)/cx_cex_io.h:	cx_cex_io.h
			cp cx_cex_io.h $(INC)

$(INC)/cx_iostream.h:	cx_iostream.h
			cp cx_iostream.h $(INC)

$(INC)/cx_datatype.h:	cx_datatype.h
			cp cx_datatype.h $(INC)

$(INC)/cx_errorqueue.h:	cx_errorqueue.h
			cp cx_errorqueue.h $(INC)

$(INC)/cx_property.h:	cx_property.h
			cp cx_property.h $(INC)

$(INC)/cx_spewob.h:	cx_spewob.h
			cp cx_spewob.h $(INC)

$(INC)/cx_stream.h:	cx_stream.h
			cp cx_stream.h $(INC)

$(INC)/cx_binary.h:	cx_binary.h
			cp cx_binary.h $(INC)

$(INC)/cx_string.h:	cx_string.h
			cp cx_string.h $(INC)

$(INC)/cx_utils.h:	cx_utils.h
			cp cx_utils.h $(INC)

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cx_spewob.o:		cx_spewob.c		$(HEADERS)
cx_basics.o:		cx_basics.c		$(HEADERS)
cx_cex_io.o:		cx_cex_io.c		$(HEADERS)
cx_iostream.o:		cx_iostream.c		$(HEADERS)
cx_datatype.o:		cx_datatype.c		$(HEADERS)
cx_errorqueue.o:	cx_errorqueue.c		$(HEADERS)
cx_property.o:		cx_property.c		$(HEADERS)
cx_stream.o:		cx_stream.c		$(HEADERS)
cx_binary.o:		cx_binary.c		$(HEADERS)
cx_string.o:		cx_string.c		$(HEADERS)


cx_version.o::
		/bin/sh ./new_version > cx_version.c
		$(CC) $(CFLAGS) -c -o cx_version.o cx_version.c

#----------------------------------------------------------------------------
#  Test program dependencies
#----------------------------------------------------------------------------

testcexrw2.o:	testcexrw2.c $(CXLIB) $(HEADERS)

testcexrw.o:	testcexrw.c $(CXLIB) $(HEADERS)

testcextd.o:	testcextd.c $(CXLIB) $(HEADERS)

testcexdt.o:	testcexdt.c $(CXLIB) $(HEADERS)

helloworlds.o:	helloworlds.c $(CXLIB) $(HEADERS)

helloworld.o:	helloworld.c $(CXLIB) $(HEADERS)

testbin2cex.o:	testbin2cex.c $(CXLIB) $(HEADERS)

testcex2bin.o:	testcex2bin.c $(CXLIB) $(HEADERS)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	helloworlds testcexrw testcexdt testcextd   helloworld \
		testcexrw2

helloworlds:	helloworlds.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) helloworlds.o \
		$(CXLIB) $(SYSLIBS)

helloworld:	helloworld.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) helloworld.o \
		$(CXLIB) $(SYSLIBS)

testcexrw:	testcexrw.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) testcexrw.o \
		$(CXLIB) $(SYSLIBS)

testcexrw2:	testcexrw2.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) testcexrw2.o \
		$(CXLIB) $(SYSLIBS)

testcextd:	testcextd.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) testcextd.o \
		$(CXLIB) $(SYSLIBS)

testcexdt:	testcexdt.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) testcexdt.o \
		$(CXLIB) $(SYSLIBS)

testbin2cex:	testbin2cex.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) testbin2cex.o \
		$(CXLIB) $(SYSLIBS)

testcex2bin:	testcex2bin.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) testcex2bin.o \
		$(CXLIB) $(SYSLIBS)

rastestread:	rastestread.o $(CXLIB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) rastestread.o \
		$(CXLIB) $(SYSLIBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) rastestread *pure* core

cleanall:	clean

