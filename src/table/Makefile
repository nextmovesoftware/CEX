#----------------------------------------------------------------------------
#  $CX_ROOT/src/message/Makefile -- make CX message support
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
HEADERS   = $(CXINCD)/cx_message.h

#----------------------------------------------------------------------------
#  Non-installable targets
#----------------------------------------------------------------------------

TESTPROGS = testmess testmessin
# TESTPROGS = trymsg testmsg

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers, install applics
#----------------------------------------------------------------------------

install:	$(HEADERS) library $(APPLICS)
		@echo "+-------------------------------------------------"
		@echo "| CX message support is up to date and installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprograms
#----------------------------------------------------------------------------

all:    	install $(TESTPROGS)
		@echo "+--------------------------------------------------"
		@echo "| CX message support and testprogs are up to date."
		@echo "+--------------------------------------------------"

#----------------------------------------------------------------------------
#  Installable headers (EXPORT_HEADERS)
#----------------------------------------------------------------------------

$(CXINCD)/cx_message.h:	cx_message.h
			cp cx_message.h $(CXINCD)

#----------------------------------------------------------------------------
#  CX library
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a

LFILES = cx_message.o cx_log.o

library:	$(CXLIB)

$(CXLIB):	$(LFILES)
		$(AR) $(AR_PUTFLAGS) $(CXLIB) $(LFILES)
		$(RANLIB) $(CXLIB)
		@echo $(CXLIB) " is up to date."

#----------------------------------------------------------------------------
#  Library dependencies
#----------------------------------------------------------------------------

cx_message.o:	cx_message.c $(CXINCD)/cx_message.h
cx_log.o:	cx_log.c cx_message.c $(CXINCD)/cx_message.h

#----------------------------------------------------------------------------
#  Application program rules
#----------------------------------------------------------------------------

applics:	$(APPLICS)

#----------------------------------------------------------------------------
#  Test program dependencies
#----------------------------------------------------------------------------

testmess.o:	testmess.c $(LFILES) $(HEADERS) $(CXLIB)

testmessin.o:	testmessin.c $(LFILES) $(HEADERS) $(CXLIB)

trymsg.o:	trymsg.c $(LFILES) $(HEADERS) $(CXLIB)

trylog.o:	trylog.c $(LFILES) $(HEADERS) $(CXLIB)

testmsg.o:	testmsg.c $(LFILES) $(HEADERS) $(CXLIB)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	$(TESTPROGS)


TESTMESS_OB = testmess.o cx_message.o $(CXLIB)

testmess:	$(TESTMESS_OB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TESTMESS_OB)


TESTMI_OB = testmessin.o cx_message.o $(CXLIB)

testmessin:	$(TESTMI_OB)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TESTMI_OB)


TRYMSG_OBS = trymsg.o cx_message.o $(CXLIB)

trymsg:		$(TRYMSG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TRYMSG_OBS)

TRYLOG_OBS = trylog.o cx_message.o $(CXLIB) $(CX_ROOT)/lib/libcu.a

trylog:		$(TRYLOG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TRYLOG_OBS)

TESTMSG_OBS = testmsg.o cx_message.o $(CXLIB)

testmsg:	$(TESTMSG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TESTMSG_OBS)

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core
