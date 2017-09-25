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

CXLIB     = $(CX_ROOT)/lib/libcx.a
CXINCD    = $(CX_ROOT)/include
LOCALINCS = -I../include
HEADERS   = $(CXINCD)/cx_message.h

#----------------------------------------------------------------------------
#  Non-installable targets
#----------------------------------------------------------------------------

TESTPROGS = trymsg trylog orientxy  teelog  addlog
# TESTPROGS = trymsg testmsg

#----------------------------------------------------------------------------
#  Application program rules
#----------------------------------------------------------------------------

applics:	$(APPLICS)

#----------------------------------------------------------------------------
#  Test program dependencies
#----------------------------------------------------------------------------

addlog.o:	addlog.c $(LFILES) $(HEADERS) $(CXLIB)

teelog.o:	teelog.c $(LFILES) $(HEADERS) $(CXLIB)

trymsg.o:	trymsg.c $(LFILES) $(HEADERS) $(CXLIB)

trylog.o:	trylog.c $(LFILES) $(HEADERS) $(CXLIB)

testmsg.o:	testmsg.c $(LFILES) $(HEADERS) $(CXLIB)

#----------------------------------------------------------------------------
#  Test programs
#----------------------------------------------------------------------------

testprogs:	$(TESTPROGS)

ADDLOG_OBS = addlog.o $(CXLIB)

addlog:		$(ADDLOG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(ADDLOG_OBS)

TEELOG_OBS = teelog.o $(CXLIB)

teelog:		$(TEELOG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TEELOG_OBS)

TRYMSG_OBS = trymsg.o cx_message.o $(CXLIB)

trymsg:		$(TRYMSG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TRYMSG_OBS)

TRYLOG_OBS = trylog.o cx_message.o $(CXLIB) $(CX_ROOT)/lib/libcu.a

trylog:		$(TRYLOG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TRYLOG_OBS)

TESTMSG_OBS = testmsg.o cx_message.o $(CXLIB)

testmsg:	$(TESTMSG_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(TESTMSG_OBS)

###

#----------------------------------------------------------------------------

orientxy.o:	orientxy.c $(HEADERS) $(CXLIB) $(CULIB)

O3D_OBS = orientxy.o $(CXLIB)

orientxy:	$(O3D_OBS)
		$(PURIFY) $(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(O3D_OBS) -lm

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
		rm -f *.[oa] $(TESTPROGS) $(APPLICS) *pure* core
