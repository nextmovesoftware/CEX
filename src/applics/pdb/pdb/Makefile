#----------------------------------------------------------------------------
#  $CX_ROOT/src/applics/pdb/pdb/Makefile -- make libpdb.a support
#----------------------------------------------------------------------------
#  Usage:
#     make ............ same as make install
#     make all ........ makes everything, including test programs
#     make install .... make & install  everything except test programs
#     make clean ...... cleans out everything except sources
#----------------------------------------------------------------------------
#  Contributing author and institution: Dave Weininger, Conrad Huang
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

SHELL      = /bin/sh
INCDIR     = $(CX_ROOT)/include
BINDIR     = $(CX_ROOT)/bin
LIBDIR     = $(CX_ROOT)/lib
LOCALINCS  = -I$(INCDIR)
ALLSUBDIRS = $(SUBDIRS)


#
#	For machines that don't have lint, remove $(LINTLIB) from the
#	"all" target.
#
#	On BSD machines, LINTOPT should be '-C', RANLIB should be 'ranlib'
#
#	On System V machines, LINTOPT should be '-o $(NOTHING)',
#	RANLIB should be ':'
#

NOTHING =
LINTOPT = -o $(NOTHING)
LIBRARY = pdb
LIBARCH = lib$(LIBRARY).a
LINTLIB = llib-l$(LIBRARY).ln
OBJS    = pdb_read.o pdb_sprntf.o pdb_sscanf.o pdb_write.o pdbrun.o ms.o
SRCS    = pdb_read.c pdb_sprntf.c pdb_sscanf.c pdb_write.c pdbrun.o ms.c
LIBINST = $(LIBDIR)/$(LIBARCH)



install:	$(LIBINST)

$(LIBINST):	$(LIBARCH)
		cp pdb.h $(CX_ROOT)/include
		cp $(LIBARCH) $(LIBINST)
		$(RANLIB) $(LIBINST)

all:		$(LIBARCH)

$(LIBARCH):     $(OBJS)
		@echo "Loading $(LIBARCH) ... "
		@$(AR) $(AR_PUTFLAGS) $(LIBARCH) $(OBJS)
		@$(RANLIB) $(LIBARCH)
		@echo "done"

$(LINTLIB):	$(SRCS) $(HDRS)
		@echo "Creating $(LINTLIB) ... "
		@lint $(LINTOPT)$(LIBRARY) $(SRCS)
		@echo "done"

clean:;		@rm -f $(OBJS) $(LIBARCH) $(LINTLIB)

lint:		$(SRCS)
		lint -u $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags -w $(HDRS) $(SRCS)
