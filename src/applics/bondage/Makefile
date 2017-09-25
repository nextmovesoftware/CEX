#----------------------------------------------------------------------------
#  $CX_ROOT/src/applics/bondage/Makefile -- make bondage applications
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

PROGRAM   = bondage

INC       = $(CX_ROOT)/include
LINKER	= f77

BINDIR    = $(CX_ROOT)/bin

# F77LIBS = -lsocket -lnsl -lw -ldl -lintl -lm -lmalloc


#----------------------------------------------------------------------------
#  Libraries
#----------------------------------------------------------------------------

CXLIB  = $(CX_ROOT)/lib/libcx.a
CXFLIB = $(CX_ROOT)/lib/libcx_f.a
LIBS   = $(CXFLIB) $(CXLIB) $(F77LIBS)

ALLOBJS	=  bondage.o  addbnd.o atype.o autobond.o bcktrk.o bndinf.o col13.o \
	   conjn.o cross.o distance.o dot.o findring.o genbond.o gencon.o \
	   getang.o getbnd.o getbnm.o getgeo.o getint.o getradii.o getreal.o \
   	gettor.o lenstr.o makebnd.o ncross.o newarg.o newvers.o nxtatt.o parse.o \
   	quicksort.o radiidat.o ring5.o setdouble.o squeeze.o univec.o \
   	upper.o verifyatm.o writpf.o

#----------------------------------------------------------------------------
#  default, install
#----------------------------------------------------------------------------

install:	$(PROGRAM)
		@echo "Installing application programs ..."
		cp $(PROGRAM) $(BINDIR); cd $(BINDIR); $(STRIP) $(PROGRAM);
		@echo "+-------------------------------------------------"
		@echo "| bondage is installed."
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  all -- make install and testprogs
#----------------------------------------------------------------------------

all:		install $(TESTPROGS)
		@echo "+-------------------------------------------------"
		@echo "| bondage is up to date.
		@echo "+-------------------------------------------------"

#----------------------------------------------------------------------------
#  program dependencies
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
#  programs
#----------------------------------------------------------------------------

$(PROGRAM):	$(ALLOBJS) $(INC)/cx_f.inc $(INC)/cx_f_molecule.inc
	@echo -n "Making $(PROGRAM) ... "
	@echo $(LINKER) $(FFLAGS) -o $(PROGRAM) $(ALLOBJS) $(LIBS)
	@$(LINKER) $(FFLAGS) -o $(PROGRAM) $(ALLOBJS) $(LIBS)
	@echo "done"

#----------------------------------------------------------------------------
#  Clean up instructions
#----------------------------------------------------------------------------

clean:
	rm -f *.[oa] $(TESTPROGS) $(PROGRAM) *pure* core

