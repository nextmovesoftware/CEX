#----------------------------------------------------------------------------
#  $CX_ROOT/src/include/Makefile -- CX include support
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

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

HEADERS	  = $(INC)/cx.h              \
	    $(INC)/cx_applics.h      \
	    $(INC)/cx_exotic.h       \
	    $(INC)/cx_limits.h       \
	    $(INC)/cx_types.h

#----------------------------------------------------------------------------
#  default -- make contribution to library, install headers files
#----------------------------------------------------------------------------

install:	$(HEADERS)
		@echo "+-------------------------------------------------"
		@echo "| CX headers up to date and installed."
		@echo "+-------------------------------------------------"

all:		install

#----------------------------------------------------------------------------
#  Installable headers
#----------------------------------------------------------------------------

$(INC)/cx.h:		cx.h
			cp cx.h $(INC)

$(INC)/cx_applics.h:	cx_applics.h
			cp cx_applics.h $(INC)

$(INC)/cx_exotic.h:	cx_exotic.h
			cp cx_exotic.h $(INC)

$(INC)/cx_limits.h:	cx_limits.h
			cp cx_limits.h $(INC)

$(INC)/cx_types.h:	cx_types.h
			cp cx_types.h $(INC)

#----------------------------------------------------------------------------
#  Clean up instructions (cleanup doesn't apply)
#----------------------------------------------------------------------------

clean:

cleanall:

