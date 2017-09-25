/*****************************************************************************
*  cx_limits.h -- various CX limits, some of which are machine-dependent
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Dave Weininger, Daylight CIS, Inc.
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#ifndef CX_LIMITS_INCLUDED
#define CX_LIMITS_INCLUDED 1

/*** Maximum path name -- should be mdep (255 for Posix, 1024 for Sun). ***/

#define CX_PATH_MAX 1024

#endif /* CX_LIMITS_INCLUDED */
