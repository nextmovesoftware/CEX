/*****************************************************************************
*  cx_spewob.h -- definitions for cx_spewob() utility
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

#ifndef CX_SPEWOB_INCLUDED
#define CX_SPEWOB_INCLUDED 1

#include <stdio.h>
#include <string.h>
#include "cx_types.h"

/*** Output level flags for cu_spewob(). ***/

#define CX_SPEW_NONE       0
#define CX_SPEW_DATA       1
#define CX_SPEW_CHILDREN   2
#define CX_SPEW_PROPS      4
#define CX_SPEW_PROPNAME   8
#define CX_SPEW_DATATAG   16
#define CX_SPEW_CLASS     32
#define CX_SPEW_SUMTUPLE  64
#define CX_SPEW_DEFAULT  (CX_SPEW_DATA | CX_SPEW_PROPS | CX_SPEW_CHILDREN)

/*** Public C-wrapper and entry point. ***/

#define cx_spewob cx_e_spewob

void cx_e_spewob(FILE *fp, cx_Object ob, cx_Integer verb);

#endif /* CX_SPEWOB_INCLUDED */
