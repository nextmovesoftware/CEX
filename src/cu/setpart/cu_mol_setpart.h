/*****************************************************************************
*  cu_mol_setpart.h -- functions in cu_mol_setpart.c
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

#ifndef CU_MOL_SETPART_INCLUDED
#define CU_MOL_SETPART_INCLUDED 1

#include "cx_types.h"

/** Simple definition of single cu-level function. ***/

cx_Integer cu_mol_setpart(cx_Object mol, cx_String partprop);

#endif /* CX_MOL_SETPART_INCLUDED */
