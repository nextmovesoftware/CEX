/*****************************************************************************
*  cx_binary.h -- definitions for binary support
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

#ifndef CX_BINARY_INCLUDED
#define CX_BINARY_INCLUDED 1

#include "cx_types.h"

/*** Two binary creation functions are public. ***/

#define cx_create_binary cx_e_create_binary
#define cx_parse_binary  cx_e_parse_binary

cx_Object cx_e_create_binary(cx_Object parent, cx_Integer len, cx_Binary ptr);
cx_Object cx_e_parse_binary (cx_Object parent, cx_String str);

#endif /* CX_BINARY_INCLUDED */
