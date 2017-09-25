/*****************************************************************************
*  cx_error.h -- delarations for CX error package
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

#ifndef CX_ERRORS_INCLUDED
#define CX_ERRORS_INCLUDED 1

#include "cx_types.h"

/*** Standard severity flags. ***/

#define CX_ERR_NONE  "NOERROR"
#define CX_ERR_NOTE  "NOTE   "
#define CX_ERR_WARN  "WARNING"
#define CX_ERR_ERROR "ERROR  "
#define CX_ERR_FATAL "FATAL  "

/*** One low-level function is visible. ***/

#define cx_errorqueue cx_e_errorqueue

cx_Object cx_e_errorqueue(void);

/*** The public high level errorqueue interface. ***/

#define cx_error_count cx_e_error_count
#define cx_error_spew  cx_e_error_spew
#define cx_error_save  cx_e_error_save

cx_Integer cx_error_count(cx_String errlev                                );
cx_Integer cx_error_spew (FILE      *fp,   cx_String errlev               );
cx_Integer cx_error_save (cx_String msg,   cx_String errlev, cx_String src);

#endif /* CX_ERRORS_INCLUDED */
