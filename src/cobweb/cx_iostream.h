/*****************************************************************************
*  cx_iostream.h -- low level physical I/O package
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Roger Sayle, GlaxoWellcome R&D, UK.
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#ifndef CX_IOSTREAM_INCLUDED
#define CX_IOSTREAM_INCLUDED 1

#include "cx_types.h"

/*** Public C-wrapper and entry point. ***/

#define CX_IO_READ  0
#define CX_IO_WRITE 1
#define cx_create_iostream cx_e_create_iostream

cx_Object cx_e_create_iostream(cx_String fname, cx_Integer access);

/*** Toolkit internal entry points ***/

cx_Integer cx_e_iogetc ( cx_Object fp );
cx_Integer cx_e_ioeof  ( cx_Object fp );
void       cx_e_ioputc ( int ch, cx_Object fp );
void       cx_e_ioputs ( char *ptr, cx_Object fp );
void       cx_e_ioflush( cx_Object fp );

cx_Integer cx_e_iostream_append( cx_Object fp, cx_Object ob );
cx_Object  cx_e_iostream_next  ( cx_Object fp );

void       cx_e_iostream_cleanup( void );
#endif /* CX_IOSTREAM_INCLUDED */
