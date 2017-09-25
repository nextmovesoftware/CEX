/*****************************************************************************
*  cx_stream.h -- definitions for stream support
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

#ifndef CX_STREAM_INCLUDED
#define CX_STREAM_INCLUDED 1

#include "cx_types.h"

/*** C-wrappers for public stream/sequence support functions. ***/

#define cx_create_sequence cx_e_create_sequence
#define cx_next            cx_e_next 
#define cx_atend           cx_e_atend
#define cx_append          cx_e_append
#define cx_delete          cx_e_delete

/*** Definitions of public stream/sequence support functions. ***/

cx_Object  cx_e_create_sequence(cx_Object ob);
cx_Object  cx_e_next           (cx_Object ob);
cx_Integer cx_e_atend          (cx_Object ob);
cx_Integer cx_e_append         (cx_Object sob, cx_Object addob);
cx_Integer cx_e_delete         (cx_Object sob, cx_Object zapob);

/*** Definitions of non-public stream support functions. ***/

cx_Integer cx_e_base_count   (cx_Object ob, cx_Integer type );
cx_Object  cx_e_base_stream  (cx_Object ob, cx_Integer type );
cx_Integer cx_e_base_append  (cx_Object ob, cx_Object  addob);
cx_Integer cx_e_base_delete  (cx_Object ob, cx_Object  zapob);
cx_Object  cx_e_create_stream(cx_Object parent              );

#endif /* CX_STREAM_INCLUDED */

