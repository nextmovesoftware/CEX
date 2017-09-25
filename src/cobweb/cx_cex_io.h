/*****************************************************************************
*  cx_cex_io.h -- delarations for CX cex (low level datatree I/O) package
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

#ifndef CX_CEXIO_INCLUDED
#define CX_CEXIO_INCLUDED 1

#include "cx_types.h"

/*** CEX quote character. ***/

#define CX_QCEX_CHAR '\"'
#define CX_QCEX_STR  "\""

/*** Tuple separator, as character and string constants. ***/

#define CX_TUPLE_CHAR ';'
#define CX_TUPLE_STR  ";"

/*** Flags used by cx_cex_listfmt(). ***/

#define CX_FMT_RAW   0
#define CX_FMT_DUMP  1
#define CX_FMT_LIST  2

/*** Flags used by cx_e_cex_xtagdata(). ***/

#define CX_CXT_UNKNOWN    0
#define CX_CXT_IDENTIFIER 4201
#define CX_CXT_DATAITEM   4202
#define CX_CXT_PROPERTY   4203

/*** C-wrappers. ***/

#define cx_cex_listfmt     cx_e_cex_listfmt
#define cx_cex_set_listfmt cx_e_cex_set_listfmt
#define cx_cex_eof         cx_e_cex_eof

/*** Public entry points. ***/

cx_Integer cx_e_cex_listfmt    (void);
void       cx_e_cex_set_listfmt(cx_Integer boo);
cx_Integer cx_e_cex_eof        (void);

/*** Non-public entry point for package registration and etc. ***/

cx_Integer cx_e_register_root_type   (cx_String tag, cx_Integer type);
void       cx_e_deregister_root_types(void                          );
cx_String  cx_e_cex_read             (cx_Object ins                 );
cx_Integer cx_e_cex_write            (cx_Object outs, cx_String cex );

cx_String  cx_e_cex_xtagdata         (cx_String tree,  cx_Integer *pcxt,
			              cx_String *ptag, cx_String  *pdat);

cx_Integer cx_tag2roottype(cx_String tag); /* EXPERIMENTAL */

    /*** cx_receive is no longer public. ***/
cx_Object  cx_e_receive(cx_Object table, cx_Object ins, cx_Object outs);


/*** Should this stuff go into utils? ***/

cx_Integer cx_needs_quotes(cx_String str);
cx_String  cx_find_char   (cx_String str, cx_String sc);
cx_String  cx_unquote_dup (cx_String sin);
cx_String  cx_unquote_ndup(cx_String sin, int lens);
cx_String  cx_quote_dup   (cx_String str);


#endif /* CX_CEXIO_INCLUDED */
