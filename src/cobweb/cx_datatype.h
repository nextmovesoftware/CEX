/*****************************************************************************
*  cx_datatype.h -- CX definitions for datatype support
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

#ifndef CX_DATATYPE_INCLUDED
#define CX_DATATYPE_INCLUDED 1

#include "cx_types.h"

/*** Symbolic names for standard properties. ***/

#define CX_PROP_TAG   "tag"
#define CX_PROP_PNAME "property name"
#define CX_PROP_VNAME "verbose name"
#define CX_PROP_SHAPE "shape"
#define CX_PROP_LANG  "language"
#define CX_PROP_DESC  "description"
#define CX_PROP_MARK  "_mark"

/*** C-wrappers for public functions. ***/

#define cx_create_datatypetable  cx_e_create_datatypetable
#define cx_datatype              cx_e_datatype
#define cx_default_datatypetable cx_e_default_datatypetable
#define cx_pname2datatype        cx_e_pname2datatype
#define cx_set_datatype          cx_e_set_datatype
#define cx_tag2datatype          cx_e_tag2datatype

#define cx_create_datatype      cx_e_create_datatype

/*** Public function declarations. ***/

cx_Integer cx_e_dt_mark              (cx_Object dt                     );
cx_Integer cx_e_dt_setmark           (cx_Object dt, cx_Integer mark    );
cx_Integer cx_e_dt_table_setmarks    (cx_Object table, cx_Integer mark );
cx_Integer cx_e_set_datatype         (cx_Object ob, cx_Object dt       );
cx_Object  cx_e_create_datatypetable (cx_Object parent, cx_String name );
cx_Object  cx_e_datatype             (cx_Object ob                     );
cx_Object  cx_e_default_datatypetable(void                             );
cx_Object  cx_e_pname2datatype       (cx_Object table, cx_String pname );
cx_Object  cx_e_tag2datatype         (cx_Object table, cx_String tag   );

cx_Object cx_e_create_datatype       (cx_Object table, cx_String tag,
                                      cx_String vname, cx_String pname,
                                      cx_String shape, cx_String lang,
				      cx_String desc                   );

#endif /* CX_DATATYPE_INCLUDED */
