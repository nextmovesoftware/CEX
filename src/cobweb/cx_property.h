/*****************************************************************************
*  cx_property.h -- definitions for property object support
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

#ifndef CX_PROPERTY_INCLUDED
#define CX_PROPERTY_INCLUDED 1

#include "cx_basics.h"

/*** C-wrappers for public property support functions. ***/

#define cx_prop_name       cx_e_prop_name
#define cx_prefix2props    cx_e_prefix2props

#define cx_sprop           cx_e_sprop
#define cx_iprop           cx_e_iprop
#define cx_rprop           cx_e_rprop
#define cx_realformat      cx_e_realformat

/* cx_set_sprop used to be cx_e_set_sprop */
#define cx_set_sprop       cx_e_setproperty
#define cx_set_iprop       cx_e_set_iprop
#define cx_set_rprop       cx_e_set_rprop
#define cx_set_realformat  cx_e_set_realformat

/*** Definitions of property support functions. ***/

cx_Object  cx_e_prefix2props  (cx_Object ob, cx_String prefix                );
cx_String  cx_e_prop_name     (cx_Object ob                                  );
cx_String  cx_e_sprop         (cx_Object ob, cx_String pname                 );
cx_Object  cx_e_set_sprop     (cx_Object ob, cx_String pname, cx_String sval );
cx_Integer cx_e_iprop         (cx_Object ob, cx_String pname                 );
cx_Object  cx_e_set_iprop     (cx_Object ob, cx_String pname, cx_Integer ival);
cx_Object  cx_e_set_rprop     (cx_Object ob, cx_String pname, cx_Real    rval);
cx_Real    cx_e_rprop         (cx_Object ob, cx_String pname                 );
cx_String  cx_e_realformat    (void                                          );
void       cx_e_set_realformat(cx_String fmt                                 );

/*** Base "setproperty" function is visible but not public. ***/

cx_Object cx_e_property        (cx_Object ob, cx_String pname              );
cx_Object cx_e_base_setproperty(cx_Object ob, cx_String name, cx_String val);


#endif /* CX_PROPERTY_INCLUDED */
