/*****************************************************************************
*  cx_string.c -- support for string objects
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

#include <stdio.h>
#include <string.h>
#include "cx_cobweb.h"

/*** Are we initialized? ***/

static int initialized = FALSE;

/*============================================================================
 *  string_destroy() -- destroy a given string object
 */
 
static void string_destroy(cx_Object ob)
{
   /*** Reject if not a string object. ***/

   if (CX_OB_STRING != cx_type(ob)) return;

   /*** Extract object content cx_String and free it, if able. ***/

   cx_free((cx_String) cx_e_base_content(ob));

   /*** Destroy base object and any children. ***/

   cx_e_base_destroy(ob);
}

/*============================================================================
 *  string_value() -- return stringvalue of a string object
 */
 
static cx_String string_value(cx_Object ob)
{
   /*** Reject if not a string object. ***/

   if (NULL == ob || CX_OB_STRING != cx_type(ob)) return NULL;

   /*** Return object content as cx_String. ***/

   return (cx_String) cx_e_base_content(ob);
}

/*============================================================================
 *  string_init() -- initialize string-specific functions
 */
 
static cx_Integer string_init(void)
{
   /*** Define string object type. ***/

   cx_e_set_typename(CX_OB_STRING, "String");

   /*** String-specific functions ***/

   cx_set_method(CX_OB_STRING, "destroy",     string_destroy);
   cx_set_method(CX_OB_STRING, "stringvalue", string_value  );

   /*** Use normal (base) count and stream methods. ***/

   cx_set_method(CX_OB_STRING, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_STRING, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_STRING, "setproperty", cx_e_base_setproperty);

   /*** Strings aren't root objects: no cexin or send methods. ***/

   cx_set_method(CX_OB_STRING, "cexin", NULL);
   cx_set_method(CX_OB_STRING, "send",  NULL);

   /*** Mark initialized and return happy. ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  cx_e_create_string() -- create a string object, set initial value
 *
 *  Should this deal with quoting, e.g., \007, \t, \n, etc?
 */
 
cx_Object cx_e_create_string(cx_Object parent, cx_String s)
{
   cx_Object sob;

   /*** Initialize polymorphic functions if needed. ***/

   if (!initialized) string_init();

   /*** Make new CX_OB_STRING object with given parent. ***/

   if (NULL == (sob = cx_e_base_create(parent, CX_OB_STRING))) return NULL;

   /*** Set content of new string object to copy of given string, or NULL. ***/

   cx_e_base_set_content(sob, (void *) (s ? cx_strdup(s) : NULL));

   /*** Return initialized string object. ***/

   return (cx_Object) sob;
}
