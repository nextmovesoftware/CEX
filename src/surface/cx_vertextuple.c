/*****************************************************************************
*  cx_vertextuple.c -- support for CX Vertextuple objects
*
*  An vertextuple is a glorified string object whose content is the name of
*  an vertex property (which must be unique amoung surface's vertex props).
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
#include "cx_surface.h"

/*** Are vertextuple methods initialized? ***/
 
static int initialized = FALSE;

/*============================================================================
 *  vtuple_destroy() -- destroy a given vertextuple object
 *
 *  This destroys the vertextuple properties.
 */
 
static void vtuple_destroy(cx_Object tuple)
{
   cx_Object surf, tob, vertex, vertices;
   cx_String tname;

   /*** Reject if not an vertextuple object. ***/

   if (CX_OB_VERTEXTUPLE != cx_type(tuple)) return;

   /*** Extract tuple name. ***/

   tname = (cx_String) cx_e_base_content(tuple);

   /*** Destroy vertex properties of type tname. ***/

   if (tname) {
      surf   = cx_ancestor(tuple, CX_OB_SURFACE);
      vertices = cx_stream(surf, CX_OB_VERTEX);
      while (NULL != (vertex = cx_next(vertices))) {
         tob = cx_e_property(vertex, tname);
	 cx_destroy(tob);
      }
      cx_destroy(vertices);
      cx_free(tname);
   }

   /*** Destroy base object and any children. ***/

   cx_e_base_destroy(tuple);
}

/*============================================================================
 *  vtuple_stringvalue() -- return stringvalue of given vertextuple
 *
 *  This always quotes its data.
 */
 
static cx_String vtuple_stringvalue(cx_Object tuple)
{
   cx_Object surf, vertex, vertices;
   cx_String tname, s;
   int       cont;

   /*** Reject if not an vertextuple object. ***/

   if (CX_OB_VERTEXTUPLE != cx_type(tuple)) return NULL;

   /*** Extract tuple name. ***/

   if (NULL == (tname = (cx_String) cx_e_base_content(tuple))) return NULL;

   /*** Make semicolon-separated list of vertex properties of type tname. ***/

   cx_scratchpad(NULL);
   cx_scratchpad( "\"" );
   surf   = cx_ancestor(tuple, CX_OB_SURFACE);
   vertices = cx_stream(surf, CX_OB_VERTEX);
   for (cont = FALSE; NULL != (vertex = cx_next(vertices)); cont = TRUE) {

      /*** Possibly add tuple-delimiter, add stringvalue. ***/

      s = cx_sprop(vertex, tname);
      if (cont) cx_scratchpad( ";" );
      if (s   ) cx_scratchpad( s   );
   }

   /*** Add final quote. ***/

   cx_scratchpad( "\"" );

   /*** Clean up and return static buffer. ***/

   cx_destroy(vertices);
   return cx_scratchpad("");  /* duplicate? remove trailing commas? */
}

/*============================================================================
 *  vertextuple_init() -- initialize vertextuple-specific functions
 *
 *  Called only by cx_e_create_vertextuple().
 */
 
static cx_Integer vertextuple_init(void)
{
   /*** Define vertextuple object type. ***/
 
   cx_e_set_typename(CX_OB_VERTEXTUPLE, "Vertextuple");
 
   /*** No cexin or send methods for vertextuples. ***/

   cx_set_method(CX_OB_VERTEXTUPLE, "cexin",  NULL);
   cx_set_method(CX_OB_VERTEXTUPLE, "send",   NULL);

   /*** Vertextuple-specific functions ***/

   cx_set_method(CX_OB_VERTEXTUPLE, "destroy",     vtuple_destroy    );
   cx_set_method(CX_OB_VERTEXTUPLE, "stringvalue", vtuple_stringvalue);

   /*** Use normal (base) count, stream, and setproperty methods. ***/

   cx_set_method(CX_OB_VERTEXTUPLE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_VERTEXTUPLE, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_VERTEXTUPLE, "setproperty", cx_e_base_setproperty);

   /*** Mark methods initialized and return successfully. ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  cx_e_create_vertextuple() -- creates vertextuple and sets initial value
 *
 *  This doesn't actually check that surf is a surface, so this could
 *  presumably work for any vertex-containing object.
 */
 
cx_Object cx_e_create_vertextuple(cx_Object surf, cx_String name)
{
   cx_Object ob, obs;
   cx_String str;

   /*** Initialize vertextuple methods once. ***/

   if (!initialized) vertextuple_init();

   /*** Require non-null vertextuple name. ***/

   if (NULL == name) return NULL;

   /*** Look for extant vertextuple with given name and destroy it. ***/

   obs = cx_stream(surf, CX_OB_VERTEXTUPLE);
   while (NULL != (ob = cx_next(obs))) {
      str = cx_e_base_content(ob);
      if (0 == cx_strcmp(str, name)) break;
   }
   cx_destroy(obs);
   if (ob) cx_destroy(ob);

   /*** Make new CX_OB_VERTEXTUPLE object with given parent surf. ***/

   if (NULL == (ob = cx_e_base_create(surf, CX_OB_VERTEXTUPLE))) return NULL;

   /*** Set content of new vertextuple object to copy of given name. ***/

   cx_e_base_set_content(ob, (void *) cx_strdup(name));

   /*** Return initialized vertextuple object. ***/

   return (cx_Object) ob;
}

/*============================================================================
 *  cx_e_vertextuple_name() -- return name of vertextuple (property)
 */
 
cx_String cx_e_vertextuple_name(cx_Object vtuple)
{
   /*** Reject if not an vertextuple object. ***/

   if (CX_OB_VERTEXTUPLE != cx_type(vtuple)) return NULL;

   /*** Return object content as cx_String. ***/

   return (cx_String) cx_e_base_content(vtuple);
}

/*============================================================================
 *  cx_e_prefix2vtuples() -- return surf's vtuples w/ names starting w/prefix
 */
 
cx_Object cx_e_prefix2vtuples(cx_Object surf, cx_String prefix)
{
   cx_Object tuples, tuple, seq = cx_create_sequence(NULL);
   int       lens;
 
   /*** Return NULL if surf or prefix is NULL. ***/
 
   if (NULL == surf || NULL == prefix) return NULL;
 
   /*** Add tuples with given prefix to sequence. ***/
 
   lens   = strlen(prefix);
   tuples = cx_stream(surf, CX_OB_VERTEXTUPLE);
   while (NULL != (tuple = cx_next(tuples)))
      if (0 == cx_strncmp(prefix, (cx_String) cx_e_base_content(tuple), lens))
          cx_append(seq, tuple);
   cx_destroy(tuples);
 
   /*** Return (possibly empty) sequence of vertextuples. ***/
 
   cx_reset(seq);
   return seq;
}
