/*****************************************************************************
*  cx_facetuple.c -- support for CX Facetuple objects
*
*  An facetuple is a glorified string object whose content is the name of
*  an face property (which must be unique amoung surface's face props).
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Dave Weininger, Anthony Nicholls,
*                                       Conrad Huang
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

/*** Are facetuple methods initialized? ***/

static int initialized = FALSE;

/*============================================================================
 *  ftuple_destroy() -- destroy a given facetuple object
 *
 *  This destroys the facetuple properties.
 */
 
static void ftuple_destroy(cx_Object tuple)
{
   cx_Object surf, tob, face, faces;
   cx_String tname;

   /*** Reject if not a facetuple object. ***/

   if (CX_OB_FACETUPLE != cx_type(tuple)) return;

   /*** Extract tuple name. ***/

   tname = (cx_String) cx_e_base_content(tuple);

   /*** Destroy face properties of type tname. ***/

   if (tname) {
      surf   = cx_ancestor(tuple, CX_OB_SURFACE);
      faces = cx_stream(surf, CX_OB_FACE);
      while (NULL != (face = cx_next(faces))) {
         tob = cx_e_property(face, tname);
	 cx_destroy(tob);
      }
      cx_destroy(faces);
      cx_free(tname);
   }

   /*** Destroy base object and any children. ***/

   cx_e_base_destroy(tuple);
}

/*============================================================================
 *  ftuple_stringvalue() -- return stringvalue of given facetuple
 *
 *  This always quotes its data.
 */
 
static cx_String ftuple_stringvalue(cx_Object tuple)
{
   cx_Object surf, face, faces;
   cx_String tname, s;
   int       cont;
 
   /*** Reject if not an facetuple object. ***/
 
   if (CX_OB_FACETUPLE != cx_type(tuple)) return NULL;
 
   /*** Extract tuple name. ***/
 
   if (NULL == (tname = (cx_String) cx_e_base_content(tuple))) return NULL;
 
   /*** Make semicolon-separated list of face properties of type tname. ***/
 
   cx_scratchpad(NULL);
   cx_scratchpad( "\"" );
   surf   = cx_ancestor(tuple, CX_OB_SURFACE);
   faces = cx_stream(surf, CX_OB_FACE);
   for (cont = FALSE; NULL != (face = cx_next(faces)); cont = TRUE) {
 
      /*** Possibly add tuple-delimiter, add stringvalue. ***/
 
      s = cx_sprop(face, tname);
      if (cont) cx_scratchpad( ";"  );
      if (s   ) cx_scratchpad( s    );
   }
   cx_scratchpad( "\"" );
 
   /*** Clean up and return static buffer. ***/
 
   cx_destroy(faces);
   return cx_scratchpad("");  /* duplicate? remove trailing commas? */
}

/*============================================================================
 *  facetuple_init() -- initialize facetuple-specific functions
 *
 *  Called only once by cx_e_create_facetuple().
 */
 
static cx_Integer facetuple_init(void)
{
   /*** Define facetyple object type. ***/
 
   cx_e_set_typename(CX_OB_FACETUPLE, "Facetuple");
 
   /*** No create, cexin, or send methods for facetuples. ***/

   cx_set_method(CX_OB_FACETUPLE, "cexin",  NULL);
   cx_set_method(CX_OB_FACETUPLE, "send",   NULL);

   /*** Facetuple-specific functions ***/

   cx_set_method(CX_OB_FACETUPLE, "destroy",     ftuple_destroy    );
   cx_set_method(CX_OB_FACETUPLE, "stringvalue", ftuple_stringvalue);

   /*** Use normal (base) count, stream, and setproperty methods. ***/

   cx_set_method(CX_OB_FACETUPLE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_FACETUPLE, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_FACETUPLE, "setproperty", cx_e_base_setproperty);


   /*** Mark methods initialized and return successfully. ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  cx_e_create_facetuple() -- creates facetuple and sets initial value
 */
 
cx_Object cx_e_create_facetuple(cx_Object surf, cx_String name)
{
   cx_Object ob, obs;
   cx_String str;

   /*** Initialize facetyple methods once. ***/

   if (!initialized) facetuple_init();

   /*** Require non-null facetuple name. ***/

   if (NULL == name) return NULL;

   /*** Look for extant facetuple with given name and destroy it. ***/

   obs = cx_stream(surf, CX_OB_FACETUPLE);
   while (NULL != (ob = cx_next(obs))) {
      str = cx_e_base_content(ob);
      if (0 == cx_strcmp(str, name)) break;
   }
   cx_destroy(obs);
   if (ob) cx_destroy(ob);

   /*** Make new CX_OB_FACETUPLE object with given parent. ***/

   if (NULL == (ob = cx_e_base_create(surf, CX_OB_FACETUPLE))) return NULL;

   /*** Set content of new facetuple object to copy of given name. ***/

   cx_e_base_set_content(ob, (void *) cx_strdup(name));

   /*** Return initialized facetuple object. ***/

   return (cx_Object) ob;
}

/*============================================================================
 *  cx_e_facetuple_name() -- return name of facetuple (property)
 */
 
cx_String cx_e_facetuple_name(cx_Object ftuple)
{
   /*** Reject if not a facetuple object. ***/
 
   if (CX_OB_FACETUPLE != cx_type(ftuple)) return NULL;
 
   /*** Return object content as cx_String. ***/
 
   return (cx_String) cx_e_base_content(ftuple);
}

/*============================================================================
 *  cx_e_prefix2ftuples() -- return surf's ftuples w/names starting w/prefix
 */
 
cx_Object cx_e_prefix2ftuples(cx_Object surf, cx_String prefix)
{
   cx_Object tuples, tuple, seq = cx_create_sequence(NULL);
   int       lens;
 
   /*** Return NULL if surf or prefix is NULL. ***/
 
   if (NULL == surf || NULL == prefix) return NULL;
 
   /*** Add tuples with given prefix to sequence. ***/
 
   lens   = strlen(prefix);
   tuples = cx_stream(surf, CX_OB_FACETUPLE);
   while (NULL != (tuple = cx_next(tuples)))
      if (0 == cx_strncmp(prefix, (cx_String) cx_e_base_content(tuple), lens))
          cx_append(seq, tuple);
   cx_destroy(tuples);
 
   /*** Return (possibly empty) sequence of facetuples. ***/
 
   cx_reset(seq);
   return seq;
}
