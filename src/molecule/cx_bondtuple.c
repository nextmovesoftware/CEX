/*****************************************************************************
*  cx_bondtuple.c -- support for CX Bondtuple objects
*
*  An bondtuple is a glorified string object whose content is the name of
*  an bond property (which must be unique amoung molecule's bond props).
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
#include "cx_molecule.h"

/*** Are bondtuple methods initialized? ***/

static int initialized = FALSE;

/*============================================================================
 *  btuple_destroy() -- destroy a given bondtuple object
 *
 *  This destroys the bondtuple properties.
 */
 
static void btuple_destroy(cx_Object tuple)
{
   cx_Object mol, tob, bond, bonds;
   cx_String tname;

   /*** Reject if not a bondtuple object. ***/

   if (CX_OB_BONDTUPLE != cx_type(tuple)) return;

   /*** Extract tuple name. ***/

   tname = (cx_String) cx_e_base_content(tuple);

   /*** Destroy bond properties of type tname. ***/

   if (tname) {
      mol   = cx_ancestor(tuple, CX_OB_MOLECULE);
      bonds = cx_stream(mol, CX_OB_BOND);
      while (NULL != (bond = cx_next(bonds))) {
         tob = cx_e_property(bond, tname);
	 cx_destroy(tob);
      }
      cx_destroy(bonds);
      cx_free(tname);
   }

   /*** Destroy base object and any children. ***/

   cx_e_base_destroy(tuple);
}

/*============================================================================
 *  btuple_stringvalue() -- return stringvalue of given bondtuple
 *
 *  This always quotes its data.
 */
 
static cx_String btuple_stringvalue(cx_Object tuple)
{
   cx_Object mol, bond, bonds;
   cx_String tname, s;
   int       cont;
 
   /*** Reject if not an bondtuple object. ***/
 
   if (CX_OB_BONDTUPLE != cx_type(tuple)) return NULL;
 
   /*** Extract tuple name. ***/
 
   if (NULL == (tname = (cx_String) cx_e_base_content(tuple))) return NULL;
 
   /*** Make semicolon-separated list of bond properties of type tname. ***/
 
   cx_scratchpad(NULL);
   cx_scratchpad( "\"" );
   mol   = cx_ancestor(tuple, CX_OB_MOLECULE);
   bonds = cx_stream(mol, CX_OB_SMIBONDS);
   for (cont = FALSE; NULL != (bond = cx_next(bonds)); cont = TRUE) {
 
      /*** Possibly add tuple-delimiter, add stringvalue. ***/
 
      s = cx_sprop(bond, tname);
      if (cont) cx_scratchpad( ";"  );
      if (s   ) cx_scratchpad( s    );
   }
   cx_scratchpad( "\"" );
 
   /*** Clean up and return static buffer. ***/
 
   cx_destroy(bonds);
   return cx_scratchpad("");  /* duplicate? remove trailing commas? */
}

/*============================================================================
 *  bondtuple_init() -- initialize bondtuple-specific functions
 *
 *  Called only once by cx_e_create_bondtuple().
 */
 
static cx_Integer bondtuple_init(void)
{
   /*** Define bondtyple object type. ***/
 
   cx_e_set_typename(CX_OB_BONDTUPLE, "Bondtuple");
 
   /*** No create, cexin, or send methods for bondtuples. ***/

   cx_set_method(CX_OB_BONDTUPLE, "cexin",  NULL);
   cx_set_method(CX_OB_BONDTUPLE, "send",   NULL);

   /*** Bondtuple-specific functions ***/

   cx_set_method(CX_OB_BONDTUPLE, "destroy",     btuple_destroy    );
   cx_set_method(CX_OB_BONDTUPLE, "stringvalue", btuple_stringvalue);

   /*** Use normal (base) count, stream, and setproperty methods. ***/

   cx_set_method(CX_OB_BONDTUPLE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_BONDTUPLE, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_BONDTUPLE, "setproperty", cx_e_base_setproperty);


   /*** Mark methods initialized and return successfully. ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  cx_e_create_bondtuple() -- creates bondtuple and sets initial value
 */
 
cx_Object cx_e_create_bondtuple(cx_Object mol, cx_String name)
{
   cx_Object ob, obs;
   cx_String str;

   /*** Initialize bondtyple methods once. ***/

   if (!initialized) bondtuple_init();

   /*** Require non-null bondtuple name. ***/

   if (NULL == name) return NULL;

   /*** Look for extant bondtuple with given name and destroy it. ***/

   obs = cx_stream(mol, CX_OB_BONDTUPLE);
   while (NULL != (ob = cx_next(obs))) {
      str = cx_e_base_content(ob);
      if (0 == cx_strcmp(str, name)) break;
   }
   cx_destroy(obs);
   if (ob) cx_destroy(ob);

   /*** Make new CX_OB_BONDTUPLE object with given parent. ***/

   if (NULL == (ob = cx_e_base_create(mol, CX_OB_BONDTUPLE))) return NULL;

   /*** Set content of new bondtuple object to copy of given name. ***/

   cx_e_base_set_content(ob, (void *) cx_strdup(name));

   /*** Return initialized bondtuple object. ***/

   return (cx_Object) ob;
}

/*============================================================================
 *  cx_e_bondtuple_name() -- return name of bondtuple (property)
 */
 
cx_String cx_e_bondtuple_name(cx_Object btuple)
{
   /*** Reject if not a bondtuple object. ***/
 
   if (CX_OB_BONDTUPLE != cx_type(btuple)) return NULL;
 
   /*** Return object content as cx_String. ***/
 
   return (cx_String) cx_e_base_content(btuple);
}

/*============================================================================
 *  cx_e_prefix2btuples() -- return mol's btuples w/names starting w/prefix
 */
 
cx_Object cx_e_prefix2btuples(cx_Object mol, cx_String prefix)
{
   cx_Object tuples, tuple, seq = cx_create_sequence(NULL);
   int       lens;
 
   /*** Return NULL if mol or prefix is NULL. ***/
 
   if (NULL == mol || NULL == prefix) return NULL;
 
   /*** Add tuples with given prefix to sequence. ***/
 
   lens   = strlen(prefix);
   tuples = cx_stream(mol, CX_OB_BONDTUPLE);
   while (NULL != (tuple = cx_next(tuples)))
      if (0 == cx_strncmp(prefix, (cx_String) cx_e_base_content(tuple), lens))
          cx_append(seq, tuple);
   cx_destroy(tuples);
 
   /*** Return (possibly empty) sequence of bondtuples. ***/
 
   cx_reset(seq);
   return seq;
}
