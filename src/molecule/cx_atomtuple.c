/*****************************************************************************
*  cx_atomtuple.c -- support for CX Atomtuple objects
*
*  An atomtuple is a glorified string object whose content is the name of
*  an atom property (which must be unique amoung molecule's atom props).
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

/*** Are atomtuple methods initialized? ***/
 
static int initialized = FALSE;

/*============================================================================
 *  atuple_destroy() -- destroy a given atomtuple object
 *
 *  This destroys the atomtuple properties.
 */
 
static void atuple_destroy(cx_Object tuple)
{
   cx_Object mol, tob, atom, atoms;
   cx_String tname;

   /*** Reject if not an atomtuple object. ***/

   if (CX_OB_ATOMTUPLE != cx_type(tuple)) return;

   /*** Extract tuple name. ***/

   tname = (cx_String) cx_e_base_content(tuple);

   /*** Destroy atom properties of type tname. ***/

   if (tname) {
      mol   = cx_ancestor(tuple, CX_OB_MOLECULE);
      atoms = cx_stream(mol, CX_OB_ATOM);
      while (NULL != (atom = cx_next(atoms))) {
         tob = cx_e_property(atom, tname);
	 cx_destroy(tob);
      }
      cx_destroy(atoms);
      cx_free(tname);
   }

   /*** Destroy base object and any children. ***/

   cx_e_base_destroy(tuple);
}

/*============================================================================
 *  atuple_stringvalue() -- return stringvalue of given atomtuple
 *
 *  This always quotes its data.
 */
 
static cx_String atuple_stringvalue(cx_Object tuple)
{
   cx_Object mol, atom, atoms;
   cx_String tname, s;
   int       cont;

   /*** Reject if not an atomtuple object. ***/

   if (CX_OB_ATOMTUPLE != cx_type(tuple)) return NULL;

   /*** Extract tuple name. ***/

   if (NULL == (tname = (cx_String) cx_e_base_content(tuple))) return NULL;

   /*** Make semicolon-separated list of atom properties of type tname. ***/

   cx_scratchpad(NULL);
   cx_scratchpad( "\"" );
   mol   = cx_ancestor(tuple, CX_OB_MOLECULE);
   atoms = cx_stream(mol, CX_OB_SMIATOMS);
   for (cont = FALSE; NULL != (atom = cx_next(atoms)); cont = TRUE) {

      /*** Possibly add tuple-delimiter, add stringvalue. ***/

      s = cx_sprop(atom, tname);
      if (cont) cx_scratchpad( ";" );
      if (s   ) cx_scratchpad( s   );
   }

   /*** Add final quote. ***/

   cx_scratchpad( "\"" );

   /*** Clean up and return static buffer. ***/

   cx_destroy(atoms);
   return cx_scratchpad("");  /* duplicate? remove trailing commas? */
}

/*============================================================================
 *  atomtuple_init() -- initialize atomtuple-specific functions
 *
 *  Called only by cx_e_create_atomtuple().
 */
 
static cx_Integer atomtuple_init(void)
{
   /*** Define atomtuple object type. ***/
 
   cx_e_set_typename(CX_OB_ATOMTUPLE, "Atomtuple");
 
   /*** No cexin or send methods for atomtuples. ***/

   cx_set_method(CX_OB_ATOMTUPLE, "cexin",  NULL);
   cx_set_method(CX_OB_ATOMTUPLE, "send",   NULL);

   /*** Atomtuple-specific functions ***/

   cx_set_method(CX_OB_ATOMTUPLE, "destroy",     atuple_destroy    );
   cx_set_method(CX_OB_ATOMTUPLE, "stringvalue", atuple_stringvalue);

   /*** Use normal (base) count, stream, and setproperty methods. ***/

   cx_set_method(CX_OB_ATOMTUPLE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_ATOMTUPLE, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_ATOMTUPLE, "setproperty", cx_e_base_setproperty);

   /*** Mark methods initialized and return successfully. ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  cx_e_create_atomtuple() -- creates atomtuple and sets initial value
 *
 *  This doesn't actually check that mol is a molecule, so this could
 *  presumably work for any atom-containing object.
 */
 
cx_Object cx_e_create_atomtuple(cx_Object mol, cx_String name)
{
   cx_Object ob, obs;
   cx_String str;

   /*** Initialize atomtuple methods once. ***/

   if (!initialized) atomtuple_init();

   /*** Require non-null atomtuple name. ***/

   if (NULL == name) return NULL;

   /*** Look for extant atomtuple with given name and destroy it. ***/

   obs = cx_stream(mol, CX_OB_ATOMTUPLE);
   while (NULL != (ob = cx_next(obs))) {
      str = cx_e_base_content(ob);
      if (0 == cx_strcmp(str, name)) break;
   }
   cx_destroy(obs);
   if (ob) cx_destroy(ob);

   /*** Make new CX_OB_ATOMTUPLE object with given parent mol. ***/

   if (NULL == (ob = cx_e_base_create(mol, CX_OB_ATOMTUPLE))) return NULL;

   /*** Set content of new atomtuple object to copy of given name. ***/

   cx_e_base_set_content(ob, (void *) cx_strdup(name));

   /*** Return initialized atomtuple object. ***/

   return (cx_Object) ob;
}

/*============================================================================
 *  cx_e_atomtuple_name() -- return name of atomtuple (property)
 */
 
cx_String cx_e_atomtuple_name(cx_Object atuple)
{
   /*** Reject if not an atomtuple object. ***/

   if (CX_OB_ATOMTUPLE != cx_type(atuple)) return NULL;

   /*** Return object content as cx_String. ***/

   return (cx_String) cx_e_base_content(atuple);
}

/*============================================================================
 *  cx_e_prefix2atuples() -- return mol's atuples w/ names starting w/prefix
 */
 
cx_Object cx_e_prefix2atuples(cx_Object mol, cx_String prefix)
{
   cx_Object tuples, tuple, seq = cx_create_sequence(NULL);
   int       lens;
 
   /*** Return NULL if mol or prefix is NULL. ***/
 
   if (NULL == mol || NULL == prefix) return NULL;
 
   /*** Add tuples with given prefix to sequence. ***/
 
   lens   = strlen(prefix);
   tuples = cx_stream(mol, CX_OB_ATOMTUPLE);
   while (NULL != (tuple = cx_next(tuples)))
      if (0 == cx_strncmp(prefix, (cx_String) cx_e_base_content(tuple), lens))
          cx_append(seq, tuple);
   cx_destroy(tuples);
 
   /*** Return (possibly empty) sequence of atomtuples. ***/
 
   cx_reset(seq);
   return seq;
}
