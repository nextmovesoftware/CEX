/*****************************************************************************
*  cu_mol_setpart.c -- set property indicating disconnected components
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
#include "cu_mol_setpart.h"

/*============================================================================
 *  cu_mol_setpart() - find disconnected components of molecule
 *
 *  This creates an integer atom property "part" for the given molecule
 *  corresponding to the 0-origin number of the disconnected part, where
 *  parts are numbered by their lowest numbered atom.  Returns the number
 *  total number of parts (disconnected components) found.
 */

static void dfspart(cx_Object atom, cx_String partprop, int partno)
{
   cx_Object bonds, bond, xatom;

   /*** Loop over connected atoms. ***/

   bonds = cx_stream(atom, CX_OB_BOND);
   while (NULL != (bond = cx_next(bonds))) {
      xatom = cx_xatom(atom, bond);

      /*** If connected atom is unvisited, set partprop and recurse. ***/

      if (-1 == cx_iprop(xatom, partprop)) {
         cx_set_iprop(xatom, partprop, partno);
         dfspart(xatom, partprop, partno);
      }
   }
   cx_destroy(bonds);
}

cx_Integer cu_mol_setpart(cx_Object mol, cx_String partprop)
{
   int       nparts = 0;
   cx_Object atoms, atom;

   /*** Initialize all atom partprop properties to -1 (unvisited). ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms)))
      cx_set_iprop(atom, partprop, -1);

   /*** Set parts via a depth-first search starting at unvisited atoms. ***/

   cx_reset(atoms);
   while (NULL != (atom = cx_next(atoms))) {
      if (-1 == cx_iprop(atom, partprop)) {
         cx_set_iprop(atom, partprop, nparts);
         dfspart(atom, partprop, nparts);
         nparts++;
      }
   }
   cx_destroy(atoms);

   /*** Return number of parts. ***/

   return nparts;
}

/*============================================================================
 *  cu_f_mol_setpart_() - fortran wrapper for cu_mol_setpart()
 */

cx_Integer cu_f_mol_setpart_(cx_Object *mol, cx_String partprop, cx_Integer mx)
{
   cx_String  pname;
   cx_Integer rv;

   /*** Return 0 if request is invalid. ***/

   if (NULL == *mol || NULL == partprop || 0 == mx) return 0;

   /*** Call cu_mol_setpart() with temporary C-string. ***/

   pname = cx_strndup(partprop, mx);
   rv = cu_mol_setpart(*mol, pname);
   cx_free(pname);
   return rv;
}
