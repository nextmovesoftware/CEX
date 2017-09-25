/*****************************************************************************
*  cx_mol_copy.c -- copy molecule various ways
*
*  cx_e_mol_copy(mol)
*
*    Return copy of molecule with the following properties also copied:
*       Molecule: "molname", "input smiles"
*       Atoms:    ATNUMB, ATSYMB, MASS, CHARGE, IMPH, CHIRAL
*       Bonds:    BORDER, DBO
*    Atom, bond & molecule children (except for atoms & bonds) aren't copied.
*
*  cx_e_mol_heavycopy(mol)
*
*    Return copy of molecule containing only heavy atoms and "abnormal"
*    hydrogens with "implicit hcount" property derived from previous value.
*    Hydrogens with specified mass or non-zero charge or not connected to
*    exactly one non-hydrogen are "abnormal" and are retained.
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

#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "cx_cobweb.h"
#include "cx_molecule.h"

/*============================================================================
 *  cx_mol_copy() -- copy molecule
 */

cx_Object cx_e_mol_copy(cx_Object mol)
{
   cx_Object  copy, atom, atoms, a1, a2, bond, bonds, cob, *coba, tord;
   cx_Integer na, bo, inord = 0;

   /*** Create a new molecule (copy) and copy molecular properties to it. ***/

   copy = cx_create_molecule(NULL);
   cx_set_sprop(copy, "molname",      cx_sprop(mol, "molname"     ));
   cx_set_sprop(copy, "input smiles", cx_sprop(mol, "input smiles"));

   /*** Create an array of atoms (aa) for crossreferencing. ***/

   na   = cx_count(mol, CX_OB_ATOM);
   coba = (cx_Object *) cx_malloc(na * sizeof(cx_Object));

   /*** Create an atomtuple (tord) for reverse crossreferencing. ***/

   tord = cx_create_atomtuple(mol, "_inord");

   /*** Create new atoms in copy and copy atomic properties. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms))) {
      cob = cx_create_atom(copy);
      cx_set_sprop(cob, CX_PROP_ATNUMB,  cx_sprop(atom, CX_PROP_ATNUMB));
      cx_set_sprop(cob, CX_PROP_ATSYMB,  cx_sprop(atom, CX_PROP_ATSYMB));
      cx_set_sprop(cob, CX_PROP_MASS,    cx_sprop(atom, CX_PROP_MASS  ));
      cx_set_sprop(cob, CX_PROP_CHARGE,  cx_sprop(atom, CX_PROP_CHARGE));
      cx_set_sprop(cob, CX_PROP_IMPH,    cx_sprop(atom, CX_PROP_IMPH  ));
      cx_set_sprop(cob, CX_PROP_CHIRAL,  cx_sprop(atom, CX_PROP_CHIRAL));

      /*** Save forward and reverse crossreferences. ***/

      coba[inord] = cob;
      cx_set_iprop(atom, "_inord", inord++);
   }
   cx_destroy(atoms);

   /*** Create new bonds in new molecule and copy bond properties. ***/

   bonds = cx_stream(mol, CX_OB_BOND);
   while (NULL != (bond = cx_next(bonds))) {

      /*** Create bonds between corresponding atoms. ***/

      atoms = cx_stream(bond, CX_OB_ATOM);
      a1    = coba[cx_iprop(cx_next(atoms), "_inord")];
      a2    = coba[cx_iprop(cx_next(atoms), "_inord")];
      bo    = cx_iprop(bond, CX_PROP_BORDER);
      cob   = cx_create_bond(a1, a2, bo);
      cx_destroy(atoms);

      /*** Copy bond properties. ***/

      cx_set_sprop(cob, CX_PROP_DBO, cx_sprop(bond, CX_PROP_DBO));
   }
   cx_destroy(bonds);

   /*** Free temp array and temp property, return copy of molecule. ***/

   cx_free(coba);
   cx_destroy(tord);
   return copy;
}

/*============================================================================
 *  cx_e_mol_heavycopy() -- copy molecule but only heavy atoms
 */

cx_Object cx_e_mol_heavycopy(cx_Object mol)
{
   cx_Object  copy, atom, atoms, a1, a2, bond, bonds, cob, *coba, tord;
   cx_Integer na, ia, bo, imph, inord;

   /*** Create a new molecule (copy) and copy molecular properties to it. ***/

   copy = cx_create_molecule(NULL);
   cx_set_sprop(copy, "molname",      cx_sprop(mol, "molname"     ));
   cx_set_sprop(copy, "input smiles", cx_sprop(mol, "input smiles"));

   /*** Create & initialize an array of atoms (aa) for crossreferencing. ***/

   na   = cx_count(mol, CX_OB_ATOM);
   coba = (cx_Object *) cx_malloc(na * sizeof(cx_Object));
   for (ia = 0; ia < na; ia++)
      coba[ia] = NULL;

   /*** Create an atomtuple (tord) for reverse crossreferencing. ***/

   tord = cx_create_atomtuple(mol, "_inord");

   /*** Copy heavy atoms and their properties to new molecule. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   for (inord = 0; NULL != (atom = cx_next(atoms)); inord++) {

      /*** Set forward crossreference (old atom's index into aa). ***/

      cx_set_iprop(atom, "_inord", inord);

      /*** Ignore hydrogens. ***/

      if (1 == cx_iprop(atom, "atomic number")) continue;

      /*** Create new atom, copy properties other than "implicit hcount". ***/

      cob = cx_create_atom(copy);
      cx_set_sprop(cob, CX_PROP_ATNUMB,  cx_sprop(atom, CX_PROP_ATNUMB));
      cx_set_sprop(cob, CX_PROP_ATSYMB,  cx_sprop(atom, CX_PROP_ATSYMB));
      cx_set_sprop(cob, CX_PROP_MASS,    cx_sprop(atom, CX_PROP_MASS  ));
      cx_set_sprop(cob, CX_PROP_CHARGE,  cx_sprop(atom, CX_PROP_CHARGE));
      cx_set_sprop(cob, CX_PROP_IMPH,    cx_sprop(atom, CX_PROP_IMPH  ));
      cx_set_sprop(cob, CX_PROP_CHIRAL,  cx_sprop(atom, CX_PROP_CHIRAL));

      /*** Save reverse crossreference (save new atom in aa). ***/

      coba[inord] = cob;
   }
   cx_destroy(atoms);

   /*** Create new bonds in new molecule and copy bond properties. ***/

   bonds = cx_stream(mol, CX_OB_BOND);
   while (NULL != (bond = cx_next(bonds))) {

      /*** Find new atoms corresponding to old atoms on old bond. ***/

      atoms = cx_stream(bond, CX_OB_ATOM);
      a1    = coba[cx_iprop(cx_next(atoms), "_inord")];
      a2    = coba[cx_iprop(cx_next(atoms), "_inord")];
      cx_destroy(atoms);

      /*** If a1 AND a2 are not NULL, make a normal bond. ***/

      if (NULL != a1 && NULL != a2) {
         bo    = cx_iprop(bond, CX_PROP_BORDER);
         cob   = cx_create_bond(a1, a2, bo);
         cx_set_sprop(cob, CX_PROP_DBO, cx_sprop(bond, CX_PROP_DBO));

      /*** If a1 XOR a2 are NULL, this is a bond to a zapped hydrogen. ***/

      } else if (NULL != a1 && NULL == a2) {
	 imph = cx_iprop(a1, "implicit hcount");
	 cx_set_iprop(a1, "implicit hcount", imph + 1);

      } else if (NULL == a1 && NULL != a2) {
	 imph = cx_iprop(a2, "implicit hcount");
	 cx_set_iprop(a2, "implicit hcount", imph + 1);
      }
   }
   cx_destroy(bonds);

   /*** Free temp array and temp property, return copy of molecule. ***/

   cx_free(coba);
   cx_destroy(tord);
   return copy;
}
