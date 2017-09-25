/*****************************************************************************
*  cx_mol_hydrogens.c -- various molecular hydrogen manipulations
*
*  void cx_e_mol_addhall(mol) -- make all hydrogens explicit
*    Makes all hydrogens explicit based on "implicit hcount" property,
*    i.e., adds "implicit hcount" hydrogens to extant atoms.
*
*  void cx_e_mol_zaph() -- remove normal hydrogens & adjust "implicit hcount"
*
*  void cx_e_mol_setimph(cx_Object mol)
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
 *  normh() -- return "normal" number of implicit hydrogens
 *
 *  This returns the unmet valence based on lowest normal valence for
 *  atom which is consistent with explicit bonds, else 0. 
 *
 *             Lowest normal valences at given oxidation state
 *           +---+---+-----+---+---+-----+-------+----+----+---+
 *  Element: | H | C |  N  | O | F |  P  |   S   | Cl | Br | I |
 *           +---+---+-----+---+---+-----+-------+----+----+---+
 *  Neutral: | 1 | 4 | 3,5 | 2 | 1 | 3,5 | 2,4,6 |  1 |  1 | 1 |
 *           +---+---+-----+---+---+-----+-------+----+----+---+
 *       +1: | 0 | 5 |  4  | 3 | - |  4  |   -   |  - |  - | - |
 *           +---+---+-----+---+---+-----+-------+----+----+---+
 *       -1: | - | 3 |  2  | 1 | 0 |  -  |   -   |  0 |  0 | 0 |
 *           +---+---+-----+---+---+-----+-------+----+----+---+
 *
 *  If not listed above, returns -1.
 */

static int normh(cx_Object atom)
{
   cx_Object  bond, bonds;
   int        tbo, val;

   /*** Return 0 for NULL or invalid atom. ***/

   if (NULL == atom || CX_OB_ATOM != cx_type(atom)) return 0;

   /*** Loop over atom's bonds, calculating total explicit bond order. ***/

   tbo   = 0;
   bonds = cx_stream(atom, CX_OB_BOND);
   while (NULL != (bond = cx_next(bonds)))
      tbo += cx_iprop(bond, "bond order");
   cx_destroy(bonds);

   /*** 11 elements have well-defined valences.  ***/
   /*** Switch on 100 * charge + atomic number. ***/

   switch (100 * cx_iprop(atom, "charge") + cx_iprop(atom, "atomic number")) {
      case    1:  val = 1;                                  break;  /* H  */
      case  101:  val = 0;                                  break;  /* H+ */
      case    5:  val = 3;                                  break;  /* B  */
      case -105:  val = 3;                                  break;  /* B- */
      case  105:  val = 4;                                  break;  /* B+ */
      case    6:  val = 4;                                  break;  /* C  */
      case -106:  val = 3;                                  break;  /* C- */
      case  106:  val = 5;                                  break;  /* C+ */
      case    7:  val = (3 < tbo ? 5 : 3);                  break;  /* N  */
      case -107:  val = 2;                                  break;  /* N- */
      case  107:  val = 4;                                  break;  /* N+ */
      case    8:  val = 2;                                  break;  /* O  */
      case  108:  val = 3;                                  break;  /* O+ */
      case -108:  val = 1;                                  break;  /* O- */
      case    9:  val = 1;                                  break;  /* F  */
      case -109:  val = 0;                                  break;  /* F- */
      case   15:  val = (3 < tbo ? 5 : 3);                  break;  /* P  */
      case  115:  val = 4;                                  break;  /* P+ */
      case   16:  val = (4 < tbo ? 6 : (2 < tbo ? 4 : 2));  break;  /* S  */
      case   17:  val = 1;                                  break;  /* Cl */
      case -117:  val = 0;                                  break;  /* Cl-*/
      case   35:  val = 1;                                  break;  /* Br */
      case -135:  val = 0;                                  break;  /* Br-*/
      case   53:  val = 1;                                  break;  /* I  */
      case -153:  val = 0;                                  break;  /* I- */
      default:    val = -1;                                 break;
   }

   /*** If not a known element/oxidation state, return -1. ***/

   if (-1 == val) return -1;

   /*** Normal hcount is valence - explicit bonds, but not negative. ***/

   if (val > tbo) return (val - tbo);
   return 0;
}

/*============================================================================
 *  cx_e_mol_zaph() -- remove normal hydrogens and adjust "implicit hcount"
 */

void cx_e_mol_zaph(cx_Object mol)
{
   cx_Object  xatom, atom, atoms, bond, bonds, doomed;
   cx_Integer nh;

   /*** Loop over non-hydrogens molecule. ***/

   doomed = cx_create_sequence(NULL);
   atoms  = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms))) {
      if (1 != cx_iprop(atom, "atomic number")) {

	 /*** Loop over bonds to this atom looking for simple H's. ***/

	 nh    = 0;
	 bonds = cx_stream(atom, CX_OB_BOND);
	 while (NULL != (bond = cx_next(bonds))) {
	    xatom = cx_xatom(atom, bond);

            /*** Doom if uncharged, unmassed, singly connected hydrogen. ***/

            if ( 1 == cx_iprop(xatom, "atomic number") &&
	         0 == cx_iprop(xatom, "charge"       ) &&
	         0 == cx_iprop(xatom, "mass"         ) &&
	         1 == cx_count(xatom, CX_OB_BOND     )    ) {
               cx_append(doomed, xatom);
	       nh++;
	    }
	 }
	 cx_destroy(bonds);

	 /*** Correct implicit hcount. ***/

	 if (nh) {
	    nh += cx_iprop(atom, "implicit hcount");
	    cx_set_iprop(atom, "implicit hcount", nh);
	 }
      }
   }
   cx_destroy(atoms);

   /*** Blow away doomed atoms. ***/

   cx_reset(doomed);
   while (NULL != (atom = cx_next(doomed)))
      cx_destroy(atom);
   cx_destroy(doomed);
}

/*============================================================================
 *  cx_e_mol_setimph() -- invents "implicit hcount" for common elements
 */

void cx_e_mol_setimph(cx_Object mol)
{
   cx_Object  atom, atoms;
   cx_Integer nh;

   /*** Loop over atoms in molecule, setting normal hcounts. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms)))
      if (0 <= (nh = normh(atom)))
	 cx_set_iprop(atom, "implicit hcount", nh);
   cx_destroy(atoms);
}

/*============================================================================
 *  cx_e_mol_addhall() -- make implicit hydrogens explicit
 *
 *  I.e., adds "implicit hcount" hydrogens to extant atoms.
 */

void cx_e_mol_addhall(cx_Object mol)
{
   cx_Object atom, atoms, hydro;
   int       imph;

   /*** Loop over atoms in molecule with implicit hcounts. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms))) {
      if (0 < (imph = cx_iprop(atom, "implicit hcount"))) {
	 while (imph--) {
	    hydro = cx_create_atom(mol);
            cx_set_iprop(hydro, "atomic number",   1 );
	    cx_set_sprop(hydro, "atomic symbol",  "H");
            cx_set_iprop(hydro, "charge",          0 );
            cx_set_iprop(hydro, "implicit hcount", 0 );
	    cx_create_bond(atom, hydro, 1);
	 }
      }
      cx_set_iprop(atom, "implicit hcount", 0);
   }
   cx_destroy(atoms);
}
