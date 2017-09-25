/*****************************************************************************
*  cx_molecule.h -- CX molecule package definitions
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

#ifndef CX_MOLECULE_INCLUDED
#define CX_MOLECULE_INCLUDED 1

/*****************************************************
 *  Supported classes defined in cx_types.h:
 *
 *     CX_OB_MOLECULE
 *     CX_OB_ATOM
 *     CX_OB_BOND
 *     CX_OB_ATOMTUPLE
 *     CX_OB_BONDTUPLE
 *     CX_OB_SMIATOMS
 *     CX_OB_SMIBONDS
 */

#include "cx_types.h"

/*** Molecule property names. ***/

#define CX_PROP_INSMI "input smiles"

/*** Atom and Bond property names. ***/

#define CX_PROP_INORD  "input order"
#define CX_PROP_VISIT  "visit"

/*** Atom property names. ***/

#define CX_PROP_ATNUMB "atomic number"
#define CX_PROP_ATSYMB "atomic symbol"
#define CX_PROP_ALABEL "atom label"
#define CX_PROP_IMPH   "implicit hcount"
#define CX_PROP_MASS   "mass"
#define CX_PROP_CHARGE "charge"
#define CX_PROP_CHIRAL "chirality"

/*** Bond property names. ***/

#define CX_PROP_BORDER "bond order"
#define CX_PROP_BSYMB  "bond symbol"
#define CX_PROP_DBO    "dbo"
#define CX_PROP_BLABEL "bond label"

/*** Chiral classes. ***/

#define CX_CHI_NONE     0       /* No chirality */
#define CX_CHI_TH       3       /* Tetrahedral */
#define CX_CHI_AL       4       /* Allene-like */
#define CX_CHI_SP       16      /* Square Planar */
#define CX_CHI_TB       17      /* Trigonal Bipyramidal */
#define CX_CHI_OH       18      /* Octahedral */

/*** Symbolic names for double bond orientations. ***/

#define CX_CHI_NO_DBO  0   /* No orientation */
#define CX_CHI_CIS     1   /* Cis (same-side) orientation */
#define CX_CHI_TRANS   2   /* Trans (opposite-side) orientation */
#define CX_CHI_UP      3   /* Single bond indicator "up" */
#define CX_CHI_DOWN    4   /* Single bond indicator "down" */
#define CX_CHI_FORWARD 5   /* Double bond sense (hi-hi or lo-lo) */
#define CX_CHI_REVERSE 6   /* Double bond sense (hi-lo or lo-hi) */

/*** C-wrappers for all public functions in molecule package. ***/

#define cx_atomtuple_name   cx_e_atomtuple_name
#define cx_bond             cx_e_bond
#define cx_bondtuple_name   cx_e_bondtuple_name
#define cx_create_atom      cx_e_create_atom
#define cx_create_atomtuple cx_e_create_atomtuple
#define cx_create_bond      cx_e_create_bond
#define cx_create_bondtuple cx_e_create_bondtuple
#define cx_create_molecule  cx_e_create_molecule
#define cx_mol_addhall      cx_e_mol_addhall
#define cx_mol_copy         cx_e_mol_copy
#define cx_mol_heavycopy    cx_e_mol_heavycopy
#define cx_mol_setimph      cx_e_mol_setimph
#define cx_mol_zaph         cx_e_mol_zaph
#define cx_molecule_pkg     cx_e_molecule_pkg
#define cx_prefix2atuples   cx_e_prefix2atuples
#define cx_prefix2btuples   cx_e_prefix2btuples
#define cx_smilin           cx_e_smilin
#define cx_xatom            cx_e_xatom

/*** Declarations of public functions in cx_molecule.c. ***/

cx_Object  cx_e_bond           (cx_Object a1,     cx_Object a2               );
cx_Object  cx_e_create_atom    (cx_Object mol                                );
cx_Object  cx_e_create_bond    (cx_Object a1,     cx_Object a2, cx_Integer bo);
cx_Object  cx_e_create_molecule(cx_Object parent                             );
cx_Integer cx_e_molecule_pkg   (void                                         );
cx_Object  cx_e_smilin         (cx_Object parent, cx_String smi              );
cx_Object  cx_e_xatom          (cx_Object fromat, cx_Object bond             );


/*** Declarations of public functions in cx_atomtuple.c. ***/

cx_Object cx_e_create_atomtuple(cx_Object mol,   cx_String name  );
cx_String cx_e_atomtuple_name  (cx_Object atuple                 );
cx_Object cx_e_prefix2atuples  (cx_Object mol,   cx_String prefix);

/*** Declarations of public functions in cx_bondtuple.c. ***/

cx_Object cx_e_create_bondtuple(cx_Object mol,   cx_String name  );
cx_String cx_e_bondtuple_name  (cx_Object btuple                 );
cx_Object cx_e_prefix2btuples  (cx_Object mol,   cx_String prefix);

/*** Declarations of public functions in cx_hydrogens.c. ***/

void cx_e_mol_zaph   (cx_Object mol);
void cx_e_mol_setimph(cx_Object mol);
void cx_e_mol_addhall(cx_Object mol);

/*** Declarations of public functions in cx_mol_copy.c. ***/

cx_Object cx_e_mol_copy     (cx_Object mol);
cx_Object cx_e_mol_heavycopy(cx_Object mol);


#endif /* CX_MOLECULE_INCLUDED */
