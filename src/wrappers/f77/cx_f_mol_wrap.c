/*****************************************************************************
*  cx_f_mol_wrap.c
*
*
*****************************************************************************/

#include <string.h>
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>

#include "cx.h"

/*** Variable type used for implicit Fortran string length ***/

typedef cx_Integer  FLENS;

/*** Fortran true and false. ***/

#define FTRUE  1
#define FFALSE 0

/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Some compilers append a "_" to fortran function names, but others
* do not.  Because this was written first for Sun (which does append
* "_"), the code has the "_" in the names.  For machines that don't
* use this convention, we just redefine these names back to their
* "real" values.
======================================================================*/

#if defined(HPUX) || defined(NEXT)

#define cx_f_atomtuple_name_	cx_f_atomtuple_name
#define cx_f_bond_		cx_f_bond
#define cx_f_bondtuple_name_	cx_f_bondtuple_name
#define cx_f_create_atom_	cx_f_create_atom
#define cx_f_create_atomtuple_	cx_f_create_atomtuple
#define cx_f_create_bond_	cx_f_create_bond
#define cx_f_create_bondtuple_	cx_f_create_bondtuple
#define cx_f_create_molecule_	cx_f_create_molecule
#define cx_f_mol_addhall_	cx_f_mol_addhall
#define cx_f_mol_copy_		cx_f_mol_copy
#define cx_f_mol_heavycopy_	cx_f_mol_heavycopy
#define cx_f_mol_setimph_	cx_f_mol_setimph
#define cx_f_mol_zaph_		cx_f_mol_zaph
#define cx_f_molecule_pkg_	cx_f_molecule_pkg
#define cx_f_prefix2atuples_	cx_f_prefix2atuples
#define cx_f_prefix2btuples_	cx_f_prefix2btuples
#define cx_f_smilin_		cx_f_smilin
#define cx_f_xatom_		cx_f_xatom

#endif


/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Linux-ppc appends "__" to each Fortran function name!
======================================================================*/

#if defined(LINUX)

#define cx_f_atomtuple_name_	cx_f_atomtuple_name__
#define cx_f_bond_		cx_f_bond__
#define cx_f_bondtuple_name_	cx_f_bondtuple_name__
#define cx_f_create_atom_	cx_f_create_atom__
#define cx_f_create_atomtuple_	cx_f_create_atomtuple__
#define cx_f_create_bond_	cx_f_create_bond__
#define cx_f_create_bondtuple_	cx_f_create_bondtuple__
#define cx_f_create_molecule_	cx_f_create_molecule__
#define cx_f_mol_addhall_	cx_f_mol_addhall__
#define cx_f_mol_copy_		cx_f_mol_copy__
#define cx_f_mol_heavycopy_	cx_f_mol_heavycopy__
#define cx_f_mol_setimph_	cx_f_mol_setimph__
#define cx_f_mol_zaph_		cx_f_mol_zaph__
#define cx_f_molecule_pkg_	cx_f_molecule_pkg__
#define cx_f_prefix2atuples_	cx_f_prefix2atuples__
#define cx_f_prefix2btuples_	cx_f_prefix2btuples__
#define cx_f_smilin_		cx_f_smilin__
#define cx_f_xatom_		cx_f_xatom__

#endif

/*============================================================================
 *  c2fstr -- copy C char array to fortran string
 *
 *  This copies MIN(lens,mxout) chars of str to out and returns the number of
 *  characters copied; if less than mxout, out is blank-padded to mxout - 1.
 */

static cx_Integer c2fstr(cx_Integer lens, cx_String str, cx_String out,
                         FLENS mxout)
{
   if (NULL == out  ) return 0;
   if (lens >  mxout) lens = mxout;
   if (NULL == str  ) lens = 0;
   if (0    <  lens ) memcpy(out, str, lens);
   if (lens <  mxout) memset(out + lens, ' ', mxout - lens);
   return lens;
}

/*============================================================================
 *  f2cstr -- copy Fortran string to C string
 */

static cx_String f2cstr(cx_Integer lens, cx_String str)
{
   cx_String string;
   if (0    > lens) lens = 0;
   if (NULL == str) str = "";
   string = (cx_String) malloc(lens + 1);
   strncpy(string, str, lens);
   string[lens] = '\0';
   return string;
}

/*============================================================================
 *  cx_f_atomtuple_name_
 */

cx_Integer cx_f_atomtuple_name_(cx_Object *tuple, cx_String buf, FLENS mx)
{
   cx_String str = cx_atomtuple_name(*tuple);
   return c2fstr(strlen(str), str, buf, mx);
}

/*============================================================================
 *  cx_f_bond_
 */

cx_Integer cx_f_bond_(cx_Object *a1, cx_Object *a2)
{
   return (cx_Integer) cx_bond(*a1, *a2);
}

/*============================================================================
 *  cx_f_bondtuple_name_
 */

cx_Integer cx_f_bondtuple_name_(cx_Object *tuple, cx_String buf, FLENS mx)
{
   cx_String str = cx_bondtuple_name(*tuple);
   return c2fstr(strlen(str), str, buf, mx);
}

/*============================================================================
 *  cx_f_create_atom_
 */

cx_Integer cx_f_create_atom_(cx_Object *mol)
{
   return (cx_Integer) cx_create_atom(*mol);
}

/*============================================================================
 *  cx_f_create_atomtuple_
 */

cx_Integer cx_f_create_atomtuple_(cx_Object *mol, cx_String str, FLENS lens)
{
   cx_String s  = f2cstr(lens, str);
   cx_Object ob = cx_create_atomtuple(*mol, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_create_bond_
 */

cx_Integer cx_f_create_bond_(cx_Object *a1, cx_Object *a2, cx_Integer *bo)
{
   return (cx_Integer) cx_create_bond(*a1, *a2, *bo);
}

/*============================================================================
 *  cx_f_create_bondtuple_
 */

cx_Integer cx_f_create_bondtuple_(cx_Object *mol, cx_String str, FLENS lens)
{
   cx_String s  = f2cstr(lens, str);
   cx_Object ob = cx_create_bondtuple(*mol, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_create_molecule_
 */

cx_Integer cx_f_create_molecule_(cx_Object *parent)
{
   return (cx_Integer) cx_create_molecule(*parent);
}

/*============================================================================
 *  cx_f_mol_addhall_
 */

void cx_f_mol_addhall_(cx_Object *mol)
{
   cx_mol_addhall(*mol);
}

/*============================================================================
 *  cx_f_mol_copy_
 */

cx_Integer cx_f_mol_copy_(cx_Object *mol)
{
   return (cx_Integer) cx_mol_copy(*mol);
}

/*============================================================================
 *  cx_f_mol_heavycopy_
 */

cx_Integer cx_f_mol_heavycopy_(cx_Object *mol)
{
   return (cx_Integer) cx_mol_heavycopy(*mol);
}

/*============================================================================
 *  cx_f_mol_setimph_
 */

void cx_f_mol_setimph_(cx_Object *mol)
{
   cx_mol_setimph(*mol);
}

/*============================================================================
 *  cx_f_mol_zaph_
 */

void cx_f_mol_zaph_(cx_Object *mol)
{
   cx_mol_zaph(*mol);
}

/*============================================================================
 *  cx_f_molecule_pkg_
 */

cx_Integer cx_f_molecule_pkg_(void)
{
   return (cx_Integer) cx_molecule_pkg();
}

/*============================================================================
 *  cx_f_prefix2atuples_
 */

cx_Integer cx_f_prefix2atuples_(cx_Object *mol, cx_String prefix, FLENS mx)
{
   cx_String s  = f2cstr(mx, prefix);
   cx_Object ob = cx_prefix2atuples(*mol, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_prefix2btuples_
 */

cx_Integer cx_f_prefix2btuples_(cx_Object *mol, cx_String prefix, FLENS mx)
{
   cx_String s  = f2cstr(mx, prefix);
   cx_Object ob = cx_prefix2btuples(*mol, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_smilin_
 */

cx_Integer cx_f_smilin_(cx_Object *par, cx_String smi, FLENS lens)
{
   cx_String str = f2cstr(lens, smi);
   cx_Object rv  = cx_smilin(*par, str);
   cx_free(str);
   return (cx_Integer) rv;
}

/*============================================================================
 *  cx_f_xatom_
 */

cx_Integer cx_f_xatom_(cx_Object fromat, cx_Object bond)
{
   return (cx_Integer) cx_xatom(fromat, bond);
}
