/*****************************************************************************
*  cx_molecule.c -- support for Molecule, Atom, and Bond objects
*
*  The code in this file is designed to be robust and self-contained.
*  Compared to the code in the Daylight SMILES toolkit, this code sacrifices
*  execution speed (no hashing), interface simplicity (not object oriented),
*  and power (no uniquification or "mod_on" mode).  However, it does parse
*  and generate the 4.4x language with simple, supportable code.
*
*  The C interface is defined in cx_mol.h.
*
*  This package uses the "cx_basics", "cx_util" and "cx_err" packages.
*
*  Limitations:
*    Elements ..................................... 0=*,1=H,..,105=Ha
*    Bond types (bond order) ...................... 1, 2, 3
*    Maximum number of atoms per molecule ......... 2^31 (2147483648)
*    Maximum number of bonds per molecule ......... 2^31 (2147483648)
*    Maximum number of bonds per atom ............. 2^31 (2147483648)
*    Maximum number of atoms per bond ............. 2
*    Maximum absolute charge per atom ............. 2^15 (32768)
*    Maximum atomic mass .......................... 2^15 (32768)
*    Maximum number of implicit H's per atom ...... 2^15 (32768)
*    Maximum depth of branch nesting .............. 20
*    Maximum number of concurrent ring closures ... 1000
*
*  To do:
*   [x] Use atom indexes rather than pointers in bond structure
*   [x] genes needs to write ring closure bond orders once
*   [x] Clean up spew function
*   [-] Possibly add bond array (deal with nb?)
*   [x] Chirality
*   [x] dbo
*   [-] transition metals?
*   [x] reallocate genes as needed
*   [ ] mark molecule edited and/or uncache smiles
*   [x] use error function
*   [x] error not detected for C1=CCCCC-1, but C-1CCCCC=1 is found
*   [x] error not detected for CC[+NH2]CC, interpreted as C[CH2+][NH2]CC
*   [x] smilin should check for more than one bond between pairs of atoms
*   [ ] quote explicit H's
*
*******************************************************************************
*
*  Notes on double bond orientation.
*
*   The method used here for dealing with double bond orientation differs from
*   that used in the Daylight Toolkit (where dbo's are strictly bond attribs).
*   This way is neither as efficient or powerful, but avoids the "clever"
*   bitshifting codes and provides atom-oriented dbo specification.
*
*   o  Double bond orientation is defined for (at least) doubly-substututed
*      double bonds as FORWARD if the substituent atoms with higher index
*      on each end are on the same side (REVERSE if on opposite sides).
*      This attribute is accessed internally via set_dbo() and get_dbo() and
*      externally via cx_bond_set_dbo() and cx_bond_dbo().
*
*   o  An internal function get_iaa2dbo() is like get_dbo() except that it
*      returns the "highest" orientation (doesn't require double bond spec).
*      get_iaa2dbo() is intended for testing and debugging.
*
*   o  Single bonds can have direction UP or DOWN, interpreted a1->bond->a2
*      e.g., UP means a1->a2 is UP, DOWN means a1->a2 is DOWN.  Functions
*      set_bond_dir() and get_bond_dir() are used to access this attribute.
*
*   o  The utility function find_dss_bonds() finds =,-,- bonds to an atom.
*
*   o  The only use of bond direction is to set dbo's via set_bond_dbos().
*      Bond directions are used for temporary description set_bond_dbos().
*
*----------------------------------------------------------------------------
*
* Atom object
*   header
*      parent .... molecule
*   attributes
*      bonds  ..... stream of bonds
*   properties
*      inord ...... "input order"      "1"      - "2147483648"
*      atnum ...... "atomic number"    "0"      - "105"
*      imph ....... "implicit hcount"  "0"      - "32768"
*      mass ....... "mass"             "0"      - "32768"
*      charge ..... "charge"           "-32768" - "32768"
*      chiral ..... "chirality"         per CX_CHI_* flags
*    > visit ...... "visit"            "0"      - "1"
*    > label ...... "atom label"       e.g., "[13CH5+]"
*   o  atsym ...... "atomic symbol"    "*"      - "Ha"
*
* Bond object
*   header
*      parent .... molecule
*   attributes
*      atoms ...... stream of atoms
*   properties
*      order ...... "bond order"      "1"      - "3"
*      symbol ..... "bond symbol"     "-", "=", "#"
*      dbo ........ "dbo"             "none", "cis", "trans"
*    > visit ...... "visit"           "0"      - "1"
*
*  Molecule object
*   header
*      parent ..... molecule
*   attributes
*      atoms ...... stream of atoms
*      bonds  ..... stream of bonds
*   properties
*    x smiles ..... generated SMILES
*    x insmi ...... input SMILES
*    x molname .... text following first space
*
*----------------------------------------------------------------------------
*  Modified 12 Jan 2000 for cex131 to allow more than 100 ring closures:
*    Defined MX_RINGS to be 1000 (previously fixed at 100).
*    Created ^nnn extension to syntax.
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

/*** Internal molecule struct only contains "invisible" parts. ***/

typedef struct {
   int       modified;       /* has mol been modified since smiles call? */
   cx_Object asord,  bsord;  /* sequence of atoms, bonds in SMILES order */
   cx_String genes;          /* generated SMILES */
} CI_MOL;

/*** Internal definitions. ***/

#define MX_NESTING         4096
#define MX_CHAR_PER_ALABEL 20
#define MX_ELEMENTS        105
 
static char *ptab[] = {
    "*",  /* Slot for wildcard atom. */
   "H",  "He", "Li", "Be", "B",  "C",  "N",  "O",  "F",  "Ne",
   "Na", "Mg", "Al", "Si", "P",  "S",  "Cl", "Ar", "K",  "Ca",
   "Sc", "Ti", "V",  "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn",
   "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y",  "Zr",
   "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn",
   "Sb", "Te", "I",  "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd",
   "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
   "Lu", "Hf", "Ta", "W",  "Re", "Os", "Ir", "Pt", "Au", "Hg",
   "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th",
   "Pa", "U",  "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm",
   "Md", "No", "Lr", "Rf", "Ha",
   NULL  /* Sentinel at the end */
};

/*** Internal definition -- new for cex131 (previously 100). ***/

#define MX_RINGS 1000

/*** Handy macros. ***/

#define NOT_MOLECULE(o) (NULL == o || CX_OB_MOLECULE != cx_type(o))
#define NOT_ATOM(o)     (NULL == o || CX_OB_ATOM     != cx_type(o))
#define NOT_BOND(o)     (NULL == o || CX_OB_BOND     != cx_type(o))

/*** Are molecule methods initialized?  Not initially. ***/

static int initialized = FALSE;

/*** Prototype ***/

static int normh(cx_Object);

/*** Handy non-object-oriented static utilities ***/

/*============================================================================
 *  num2sym() -- convert atomic number to atomic symbol, or NULL on error.
 */

static char *num2sym(int *plen, int atnum)
{
    if (atnum < 0 || atnum > MX_ELEMENTS) return NULL;
    *plen = strlen(ptab[atnum]);
    return ptab[atnum];
}
 
/*============================================================================
 *  sym2num() -- convert atomic symbol to atomic number, or -1 on error.
 */


static int sym2num(int len, char *atsym)
{
   int  atnum;
   char *ps;

   /*** One character symbols. ***/

   if (1 == len) {
      for (atnum = 0; ptab[atnum]; atnum++) {
         ps = ptab[atnum];
         if ('\0' == *(ps + 1) && *ps == *atsym) return atnum;
      }

   /*** Two character symbols. ***/

   } else {
      for (atnum = 0; ptab[atnum]; atnum++) {
         ps = ptab[atnum];
         if (0 == strncmp(ps, atsym, len)) return atnum;
      }
   }

   /*** Not found. ***/

   return -1;
}
 
/*============================================================================
 *  needquote() -- must atoms of specified atomic number be quoted in SMILES?
 */

static int needquote(int atnum)
{
   switch (atnum) {
      case  0:  /* *  */
      case  5:  /* B  */
      case  6:  /* C  */
      case  7:  /* N  */
      case  8:  /* O  */
      case  9:  /* F  */
      case 15:  /* P  */
      case 16:  /* S  */
      case 17:  /* Cl */
      case 35:  /* Br */
      case 53:  /* I  */
         return FALSE;
      default:
         return TRUE;
   }
}


/*** Object-level support for local versions of polymorphic methods. ***/

/*============================================================================
 *  molecule_is_modified() and mark_molecule_modified()
 */

static cx_Integer molecule_is_modified(cx_Object mol)
{
   CI_MOL *ms = cx_e_base_content(mol);
   return (ms ? ms->modified : FALSE);
}

static void mark_molecule_modified(cx_Object mol, cx_Integer boo)
{
   CI_MOL *ms = cx_e_base_content(mol);
   if (ms) ms->modified = boo;
}

/*** Local versions of polymorphic methods on atoms follow. ***/

/*============================================================================
 *  atom_destroy() -- destroy an atom object
 */

static void atom_destroy(cx_Object atom)
{
   cx_Object bond, bonds, xatom, xbonds, doomed;

   /*** Content is a stream of bonds -- copy them to a safe place. ***/

   doomed = cx_create_sequence(NULL);
   bonds  = cx_e_base_content(atom);
   cx_reset(bonds);
   while (NULL != (bond = cx_next(bonds))) {

      /*** Doom the bond. ***/

      cx_append(doomed, bond);

      /*** Find atom across bond, remove bond from its stream of bonds. ***/

      xatom  = cx_xatom(atom, bond);
      xbonds = cx_e_base_content(xatom);
      cx_e_base_delete(xbonds, bond);
   }

   /*** Destroy this atom's stream of bonds. ***/

   cx_destroy(bonds);

   /*** Destroy bonds from this atom in a low-level manner. ***/

   cx_reset(doomed);
   while (NULL != (bond = cx_next(doomed))) {
      cx_destroy(cx_e_base_content(bond));
      cx_e_base_destroy(bond);
   }
   cx_destroy(doomed);

   /*** Destroy this atom's base structure and any children. ***/

   cx_e_base_destroy(atom);
}

/*============================================================================
 *  atom_count() -- count class over atoms (special for count of bonds)
 *
 *  Caller (cx_stream dispatcher) guarantees that atom is actually an atom.
 */

static cx_Integer atom_count(cx_Object atom, cx_Integer type)
{
   cx_Object  bonds;
   cx_Integer count = 0;

   /*** A stream of bonds is special, since they're not children of atom. ***/

   if (CX_OB_BOND == type) {
      bonds = cx_e_base_content(atom);
      cx_reset(bonds);
      while (NULL != cx_next(bonds))
         count++;
      return count;
   }

   /*** Otherwise, use base count function. ***/

   return cx_e_base_count(atom, type);
}

/*============================================================================
 *  atom_stream() -- return stream over atoms (special for stream of bonds)
 *
 *  Caller (cx_stream dispatcher) guarantees that atom is actually an atom.
 */

static cx_Object atom_stream(cx_Object atom, cx_Integer type)
{
   cx_Object bond, bonds, copy;

   /*** A stream of bonds is special, since they're not children of atom. ***/

   if (CX_OB_BOND == type) {
      copy  = cx_e_create_stream(atom);
      bonds = cx_e_base_content(atom);
      cx_reset(bonds);
      while (NULL != (bond = cx_next(bonds)))
         cx_e_base_append(copy, bond);
      cx_reset(copy);
      return copy;
   }

   /*** Otherwise, use base stream function. ***/

   return cx_e_base_stream(atom, type);
}

/*============================================================================
 *  atom_setproperty() -- does base function but marks molecule modified
 */
 
static cx_Object atom_setproperty(cx_Object atom, cx_String nam, cx_String val)
{
   cx_Integer boo = FALSE;
   cx_Object  mol = cx_parent(atom);

   /*** Mark molecule modified only if really needed. ***/

   if (FALSE == molecule_is_modified(mol)) {
      if      (0 == cx_strcmp(nam, CX_PROP_ATNUMB)) boo = TRUE;
      else if (0 == cx_strcmp(nam, CX_PROP_IMPH)  ) boo = TRUE;
      else if (0 == cx_strcmp(nam, CX_PROP_CHARGE)) boo = TRUE;
      else if (0 == cx_strcmp(nam, CX_PROP_MASS)  ) boo = TRUE;
      else if (0 == cx_strcmp(nam, CX_PROP_CHIRAL)) boo = TRUE;
      if (boo) mark_molecule_modified(mol, TRUE);
   }

   /*** Call normal base function and return result. ***/

   return cx_e_base_setproperty(atom, nam, val);
}

/*============================================================================
 *  atom_label() -- return atom label for given atom, or NULL
 */

static cx_String atom_label(cx_Object atom, int verb)
{
   static char lab[MX_CHAR_PER_ALABEL];
   char        *p, *s;
   int         lens;
   int         an     = cx_iprop(atom, "atomic number"  );
   int         mass   = cx_iprop(atom, "mass"           );
   int         chiral = cx_iprop(atom, "chirality"      );
   int         charge = cx_iprop(atom, "charge"         );
   int         imph   = cx_iprop(atom, "implicit hcount");

   /*** If abbrieviated label desired and sufficient, return atomic symbol ***/

   if (FALSE       == verb          &&   /* not verbose request */
       FALSE       == needquote(an) &&   /* has default valence */
       0           == mass          &&   /* mass not specified  */
       0           == chiral        &&   /* mass not specified  */
       0           == charge        &&   /* no formal charge    */
       normh(atom) == imph               /* normal hcount      */
      ) return num2sym(&lens, an);

   /*** Start with open-bracket. ***/

   p   = lab;
  *p++ = '[';

   /*** Add mass if specified. ***/

   if (0 != mass) { sprintf(p, "%d", mass); p += strlen(p); }

   /*** Add atomic symbol. ***/

   s = num2sym(&lens, an);
   strncpy(p, s, lens);
   p += lens;

   /*** Add chirality symbol @ or @@ as needed. ***/

#ifdef LATERCHIRAL
   if (CX_CHI_NONE != chiral) {
      *p++ = '@';
      if (CX_CHI_THCW == chiral) *p++ = '@';
   }
#endif

   /*** Add implicit hydrogen count if non-zero. ***/

   if (0 < imph) {
      *p++ = 'H';
      if (1 < imph) { sprintf(p, "%d", imph); p += strlen(p); }
   }

   /*** Add charge if non-zero. ***/

   if (0 != charge) {
      if      ( 1 == charge) *p++ = '+';
      else if (-1 == charge) *p++ = '-';
      else    { sprintf(p, "%+d", charge); p += strlen(p); }
   }

   /*** Add close-bracket and NULL-terminate. ***/

   *p++ = ']';
   *p   = '\0';

   /*** Return pointer to static string. ***/

   return lab;
}

/*============================================================================
 *  atom_stringvalue() -- atom's stringvalue is verbose atom label
 */

static cx_String atom_stringvalue(cx_Object atom)
{
   return atom_label(atom, TRUE);
}

/*** Local versions of polymorphic methods on bonds follow. ***/

/*============================================================================
 *  bond_destroy() -- destroy an bond object
 */

static void bond_destroy(cx_Object bond)
{
   cx_Object atom, atoms;

   /*** Content is a stream of atoms. ***/

   atoms = cx_e_base_content(bond);
   cx_reset(atoms);

   /*** Remove this bond from atoms' streams of bonds. ***/

   while (NULL != (atom = cx_next(atoms)))
      cx_e_base_delete(cx_e_base_content(atom), bond);

   /*** Destroy content and base structure and any children. ***/

   cx_destroy(atoms);
   cx_e_base_destroy(bond);
}

/*============================================================================
 *  bond_setproperty() -- does base function but marks molecule modified
 */
 
static cx_Object bond_setproperty(cx_Object bond, cx_String nam, cx_String val)
{
   cx_Integer boo = FALSE;
   cx_Object  mol = cx_parent(bond);

   /*** Mark molecule modified only if really needed. ***/

   if (FALSE == molecule_is_modified(mol)) {
      if      (0 == cx_strcmp(nam, CX_PROP_BORDER)) boo = TRUE;
      else if (0 == cx_strcmp(nam, CX_PROP_DBO)   ) boo = TRUE;
      if (boo) mark_molecule_modified(mol, TRUE);
   }

   /*** Call normal base function and return result. ***/

   return cx_e_base_setproperty(bond, nam, val);
}


/*============================================================================
 *  bond_count() -- count class over bonds (special for count of atoms)
 *
 *  Caller (cx_stream dispatcher) guarantees that bond is actually an bond.
 */

static cx_Integer bond_count(cx_Object bond, cx_Integer type)
{
   cx_Object  atoms;
   cx_Integer count = 0;

   /*** A stream of atoms is special, since they're not children of bond. ***/

   if (CX_OB_ATOM == type) {
      atoms = cx_e_base_content(bond);
      cx_reset(atoms);
      while (NULL != cx_next(atoms))
         count++;
      return count;
   }

   /*** Otherwise, use base count function. ***/

   return cx_e_base_count(bond, type);
}

/*============================================================================
 *  bond_stream() -- return stream over bonds (special for stream of atoms)
 *
 *  Caller (cx_stream dispatcher) guarantees that bond is actually a bond.
 */

static cx_Object bond_stream(cx_Object bond, cx_Integer type)
{
   cx_Object atom, atoms, copy;

   /*** A stream of atoms is special, since they're not children of bond. ***/

   if (CX_OB_ATOM == type) {
      copy  = cx_e_create_stream(bond);
      atoms = cx_e_base_content(bond);
      cx_reset(atoms);
      while (NULL != (atom = cx_next(atoms)))
         cx_e_base_append(copy, atom);
      cx_reset(copy);
      return copy;
   }

   /*** Otherwise, use base stream function. ***/

   return cx_e_base_stream(bond, type);
}

/*** Local versions of polymorphic methods on molecules follow. ***/

/*============================================================================
 *  append_rc() -- append ring closure bond symbol and digit to label
 *
 *  Ring closure symbol is added only if bo is 2 or 3.
 */

static void append_rc(cx_Object atom, int ir, int bo, int bdir)
{
   char *alab, *str, *bsym = "";
   int   lens;

   /*** Select bond symbols based primarily on bond order. ***/

   switch (bo) {

      /*** Direction bonds are kinds of single bonds. ***/

      case 1:  if      (CX_CHI_UP   == bdir) bsym = "/";
               else if (CX_CHI_DOWN == bdir) bsym = "\\";
               break;

      /*** Double and single bonds are indicated. ***/

      case 2:  bsym = "="; break;
      case 3:  bsym = "#"; break;

      /*** Other bond orders shouldn't happen. ***/

      default: bsym = "";  break;
   }

   /*** Allocate new label. ***/

   alab   = cx_sprop(atom, "label");
   lens   = strlen(alab) + 5;
   str    = (char *) cx_malloc(lens * sizeof(char));

   /*** Write one or two (or three) digit ring closures. ***/

   if      (10  > ir) sprintf(str, "%s%s%d",   alab, bsym, ir);
   else if (100 > ir) sprintf(str, "%s%s%%%d", alab, bsym, ir);
   else               sprintf(str, "%s%s^%d",  alab, bsym, ir);

   /*** Free old label and point to new one. ***/

   cx_set_sprop(atom, "label", str);
   cx_free(str);
}

/*============================================================================
 *  genpart() -- generate part of a SMILES starting at atom
 */

static void genpart(cx_Object   atom,   /* starting atom */
                    int        *pnrc,   /* ptr to number of ring closures */
                    char      **buf,    /* output buffer */
                    int        *pms,    /* ptr to max size of buf */
                    char      **pp,     /* ptr to next buf char */
                    cx_Object   asord,  /* atoms in smiles order seq */
                    cx_Object   bsord ) /* bonds in smiles order seq */
{
   cx_Object    anext, xatom, bnext, bond, bonds;
   char         *p = *pp;
   int          nnew, off, bo;

   /*** Make room as needed. ***/

   if ((p + 20) > (*buf + *pms)) {
      off   = (*pp - *buf);
      *pms *= 2;
      *buf  = (char *) cx_realloc(*buf, *pms);
      p     = *buf + off;
   }

   /*** Add atom label to genes starting at *pp, mark atom visited. ***/

   strcpy(p, cx_sprop(atom, "label"));
   p += strlen(p);

   /*** Mark atom visited and record order, if able. ***/

   cx_set_iprop(atom, "visit", 1);
   if (asord) cx_append(asord, atom);

   /*** Repeat until DFS is complete. ***/

   do {

      /*** Find lowest-numbered unvisited connection. ***/

      anext = NULL;
      bnext = NULL;
      nnew  = 0;
      bonds = cx_stream(atom, CX_OB_BOND);
      while (NULL != (bond = cx_next(bonds))) {
         if (0 == cx_iprop(bond, "visit")) {
            xatom = cx_e_xatom(atom, bond);

            /*** Deal with ring closure here. ***/

            if (0 != cx_iprop(xatom, "visit")) {
              *pnrc += 1;
               bo = cx_iprop(bond, "bond order");
               append_rc(xatom, *pnrc, bo, CX_CHI_NO_DBO);
               append_rc(atom,  *pnrc, 0,  CX_CHI_NO_DBO);
/*
append_rc(&xatom->label, *pnrc, bond->order, revdbo(bond->dbo));
append_rc(&atom->label,  *pnrc, 0,           CX_CHI_NO_DBO    );
*/
               cx_set_iprop(bond, "visit", 2);

            /*** Increment number of new choices. ***/

            } else {
               nnew++;

               /*** Record lowest-numbered connected-but-unvisited atom. ***/
               /* was: if (NULL == anext || xatom->inord < anext->inord) */

               if (NULL == anext || xatom < anext) {
                  bnext = bond;
                  anext = xatom;
               }
            }
         }
      }
      cx_destroy(bonds);

      /*** Generate part if connected-but-unvisited atom was found. ***/

      if (anext) {
         if (1 <  nnew) *p++ = '(';

         bo = cx_iprop(bnext, "bond order");

#ifdef LATERCHIRAL
         if (1 == cx_iprop(bnext, "bond order")) {
            switch (get_bond_dir(bnext, atom)) {
               case CX_CHI_UP:   *p++ = '/';   break;
               case CX_CHI_DOWN: *p++ = '\\';  break;
            }
         }
#endif

         if      (2 == bo) *p++ = '=';
         else if (3 == bo) *p++ = '#';
         cx_set_iprop(bnext, "visit", 1);

         if (bsord) cx_append(bsord, bnext);

         genpart(anext, pnrc, buf, pms, &p, asord, bsord);
         if (1 < nnew) *p++ = ')';
      }

      /*** Done when all connections are visited. ***/

   } while (anext);

   *p  = '\0';
   *pp = p;
}

/*============================================================================
 *  molecule_stringvalue() -- return "sillysmiles" for molecule, or NULL
 *
 *  This function is not public: use cx_stringvalue(mol).
 */

static cx_String molecule_stringvalue(cx_Object mol)
{
   CI_MOL      *ms;
   cx_Object    atom, atoms, bond, bonds;
   char        *buf, *p;
   int          i, mbuf, nrc;
   
   /*** Require valid molecule. ***/

   if (NULL == mol || NULL == (ms = cx_e_base_content(mol))) return NULL;
   
   /*** Return generated smiles if available and molecule is unmodified. ***/

   if (NULL != ms->genes && FALSE == ms->modified) return ms->genes;

   /*** Else, trash cache. ***/

   cx_free(ms->genes);     ms->genes = NULL;
   cx_destroy(ms->asord);  ms->asord = cx_create_sequence(mol);
   cx_destroy(ms->bsord);  ms->bsord = cx_create_sequence(mol);
   ms->modified = FALSE;

   /*** Create initial buffer. ***/

   mbuf = CX_MAX(64, 2 * cx_count(mol, CX_OB_ATOM));
   mbuf = 32;
   buf  = (char *) cx_malloc(mbuf * sizeof(char));
   p    = buf;

   /*** Zap extant atomic labels, generate new ones. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms)))
      cx_set_sprop(atom, "label", atom_label(atom, FALSE));

   /*** Rationalize or invent dbo's. ***/

#ifdef LATERCHIRAL
   set_bond_dirs(mol);
#endif

   /*** Initialize visit flags for first DFS pass. ***/

   cx_reset(atoms);
   while (NULL != (atom = cx_next(atoms)))
      cx_set_iprop(atom, "visit", 0);

   bonds = cx_stream(mol, CX_OB_BOND);
   while (NULL != (bond = cx_next(bonds)))
      cx_set_iprop(bond, "visit", 0);

   /*** Loop through atoms once in input order, DFS-ing. ***/

   nrc = 0;
   cx_reset(atoms);
   for (i = 0; NULL != (atom = cx_next(atoms)); i++) {
      if (0 == cx_iprop(atom, "visit")) {
         if (i) *p++ = '.';
         genpart(atom, &nrc, &buf, &mbuf, &p, ms->asord, ms->bsord);
      }
   }

   /*** Redo if rings found. ***/

   if (0 < nrc) {

      /*** Reset (unvisit) all atoms and bonds except ring closures. ***/

      cx_reset(atoms);
      while (NULL != (atom = cx_next(atoms)))
         cx_set_iprop(atom, "visit", 0);

      cx_reset(bonds);
      while (NULL != (bond = cx_next(bonds)))
         if (2 != cx_iprop(bond, "visit")) cx_set_iprop(bond, "visit", 0);

      /*** Loop through atoms again in input order, DFS-ing. ***/

      p = buf;
      cx_reset(atoms);
      for (i = 0; NULL != (atom = cx_next(atoms)); i++) {
         if (0 == cx_iprop(atom, "visit")) {
            if (i) *p++ = '.';
            genpart(atom, &nrc, &buf, &mbuf, &p, NULL, NULL);
         }
      }
   }

   /*** Return newly allocated string in molecule and return it. ***/

   cx_destroy(atoms);
   cx_destroy(bonds);
   ms->genes = cx_strdup(buf);

#ifdef DEBUG
{
 int i = 0;
 cx_Object atom, atoms = cx_stream(mol, CX_OB_SMIATOMS);
 printf("Atoms in SMILES order:\n");
 while (NULL != (atom = cx_next(atoms))) {
  printf("%4d: %s\n", i++, cx_stringvalue(atom));
 }
 cx_destroy(atoms);
}
#endif

   cx_free(buf);
   return ms->genes;
}

/*============================================================================
 *  molecule_destroy() -- destroy a molecule object
 */

static void molecule_destroy(cx_Object mol)
{
   CI_MOL    *ms;
   cx_Object ob, obs;

   /*** Get internal molecule struct. ***/

   if (NULL == (ms = (CI_MOL *) cx_e_base_content(mol))) return;

   /*** Destroy contents and struct. ***/

   cx_destroy(ms->asord);
   cx_destroy(ms->bsord);
   cx_free(ms->genes);
   cx_free(ms);

   /*** Destroy atoms and bonds separately from other children. ***/

   obs = cx_stream(mol, CX_OB_ANY);
   while (NULL != (ob = cx_next(obs))) {
      if (CX_OB_ATOM == cx_type(ob) || CX_OB_BOND == cx_type(ob)) {
         cx_destroy(cx_e_base_content(ob));
         cx_e_base_destroy(ob);
      }
   }
   cx_destroy(obs);

   /*** Destroy base structure and any children. ***/

   cx_e_base_destroy(mol);
}

/*============================================================================
 *  molecule_count() -- molecule count generator (special for pseudoclasses)
 */

static cx_Integer molecule_count(cx_Object mol, cx_Integer type)
{
   CI_MOL     *ms;
   cx_Integer  count = 0;
   cx_Object   seq   = NULL;

   /*** Look for pseudoclasses, else use base count function. ***/

   if (CX_OB_SMIATOMS == type) {
      if (NULL == (ms = cx_e_base_content(mol))) return 0;
      seq = ms->asord;
   } else if (CX_OB_SMIBONDS == type) {
      if (NULL == (ms = cx_e_base_content(mol))) return 0;
      seq = ms->bsord;
   } else {
      return cx_e_base_count(mol, type);
   }

   /*** Generate fresh internal sequences if needed. ***/

   if (NULL == seq || ms->modified) {
      molecule_stringvalue(mol);
      if      (CX_OB_SMIATOMS == type) seq = ms->asord;
      else if (CX_OB_SMIBONDS == type) seq = ms->bsord;
   }

   /*** Count elements in sequence and return value. ***/

   while (NULL != cx_next(seq))
      count++;

   return count;
}

/*============================================================================
 *  molecule_stream() -- molecule stream generator (special for pseudoclasses)
 */

static cx_Object molecule_stream(cx_Object mol, cx_Integer type)
{
   CI_MOL     *ms;
   cx_Object   ob, seq = NULL;
   cx_Object   copy    = NULL;

   /*** Look for pseudoclasses, else use base stream function. ***/

   if (CX_OB_SMIATOMS == type) {
      if (NULL == (ms = cx_e_base_content(mol))) return NULL;
      seq = ms->asord;
   } else if (CX_OB_SMIBONDS == type) {
      if (NULL == (ms = cx_e_base_content(mol))) return NULL;
      seq = ms->bsord;
   } else {
      return cx_e_base_stream(mol, type);
   }

   /*** Regenerate sequences if needed. ***/

   if (NULL == seq || ms->modified) {
      molecule_stringvalue(mol);
      if      (CX_OB_SMIATOMS == type) seq = ms->asord;
      else if (CX_OB_SMIBONDS == type) seq = ms->bsord;
   }

   /*** Copy sequence and return as stream. ***/

   if (seq) {
      copy = cx_e_create_stream(mol);
      cx_reset(seq);
      while (NULL != (ob = cx_next(seq)))
         cx_e_base_append(copy, ob);
      cx_reset(copy);
      return copy;
   }

   /*** Return unsuccessfully. ***/

   return NULL;
}

/*============================================================================
 *  setatomprops() -- expt'l, creates atomtuple and sets atom properties
 *
 *  Returns atomtuple object.
 */

static cx_Object setatomprops(cx_Object parent, cx_Object mol, cx_String pname,
                              cx_String data,   cx_Object dt)
{
   cx_Object  atuple, atoms, atom;
   cx_String  p, q, shape, list = cx_strdup(data);
   int        dim;
   char       buf[80];   /* to hold unique property name, e.g., "color.1" */
   static int uid = 1;   /* should be on a molecule basis */
 
   /*** Create unique property name. ***/

   if (NULL == pname) return NULL;
   sprintf(buf, "%s.%d", pname, uid++);

   /*** Create atomtuple with given property name. ***/

   if (NULL == (atuple = cx_create_atomtuple(parent, buf))) return NULL;
   cx_set_datatype(atuple, dt);

   /** Extract dimension from shape. ***/

   shape = cx_sprop(dt, "shape");
   dim   = ((isdigit(*shape) && '0' != *shape) ? (*shape - '0') : 1);
   dim   = CX_MAX(dim, 1);

   /*** Assign items in semicolon-delimited list to atoms in stream. ***/

   atoms = cx_stream(mol, CX_OB_ATOM); /* input order */
   cx_reset(atoms);
   for (p = list; (atom = cx_next(atoms)); p = (p ? q + 1 : NULL)) {

      /*** Extract semicolon-delimited items. ***/

      q = cx_strqbrk(p, ";");

      /*** Assign terminated item(s) to atom as property. ***/

      if (q) *q = '\0';
      cx_e_base_setproperty(atom,buf,p);
      if (q) *q = ';';

      /*** Break if last item was really the last one. ***/

      if (NULL == q) break;
   }
   cx_destroy(atoms);
   cx_free(list);

   return atuple;
}

/*============================================================================
 *  setbondprops() -- expt'l, creates bondtuple and sets bond properties
 *
 *  Returns bondtuple object.
 */

static cx_Object setbondprops(cx_Object parent, cx_Object mol, cx_String pname,
                              cx_String data,   cx_Object dt)
{
   cx_Object  btuple, bonds, bond;
   cx_String  p, q, shape, list = cx_strdup(data);
   int        dim;
   char       buf[80];  /* to hold unique property name, e.g., "color.1" */
   static int uid = 1;  /* should be on a molecule basis */
 

   /*** Create unique property name. ***/

   if (NULL == pname) return NULL;
   sprintf(buf, "%s.%d", pname, uid++);

   /*** Create bondtuple with given property name. ***/

   if (NULL == (btuple = cx_create_bondtuple(parent, buf))) return NULL;
   cx_set_datatype(btuple, dt);

   /** Extract dimension from shape. ***/

   shape = cx_sprop(dt, "shape");
   dim   = ((isdigit(*shape) && '0' != *shape) ? (*shape - '0') : 1);
   dim   = CX_MAX(dim, 1);

   /*** Assign items in semicolon-delimited list to bonds in stream. ***/

   bonds = cx_stream(mol, CX_OB_BOND); /* input order */
   cx_reset(bonds);
   for (p = list; NULL != (bond = cx_next(bonds)); p = q + 1) {

      /*** Extract semicolon-delimited items. ***/

      q = cx_strqbrk(p, ";");

      /*** Assign terminated item(s) to bond as property. ***/

      if (q) *q = '\0';
      cx_e_base_setproperty(bond, buf, p);
      if (q) *q = ';';

      /*** Break if last item was really the last one. ***/

      if (NULL == q) break;
   }
   cx_destroy(bonds);
   cx_free(list);

   return btuple;
}

/*============================================================================
 *  molecule_cexin() -- read molecule as CEX tree, save in object
 *
 *  If table is NULL, the default datatype table is used.
 *  If successful, return the molecule; if not, return NULL;
 *
 *  Now (0.33) also interprets partial trees (not containing unquoted EOT)
 *  and calls root datatype parsers as needed.
 */
 
static cx_Object molecule_cexin(cx_String cex, cx_Object tabin)
{
   cx_Object  table, dt, id, di, ob, mol = NULL;
   cx_Integer cxt;
   cx_String  p, q, moltag, insmi, tag, data, lang, shape, pname;
   cx_String  func = "molecule_cexin";
   char       errbuf[80];

   char       cold, *hold;
   cx_Integer rt, skip2id;

   /* Avoid uninitialised data! */
   dt = (cx_Object)0;

   /*** Reject NULL cex strings. ***/
 
   if (NULL == cex) return NULL;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());

   /*** Extract root tag and content, return in error if unable. ***/
 
   if (NULL == (p = cx_e_cex_xtagdata(cex, &cxt, &moltag, &insmi))) {
      strncpy(errbuf, cex, 50);
      errbuf[50] = '\0';
      cx_error_save("Can't parse tree starting:", CX_ERR_ERROR, func);
      cx_error_save(errbuf,                       CX_ERR_ERROR, func);
 
   /*** Full datatrees must start with an identifier. ***/
 
   } else if (CX_CXT_IDENTIFIER != cxt && NULL != cx_strqbrk(cex, "|")) {
      cx_error_save("Expected root identifier:", CX_ERR_ERROR, func);
      cx_error_save(moltag,                      CX_ERR_ERROR, func);
 
   /*** Extract datatype; return in error if unable. ***/
 
   } else if (NULL == (dt = cx_tag2datatype(table, moltag))) {
      cx_error_save("Can't get datatype for:", CX_ERR_ERROR, func);
      cx_error_save(moltag,                    CX_ERR_ERROR, func);
 
   /*** Check for XSMILES language; return in error if not. ***/
 
   } else if (NULL == (lang = cx_sprop(dt, "language"))) {
      cx_error_save("language not defined for:",  CX_ERR_ERROR, func);
      cx_error_save(moltag,                       CX_ERR_ERROR, func);
 
   } else if (0 != cx_strcmp("XSMILES", lang)) {
      sprintf(errbuf, "root %s language %s, expected XSMILES\n", moltag, lang);
      cx_error_save(errbuf, CX_ERR_ERROR, func);

   /*** Parse XSMILES into molecule object; return in error if fails. ***/
 
   } else {
      mol = cx_smilin(NULL, insmi); /* "forward" reference to public entry */
   }

   /*** Clean up and exit if can't interpret molecule for any reason. ***/

   if (NULL == mol) { cx_free(moltag); cx_free(insmi); return NULL; }

   /*** We're done with tag/data in any case. ***/

   cx_free(moltag);
   cx_free(insmi);

   /*** Attach datatype to molecule. ***/

   cx_set_datatype(mol, dt);

   /* DW seems like we need to do this somewhere to set up order? */
   /* cx_stringvalue(mol); */
 
   /*** Interpret remaining data. ***/

   skip2id = FALSE;
   id = di = mol;
   for ( ; NULL != (q = cx_e_cex_xtagdata(p, &cxt, &tag, &data)); p = q) {
   
      /*** Skip to next id if handled elsewhere. ***/

      if (skip2id) {
         if (CX_CXT_IDENTIFIER == cxt) skip2id = FALSE;
         else                          continue;
      }
   
      /*** Get datatype from tag, language & shape from datatype. ***/

      if (NULL == (dt = cx_tag2datatype(table, tag))) {
         cx_error_save("Can't get datatype for:",    CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (lang = cx_sprop(dt, "language"))) {
         cx_error_save("language not defined for:",  CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (pname = cx_sprop(dt, "property name"))) {
         cx_error_save("no property name for:",      CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (shape = cx_sprop(dt, "shape"))) {
         cx_error_save("shape not defined for:",     CX_ERR_NOTE, func);
         cx_error_save(tag,                          CX_ERR_NOTE, func);
         cx_error_save("... using shape 1 (scalar)", CX_ERR_NOTE, func);
         shape = "1";
      }

      /*** Look for root type of this tag (or CX_OB_INVALID). ***/

      rt = cx_tag2roottype(tag);

      /*** Attach identifiers to molecule. ***/

      if (CX_CXT_IDENTIFIER == cxt) {

         /*** If could-be root identifier, use its parser. ***/

         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, "$|");
            if (hold) {
                cold = *hold;
                *hold = '\0';
                cx_set_parent(cx_e_cexin(p, rt, table), mol);
                *hold = cold;
            } else
                cx_set_parent(cx_e_cexin(p, rt, table), mol);
            skip2id = TRUE;

         /*** Molecular subtrees. ***/

         } else if (CX_SHAPE_ATOM == shape[1]) {
            id = di = setatomprops(mol, mol, pname, data, dt);
         } else if (CX_SHAPE_BOND == shape[1]) {
            id = di = setbondprops(mol, mol, pname, data, dt);
         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("mol: atom/bond-tuples only" , CX_ERR_ERROR, func);
            cx_error_save(shape,                         CX_ERR_ERROR, func);
            cx_error_save(tag,                           CX_ERR_ERROR, func);
         } else {
            id = di = ob = cx_create_string(mol, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach dataitems to atoms, bonds, or last identifier. ***/

      } else if (CX_CXT_DATAITEM == cxt) {

         /*** If could-be root datatype, use its parser. ***/

         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, ">");
            if (hold) {
                hold++;
                cold = *hold;
                *hold = '\0';
                cx_set_parent(cx_e_cexin(p, rt, table), mol);
                *hold = cold;
            } else
                cx_set_parent(cx_e_cexin(p, rt, table), mol);

         /*** Molecular dataitems. ***/

         } else if (CX_SHAPE_ATOM == shape[1]) {
            di = setatomprops(id, mol, pname, data, dt);
         } else if (CX_SHAPE_BOND == shape[1]) {
            di = setbondprops(id, mol, pname, data, dt);
         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("mol: atom/bond-tuples only" , CX_ERR_ERROR, func);
            cx_error_save(shape,                         CX_ERR_ERROR, func);
            cx_error_save(tag,                           CX_ERR_ERROR, func);
         } else {
            di = ob = cx_create_string(id, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach properties to last dataitem. ***/

      } else if (CX_CXT_PROPERTY == cxt) {
         if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("tuple-based props not ok" , CX_ERR_ERROR, func);
            cx_error_save(shape,                       CX_ERR_ERROR, func);
            cx_error_save(tag,                         CX_ERR_ERROR, func);
         } else {
            cx_set_sprop(di, pname, data);
         }

      /*** Humm. ***/

      } else {
         cx_error_save("Unexpected context (ouch!)" , CX_ERR_ERROR, func);
         cx_error_save(tag,                           CX_ERR_ERROR, func);
         cx_error_save(data,                          CX_ERR_ERROR, func);
      }

      /*** Free this tag/data pair. ***/

      cx_free(tag);
      cx_free(data);
   }
 
   /*** return molecule. ***/

   return mol;
}

/*============================================================================
 *  molecule_send() -- read molecule as CEX tree, save in object
 *
 *  Write given molecule, children, properties, and datatypes to output in
 *  a def-before-ref fashion.  cx_dt_table_setmarks(table,FALSE) should be
 *  called before this function is called on a particular stream.
 *
 *  If table is NULL, the default datatype table is used.
 *
 *  Uses local directly recursive function sendmol().
 *
 *  Returns TRUE iff successful.
 */

static int has_visible_children(cx_Object ob)
{
   cx_Object kid, kids = cx_stream(ob, CX_OB_ANY);
   while (NULL != (kid = cx_next(kids)))
      if (CX_OB_PROPERTY != cx_type(kid) && cx_datatype(kid)) return TRUE;
   return FALSE;
}

 
static cx_Integer sendmol(cx_Object ob, cx_Object table, cx_String eod,
                          cx_Object outs, int pass, int lev)
{
   cx_Object dt, kid, kids, prop, props;
   cx_String pname;

   /*** Successful no-op if ob is NULL or has no associated datatype. ***/
 
   if (NULL == ob || NULL == (dt = cx_datatype(ob))) return TRUE;

   /*** Unsuccessful no-op if table, isn't. ***/
 
   if (CX_OB_DATATYPE_TABLE != cx_type(table)) return FALSE;
 
   /*** Send datatype on pass 0, print dataitem on pass 1. ***/

   if (0 == pass) {
      cx_append(outs, dt);

   } else if (0 == lev) {
      cx_e_ioputc('$', outs);
      cx_e_ioputs(cx_sprop(dt,"tag"), outs);
      cx_e_ioputc('<', outs);
      cx_e_ioputs(cx_stringvalue(ob), outs);
      cx_e_ioputc('>', outs);
      cx_e_ioputs(eod, outs);


   /*** EXPERIMENTAL, NEW IN 033, DEDUCE RECURSION ON OUTPUT ***/

   } else if (has_visible_children(ob)) {
      cx_e_ioputc('$', outs);
      cx_e_ioputs(cx_sprop(dt,"tag"), outs);
      cx_e_ioputc('<', outs);
      cx_e_ioputs(cx_stringvalue(ob), outs);
      cx_e_ioputc('>', outs);
      cx_e_ioputs(eod, outs);

   } else {
      cx_e_ioputs(cx_sprop(dt,"tag"), outs);
      cx_e_ioputc('<', outs);
      cx_e_ioputs(cx_stringvalue(ob), outs);
      cx_e_ioputc('>', outs);
      cx_e_ioputs(eod, outs);
   }

   /*** Send property datatype on pass 0, print property on pass 1. ***/

   props = cx_stream(ob, CX_OB_PROPERTY);
   while (NULL != (prop = cx_next(props))) {
      pname = cx_prop_name(prop);
      if (NULL != (dt = cx_pname2datatype(table, pname))) {
         if (0 == pass) {
            cx_append(outs, dt);
         } else {
            cx_e_ioputc('/', outs);
            cx_e_ioputs(cx_sprop(dt, "tag"), outs);
            cx_e_ioputc('<', outs);
            cx_e_ioputs(cx_stringvalue(prop), outs);
            cx_e_ioputc('>', outs);
            cx_e_ioputs(eod, outs);
         }
      }
   }
   cx_destroy(props);

   /*** Print children recursively. ***/

   kids = cx_stream(ob, CX_OB_ANY);
   while (NULL != (kid = cx_next(kids)))
      if (CX_OB_PROPERTY != cx_type(kid))
         sendmol(kid, table, eod, outs, pass, lev + 1);
   cx_destroy(kids);

   /*** Return successfully. ***/

   return TRUE;
} 
 
static cx_Integer molecule_send(cx_Object ob, cx_Object tabin, cx_Object outs)
{
   cx_Integer ok;
   cx_String  eod, eot;
   cx_Object  table;

   /*** Process trivial or invalid requests. ***/

   if (NULL == ob  ) return TRUE;
   if (NULL == outs) return FALSE;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());

   /*** Set end-of-data and end-of-tree delimiters from listing format. ***/
 
   switch (cx_cex_listfmt()) {
      default:           /* Avoid compiler warnings! */
      case CX_FMT_DUMP:  eod = "";    eot = "\n";  break;
      case CX_FMT_RAW:   eod = "";    eot = "";    break;
      case CX_FMT_LIST:  eod = "\n";  eot = "\n";  break;
   }

   /*** Send datatypes, send datatree, return success. ***/

   ok = sendmol(ob, table, eod, outs, 0, 0);
   if (ok) ok = sendmol(ob, table, eod, outs, 1, 0);
   cx_e_ioputc('|', outs);
   cx_e_ioputs(eot, outs);
   return ok;
}

/*** Local auto-initialization function follows. ***/

/*============================================================================
 *  molecule_init() -- initialize molecule-specific functions
 */
 
void molecule_init(void)
{
   /*** Define molecule and molecule-based types. ***/
 
   cx_e_set_typename(CX_OB_MOLECULE, "Molecule"             );
   cx_e_set_typename(CX_OB_ATOM,     "Atom"                 );
   cx_e_set_typename(CX_OB_BOND,     "Bond"                 );
   cx_e_set_typename(CX_OB_SMIATOMS, "Atoms in SMILES order");
   cx_e_set_typename(CX_OB_SMIBONDS, "Bonds in SMILES order");

   /*** Register root tag. ***/

   cx_e_register_root_type("MOL", CX_OB_MOLECULE);
 
   /*** Atom-specific functions. ***/

   cx_set_method(CX_OB_ATOM, "destroy",     atom_destroy    );
   cx_set_method(CX_OB_ATOM, "stream",      atom_stream     );
   cx_set_method(CX_OB_ATOM, "count",       atom_count      );
   cx_set_method(CX_OB_ATOM, "setproperty", atom_setproperty);
   cx_set_method(CX_OB_ATOM, "stringvalue", atom_stringvalue);

   /*** Can't cexin or send send atoms. ***/

   cx_set_method(CX_OB_ATOM, "cexin",   NULL);
   cx_set_method(CX_OB_ATOM, "send",    NULL);


   /*** Bond-specific functions. ***/

   cx_set_method(CX_OB_BOND, "destroy",     bond_destroy    );
   cx_set_method(CX_OB_BOND, "setproperty", bond_setproperty);
   cx_set_method(CX_OB_BOND, "stream",      bond_stream     );
   cx_set_method(CX_OB_BOND, "count",       bond_count      );


   /*** Can't create, stringvalue, cexin, or send bonds. ***/

   cx_set_method(CX_OB_BOND, "stringvalue", NULL);
   cx_set_method(CX_OB_BOND, "cexin",       NULL);
   cx_set_method(CX_OB_BOND, "send",        NULL);


   /*** Molecule-specific functions. ***/

   cx_set_method(CX_OB_MOLECULE, "cexin",       molecule_cexin       );
   cx_set_method(CX_OB_MOLECULE, "send",        molecule_send        );
   cx_set_method(CX_OB_MOLECULE, "setproperty", cx_e_base_setproperty);
   cx_set_method(CX_OB_MOLECULE, "stringvalue", molecule_stringvalue );
   cx_set_method(CX_OB_MOLECULE, "destroy",     molecule_destroy     );
   cx_set_method(CX_OB_MOLECULE, "count",       molecule_count       );
   cx_set_method(CX_OB_MOLECULE, "stream",      molecule_stream      );

   /*** Mark molecule methods initialized and return successfully. ***/
       
   initialized = TRUE;

#ifdef OKWAY
   /*** Create a couple datatypes in the default table. ***/

   cx_create_datatype(NULL, "MOL", "molecule", "Molecule", "1", "XSMILES",
                      "Molecule represented in XSMILES (Exchange SMILES)");

   cx_create_datatype(NULL, "XYZ", "coordinates", "Atomic coordinates", "1",
                      "REAL", "Atomic (X,Y,Z) coordinates in Angstroms");
#endif
}
 
/*============================================================================
 *  cx_molecule_pkg() -- public molecule package initialization
 */

cx_Integer cx_e_molecule_pkg(void)
{
   /*** Initialize molecule methods if needed. ***/

   if (!initialized) molecule_init();
   return TRUE;
}

/*** Non-polymorphic external functions follow. ***/

/*============================================================================
 *  cx_e_create_atom() -- return a newly allocated & initialized atom, or NULL
 */

cx_Object cx_e_create_atom(cx_Object mol)
{
   cx_Object atom;

   /*** Validate parent and mark molecule modified. ***/

   if (NOT_MOLECULE(mol)) return NULL;
   mark_molecule_modified(mol, TRUE);

   /*** Make new atom object. ***/

   atom = (cx_Object) cx_e_base_create(mol, CX_OB_ATOM);
   if (NULL == atom) return NULL;

   /*** Create stream of bonds, store as (only) content. ***/

   cx_e_base_set_content(atom, (void *) cx_e_create_stream(atom));

   /*** Return atom as (cx_Object) ***/

   return atom;
}

/*============================================================================
 *  alloc_bond() -- return a newly allocated & initialized bond, or NULL
 */
 
static cx_Object alloc_bond(cx_Object mol)
{
   cx_Object bond;
 
   /*** Mark parent modified. ***/
 
   mark_molecule_modified(mol, TRUE);
 
   /*** Make new bond object. ***/
 
   bond = (cx_Object) cx_e_base_create(mol, CX_OB_BOND);
   if (NULL == bond) return NULL;
 
   /*** Store stream of atoms as bond's (only) content. ***/
 
   cx_e_base_set_content(bond, (void *) cx_e_create_stream(bond));
 
   /*** Return bond as (cx_Object) ***/
 
   return bond;
}
 
/*============================================================================
 *  cx_e_create_bond() -- return a newly allocated & initialized bond, or NULL
 */

cx_Object cx_e_create_bond(cx_Object a1, cx_Object a2, cx_Integer bo)
{
   cx_Object mol, bond, atoms;

   /*** Validate end atoms, bond order. ***/

   if (NOT_ATOM(a1) || NOT_ATOM(a2) || a1 == a2) return NULL;
   if (NULL == (mol = cx_parent(a1)) || mol != cx_parent(a2)) return NULL;
   if (1 > bo || 3 < bo) return NULL;

   /*** Only make bond if no bond between given atoms already exists. ***/

   if (NULL == (bond = cx_bond(a1, a2))) {

      /*** Create bond. ***/

      if (NULL == (bond = alloc_bond(mol))) return NULL;

      /*** Add atoms to bond's atom stream. ***/

      atoms = cx_e_base_content(bond);
      cx_e_base_append(atoms, a1);
      cx_e_base_append(atoms, a2);

      /*** Add bond to atoms' bond streams. ***/

      cx_e_base_append(cx_e_base_content(a1), bond);
      cx_e_base_append(cx_e_base_content(a2), bond);
   }

   /*** Add (or reset) bond order as an integer property. ***/

   cx_set_iprop(bond, "bond order", bo);

   /*** Return new bond object. ***/

   return bond;
}

/*============================================================================
 *  cx_e_bond() -- return bond between two atoms or NULL
 */

cx_Object cx_e_bond(cx_Object a1, cx_Object a2)
{
   cx_Object bond1, bond2, bond1s, bond2s;

   /*** Validate end atoms. ***/

   if (NOT_ATOM(a1) || NOT_ATOM(a2)) return NULL;

   /*** Get bonds on atoms. ***/

   if (NULL == (bond1s = cx_e_base_content(a1))) return NULL;
   if (NULL == (bond2s = cx_e_base_content(a2))) return NULL;

   /* Avoid uninitialized data! */
   bond2 = (cx_Object)0;

   /*** Loop over both atoms' bonds, looking for common bond. ***/

   cx_reset(bond1s);
   while (NULL != (bond1 = cx_next(bond1s))) {
      cx_reset(bond2s);
      while (NULL != (bond2 = cx_next(bond2s)))
         if (bond1 == bond2) break;
      if (bond1 == bond2) break;
   }

   /*** Return common bond if found. ***/

   return ((bond1 && (bond1 == bond2)) ? bond1 : NULL);
}

/*============================================================================
 *  cx_e_xatom() -- return atom across bond from another
 */
 
cx_Object cx_e_xatom(cx_Object fromat, cx_Object bond)
{
   cx_Object atoms, xatom;
   atoms = cx_stream(bond, CX_OB_ATOM);
   if      (fromat == (xatom = cx_next(atoms))) xatom = cx_next(atoms);
   else if (fromat != cx_next(atoms)          ) xatom = NULL;
   cx_destroy(atoms);
   return xatom;
}

/*============================================================================
 *  normh() -- return "normal" number of implicit hydrogens -- local version
 *
 *  This returns the unmet valence based on lowest normal valence for
 *  NEUTRAL atom which is consistent with explicit bonds, else 0.
 *
 *  H(1), B(3), C(4), N(3,5), O(2), F(1), P(3,5), S(2,4,6), Cl(1), Br(1), I(1)
 *
 *  Note that this is a much simpler and "righter" function than that used
 *  externally, since here we're only dealing with truly implicit hcounts,
 *  i.e., anything weird will be already specified (like normal SMILES rules).
 */

static int normh(cx_Object atom)
{
   cx_Object  bond, bonds;
   int        tbo, val;

   /*** Return 0 for NULL or invalid atom. ***/

   if (NOT_ATOM(atom)) return 0;
   if (NULL == (bonds = cx_e_base_content(atom))) return 0;
   cx_reset(bonds);

   /*** Loop over atom's bonds, calculating total explicit bond order. ***/

   tbo = 0;
   while (NULL != (bond = cx_next(bonds)))
      tbo += cx_iprop(bond, "bond order");

   /*** 11 elements have well-defined valences. ***/

   switch (cx_iprop(atom, "atomic number")) {
      case  1:  val = 1;                                  break;  /* H  */
      case  5:  val = 3;                                  break;  /* B  */
      case  6:  val = 4;                                  break;  /* C  */
      case  7:  val = (3 < tbo ? 5 : 3);                  break;  /* N  */
      case  8:  val = 2;                                  break;  /* O  */
      case  9:  val = 1;                                  break;  /* F  */
      case 15:  val = (3 < tbo ? 5 : 3);                  break;  /* P  */
      case 16:  val = (4 < tbo ? 6 : (2 < tbo ? 4 : 2));  break;  /* S  */
      case 17:  val = 1;                                  break;  /* Cl */
      case 35:  val = 1;                                  break;  /* Br */
      case 53:  val = 1;                                  break;  /* I  */
      default:  val = 0;
   }

   /*** Normal hcount is valence - explicit bonds, but not negative. ***/

   return (val > tbo ? (val - tbo) : 0);
}

/*============================================================================
 *  setimphs() -- set implicit H-counts as needed
 */

static void setimphs(cx_Object mol)
{
   char      buf[8];
   cx_Object atom, atoms;

   /*** Noop for NULL or invalid molecule. ***/

   if (NOT_MOLECULE(mol)) return;

   /*** Loop over atoms in molecule with implicit hcounts. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms))) {
      if (NULL == cx_e_property(atom, "implicit hcount")) {
         sprintf(buf, "%d", normh(atom));
         cx_set_sprop(atom, "implicit hcount", buf);
      }
   }
   cx_destroy(atoms);
}

/*============================================================================
 *  smiles_error() -- generate helpful smiles error message
 */

static void smiles_error(char *msg, char *smi, char *oops, char *func)
{
   char *buf;
   int   lens;

   /*** Write SMILES after msg, with oops position indicated. ***/

   buf  = (char *) cx_malloc((strlen(msg) + strlen(smi) + 6) * sizeof(char));
   lens = oops - smi;
   sprintf(buf, "%s: %.*s<%c>%s", msg, lens, smi, *oops, oops + 1);
   cx_error_save(buf, CX_ERR_ERROR, func);
   cx_free(buf);
}
 
/*============================================================================
 *  cx_e_create_molecule() -- return newly alloced & initialized mol, or NULL
 */

cx_Object cx_e_create_molecule(cx_Object parent)
{
   CI_MOL    *ms;
   cx_Object  mol;

   /*** Initialize molecule methods if needed. ***/

   if (!initialized) molecule_init();

   /*** Create internal molecule struct and and base object. ***/

   if (NULL == (ms = (CI_MOL *) cx_malloc(sizeof(CI_MOL)))) return NULL;
   if (NULL == (mol = cx_e_base_create(parent, CX_OB_MOLECULE))) return NULL;

   /*** Initialize internal molecule struct. ***/

   ms->modified  = TRUE;
   ms->asord     = NULL;
   ms->bsord     = NULL;
   ms->genes     = NULL;

   /*** Set molecule object content to struct and return object. ***/

   cx_e_base_set_content(mol, ms);

   return mol;
}

/*============================================================================
 *  cx_e_smilin() -- simple SMILES interpreter
 *
 *  1%12%123^123  1, 12, 12, 3, and 123
 *
 *  [x] check for more than one bond between pairs of atoms
 *  [ ] ringbdir
 */

cx_Object cx_e_smilin(cx_Object parent, cx_String smi)
{
   cx_Object    mol;                    /* working molecule */
   cx_Object    atom;                   /* temporary atom */
   cx_Object    fromat[MX_NESTING];     /* "from" atom indicies */
   cx_Object    ringat[MX_RINGS];       /* ring closure atoms */
   short        ringbo[MX_RINGS];       /* ring bond orders */
#ifdef LATERCHIRAL
   short        ringbdir[MX_RINGS];     /* ring bond direction */
   int          bdir   = CX_CHI_NO_DBO; /* bond direction, undefined */
#endif
   int          quoted = FALSE;         /* quoted state */
   int          lev    = 0;             /* nesting level */
   int          bo     = 0;             /* bond order, undefined */
   int          mass   = 0;             /* atomic mass, undefined */
   int          charge = 0;             /* charge */
   int          imph   = -1;            /* implicit hcount */
   int          ir;                     /* ring index */
   int          atnum = -1;             /* temporary atom number */
   int          i;                      /* temporary index */
   char        *p, *pp;                 /* ptr to current & lookahead char */
   char         errbuf[80];             /* error message buffer */

   /* Avoid unitialized data! */
   atom = (cx_Object)0;

   /*** Initialize ring arrays. ***/

   for (i = 0; i < MX_RINGS; i++) {
#ifdef LATERCHIRAL
      ringbdir[i] = CX_CHI_NO_DBO;
#endif
      ringat[i]   = NULL;
      ringbo[i]   = 0;
   }

   /*** Initialize initial from[] element, e.g., (OC)C == COC. ***/

   fromat[0] = NULL;

   /*** Allocate molecule (auto-inits methods) and record input SMILES. ***/

   mol = cx_e_create_molecule(parent);
   cx_set_sprop(mol, "input smiles", smi);

   /*** Loop over input. ***/

   for (p = smi; *p; p++) {
      pp = p + 1;
      switch (*p) {

      /*** Deal with branching. ***/

      case '(': fromat[lev + 1] = fromat[lev]; lev++; break;    /* push    */
      case ')':                                lev--; break;    /* pop     */

      /*** Deal with quoting. ***/

      case '[':
         if (quoted) {
            smiles_error("missing ] bracket", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;

         } else {
            quoted = TRUE;

            /*** Look for leading atomic mass. ***/

            if (isdigit(*pp)) {
               mass = *pp - '0';
               for (p++; isdigit(*(p+1)); p++)
                  mass = 10 * mass + (*(p+1) - '0');
            }
         }
         break;

      case ']':
         if (!quoted) {
            smiles_error("missing [ bracket", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;
         } else {
            quoted = FALSE;
         }
         break;

      /*** Deal with disconnections. ***/

      case '.': fromat[lev] = NULL;               break;    /* disco */

      /*** Record bond orders. ***/

#ifdef LATERCHIRAL
      case '/':  bo = 1; bdir = CX_CHI_UP;        break;    /* up */
      case '\\': bo = 1; bdir = CX_CHI_DOWN;      break;    /* down */
#endif

      case '=': bo = 2;                           break;    /* double */
      case '#': bo = 3;                           break;    /* triple */
      case '-': if (!quoted) { bo = 1; break; }             /* single */
             /* else - is quoted, fall through to negative charge */

      /*** Deal with charges. ***/

      case '+':
         if (!quoted) {
            smiles_error("charge not in []'s", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;
#ifdef NOTANERROR
         } else if (NULL == fromat[lev]) {
            smiles_error("charge precedes atomic symbol", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;
#endif

         /*** Assign + or - charge to current "from" atom. ***/

         } else {
/* atom = fromat[lev]; */
            charge = ('+' == *p ? 1 : -1);

            /*** Allow integral charge value, i.e., [+|-]ddd... ***/

            if (isdigit(*pp)) {
               charge *= *pp - '0';
               for (p++; isdigit(*(p+1)); p++)
                  charge = 10 * charge + (*(p+1) - '0');

            /*** Allow +[+..] and -[-..] specification, e.g. [Fe+++]. ***/

            } else if ('+' == *pp) {
               for ( ; '+' == *(p+1); p++)
                  charge += 1;

            } else if ('-' == *pp) {
               for ( ; '-' == *(p+1); p++)
                  charge -= 1;
            }

            /*** Set charge property only if specified (possibly to 0). ***/

            /* if (charge) cx_set_iprop(atom, "charge", charge); */
            cx_set_iprop(atom, "charge", charge);
         }
         break;

      /*** Deal with ring closures. ***/

      case '%': case '^':
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':

         /*** Extract ring index ir as digit n, %nn, ^nnn, or err-out. ***/

         if ('%' == *p) {
            if (isdigit(*(p+1)) && isdigit(*(p+2))) {
               ir  = 10 * (*(p+1) - '0') + (*(p+2) - '0');
               p  += 2;
            } else {
               smiles_error("expect 2 digits after `%'", smi, p, "cx_smilin");
               cx_destroy(mol);
               return NULL;
            }
         } else if ('^' == *p) {
            if (isdigit(*(p+1)) && isdigit(*(p+2)) && isdigit(*(p+3))) {
               ir  = 100 * (*(p+1)-'0') + 10 * (*(p+2)-'0') + (*(p+3)-'0');
               p  += 3;
            } else {
               smiles_error("expect 3 digits after `^'", smi, p, "cx_smilin");
               cx_destroy(mol);
               return NULL;
            }
         } else { /* digit */
            ir  = *p - '0';
	 }

         /*** If ring closure ir is unmatched, open it. ***/

         if (NULL == ringat[ir]) {
            ringat[ir]   = fromat[lev];
            ringbo[ir]   = bo;
#ifdef LATERCHIRAL
            ringbdir[ir] = bdir;
#endif

         /*** Conflicting ring closure bond orders are erroneous. ***/

         } else if ((bo && ringbo[ir]) && (bo != ringbo[ir])) {
            sprintf(errbuf, "bond order conflict: %d vs %d", ringbo[ir], bo);
            cx_error_save(errbuf, CX_ERR_ERROR, "cx_smilin");
            smiles_error("conflicting closure", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;

         /*** Non-opposite ring closure directions are erroneous. ***/

#ifdef LATERCHIRAL
         } else if (CX_CHI_NO_DBO != bdir && CX_CHI_NO_DBO != ringbdir[ir] &&
                    bdir != revdbo(ringbdir[ir])) {
            sprintf(errbuf, "ring bond direction conflict: %d vs %d",
                    bdir, ringbdir[ir]);
            cx_error_save(errbuf, CX_ERR_ERROR, "cx_smilin");
            smiles_error("ring bond direction conflict", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;
#endif

         /*** Don't allow more than one bond between atoms. ***/

         } else if (NULL != cx_bond(fromat[lev], ringat[ir])) {
            smiles_error("atoms already bonded", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;

         /*** Else create ring closure bond and close ir. ***/

         } else {
            bo   = (ringbo[ir] ? ringbo[ir] : (bo ? bo : 1));
            cx_create_bond(fromat[lev], ringat[ir], bo);

            /*** Set ring closure bond direction. ***/

#ifdef LATERCHIRAL
            if (CX_CHI_NO_DBO != bdir || CX_CHI_NO_DBO != ringbdir[ir]) {
               bdir = (CX_CHI_NO_DBO != bdir ? bdir : revdbo(ringbdir[ir]));
               set_bond_dir(mol->bonds[i], mol->atoms[ifrom[lev]], bdir);
            }
#endif

            /*** Tidy up. ***/

            ringat[ir]   = NULL;
            ringbo[ir]   = 0;
#ifdef LATERCHIRAL
            ringbdir[ir] = CX_CHI_NO_DBO;
#endif
         }
         bo   = 0;
#ifdef LATERCHIRAL
         bdir = CX_CHI_NO_DBO;
#endif
         break;

      /*** Recognize atomic symbols. ***/

      case '*': case 'A': case 'B': case 'C': case 'D':
      case 'E': case 'F': case 'G': case 'H': case 'I':
      case 'K': case 'L': case 'M': case 'N': case 'O':
      case 'P': case 'R': case 'S': case 'T': case 'U':
      case 'V': case 'W': case 'X': case 'Y': case 'Z':

         /*** Start with invalid number. ***/

         atnum = -1;

         /*** If next character is lower case, try 2-char symbol. ***/

         if (islower(*pp)) atnum = sym2num(2, p);

         /*** If 2-char symbol, advance pointer, else try 1-char symbol. ***/

         if (-1 < atnum)   p++;
         else              atnum = sym2num(1, p);

         /*** Reject if atomic symbol is unknown. ***/

         if (0 > atnum) {
            smiles_error("bad atomic symbol", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;
         }

         /*** Reject if atomic symbol needs quoting. ***/

         if (!quoted && needquote(atnum)) {
            smiles_error("symbol needs []'s", smi, p, "cx_smilin");
            cx_destroy(mol);
            return NULL;
         }

         /*** Add new atom to molecule. ***/

         atom = cx_e_create_atom(mol);
         cx_set_iprop(atom, "atomic number", atnum             );
         cx_set_sprop(atom, "atomic symbol", num2sym(&i, atnum));

         /*** Bond to "from" atom but never to self. ***/

         if (NULL != fromat[lev] && atom != fromat[lev]) {
               cx_create_bond(atom, fromat[lev], (bo ? bo : 1));
#ifdef LATERCHIRAL
            if (CX_CHI_NO_DBO != bdir)
               set_bond_dir(mol->bonds[i], mol->atoms[ifrom[lev]], bdir);
#endif
         }

         /*** This atom becomes the next "from" atom. ***/

         fromat[lev] = atom;

         /*** If not quoted flag hydrogen count as "implicit". ***/

         if (!quoted) imph = -1;
         /* if (!quoted) atom->imph = -1; */

         /*** (DON'T) Set atomic charge property if specified. ***/

         /* if (0 && charge) cx_set_iprop(atom, "charge", charge); */

         /*** Set atomic mass property if specified. ***/

         if (mass) cx_set_iprop(atom, "mass", mass);

         /*** Deal with chiral specifications: @, @@, @1, @2. ***/
#ifdef LATERCHIRAL

         if (quoted && '@' == *(p+1)) {
            p++;
            if ('@' == *(p+1) || '2' == *(p+1)) {
               atom->chiral = CX_CHI_THCW;
               p++;
            } else {
               atom->chiral = CX_CHI_THCCW;
               if ('1' == *(p+1)) p++;
            }
         }
#endif

         /*** Deal with explicit hydrogen counts, e.g. [NH2] or [N]. ***/

         if (quoted && NULL != atom) {

            /*** Absence of Hspec alone implies hydrogen count of zero. ***/

            if ('H' != *(p+1)) {
               imph = 0;

            /*** Else H (alone) implies hydrogen count of one. ***/

            } else {
               imph = 1;
               p++;

               /*** Digit following quoted H is implicit hydrogen count. ***/

               if (isdigit(*(p+1))) {
                  imph = *(p+1) - '0';
                  for (p++; isdigit(*(p+1)); p++)
                     imph = 10 * imph + (*(p+1) - '0');
               }
            }

            /** Set "implicit hcount" property, possibly to zero. ***/

            cx_set_iprop(atom, "implicit hcount", imph);
         }

         /*** Reset default attributes to "undefined". ***/

#ifdef LATERCHIRAL
         bdir   = CX_CHI_NO_DBO;
#endif
         bo     = 0;
         charge = 0;
         mass   = 0;
         imph   = -1;
         break;

      /*** Extract text following SMILES as "molname". ***/

      case ' ':
         if (*(p+1)) cx_set_sprop(mol, "molname", p + 1);
         p = p + strlen(p);
         break;

      /*** Reject on encountering any other character. ***/

      default:
         smiles_error("invalid character", smi, p, "cx_smilin");
         cx_destroy(mol);
         return NULL;
      }
   }

   /*** Detect mismatch errors. ***/

   if (0 != lev) {
      cx_error_save("mismatched parens", CX_ERR_ERROR, "cx_smilin");
      cx_destroy(mol);
      return NULL;
   }
   
   if (quoted) {
      cx_error_save( "missing closing `]'", CX_ERR_ERROR, "cx_smilin");
      cx_destroy(mol);
      return NULL;
   }

   for (i = 0; i < MX_RINGS; i++)
      if (NULL != ringat[i]) {
         sprintf(errbuf, "unmatched ring closure: %d", i);
         cx_error_save(errbuf, CX_ERR_ERROR, "cx_smilin");
         cx_destroy(mol);
         return NULL;
      }

   /*** Set H-counts. ***/

   setimphs(mol);

   /*** Set double bond orientations from bond directions. ***/

#ifdef LATERCHIRAL
   set_bond_dbos(mol);
#endif

   /*** Return molecule as (cx_Object). ***/

   return (cx_Object) mol;
}
