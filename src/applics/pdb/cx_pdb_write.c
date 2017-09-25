/*****************************************************************************
*  cx_pdb_write.c -- CX support for writing PDB format
*
*  This file provides functions to write PDB's using CX utilities:
*
*     cx_pdb_write() ........... write pdb to stream
*     cx_pdb_set_atomnames() ... invent PDB-style atomnames
*
*----------------------------------------------------------------------------
*
*  cx_Integer cx_pdb_write(FILE *fp,
*                          cx_Object molecule,
*                          cx_String name,
*                          cx_String remark,
*                          cx_String alabs_prop,
*                          cx_String resid_prop,
*                          cx_String chain_prop,
*                          cx_String resno_prop,
*                          cx_String coord_prop,
*                          cx_String occup_prop,
*                          cx_String bvalu_prop,
*                          cx_String acolor_prop);
*                          cx_String arad_prop);
*
*  which writes a PDB entry to given stream for the given molecule.
*
*  Strings provided for `name' and `remark' are output if not NULL.
*
*  Atom labels are taken from the atom property named by the `alabs_prop'
*  argument; simple atomic symbols are used for atoms with missing names.
*  If the `alabs_prop' argument is NULL, PDB-style atomic labels are invented.
*
*  Residue labels are taken from the atom property named by the `resid_prop'
*  argument; if NULL or missing, the string "RES" is used.
*
*  Chain designators are taken from the atom property named by the `chain_prop'
*  argument; if NULL or missing, chain designators are not output.
*
*  Residue numbers are taken from the atom property named by the `resno_prop'
*  argument; if this argument is NULL and any bonds exist, the disconnected
*  component number is used, otherwise this field is left blank.
*
*  Coordinates are obtained from the atom property named by the `coord_prop'
*  argument; if NULL or missing, coordinates of (0,0,0) are used.
*
*  Occupancy data are taken from the atomic property named by the `occup_prop'
*  argument, if NULL, the occupancy field is left blank.
*
*  B-value data are taken from the atomic property named by the `bvalu_prop'
*  argument, if NULL, the B-value field is left blank.
*
*  Atom colors are taken from the atomic property named by the `acolor_prop'
*  argument, if NULL, no color records will be generated.
*
*  Atom radii are taken from the atomic property named by the `arad_prop'
*  argument, if NULL, no radius records will be generated.
* 
*  FALSE is returned on error, in which case error messages will be queued.
*
*  See cx_pdb.doc for authoratative documentation on PDB dialect used here!
*----------------------------------------------------------------------------
*
*  cx_Integer cx_pdb_set_atomnames(cx_Object mol, cx_Object *aa);
*
*  Invent PDB-style four-character names for all atoms in `mol' and attach as
*  the atom property "atomname".  If not NULL, integer array `aa' is assumed
*  to contain 0-origin indicies (i.e., aa[0] is the first atom) and is used
*  for atom naming.  Returns 0 on error, 1 on success, and 2 if successful
*  but atom names are not unique (i.e., more than 2803 atoms or an atom has
*  more than 61 attached hydrogens).
*
*  Invented labels for all atoms except hydrogens consist of a right-
*  justified atomic symbol in the first two characters followed by a
*  left-justified 2-character identifier corresponding to the sequential
*  number of atoms.  If there are more than 99 atoms, an ad hoc identifier
*  is used, i.e., [1,2,..98,99,aa,ab,..,ay,az,aA,aB,..ZY,ZZ], with alpha
*  id's repeating after 2802 atoms.
*
*  Invented hydrogen atom labels consist of the letter H, D, or T in
*  position 2 followed by the 2-character identifier of the atom to which
*  they are attached; if more than one hydrogen is attached to an atom,
*  a sequential digit appears in position 1.  Hydrogen atoms bonded to
*  other than one other non-hydrogen are given normal atom identifiers.
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
#include "cx.h"
#include "cx_molecule.h"
#include "cu.h"
#include "cx_pdb.h"
#include "pdb.h"

#define PDBRUN_SUPPORT

/*============================================================================
 *  onemap - returns static, left-justified, one-character string
 *
 *  Maps integers to single characters (digits then letters), i.e.,
 *
 *    number: [..,-2,-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,..,60,61,62,63,..]
 *    return: [.., 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b,.., Y, Z, a, b,..]
 *     group:  ..---------  -------------------------  -------------  -----..
 *                                                                    wrap ..
 *
 *  This is used as an ad hoc mapping for one character fields (e.g., hcount).
 *  Note only numbers 1-9 are mapped uniquely.
 */

static char *onemap(int n)
{
  static char cmap[52] = {
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  };
  static char str[2];

  /*** Return "0" iff zero or negative. ***/

  if (1 > n) return "0";

  /*** Return digit for small numbers, letter for others. ***/

  str[0] = (10 > n ? ('0' + n) : cmap[(n - 10) % 52]);
  str[1] = '\0';
  return str;
}

/*============================================================================
 *  twomap - returns static, left-justified, two-character string
 *
 *  Maps integers to single characters (digits then letters), i.e.,
 *
 * number: [-2,-1,0,1,2,..,98,99,100,101,..,124,125,126,..,2802,2803,2804,2805]
 * return: [ 0, 0,0,1,2,..,98,99, aa, ab,.., ay, az, aA,..,  ZY,  ZZ,  aa,  ab]
 *  group: ..------ -------------  ---------------------------------   -------
 *                                                                     wrap....
 *
 *  This is used as an ad hoc mapping for two character fields (e.g., alabs).
 *  Note only numbers 1-99 are mapped uniquely.
 */

static char *twomap(int n)
{
  static char cmap[52] = {
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  };
  static char str[4];

  /*** Return "0 " iff zero or negative. ***/

  if (1 > n) return "0 ";

  /*** Return two digits for small numbers, two letters for others. ***/

  if (100 > n) {
     sprintf(str, "%-2d", n);
  } else {
     n      = (n - 100) % 2704;
     str[0] = cmap[n / 52];
     str[1] = cmap[n % 52];
     str[2] = '\0';
  }
  return str;
}

/*============================================================================
 *  pdb_atom_symbol() -- return PDB-style atomic symbol
 *
 *  This returns a two character, right-justified atomic symbol.
 *  " D" and " T" are returned for deuterium and tritium.
 */

static char *pdb_atom_symbol(cx_Object atom)
{
   static char  buf[4];
   char        *asym = cx_sprop(atom, "atomic symbol");

   /*** Return "**" on error (shouldn't happen). ***/

   if (NULL == asym) return "**";

   /*** Use two character atomic symbols literally. ***/

   if ('\0' != asym[1]) return asym;

   /*** Provide special isotopic hydrogen symbols. ***/

   if ('H' == *asym) {
      switch (cx_iprop(atom, "mass")) {
         case 2:  return " D";
         case 3:  return " T";
         default: return " H";
      }
   }

   /*** Right justify one-character atomic symbols. ***/

   buf[0] = ' ';
   buf[1] = *asym;
   buf[2] = '\0';
   return buf;
}

/*============================================================================
 *  cx_pdb_set_atomnames() - invent 4-character atom names
 *
 *  Invent PDB-style four-character names for all atoms in `mol' and attach as
 *  the atom property "atomname".  If not NULL, integer array `aa' is assumed
 *  to contain 0-origin indicies (i.e., aa[0] is the first atom) and is used
 *  for atom naming.  Returns 0 on error, 1 on success, and 2 if successful
 *  but atom names are not unique (i.e., more than 2803 atoms or an atom has
 *  more than 61 attached hydrogens).
 *
 *  Invented labels for all atoms except hydrogens consist of a right-
 *  justified atomic symbol in the first two characters followed by a
 *  left-justified 2-character identifier corresponding to the sequential
 *  number of atoms.  If there are more than 99 atoms, an ad hoc identifier
 *  is used, i.e., [1,2,..98,99,aa,ab,..,ay,az,aA,aB,..ZY,ZZ], with alpha
 *  id's repeating after 2802 atoms.
 *
 *  Invented hydrogen atom labels consist of the letter H, D, or T in
 *  position 2 followed by the 2-character identifier of the atom to which
 *  they are attached; if more than one hydrogen is attached to an atom,
 *  a sequential digit appears in position 1.  Hydrogen atoms bonded to
 *  other than one other non-hydrogen are given normal atom identifiers.
 */

cx_Integer cx_pdb_set_atomnames(cx_Object mol, cx_Object *aa)
{
   cx_Integer  rv = 1;
   int         k, na, nh;
   char        buf[8], *hlab;
   cx_Object   atoms, atom, xatom, hat, bonds, bond;

   /*** Can't operate on NULL molecule. ***/

   if (NULL == mol) return 0;

   /*** Clear extant atomnames. ***/

   na = cx_count(mol, CX_OB_ATOM);

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms)))
      cx_set_sprop(atom, "atomname", NULL);

   /*** Loop over non-hydrogen atoms in molecule. ***/

   cx_reset(atoms);
   for (k = 0; k < na; k++) {
      atom = (aa ? aa[k] : cx_next(atoms));
      if (1 != cx_iprop(atom, "atomic number") ||
	  1 != cx_count(atom, CX_OB_BOND))          {

         /*** Name is right-justified atomic symbol followed by atom id. ***/

         sprintf(buf, "%s%s", pdb_atom_symbol(atom), twomap(k + 1));
         cx_set_sprop(atom, "atomname", buf);

         /*** If generated name is not unique, dink return value. **/

         if (2802 < k) rv = 2;

         /*** Loop over attached hydrogens. ***/

	 nh    = 0;
	 hat   = NULL;
	 bonds = cx_stream(atom, CX_OB_BOND);
	 while (NULL != (bond = cx_next(bonds))) {
	    xatom = cx_xatom(atom, bond);
            if (1 == cx_iprop(xatom, "atomic number")) {

               /*** Create PDB-style hydrogen label for xatom. ***/

               sprintf(buf, "%s%s", pdb_atom_symbol(xatom), twomap(k + 1));
               *buf = *onemap(++nh);
               cx_set_sprop(xatom, "atomname", buf);

               /*** Save first hydrogen so marked. ***/

               if (NULL == hat) hat = xatom;

               /*** Just for completeness, record if H-label not unique. **/

               if (61 < nh) rv = 2;
            }
         }

         /*** Remove H sequence number if only 1 attached hydrogen. ***/

         if (1 == nh) {
            hlab = cx_sprop(hat, "atomname");
	   *hlab = ' ';
            cx_set_sprop(hat, "atomname", hlab);
         }
      }
   }

   /*** Name remaining atoms, e.g., [H]-[H], as standard atom name. ***/

   cx_reset(atoms);
   for (k = 0; k < na; k++) {
      atom = (aa ? aa[k] : cx_next(atoms));
      if (NULL == cx_sprop(atom, "atomname")) {
         sprintf(buf, "%s%s", pdb_atom_symbol(atom), twomap(k + 1));
         cx_set_sprop(atom, "atomname", buf);
      }
   }
   cx_destroy(atoms);

   return rv;
}


/*============================================================================
 *  sort_atoms_by_part() - index sort atoms by part
 *
 *----------------------------------------------------------------------------
 *  Return integer array with atom indicies sorted by part.
 *  Also sets a 0-index new order in integer atom property "neworder".
 *
 *  Operates by dfs and distribution sort (linear in time and space).
 *  Returns NULL if molecule has no atoms or on error.
 *
 *----------------------------------------------------------------------------
 *  Old way:
 *
 *  This fills integer arrays with atom indexes and reverse indexes
 *  sorted by part.  Original order of atoms is maintained within parts.
 *  Operates by dfs and distribution sort (linear in time and space).
 *  Returns number of parts or 0 if molecule has no atoms or on error.
 *
 *  new[i] is the atom index of the i-th entry in new order
 *  e.g., new[1] is the first atom to be output.
 *
 *  old[i] is the new order of atom index with index i.
 *  e.g., old[1] is the output position of the atom with index 1.
 *  
 */

static cx_Object *sort_atoms_by_part(cx_Object mol)
{
   int      *freq, na, ia, nparts, part;
   cx_Object atoms, atom, *aatmp, *aaout;

   /*** Return 0 if molecule has no atoms. ***/

   if (1 > (na = cx_count(mol, CX_OB_ATOM))) return NULL;

   /*** Partition. ***/

   nparts = cu_mol_setpart(mol, "part");

   /*** Create & initialize array of atoms in original order. ***/

   aatmp = (cx_Object *) cx_malloc(na * sizeof(cx_Object));
   atoms = cx_stream(mol, CX_OB_ATOM);
   for (ia = 0; NULL != (atom = cx_next(atoms)); ia++)
      aatmp[ia] = atom;
   cx_destroy(atoms);

   /*** Create & initialize frequency distribution array. ***/

   freq = (int *) cx_malloc(nparts * sizeof(int));

   for (part = 0; part < nparts; part++)
      freq[part] = 0;

   /*** Fill frequency distribution array in forward order. ***/

   for (ia = 0; ia < na; ia++) {
      part = cx_iprop(aatmp[ia], "part");
      freq[part]++;
   }

   /*** Make frequency distribution array cumulative. ***/

   for (part = 1; part < nparts; part++)
      freq[part] += freq[part - 1];

   /*** Do stable distribution sort into output array. ***/

   aaout = (cx_Object *) cx_malloc(na * sizeof(cx_Object));
   for (ia = na - 1; ia >= 0; ia--) {
      part               = cx_iprop(aatmp[ia], "part");
      freq[part]        -= 1;
      aaout[freq[part]]  = aatmp[ia];
      cx_set_iprop(aatmp[ia], "neworder", freq[part]);
   }

   /*** Toss temporary arrays and return array of sorted indicies. ***/

   cx_free(freq);
   cx_free(aatmp);
   return aaout;
}

/*============================================================================
 *  atuplename() -- return property name of atomtuple with given prefix
 *
 *  Return NULL if not found; warn if there's more than one.
 *  Cute move: string is parented by the tuple property.
 */

static cx_String atuplename(cx_Object mol, cx_String prefix)
{
   cx_Object tuples, tuple;
   char      *tname, ebuf[80], *func = "atuplename";

   /*** Return NULL if either argument is NULL. ***/

   if (NULL == mol || NULL == prefix) return NULL;

   /*** Return NULL if no tuple names start with given prefix. ***/

   if (NULL == (tuples = cx_prefix2atuples(mol, prefix))) return NULL;
   if (NULL == (tuple  = cx_next(tuples))               ) return NULL;

   /*** Get atomtuple name to return. ***/

   tname = cx_atomtuple_name(tuple);

   /*** Warn if more than one exists. ***/

   if (NULL != (tuple = cx_next(tuples))) {
      sprintf(ebuf, "tuple prefix \"%s\" is ambiguous:", prefix);
      cx_error_save(ebuf,                     CX_ERR_WARN, func);
      cx_error_save(tname,                    CX_ERR_WARN, func);
      cx_error_save(cx_atomtuple_name(tuple), CX_ERR_WARN, func);
      while (NULL != (tuple = cx_next(tuples)))
         cx_error_save(cx_atomtuple_name(tuple), CX_ERR_WARN, func);
   }

   /*** Clean up and return first one, or NULL. ***/

   cx_destroy(tuples);
   return tname;

}

/*============================================================================
 *  cx_pdb_write() - write a pdb entry to given stream for given molecule
 */

cx_Integer cx_pdb_write(FILE *fp,              /* output stream             */
                        cx_Object mol,         /* molecule to output        */
                        cx_String name,        /* for COMPND record         */
                        cx_String remark,      /* for REMARK record         */
                        cx_String alabs_prop,  /* atom label prop name      */
                        cx_String resid_prop,  /* residue atomprop name     */
                        cx_String chain_prop,  /* chain atomprop name       */
                        cx_String resno_prop,  /* resno atomprop name       */
                        cx_String coord_prop,  /* coordinates atomprop name */
                        cx_String occup_prop,  /* occupancy atomprop name   */
                        cx_String bvalu_prop,  /* bvalue atomprop name      */
                        cx_String acolor_prop, /* color atomprop name       */
                        cx_String arad_prop)   /* radius atomprop name      */
{
   cx_Object  *aa, atoms, atom, bonds, bond, xatom;
   cx_String  alabs, resid, chain, resno, coord, occup, bvalu;
   float      x, y, z;
   char       *p;
   int        *con, ncon, mcon, ifrns, doter;
   int        i, j, k, na, bo, part, opart;
   pdb_record pdb;
#ifdef PDBRUN_SUPPORT
   cx_String  color, radius;
   pdb_record user_color, user_radius;
   float      r, g, b;
#endif

   /*** Find names of atomtuple properties. ***/

   alabs = atuplename(mol, alabs_prop);
   resid = atuplename(mol, resid_prop);
   chain = atuplename(mol, chain_prop);
   resno = atuplename(mol, resno_prop);
   coord = atuplename(mol, coord_prop);
   occup = atuplename(mol, occup_prop);
   bvalu = atuplename(mol, bvalu_prop);
#ifdef PDBRUN_SUPPORT
   color = atuplename(mol, acolor_prop);
   radius = atuplename(mol, arad_prop);
#endif

   /*** Sort atoms by part, i.e., disconnected components. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   na    = cx_count(mol, CX_OB_ATOM);
   aa    = sort_atoms_by_part(mol);

   /*** If alabs is NULL, invent "atomname" labels. ***/

   if (NULL == alabs) { cx_pdb_set_atomnames(mol, aa); alabs = "atomname"; }

   /*** Determine if (any) residue numbers are available. ***/

   ifrns = 0;
   if (resno)
      for ( k = 0; k < na; k++)
         if (0 != cx_iprop(aa[k], resno)) { ifrns = 1; break; }

   /*** Determine if TER separators needed. ***/

   doter = (0 < cx_count(mol, CX_OB_BOND));

   /*** Write COMPND record. ***/

   pdb.record_type = PDB_COMPND;
   pdb.pdb.author.continuation = ' ';
   strncpy(pdb.pdb.compnd.data, (name && *name) ? name : "(noname)",
      sizeof pdb.pdb.compnd.data - 1);
   pdb_write_record(fp, &pdb, NULL, 0);

   /*** Write REMARK record if available. ***/

   if (remark && *remark) fprintf(fp, "REMARK %s\n", remark);

#ifdef PDBRUN_SUPPORT
   /*** Write PDBRUN stuff in there is data. ***/
   if (NULL != color || NULL != radius) {
      pdb.record_type = PDB_USER_PDBRUN;
      pdb.pdb.user_pdbrun.version = 6;
      pdb_write_record(fp, &pdb, NULL, 0);
      user_color.record_type = PDB_USER_COLOR;
      user_color.pdb.user_color.rgb[0] = -1;
      (void) strcpy(user_color.pdb.user_color.spec, "unnamed");
      user_radius.record_type = PDB_USER_RADIUS;
      user_radius.pdb.user_radius.radius = 0;
   }
#endif

   /*** Write ATOM records by part (disconnected component number). ***/

   opart = 0;
   cx_reset(atoms);
   for (k = 0; k < na; k++) {
      atom = aa[k];

      /*** Write unnumbered TER record when part changes. ***/

      if (doter && opart != (part = cx_iprop(atom, "part"))) {
         fprintf(fp, "TER   \n");
         opart = part;
      }

#ifdef PDBRUN_SUPPORT
      /*** Write PDBRUN type information if it changed ***/
      if (NULL != color) {
	 p = (char *) cx_sprop(atom, color);
	 if (p != NULL && 3 == sscanf(p, "%f,%f,%f", &r, &g, &b))
	    if (user_color.pdb.user_color.rgb[0] != r
	    ||  user_color.pdb.user_color.rgb[1] != g
	    ||  user_color.pdb.user_color.rgb[2] != b) {
	       user_color.pdb.user_color.rgb[0] = r;
	       user_color.pdb.user_color.rgb[1] = g;
	       user_color.pdb.user_color.rgb[2] = b;
	       pdb_write_record(fp, &user_color, NULL, 0);
	    }
      }
      if (NULL != radius) {
	 r = cx_rprop(atom, radius);
	 if (r != user_radius.pdb.user_radius.radius) {
	    user_radius.pdb.user_radius.radius = r;
	    pdb_write_record(fp, &user_radius, NULL, 0);
	 }
      }
#endif

      /*** Start ATOM record. ***/

      pdb.record_type = PDB_ATOM;
      pdb.pdb.atom.serial_num = cx_iprop(atom, "neworder") + 1;
      pdb.pdb.atom.ftnote_num = 0;

      /*** Atom names are always four characters; use symbol if missing. ***/

      p = (char *) cx_sprop(atom, alabs);
      if (p) {
	 strncpy(pdb.pdb.atom.name, p, 4);
	 pdb.pdb.atom.name[4] = '\0';
      }
      else {
	 pdb.pdb.atom.name[0] = ' ';
	 sprintf(pdb.pdb.atom.name + 1, p, 2);
	 pdb.pdb.atom.name[3] = '\0';
      }

      /*** Residue name is limited to three characters. ***/

      p = (resid ? cx_sprop(atom, resid) : NULL);
      strncpy(pdb.pdb.atom.residue.name, (p ? p : "RES"), 3);
      pdb.pdb.atom.residue.name[3] = '\0';

      /*** Chain property is limited to one character. ***/

      p = (chain ? cx_sprop(atom, chain) : NULL);
      pdb.pdb.atom.residue.chain_id = ((p && *p)? *p : ' ');

      /*** Write residue numbers or 1-origin part numbers if unavailable. ***/

      if (ifrns)       pdb.pdb.atom.residue.seq_num = cx_iprop(atom, resno);
      else if (!doter) pdb.pdb.atom.residue.seq_num = 0;
      else             pdb.pdb.atom.residue.seq_num = cx_iprop(atom, "part")+1;

      /*** Write spaces into mystery field. ***/

      pdb.pdb.atom.residue.insert_code = ' ';
      pdb.pdb.atom.alt_loc = ' ';

      /*** Get and write coordinates (actually, "coordinates.1"). ***/
/*
      if( (p=cx_sprop(atom,coord)) )
      {   sscanf(p, "%g,%g,%g", &x, &y, &z);
      } else x = y = z = 0.0;
*/
      p = cx_sprop(atom,coord);
      if (NULL == p || 3 != sscanf(p, "%g,%g,%g", &x, &y, &z))
        x = y = z = 0.0;

      pdb.pdb.atom.x = x;
      pdb.pdb.atom.y = y;
      pdb.pdb.atom.z = z;

      /*** Write real properties occupancy & bvalue, leave blank if NULL. ***/

      if (occup) pdb.pdb.atom.occupancy = cx_rprop(atom, occup);
      else       pdb.pdb.atom.occupancy = 0;

      if (bvalu) pdb.pdb.atom.temp_factor = cx_rprop(atom, bvalu);
      else       pdb.pdb.atom.temp_factor = 0;

      /*** Done with atom record. ***/

      pdb_write_record(fp, &pdb, NULL, 0);
   }

   /*** Write TER record to delimit CONECT records, if needed. ***/

   if (doter) fprintf(fp, "TER   \n");

   /*** Write CONECT records; loop over atoms in ascending index order. ***/

   pdb.record_type = PDB_CONECT;
   pdb.pdb.conect.bonds[0].hydrogen[0] = 0;
   pdb.pdb.conect.bonds[0].hydrogen[1] = 0;
   pdb.pdb.conect.bonds[0].salt = 0;
   pdb.pdb.conect.bonds[1].hydrogen[0] = 0;
   pdb.pdb.conect.bonds[1].hydrogen[1] = 0;
   pdb.pdb.conect.bonds[1].salt = 0;
   mcon = 10;
   con  = (int *) cx_malloc(mcon * sizeof(int));   /* alloc tmp array */

   for (k = 0; k < na; k++) {
      atom = aa[k];

      /*** Loop over bonds from this atom. ***/

      if (0 == cx_count(atom, CX_OB_BOND)) continue;
      ncon = 0;

      bonds = cx_stream(atom, CX_OB_BOND);
      while (NULL != (bond = cx_next(bonds))) {

         /*** Find "to" atom. ***/

	 xatom = cx_xatom(atom, bond);

         /*** Find bond order and record new "to" atom index `bo' times. ***/

         bo = cx_iprop(bond, "bond order");
         for (i = 0; i < bo; i++) {

            /*** Reallocate connection array as needed. **/

            if (ncon >= mcon) {
               mcon *= 2;
               con   = (int *) cx_realloc(con, mcon * sizeof(int));
            }

            /*** Record new "to" connection. ***/

            con[ncon++] = cx_iprop(xatom, "neworder") + 1;
         }
      }

      /*** Output "from" and "to" connection indicies in order. ***/

      pdb.pdb.conect.serial_num = cx_iprop(atom, "neworder") + 1;
      j = 0;
      while (j < ncon) {
	 for (i = 0; i < 4; i++, j++)
	    pdb.pdb.conect.covalent[i] = j < ncon ? con[j] : 0;
	 pdb_write_record(fp, &pdb, NULL, 0);
      }
   }

   /*** Terminate entry with END record. ***/

   pdb.record_type = PDB_END;
   pdb_write_record(fp, &pdb, NULL, 0);

   /*** Tidy up and return successfully. **/

   cx_free(aa);
   cx_free(con);
   return TRUE;
}
