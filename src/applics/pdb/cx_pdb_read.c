/*****************************************************************************
*  cx_pdb_read.c -- CX support for reading PDB format
*
*  This file provides functions to read and write PDB's using CX utilities:
*
*     cx_pdb_read() ............ read pdb from stream
*     cx_pdb_eof() ............. is cx_pdb_read() at eof?
*
*----------------------------------------------------------------------------
*
*  cx_Object cx_pdb_read(FILE *fp);
*  
*  which reads the next pdb entry on the given stream `fp' and returns a
*  molecule as a cx_Object with the following properties set:
*  
*  molecule properties:
*     "name" ........... molecule name            (string)
*     "remark" ......... arbitrary remark         (string)
*
*  atom properties:
*     "atomname" ....... atom name                (string  tuple   )
*     "resname" ........ residue name             (string  tuple   )
*     "chain" .......... chain designator         (string  tuple   )
*     "resno" .......... residue number           (integer tuple   )
*     "coordinates" .... (x,y,z) coordinates      (real    tuple x3)
*     "occupancy" ...... location occupancy       (real    tuple   )
*     "bvalue" ......... temperature B-value      (real    tuple   )
*
*  Note: All properties are actually stored as string properties as per input.
*        Numeric properties are checked for validity.
*
*  NULL is returned on error, which may be due to invalid input or EOF.
*  If not due to EOF, error messages will be queued (see cx.doc).
*  
*----------------------------------------------------------------------------
*  
*  cx_Integer cx_pdb_eof(void);
*  
*  which returns TRUE if and only if an EOF was encountered during the last
*  call to cx_pdb_read().
*
*----------------------------------------------------------------------------
*
*  Notes on PDB dialect
*
*  On input (cx_pdb_read):
*
*    o  reads only COMPND, REMARK, TER, ATOM, HETATM, CONECT and END records
*    o  COMPND and REMARK records are optional
*    o  TER records are ignored
*    o  EOF or END record terminates molecule
*    o  ATOM and HETATOM records are synonymous
*    o  Leading non-alphanumerics in atom symbol allowed (ignored)
*    o  Atomic symbols "D" and "T" are recognized as [2H] and [3H]
*    o  "Wildcard" atoms (*) used for atoms with invalid atomic symbols
*    o  Atom index (2nd field) is used for CONECT semantics, any order is ok
*    o  Repeated connections in a CONECT record interpreted as multiple bonds
*
*  ATOM record format
*
*    0123456789012345678901234567890123456789012345678901234567890123456
*    123456789012345678901234567890123456789012345678901234567890123456
*    ATOM   1169  O   LEU A 157      37.237  39.482  45.376  1.00 30.00
*    ------.----.----.--- -.---.---.------- ------- ------- ----- -----
*    1      2    3    4   5 6   7   8       9       10      11    12
*
*    #   COLS  MEANING
*    --- -- -- --------------------------------------------------------------
*    1    1- 6  record type flag, i.e., ATOM or HETATM
*    2    8-11  atom index, monotonically increasing ordinal integers
*    3   13-16  atom label, 1st 2 chars are right-justified atomic symbol
*    4   18-20  residue name (3 chars?)
*    5   22-22  chain designator, alphanumeric character
*    6   24-26  residue number, ascending integers
*    7   28-30  mystery field
*    8   32-38  x-coordinate
*    9   40-46  y-coordinate
*    10  48-54  z-coordinate
*    11  56-60  occupancy, real number in range 0.0 to 1.0
*    12  62-66  B-value (temp), real number in range 0.0 to 100.0 (99.99?)
*
*  See cx_pdb.doc for authoratative documentation!
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
/* #include "cx_molecule.h" */
#include "cx_pdb.h"
#include "pdb.h"

#define	PDBRUN_SUPPORT

/*** Globally available flag (mostly so caller can reset it) ***/

static cx_Integer cx_pdb_pdbrun = FALSE; /* global flag set TRUE if
					  * input is pdbrun vers >= 6 */

/*** Maximum length of input line. ***/

#define MX_LINE 2048

/*** Semi-global end-of-file flag. ***/

static cx_Integer ateof = FALSE;

/*============================================================================
 *  trim() -- remove leading spaces
 */

static char *trim(char *s)
{
   while (' ' == *s)
     s++;
   return s;
}

/*============================================================================
 *  trunk() -- truncate string after last non-space character, return argument
 */

static char *trunk(char *s)
{
   char *p = s;
   char *q = NULL;

   do {
      if      (' ' != *p) q = NULL;
      else if (NULL == q) q = p;
   } while (*(++p));

   if (q) *q = '\0';
   return s;
}

/*============================================================================
 *  getline() -- get an input line safely
 *
 *  This reads stream fp up to the first newline, putting the first (nc - 1)
 *  characters into char array s and returning s as a zero-terminated string,
 *  excluding the newline.  If the line is longer than (nc-1) chars, writes
 *  an error message and returns a truncated string.  Returns NULL on EOF.
 */
 
static char *getline(char *s, FILE *fp, int nc)
{
    char erk[44];
    char *p;
    int c;
 
   /*** Do a normal fgets(), returning NULL on EOF. ***/

   if (NULL == fgets(s, nc, fp)) return NULL;

   /*** Truncate newline, zero-terminate, and return. ***/

   p = strchr(s, '\n');
   if (p) { *p = '\0'; return s; }

   /*** Oops, line too long: write error message. ***/

   cx_error_save("input line too long:", CX_ERR_ERROR, "getline");
   strncpy(erk, s, 40);
   erk[40] = '.'; erk[41] = '.'; erk[42] = '.'; erk[43] = '\0';
   cx_error_save(erk, CX_ERR_ERROR, "getline");
   cx_error_save("skipping remainder of line", CX_ERR_ERROR, "getline");

   /*** Truncate line, skip rest of line, and return s. ***/

   s[nc-1] = '\0';
   while ('\n' != (c = getc(fp)))
      if (EOF == c) return NULL;
   return s;
}


/*============================================================================
 * sym2num -- return atomic number from atomic symbol or -1 on error.
 */

static char *sym[] = { "*",
   "H",  "He", "Li", "Be", "B",  "C",  "N",  "O",  "F",  "Ne",
   "Na", "Mg", "Al", "Si", "P",  "S",  "Cl", "Ar", "K",  "Ca",
   "Sc", "Ti", "V",  "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn",
   "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y",  "Zr",
   "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn",
   "Sb", "Te", "I",  "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd",
   "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
   "Lu", "Hf", "Ta", "W",  "Re", "Os", "Ir", "Pt", "Au", "Hg",
   "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th",
   "Pa", "U",  "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm"
};

static int sym2num(char *str)
{
   char s[4];
   int  i;

   /*** Make NULL-terminated string for comparison. ***/

   s[0] = str[0];
   s[1] = (' ' == str[1] ? '\0' : tolower(str[1]));
   s[2] = '\0';

   /*** Linear search for standard atomic symbol. ***/

   for (i = 0; i < 101; i++)
      if (0 == strcmp(s, sym[i])) return i;

   /*** Allow "D" and "T" as synonyms for "H". ***/

   if (('D' == s[0] || 'T' == s[0]) && '\0' == s[1]) return 1;

   /*** Return -1 if unknown. ***/

   return -1;
}

/*============================================================================
 *  cx_pdb_eof() -- returns TRUE iff last cx_pdb_read() call encountered EOF
 */

cx_Integer cx_pdb_eof(void)
{
   return ateof;
}

/*============================================================================
 *  newatomtuple()
 */

static cx_Object newatomtuple(cx_Object parent, cx_String pname)
{
   cx_Object atuple = cx_create_atomtuple(parent, pname);
   cx_set_datatype(atuple, cx_pname2datatype(NULL, pname));
   return atuple;
}

/*============================================================================
 *  make_bond() -- create a bond when given two indices
 */

static int make_bond(cx_Object *atom, int max, int from, int to, char *buf)
{
   cx_Object bond;
   int       bo;
   char      *func = "make_bond";

   /*** Validate. ***/

   if (to == 0)
      return TRUE;
   if (to > max) {
      cx_error_save("invalid atom index", CX_ERR_ERROR, func);
      cx_error_save(buf, CX_ERR_ERROR, func);
      return FALSE;
   }

   /*** Ignore redundant information. ***/

   if (to <= from)
      return TRUE;

   /*** Create new bond. ***/

   if (NULL == (bond = cx_bond(atom[from], atom[to]))) {
      bond = cx_create_bond(atom[from], atom[to], 1);

   /*** Increment bond order. ***/

   } else {
      bo = cx_iprop(bond, "bond order");
      cx_set_iprop(bond, "bond order", bo + 1);
   }
   return TRUE;
}

/*============================================================================
 *  cx_pdb_read() -- read PDB on stream fp and returns annotated molecule
 *                   or camera
 */

cx_Object cx_pdb_read(FILE *fp)
{
   cx_Object  mol;          /* output object */
   cx_Object  atom;         /* temporary objects */
   cx_Object  *xa;          /* array of atom objects by atom index */
   char       buf[MX_LINE];
   int        natoms, skip, lineno, xroom, mix;
   char       xstr[16], ystr[16], zstr[16], xyzstr[80];
#ifdef PDBRUN_SUPPORT
   int        have_color, have_radius;
   int        made_color, made_radius;
   char       rgbstr[80];
   cx_Real    rad;
   cx_Object  camera;
#endif
   float      x, y, z;

   char       *func = "cx_pdb_read";
   char       *p, *pbuf, asym[6], str[8];
   int        atnum, ok, from, i, aindx;
   cx_Integer rgb;

   pdb_record pdb;
   struct pdb_conect *conect;

   /*** Initialize and create empty molecule. ***/

   ateof    = FALSE;    /* global flag is set TRUE iff EOF encountered */
   skip     = FALSE;    /* local flag set TRUE if input is invalid */
   natoms   = 0;
   mix      = 0;        /* maximum atom index, for validation */
#ifdef PDBRUN_SUPPORT
   have_radius = FALSE;	/* local flag set TRUE if USER RADIUS seen */
   made_radius = FALSE;	/* local flag set TRUE if radius tuple created */
   have_color = FALSE;	/* local flag set TRUE if USER COLOR seen */
   made_color = FALSE;	/* local flag set TRUE if color tuple created */
   camera = NULL;	/* no camera by default */
   rad = 0.0;           /* no radius by deafult */
#endif
   lineno   = 0;
   xroom    = 100;

   if (NULL == (xa = (cx_Object *) cx_malloc(xroom * sizeof(cx_Object)))) {
      cx_error_save("out of memory creating xa", CX_ERR_ERROR, func);
      return NULL;
   }
   for (i = 0; i < xroom; i++)
      xa[i] = NULL;

   if (NULL == (mol = cx_create_molecule(NULL))) {
      cx_error_save("out of memory creating molecule", CX_ERR_ERROR, func);
      return NULL;
   }

   /*** Create tuples for atom properties. ***/

   ok  =       (NULL != newatomtuple(mol, "atomname"   ));
   ok  = ok && (NULL != newatomtuple(mol, "resname"    ));
   ok  = ok && (NULL != newatomtuple(mol, "chain"      ));
   ok  = ok && (NULL != newatomtuple(mol, "resno"      ));
   ok  = ok && (NULL != newatomtuple(mol, "coordinates"));
   ok  = ok && (NULL != newatomtuple(mol, "occupancy"  ));
   ok  = ok && (NULL != newatomtuple(mol, "bvalue"     ));

   if (!ok) {
      cx_error_save("out of memory creating atomtuples", CX_ERR_ERROR, func);
      return NULL;
   }

   /*** Loop over input line by line. ***/

   while (NULL != (pbuf = getline(buf, fp, MX_LINE))) {
      lineno++;
      pdb = pdb_read_string(buf);

      /*** END record: break out of input loop. ***/

      if (PDB_END == pdb.record_type) break;

      /*** Skip to next record if this molecule isn't valid. ***/

      if (skip) continue;

      /*** COMPND record: save name as molecular property "name". ***/

      if (PDB_COMPND == pdb.record_type) {
         p = trunk(trim(pdb.pdb.compnd.data));
         if (*p) cx_set_sprop(mol, "name", p);

      /*** REMARK record: save as molecular property "remark". ***/

      } else if (PDB_REMARK == pdb.record_type) {
         p = trunk(trim(buf + 6));
         if (*p) cx_set_sprop(mol, "remark", p);

      /*** TER record: ignore. ***/

      } else if (PDB_TER == pdb.record_type) {
         ;

     /*** ATOM or HETATM -- create new atom. ***/

      } else if (PDB_ATOM == pdb.record_type || 
                 PDB_HETATM == pdb.record_type) {

         /*** Extract atom index. ***/

	 aindx = pdb.pdb.atom.serial_num;

         /*** Extract atomic symbol,  ' N1 ' ' N10' 'Br2 ' 'Br12' '2H16' ***/

         p  = asym;
         if (isupper(pdb.pdb.atom.name[0])) *p++ = pdb.pdb.atom.name[0];
         *p++ = pdb.pdb.atom.name[1];
         *p   = '\0';

         /*** Determine atomic number. ***/

         if ( 0 > (atnum = sym2num(asym))) {
            atnum = 0;
            cx_error_save("unknown atomic symbol", CX_ERR_WARN, func);
            cx_error_save(buf, CX_ERR_WARN, func);
         }

         /*** Add atom. ***/

         if (NULL == (atom = cx_create_atom(mol))) {
            cx_error_save("can't add atom", CX_ERR_ERROR, func);
            cx_error_save(buf, CX_ERR_ERROR, func);
            skip = TRUE;
            continue;
         } 

         /*** Add atom. ***/

         cx_set_iprop(atom, "atomic number", atnum);

         /*** Recognize "D" and "T" as isotopic hydrogen. ***/

         if (1 == atnum && ('D' == *asym || 'T' == *asym))
            cx_set_iprop(atom, "mass", ('D' == *asym ? 2 : 3));

         /*** Keep track of explicit atom count for later validation. ***/

         natoms++;

         /*** Attach name to atom as "atomname" property. ***/

         cx_set_sprop(atom, "atomname", pdb.pdb.atom.name);

         /*** Extract residue, add to atom as "resname" property. ***/

         cx_set_sprop(atom, "resname", pdb.pdb.atom.residue.name);

         /*** Extract chain designator, possibly save as "chain" property. ***/

         if (' ' != pdb.pdb.atom.residue.chain_id) {
            str[0] = pdb.pdb.atom.residue.chain_id;
            str[1] = '\0';
            cx_set_sprop(atom, "chain", str);
         }

         /*** Extract residue number, save as "resno" property. ***/

         cx_set_iprop(atom, "resno", pdb.pdb.atom.residue.seq_num);

         /*** Scan for (x,y,z) coordinates as strings. ***/

         if (3 != sscanf(buf + 30, "%7s %7s %7s", xstr, ystr, zstr)) {
            cx_error_save("bad coordinates", CX_ERR_ERROR, func);
            cx_error_save(buf, CX_ERR_ERROR, func);
            skip = TRUE;
            continue;
         } 

         /*** Check that they're good numbers. ***/

         if (3 != sscanf(buf + 30, "%7g %7g %7g", &x, &y, &z)) {
            cx_error_save("invalid coordinate(s)", CX_ERR_ERROR, func);
            cx_error_save(buf, CX_ERR_ERROR, func);
            skip = TRUE;
            continue;
         }

         /*** Load as atomic "coordinates" property. ***/

	 if (ok) {
            sprintf(xyzstr, "%.3f,%.3f,%.3f", pdb.pdb.atom.x,
	       pdb.pdb.atom.y, pdb.pdb.atom.z);
            ok = ok && cx_set_sprop(atom, "coordinates", xyzstr);
	 }

         /*** Save "occupancy" property. ***/

         cx_set_rprop(atom, "occupancy", pdb.pdb.atom.occupancy);

         /*** Save "bvalue" property. ***/

         cx_set_rprop(atom, "bvalue", pdb.pdb.atom.temp_factor);

         /*** Make room for by atomic index. ***/

         if (aindx >= xroom) {

	    /*** If index looks weird, punt. ***/

            if (aindx >= (2 * xroom)) {
               cx_error_save("high-valued atom index", CX_ERR_ERROR, func);
               cx_error_save(buf, CX_ERR_ERROR, func);
               skip = TRUE;
	       continue;
	    }

	    /*** Double the space available. ***/

	    i      = xroom;
            xroom *= 2;
            xa     = (cx_Object *) cx_realloc(xa, xroom * sizeof(cx_Object));
            if (NULL == xa) {
               cx_error_save("out of memory extending xa", CX_ERR_ERROR, func);
               skip = TRUE;
	       continue;
            }

	    /*** Initialize extended crossreferences. ***/

            for (  ; i < xroom; i++)
               xa[i] = NULL;
         }

	 /*** Check that atom index hasn't already been used. ***/

         if (NULL != xa[aindx]) {
            cx_error_save("duplicate atom index", CX_ERR_ERROR, func);
            cx_error_save(buf, CX_ERR_ERROR, func);
            skip = TRUE;
	    continue;
         }

         /*** Crossreference by atomic index. ***/

         xa[aindx] = atom;
         mix       = CX_MAX(mix, aindx);

#ifdef PDBRUN_SUPPORT
	 /*** Add color and radius if defined. ***/
	 if (have_color) cx_set_sprop(atom, "atom color", rgbstr);
	 if (have_radius) cx_set_rprop(atom, "atom covalent radius", rad);
#endif

      /*** CONECT: interpret bond record ***/


      } else if (PDB_CONECT == pdb.record_type) {

         /***  Loop over atom indicies. ***/

	 conect = &pdb.pdb.conect;
         from = conect->serial_num;
	 if (from < 1 || from > mix) {
               cx_error_save("invalid atom index", CX_ERR_ERROR, func);
               cx_error_save(buf, CX_ERR_ERROR, func);
               skip = TRUE;
	       continue;
	 }
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->covalent[0], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->covalent[1], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->covalent[2], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->covalent[3], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->bonds[0].hydrogen[0], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->bonds[0].hydrogen[1], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->bonds[0].salt, buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->bonds[1].hydrogen[0], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->bonds[1].hydrogen[1], buf);
	 if (!skip)
	    skip = !make_bond(xa, mix, from, conect->bonds[1].salt, buf);
         if (skip) continue;

#ifdef PDBRUN_SUPPORT

      /*** USER PDBRUN: interpret pdbrun record ***/

      } else if (PDB_USER_PDBRUN == pdb.record_type) {
	 if (pdb.pdb.user_pdbrun.version < 5) continue;
         cx_create_datatype(NULL, "ACOLOR",
			    "atom color", "Atom Color", "3A",  "REAL",
			    "RGB atom color with components [0,1]");
         cx_create_datatype(NULL, "ARAD",
			    "atom covalent radius", "Atom Covalent Radius",
			    "1A",  "REAL",
			    "Atomic covalent radius (in Angstrom)");
	 cx_pdb_pdbrun = TRUE;

      /*** USER COLOR: interpret color record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_COLOR == pdb.record_type) {
         if (!made_color) {
	    if (NULL == newatomtuple(mol, "atom color")) {
               cx_error_save("out of memory creating color atomtuples",
			     CX_ERR_ERROR, func);
               return NULL;
	    }
	    made_color = TRUE;
	 }
	 (void) sprintf(rgbstr, "%.3f,%.3f,%.3f",
			pdb.pdb.user_color.rgb[0],
			pdb.pdb.user_color.rgb[1],
			pdb.pdb.user_color.rgb[2]);
	 have_color = TRUE;

      /*** USER RADIUS: interpret radius record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_RADIUS == pdb.record_type) {
         if (!made_radius) {
	    if (NULL == newatomtuple(mol, "atom covalent radius")) {
               cx_error_save("out of memory creating color atomtuples",
			     CX_ERR_ERROR, func);
               return NULL;
	    }
	    made_radius = TRUE;
	 }
	 rad = pdb.pdb.user_radius.radius;
	 have_radius = TRUE;

      /*** USER EYEPOS: interpret camera position record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_EYEPOS == pdb.record_type) {
         if (NULL == camera && NULL == (camera = cu_create_camera(NULL))) {
            cx_error_save("out of memory creating camera", CX_ERR_ERROR, func);
            return NULL;
         }
	 cx_set_rprop(camera, CU_PROP_CAMERA_X, pdb.pdb.user_eyepos.xyz[0]);
	 cx_set_rprop(camera, CU_PROP_CAMERA_Y, pdb.pdb.user_eyepos.xyz[1]);
	 cx_set_rprop(camera, CU_PROP_CAMERA_Z, pdb.pdb.user_eyepos.xyz[2]);

      /*** USER ATPOS: interpret scene center record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_ATPOS == pdb.record_type) {
         if (NULL == camera && NULL == (camera = cu_create_camera(NULL))) {
            cx_error_save("out of memory creating camera", CX_ERR_ERROR, func);
            return NULL;
         }
	 cx_set_rprop(camera, CU_PROP_SCENE_X, pdb.pdb.user_atpos.xyz[0]);
	 cx_set_rprop(camera, CU_PROP_SCENE_Y, pdb.pdb.user_atpos.xyz[1]);
	 cx_set_rprop(camera, CU_PROP_SCENE_Z, pdb.pdb.user_atpos.xyz[2]);

      /*** USER WINDOW: interpret window (clipping info) record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_WINDOW == pdb.record_type) {
         if (NULL == camera && NULL == (camera = cu_create_camera(NULL))) {
            cx_error_save("out of memory creating camera", CX_ERR_ERROR, func);
            return NULL;
         }
	 cx_set_rprop(camera, CU_PROP_VIEW_XMIN,   pdb.pdb.user_window.left  );
	 cx_set_rprop(camera, CU_PROP_VIEW_XMAX,   pdb.pdb.user_window.right );
	 cx_set_rprop(camera, CU_PROP_VIEW_YMIN,   pdb.pdb.user_window.bottom);
	 cx_set_rprop(camera, CU_PROP_VIEW_YMAX,   pdb.pdb.user_window.top   );
	 cx_set_rprop(camera, CU_PROP_CLIP_HITHER, pdb.pdb.user_window.hither);
	 cx_set_rprop(camera, CU_PROP_CLIP_YON,    pdb.pdb.user_window.yon   );

      /*** USER FOCUS: interpret focal length record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_FOCUS == pdb.record_type) {
         if (NULL == camera && NULL == (camera = cu_create_camera(NULL))) {
            cx_error_save("out of memory creating camera", CX_ERR_ERROR, func);
            return NULL;
         }
	 cx_set_rprop(camera, CU_PROP_FOCUS_DIST, pdb.pdb.user_focus.focus);

      /*** USER VIEWPORT: interpret render size record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_VIEWPORT == pdb.record_type) {
         if (NULL == camera && NULL == (camera = cu_create_camera(NULL))) {
            cx_error_save("out of memory creating camera", CX_ERR_ERROR, func);
            return NULL;
         }
	 cx_set_rprop(camera, CU_PROP_RENDER_WIDTH, 
		   pdb.pdb.user_viewport.xmax - pdb.pdb.user_viewport.xmin);
	 cx_set_rprop(camera, CU_PROP_RENDER_HEIGHT, 
		   pdb.pdb.user_viewport.ymax - pdb.pdb.user_viewport.ymin);

      /*** USER BGCOLOR: interpret background color record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_BGCOLOR == pdb.record_type) {
         if (NULL == camera && NULL == (camera = cu_create_camera(NULL))) {
            cx_error_save("out of memory creating camera", CX_ERR_ERROR, func);
            return NULL;
         }
         rgb  = ((int) (255 * pdb.pdb.user_bgcolor.rgb[0])) << 8; /* red   */
         rgb &= ((int) (255 * pdb.pdb.user_bgcolor.rgb[0])) << 4; /* green */
         rgb &= ((int) (255 * pdb.pdb.user_bgcolor.rgb[0]))     ; /* blue  */

      /*** USER FILE: interpret file record ***/

      } else if (cx_pdb_pdbrun && PDB_USER_FILE == pdb.record_type) {
	 if (NULL != camera) {
	    /* clean up and return camera.  If we get here, there should not
	     * be a molecule, or the pdbrun file is completely screwed up. */
	    cx_free(xa);
	    if (NULL != mol) {
	       if (0 < cx_count(mol, CX_OB_ATOM)) {
                  cx_error_save("camera and mol defined simultaneously",
				CX_ERR_ERROR, func);
	          cx_destroy(mol);
	          cx_destroy(camera);
	          return NULL;
	       }
	       cx_destroy(mol);
	    }
	    return camera;
	 }
#endif
      }
   }

   /*** Deallocate temporary storage. ***/
  
   cx_free(xa);

   /*** Set EOF flag. ***/

   if (NULL == pbuf) ateof = TRUE;

   /*** If structure isn't valid, deallocate it and return NULL. ***/

   if (skip || 1 > cx_count(mol, CX_OB_ATOM)) {
      cx_destroy(mol);
      return NULL;
   }

   /*** Success: return annotated molecule. ***/

   return mol;
}
