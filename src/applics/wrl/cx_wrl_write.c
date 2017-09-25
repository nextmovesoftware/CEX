/*****************************************************************************
*  cx_wrl_write.c -- CX support for writing PDB format
*
*  This file provides functions to write PDB's using CX utilities:
*
*     cx_wrl_write() ........... write standard VRML model to stream
*     cx_wrl_setatomcolors() ... set standard atom RGB colors
*     cx_wrl_setatomradii() .... set atom radii proportional to std values
*     cx_wrl_setbondradii() .... set bond radii to given value
*
*----------------------------------------------------------------------------
*
*  cx_Integer cx_wrl_write(FILE *fp, cx_Object mol, cx_Integer model);
*
*     Write VRML model of CEX molecule to given stream.  FALSE is returned
*     on error (error messages will be queued).  Attributes are determined
*     from flag `model' as per constants in cx_wrl.h, e.g., CX_WRL_BALLSTICK.
*     If `model' CX_WRL_NONE, all properties must be set up in advance with
*     property names per cx_wrl.h, e.g., CX_WRL_COORDINATES.
*
*  cx_Integer cx_wrl_setatomradii(cx_Object mol, cx_Real factor);
*
*     Generate CX_WRL_ATOMRADIUS properties proportional to standard
*     atomic radii, i.e., a factor of 1.0 creates full size spheres.
*     This function is normally used only for custom model building.
*
*  cx_Integer cx_wrl_setbondradii(cx_Object mol, cx_Real radius);
*
*     Generate CX_WRL_BONDRADIUS properties of given value (Angstroms).
*     This function is normally used only for custom model building.
*
*  cx_Integer cx_wrl_setatomcolors(cx_Object mol);
*
*     Generate standard CX_WRL_ATOMCOLOR properties.
*     This function is normally used only for custom model building.
*
*----------------------------------------------------------------------------
*  To do:
*  [ ] fill in rest of radii
*  [ ] add user remark
*  [ ] deal with names
*  [ ] deal with bondcolors
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
#include <math.h>

#include "cx.h"
#include "cx_molecule.h"
#include "cu.h"
#include "cx_wrl.h"

#define XX_Real double

/*** Info string. ***/

#define XX_CREATOR "Created by cex2wrl 0.34"

/*** Hint: black background if wireframe, light gray otherwise. ***/

#define XX_BG_WIRE  "0.0 0.0 0.0"
#define XX_BG_SOLID "0.9 0.9 0.9"

/*** Hint: preferrred viewer ("examiner" or "walk") ***/

#define XX_VIEWER   "examiner"

/*** Fixed properties used here. ***/

#define XX_PROP_BONDORDER    "bond order"
#define XX_PROP_ATOMICNUMBER "atomic number"

/*** Temporary properties. ***/

#define XX_PROP_TMP  "wrl_aye"
#define XX_PROP_FLAG "wrl_flag"

/*** Standard atom colors as CEX multiplet. ***/

#define MX_RGB 106

static char *rgb[MX_RGB*3] = {
   "1.00,1.00,1.00",  "1.00,1.00,1.00",  "0.96,0.87,0.70",  "0.50,1.00,0.83",
   "0.50,1.00,0.83",  "0.82,0.70,0.55",  "0.00,1.00,0.00",  "0.00,1.00,1.00",
   "1.00,0.27,0.00",  "1.00,0.00,1.00",  "0.96,0.87,0.70",  "0.50,1.00,0.83",
   "0.50,1.00,0.83",  "0.82,0.71,0.55",  "0.00,0.98,0.60",  "1.00,0.65,0.00",
   "1.00,1.00,0.00",  "1.00,0.00,1.00",  "0.96,0.87,0.70",  "0.50,1.00,0.83",
   "0.50,1.00,0.83",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",
   "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",
   "0.69,0.77,0.87",  "0.85,0.65,0.13",  "0.69,0.77,0.87",  "0.96,0.87,0.70",
   "0.96,0.87,0.70",  "0.96,0.87,0.70",  "0.96,0.87,0.70",  "1.00,0.00,1.00",  
   "0.96,0.87,0.70",  "0.50,1.00,0.83",  "0.50,1.00,0.83",  "0.69,0.77,0.87",  
   "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  
   "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.90,0.90,0.90",  
   "0.69,0.77,0.87",  "0.96,0.87,0.70",  "0.96,0.87,0.70",  "0.96,0.87,0.70",  
   "0.96,0.87,0.70",  "1.00,0.00,1.00",  "0.96,0.87,0.70",  "0.50,1.00,0.83",  
   "0.50,1.00,0.83",  "0.69,0.77 0.87",  "1.00,0.76,0.80",  "1.00,0.76,0.80",  
   "1.00,0.76,0.80",  "1.00,0.76,0.80",  "1.00,0.76,0.80",  "1.00,0.76,0.80",  
   "1.00,0.76,0.80",  "1.00,0.76,0.80",  "1.00,0.76,0.80",  "1.00,0.76,0.80",  
   "1.00,0.76,0.80",  "1.00,0.76,0.80",  "1.00,0.76,0.80",  "1.00,0.76,0.80",  
   "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  
   "0.69,0.77,0.87",  "0.69,0.77,0.87",  "0.69,0.77,0.87",  "1.00,0.84,0.00",  
   "0.69,0.77,0.87",  "0.96,0.87,0.70",  "0.96,0.87,0.70",  "0.96,0.87,0.70",  
   "0.96,0.87,0.70",  "1.00,0.00,1.00",  "0.96,0.87,0.70",  "0.50,1.00,0.83",  
   "0.50,1.00,0.83",  "0.69,0.71,0.87",  "1.00,0.71,0.80",  "1.00,0.71,0.80",  
   "1.00,0.71,0.80",  "1.00,0.71,0.80",  "1.00,0.71,0.80",  "1.00,0.71,0.80",  
   "1.00,0.71,0.80",  "1.00,0.71,0.80",  "1.00,0.71,0.80",  "1.00,0.71,0.80",  
   "1.00,0.71,0.80",  "1.00,0.71,0.80",  "1.00,0.71,0.80",  "1.00,0.71,0.80",  
   "0.82,0.71,0.55",  "0.82,0.71,0.55"
};

/*============================================================================
 *  atomradius() -- return "atomradius" of given atom, or -1.0 if unable.
 */

static float atomradius(cx_Object atom)
{
   cx_String str;
   if (CX_OB_ATOM != cx_type(atom)) return -1.0;
   if (NULL == (str = cx_sprop(atom, CX_WRL_ATOMRADIUS))) return -1.0;
   if ('\0' == *str) return -1.;
   return cx_rprop(atom, CX_WRL_ATOMRADIUS);
}

/*============================================================================
 *  atomcolor() -- return "atomcolor" property of atom, or NULL if unable.
 *
 *  atomcolor is a CEX triplet, e.g., "1.0,1.0,0.0" is yellow
 */

static cx_String atomcolor(cx_Object atom)
{
   cx_String str;
   if (CX_OB_ATOM != cx_type(atom)) return NULL;
   if (NULL == (str = cx_sprop(atom, CX_WRL_ATOMCOLOR))) return NULL;
   if ('\0' == *str) return NULL;
   return cx_sprop(atom, CX_WRL_ATOMCOLOR);
}

/*============================================================================
 *  bondradius() -- return "bondradius" of given bond, or -1.0 if unable.
 */

static float bondradius(cx_Object bond)
{
   cx_String str;
   if (CX_OB_BOND != cx_type(bond)) return -1.0;
   if (NULL == (str = cx_sprop(bond, CX_WRL_BONDRADIUS))) return -1.0;
   if ('\0' == *str) return -1.0;
   return cx_rprop(bond, CX_WRL_BONDRADIUS);
}

/*============================================================================
 *  coordinates() -- find "coordinates" of given atom, return success
 */

static int coordinates(cx_Object atom, float *px, float *py, float *pz)
{
   cx_String str;

   if (CX_OB_ATOM == cx_type(atom)) {
      if (NULL != (str = cx_sprop(atom, CX_WRL_COORDINATES)) && *str)
         return (3 == sscanf(str, "%g,%g,%g", px, py, pz));
   }
   return FALSE;
}

/*============================================================================
 *  cx_wrl_setatomcolors() -- set given property to standard atom RGB colors
 */

cx_Integer cx_wrl_setatomcolors(cx_Object mol)
{
   cx_Object atoms, atom;
   int       atno;

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms))) {
      atno = cx_iprop(atom, XX_PROP_ATOMICNUMBER);
      if (0 <= atno || MX_RGB >= atno)
	 cx_set_sprop(atom, CX_WRL_ATOMCOLOR, rgb[atno]);
      else
	 cx_set_sprop(atom, CX_WRL_ATOMCOLOR, rgb[0]);
   }
   cx_destroy(atoms);
   return TRUE;
}

/*============================================================================
 *  cx_wrl_setatomradii() -- set atom radii proportional to standard values
 */

cx_Integer cx_wrl_setatomradii(cx_Object mol, cx_Real factor)
{
   cx_Object atoms, atom;
   float     radius;

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms))) {
      switch (cx_iprop(atom, XX_PROP_ATOMICNUMBER)) {
         case  0:  radius = 0.00;  break;   /* wildcard */
         case  1:  radius = 0.80;  break;   /* H  */
         case  6:  radius = 1.65;  break;   /* C  */
         case  7:  radius = 1.55;  break;   /* N  */
         case  8:  radius = 1.40;  break;   /* O  */
         case  9:  radius = 1.45;  break;   /* F  */
         case 15:  radius = 1.80;  break;   /* P  */
         case 16:  radius = 1.90;  break;   /* S  */
         case 17:  radius = 1.80;  break;   /* Cl */
         case 35:  radius = 1.95;  break;   /* Br */
         case 53:  radius = 2.10;  break;   /* I  */
         default:  radius = 1.50;  break;   /* other */
      }
      cx_set_rprop(atom, CX_WRL_ATOMRADIUS, factor * radius);
   }
   cx_destroy(atoms);
   return TRUE;
}

/*============================================================================
 *  cx_wrl_setbondradii() -- set bond radii to given value
 */

cx_Integer cx_wrl_setbondradii(cx_Object mol, cx_Real radius)
{
   cx_Object bonds, bond;

   bonds = cx_stream(mol, CX_OB_BOND);
   while (NULL != (bond = cx_next(bonds)))
      cx_set_rprop(bond, CX_WRL_BONDRADIUS, radius);
   cx_destroy(bonds);
   return TRUE;
}

/*============================================================================
 *  bond2endpts() -- get (ordered) coords of bond endpoints, return success
 */

static int bond2endpts(cx_Object atom, cx_Object bond,
                       float e1[3], float e2[3])
{
   cx_Object as, a1, a2;

   /*** Get atoms on bond, if atom specified, make it first. ***/

   as = cx_stream(bond, CX_OB_ATOM);
   a1 = cx_next(as);
   a2 = cx_next(as);
   cx_destroy(as);
   if (NULL == a1 || NULL == a2) return FALSE;
   if (a2 == atom) { as = a2; a2 = a1; a1 = as; }

   /*** Get coordinates (endpoints e1, e2). ***/

   if (FALSE == coordinates(a1, &e1[0], &e1[1], &e1[2])) return FALSE;
   if (FALSE == coordinates(a2, &e2[0], &e2[1], &e2[2])) return FALSE;
   return TRUE;
}

/*============================================================================
 *  bond2endmid() -- get coords of bond endpoint, midpoint, return success
 */

static int bond2endmid(cx_Object atom, cx_Object bond,
                       float e1[3], float e2[3])
{
   /*** Get coordinates of atoms on bond (endpoints e1, e2). ***/

   if (FALSE == bond2endpts(atom, bond, e1, e2)) return FALSE;

   /*** Convert endpoint e2 to midpoint. ***/

   e2[0] = (e1[0] + e2[0]) / 2.0;
   e2[1] = (e1[1] + e2[1]) / 2.0;
   e2[2] = (e1[2] + e2[2]) / 2.0;

   return TRUE;
}

/*============================================================================
 *  cx_mat4_identity() -- create an identity matrix
 */

static void cx_mat4_identity(XX_Real m[4][4])
{
   int i, j;

   for (i = 0; i < 4; i++) {
      for (j = 0; j < 4; j++)
         m[i][j] = 0;
      m[i][i] = 1;
   }
}

/*============================================================================
 *  cx_mat4_multiply() -- multiply two matrices
 */

static void cx_mat4_multiply(XX_Real m1[4][4], XX_Real m2[4][4],
                             XX_Real result[4][4])
{
   int    i, j, k;
   XX_Real tmp;

   for (i = 0; i < 4; i++) {
      for (j = 0; j < 4; j++) {
         tmp = 0;
         for (k = 0; k < 4; k++)
            tmp += m1[i][k] * m2[k][j];
         result[i][j] = tmp;
      }
   }
}

/*============================================================================
 *  cx_mat3_cross() -- take the cross product of two 3x3 vectors
 */

static void cx_mat3_cross(XX_Real c[3], XX_Real a[3], XX_Real b[3])
{
   c[0] = a[1] * b[2] - a[2] * b[1];
   c[1] = a[2] * b[0] - a[0] * b[2];
   c[2] = a[0] * b[1] - a[1] * b[0];
}

/*============================================================================
 *  cx_vec3_normalize() -- normalize a vector
 */

static void cx_vec3_normalize(XX_Real v[3])
{
   XX_Real d = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
   v[0] /= d;  v[1] /= d;  v[2] /= d;
}

/*============================================================================
 *  cx_xyz2rotmat() -- calulate rotation matrix to move between vectors
 */

static int cx_xyz2rotmat(XX_Real f[3], XX_Real t[3], XX_Real rotmat[4][4])
{
   int     i;
   XX_Real a[3], g[3], u[3], m1[4][4], m2[4][4];

   /*** Construct unit vectors. ***/

   cx_mat3_cross(a, f, t);  cx_vec3_normalize(a);
   cx_mat3_cross(g, a, f);  cx_vec3_normalize(g);
   cx_mat3_cross(u, a, t);  cx_vec3_normalize(u);

   /*** Construct inverse and transformation matrices. ***/

   cx_mat4_identity(m1);
   cx_mat4_identity(m2);
   for (i = 0; i < 3; i++) {
      m1[i][0] = f[i];  m1[i][1] = a[i];  m1[i][2] = g[i];
      m2[0][i] = t[i];  m2[1][i] = a[i];  m2[2][i] = u[i];
   }

   /*** Construct inverse and transform matrices. ***/

   cx_mat4_identity(m1);
   cx_mat4_identity(m2);
   for (i = 0; i < 3; i++) {
      m1[i][0] = f[i];  m1[i][1] = a[i];  m1[i][2] = g[i];
      m2[0][i] = t[i];  m2[1][i] = a[i];  m2[2][i] = u[i];
   }

   /*** Rotation matrix is product of inverse and transformation matrices. ***/
   
   cx_mat4_multiply(m1, m2, rotmat);
   return 1;
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

#ifdef LATERPERHAPS
/*============================================================================
 *  btuplename() -- return property name of bondtuple with given prefix
 *
 *  Return NULL if not found; warn if there's more than one.
 *  Cute move: string is parented by the tuple property.
 */

static cx_String btuplename(cx_Object mol, cx_String prefix)
{
   cx_Object tuples, tuple;
   char      *tname, ebuf[80], *func = "btuplename";

   /*** Return NULL if either argument is NULL. ***/

   if (NULL == mol || NULL == prefix) return NULL;

   /*** Return NULL if no tuple names start with given prefix. ***/

   if (NULL == (tuples = cx_prefix2btuples(mol, prefix))) return NULL;
   if (NULL == (tuple  = cx_next(tuples))               ) return NULL;

   /*** Get bondtuple name to return. ***/

   tname = cx_bondtuple_name(tuple);

   /*** Warn if more than one exists. ***/

   if (NULL != (tuple = cx_next(tuples))) {
      sprintf(ebuf, "tuple prefix \"%s\" is ambiguous:", prefix);
      cx_error_save(ebuf,                     CX_ERR_WARN, func);
      cx_error_save(tname,                    CX_ERR_WARN, func);
      cx_error_save(cx_bondtuple_name(tuple), CX_ERR_WARN, func);
      while (NULL != (tuple = cx_next(tuples)))
         cx_error_save(cx_bondtuple_name(tuple), CX_ERR_WARN, func);
   }

   /*** Clean up and return first one, or NULL. ***/

   cx_destroy(tuples);
   return tname;

}
#endif

/*============================================================================
 *  addcap() -- draw small sphere at given point
 */

static cx_Integer addcap(FILE *fp, float a[3], float radius)
{
   fprintf(fp,
      "    Separator {\n"
      "      Translation { translation %.3f %.3f %.3f }\n"
      "      Sphere { radius %.3f }\n"
      "    }\n",
      a[0], a[1], a[2], radius);

   return 1;
}

/*============================================================================
 *  addrod() -- draw cylinder between two points
 */

static cx_Integer addrod(FILE *fp, float a[3], float b[3], float radius)
{
   float      h, height;
   XX_Real    o[3], t[3], v[3], tm[4][4];
   int        i, j;

   /*** Original cylinder direction vector (o) is along Y axis. ***/

   o[0] = 0.0;  o[1] = 1.0;  o[2] = 0.0;

   /*** Calculate translation (t), direction vector (v) and height (h2). ***/

   h = 0.0;
   for (i = 0; i < 3; i++) {
      t[i]  = (b[i] + a[i]) / 2.0;
      v[i]  = (b[i] - a[i]) / 1.0;
      h    += v[i] * v[i];
   }
   height = sqrt(h);

   /*** Form rotation matrix. ***/

   cx_vec3_normalize(v);
   cx_xyz2rotmat(o, v, tm);

   /*** Copy translation elements into transform matrix. ***/

   for (i = 0; i < 3; i++)
      tm[3][i] = t[i];

   /*** Global scaling. ***/

   tm[3][3] = 1.0;

   /*** Write transform matrix. ***/

   fprintf(fp, "    Separator {\n");
   fprintf(fp, "      MatrixTransform { matrix\n");

   for (i = 0; i < 4; i++) {
      fprintf(fp, "        ");
      for (j = 0; j < 4; j++)
         fprintf(fp, " %10.5f", tm[i][j]);
      fprintf(fp, "\n");
   }

   fprintf(fp, "      }\n");
   fprintf(fp, "      Cylinder { parts SIDES radius %.3f height %.3f }\n",
           radius, height );
   fprintf(fp, "    }\n");

   return TRUE;
}
      

/*============================================================================
 *
 */

static void dircos(float a[3], float b[3], float dc[3])
{
   float v[3], d;
   v[0] = b[0] - a[0];  v[1] = b[1] - a[1];  v[2] = b[2] - a[2];
   d    = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
   dc[0] = v[0] / d;  dc[1] = v[1] / d;  dc[2] = v[2] / d;
}

/*============================================================================
 *  muperp() -- return direction mutually perpendicular to two given vectors
 */

static void muperp(float dca[3], float dcb[3], float dcp[3])
{
   float d;
   dcp[0]  = dca[1] * dcb[2] - dca[2] * dcb[1];
   dcp[1]  = dca[2] * dcb[0] - dca[0] * dcb[2];
   dcp[2]  = dca[0] * dcb[1] - dca[1] * dcb[0];
   d       = sqrt(dcp[0]*dcp[0] + dcp[1]*dcp[1] + dcp[2]*dcp[2]);
   dcp[0] /= d;
   dcp[1] /= d;
   dcp[2] /= d;
}

/*============================================================================
 *
 */

static cx_Object planebond(cx_Object bond)
{
   cx_Object as, a, bs, b;

   /* Default return value! */
   b = (cx_Object)0;

   /*** Look at atoms `a' on end of given bond. ***/

   as = cx_stream(bond, CX_OB_ATOM);
   while (NULL != (a = cx_next(as))) {

      /*** Find bond `b' other than `bond' connected to end atom. ***/

      bs = cx_stream(a, CX_OB_BOND);
      while (NULL != (b = cx_next(bs)))
         if (b != bond) break;
      cx_destroy(bs);
      if (NULL != b) break;
   }
   cx_destroy(as);

   /*** Return in-plane bond. ***/

   return b;
}

/*============================================================================
 *  hotrod() -- draw stick from a1 to a1-a2 midpoint
 */

static cx_Integer hotrod(FILE *fp, cx_Object a1, cx_Object a2,
                         float radius, float boff)
{
   cx_Object  bond, bond2;
   float      c1[3], c2[3], v1[3], v2[3], dc1[3], dc2[3], dcp[3], p1[3], p2[3];
   int        i, k, border, needcap;

   /*** Endcap(s) needed if atom `a1' is zero size. **/

   needcap = (0.0 == atomradius(a1));

   /*** Get bond. ***/

   if (NULL == (bond = cx_bond(a1, a2))) return FALSE;

   /*** Get coordinates (c1, c2) of atoms a1 and a1-a2 midpoint, resp. ***/

   bond2endmid(a1, bond, c1, c2);

   /*** Get bond order. ***/

   border = cx_iprop(bond, XX_PROP_BONDORDER);

   /*** Single bonds are represented as large central cylinder. ***/

   switch (border) {
   case 1:
      addrod(fp, c1, c2, radius);
      if (needcap) addcap(fp, c1, radius);
      break;

   /*** Double bonds are represented as half-size cylinders. ***/

   case 2:

      dircos(c1, c2, dc1);  /*** Direction cosine of this bond. ***/

      /*** Get endpoints of in-plane bond. ***/

      if (NULL == (bond2 = planebond(bond))) break;
      bond2endpts(NULL, bond2, v1, v2);

      dircos(v1, v2, dc2);  /*** Direction cosine of other bond. ***/

      muperp(dc1, dc2, dcp); /*** mutually perpendicular direction ***/

      /*** Scale normalized perpendicular direction cosines. ***/

      dcp[0] *= boff;  dcp[1] *= boff;  dcp[2] *= boff;

      /*** Draw both ways. ***/

      for (k = -1; k < 2; k += 2) {
         for (i = 0; i < 3; i++) {
            p1[i] = c1[i] + k * dcp[i];
            p2[i] = c2[i] + k * dcp[i];
         }
         /* addrod(fp, p1, p2, radius / 2.0); */
         addrod(fp, p1, p2, radius);
         if (needcap) addcap(fp, p1, radius);
      }
      break;

   /*** Triple bonds. ***/

   case 3:
      bond2endmid(a1, bond, c1, c2);

      dircos(c1, c2, dc1);  /*** Direction cosine of this bond. ***/

      v1[0] = c1[0];  v1[1] = c1[1];  v1[2] = c1[2];

      v2[0] = c1[0];
      v2[1] = c1[2];
      v2[2] = -(v1[0] * v2[0] + v1[1] * v2[1]) / v1[2];

      dircos(v1, v2, dc2);  /*** Direction cosine of other bond. ***/
      muperp(dc1, dc2, dcp); /*** mutually perpendicular direction ***/

      /*** Scale normalized perpendicuilar direction cosines. ***/

      dcp[0] *= boff; dcp[1] *= boff; dcp[2] *= boff;

      /*** Draw three ways. ***/

      for (k = -1; k < 2; k++) {
         for (i = 0; i < 3; i++) {
            p1[i] = c1[i] + k * dcp[i];
            p2[i] = c2[i] + k * dcp[i];
         }
         addrod(fp, p1, p2, radius);
         if (needcap) addcap(fp, p1, radius);
      }
      break;
   }
   return TRUE;
}

/*============================================================================
 *  print_bondpoints()
 */

static void print_bondpoints(FILE *fp, cx_Object bond, int *paye)
{
   int       border, i;
   float     e1[3], e2[3], v1[3], v2[3], p1[3], p2[3], p3[3];
   float     dc1[3];  /* direction (cosines) of given bond          */
   float     dc2[3];  /* direction (cosines) of other in-plane bond */
   float     dcp[3];  /* mutually perpendicular direction (cosines) */
   float     boff;    /* bond offset in perpendicular direction     */
   cx_Object bond2;


   /*** Get order of given bond. ***/

   border = cx_iprop(bond, XX_PROP_BONDORDER);

   /*** Get and record coordinates of endpoint and midpoint. ***/

   bond2endmid(NULL, bond, e1, e2);   /* get end-mid points */
   fprintf(fp, "        %.3f %.3f %.3f,\n", e2[0], e2[1], e2[2]);
   cx_set_iprop(bond, XX_PROP_TMP, ++(*paye));

   /*** If not single bond, record additional points for this bond. ***/

   if (1 != border) {

      /*** Get hither and yon endpoints. ***/

      bond2endpts(NULL, bond, e1, e2);

      /*** Get direction cosines of bond ***/

      dircos(e1, e2, dc1);

      /*** Get endpoints of another in-plane bond. ***/

      if (NULL == (bond2 = planebond(bond))) return;
      bond2endpts(NULL, bond2, v1, v2);

      /*** Calculate a mutually perpendicular direction. ***/

      dircos(v1, v2, dc2);
      muperp(dc1, dc2, dcp);

      /*** Calculate out-of-plane offset. ***/

      boff = (2 == border ? 0.10 : 0.15);
      for (i = 0; i < 3; i++)
         dcp[i] *= boff;

      /*** Next three points are a1-midpoint-a2 on "-" side. ***/

      for (i = 0; i < 3; i++) {
         p1[i] = e1[i] - dcp[i];
         p2[i] = e2[i] - dcp[i];
         p3[i] = 0.5 * (e1[i] + e2[i]) - dcp[i];
      }
      fprintf(fp, "        %.3f %.3f %.3f,\n", p1[0], p1[1], p1[2]);
      fprintf(fp, "        %.3f %.3f %.3f,\n", p3[0], p3[1], p3[2]);
      fprintf(fp, "        %.3f %.3f %.3f,\n", p2[0], p2[1], p2[2]);

      /*** Next three points are a1-midpoint-a2 on "+" side. ***/

      for (i = 0; i < 3; i++) {
         p1[i] = e1[i] + dcp[i];
         p2[i] = e2[i] + dcp[i];
         p3[i] = 0.5 * (e1[i] + e2[i]) + dcp[i];
      }
      fprintf(fp, "        %.3f %.3f %.3f,\n", p1[0], p1[1], p1[2]);
      fprintf(fp, "        %.3f %.3f %.3f,\n", p3[0], p3[1], p3[2]);
      fprintf(fp, "        %.3f %.3f %.3f,\n", p2[0], p2[1], p2[2]);

      /*** Increment count of indexed points. ***/

      *paye += 6;
   }
}

/*============================================================================
 *  wrl_write() - write a VRML entry using fixed internal properties
 */

static cx_Integer wrl_write(FILE *fp, cx_Object mol)
{
   cx_Object  atoms = NULL; /* all atoms */
   cx_Object  bonds = NULL; /* all bonds */
   cx_Object  atom, a, as, aas, bond, bs, atom2s;
   cx_String  color;
   float      x, y, z, caprad, radius, boff, red, green, blue;
   int        aye, border, ib, ia, balls, sticks, lines;

   /*** Set atomprop XX_PROP_TMP to zero for all atoms. ***/

   atoms = cx_stream(mol, CX_OB_ATOM);
   while (NULL != (atom = cx_next(atoms)))
      cx_set_iprop(atom, XX_PROP_TMP, 0);

   /*** Set atomprop XX_PROP_TMP to 1 for all atoms with zero-size bonds. ***/

   bonds = cx_stream(mol, CX_OB_BOND);
   while (NULL != (bond = cx_next(bonds))) {
      if (0.0 == bondradius(bond)) {
         as = cx_stream(bond, CX_OB_ATOM);
         cx_set_iprop(cx_next(as), XX_PROP_TMP, 1);
         cx_set_iprop(cx_next(as), XX_PROP_TMP, 1);
         cx_destroy(as);
      }
   }

   /*** Reset nonzero XX_PROP_TMP atomprop's to one-origin aye index. ***/

   aye = 0;
   cx_reset(atoms);
   while (NULL != (atom = cx_next(atoms)))
      if (cx_iprop(atom, XX_PROP_TMP)) cx_set_iprop(atom, XX_PROP_TMP, ++aye);

   /*** VRML header. ***/

   fprintf(fp, "#VRML V1.0 ascii\n");

   /*** Creator info. ***/

   fprintf(fp, "  Info { string \"%s\" }\n", XX_CREATOR);

   /*** Hint to set background: wireframe, other. ***/

   if (aye)
      fprintf(fp, "DEF BackgroundColor Info { string \"%s\" }\n", XX_BG_WIRE);
   else
      fprintf(fp, "DEF BackgroundColor Info { string \"%s\" }\n", XX_BG_SOLID);
     
   /**** Hint to set preferrred viewer ("examiner" or "walk") ***/

   fprintf(fp, "DEF Viewer Info { string \"%s\" }\n", XX_VIEWER);

   /*** Separator, (molinfo?), Separator. ***/

   fprintf(fp, "Separator {\n");
   fprintf(fp, "  Separator {\n");

   /*** If needed, list coordinates to marked atoms and bonds. ***/

   if (0 < aye) {
      fprintf(fp, "    Coordinate3 {\n");
      fprintf(fp, "      point [\n");

      /*** Loop over atoms, write coordinates in index order. ***/

      cx_reset(atoms);
      while (NULL != (atom = cx_next(atoms))) {
         if (0 < cx_iprop(atom, XX_PROP_TMP)) {
            if (coordinates(atom, &x, &y, &z))
               fprintf(fp, "        %.3f %.3f %.3f,\n", x, y, z);
            else
               fprintf(fp, "        %.3f %.3f %.3f,\n", 0.0, 0.0, 0.0);
         }
      }

      /*** Loop over 0-size bonds, save index, write points. ***/

      cx_reset(bonds);
      while (NULL != (bond = cx_next(bonds)))
         if (0.0 == bondradius(bond)) print_bondpoints(fp, bond, &aye);
      cx_destroy(bonds);

      /*** Done with coordinate list. ***/

      fprintf(fp, "      ]\n");
      fprintf(fp, "    }\n");
   }

   /*** Set all atom flags to FALSE. ***/

   cx_reset(atoms);
   while (NULL != (atom = cx_next(atoms)))
      cx_set_iprop(atom, XX_PROP_FLAG, FALSE);

   /*** Loop over atoms, find first with new color. ***/

   cx_reset(atoms);
   for (color = NULL; NULL != (atom = cx_next(atoms)); color = NULL) {
      if (cx_iprop(atom, XX_PROP_FLAG)) continue;
      color = atomcolor(atom);

      /*** Make sequence of atoms with this color. ***/

      as     = cx_create_sequence(mol);
      atom2s = cx_stream(mol, CX_OB_ATOM);
      while (NULL != (a = cx_next(atom2s))) {
	 if (0 == cx_strcmp(color, atomcolor(a))) {
             cx_append(as, a);
             cx_set_iprop(a, XX_PROP_FLAG, TRUE);
	 }
      }
      cx_destroy(atom2s);

      /*** Establish rgb colors. ***/

      if (NULL == color || 3 != sscanf(color, "%g,%g,%g", &red, &green, &blue))
	 red = green = blue = 0.5;

      /*** Check for atoms as balls, bonds as sticks and/or lines. ***/

      cx_reset(as);
      balls  = FALSE;
      sticks = FALSE;
      lines  = FALSE;
      while (NULL != (atom = cx_next(as))) {
         if (0.0 < atomradius(atom)) balls  = TRUE;
         bs = cx_stream(atom, CX_OB_BOND);
         while (NULL != (bond = cx_next(bs))) {
            radius = bondradius(bond);
            if      (0.0 == radius) lines  = TRUE;
            else if (0.0 <  radius) sticks = TRUE;
         }
         cx_destroy(bs);
      }

      /*** Create material, make specular if solid. ***/

      if (balls) {
         fprintf(fp, "    Material {\n");
         fprintf(fp, "      diffuseColor  %.2f %.2f %.2f\n",
                 red, green, blue);
         fprintf(fp, "      specularColor %.2f %.2f %.2f\n",
                 0.8 + 0.2 * red, 0.8 + 0.2 * green, 0.8 + 0.2 * blue);
         fprintf(fp, "    }\n");
      } else {
         fprintf(fp, "    Material { diffuseColor %.2f %.2f %.2f }\n",
                 red, green, blue);
      }

      /*** Write indexed line set for 0-size half bonds from such atoms. ***/

      if (lines) {
         fprintf(fp, "    IndexedLineSet {\n");
         fprintf(fp, "      coordIndex [\n");

         /*** Loop over atoms of this atomic number. ***/

         cx_reset(as);
         while (NULL != (atom = cx_next(as))) {

            /*** Loop over half-bonds. ***/

            fprintf(fp, "        ");
            ia = cx_iprop(atom, XX_PROP_TMP) - 1;
            bs = cx_stream(atom, CX_OB_BOND);
            while (NULL != (bond = cx_next(bs)))
               if (0.0 == bondradius(bond)) {
                  border = cx_iprop(bond, XX_PROP_BONDORDER);
                  ib     = cx_iprop(bond, XX_PROP_TMP) - 1;
                  if (2 != border)
                     fprintf(fp, "%d, %d, -1, ", ia, ib);
                  if (1 != border) {
                     aas = cx_stream(bond, CX_OB_ATOM);
                     if (atom == cx_next(aas)) {
                        fprintf(fp, "%d, %d, %d, %d, -1, ",
                           ib + 2, ib + 1, ib + 4, ib + 5);
                     } else {
                        fprintf(fp, "%d, %d, %d, %d, -1, ",
                           ib + 2, ib + 3, ib + 6, ib + 5);
                     }
                     cx_destroy(aas);
                  }
               }
            fprintf(fp, "\n");
            cx_destroy(bs);
         }

         /*** Done with this set of bonds (indexed line set). ***/

         fprintf(fp, "      ]\n");
         fprintf(fp, "    }\n");
      }

      /*** Draw half-sticks for bonds of non-zero size. ***/

      if (sticks) {
         cx_reset(as);
         while (NULL != (atom = cx_next(as))) {
            bs     = cx_stream(atom, CX_OB_BOND);
            caprad = 0.0;
            while (NULL != (bond = cx_next(bs))) {
               if (0.0 < (radius = bondradius(bond))) {
                  if (caprad < radius) caprad = radius;
                  border = cx_iprop(bond, XX_PROP_BONDORDER);
                  boff   = (2 == border ? radius : 2 * radius);
                  hotrod(fp, atom, cx_xatom(atom, bond), radius, boff);
               }
            }
            cx_destroy(bs);
         }
      }

      /*** Also, if balls are on, loop over atoms again drawing balls. ***/

      if (balls) {
         cx_reset(as);
         while (NULL != (atom = cx_next(as))) {

            /*** Create atom spheres as needed. ***/
      
            if (0.0 < (radius = atomradius(atom))) {
               if (coordinates(atom, &x, &y, &z)) {
                  fprintf(fp,
                     "    Separator {\n"
                     "      Translation { translation %.3f %.3f %.3f }\n"
                     "      Sphere { radius %.3f }\n"
                     "    }\n",
                     x, y, z, radius);
               }
            }
         }
      }

   } /* done with this material */

   /*** Tidy up and return successfully. **/

   fprintf(fp, "  }\n");
   fprintf(fp, "}\n");

   cx_destroy(atoms);
   return TRUE;
}

/*============================================================================
 *  cx_wrl_write() -- write standard VRML model to stream
 */

cx_Integer cx_wrl_write(FILE *fp, cx_Object mol, cx_Integer model)
{
   cx_Object ob, obs;
   cx_String str, func = "wrl_write";
   float     arad, brad;

   /*** Don't do any setup if called with model NONE. ***/

   if (CX_WRL_NONE != model) {

      /*** Put coordinates in local property. ***/
    
      if (NULL == (str = atuplename(mol, "coordinates"))) {
         cx_error_save("no coordinates found", CX_ERR_WARN, func);
         return FALSE;
      }

      obs = cx_stream(mol, CX_OB_ATOM);
      while (NULL != (ob = cx_next(obs)))
         cx_set_sprop(ob, CX_WRL_COORDINATES, cx_sprop(ob, str));

      /*** Setup std model (CX_WRL_ATOMRADIUS, CX_WRL_BONDRADIUS). ***/

      switch (model) {
         default:
         case CX_WRL_WIREFRAME:  arad = 0.00; brad = 0.00; break;
         case CX_WRL_STICKS:     arad = 0.00; brad = 0.10; break;
         case CX_WRL_BALLSTICK:  arad = 0.25; brad = 0.10; break;
         case CX_WRL_SPACEFILL:  arad = 1.00; brad = 0.00; break;
      }
      cx_wrl_setatomradii(mol, arad);
      cx_wrl_setbondradii(mol, brad);

      /*** Set up standard atom colors. ***/

      cx_wrl_setatomcolors(mol);

      /*** No default bond colors (implies half-bonds). ***/

      obs = cx_stream(mol, CX_OB_BOND);
      while (NULL != (ob = cx_next(obs)))
         cx_set_sprop(ob, CX_WRL_BONDCOLOR, NULL);
   }

   /*** Call general function. ***/

   return wrl_write(fp, mol);
}
