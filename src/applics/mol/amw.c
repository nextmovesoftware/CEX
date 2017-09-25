/*****************************************************************************
*  amw.c -- CEX average molecular weight generator
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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "cx.h"
#include "cx_molecule.h"

#define MX_ELEM 105

/*** aaw -- average atomic weight data ***/

static double aaw[MX_ELEM + 1] = {
   0.000, 1.008, 4.0026, 6.94, 9.01218, 10.81, 12.011, 14.0067, 15.9994,
   18.99846, 20.17, 22.98977, 24.305, 26.9815, 28.086, 30.9738, 32.06,
   35.453, 39.948, 39.1, 40.08, 44.9559, 47.9, 50.941, 51.996, 54.938,
   55.847, 58.9332, 58.71, 63.543, 65.38, 69.72, 72.59, 74.9216, 78.96,
   79.904, 83.8, 85.467, 87.62, 88.9059, 91.22, 92.9064, 95.94, 98.9062,
   101.07, 102.9055, 106.4, 107.868, 112.4, 114.82, 118.69, 121.75, 127.6,
   126.9045, 131.3, 132.9055, 137.34, 138.9055, 140.12, 140.9077, 144.24,
   145., 150.4, 151.96, 157.25, 158.9254, 162.5, 164.9303, 167.26, 168.9342,
   173.04, 174.97, 178.49, 180.947, 183.85, 186.2, 190.2, 192.22, 195.09,
   196.9665, 200.59, 204.37, 207.2, 208.9806, 209., 210., 222., 223., 226.,
   227., 232.0381, 231.0359, 238.029, 237.0408, 244., 243., 247., 247.,
   251., 254.09, 257.08, 258.098, 259.10, 260.1, 261.1, 262.11
};

/*** prec -- precision, as places past decimal point ***/

static int prec[MX_ELEM + 1] = { 6,
   3,4,2,5,2,3,4,4,5,2,5,3,4,3,4,2,3,3,1,2,4,1,3,3,3,3,4,2,3,2,2,2,4,2,3,
   1,3,2,4,2,4,2,4,2,4,1,3,1,2,2,2,1,4,1,4,2,4,2,4,2,0,1,2,2,4,1,4,2,4,2,
   2,2,3,2,1,1,2,2,4,2,2,1,4,0,0,0,0,0,0,4,4,3,4,0,0,0,0,0,2,2,3,2,1,1,2
};
  
/*** Handy macro. ***/

#define NOTE(msg)  cx_error_save(msg, CX_ERR_NOTE,  *argv)
#define FATAL(msg) cx_error_save(msg, CX_ERR_FATAL, *argv)

/*============================================================================
 *  amwof() -- calculate average molecular weight of molecule
 *
 *  This function ignores atomic mass and accounts for implicit hydrogens.
 */

static cx_String amwof(cx_Object mol)
{
   static char buf[20];
   int         atno, imph, p = 6;
   double      amw = 0.0;
   cx_Object   atom, atoms = cx_stream(mol, CX_OB_ATOM);

   /*** Loop over atoms, adding up molecular weight. ***/

   while (NULL != (atom = cx_next(atoms))) {
     atno = cx_iprop(atom, "atomic number");

     /*** Add atomic weight of heavy atoms, track precision. ***/

     if (0 < atno && MX_ELEM >= atno) {
        amw += aaw[atno];
        p    = CX_MIN(p, prec[atno]);
     }

     /*** Add atomic weight of non-explicit hydrogens. ***/

     if (0 < (imph = cx_iprop(atom, "implicit hcount"))) {
        amw += imph * aaw[1];
        p    = CX_MIN(p, prec[1]);
     }
   }
   cx_destroy(atoms);

   /*** Write ave mol wt to correct precision and return it. ***/

   sprintf(buf, "%.*f", p, amw);
   return (buf);
}

/*============================================================================
 *  main() for amw
 */

int main(int argc, char **argv)
{
   cx_Object ob, ins, outs;

   /*** This program takes no options -- show usage if any specified. ***/

   if (1 < argc) {
      FATAL("This program doesn't accept options or arguments.");
      NOTE ("usage:   cex input | amw | cex output"            );
      NOTE ("Adds AMW (ave mol wt) to molecules on cex stream" );
      cx_error_spew(stderr, NULL);
      exit(0);
   }

   /*** Initialize molecule package. ***/

   cx_molecule_pkg();

   /*** Create AMW datatype in default datatype table. ***/

   cx_create_datatype(NULL, "AMW", "ave mol wt", "Ave. Mol. Wt.",
                      "1",  "REAL", "Average molecular weight");

   /*** Create input and output streams. ***/

   ins  = cx_create_iostream("-", CX_IO_READ );
   outs = cx_create_iostream("-", CX_IO_WRITE);

   /*** Loop over objects on standard input and output streams. ***/

   while (NULL != (ob = cx_next(ins))) {

      /*** Calculate AMW for molecules, add as property. ***/

      if (CX_OB_MOLECULE == cx_type(ob))
         cx_set_sprop(ob, "ave mol wt", amwof(ob));

      /*** Send object to stdout. ***/

      cx_append(outs, ob);

      /*** Clean up. ***/

      cx_error_spew(stderr, NULL);
      cx_destroy(ob);
   }

   /*** DON'T: Transmit remaining datatypes. (This was only a test!) ***/

   /* cx_append(outs, cx_default_datatypetable()); */

   /*** Clean up, close files, and exit with 0. ***/

   cx_error_spew(stderr, NULL);
   cx_destroy(outs);
   cx_cleanup();
   return 0;
}
