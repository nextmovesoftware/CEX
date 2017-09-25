/*****************************************************************************
*  trymol.c -- a non-standard test program
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

/*============================================================================
 *  molbug() -- add tuples for molecule innards
 */

static char *tm[] =  {
  "_MI", CX_PROP_INSMI,  "Input SMILES",    "1",  "STRING", "insmi",
  NULL
};

static char *ta[] =  {
  "_AI", CX_PROP_IMPH,   "Implicit hcount", "1A", "INTEGER", "atom imph",
  "_AN", CX_PROP_ATNUMB, "Atomic number"  , "1A", "INTEGER", "atom number",
  "_AS", CX_PROP_ATSYMB, "Atomic symbol"  , "1A", "STRING" , "atom symbol",
  "_AM", CX_PROP_MASS,   "Atom mass"      , "1A", "INTEGER", "atom mass",
  "_AC", CX_PROP_CHARGE, "Atom charge"    , "1A", "INTEGER", "atom charge",
  "_AL", CX_PROP_CHIRAL, "Atom chirality" , "1A", "STRING" , "atom chirality",
  "_Al", "label",        "Label"          , "1A", "INTEGER", "label",
  NULL
};

static char *tb[] =  {
  "_BO", CX_PROP_BORDER, "Bond order",      "1B", "INTEGER", "bond order",
  "_BS", CX_PROP_BSYMB,  "Bond symbol",     "1B", "STRING",  "bond symbol",
  "_BS", CX_PROP_DBO,    "Bond dbo",        "1B", "STRING",  "bond dbo",
  NULL
};

static void molbug(cx_Object mol, cx_Object tab)
{
   char **p;
   static pass0 = TRUE;

   for (p = tm; *p; p += 6)
      cx_create_datatype(tab, p[0], p[1], p[2], p[3], p[4], p[5]);

   for (p = ta; *p; p += 6)
      cx_set_datatype(
         cx_create_atomtuple(mol, p[1]),
         cx_create_datatype(tab, p[0], p[1], p[2], p[3], p[4], p[5]) ); 

   for (p = tb; *p; p += 6)
      cx_set_datatype(
         cx_create_bondtuple(mol, p[1]),
         cx_create_datatype(tab, p[0], p[1], p[2], p[3], p[4], p[5]) );
}

/*============================================================================
 *  main() non-standard test program
 */

int main(int argc, char **argv)
{
   int        verb = 7;
   char      *p, smi[1000];
   cx_Object  mol, copy, dt, ins;

   /*** Initialize. ***/

   if (2 == argc && '-' == *argv[1]) {
      verb = cx_atoi(argv[1] + 1);
      printf("verb = %d\n");
   }

   cx_molecule_pkg();

   /*** Read in datatypes ***/

   ins = cx_create_iostream("$CX_ROOT/data/datatypes.cex", CX_IO_READ);
   if (NULL == ins) {
      cx_error_save("can't open datatypes.cex", NULL, NULL);
      cx_error_spew(stderr, NULL);
      exit(1);
   } else {
      cx_next(ins);
      cx_destroy(ins);
   }
 
   /*** Get MOL datatype to attach to molecule. ***/

   dt = cx_tag2datatype(NULL, "MOL");

   /*** Read input. ***/

   while (printf("XSMI: ") && NULL != (fgets(smi, 1000, stdin))) {
      if (NULL != (p = strchr(smi, '\n'))) *p = '\0';
      printf("got: %s\n", smi);
      mol = cx_smilin(NULL, smi);
      if (mol) {

	 /*** Attach properties to molecule for debugging and show it. ***/

         cx_set_datatype(mol, dt);
         molbug(mol, NULL);
         printf("got molecule from cx_smilin():\n");
	 cx_spewob(stdout, mol, verb);

	 /*** Make heavycopy and show that. ***/

         copy = cx_mol_heavycopy(mol);
	 molbug(copy, NULL);
         cx_set_datatype(copy, dt);
         printf("after cx_mol_heavycopy(mol):\n");
	 cx_spewob(stdout, copy, verb);

	 /*** Set implicit hydrogens and display. ***/

	 cx_mol_setimph(copy);
         printf("after cx_setimph(), copy:\n");
	 cx_spewob(stdout, copy, verb);

	 /*** Make explicit hydrogens and display. ***/

	 cx_mol_addhall(copy);
         printf("after cx_addhall(), copy:\n");
	 cx_spewob(stdout, copy, verb);

	 /*** Make implicit hydrogens and display. ***/

	 cx_mol_zaph(copy);
         printf("after cx_mol_zaph(), copy:\n");
	 cx_spewob(stdout, copy, verb);

	 /*** Clean up. ***/

	 cx_destroy(copy);
	 cx_destroy(mol);
      }
      cx_error_spew(stdout, NULL);
   }

   cx_cleanup();
   printf("\nBye.\n");
   exit(0);
}
