/*****************************************************************************
*  testpdbin.c -- test pdb reading
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
#include "cx.h"
#include "cx_molecule.h"
#include "cx_pdb.h"

/*============================================================================
 *  test program
 */
 
main(int argc, char **argv)
{
   cx_Object  mol, dt, ins;
   cx_String  smi, str, fn = NULL;
   int        verb = 0;
   FILE      *fpin;

   /*** Deal with command line. ***/

   if (2 == argc) {
      fn   = argv[1];
   } else if (3 == argc && '-' == *argv[1]) {
      verb = cx_atoi(argv[1] + 1);
      fn   = argv[2];
   } else {
      fn   = NULL;
/*
   } else if (2 != argc) {
      cx_error_save("oops, need one argument, file.pdb", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
*/
   }

   /*** Initialize known packages. ***/

   cx_molecule_pkg();

   /*** Read datatypes into default table. ***/

   ins = cx_create_iostream("$CX_ROOT/data/datatypes.cex", CX_IO_READ);
   cx_next(ins);
   /* cx_spewob(stdout, cx_dt_table_default(), 3); */
   cx_destroy(ins);

   /*** Open input file. ***/

   if (NULL == fn) {
      fpin = stdin;
   } else if (NULL == (fpin = fopen(fn, "r"))) {
      cx_error_save("can't open input file", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Loop over pdb file. ***/

   do {

      /*** Read next pdb entry. ***/

      mol = cx_pdb_read(fpin);

      /*** If successful, write output. ***/

      if (NULL != mol) {
         /* cx_mol_addimph(mol); */
         smi = cx_stringvalue(mol);
         str = cx_sprop(mol, "molname");
         printf("%s %s\n", smi, (str ? str : "no name"));
         if (verb) {
	    dt = cx_e_tag2datatype(NULL, "MOL");
	    cx_set_datatype(mol, dt);
	    cx_spewob(stdout, mol, verb);
         }

      }

      /*** Deallocate molecule. ***/

      cx_destroy(mol);

      /*** Spew errors for good measure. ***/

      cx_error_spew(stderr, NULL);

   /*** Terminate on EOF. ***/
   
   } while (FALSE == cx_pdb_eof());

   cx_cleanup();
   fclose(fpin);
   exit(0);
}
