/*****************************************************************************
*  testpdbout.c -- test cx_pdb_write()
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

#include <stdio.h>
#include <string.h>
#include "cx.h"
#include "cx_molecule.h"
#include "cx_pdb.h"

/*============================================================================
 *  main() non-standard test program
 */

int main(int argc, char **argv)
{
   int        ok, verb = 3;
   char      *p, smi[1000];
   cx_Object  mol;

   /*** Initialize. ***/

   if (2 == argc && '-' == *argv[1]) {
      switch (argv[1][1]) {
      case '0':
	 verb = 3;
	 break;
      case '1': case '2': case '3': case '4': case '5': case '6': case '7':
	 verb = argv[1][1] - '0';
	 break;
      default:
	 fprintf(stderr, "usage: %s [-01234567]\n", *argv);
	 exit(1);
      }
   }

   /*** Initialize known packages. ***/

   cx_molecule_pkg();

   /*** Read input. ***/

   while (NULL != (fgets(smi, 1000, stdin))) {
      if (NULL != (p = strchr(smi, '\n'))) *p = '\0';
/* printf("got: %s\n", smi); */
      mol = cx_smilin(NULL, smi);
      if (mol) ok = cx_pdb_write(stdout, mol, cx_sprop(mol, "molname"),
				 "remark", NULL, "resname", "chain", "resno",
				 "x", "occupancy", "bvalue", NULL, NULL);
/* if (mol) { printf("mol: %p\n", mol); cx_spewob(stdout, mol, verb); }; */
      cx_error_spew(stdout, NULL);
   }

   exit(0);
}
