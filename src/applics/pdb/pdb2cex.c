/*****************************************************************************
*  pdb2cex.c -- application converts .pdb to .cex
*
*----------------------------------------------------------------------------
*
*  pdb2cex [ -t datatypes.cex ] [ in.pdb [ out.cex ] ]
*  cex2pdb [ [ in.cex ] out.pdb ]
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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "cx.h"
#include "cx_molecule.h"
#include "cx_pdb.h"

/*** Handy macros. ***/

#define OUCH(msg) cx_error_save(msg, CX_ERR_FATAL, *av)
#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE,  *av)

/*============================================================================
 *  proginit() -- initialize cx packages, interpret arguments, return files
 *
 *  On error, writes usage summary and exits.  Any return is successful.
 */
 
static void proginit(int *pac, char **av, FILE **ppin, cx_Object *outs)
{
   int        ia, ac = *pac;
   char      *oops, *oops2, *fnin, *fnout, *fndts;
   cx_Object  indts;
 
   /*** Set default values. ***/
 
   fnin  = NULL;  fnout  = NULL;      /* NULL file names mean default */
   fndts = NULL;  indts  = NULL;      /* deal with datatypes locally */
   cx_cex_set_listfmt(CX_FMT_LIST);   /* deal with format locally */
   oops = oops2 = NULL;               /* NULL oops' mean no errors */
 
   /*** Deal with command line. ***/
 
   for (ia = 1; ia < ac; ia++) {
      if ('-' != *av[ia]) {
         if      (NULL == fnin ) fnin  = av[ia];
         else if (NULL == fnout) fnout = av[ia];
	 else { oops = "too many files specified"; oops2 = av[ia]; }
      } else if ('i' ==  av[ia][1]) {
         if (NULL == fnin ) fnin  = av[++ia];
	 else { oops = "input file specified twice"; oops2 = av[ia]; }
      } else if ('o' ==  av[ia][1]) {
         if (NULL == fnout ) fnout  = av[++ia];
	 else { oops = "output file specified twice"; oops2 = av[ia]; }
      } else if ('t' ==  av[ia][1]) {
         if (NULL == fndts ) fndts  = av[++ia];
	 else { oops = "datatypes file specified twice"; oops2 = av[ia]; }
      } else if ('f' ==  av[ia][1]) {
	 switch (av[ia][2]) {
	    case 'd':  cx_cex_set_listfmt(CX_FMT_DUMP);  break;
	    case 'l':  cx_cex_set_listfmt(CX_FMT_LIST);  break;
	    case 'r':  cx_cex_set_listfmt(CX_FMT_RAW );  break;
	    default:   oops = "unknown -f(ormat)"; oops2 = av[ia]; break;
         }
      } else {
         oops = "unknown option"; oops2 = av[ia];
      }
   }
 
   /*** If ok so far, open input and output files. ***/

   if (!oops) {
      *ppin = (NULL == fnin ? stdin : fopen(fnin, "r"));
      if (NULL == ppin) { oops = "can't open input file"; oops2 = fnin; }
 
      if (NULL == fnout) fnout = "-";
      *outs = cx_create_iostream(fnout, CX_IO_WRITE);
      if (NULL == *outs) { oops = "can't open output file"; oops2 = fnout; }
   }
 
   /*** If still ok, read datatypes into default datatype table. ***/

   if (!oops) {
      if (NULL == fndts) fndts = "$CX_ROOT/data/datatypes.cex";
      indts = cx_create_iostream(fndts, CX_IO_READ);
      if (NULL == indts) { oops = "can't open datatypes file"; oops2 = fndts; }
      cx_next(indts);
      cx_destroy(indts);
   }

   /*** Initialize known packages. ***/

   cx_molecule_pkg();

   /*** Exit with usage summary on syntax error. ***/

   if (oops) {
      OUCH(oops);
      OUCH(oops2);
      NOTE("");
      NOTE("Usage:  pdb2cex [options] [in.pdb [out.cex]]");
      NOTE("");
      NOTE("Options:");
      NOTE("  -fd ............... write dump format (eot newlines)");
      NOTE("  -fl ............... write list format (eod & eot newlines)");
      NOTE("  -fr ............... write raw format (no newlines)");
      NOTE("  -i in.pdb ......... specify .pdb input file ");
      NOTE("  -o out.cex ........ specify .cex output file");
      NOTE("  -t datatypes.cex .. specify .cex datatypes file");
      NOTE("");
      NOTE("  If in.pdb  unspecified, input from standard input.");
      NOTE("  If out.cex unspecified, output to standard output.");
      NOTE("  If no datatypes.cex, PDB datatypes are generated.");
      NOTE("  If format  unspecified, list  format is used (-fl).");
      cx_error_spew(stderr, NULL);
      exit(1);
   }
}


/*============================================================================
 *  main program
 */
 
int main(int argc, char *argv[])
{
   cx_Object  obj, dt, outs;
   cx_String  dt_tag;
   FILE      *fpin;

   /*** Deal with command line; some missions never return. ***/

   proginit(&argc, argv, &fpin, &outs);

   /*** Loop over pdb file, reading objects until EOF. ***/

   do {

      obj = cx_pdb_read(fpin);

      /*** If successful, write output. ***/

      if (NULL != obj) {
	 switch (cx_type(obj)) {
	   case CX_OB_MOLECULE:
              /* cx_mol_addimph(mol); */

/*
 * smi = cx_stringvalue(mol);
 * str = cx_sprop(mol, "molname");
 * printf("%s %s\n", smi, (str ? str : "no name"));
 */

	      dt_tag = "MOL";
	      break;

	   case CU_OB_CAMERA:
	      cu_camera_create_datatypes(NULL);
	      dt_tag = CU_TAG_CAMERA;
	      break;

	   default:
	      dt_tag = NULL;
	      break;
	 }

	 /*** Assign datatype to object and write as cex. ***/


	 if (NULL != dt_tag && NULL != (dt = cx_tag2datatype(NULL, dt_tag))) {
	    cx_set_datatype(obj, dt);
	    cx_append(outs, obj);
	 }

         /*** Deallocate object. ***/

         cx_destroy(obj);
      }

      /*** Spew errors for good measure. ***/

      cx_error_spew(stderr, NULL);

   /*** Terminate on EOF. ***/
   
   } while (FALSE == cx_pdb_eof());

   /*** Clean up and exit with 0. ***/

   cx_destroy(outs);
   fclose(fpin);
   cx_cleanup();
   return 0;
}
