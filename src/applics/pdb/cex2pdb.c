/*****************************************************************************
*  cex2pdb.c -- application converts .pdb to .cex
*
*----------------------------------------------------------------------------
*
*  cex2pdb [ -t datatypes.cex ] [ in.pdb [ out.cex ] ]
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
#include "cu_camera.h"
#include "cx_pdb.h"

/*** Handy macros. ***/

#define OUCH(msg) cx_error_save(msg, CX_ERR_FATAL, *av)
#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE,  *av)

/*============================================================================
 *  proginit() -- initialize cx packages, interpret arguments, return files
 *
 *  On error, writes usage summary and exits.  Any return is successful.
 */
 
static void proginit(int *pac, char **av, cx_Object *ins, FILE **ppout,
		     cx_String *nam,    cx_String *rem,
		     cx_String *pnam,   cx_String *prem,   cx_String *palabs,
		     cx_String *presid, cx_String *pchain, cx_String *presno,
		     cx_String *pcoord, cx_String *poccup, cx_String *pbvalu,
		     cx_String *acolor, cx_String *arad)
{
   int        ia, ac = *pac;
   char      *oops, *oops2, *fnin, *fnout, *fndts, *pold, *pnew;
   cx_Object  indts;
 
   /*** Set default values for control arguments. ***/
 
   fnin  = NULL;  fnout  = NULL;      /* NULL file names mean default */
   fndts = NULL;  indts  = NULL;      /* deal with datatypes locally */
   oops = oops2 = NULL;               /* NULL oops' mean no errors */

   /*** Set default for cx_pdb_write() arguments. ***/

   *nam    = NULL;
   *rem    = NULL;
   *pnam   = "molname";
   *prem   = "remark";
   *palabs = "atomname";
   *presid = "resnam";
   *pchain = "chain";
   *presno = "resno";
   *pcoord = "coordinates";
   *poccup = "occupancy";
   *pbvalu = "bvalue";
   *acolor = "atom color";
   *arad   = "atom covalent radius";
 
   /*** Deal with command line. ***/
 
   for (ia = 1; ia < ac; ia++) {

      /** Extract input or output file name as argument. ***/

      if ('-' != *av[ia]) {
         if      (NULL == fnin ) fnin  = av[ia];
         else if (NULL == fnout) fnout = av[ia];
	 else { oops = "too many files specified"; oops2 = av[ia]; }

      /** Extract input file name as -i option. ***/

      } else if ('i' ==  av[ia][1]) {
         if (NULL == fnin ) fnin  = av[++ia];
	 else { oops = "input file specified twice"; oops2 = av[ia]; }

      /** Extract output file name as -o option. ***/

      } else if ('o' ==  av[ia][1]) {
         if (NULL == fnout ) fnout  = av[++ia];
	 else { oops = "output file specified twice"; oops2 = av[ia]; }

      /** Extract datatypes file name as -t option. ***/

      } else if ('t' ==  av[ia][1]) {
         if (NULL == fndts ) fndts  = av[++ia];
	 else { oops = "datatypes file specified twice"; oops2 = av[ia]; }

      /** Extract name and remark formats as -n and -r options. ***/

      } else if ('n' ==  av[ia][1]) {
	 *nam = av[++ia];
	 if (NULL == *nam) {
	    oops  = "option missing value";
	    oops2 = "-n <namefmt>";
	 } else if (60 < (int) strlen(*nam)) {
	    oops  = "-n namefmt > 60 chars max";
	    oops2 = *nam;
	 }

      } else if ('r' ==  av[ia][1]) {
	 *rem = av[++ia];
	 if (NULL == *rem) {
	    oops  = "option missing value";
	    oops2 = "-r <remarkfmt>";
	 } else if (60 < (int) strlen(*rem)) {
	    oops = "-r remarkfmt > 60 chars max";
	    oops2 = *rem;
         }

      /** Extract property mapping. ***/

      } else if ('m' ==  av[ia][1]) {

	 /*** Get oldprop-newprop arguments. ***/

	 if (NULL == (pold = av[++ia])) {
	    oops  = "missing required -m option values";
	    oops2 = "-m <oldprop> <newprop>";
	 } else if (NULL == (pnew = av[++ia])) {
	    oops  = "missing 2nd required -m option value";
	    oops2 = "-m <oldprop> <newprop>";
	 } 

	 /*** Look for known properties and assign if found. ***/
	 else if (0 == cx_strcmp(pold, "molname"    )) *pnam   = pnew;
	 else if (0 == cx_strcmp(pold, "remark"     )) *prem   = pnew;
	 else if (0 == cx_strcmp(pold, "atomname"   )) *palabs = pnew;
	 else if (0 == cx_strcmp(pold, "resnam"     )) *presid = pnew;
	 else if (0 == cx_strcmp(pold, "chain"      )) *pchain = pnew;
	 else if (0 == cx_strcmp(pold, "resno"      )) *presno = pnew;
	 else if (0 == cx_strcmp(pold, "coordinates")) *pcoord = pnew;
	 else if (0 == cx_strcmp(pold, "occupancy"  )) *poccup = pnew;
	 else if (0 == cx_strcmp(pold, "bvalue"     )) *pbvalu = pnew;

         /*** Generate error if unexpected property name is encountered. ***/

	 else { oops  = "unknown pdb property:"; oops2 = pold; } 

      /*** Generate error if unexpected option is encountered. ***/

      } else {
         oops = "unknown option"; oops2 = av[ia];
      }
      if (oops) break;
   }

   /*** If ok so far, open input and output files. ***/

   if (!oops) {
      if (NULL == fnin) fnin = "-";
      *ins = cx_create_iostream(fnin, CX_IO_READ);
      if (NULL == *ins) { oops = "can't open input file"; oops2 = fnin; }
 
      *ppout = (NULL == fnout ? stdout : fopen(fnout, "w"));
      if (NULL == *ppout) { oops = "can't open output file"; oops2 = fnout; }
   }

   /*** If still ok and datatypes file specified, read into default table. ***/

   if (!oops && NULL != fndts) {
      indts = cx_create_iostream(fndts, CX_IO_READ);
      if (NULL == indts) { oops = "can't open datatypes file"; oops2 = fndts; }
      cx_next(indts);
      cx_destroy(indts);
   }

   /*** Initialize known packages. ***/

   cx_molecule_pkg();
   cu_camera_pkg();

   /*** On error, exit with usage summary. ***/

   if (oops) {
      OUCH(oops);
      OUCH(oops2);
      NOTE("");
      NOTE("Usage:  cex2pdb [options] [in.cex [out.pdb]]");
      NOTE("");
      NOTE("Options specifying files:");
      NOTE("  -i in.cex .............. specify .cex input file ");
      NOTE("  -o out.pdb ............. specify .pdb output file");
      NOTE("  -t datatypes.cex ....... specify .cex datatypes file");
      NOTE("");
      NOTE("  If in.cex  unspecified, input from standard input.");
      NOTE("  If out.cex unspecified, output to standard output.");
      NOTE("  If no datatypes.cex, datatypes are read from input.");
      NOTE("");
      NOTE("Options specifying fields:");
      NOTE("  -n <namefmt> ........... specify name (COMPND field)");
      NOTE("  -r <remarkfmt> ......... specify remark (REMARK field)");
      NOTE("");
      NOTE("  \"%d\" in <namefmt> or <remarkfmt> gets serial number.");
      NOTE("");
      NOTE("Options mapping input properties to output fields:"   );
      NOTE("  -m molname <prop> ...... map prop to molecule name" );
      NOTE("  -m remark <prop> ....... map prop to remark"        );
      NOTE("  -m atomname <prop> ..... map prop to atom name*"    );
      NOTE("  -m resnam <prop> ....... map prop to residue name"  );
      NOTE("  -m chain <prop> ........ map prop to chain id"      );
      NOTE("  -m resno <prop> ........ map prop to residue number");
      NOTE("  -m coordinates <prop> .. map prop to coordinates"   );
      NOTE("  -m occupancy <prop> .... map prop to occupancy"     );
      NOTE("  -m bvalue <prop> ....... map prop to B-value"       );
      NOTE("");
      NOTE("  *special case: \"-m atomname PDB\" invents PDB-style names");
      cx_error_spew(stderr, NULL);
      exit(1);
   }
}


/*============================================================================
 *  main program
 */
 
int main(int argc, char *argv[])
{
   cx_Object  ob, ins;
   FILE      *fpout;
   char       nambuf[80], rembuf[80];
   cx_String  xnam, xrem, name, remark, usenam, userem;
   cx_String  alabs, resid, chain, resno, coord, occup, bvalu;
   cx_String  acolor, arad;
   int        nin = 0;

   /*** Deal with command line; some missions never return. ***/

   proginit(&argc, argv, &ins, &fpout, &xnam, &xrem, &name, &remark,
	    &alabs, &resid, &chain, &resno, &coord, &occup, &bvalu,
	    &acolor, &arad);

   /*** Special case: if alabs is "PDB", invent PDB-style atom names. ***/

   if (0 == cx_strcmp("PDB", alabs)) alabs = NULL;

   /*** Read objects from cex input stream; datatypes never appear. ***/
    
   while (NULL != (ob = cx_next(ins))) {

      switch (cx_type(ob)) {

      /*** If ob is a camera, write out a pdbrun header. ***/

      case CU_OB_CAMERA:
	 cu_camera2pdb(fpout, ob);
         cx_destroy(ob);
	 break;

      /*** If molecule, write it as pdb. ***/

      case CX_OB_MOLECULE:
      
	 /*** Increment molecule count. ***/

         nin++;

	 /*** Possibly set name. ***/
      
	 if (xnam) { sprintf(nambuf, xnam, nin);  usenam = nambuf; }
         else      { usenam = cx_sprop(ob, name);                  }

	 /*** Possibly set remark. ***/

         if (xrem) { sprintf(rembuf, xrem, nin);  userem = rembuf; }
         else      { userem = cx_sprop(ob, remark);                }

         /*** Write pdb. ***/

         cx_pdb_write(fpout, ob, usenam, userem,
                      alabs, resid, chain, resno, coord, occup, bvalu,
		      acolor, arad);

         /*** Clean up after this molecule: destroy it and spew errors. ***/

         cx_destroy(ob);
         cx_error_spew(stderr, NULL);

      /*** Ignore other object types. ***/

      default:
	 break;
      }
   }

   /*** Spew errors to stderr. ***/
 
   cx_error_spew(stderr, NULL);

   /*** Clean up and exit with 0. ***/

   cx_destroy(ins);
   fclose(fpout);
   cx_cleanup();
   return 0;
}
