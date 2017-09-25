/*****************************************************************************
*  cex2wrl.c -- application converts .cex to .wrl
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
/* #include "cu_camera.h" */
#include "cx_wrl.h"

/*** Handy macros. ***/

#define OUCH(msg) cx_error_save(msg, CX_ERR_FATAL, *av)
#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE,  *av)

/*============================================================================
 *  proginit() -- initialize cx packages, interpret arguments, return files
 *
 *  On error, writes usage summary and exits.  Any return is successful.
 */
 
static void proginit(int ac, char **av, cx_Object *ins, FILE **ppout,
		     cx_Integer *model)
{
   int   ia;
   char *oops, *oops2, *fnin, *fnout, *pm;
 
   /*** Set default values for control arguments. ***/
 
   fnin = fnout = NULL;       /* NULL file names mean default */
   oops = oops2 = NULL;       /* NULL oops' mean no errors    */
  *model = CX_WRL_WIREFRAME;  /* Default model is wireframe   */

   /*** Deal with command line. ***/
 
   for (ia = 1; ia < ac; ia++) {

      /** Extract input or output file name as argument. ***/

      if ('-' != *av[ia]) {
         if      (NULL == fnin ) fnin  = av[ia];
         else if (NULL == fnout) fnout = av[ia];
	 else { oops = "too many files specified"; oops2 = av[ia]; }

      /** Extract input file name as -i option. ***/

      } else if ('i' ==  av[ia][1]) {
         if (NULL == fnin) fnin  = av[++ia];
	 else { oops = "input file specified twice"; oops2 = av[ia]; }

      /** Extract output file name as -o option. ***/

      } else if ('o' ==  av[ia][1]) {
         if (NULL == fnout ) fnout  = av[++ia];
	 else { oops = "output file specified twice"; oops2 = av[ia]; }

      /** Extract predefined model option. ***/

      } else if ('p' ==  av[ia][1]) {

	 /*** Validate. ***/

	 if (NULL == (pm = av[++ia])) {
	    oops  = "missing required -p option values";
	    oops2 = "expect wireframe, sticks, ballstick or spacefill";
	 }

	 /*** Predefined models. ***/

	 else if (0 == strcmp("wireframe", pm)) { *model = CX_WRL_WIREFRAME; }
	 else if (0 == strcmp("sticks",    pm)) { *model = CX_WRL_STICKS;    }
	 else if (0 == strcmp("ballstick", pm)) { *model = CX_WRL_BALLSTICK; }
	 else if (0 == strcmp("spacefill", pm)) { *model = CX_WRL_SPACEFILL; }

	 /*** Unknown predefined model. ***/

	 else { oops  = "unknown -p option value:"; oops2 = pm; }

      /*** Generate error if unexpected option is encountered. ***/

      } else {
         oops = "unknown option"; oops2 = av[ia];
      }
      if (oops) break;
   }

   /*** If ok so far, open input stream and output file. ***/

   if (!oops) {
      if (NULL == fnin) fnin = "-";
      *ins = cx_create_iostream(fnin, CX_IO_READ);
      if (NULL == *ins) { oops = "can't open input stream"; oops2 = fnin; }
 
      *ppout = (NULL == fnout ? stdout : fopen(fnout, "w"));
      if (NULL == *ppout) { oops = "can't open output file"; oops2 = fnout; }
   }

   /*** Initialize known packages. ***/

   cx_message_pkg();
   cx_molecule_pkg();
   /* cu_camera_pkg(); */

   /*** On error, exit with usage summary. ***/

   if (oops) {
      OUCH(oops);
      OUCH(oops2);
      NOTE("");
      NOTE("Usage:  cex2wrl [options] [in.cex [out.wrl]]");
      NOTE("");
      NOTE("Input/output files may be specified as argument or options:");
      NOTE("  -i in.cex .............. specify .cex input file "  );
      NOTE("  -o out.wrl ............. specify .wrl output file"  );
      NOTE("  (stdin/stdout are used if not otherwise specified.)");
      NOTE("");
      NOTE("Option specifying type of model:");
      NOTE("  -p <wireframe|sticks|ballstick|spacefill>");
      NOTE("");
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Possibly show notes. ***/
   
   cx_error_spew(stderr, NULL);
}

/*============================================================================
 *  main program
 */
 
int main(int argc, char *argv[])
{
   cx_Object   ob, ins;
   cx_Integer  model;
   FILE       *fpout;

   /*** Deal with command line; some missions never return. ***/

   proginit(argc, argv, &ins, &fpout, &model);

   /*** Read objects from cex input stream; datatypes never appear. ***/
    
   while (NULL != (ob = cx_next(ins))) {

      /*** Switch on object type. ***/

      switch (cx_type(ob)) {

      /*** If molecule, write it as wrl. ***/

      case CX_OB_MOLECULE:
      
         cx_wrl_write(fpout, ob, model);

         /*** Clean up after this molecule: destroy it and spew errors. ***/

         cx_destroy(ob);
         cx_error_spew(stderr, NULL);
         break;

      /*** Process camera someday (ignore for now). ***/

      case CU_OB_CAMERA:
         cx_destroy(ob);
	 break;

      /*** Ignore (destroy?) other object types. ***/

      default:
         /* cx_destroy(ob); ? */
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
