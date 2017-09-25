/*****************************************************************************
*  printcex.c -- produce an 80-character-wide formatted version of cex trees
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
#include <ctype.h>

#include "cx.h"
#include "cx_molecule.h"
#include "cx_surface.h"
#include "cu_camera.h"

#define NOTE(msg)  cx_error_save(msg, CX_ERR_NOTE,  *argv)
#define FATAL(msg) cx_error_save(msg, CX_ERR_FATAL, *argv)

/*============================================================================
 *  proginit() -- interpret arguments and initialize program
 *
 *  On error, writes usage summary and exits.  Any return is successful.
 */

static void proginit(int argc, char **argv, cx_Object *ins, 
                     FILE **ppout, int *vdat, int *vtab, cx_String *mlev)
{
   char      *fnin, *fnout, *pok, *oops, *oops2;
   int        ia;

   /*** Set default values. ***/

   *vdat = CX_SPEW_DEFAULT;
   *vtab = CX_SPEW_NONE;
   *mlev = CX_ERR_ERROR;
   fnin  = (char*)0;
   fnout = (char*)0;
   oops  = (char*)0;
   oops2 = (char*)0;

   /*** Deal with command line arguments. ***/

   for (ia = 1; ia < argc; ia++) {

      /*** Extract input, output file names; error if more than two. ***/

      if ('-' != *argv[ia]) {
         if      (NULL == fnin ) fnin  = argv[ia];
         else if (NULL == fnout) fnout = argv[ia];
         else { oops = "too many file names specified"; oops2 = argv[ia]; }

      /*** Deal with options. ***/

      } else {
         if      (0 == cx_strcmp("-oa", argv[ia]))  *vtab = CX_SPEW_DEFAULT;
         else if (0 == cx_strcmp("-or", argv[ia]))  *vtab = CX_SPEW_NONE;
         else if (0 == cx_strcmp("-me", argv[ia]))  *mlev = CX_ERR_ERROR;
         else if (0 == cx_strcmp("-mn", argv[ia]))  *mlev = CX_ERR_NOTE;
         else if (0 == cx_strcmp("-mw", argv[ia]))  *mlev = CX_ERR_WARN;

         /*** Interpret argument as number if it starts with a digit. ***/

         else if (isdigit(argv[ia][1])) {
            *vdat = -1 * strtol(argv[ia], &pok, 10);
            if (pok == argv[ia] || 0 > *vdat || 127 < *vdat) {
               oops  = "bad -<#> value, expected 0 - 127:";
               oops2 = argv[ia];
            }

         /*** Else, punt. ***/
         
         } else { oops  = "unknown option:"; oops2 = argv[ia]; }
      }
   }

   /*** Open or assign input and output files. ***/

   if (NULL == oops) {
      if (NULL == fnin) fnin = "-";
      if (NULL == (*ins = cx_create_iostream(fnin, CX_IO_READ))) {
         oops = "can't open input stream"; oops2 = fnin;
      } else {
         *ppout  = (NULL == fnout ? stdout : fopen(fnout, "w"));
         if (NULL == ppout) { oops = "can't open output file"; oops2 = fnout; }
      }
   }

   /*** Initialize known packages. ***/

   cx_molecule_pkg();
   cx_surface_pkg();
   cu_camera_pkg();
	
   /*** Exit on usage error. ***/

   if (oops) {
      NOTE ( "printcex -- print cex stream objects as datatrees"   );
      NOTE ( ""                                                    );
      FATAL(oops                                                   );
      FATAL(oops2                                                  );
      NOTE (""                                                     );
      NOTE ("Usage:  printcex [options] [in.cex [out.txt]]"        );
      NOTE (""                                                     );
      NOTE ("Input is from standard input if `in.cex'  is omitted.");
      NOTE ("Output is to standard output if `out.txt' is omitted.");
      NOTE ("Error messages are always written to standard error." );
      NOTE (""                                                     );
      NOTE ("Error message control options (at most one of):"      );
      NOTE ("  -me: show ERROR messages only (default)"            );
      NOTE ("  -mn: show NOTE, WARNING and ERROR messages"         );
      NOTE ("  -mw: show WARNING and ERROR messages"               );
      NOTE (""                                                     );
      NOTE ("Output content control options (at most one of):"     );
      NOTE ("  -oa: (o)utput (a)ll objects, including datatypes"   );
      NOTE ("  -or: (o)utput (r)oot objects only (default)"        );
      NOTE (""                                                     );
      NOTE ("Formatted output control option:"                     );
      NOTE ("  -<#>: print data at level #,"                       );
      NOTE ("    where # is bitwise print level (sum of):"         );
      NOTE ("      1 = data           16 = raw datatags"           );
      NOTE ("      2 = children       32 = object classes"         );
      NOTE ("      4 = properties     64 = summarize tuples"       );
      NOTE ("      8 = property names                      "       );
      NOTE (""                                                     );
      NOTE ("Default option settings are:  -me -or -7, i.e.,"      );
      NOTE ("\"print data, children & properties; show errors\""   );
      cx_error_spew(stderr, NULL);
      exit(1);
   }
}

/*============================================================================
 *  main()  for printcex program
 */

int main(int argc, char **argv)
{
   cx_Object   ob, ins;
   cx_String   mlev;
   int         vdat, vtab;
   FILE *fpout;

   /*** Initialize program, any return is successful. ***/

   proginit(argc, argv, &ins, &fpout, &vdat, &vtab, &mlev);

   /*** Read objects on input cex stream. ***/

   while (NULL != (ob = cx_next(ins))) {

      /*** Write data to output if not suppressed. ***/

      if (0 < vdat) cx_spewob(fpout, ob, vdat);
      cx_error_spew(stderr, mlev);
      cx_destroy(ob);
   }

   /*** If required (-oa), write (remaining) datatypes to output. ***/

   if (0 < vtab) cx_spewob(fpout, cx_default_datatypetable(), vtab);

   /*** Dump errors to stderr, clean up, close files and exit with 0. ***/

   cx_error_spew(stderr, mlev);
   cx_destroy(ins);
   fclose(fpout);
   cx_cleanup();
   return 0;
}
