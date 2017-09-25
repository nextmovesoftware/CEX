/*****************************************************************************
*  hydrogenate.c -- CEX hydrogen control program
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

/*** Output flags. ***/

#define OP_HC2HA 1
#define OP_HA2HC 2
#define OP_NEWHC 3
#define OP_NEWHA 4

/*** Handy macro. ***/

#define NOTE(msg)  cx_error_save(msg, CX_ERR_NOTE,  *argv)
#define FATAL(msg) cx_error_save(msg, CX_ERR_FATAL, *argv)

/*============================================================================
 *  proginit() -- interpret arguments and initialize
 *
 *  On error, writes usage summary and exits; any return is successful.
 */

static void proginit(int argc, char **argv, cx_Object *ins, cx_Object *outs,
                     int *op,  char **elev)
{
   char  *oops, *oops2, *fnin, *fnout;
   int    ia;

   /*** Set default values. ***/

    fnin  = (char*)0;
    fnout = (char*)0;
   *op    = OP_NEWHA;
   *elev  = CX_ERR_ERROR;
    oops  = (char*)0;
    oops2 = (char*)0;

   /*** Initialize molecule package. ***/
    
   cx_molecule_pkg();

   /*** Deal with command line arguments. ***/

   for (ia = 1; ia < argc; ia++) {

      /*** Extract input, output file names; error if more than two. ***/

      if ('-' != *argv[ia]) {
         if      (NULL == fnin ) fnin  = argv[ia];
         else if (NULL == fnout) fnout = argv[ia];
         else { oops = "too many file names specified"; oops2 = argv[ia]; }

      /*** Deal with options. ***/

      } else {
         if      (0 == cx_strcmp("-ha", argv[ia]))  *op   = OP_HC2HA;
         else if (0 == cx_strcmp("-hc", argv[ia]))  *op   = OP_NEWHC;
         else if (0 == cx_strcmp("-hr", argv[ia]))  *op   = OP_HA2HC;
         else if (0 == cx_strcmp("-hx", argv[ia]))  *op   = OP_NEWHA;
         else if (0 == cx_strcmp("-me", argv[ia]))  *elev = CX_ERR_ERROR;
         else if (0 == cx_strcmp("-mn", argv[ia]))  *elev = CX_ERR_NOTE;
         else if (0 == cx_strcmp("-mw", argv[ia]))  *elev = CX_ERR_WARN;
         else { oops  = "unknown option:"; oops2 = argv[ia]; }
      }
   }

   /*** Open input and output files. ***/

   if (NULL == oops) {
      if (NULL == fnin) fnin = "-";
      *ins = cx_create_iostream(fnin, CX_IO_READ);
      if (NULL == *ins) {
	 oops = "can't open input stream"; oops2 = fnin;
      } else {
         if (NULL == fnout) fnout = "-";
         *outs = cx_create_iostream(fnout, CX_IO_WRITE);
         if (NULL == *outs) { oops = "can't open output file"; oops2 = fnout; }
      }
   }

   /*** Exit on syntax error. ***/

   if (oops) {
      NOTE ("hydrogenate -- add (or remove) cex molecule hydrogens");
      NOTE (""                                                     );
      FATAL(oops                                                   );
      FATAL(oops2                                                  );
      NOTE (""                                                     );
      NOTE ("Usage:  hydrogenate [options] [in.cex [out.cex]]"     );
      NOTE (""                                                     );
      NOTE ("Standard input  used if `in.cex'  is not specified."  );
      NOTE ("Standard output used if `out.cex' is not specified."  );
      NOTE ("Error messages are written to standard error."        );
      NOTE (""                                                     );
      NOTE ("Options may include one of:"                          );
      NOTE (" -ha: convert hydrogen counts to hydrogen atoms"      );
      NOTE (" -hc: invent hydrogen counts from scratch"            );
      NOTE (" -hr: remove normal hydrogens, update counts"         );
      NOTE (" -hx: explicit hydrogens, like -hc then -ha (default)");
      NOTE (""                                                     );
      NOTE ("and one of:"                                          );
      NOTE (" -me: show ERROR messages only (default)"             );
      NOTE (" -mn: show NOTE, WARNING & ERROR messages"            );
      NOTE (" -mw: show WARNING and ERROR messages"                );
      cx_error_spew(stderr, NULL);
      exit(1);
   }
}

/*============================================================================
 *  main() for hydrogenate
 */

int main(int argc, char **argv)
{
   int        op;
   char      *elev;
   cx_Object  ob, ins, outs;

   /*** Initialize program (any return is successful). ***/

   proginit(argc, argv, &ins, &outs, &op, &elev);

   /*** Loop over objects on input stream. ***/

   while (NULL != (ob = cx_next(ins))) {

      /*** Operate only on molecule objects. ***/

      if (CX_OB_MOLECULE == cx_type(ob)) {

         /*** Switch on operation. ***/

         switch (op) {
            case OP_HC2HA: cx_mol_addhall(ob);                     break;
            case OP_NEWHC: cx_mol_setimph(ob);                     break;
            case OP_HA2HC: cx_mol_zaph(ob);                        break;
            case OP_NEWHA: cx_mol_setimph(ob); cx_mol_addhall(ob); break;
         }

         /*** Associate MOL datatype with molecule. ***/

         cx_set_datatype(ob, cx_tag2datatype(NULL, "MOL"));
      }

      /*** Send object to output stream. ***/

      cx_append(outs, ob);

      /*** Clean up. ***/

      cx_error_spew(stderr, elev);
      cx_destroy(ob);
   }

   /*** Dump errors to stderr, clean up, close files, and exit with 0. ***/

   cx_error_spew(stderr, elev);
   cx_destroy(ins);
   cx_destroy(outs);
   cx_cleanup();
   return 0;
}
