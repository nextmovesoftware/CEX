/*****************************************************************************
* testcexrw.c -- test program to read and write CEX's to/from streams
*----------------------------------------------------------------------------
*  To do:
*  [ ] out of memory error wierd
*  [ ] out of memory not detected for final character
*  [ ] how about comments, e.g., "   "
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "cx_cobweb.h"

/*============================================================================
 *  testcexrw -- test program
 */
 
main(int argc, char **argv)
{
   cx_Object   ins, outs;
   cx_String   cex;
   int         ok;

   /*** Deal with command line. ***/

   if (2 != argc) {
      cx_error_save("oops, need one argument, file.cex", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Open input file. ***/

   if (NULL == (ins = cx_create_iostream(argv[1], CX_IO_READ))) {
      cx_error_save("can't open input file", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Create stdout cx_IOStream! ***/

   outs = cx_create_iostream("-", CX_IO_WRITE);
   if (outs == NULL ) {
      cx_error_save("can't open output file", CX_ERR_FATAL, "<stdout>");
      cx_error_spew(stderr, NULL);
      exit(1);
   }


fprintf(stderr, "ins %p, outs %p\n", ins, outs);

   /*** Loop over input file. ***/

   while (1) {

      /*** Read next CEX entry, break on EOF. ***/

      cex = cx_e_cex_read(ins);
fprintf(stderr, "cex %p\n", cex);
      if (cx_cex_eof()) break;

      /*** Croak if CEX entry failed for some other reason than EOF. ***/

      if (!cex) {
         cx_error_save("error reading cex", CX_ERR_FATAL, *argv);
         cx_error_spew(stderr, NULL);
	 exit(1);
      }

fprintf(stderr, "cex %s\n", cex);
      /*** If successful, write CEX output. ***/

      ok = cx_e_cex_write(outs, cex);

      if (!ok) {
         cx_error_save("error writing cex", CX_ERR_FATAL, *argv);
         cx_error_spew(stderr, NULL);
	 exit(1);
      }
   }


   /*** Clean up and exit with 0. ***/

   cx_e_cex_read(NULL);
   cx_destroy(ins);
   cx_destroy(outs);
   exit(0);
}
