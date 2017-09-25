/*****************************************************************************
*  testcexdt.c -- test program to read (and write) CEX datatypes
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
 *  testcexdt -- test program
 */
 
main(int argc, char **argv)
{
   cx_Object   ob;
   cx_Integer  cxt;
   cx_String   cex, tag, data, p;
   FILE       *fpin;
   int         ok;

   /*** Deal with command line. ***/

   if (2 != argc) {
      cx_error_save("oops, need one argument, file.cex", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Open input file. ***/

   if (NULL == (fpin = fopen(argv[1], "r"))) {
      cx_error_save("can't open input file", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Loop over input file. ***/

   while (1) {

      /*** Read next CEX entry, break on EOF. ***/

      cex = cx_e_cex_read(fpin);
      if (cx_e_cex_eof()) break;

      /*** Croak if CEX entry failed for some other reason than EOF. ***/

      if (!cex) {
         cx_error_save("error reading cex", CX_ERR_FATAL, *argv);
         cx_error_spew(stderr, NULL);
	 exit(1);
      }

      /*** Parse first tag-data pair. ***/

      ok = (NULL != (p = cx_e_cex_xtagdata(cex, &cxt, &tag, &data)));

      if (!ok) {
         cx_error_save("error parsing cex", CX_ERR_FATAL, *argv);
         cx_error_save(cex,                 CX_ERR_NOTE,  *argv);
         cx_error_spew(stderr, NULL);
	 exit(1);
      }

      if (CX_CXT_IDENTIFIER != cxt || 0 == strcmp("D", tag)) {
	 ob = cx_e_cexin(cex, CX_OB_DATATYPE, NULL);
	 ok = cx_send(ob, NULL, stdout);

      } else {
         cx_error_save("only expect datatype trees, not:", CX_ERR_WARN, *argv);
         cx_error_save(cex,                                CX_ERR_NOTE, *argv);
         cx_error_spew(stderr, NULL);
      }

#ifdef OLDWAY
      /*** If successful, write CEX output. ***/

      ok = cx_e_cex_write(stdout, cex);

      if (!ok) {
         cx_error_save("error writing cex", CX_ERR_FATAL, *argv);
         cx_error_spew(stderr, NULL);
	 exit(1);
      }
#endif
      cx_error_spew(stderr, NULL);
   }


   /*** Clean up and exit with 0. ***/

   cx_e_cex_read(NULL);
   fclose(fpin);
   exit(0);
}
