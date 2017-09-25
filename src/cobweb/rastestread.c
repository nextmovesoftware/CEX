/*****************************************************************************
* rastestread.c -- test program to read from raw CEX cx_IOStream
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
   cx_Object ins, outs;
   int       ch;

   /*** Deal with command line. ***/

   if (2 != argc) {
      cx_error_save("oops, need one argument, in.cex", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Open input file. ***/

   ins = cx_create_iostream(argv[1], CX_IO_READ);
   if (NULL == ins) {
      cx_error_save("can't open input file", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Create stdout cx_IOStream! ***/

   outs = cx_create_iostream("-", CX_IO_WRITE);
   if (NULL == outs) {
      cx_error_save("can't open output file", CX_ERR_FATAL, "<stdout>");
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   while(!cx_e_ioeof(ins)) {
      ch = cx_e_iogetc(ins);
      if( ch != -1 ) cx_e_ioputc(ch, outs);
   }

   cx_destroy(ins);
   exit(0);
}
