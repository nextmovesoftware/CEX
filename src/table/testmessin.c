/*****************************************************************************
*  testmess.c -- invent a message and send it
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
#include "cx_message.h"

/*** Handy macros. ***/

#define OUCH(msg) cx_error_save(msg, CX_ERR_FATAL, *argv)
#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE,  *argv)

/*============================================================================
 *  main program
 */
 
main(int argc, char **argv)
{
   cx_Object  ob, ins;

   /*** Initialize message package. ***/

   cx_message_pkg();

   /*** Create input stream. ***/

   ins = cx_create_iostream("-", CX_IO_READ);
 
fprintf(stderr, "TESTMESSIN\n");
   
   while (NULL != (ob = cx_next(ins))) {
      fprintf(stderr, "GOT %d object\n", cx_typename(ob));
cx_spewob(stdout, ob, 7);
   cx_destroy(ob);
   }
 
   /*** Send it. ***/

/*    cx_append(outs, msg); */

   /*** Clean up and exit with 0. ***/

   cx_error_spew(stderr, CX_ERR_NOTE);

   /* cx_destroy(msg); */
   cx_destroy(ins);
   cx_cleanup();
   exit(0);
}
