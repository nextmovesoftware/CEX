/*****************************************************************************
*  addlog.c -- add strings to "log" message
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
#include "cx_molecule.h"
/* #include "cu_camera.h" */

/*** Handy macros. ***/

#define OUCH(msg) cx_error_save(msg, CX_ERR_FATAL, *argv)
#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE,  *argv)

/*============================================================================
 *  main program
 */
 
main(int argc, char **argv)
{
   cx_Object  ob, log, ins, outs;
   int        i;

   /*** Require one or more arguments, show usage summary. ***/

   if (2 > argc) {
      OUCH("Program addlog requires at least one argument.");
      NOTE("");
      NOTE("addlog -- add strings to \"log\" message(s) on CEX stream");
      NOTE("");
      NOTE("Usage:  addlog string [ string ... ]");
      NOTE("");
      NOTE("Reads and writes CEX stream on stdin/stdout, adding given");
      NOTE("string(s) to \"log\" messages on CEX stream.");
      NOTE("");
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Initialize known packages. ***/

   cx_molecule_pkg();
   cx_message_pkg();
   /* cu_camera_pkg(); */

   /*** Create input and output streams. ***/

   ins  = cx_create_iostream("-", CX_IO_READ );
   outs = cx_create_iostream("-", CX_IO_WRITE);
 
   /*** Read objects from cex input stream; datatypes never appear. ***/
    
   while (NULL != (ob = cx_next(ins))) {
fprintf(stderr, "read ob %p (%s) from standard input\n", ob, cx_typename(ob));

      /*** Accumulate log messages. ***/

      if (NULL == cx_log_manager(ob)) continue;

      /*** Pass other objects. ***/
 
      cx_append(outs, ob);
      cx_destroy(ob);
   }

   /*** Get log, spew errors to stderr, possibly zapem. ***/

cx_error_save("hi there", CX_ERR_NOTE, "addlog");
cx_error_save("Hi there, \"yourself\"!", CX_ERR_NOTE, "addlog");
/*
*/
 
   log = cx_log_manager(NULL);
fprintf(stderr, "datatype(log %p): %p\n", log, cx_datatype(log));
   for (i = 1; i < argc; i++)
{
fprintf(stderr, "should be adding %s\n", argv[i]);
      if (argv[i] && *argv[i]) cx_create_string(log, argv[i]);
}
fprintf(stderr, "log has %d children\n", cx_count(log, CX_OB_STRING));
   cx_append(outs, log);
fprintf(stderr, "log follows:\n");
cx_log_spew(stderr, log);
   cx_destroy(log);

   /*** Clean up and exit with 0. ***/

   cx_destroy(ins);
   cx_destroy(outs);
   cx_cleanup();
   exit(0);
}
