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
   cx_Object  msg, outs;

   /*** Initialize known packages. ***/

   cx_message_pkg();
#ifdef OLDWAY
   cx_molecule_pkg();
   /* cu_camera_pkg(); */
#endif

   /*** Create output stream. ***/

   outs = cx_create_iostream("-", CX_IO_WRITE);
 
   /*** Invent message. ***/

   msg = cx_create_message(NULL);

cx_set_sprop(msg, "name", "This is the name");
cx_create_string(msg, "He said, \"Hello there.\"");
cx_create_string(msg, "She said, \"Hi!\"");
cx_create_string(msg, "The rest is history.");
 
   /*** Make it visible. ***/

   cx_set_datatype(msg, cx_tag2datatype(NULL, CX_TAG_MESSAGE));

fprintf(stderr, "msg %p, datatype %p, typename %s\n",
 msg, cx_datatype(msg), cx_typename(msg));

cx_spewob(stdout, msg, 7);
 
   /*** Send it. ***/

   cx_append(outs, msg);

   /*** Clean up and exit with 0. ***/

   cx_destroy(msg);
   cx_destroy(outs);
   cx_cleanup();
   exit(0);
}
