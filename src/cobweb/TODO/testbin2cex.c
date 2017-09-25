/*****************************************************************************
* testbin2cex.c -- test program reads binary data, writes CEX stream
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

#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>


/*============================================================================
 *  testbin2cex
 */
 
main(int argc, char **argv)
{
   cx_Object      dt, pdt, bob, sob;
   cx_String      cex, sprog;
   unsigned char *prog, *p;
   char           path[CX_PATH_MAX], *oops = NULL;
   int            fd, ok;
   long           pos, size, i;

   /*** Deal with command line. ***/

   if (1 != argc) {
      cx_error_save("oops, arguments not allowed", CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Open input file. ***/

   fprintf(stderr, "Enter name of executable program: ");

   if (NULL == gets(path))
      oops = "binary not specified, exiting";
   else if (NULL != strpbrk(path, " ./:"))
      oops = "program requires a simple file name";
   else if (0 > (fd = open(path, O_RDONLY)))
      oops = "can't open binary input file";

   if (oops) {
      cx_error_save(oops, CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Measure file and read it in. ***/

   size = lseek(fd, 0L, SEEK_END);
   lseek(fd, 0L, SEEK_SET);
   prog = (unsigned char *) cx_malloc(size);
   read(fd, (char *) prog, size);
   close(fd);

   /*** Make into a binary object. ***/

   bob = cx_create_binary(NULL, (int) size, prog);

   /*** Associate datatype and attach to datatype itself. ***/

#ifdef OLDWAY
   dt = cx_create_datatype(NULL, "PROG", "program", "Program", "1", "BINARY",
			   "Executable program");
   bob = cx_create_binary(dt, (int) size, prog);
   cx_set_datatype(bob, dt);
   cx_set_sprop(dt, "_X", "hark");
   cx_set_sprop(dt, "program", cx_stringvalue(bob));
#endif

   /*** Create program datatype. ***/

   dt = cx_create_datatype(NULL, "PROG", "program", "Program", "1", "BINARY",
			   cx_stringvalue(bob));

   /*** Write to output. ***/

   cx_send(dt, NULL, stdout);

   /*** Clean up. ***/

   cx_cleanup();
   exit(0);
}
