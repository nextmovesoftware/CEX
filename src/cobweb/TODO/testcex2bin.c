/*****************************************************************************
* testcex2bin.c -- test program reads CEX, writes binary data, executes it
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
 *  testcex2bin
 */
 
main(int argc, char **argv)
{
   cx_Object      dt, bob, sob;
   cx_String      cex, sprog;
   unsigned char *prog, *p;
   char           path[CX_PATH_MAX], *oops = NULL;
   int            fd, ok, proglen;
   FILE          *fp;
   long           pos, i;

   /*** Open input file. ***/

   if (NULL == argv[1])
      oops = "binary not specified, exiting";
   else if (NULL == (fp = fopen(argv[1], "r")))
      oops = "can't open cex input file";
   else if (NULL != cx_receive(NULL, fp, stderr))
      oops = "humm.";
   else if (17 == fclose(fp))
      oops = "erk";
   else if (NULL == (dt = cx_pname2datatype(NULL, "program")))
      oops = "shucks";
   else if (NULL == (sprog = cx_sprop(dt, "property name")))
      oops = "hooey";
   else if (18 == printf("property name: %s\n", sprog))
      oops = "hogwash";
   else if (NULL == (sprog = cx_sprop(dt, "description")))
      oops = "drat";
   else if (NULL == (bob = cx_parse_binary(NULL, sprog)))
      oops = "durn";

   if (oops) {
      cx_error_save(oops, CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   fclose(fp);

   /*** Extract binary value of object. ***/
 
   prog = cx_binaryvalue(&proglen, bob);
   printf("Got %d-byte binary\n", proglen);
 
   /*** Open output file. ***/
 
   printf("Enter name of binary file to make: ");
 
   if (NULL == gets(path))
      oops = "binary not specified, exiting";
   else if (0 > (fd = open(path, O_WRONLY|O_CREAT, S_IRUSR|S_IWUSR|S_IXUSR)))
      oops = "can't open binary output file, exiting";
 
   if (oops) {
      cx_error_save(oops, CX_ERR_FATAL, *argv);
      cx_error_spew(stderr, NULL);
      exit(1);
   }
 
   /*** Write output. ***/
 
   write(fd, (char *) prog, proglen);
 
   /*** Clean up and exit with 0. ***/

   cx_e_cex_read(NULL);
   close(fd);
   exit(0);
}
