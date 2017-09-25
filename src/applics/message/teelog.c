/*****************************************************************************
*  teelog.c -- copy "log" message to file (or standard error)
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
#include <math.h>
#include "cx.h"
#include "cx_message.h"
#include "cx_molecule.h"
/* #include "cu_camera.h" */

/*** Handy macros. ***/

#define OUCH(msg) cx_error_save(msg, CX_ERR_FATAL, *argv)
#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE,  *argv)

/*============================================================================
 *  proginit() -- initialize cx packages, interpret arguments, return files
 *
 *  On error, writes usage summary and exits.  Any return is successful.
 */
 
static void proginit(int argc, char **argv, FILE **fpp, int *ifdel)
{
   char *oops, *oops2, *fn;
   int   i;

   /*** Set default values for control arguments. ***/
 
  *fpp   = NULL;
  *ifdel = FALSE;
   oops  = oops2  = NULL;

   /*** Deal with command line. ***/
 
   for (i = 1; i < argc; i++) {

      /*** Extract file name as argument. ***/

      if ('-' != *argv[i]) {
         if (NULL == fn ) fn = argv[i];
	 else { oops = "too many files specified"; oops2 = argv[i]; }

      /*** Check for -d option. ***/

      } else if ('d' ==  argv[i][1]) {
         *ifdel = TRUE;

      /*** Generate error if unexpected option is encountered. ***/

      } else {
         oops = "unknown option"; oops2 = argv[i];
      }
      if (oops) break;
   }

   /*** If ok so far, open output files. ***/

   if (!oops) {
      *fpp = (NULL == fn ? stderr : cx_fopen(fn, "w"));
      if (NULL == *fpp) { oops = "can't open output file"; oops2 = fn; }
   }

   /*** Initialize known packages. ***/

   cx_molecule_pkg();
   cx_message_pkg();
/*    cu_camera_pkg(); */

   /*** On error, exit with usage summary. ***/

   if (oops) {
      OUCH(oops);
      OUCH(oops2);
      NOTE("");
      NOTE("teelog -- copy \"log\" message(s) to file or stderr");
      NOTE("");
      NOTE("Usage:  teelog [-d] [file]");
      NOTE("");
      NOTE("Reads and writes CEX stream on stdin/stdout, copying \"log\"");
      NOTE("messages to 'file' (or stderr if not 'file' not specified).");
      NOTE("If -d option is specified, log messages are deleted.");
      NOTE("");
      cx_error_spew(stderr, NULL);
      exit(1);
   }

   /*** Possibly show notes. ***/
   
   cx_error_spew(stderr, NULL);
}

/*============================================================================
 *  main program
 */
 
main(int argc, char **argv)
{
   cx_Object  ob, log;
   FILE      *fp;
   int        ifdel;
 
   /*** Initialize program. ***/

   proginit(argc, argv, &fp, &ifdel);

   /*** Read objects from cex input stream; datatypes never appear. ***/
    
   while (NULL != (ob = cx_receive(NULL, stdin, NULL))) {

      /*** Save log file if encountered. ***/
 
fprintf(stderr, "ob is msg %d\n", CX_OB_MESSAGE == cx_type(ob));
      if (NULL == cx_log_manager(ob)) continue;

      /*** Send/destroy object. ***/
 
      cx_send(ob, NULL, stdout);
      cx_destroy(ob);
   }

   /*** Get log, spew errors to stderr, possibly zapem. ***/
 
   log = cx_log_manager(NULL);
fprintf(stderr, "datatype(log): %d\n", cx_datatype(log));
   cx_log_spew(stderr, log);
   cx_destroy(log);

   /*** Clean up and exit with 0. ***/

   fclose(fp);
   cx_cleanup();
   exit(0);
}
