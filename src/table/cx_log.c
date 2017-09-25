/*****************************************************************************
*  cx_log.c -- manage an error log
*
*  cx_log_errors()
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
#include "cx_cobweb.h"
#include "cx_message.h"

/*============================================================================
 *  cx_log_manager() -- suck up "log" messages, return log, NULL or object
 *
 *  If called with a message named "log", saves or accumulates message.
 *  If called with a NULL object, returns extant (or new) log.
 *  If called with any other object, returns that object.
 */

cx_Object cx_log_manager(cx_Object ob)
{
   static cx_Object log = NULL;
   cx_Object        sos, sob;
   char             buf[80];

   /*** Return log if called with NULL. ***/

   if (NULL == ob) {
      if (NULL == log) {
	 log = cx_create_message(NULL);
         cx_set_sprop(ob, CX_PROP_NAME, "log");
         cx_set_datatype(log, cx_pname2datatype(NULL, CX_TAG_MESSAGE));
      }
      return log;
   }

   /*** Return object if not a "log" message. ***/

   if (CX_OB_MESSAGE != cx_type(ob)) return ob;
   if (0 != cx_strcmp("log", cx_sprop(ob, CX_PROP_NAME))) return ob;

   /*** Save first log message. ***/

   if (NULL == log) { log = ob; return NULL; }

   /*** Reparent new log contents to extant log message. ***/

   sos = cx_stream(log, CX_OB_STRING);
   while (NULL != (sob = cx_next(sos)))
      cx_set_parent(sob, log);
   cx_destroy(sos);

   /*** Return NULL to indicate log message was accepted. ***/

   return NULL;
}

/*============================================================================
 *  errlev() -- local function returns integer value of error level
 *
 *  NULL severities are treated like CX_ERR_NONE (level 0).
 *  Unknown severities are treated as higher than CX_ERR_FATAL (level 5).
 */
 
static cx_Integer errlev(cx_String severity)
{
   if (NULL == severity                         ) return 0;
   if (0    == cx_strcmp(severity, CX_ERR_NONE )) return 0;
   if (0    == cx_strcmp(severity, CX_ERR_NOTE )) return 1;
   if (0    == cx_strcmp(severity, CX_ERR_WARN )) return 2;
   if (0    == cx_strcmp(severity, CX_ERR_ERROR)) return 3;
   if (0    == cx_strcmp(severity, CX_ERR_FATAL)) return 4;
   return 5;
}

/*============================================================================
 *  cx_e_log_errors() -- save errors requisite severity to msg then zap 'em
 *
 *  Copies (actually, reparents) error messages of given severity or worse to
 *  the given message object then then destroys the errorqueue.  Includes all
 *  all messages if `severity' is NULL, none if CX_OB_NONE.  If object msg is
 *  NULL, creates a new message object.  Returns message object, or NULL if
 *  msgin is NULL and no messages were added.
 */
 
cx_Object cx_e_log_errors(cx_Object msgin, cx_String severity)
{
   cx_Object  errorqueue = cx_errorqueue();
   cx_Object  msgout     = msgin;
   cx_Object  sob, sos;
   cx_String  str;
   cx_Integer lev;
   char       buf[80];

   /*** Don't print anything if severity is CX_OB_NONE. ***/

   if (severity && 0 == cx_strcmp(severity, CX_ERR_NONE)) return msgout;

   /*** Loop through messages, saving those of requisite severity. ***/

   lev = errlev(severity);
   sos = cx_stream(errorqueue, CX_OB_STRING);

   while (NULL != (sob = cx_next(sos))) {
      if (0 == lev || lev <= errlev(cx_sprop(sob, "severity"))) {

         /*** Create message if needed. ***/

         if (NULL == msgout) {
            msgout = cx_create_message(NULL);
            cx_set_datatype(msgout, cx_pname2datatype(NULL, CX_TAG_MESSAGE));
	    cx_set_sprop(msgout, CX_PROP_NAME, "log");
         }

         /*** Initialize scratchpad. ***/
   
	 cx_scratchpad(NULL);

         /*** Write source, using at least 10 spaces. ***/

         str = cx_sprop(sob, "source");
         sprintf(buf, "%-10s ", (str ? str : ""));
	 str = cx_scratchpad(buf);

         /*** Write severity, e.g., "WARNING ". ***/

         str = cx_sprop(sob, "severity");
         sprintf(buf, "%s ", (str ? str : "UNKNOWN"));
	 str = cx_scratchpad(buf);

         /*** Message follows. ***/

         str = cx_stringvalue(sob);
	 if (str) cx_scratchpad(str);
	 else     cx_scratchpad("(message text missing)");

         /*** Copy composed error to message. ***/

         str = cx_scratchpad("");
         cx_create_string(msgout, str);

      }
   }

   /*** Destroy default error object (also zaps stream and all messages). ***/

   cx_destroy(errorqueue);

   /*** Return destination message object (or NULL). ***/

   return msgout;
}

/*============================================================================
 *  cx_e_log_spew() -- print contents of log to given stream (or stderr)
 */
 
cx_Integer cx_e_log_spew(FILE *fpin, cx_Object log)
{
   FILE *fp = (fpin ? fpin : stderr);
   char *str;

   if (CX_OB_MESSAGE != cx_type(log)) return 0;
   if (0 != cx_strcmp("log", cx_sprop(log, CX_PROP_NAME))) return 0;
   str = cx_stringvalue(log);
   if (NULL == str || '\0' == *str) return 0;
   return 1;
}
