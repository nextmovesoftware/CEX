/*****************************************************************************
*  cx_errorqueue.c -- CX error package
*
*----------------------------------------------------------------------------
*
*  HIGH LEVEL (CU) ERRORQUEUE INTERFACE
*
*  cx_Integer cx_error_count(cx_String severity);
*
*     Returns the number of errors queued which are at least as severe as
*     the given `severity' (flags per cx_errorqueue.h, e.g., CX_ERR_WARNING).
*     Counts all errors if `severity' is NULL, counts none if CX_ERR_NONE.
*
*  cx_Integer cx_error_spew(FILE *fp, cx_String severity);
*
*     Prints error messages of given severity or worse to the given stream
*     then destroys all messages. Prints all messages if `severity' is NULL,
*     prints none if CX_ERR_NONE.  Returns the number of messages printed.
*
*  cx_Integer cx_error_save(cx_String msg, cx_String severity, cx_String src);
*
*     Saves an error message with given severity (flags per cx_errorqueue.h)
*     and source (name of function generating error).  Returns success.
*
*  Note: a common idiom to print all messages if any are ERROR or worse is:
*    
*     if (cx_error_count(CX_ERR_ERROR)) cx_error_spew(stderr, NULL);
*
*
*  LOW LEVEL (CX) ERRORQUEUE INTERFACE
*
*  The low-level error utility consists support for a single
*  CX_OB_ERRORQUEUE object (only one can exist at any time).
*
*  Error messages are string objects owned by a special object of type
*  CX_OB_ERRORQUEUE ("Errorqueue"), obtained by:
*
*     cx_Object errorqueue = cx_errorqueue();
*
*  cx_errorqueue() will return the existing errorqueue object if able or
*  creating and returning a new one only if it doesn't exist, e.g.,
*  initially or after:
*
*     cx_destroy(errorqueue);
*
*  which also destroys underlying error messages (which are its children).
*
*  Error messages are obtained as a normal stream of strings, e.g.,
*
*     cx_Object sob, sos = cx_stream(errorqueue, CX_OB_STRING);
*     while (NULL != (sob = cx_next(sos)))
*        puts(stringvalue(sob));
*     cx_destroy(errorqueue);
*
*  and may be counted in the normal way, e.g.,
*
*     printf("%d errors are queued", cx_count(errorqueue, CX_OB_STRING));
*
*  Error messages are normal string objects with an errorqueue parent, e.g.:
*
*     cx_Object msg = cx_create_string(errorqueue, "Error message");
*
*  which creates and returns an error message as a string object belonging
*  to the errorqueue. The returned string object can have properties, e.g.,
*
*     cx_set_sprop(msg, "source",   "func");
*     cx_set_sprop(msg, "severity", "NOTE");
*
*  Errorqueues cannot be root objects; cx_e_cexin() and cx_send() are noops.
*
*  Finally, here's one way to print out error messages using cx functions:
*
*     cx_String str, src, sev;
*     cx_Object sob, sos, errorqueue;
*     errorqueue = cx_errorqueue();
*     sos        = cx_stream(errorqueue, CX_OB_STRING);
*     while (NULL != (sob = cx_next(sos))) {
*        str = cx_stringvalue(sob);
*        sev = cx_sprop(sob, "severity"));
*        src = cx_sprop(sob, "source"  ));
*        printf("%s: %s (%s)\n", (sev ? sev : "?"), str, (src ? src : "?"));
*     }
*     cx_destroy(errorqueue);
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

/*** The default errorqueue object is static. ***/

static cx_Object errorqueue = NULL;

/*** Are errorqueue methods initialized? ***/

static int initialized = FALSE;

/*============================================================================
 *  errorqueue_destroy() -- destroy errors using base method, possibly mark it
 */

static void errorqueue_destroy(cx_Object ob)
{
   cx_e_base_destroy(ob);
   if (ob == errorqueue) errorqueue = NULL;
}

/*============================================================================
 *  cx_e_errorqueue_init() -- initialize errorqueue-specific functions
 */
 
static cx_Integer errorqueue_init(void)
{
   /*** Define error object type. ***/
    
   cx_e_set_typename(CX_OB_ERRORQUEUE, "Errorqueue");

   /*** Errorqueue-specific functions. ***/

   cx_set_method(CX_OB_ERRORQUEUE, "destroy", errorqueue_destroy);

   /*** Use normal (base) count, stream and setproperty methods. ***/

   cx_set_method(CX_OB_ERRORQUEUE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_ERRORQUEUE, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_ERRORQUEUE, "setproperty", cx_e_base_setproperty);

   /*** Errorqueue objects don't reset, stringvalue, cexin, or send. ***/

   cx_set_method(CX_OB_ERRORQUEUE, "stringvalue", NULL);
   cx_set_method(CX_OB_ERRORQUEUE, "cexin",       NULL);
   cx_set_method(CX_OB_ERRORQUEUE, "reset",       NULL);
   cx_set_method(CX_OB_ERRORQUEUE, "send",        NULL);

   /*** Return happy (for now). ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  cx_e_errorqueue() -- create/return the "Errorqueue" object
 */

cx_Object cx_e_errorqueue(void)
{
   /*** Auto-initialize if needed. ***/

   if (!initialized) errorqueue_init();

   /*** Return extant errorqueue object if available. ***/

   if (NULL != errorqueue) return errorqueue;

   /*** Create new default (parent-less) errorqueue object and return it. ***/

   return (errorqueue = cx_e_base_create(NULL, CX_OB_ERRORQUEUE));
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
 *  cx_error_count() -- count error messages of requisite severity
 */
 
cx_Integer cx_e_error_count(cx_String severity)
{
   cx_Object  sob, sos, errorqueue = cx_errorqueue();
   cx_Integer lev, count = 0;

   /*** If NULL severity specified, count all children strings. ***/

   if (NULL == severity) return cx_count(errorqueue, CX_OB_STRING);

   /*** If CX_ERR_NONE severity specified, return 0 by definition. ***/

   if (0 == cx_strcmp(severity, CX_ERR_NONE)) return 0;

   /*** Loop through all messages, counting those with requisite severity. ***/

   lev = errlev(severity);
   sos = cx_stream(errorqueue, CX_OB_STRING);
   while (NULL != (sob = cx_next(sos)))
      if (lev <= errlev(cx_sprop(sob, "severity"))) count++;
   cx_destroy(sos);

   /*** Return count. ***/

   return count;
}

/*============================================================================
 *  cx_e_error_spew() -- print messages if any are of given severity, zap 'em
 *
 *  Prints error messages of given severity or worse to the given stream
 *  then destroys all messages.  Prints all messages if `severity' is NULL,
 *  none if CX_OB_NONE.  Returns the number of messages printed.
 */
 
cx_Integer cx_e_error_spew(FILE *fp, cx_String severity)
{
   cx_Object  sob, sos, errorqueue = cx_errorqueue();
   cx_String  str;
   cx_Integer lev, count = 0;

   /*** Don't print anything if severity is CX_OB_NONE. ***/

   if (0 != cx_strcmp(severity, CX_ERR_NONE)) {

      /*** Loop through messages, printing those of requisite severity. ***/

      lev = errlev(severity);
      sos = cx_stream(errorqueue, CX_OB_STRING);

      while (NULL != (sob = cx_next(sos))) {
         if (0 == lev || lev <= errlev(cx_sprop(sob, "severity"))) {

            /*** Write source, using at least 10 spaces. ***/

            str = cx_sprop(sob, "source");
            fprintf(fp, "%-10s ", (str ? str : ""));

            /*** Write severity, e.g., "WARNING ". ***/

            str = cx_sprop(sob, "severity");
            fprintf(fp, "%s ", (str ? str : "UNKNOWN"));

            /*** Message follows. ***/

            str = cx_stringvalue(sob);
            fprintf(fp, "%s\n", (str ? str : "(message text missing)"));

            /*** Count messages printed. ***/

            count++;
         }
      }
   }

   /*** Destroy default error object (also zaps stream and all messages). ***/

   cx_destroy(errorqueue);

   /*** Return count of messages printed. ***/

   return count;
}

/*============================================================================
 *  cx_e_error_save() -- save error message with severity and source
 *
 *  Saves an error message with given severity (flags per cx_e_errorqueue.h)
 *  and source (name of function generating error).  Returns TRUE on success,
 *  FALSE on failure (out of memory or NULL msg).
 */
 
cx_Integer cx_e_error_save(cx_String msg, cx_String severity, cx_String source)
{
   cx_Object  sob, errorqueue = cx_errorqueue();

   /*** Create message object or return in error if unable. ***/

   if (NULL == msg || NULL == (sob = cx_create_string(errorqueue, msg)))
      return FALSE;

   /*** Set message's severity and source properties. ***/

   cx_set_sprop(sob, "source",   (source   ? source   : "?"));
   cx_set_sprop(sob, "severity", (severity ? severity : "?"));

   /*** Return successfully. ***/

   return TRUE;
}

