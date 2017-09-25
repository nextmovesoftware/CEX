/*****************************************************************************
*  cx_message.h -- CX message package definitions
*
*----------------------------------------------------------------------------
*  Author: Dave Weininger  (Daylight)           
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*
*****************************************************************************/

#ifndef CX_MESSAGE_INCLUDED
#define CX_MESSAGE_INCLUDED 1

/*** Supported classes defined in cx_types.h:  CX_OB_MESSAGE ***/

#include "cx_types.h"

/*** Message datatype tags. ***/

#define CX_TAG_MESSAGE     "MSG"

/*** Message property names. ***/

#define CX_PROP_MESSAGE  "message"
#define CX_PROP_NAME     "name"
#define CX_PROP_FROM     "from"
#define CX_PROP_TO       "to"

/*** C-wrappers for all public functions in surface package. ***/

#define cx_message_pkg              cx_e_message_pkg
#define cx_messin                   cx_e_messin
#define cx_create_message           cx_e_create_message
#define cx_message_create_datatypes cx_e_message_create_datatypes

#define cx_log_errors               cx_e_log_errors


/*** Declarations of public functions in cx_surface.c. ***/

cx_Integer cx_e_message_pkg(void);
cx_Object  cx_e_create_message(cx_Object parent);
cx_Object  cx_e_messin(cx_Object parent, cx_String input);
cx_Integer cx_e_message_create_datatypes(cx_Object table);

cx_Object cx_e_log_errors(cx_Object msgin, cx_String severity);


/*** Log support. ***/

/*** Map external to internal entry points. ***/

#define cx_log_manager    cx_e_log_manager
#define cx_log_errors     cx_e_log_errors
#define cx_log_spew       cx_e_log_spew

/*  cx_log_manager(ob)
 *
 *  If called with a message named "log", saves or accumulates message.
 *  If called with a NULL object, returns extant (or new) log.
 *  If called with any other object, no-op and returns that object.
 */

cx_Object cx_e_log_manager(cx_Object ob);

/*  cx_e_log_errors()
 *
 *  Copies errors of given severity or worse to given message object then
 *  then destroys the errorqueue.  Includes all all messages if `severity'
 *  is NULL, none if CX_OB_NONE.  If object msg is NULL, creates a new
 *  message object.  Returns message object, or NULL if msgin is NULL and
 *  no messages were added.
 */
 
cx_Object cx_e_log_errors(cx_Object msgin, cx_String severity);

/*** cx_e_log_spew() -- print contents of log to given stream. ***/
 
int cx_e_log_spew(FILE *fp, cx_Object log);


#endif /* CX_MESSAGE_INCLUDED */
