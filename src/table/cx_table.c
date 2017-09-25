/*****************************************************************************
*  cx_table.c -- support for table objects
*
*  The code in this file is designed to be robust and self-contained.
*
*  The C interface is defined in cx_table.h.
*
*  This package uses the "cx_basics", "cx_util" and "cx_err" packages.
*
*  Limitations:
*    Maximum length of string in message ............ 2^31 (2147483648)
*    Maximum number of strings per message .......... 2^31 (2147483648)
*
*  To do:
*   [ ] implement message object as base
*
*----------------------------------------------------------------------------
*
* message object
*   header
*      parent .... any or none
*      content ...... none
*
*----------------------------------------------------------------------------
*
* value: He said, "Hello there."\nShe said, "Hi!"\nThe rest is history.
* $MSG<"He said, ""Hello there.""\nShe said, ""Hi!""\nThe rest is history."
*
*
*  It's sort of strange that cx_stringvalue() returns the concatenated list
*  of messages -- should cx_setstringvalue() replace them?
*
*----------------------------------------------------------------------------
*
*  External form:  $MSG<"How now, \nbrown cow?">/NAM<query>/TO<McDonald>|
*
*  Object form:    message
*                    child string "How now, "
*                    child string "brown cow?"
*                    prop  "name"  "query"
*                    prop  "to"    "McDonald"
*
*----------------------------------------------------------------------------
*  Author: David Weininger
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*
*----------------------------------------------------------------------------
*
*  MSG<"xmas zoo">/MSG<partidge>/MSG<turtledove>/MSG<french hen>
*
*  $TAB<"Column 1","Column2","Column 3">
*  /NAM<"Table 1.  Title.">
*  /ROW<"row header">
*  /TD<"table data">
*  /ROW<row2>
*  |
*
*  <TAB>
*     <NAM>"Table 1.  Title."</NAM>
*     <ROW><TH>"Column 1"</TH><TH>"Column 2"</TH><TH>"Column 3"</TH></ROW>
*     <ROW>"row header"</ROW>
*        <TD>"table data"</TD>
*     </ROW>
*     <ROW>row2</ROW>
*        <TD>"table data"</TD>
*     </ROW>
*  </TAB>
*
*
*  TAB{
*     NAM{ "Table1. Title." }
*     THS{ "Column 1", "Column", "Column 3" }
*     ROW{
*        RH{ "Row 1:" }
>
*  /ROW<"row header">
*  /TD<"table data">
*  /ROW<row2>
*  |
*
*
*****************************************************************************/

#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "cx_cobweb.h"
#include "cx_message.h"

/*** Are message methods initialized?  Not initially. ***/

static int initialized = FALSE;

/*============================================================================
 *  cx_e_table_create_datatypes() -- create message-related datatypes
 *
 *  This returns TRUE iff all datatypes are created successfully.
 */

static char *dts[] =  {

   "TAB",     CX_PROP_TABLE,    "Table", "1",   "STRING",
   "Table as sequence of rows of strings",

   "ROW",     CX_PROP_ROW,      "Row",   "1",   "STRLST",
   "Row as sequence of strings",

   "NAM",     CX_PROP_NAME,     "Name",    "1", "STRING",
   "Name as arbitrary ASCII text",
};

cx_Integer cx_e_table_create_datatypes(cx_Object table)
{
   static int defined = FALSE;
   static int rv      = FALSE;
   char **p;

   if (defined) return rv;

   if (table == NULL) table = cx_default_datatypetable();

   if (NULL != table) {
      rv = TRUE;
      for (p = dts; *p; p += 6)
         if (!cx_create_datatype(table, p[0], p[1], p[2], p[3], p[4], p[5]))
            rv = FALSE;
      defined = TRUE;
   }
   return rv;
}

/*============================================================================
 *  tab_destroy(), row_destroy() -- destroy a table and row objects
 */

static void tab_destroy(cx_Object tab)
{
   cx_e_base_destroy(tab);
}

static void row_destroy(cx_Object row)
{
   cx_e_base_destroy(row);
}

/*============================================================================
 *  message_cexin() -- read message as CEX tree, save in object
 *
 *  If table is NULL, the default datatype table is used.
 *  If successful, return the message; if not, return NULL;
 */

static cx_Object message_cexin(cx_String cex, cx_Object tabin)
{
   cx_Object  table, dt, id, di, ob, msg = NULL;
   cx_Integer cxt;
   cx_String  p, q, msgtag, tag, data, lang, shape, pname;
   cx_String  func = "message_cexin";
   char       errbuf[80];

   char       cold, *hold;
   cx_Integer rt, skip2id;

   /*** Reject NULL cex strings. ***/
 
   if (NULL == cex) return NULL;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());
 
   /*** Extract root tag, return in error if unable. ***/
 
   if (NULL == (p = cx_e_cex_xtagdata(cex, &cxt, &msgtag, &data))) {
      strncpy(errbuf, cex, 50);
      errbuf[50] = '\0';
      cx_error_save("Can't parse tree starting:", CX_ERR_ERROR, func);
      cx_error_save(errbuf,                       CX_ERR_ERROR, func);
 
   /*** Full datatrees must start with an identifier. ***/
 
   } else if (CX_CXT_IDENTIFIER != cxt && NULL != cx_strqbrk(cex, "|")) {
      cx_error_save("Expected root identifier:", CX_ERR_ERROR, func);
      cx_error_save(msgtag,                      CX_ERR_ERROR, func);
      cx_error_save(cex,                         CX_ERR_ERROR, func);
 
   /*** Extract datatype; return in error if unable. ***/
 
   } else if (NULL == (dt = cx_tag2datatype(table, msgtag))) {
      cx_error_save("Can't get datatype for:", CX_ERR_ERROR, func);
      cx_error_save(msgtag,                    CX_ERR_ERROR, func);
 
   /*** Check for "STRLST" language; return in error if not. ***/
 
   } else if (NULL == (lang = cx_sprop(dt, "language"))) {
      cx_error_save("language not defined for:",  CX_ERR_ERROR, func);
      cx_error_save(msgtag,                       CX_ERR_ERROR, func);

   } else if (0 != cx_strcmp("STRLST", lang)) {
      sprintf(errbuf, "root %s language %s, expected STRLST\n", msgtag, lang);
      cx_error_save(errbuf, CX_ERR_ERROR, func);

   /*** Should make up, and check, language. ***/
 
   /*** Create empty message object; return in error if fails. ***/

   } else {
      msg = cx_messin(NULL, data); /* fwd ref to public entry */
   }

   /*** We're done with tag/data in any case. ***/

   cx_free(msgtag);

   /*** Return NULL if can't create message for any reason. ***/

   if (NULL == msg) return NULL;

   /*** Attach datatype to message. ***/

   cx_set_datatype(msg, dt);
 
   /*** Interpret remaining data. ***/

   skip2id = FALSE;
   id = di = msg;
   for ( ; NULL != (q = cx_e_cex_xtagdata(p, &cxt, &tag, &data)); p = q) {

      /*** Skip to next id if handled elsewhere. ***/

      if (skip2id) {
         if (CX_CXT_IDENTIFIER == cxt) skip2id = FALSE;
         else                          continue;
      }
   
      /*** Get datatype from tag, language & shape from datatype. ***/

      if (NULL == (dt = cx_tag2datatype(table, tag))) {
         cx_error_save("Can't get datatype for:",    CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (lang = cx_sprop(dt, "language"))) {
         cx_error_save("language not defined for:",  CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (pname = cx_sprop(dt, "property name"))) {
         cx_error_save("no property name for:",      CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (shape = cx_sprop(dt, "shape"))) {
         cx_error_save("shape not defined for:",     CX_ERR_NOTE, func);
         cx_error_save(tag,                          CX_ERR_NOTE, func);
         cx_error_save("... using shape 1 (scalar)", CX_ERR_NOTE, func);
         shape = "1";
      }

      /*** Look for root type of this tag (or CX_OB_INVALID). ***/
 
      rt = cx_tag2roottype(tag);
 
      /*** Attach identifiers to message. ***/

      if (CX_CXT_IDENTIFIER == cxt) {
 
         /*** If could-be root identifier, use its parser. ***/
 
         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, "$|");
            if (hold) { cold = *hold; *hold = '\0'; }
            cx_set_parent(cx_e_cexin(p, rt, table), msg);
            if (hold) *hold = cold;
            skip2id = TRUE;
 
         /*** Oops. ***/
 
         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("don't know about MSG tuples", CX_ERR_ERROR, func);
            cx_error_save(shape,                         CX_ERR_ERROR, func);
            cx_error_save(tag,                           CX_ERR_ERROR, func);

         /*** Message subtrees (no tuples or children). ***/

         } else {
            id = di = ob = cx_create_string(msg, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach dataitems to last identifier. ***/

      } else if (CX_CXT_DATAITEM == cxt) {
 
         /*** If could-be root datatype, use its parser. ***/
 
         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, ">");
            if (hold) { hold++; cold = *hold; *hold = '\0'; }
            cx_set_parent(cx_e_cexin(p, rt, table), msg);
            if (hold) *hold = cold;
 
         /*** Oops. ***/
 
         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("don't know about msg tuples", CX_ERR_ERROR, func);
            cx_error_save(shape,                         CX_ERR_ERROR, func);
            cx_error_save(tag,                           CX_ERR_ERROR, func);

         /*** Message dataitems. ***/

         } else {
            di = ob = cx_create_string(id, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach properties to last dataitem. ***/

      } else if (CX_CXT_PROPERTY == cxt) {
         if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("tuple-based props not ok", CX_ERR_ERROR, func);
            cx_error_save(shape,                      CX_ERR_ERROR, func);
            cx_error_save(tag,                        CX_ERR_ERROR, func);
         } else {
            cx_set_sprop(di, pname, data);
         }

      /*** Humm. ***/

      } else {
         cx_error_save("Unexpected context (ouch!)" , CX_ERR_ERROR, func);
         cx_error_save(tag,                           CX_ERR_ERROR, func);
         cx_error_save(data,                          CX_ERR_ERROR, func);
      }

      /*** Free this tag/data pair. ***/

      cx_free(tag);
      cx_free(data);
   }
 
   /*** return message. ***/

   return msg;
}

/*============================================================================
 *  message_send() -- read message as CEX tree, save in object
 *
 *  Write given message, children, properties, and datatypes to output in
 *  a def-before-ref fashion.  cx_dt_table_setmarks(table,FALSE) should be
 *  called before this function is called on a particular stream.
 *
 *  If table is NULL, the default datatype table is used.
 *
 *  Uses local directly recursive function sendmsg().
 *
 *  Returns TRUE iff successful.
 */

static int has_visible_children(cx_Object ob)
{
   cx_Object kid, kids = cx_stream(ob, CX_OB_ANY);
   while (NULL != (kid = cx_next(kids)))
      if (CX_OB_PROPERTY != cx_type(kid) && cx_datatype(kid)) return TRUE;
   return FALSE;
}
 
static cx_Integer sendmsg(cx_Object ob, cx_Object table, cx_String eod,
                          cx_Object outs, int pass, int lev)
{
   cx_Object dt, kid, kids, prop, props;
   cx_String pname, sval;

   /*** Successful no-op if ob is NULL or has no associated datatype. ***/
 
   if (NULL == ob || NULL == (dt = cx_datatype(ob))) return TRUE;

   /*** Unsuccessful no-op if table, isn't. ***/

   if (CX_OB_DATATYPE_TABLE != cx_type(table)) return FALSE;
 
   /*** Send datatype on pass 0, print dataitem on pass 1. ***/

   if (0 == pass) {
      cx_append(outs, dt);

   } else if (0 == lev || has_visible_children(ob)) {
      cx_e_ioputc('$',                 outs);
      cx_e_ioputs(cx_sprop(dt, "tag"), outs);
      cx_e_ioputc('<',                 outs);
      cx_e_ioputs(cx_stringvalue(ob),  outs);
      cx_e_ioputc('>',                 outs);
      cx_e_ioputs(eod,                 outs);
 
   } else {
      cx_e_ioputs(cx_sprop(dt, "tag"), outs);
      cx_e_ioputc('<',                 outs);
      cx_e_ioputs(cx_stringvalue(ob),  outs);
      cx_e_ioputc('>',                 outs);
      cx_e_ioputs(eod,                 outs);
   }

   /*** Send property datatype on pass 0, print property on pass 1. ***/

   props = cx_stream(ob, CX_OB_PROPERTY);
   while (NULL != (prop = cx_next(props))) {
      pname = cx_prop_name(prop);
      if (NULL != (dt = cx_pname2datatype(table, pname))) {
         if (0 == pass) {
            cx_append(outs, dt);
         } else {
            cx_e_ioputc('/',                  outs);
            cx_e_ioputs(cx_sprop(dt, "tag"),  outs);
            cx_e_ioputc('<',                  outs);
            cx_e_ioputs(cx_stringvalue(prop), outs);
            cx_e_ioputc('>',                  outs);
            cx_e_ioputs(eod,                  outs);
         }
      }
   }
   cx_destroy(props);

   /*** Print children recursively. ***/

   kids = cx_stream(ob, CX_OB_ANY);
   while (NULL != (kid = cx_next(kids)))
      if (CX_OB_PROPERTY != cx_type(kid))
         sendmsg(kid, table, eod, outs, pass, lev + 1);
   cx_destroy(kids);

   /*** Return successfully. ***/

   return TRUE;
} 
 
static cx_Integer message_send(cx_Object ob, cx_Object table, cx_Object outs)
{
   cx_Integer ok;
   cx_String  eod, eot;

   /*** Successful no-op if ob is NULL. ***/

   if (NULL == ob) return TRUE;

   /*** Unsuccessful no-op if outs is NULL. ***/

   if (NULL == outs) return FALSE;

   /*** Set end-of-data and end-of-tree delimiters from listing format. ***/

   switch (cx_cex_listfmt()) {
      case CX_FMT_RAW:   eod = "";    eot = "";    break;
      case CX_FMT_DUMP:  eod = "";    eot = "\n";  break;
      case CX_FMT_LIST:  eod = "\n";  eot = "\n";  break;
   }

   /*** Send datatypes, send datatree, return success. ***/

   ok = sendmsg(ob, table, eod, outs, 0, 0);
   if (ok) ok = sendmsg(ob, table, eod, outs, 1, 0);
   cx_e_ioputc('|', outs);
   cx_e_ioputs(eot, outs);
   return ok;
}

/*** Local auto-initialization function follows. ***/

/*============================================================================
 *  message_init() -- initialize message-specific functions
 */
 
void message_init(void)
{
   /*** Define message and message-based types. ***/
 
   cx_e_set_typename(CX_OB_MESSAGE, "message");

   /*** Register root tag. ***/

   cx_e_register_root_type(CX_TAG_MESSAGE, CX_OB_MESSAGE);
 
   /*** message-specific functions. ***/

   cx_set_method(CX_OB_MESSAGE, "cexin",       message_cexin        );
   cx_set_method(CX_OB_MESSAGE, "send",        message_send         );
   cx_set_method(CX_OB_MESSAGE, "setproperty", cx_e_base_setproperty);
   cx_set_method(CX_OB_MESSAGE, "stringvalue", message_stringvalue  );
   cx_set_method(CX_OB_MESSAGE, "destroy",     message_destroy      );
   cx_set_method(CX_OB_MESSAGE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_MESSAGE, "stream",      cx_e_base_stream     );

   /*** Mark message methods initialized and return successfully. ***/
       
   initialized = TRUE;
}
 
/*============================================================================
 *  cx_message_pkg() -- public message package initialization
 */

cx_Integer cx_e_message_pkg(void)
{
   /*** Initialize message methods and datatypes as needed. ***/

   if (!initialized) {
      message_init();
      return cx_e_message_create_datatypes(NULL);
   }
   return TRUE;
}

/*============================================================================
 *  cx_e_messin() -- construct a mesage from stringlist value.
 *
 *   "string1","string 2",string 3,"string 4"
 */

cx_Object cx_e_messin(cx_Object parent, cx_String in)
{
   cx_Object  msg, sob;
   cx_String  p, pp, y, z;
   char       c;

   /*** Require a non-NULL, non-empty input string. ***/

   if (in == NULL || *in == '\0') return NULL;

   /*** Create an empty message. ***/

   msg = cx_e_create_message(parent);
   if (NULL == msg) return NULL;

   /*** Create an empty message. ***/

   for (pp = p = in; *p; p++) {

      /*** Treat "\\n" as delimiter. ***/

      if ('\\' == *p && 'n' == *(p+1)) {
	 p++;
	 *p  = '\0';
         sob = cx_create_string(msg, pp);
	 *p  = 'n';
	 pp = p + 1;

      /*** Also treat "\n" as delimiter. ***/

      } else if ('\n' == *p) {
	 *p = '\0';
         sob = cx_create_string(msg, pp);
	 *p = '\n';
	 pp = p + 1;
      }
   }

   if (*pp) sob = cx_create_string(msg, pp);

   /*** Reset and return. ***/

   return msg;
}

/*============================================================================
 *  cx_e_create_message() -- return newly alloced & initialized msg, or NULL
 */

cx_Object cx_e_create_message(cx_Object parent)
{
   cx_Object  msg;

   /*** Initialize message methods if needed. ***/

   if (!initialized) message_init();

   /*** Create base message object. ***/

   msg = cx_e_base_create(parent, CX_OB_MESSAGE);
   if (NULL == msg) return NULL;

   /*** Message object has no content. ***/

   cx_e_base_set_content(msg, NULL);

   return msg;
}
