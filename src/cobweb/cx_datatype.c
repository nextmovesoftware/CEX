/*****************************************************************************
*  cx_datatype.c -- support for Datatype's and DatatypeTable's
*
*  Not much tricky here, except that loading datatypes on top of eachother
*  can cause unexpected results down the road.  datatype_cexin() compares
*  datatype properties with those in the table and does the following:
*
*    o  If "tag" and "pname" don't exist, no problem (normal load).
*    o  If all 6 attributes are identical, no comment (no action).
*    o  If only "desc" and/or "vname" differ, NOTE (no action)
*    o  If other attributes differ, ERROR (no action)
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

#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "cx_cobweb.h"

/*** Name of default datatype table. ***/
 
#define CX_DEFAULT_TABLE_NAME "default datatype table"

/*** The default datatype table. ***/
 
static cx_Object default_table = NULL;

/*** Are we initialized? ***/
 
static int initialized = FALSE;

/*** Handy macros ***/

#define ERROR(msg) cx_error_save(msg, CX_ERR_ERROR, func)
#define WARN(msg)  cx_error_save(msg, CX_ERR_WARN,  func)
#define NOTE(msg)  cx_error_save(msg, CX_ERR_NOTE,  func)

/*============================================================================
 *  cx_e_dt_mark() -- return boolean mark attribute of datatype
 */

cx_Integer cx_e_dt_mark(cx_Object dt)
{
   return (NULL != dt && NULL != cx_sprop(dt, CX_PROP_MARK));
}

/*============================================================================
 *  cx_e_dt_setmark() -- set "write once" mark for datatype to given value
 */

cx_Integer cx_e_dt_setmark(cx_Object dt, cx_Integer mark)
{
   return (dt && cx_set_sprop(dt, CX_PROP_MARK, (mark ? "x" : NULL)));
}

/*============================================================================
 *  cx_e_dt_table_setmarks() -- set marks for all datatypes in table to value
 */

cx_Integer cx_e_dt_table_setmarks(cx_Object tabin, cx_Integer mark)
{
   cx_Object dts, dt, table;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());

   /*** Set all datatypes in table to given (boolean) value. ***/

   dts = cx_stream(table, CX_OB_DATATYPE);
   while (NULL != (dt = cx_next(dts)))
      cx_e_dt_setmark(dt, mark);
   cx_destroy(dts);

   return TRUE;
}

/*============================================================================
 *  table_reset() -- unmark all datatypes in table, return success
 */
 
static cx_Integer table_reset(cx_Object ob)
{
   return cx_e_dt_table_setmarks(ob, FALSE);
}

/*============================================================================
 *  table_destroy() -- destroy a given datatype table object
 */
 
static void table_destroy(cx_Object ob)
{
   /*** Reject if not a datatype table object. ***/

   if (CX_OB_DATATYPE_TABLE != cx_type(ob)) return;

   /*** Free cx_String object content, if able. ***/

   cx_free((cx_String) cx_e_base_content(ob));

   /*** Destroy base object and any children. ***/

   cx_e_base_destroy(ob);

   /*** If ob is default database table, mark it zapped. ***/

   if (ob == default_table) default_table = NULL;
}

/*============================================================================
 *  table_stringvalue() -- datatype table object's name is its stringvalue
 */
 
static cx_String table_stringvalue(cx_Object ob)
{
   /*** Reject if not a datatype table object. ***/

   if (CX_OB_DATATYPE_TABLE != cx_type(ob)) return NULL;

   /*** Return object content as cx_String (if NULL use default table). ***/

   return (cx_String) cx_e_base_content(ob == NULL ? default_table : ob);
}

/*============================================================================
 *  table_send() -- unconditionally write datatype definitions to cex stream
 *
 *   Writes all datatypes in given table output via cx_append(), i.e.,
 *   only datatypes not yet written (marked) are output.  To write all
 *   datatypes to a file, call cx_reset(table) first.
 *
 *   Second argument is ignored -- the first argument is "it".
 *
 *   Returns TRUE iff successful.
 */

cx_Integer table_send(cx_Object tabin, cx_Object tabx, cx_Object outs)
{
   cx_Object    dts, dt, table;
   cx_Integer   ok = TRUE;
 
   /* Avoid compiler warning! */
   tabx = tabx;

   /*** If given table is NULL, use default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());
 
   /*** Can't write to NULL streams or non-tables. ***/

   if (NULL == outs || CX_OB_DATATYPE_TABLE != cx_type(table)) return FALSE;
 
   /*** Loop over all datatypes, output via cx_append(). ***/

   dts = cx_stream(table, CX_OB_DATATYPE);
   while (NULL != (dt = cx_next(dts)))
      cx_append(outs, dt);
   cx_destroy(dts);
 
   /*** Return success. ***/

   return ok;
}

/*============================================================================
 *  datatype_stringvalue() -- datatype's tag is its stringvalue
 */
 
static cx_String datatype_stringvalue(cx_Object ob)
{
   return cx_sprop(ob, "tag");
}
 
/*============================================================================
 *  datatype_cexin() -- read datatype definition as CEX tree, save in table
 *
 *  If table is NULL, the datatype is put in the default datatype table.
 *  If successful, return new datatype or equivalent; if not, return NULL;
 *
 *    o  If "tag" and "pname" don't exist, no problem (normal load).
 *    o  If all 6 attributes are identical, no comment (no action).
 *    o  If only "desc" and/or "vname" differ, NOTE (no action)
 *    o  If other attributes differ, ERROR (no action)
 */

#define CX_ROOT_DATATYPE "D"
 
cx_Object datatype_cexin(cx_String cex, cx_Object tabin)
{
   cx_Object  table, dttag, dtnam, dt = NULL;
   cx_Integer cxt;
   cx_String  p, dtag, datatag, tag, data, pname, vname, shape, lang, desc;
   cx_String  func = "dt/cexin";
 
   /*** Reject NULL cex strings. ***/
 
   if (NULL == cex) return NULL;
 
   /*** If given datatype table is NULL, use default table. ***/
 
   table  = (tabin ? tabin : cx_default_datatypetable());
 
   /*** Extract root datatype, check tag is CX_ROOT_DATATYPE, free tag. ***/
 
   if (NULL == (p = cx_e_cex_xtagdata(cex, &cxt, &dtag, &datatag))) return NULL;
   if (0 != strcmp(CX_ROOT_DATATYPE, dtag)) return NULL; /* free tag, dtag? */
   cx_free(dtag);
 
   /*** Fill datatype properties. ***/
 
   pname = vname = shape = lang = desc = NULL;  
   while (NULL != (p = cx_e_cex_xtagdata(p, &cxt, &tag, &data))) {

      /*** If non-properties are encountered skip them and make warning. ***/

      if (CX_CXT_PROPERTY != cxt) {
         cx_error_save("non-property attached to datatype", CX_ERR_WARN, func);
         cx_error_save(tag,                                 CX_ERR_WARN, func);
         cx_free(data);

      /*** Switch on known datatype property tags. ***/

      } else if ('_' == tag[0] && '\0' != tag[1] && '\0' == tag[2]) {
         switch (tag[1]) {
         case 'P':                                      /* property name */
            if (pname) {
               cx_error_save("duplicate _P data ignored", CX_ERR_WARN, func);
               cx_error_save(data,                        CX_ERR_WARN, func);
            } else
               pname  = data;
            break;

         case 'V':                                      /* verbose name */
            if (vname) {
               cx_error_save("duplicate _V data ignored", CX_ERR_WARN, func);
               cx_error_save(data,                        CX_ERR_WARN, func);
            } else
               vname  = data;
            break;

         case 'S':                                      /* shape */
            if (shape) {
               cx_error_save("duplicate _S data ignored", CX_ERR_WARN, func);
               cx_error_save(data,                        CX_ERR_WARN, func);
            } else if (!isdigit(*data)) {
               cx_error_save("invalid shape (dimension)", CX_ERR_WARN, func);
               cx_error_save(data,                        CX_ERR_WARN, func);
               cx_free(data);
            } else if ('\0' != *(data+1) && !CX_TUPLESHAPE(*(data+1))) {
               cx_error_save("invalid shape (parent)", CX_ERR_WARN, func);
               cx_error_save(data,                     CX_ERR_WARN, func);
               cx_free(data);
            } else {
               shape = data;
            }
            break;

         case 'L':                                      /* language */
            if (lang) {
               cx_error_save("duplicate _L data ignored", CX_ERR_WARN, func);
               cx_error_save(data,                        CX_ERR_WARN, func);
            } else if (strcmp("INTEGER",data) && strcmp("REAL",    data) &&
                       strcmp("STRING", data) && strcmp("STRLST",  data) &&
                       strcmp("BINARY", data) && strcmp("XSMILES", data) &&
                       strcmp("SURFEX", data) ) {
               cx_error_save("unknown language", CX_ERR_WARN, func);
               cx_error_save(data,               CX_ERR_WARN, func);
               cx_free(data);
            } else {
               lang = data;
            }
            break;

         case 'X':                                      /* explanation */
            if (desc) {
               cx_error_save("duplicate _D data ignored", CX_ERR_WARN, func);
               cx_error_save(data,                        CX_ERR_WARN, func);
            } else
               desc = data;
            break;

         default:
            cx_error_save("unexpected datatype", CX_ERR_WARN, func);
            cx_error_save(tag,                   CX_ERR_WARN, func);
            cx_free(data);
            break;
         }
 
      /*** Warn if datatype tag isn't in ^_[PVSLX]$ ***/

      } else {
         cx_error_save("unexpected datatype property tag", CX_ERR_WARN, func);
         cx_error_save(tag,                                CX_ERR_WARN, func);
         cx_free(data);
      }
 
      /*** Toss the tag (e.g., "_V"). ***/
 
      cx_free(tag);
   }

   /*** Require pname. ***/
 
   if (NULL == pname) {
      cx_error_save("Required property name missing for:", CX_ERR_ERROR, func);
      cx_error_save(datatag, CX_ERR_ERROR, func);

   /*** Else add datatype ONLY if tag and name are not already used. ***/

   } else {
      dttag = cx_tag2datatype  (table, datatag);
      dtnam = cx_pname2datatype(table, pname  );
      if (NULL == dttag && NULL == dtnam) {
         dt = cx_create_datatype(table, datatag,pname,vname,shape,lang,desc);

      /*** Error if tag and/or pname already used but unequal. ***/

      } else if (dttag != dtnam) {
         if (NULL != dttag && NULL == dtnam) {
            ERROR("datatype already exists with tag:"             );
            ERROR(datatag                                         );
         } else if (NULL == dttag && NULL != dtnam) {
            ERROR("datatype already exists with property name:"   );
            ERROR(pname                                           );
         } else {
            ERROR("tag and property name used by other datatypes:");
            ERROR(datatag                                         );
            ERROR(pname                                           );
         }

      /*** Error if same tag and pname but different shape or language. ***/

      } else if (0 != cx_strcmp(shape, cx_sprop(dttag, "shape"))) {
         ERROR("datatypes have same tag & pname but different shape:");
         ERROR(cx_sprop(dttag, "shape"));
         ERROR(shape);

      } else if (0 != cx_strcmp(lang, cx_sprop(dttag, "language"))) {
         ERROR("same tag, pname & shape but different language:");
         ERROR(cx_sprop(dttag, "language"));
         ERROR(lang);

      /*** Warn if same tag and pname but different vname or desc. ***/

      } else if (0 != cx_strcmp(vname, cx_sprop(dttag, "verbose name"))) {
         WARN("datatype with given verbose name not loaded:");
         WARN(datatag);
         WARN(vname);
         WARN("same tag, pname, shape & lang; different verbose name:" );
         WARN(cx_sprop(dttag, "verbose name"));
         dt = dttag;

      } else if (0 != cx_strcmp(desc, cx_sprop(dttag, "description"))) {
         WARN("datatype not loaded:");
         WARN(datatag);
         WARN("extant datatype differs only in description:");
         WARN(cx_sprop(dttag, "description"));
         WARN(desc);
         dt = dttag;

      /*** DON'T: Note if identical datatype already exists. ***/

      } else {
         /*********
         *  NOTE("identical datatype already exists in table:");
         *  NOTE(datatag);
         *********/
         dt = dttag;
      }
   }
 
   /*** Generate error if datatype can't be returned. ***/

   if (NULL == dt) {
      cx_error_save("can't make datatype for:", CX_ERR_ERROR, func);
      cx_error_save(datatag,                    CX_ERR_ERROR, func);
   }
 
   /*** Clean up. ***/
 
   cx_free(datatag);
   cx_free(pname);
   cx_free(vname);
   cx_free(shape);
   cx_free(lang);
   cx_free(desc);
 
   /*** Return datatype. ***/
 
   return (cx_Object) dt;
}

static void datatype_emit(cx_Object outs, char *pref, char *tag, char *eot)
{
   cx_e_ioputs(pref, outs);
   cx_e_ioputc('<',  outs);
   cx_e_ioputs(tag,  outs);
   cx_e_ioputc('>',  outs);
   cx_e_ioputs(eot,  outs);
}


/*============================================================================
 *  datatype_send() -- possibly write datatype definition as CEX tree
 *
 *   Write given datatype object to output in a def-before-ref fashion
 *   using the datatype mark attribute, i.e., only write datatypes for which
 *   cx_e_dt_mark() is FALSE and set cx_e_dt_setmark(dt,TRUE) for each
 *   datatype when written.  cx_e_dt_table_setmarks(table,FALSE) should be
 *   called before this function is called on a particular stream.
 *
 *   The second argument is ignored ... cx_parent(dt) is always used
 *
 *   Returns TRUE iff successful.
 */

cx_Integer datatype_send(cx_Object dt, cx_Object tabin, cx_Object outs)
{
   cx_String  str, tag, eod, eot;
   cx_String  func = "dt/send";
 
   /* Avoid compiler warning! */
   tabin = tabin;

   /*** Reject NULL streams or datatypes. ***/
 
   if (NULL == outs || NULL == dt) return FALSE;
 
   /*** Get datatype's table; if NULL, return FALSE. ***/
 
   if (NULL == cx_parent(dt)) return FALSE;
 
   /*** If already written, return successfully. ***/
 
   if (TRUE == cx_e_dt_mark(dt)) return TRUE;
 
   /*** Set end-of-data and end-of-tree delimiters from listing format. ***/
 
   switch (cx_cex_listfmt()) {
      default:           /* Avoid compiler warnings! */
      case CX_FMT_DUMP:  eod = "";    eot = "\n";  break;
      case CX_FMT_RAW:   eod = "";    eot = "";    break;
      case CX_FMT_LIST:  eod = "\n";  eot = "\n";  break;
   }
 
   /*** Write root datatype tag; fail if unable. ***/

   tag = cx_sprop(dt, "tag");
   if (NULL == tag) {
      cx_error_save("Can't get datatype tag", CX_ERR_ERROR, func);
      return FALSE;
   }
   datatype_emit(outs, "$D", tag, eod);

   /*** Write property name attribute; fail if unable. ***/

   str = cx_sprop(dt, "property name");
   if (NULL == str) {
      cx_error_save("Can't get property name for", CX_ERR_ERROR, func);
      cx_error_save(tag,                           CX_ERR_ERROR, func);
      return FALSE;
   }
   datatype_emit(outs, "/_P", str, eod);

   /*** Write other datatype attributes if present. ***/

   str = cx_sprop(dt, "verbose name");
   if (str) datatype_emit(outs, "/_V", str,eod);

   str = cx_sprop(dt, "shape"       );
   if (str) datatype_emit(outs, "/_S", str,eod);

   str = cx_sprop(dt, "language"    );
   if (str) datatype_emit(outs, "/_L", str,eod);

   str = cx_sprop(dt, "description" );
   if (str) datatype_emit(outs, "/_X", str,eod);

   cx_e_ioputc('|', outs);
   cx_e_ioputs(eot, outs);

   /*** Mark datatype and return successfully. ***/

   cx_e_dt_setmark(dt, TRUE);
   return TRUE;
}

/*============================================================================
 *  datatype_init() -- initialize datatype/table-specific functions
 */
 
static cx_Integer datatype_init(void)
{
   /*** Define DatatypeTable and Datatype object types. ***/
    
   cx_e_set_typename(CX_OB_DATATYPE_TABLE, "DatatypeTable");
   cx_e_set_typename(CX_OB_DATATYPE,       "Datatype"     );

   /*** DatatypeTable-specific functions. ***/

   cx_set_method(CX_OB_DATATYPE_TABLE, "destroy",     table_destroy    );
   cx_set_method(CX_OB_DATATYPE_TABLE, "reset",       table_reset      );
   cx_set_method(CX_OB_DATATYPE_TABLE, "send",        table_send       );
   cx_set_method(CX_OB_DATATYPE_TABLE, "stringvalue", table_stringvalue);

   /*** Can't input a whole table (yet). ***/

   cx_set_method(CX_OB_DATATYPE_TABLE, "cexin", NULL);

   /*** Use normal (base) functions for all other methods. ***/

   cx_set_method(CX_OB_DATATYPE_TABLE, "count",       cx_e_base_count       );
   cx_set_method(CX_OB_DATATYPE_TABLE, "stream",      cx_e_base_stream      );
   cx_set_method(CX_OB_DATATYPE_TABLE, "setproperty", cx_e_base_setproperty );

   /*** Datatype-specific functions. ***/

   cx_set_method(CX_OB_DATATYPE, "stringvalue", datatype_stringvalue);
   cx_set_method(CX_OB_DATATYPE, "send",        datatype_send       );
   cx_set_method(CX_OB_DATATYPE, "cexin",       datatype_cexin      );

   /*** Use normal (base) functions for other methods. ***/

   cx_set_method(CX_OB_DATATYPE, "destroy",     cx_e_base_destroy);
   cx_set_method(CX_OB_DATATYPE, "count",       cx_e_base_count  );
   cx_set_method(CX_OB_DATATYPE, "stream",      cx_e_base_stream );
   cx_set_method(CX_OB_DATATYPE, "setproperty", cx_e_base_setproperty );

   /*** Mark done and return successfully. ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  loadem() -- load datatype datatypes into given table, return table dt
 */
 
static cx_Object loadem(cx_Object tabin)
{
   cx_Object table;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());

   /*** Just load the other datatype datatypes. ***/

   cx_create_datatype(table, "D", "datatype",      "Datatype",
      "1", "STRING", "A datatype describes data syntax and semantics");

   cx_create_datatype(table, "_P", "property name", "Datatype property",
      "1", "STRING", "Property corresponding to datatype");

   cx_create_datatype(table, "_V", "verbose name",  "Datatype name",
      "1", "STRING", "Verbose datatype tag (visible to users)");

   cx_create_datatype(table, "_S", "shape",         "Datatype shape",
      "1", "STRING", "Dimensionality of datatype, in [1-9][ABFMSV]?");

   cx_create_datatype(table, "_L", "language",      "Datatype language",
      "1", "STRING", "Language used to parse data content");

   cx_create_datatype(table, "_X", "description",   "Datatype description",
      "1", "STRING", "Full description of datatype in English");
   

   /*** Datatype for datatype table is special (return value). ***/

   return cx_create_datatype(table, "DT", "datatype table",
                             "Datatype table", "1", "STRING",
                             "Datatype table has name, contains datatypes");
}

/*============================================================================
 *  cx_create_datatypetable() -- create an (almost) empty datatype table
 *
 *  A datatype table is just a base object and a name (like string object).
 *
 *  The returned table is not quite empty: datatypes for datatype tables and
 *  datatypes themselves are automatically loaded.
 */
 
cx_Object cx_e_create_datatypetable(cx_Object parent, cx_String name)
{
   cx_Object tob, dt;

   /*** Auto-initialize if needed. ***/

   if (!initialized) datatype_init();

   /*** Make new CX_OB_DATATYPE_TABLE object with given parent. ***/

   if (NULL == (tob = cx_e_base_create(parent, CX_OB_DATATYPE_TABLE)))
      return NULL;

   /*** Set content of new datatype table object to given name. ***/

   cx_e_base_set_content(tob, (void *) cx_strdup(name));

   /*** Load datatype datatypes. ***/

   dt = loadem(tob);
   cx_set_datatype(tob, dt);

   /*** Return initialized datatype table object. ***/

   return (cx_Object) tob;
}

/*============================================================================
 *  cx_e_default_datatypetable() -- return default datatype table
 *
 *  This returns NULL on error, i.e., out of memory.
 */
 
cx_Object cx_e_default_datatypetable(void)
{
   if (NULL == default_table)
      default_table = cx_e_create_datatypetable(NULL, CX_DEFAULT_TABLE_NAME);
   return default_table;
}

/*============================================================================
 *  cx_e_create_datatype() -- create datatype with all parts
 *
 *  Should we check that datatype doesn't conflict with others?
 *
 *  parent ... parent object, a datatype table or NULL
 *  tag ...... external tag, e.g., "$XSMI", "MP"
 *  pname .... property name, e.g., "melting point"
 *  vname .... verbose name, e.g., "Melting point, C"
 *  shape .... shape, e.g., "6", "1M", "3B", "1B", etc..
 *  lang ..... language, e.g., "INTEGER", "REAL", "XSMILES", etc
 *  desc ..... verbose description of datatype
 */

cx_Object cx_e_create_datatype(cx_Object table, cx_String tag,
                               cx_String pname, cx_String vname,
                               cx_String shape, cx_String lang,
                               cx_String desc                    )
{
   cx_Object dt;

   /*** Auto-initialize if needed. ***/

   if (!initialized) datatype_init();

   /*** Use default table or check that it is a datatype table. ***/

   if      (NULL == table) table = cx_e_default_datatypetable();
   else if (CX_OB_DATATYPE_TABLE != cx_type(table)) return NULL;

   /*** Make new CX_OB_DATATYPE object with given parent. ***/

   if (NULL == (dt = cx_e_base_create(table, CX_OB_DATATYPE))) return NULL;

   /*** Set datatype contents as properties. ***/

   cx_set_sprop(dt, CX_PROP_TAG,   tag  );
   cx_set_sprop(dt, CX_PROP_PNAME, pname);
   cx_set_sprop(dt, CX_PROP_VNAME, vname);
   cx_set_sprop(dt, CX_PROP_SHAPE, shape);
   cx_set_sprop(dt, CX_PROP_LANG,  lang );
   cx_set_sprop(dt, CX_PROP_DESC,  desc );
   cx_set_sprop(dt, CX_PROP_MARK,  NULL );

   /*** Associate D datatype with this datatype. ***/

   cx_set_datatype(dt, cx_tag2datatype(table, "D"));

   /*** Return initialized datatype object. ***/

   return dt;
}

/*============================================================================
 *  cx_e_set_datatype() -- associate datatype with object, return success
 */

cx_Integer cx_e_set_datatype(cx_Object ob, cx_Object dt)
{
   /*** Check that valid datatype is provided. ***/

   if (NULL != dt && CX_OB_DATATYPE != cx_type(dt)) return FALSE;

   /*** Get object header, return FALSE if unable. ***/

   return cx_e_base_set_datatype(ob, dt);
}

/*============================================================================
 *  cx_e_datatype() -- return datatype associated with object or NULL
 */

cx_Object cx_e_datatype(cx_Object ob)
{
   return cx_e_base_datatype(ob);
}

/*============================================================================
 *  cx_e_tag2datatype() -- return datatype with given tag or NULL
 */

cx_Object cx_e_tag2datatype(cx_Object tabin, cx_String tag)
{
   cx_Object table, dts, dt, dtout = NULL;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());

   /*** Look for datatype with given tag. ***/

   dts = cx_stream(table, CX_OB_DATATYPE);
   while (NULL != (dt = cx_next(dts)))
      if (0 == cx_strcmp(tag, cx_sprop(dt, "tag"))) { dtout = dt; break; }
   cx_destroy(dts);

   /*** If found, return it; if not, return NULL. ***/

   return dtout;
}

/*============================================================================
 *  cx_e_pname2datatype() -- return datatype with given pname or NULL
 */

cx_Object cx_e_pname2datatype(cx_Object tabin, cx_String pname)
{
   cx_Object table, dts, dt, dtout = NULL;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());

   /*** Look for datatype with given pname. ***/

   dts = cx_stream(table, CX_OB_DATATYPE);
   while (NULL != (dt = cx_next(dts))) {
      if (0 == cx_strcmp(pname, cx_sprop(dt, "property name"))) {
         dtout = dt;
         break;
      }
   }
   cx_destroy(dts);

   /*** If found, return it; if not, return NULL. ***/

   return dtout;
}
