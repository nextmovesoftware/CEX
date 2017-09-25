

#include <string.h>
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>

#include "cx.h"

/*** Variable type used for implicit Fortran string length ***/

typedef cx_Integer FLENS;

/*** Fortran true and false. ***/

#define FTRUE  1
#define FFALSE 0

/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Some compilers append a "_" to fortran function names, but others
* do not.  Because this was written first for Sun (which does append
* "_"), the code has the "_" in the names.  For machines that don't
* use this convention, we just redefine these names back to their
* "real" values.
======================================================================*/

#if defined(HPUX) || defined(NEXT)

#define cx_f_ancestor_		cx_f_ancestor
#define cx_f_append_		cx_f_append
#define cx_f_atend_		cx_f_atend
#define cx_f_atof_		cx_f_atof
#define cx_f_atoi_		cx_f_atoi
#define cx_f_binaryvalue_	cx_f_binaryvalue
#define cx_f_cex_eof_		cx_f_cex_eof
#define cx_f_cex_listfmt_	cx_f_cex_listfmt
#define cx_f_cleanup_		cx_f_cleanup
#define cx_f_count_		cx_f_count
#define cx_f_create_binary_	cx_f_create_binary
#define cx_f_create_datatype_	cx_f_create_datatype
#define cx_f_create_datatypetable_	cx_f_create_datatypetable
#define cx_f_create_sequence_	cx_f_create_sequence
#define cx_f_create_string_	cx_f_create_string
#define cx_f_datatype_		cx_f_datatype
#define cx_f_default_datatypetable_	cx_f_default_datatypetable
#define cx_f_delete_		cx_f_delete
#define cx_f_destroy_		cx_f_destroy
#define cx_f_error_count_	cx_f_error_count
#define cx_f_error_save_	cx_f_error_save
#define cx_f_error_spew_	cx_f_error_spew
#define cx_f_errorqueue_	cx_f_errorqueue
#define cx_f_iprop_		cx_f_iprop
#define cx_f_next_		cx_f_next
#define cx_f_parent_		cx_f_parent
#define cx_f_parse_binary_	cx_f_parse_binary
#define cx_f_pname2datatype_	cx_f_pname2datatype
#define cx_f_prefix2props_	cx_f_prefix2props
#define cx_f_prop_name_		cx_f_prop_name
#define cx_f_realformat_	cx_f_realformat
#define cx_f_reset_		cx_f_reset
#define cx_f_rprop_		cx_f_rprop
#define cx_f_cex_set_listfmt_	cx_f_cex_set_listfmt
#define cx_f_set_datatype_	cx_f_set_datatype
#define cx_f_set_iprop_		cx_f_set_iprop
#define cx_f_set_method_	cx_f_set_method
#define cx_f_set_parent_	cx_f_set_parent
#define cx_f_set_realformat_	cx_f_set_realformat
#define cx_f_set_rprop_		cx_f_set_rprop
#define cx_f_set_sprop_		cx_f_set_sprop
#define cx_f_spewob_		cx_f_spewob
#define cx_f_sprop_		cx_f_sprop
#define cx_f_stdin_		cx_f_stdin
#define cx_f_stdout_		cx_f_stdout
#define cx_f_stderr_		cx_f_stderr
#define cx_f_stream_		cx_f_stream
#define cx_f_stringvalue_	cx_f_stringvalue
#define cx_f_tag2datatype_	cx_f_tag2datatype
#define cx_f_type_		cx_f_type
#define cx_f_type2typename_	cx_f_type2typename
#define cx_f_typename_		cx_f_typename
#define cx_f_typename2type_	cx_f_typename2type
#define cx_f_create_iostream_	cx_f_create_iostream

#endif

/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Linux-ppc appends "__" to each Fortran function name!
======================================================================*/

#if defined(LINUX)

#define cx_f_ancestor_		cx_f_ancestor__
#define cx_f_append_		cx_f_append__
#define cx_f_atend_		cx_f_atend__
#define cx_f_atof_		cx_f_atof__
#define cx_f_atoi_		cx_f_atoi__
#define cx_f_binaryvalue_	cx_f_binaryvalue__
#define cx_f_cex_eof_		cx_f_cex_eof__
#define cx_f_cex_listfmt_	cx_f_cex_listfmt__
#define cx_f_cleanup_		cx_f_cleanup__
#define cx_f_count_		cx_f_count__
#define cx_f_create_binary_	cx_f_create_binary__
#define cx_f_create_datatype_	cx_f_create_datatype__
#define cx_f_create_datatypetable_	cx_f_create_datatypetable__
#define cx_f_create_sequence_	cx_f_create_sequence__
#define cx_f_create_string_	cx_f_create_string__
#define cx_f_datatype_		cx_f_datatype__
#define cx_f_default_datatypetable_	cx_f_default_datatypetable__
#define cx_f_delete_		cx_f_delete__
#define cx_f_destroy_		cx_f_destroy__
#define cx_f_error_count_	cx_f_error_count__
#define cx_f_error_save_	cx_f_error_save__
#define cx_f_error_spew_	cx_f_error_spew__
#define cx_f_errorqueue_	cx_f_errorqueue__
#define cx_f_iprop_		cx_f_iprop__
#define cx_f_next_		cx_f_next__
#define cx_f_parent_		cx_f_parent__
#define cx_f_parse_binary_	cx_f_parse_binary__
#define cx_f_pname2datatype_	cx_f_pname2datatype__
#define cx_f_prefix2props_	cx_f_prefix2props__
#define cx_f_prop_name_		cx_f_prop_name__
#define cx_f_realformat_	cx_f_realformat__
#define cx_f_reset_		cx_f_reset__
#define cx_f_rprop_		cx_f_rprop__
#define cx_f_cex_set_listfmt_	cx_f_cex_set_listfmt__
#define cx_f_set_datatype_	cx_f_set_datatype__
#define cx_f_set_iprop_		cx_f_set_iprop__
#define cx_f_set_method_	cx_f_set_method__
#define cx_f_set_parent_	cx_f_set_parent__
#define cx_f_set_realformat_	cx_f_set_realformat__
#define cx_f_set_rprop_		cx_f_set_rprop__
#define cx_f_set_sprop_		cx_f_set_sprop__
#define cx_f_spewob_		cx_f_spewob__
#define cx_f_sprop_		cx_f_sprop__
#define cx_f_stdin_		cx_f_stdin__
#define cx_f_stdout_		cx_f_stdout__
#define cx_f_stderr_		cx_f_stderr__
#define cx_f_stream_		cx_f_stream__
#define cx_f_stringvalue_	cx_f_stringvalue__
#define cx_f_tag2datatype_	cx_f_tag2datatype__
#define cx_f_type_		cx_f_type__
#define cx_f_type2typename_	cx_f_type2typename__
#define cx_f_typename_		cx_f_typename__
#define cx_f_typename2type_	cx_f_typename2type__
#define cx_f_create_iostream_	cx_f_create_iostream__

#endif

/*============================================================================
 *  c2fstr -- copy C char array to fortran string
 *
 *  This copies MIN(lens,mxout) chars of str to out and returns the number of
 *  characters copied; if less than mxout, out is blank-padded to mxout - 1.
 */

static cx_Integer c2fstr(cx_Integer lens, cx_String str, cx_String out,
                         FLENS mxout)
{
   if (NULL == out  ) return 0;
   if (lens >  mxout) lens = mxout;
   if (NULL == str  ) lens = 0;
   if (0    <  lens ) memcpy(out, str, lens);
   if (lens <  mxout) memset(out + lens, ' ', mxout - lens);
   return lens;
}

/*============================================================================
 *  f2cstr -- copy Fortran string to C string
 */

static cx_String f2cstr(cx_Integer lens, cx_String str)
{
   cx_String string;
   if (0    > lens) lens = 0;
   if (NULL == str) str = "";
   string = (cx_String) malloc(lens + 1);
   strncpy(string, str, lens);
   string[lens] = '\0';
   return string;
}

/*============================================================================
 *  cx_f_ancestor_
 */

cx_Integer cx_f_ancestor_(cx_Object *ob, cx_Integer *type)
{
   return (cx_Integer) cx_ancestor(*ob, *type);
}

/*============================================================================
 *  cx_f_append_
 */

cx_Integer cx_f_append_(cx_Object *ob, cx_Object *addob)
{
   return (cx_append(*ob, *addob) ? FTRUE : FFALSE);
}

/*============================================================================
 *  cx_f_atend_
 */

cx_Integer cx_f_atend_(cx_Object *ob)
{
   return (cx_atend(*ob) ? FTRUE : FFALSE);
}

/*============================================================================
 *  cx_f_atof_
 */

cx_Real cx_f_atof_(cx_String str, FLENS lens)
{
   cx_String  s = f2cstr(lens, str);
   cx_Real    r = cx_atof(s);
   cx_free(s);
   return r;
}

/*============================================================================
 *  cx_f_atoi_
 */

cx_Integer cx_f_atoi_(cx_String str, FLENS lens)
{
   cx_String  s = f2cstr(lens, str);
   cx_Integer i = cx_atoi(s);
   cx_free(s);
   return i;
}

/*============================================================================
 *  cx_f_binaryvalue_
 */

cx_Integer cx_f_binaryvalue_(cx_Integer *plen, cx_Object *ob,
                             cx_String   buf,  FLENS      mx)
{
   cx_Binary  bin = cx_binaryvalue(plen, *ob);
   if( NULL == bin ) return -1;
   return c2fstr(*plen, (cx_String)bin, buf, mx);
}

/*============================================================================
 *  cx_f_cex_eof_
 */

cx_Integer cx_f_cex_eof_(void)
{
   return (cx_cex_eof() ? FTRUE : FFALSE);
}

/*============================================================================
 *  cx_f_cex_listfmt_
 */

cx_Integer cx_f_cex_listfmt_(void)
{
   return (cx_cex_listfmt() ? FTRUE : FFALSE);
}

/*============================================================================
 *  cx_f_cleanup_
 */

void cx_f_cleanup_(void)
{
   cx_cleanup();
}

/*============================================================================
 *  cx_f_count_
 */

cx_Integer cx_f_count_(cx_Object *ob, cx_Integer *type)
{
   return cx_count(*ob, *type);
}

/*============================================================================
 *  cx_f_create_binary_
 */

cx_Integer cx_f_create_binary_(cx_Object *par, cx_Integer *len, cx_Binary *bin)
{
   return (cx_Integer) cx_create_binary(*par, *len, *bin);
}

/*============================================================================
 *  cx_f_create_datatype_
 */

cx_Integer cx_f_create_datatype_( cx_Object  *table,
                                  cx_String  tag,    cx_String  vname,
                                  cx_String  pname,  cx_String  shape,
                                  cx_String  lang,   cx_String  desc,
                                  FLENS      tlen,   FLENS      vlen,
                                  FLENS      plen,   FLENS      slen,
                                  FLENS      llen,   FLENS      dlen)
{
   cx_String t  = f2cstr(tlen, tag  );
   cx_String v  = f2cstr(vlen, vname);
   cx_String p  = f2cstr(plen, pname);
   cx_String s  = f2cstr(slen, shape);
   cx_String l  = f2cstr(llen, lang );
   cx_String d  = f2cstr(dlen, desc );
   cx_Object dt = cx_create_datatype(*table, t, v, p, s, l, d);
   cx_free(t); cx_free(v); cx_free(p); cx_free(s); cx_free(l); cx_free(d);
   return (cx_Integer) dt;
}

/*============================================================================
 *  cx_f_create_datatypetable_
 */

cx_Integer cx_f_create_datatypetable_(cx_Object *par,
                                      cx_String name, cx_Integer lenn)
{
   cx_String str = f2cstr(lenn, name);
   cx_Object rv  = cx_create_datatypetable(*par, str);
   cx_free(str);
   return (cx_Integer) rv;
}

/*============================================================================
 *  cx_f_create_iostream_
 */

cx_Integer cx_f_create_iostream_(cx_String  fn,    cx_Integer *perm,
                                 cx_Integer lenfn )
{
   cx_String   f  = f2cstr(lenfn,fn);
   cx_IOStream fp = cx_e_create_iostream(f, *perm);
   cx_free(f);
   return (cx_Integer) fp;
}

/*============================================================================
 *  cx_f_create_sequence_
 */

cx_Integer cx_f_create_sequence_(cx_Object *ob)
{
   return (cx_Integer) cx_create_sequence(*ob);
}

/*============================================================================
 *  cx_f_create_string_
 */

cx_Integer cx_f_create_string_(cx_Object *par, cx_String s, FLENS lens)
{
   cx_String  str = f2cstr(lens, s);
   cx_Object  rv  = cx_create_string(*par, str);
   cx_free(str);
   return (cx_Integer) rv;
}

/*============================================================================
 *  cx_f_datatype_
 */

cx_Integer cx_f_datatype_(cx_Object *ob)
{
   return (cx_Integer) cx_datatype(*ob);
}

/*============================================================================
 *  cx_f_default_datatypetable_
 */

cx_Integer cx_f_default_datatypetable_(void)
{
   return (cx_Integer) cx_default_datatypetable();
}

/*============================================================================
 *  cx_f_delete_
 */

cx_Integer cx_f_delete_(cx_Object *ob, cx_Object *zapob)
{
   return (cx_delete(*ob, *zapob) ? FTRUE : FFALSE);
}

/*============================================================================
 *  cx_f_destroy_
 */

void cx_f_destroy_(cx_Object *ob)
{
   cx_destroy(*ob);
}

/*============================================================================
 *  cx_f_error_count_
 */

cx_Integer cx_f_error_count_(cx_String str, FLENS lens)
{
   cx_String  s  = f2cstr(lens, str);
   cx_Integer rv = cx_error_count(s);
   cx_free(s);
   return rv;
}

/*============================================================================
 *  cx_f_error_save_
 */

cx_Integer cx_f_error_save_(cx_String msg,  cx_String lev, cx_String src,
                            FLENS mlen,     FLENS llen,    FLENS slen      )
{
   cx_String  m  = f2cstr(mlen, msg);
   cx_String  l  = f2cstr(llen, lev);
   cx_String  s  = f2cstr(slen, src);
   cx_Integer rv = cx_error_save(m, l, s);
   cx_free(m); cx_free(l); cx_free(s);
   return rv;
}

/*============================================================================
 *  cx_f_error_spew_
 */

cx_Integer cx_f_error_spew_(FILE **fp, cx_String lev, FLENS llen)
{
   cx_String  l  = f2cstr(llen, lev);
   cx_Integer rv = cx_error_spew(*fp, l);
   cx_free(l);
   return rv;
}

/*============================================================================
 *  cx_f_errorqueue_
 */

cx_Integer cx_f_errorqueue_(void)
{
   return (cx_Integer) cx_errorqueue();
}

/*============================================================================
 *  cx_f_iprop
 */

cx_Integer cx_f_iprop_(cx_Object *ob, cx_String pname, FLENS plen)
{
   cx_String  p = f2cstr(plen, pname);
   cx_Integer i = cx_iprop(*ob, p);
   cx_free(p);
   return i;
}

/*============================================================================
 *  cx_f_next_
 */

cx_Integer cx_f_next_(cx_Object *ob)
{
   return (cx_Integer) cx_next(*ob);
}

/*============================================================================
 *  cx_f_parent_
 */

cx_Integer cx_f_parent_(cx_Object *ob)
{
   return (cx_Integer) cx_parent(*ob);
}

/*============================================================================
 *  cx_f_parse_binary_
 */

cx_Integer cx_f_parse_binary_(cx_Object *par, cx_String str, FLENS mx)
{
   cx_String s  = f2cstr(mx, str);
   cx_Object rv = cx_parse_binary(*par, s);
   cx_free(s);
   return (cx_Integer) rv;
}

/*============================================================================
 *  cx_f_pname2datatype_
 */

cx_Integer cx_f_pname2datatype_(cx_Object *table, cx_String pname, FLENS mx)
{
   cx_String s  = f2cstr(mx, pname);
   cx_Object dt = cx_pname2datatype(*table, s);
   cx_free(s);
   return (cx_Integer) dt;
}

/*============================================================================
 *  cx_f_prefix2props_
 */

cx_Integer cx_f_prefix2props_(cx_Object *ob, cx_String prefix, FLENS mx)
{
   cx_String s  = f2cstr(mx, prefix);
   cx_Object rv = cx_prefix2props(*ob, s);
   cx_free(s);
   return (cx_Integer) rv;
}

/*============================================================================
 *  cx_f_prop_name_
 */

cx_Integer cx_f_prop_name_(cx_Object *prop, cx_String buf, FLENS mx)
{
   cx_String name = cx_prop_name(*prop);
   return c2fstr(cx_strlen(name), name, buf, mx);
}

/*============================================================================
 *  cx_f_realformat_
 */

cx_Integer cx_f_realformat_(cx_String buf, FLENS mx)
{
   cx_String fmt = cx_realformat();
   if (NULL == fmt) return -1;
   return c2fstr(cx_strlen(fmt), fmt, buf, mx);
}


/*============================================================================
 *  cx_f_reset_
 */

cx_Integer cx_f_reset_(cx_Object *ob)
{
   return cx_reset(*ob);
}

/*============================================================================
 *  cx_f_rprop_
 */

cx_Real cx_f_rprop_(cx_Object *ob, cx_String pname, FLENS plen)
{
   cx_String  p = f2cstr(plen, pname);
   cx_Real    r = cx_rprop(*ob, p);
   cx_free(p);
   return r;
}


/*============================================================================
 *  cx_f_cex_set_listfmt_
 */

void cx_f_cex_set_listfmt_(cx_Integer *boo)
{
   cx_cex_set_listfmt(!!(*boo));
}

/*============================================================================
 *  cx_f_set_datatype_
 */

cx_Integer cx_f_set_datatype_(cx_Object *ob, cx_Object *dt)
{
   return cx_set_datatype(*ob, *dt);
}

/*============================================================================
 *  cx_f_set_iprop_
 */

cx_Integer cx_f_set_iprop_(cx_Object *ob, cx_String pname,
                           cx_Integer *i, FLENS plen)
{
   cx_String  p    = f2cstr(plen, pname);
   cx_Object  prop = cx_set_iprop(*ob, p, *i);
   cx_free(p);
   return (cx_Integer) prop;
}

/*============================================================================
 *  cx_f_set_method_
 */

cx_Integer cx_f_set_method_(cx_Integer *typ, cx_String mname,
                            cx_Method *func, cx_Integer mx)
{
   cx_String  what = f2cstr(mx, mname);
   cx_Integer rv   = cx_set_method(*typ, what, *func);
   cx_free(what);
   return rv;
}

/*============================================================================
 *  cx_f_set_parent_
 */

cx_Integer cx_f_set_parent_(cx_Object *ob, cx_Object *parent)
{
   return cx_set_parent(*ob, *parent);
}

/*============================================================================
 *  cx_f_set_realformat_
 */

void cx_f_set_realformat_(cx_String fmt, FLENS flen)
{
   cx_String s = f2cstr(flen, fmt);
   cx_set_realformat(s);
   cx_free(s);
}

/*============================================================================
 *  cx_f_set_rprop_
 */

cx_Integer cx_f_set_rprop_(cx_Object *ob, cx_String pname,
                           cx_Real *rval, FLENS plen)
{
   cx_String  p    = f2cstr(plen, pname);
   cx_Object  prop = cx_set_rprop(*ob, p, *rval);
   cx_free(p);
   return (cx_Integer) prop;
}

/*============================================================================
 *  cx_f_set_sprop_
 */

cx_Integer cx_f_set_sprop_(cx_Object *ob,
                           cx_String pname, cx_String sval,
                           FLENS plen,      FLENS slen)
{
   cx_String  p    = f2cstr(plen, pname);
   cx_String  s    = f2cstr(slen, sval);
   cx_Object  prop = cx_set_sprop(*ob, p, s);
   cx_free(p); cx_free(s);
   return (cx_Integer) prop;
}

/*============================================================================
 *  cx_f_spewob_
 */

void cx_f_spewob_(FILE **fp, cx_Object *ob, cx_Integer *verb)
{
   cx_spewob(*fp, *ob, *verb);
}

/*============================================================================
 *  cx_f_sprop_
 */

cx_Integer cx_f_sprop_(cx_Object *ob, cx_String pname, cx_String buf,
                       FLENS plen, FLENS mx)
{
   cx_String  p, s;
   cx_Integer rv;
   p  = f2cstr(plen, pname);
   s  = cx_sprop(*ob, p);
   if (NULL == s) { *buf = ' '; return 0; }
   rv = c2fstr(cx_strlen(s), s, buf, mx);
   cx_free(p);
   /* cx_free(s); */
   return rv;
}

/*============================================================================
 *  cx_f_stdin_
 */

cx_Integer cx_f_stdin_(void)
{
   return (cx_Integer)stdin;
}

/*============================================================================
 *  cx_f_stdout_
 */

cx_Integer cx_f_stdout_(void)
{
   return (cx_Integer)stdout;
}

/*============================================================================
 *  cx_f_stderr_
 */

cx_Integer cx_f_stderr_(void)
{
   return (cx_Integer)stderr;
}

/*============================================================================
 *  cx_f_stream_
 */

cx_Integer cx_f_stream_(cx_Object *ob, cx_Integer *type)
{
   return (cx_Integer) cx_stream(*ob, *type);
}

/*============================================================================
 *  cx_f_stringvalue_
 */

cx_Integer cx_f_stringvalue_(cx_Object *ob, cx_String buf, FLENS mx)
{
   cx_String str = cx_stringvalue(*ob);
   if (NULL == str) return -1;
   return c2fstr(cx_strlen(str), str, buf, mx);
}

/*============================================================================
 *  cx_f_tag2datatype_
 */

cx_Integer cx_f_tag2datatype_(cx_Object *table, cx_String tag, FLENS mx)
{
   cx_String s  = f2cstr(mx, tag);
   cx_Object dt = cx_tag2datatype(*table, s);
   cx_free(s);
   return (cx_Integer) dt;
}

/*============================================================================
 *  cx_f_type_
 */

cx_Integer cx_f_type_(cx_Object *ob)
{
   return (cx_Integer) cx_type(*ob);
}

/*============================================================================
 *  cx_f_type2typename_
 */

cx_Integer cx_f_type2typename_(cx_Integer *type, cx_String buf, FLENS mx)
{
   cx_String str = cx_type2typename(*type);
   if (NULL == str) return -1;
   return c2fstr(cx_strlen(str), str, buf, mx);
}

/*============================================================================
 *  cx_f_typename_
 */

cx_Integer cx_f_typename_(cx_Object *ob, cx_String buf, FLENS mx)
{
   cx_String str = cx_typename(*ob);
   if (NULL == str) return -1;
   return c2fstr(cx_strlen(str), str, buf, mx);
}

/*============================================================================
 *  cx_f_typename2type_
 */

cx_Integer cx_f_typename2type_(cx_String name, FLENS mx)
{
   cx_String  str  = f2cstr(mx, name);
   cx_Integer type = cx_typename2type(str);
   cx_free(str);
   return type;
}
