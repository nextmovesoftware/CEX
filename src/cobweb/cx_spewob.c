/*****************************************************************************
*  cx_spewob.c -- print object as a tree
*
*  Note that this is a user-level function (doesn't use cx_ internals).
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

/********************************** moved to cx_spewob.h *********************
*  #define CX_SPEW_NONE       0
*  #define CX_SPEW_DATA       1
*  #define CX_SPEW_CHILDREN   2
*  #define CX_SPEW_PROPS      4
*  #define CX_SPEW_PROPNAME   8
*  #define CX_SPEW_DATATAG   16
*  #define CX_SPEW_CLASS     32
*  #define CX_SPEW_SUMTUPLE  64
*  #define CX_SPEW_DEFAULT  (CX_SPEW_DATA | CX_SPEW_PROPS | CX_SPEW_CHILDREN)
*****************************************************************************/

#define CX_INDENT  20
#define CX_WRAPCOL 79

/*============================================================================
 *  sayprefix() -- print leading vertical bars and spaces as per level `lev'
 *
 *  This returns the number of characters written.
 */

static int sayprefix(FILE *fp, cx_Integer lev)
{
   int nout = 0;
   fprintf(fp, "|");  nout++;
   while (lev--) { fprintf(fp, "  |"); nout += 2; }
   fprintf(fp, " ");  nout ++;
   return nout;
}

/*============================================================================
 *  saydelim() -- print horizontal delimiter with prefix as per level `lev'
 */

static void saydelim(FILE *fp, cx_Integer lev)
{
   int n = 78;
   while (0 < lev--) { fprintf(fp, "|  "); n -= 3; }
   fprintf(fp, "+");
   while (n--) fprintf(fp, "-");
   fprintf(fp, "\n");
}

/*============================================================================
 *  tidybreak() -- find a tidy place to break line
 */

static char *tidybreak(char *str, int lim)
{
   char *s, *tb, *last = str + lim;     /* set max limit  */
   char *qu, *sp, *co, *sp2, *co2;      /* quote, space, semi */
   int  lens = cx_strlen(str);          /* measure string */
   int  quoted = FALSE;                 /* in quotes?     */

   /*** If string is short enough to fit, use it all. ***/

   if (lens <= lim) return (str + lens);

   /*** Find last endquote, comma, and space. ***/
   
   for (s = qu = sp = co = sp2 = co2 = str; *s && s < last; s++) {
      switch (*s) {
         case '"':  quoted = !quoted;  if (!quoted) qu = s;  break;
         case '\t': /* tab same as space */
         case ' ':  sp2 = s; if (!quoted) sp = s;  break;
         case ':':  /* colon same as comma */
         case ';':  /* semicolon same as comma */
         case ',':  co2 = s; if (!quoted) co = s;  break;
         case '\\': if ('n' == *(s+1)) return s;  /* absolute priority */
      }
   }

   /*** Pick tidy place to break in the last half of space allowed. ***/

   s  = str + lim / 2;
   tb = CX_MAX(sp, co);        /* last unquoted space or comma ... */
   if (tb  > s) return tb;     /* ... if in last half of line,     */
   if (qu  > s) return qu;     /* else endquote in last half,      */
   if (sp2 > s) return sp2;    /* else quoted space in last half,  */
   if (co2 > s) return co2;    /* else quoted comma in last half,  */

   return last;                /* else no tidy place, truncate.    */
}

/*============================================================================
 *  say() -- print a line containing tag and string data
 */

static void say(FILE *fp, cx_Integer lev, cx_String tag, cx_String sdata)
{
   int   pout, nout, lentag, n, nl;
   char *s, *tidy;

   /*** Print prefix and tag, measuring number of characters written. ***/

   lentag = cx_strlen(tag);            /* measure tag */
   n      = CX_INDENT - lentag;        /* number of leading dots */
   nout   = sayprefix(fp, lev);        /* write prefix, save length */
   pout   = nout + CX_INDENT + 2;      /* continuations indented from here */
   fprintf(fp, "%s ", tag);            /* write tag */
   nout  += lentag + 1;                /* update output counter */

   /*** Write dots until data column. ***/

   while (0 < n--) { fprintf(fp, "."); nout++; }
   fprintf(fp, " ");
   nout++;

   /*** If no data, we're done. ***/

   if (NULL == sdata) { fprintf(fp, "\n"); return; }

   /*** Else wrap (newline stuff added to v200, DW). ***/

   tidy = tidybreak(sdata, CX_WRAPCOL - nout - 2);
   for (s = sdata; *s; s++) {
      nl = ('\\' == *s && 'n' == *(s+1));
      if (!nl) { fprintf(fp, "%c", *s); nout++; }
      if (nl || tidy == s) {
	 if (nl) s++;
         fprintf(fp, "\n");
         nout = sayprefix(fp, lev);
         while (nout < pout)  { fprintf(fp, " "); nout++; }
         tidy = tidybreak(s + 1, CX_WRAPCOL - nout - 2);
      }
   }
   fprintf(fp, "\n");
}

/*============================================================================
 *  tuplesum -- summarize tuple given datatype and stringvalue
 */

static cx_String tuplesum(cx_Object dt, cx_Object ob)
{
   static char buf[80];
   cx_String   p, q, str, typename, shape, lang;
   int         dim, count, empty;

   /*** If not a tuple or multidimensional, return stringvalue. ***/

   typename = cx_typename(ob);
   shape    = cx_sprop(dt, "shape");
   dim      = cx_atoi(shape);
   if (2 > dim && NULL == strstr(typename, "tuple")) return cx_stringvalue(ob);

   /*** Count full and empty elements. ***/

   str = cx_strdup(cx_stringvalue(ob));
   for (count = empty = 0, p = q = str; *p; p++)
      if (',' == *p) { count++; if (p == q) empty++; q = p + 1; }
   count++;
   if (p == q) empty++;
   cx_free(str);

   /*** Also get language. ***/

   lang = cx_sprop(dt, "language");

   /*** Create summary in static buffer and return it. ***/

   if (empty)
      sprintf(buf, "* %d-D %s * %d %s values * %d empty *",
              dim, typename, count, lang, empty);
   else
      sprintf(buf, "* %d-D %s * %d %s values *",
              dim, typename, count, lang);

   return buf;
}

/*============================================================================
 *  vnameof -- return vname/tag/pname of datatype as per flag verb
 */

static cx_String vnameof(cx_Object dt, cx_Integer verb)
{
   static char buf[80];

   /*** If neither or one or the other, return appropriate value. ***/

   switch (verb & (CX_SPEW_DATATAG | CX_SPEW_PROPNAME)) {
      case 0:                return cx_sprop(dt, "verbose name" );
      case CX_SPEW_DATATAG:  return cx_sprop(dt, "tag"          );
      case CX_SPEW_PROPNAME: return cx_sprop(dt, "property name");
   }

   /*** Else, get fancy and do both. ***/

   sprintf(buf, "%s/%s", cx_sprop(dt, "tag"), cx_sprop(dt, "property name"));
   return buf;
}

/*============================================================================
 *  spewob -- internal function is directly recursive
 *
 *  verb semantics as per cx_spewob.h
 *
 *  CX_SPEW_DATA ........ show data
 *  CX_SPEW_CHILDREN .... show children recursively
 *  CX_SPEW_PROPS ....... show properties
 *  CX_SPEW_PROPNAME .... label data with property name
 *  CX_SPEW_DATATAG ..... label data with raw datatag
 *  CX_SPEW_CLASS ....... show object class
 *  CX_SPEW_SUMTUPLE .... summarize tuples
 *
 *  e.g., 7 = 000111 = print data, children, and properties
 */

static void spewob(FILE *fp, cx_Object ob, cx_Integer lev, cx_Integer verb)
{
   cx_Object prop, props, kids, kid, dt, pdt;
   cx_String pname;
   int       type;

   /*** Find object class and decide whether to recurse. ***/

   if (ob && verb) {
      type     = cx_type(ob);

      /*** Deal with invalid objects specially (shouldn't happen). ***/

      if (CX_OB_INVALID == type) {
         say(fp, lev, "INVALID OBJECT", "OBJECT HAS NO CLASS");

      /*** Print objects with associated datatypes. ***/

      } else if (NULL != (dt = cx_datatype(ob))) {

         /*** Print delimiter, verbose name (or class), and stringvalue. ***/

         saydelim(fp, lev);
         if (verb & CX_SPEW_DATA) {
            if (verb & CX_SPEW_SUMTUPLE)
               say(fp, lev, vnameof(dt, verb), tuplesum(dt, ob));
            else
               say(fp, lev, vnameof(dt, verb), cx_stringvalue(ob));
         }

         /*** Print additional information about object if needed. ***/

         if (verb & CX_SPEW_CLASS) {

            /*** Print object class. ***/

            say(fp, lev, "(object class)", cx_typename(ob));

            /*** Print methods. (SOMEDAY) ***/

            /* spewob(fp, cx_zapfunc(ob), lev + 1, FALSE); */
         }

         /*** Print object's props if needed & datatype is available. ***/

         if (verb & CX_SPEW_PROPS) {
            props = cx_stream(ob, CX_OB_PROPERTY);
            while (NULL != (prop = cx_next(props))) {
               pname = cx_prop_name(prop);
               if (NULL != (pdt = cx_pname2datatype(cx_parent(dt), pname))) {
                  if (verb & CX_SPEW_SUMTUPLE)
                     say(fp, lev, vnameof(pdt, verb), tuplesum(pdt, prop));
                  else
                     say(fp, lev, vnameof(pdt, verb), cx_stringvalue(prop));
               }
            }
            cx_destroy(props);
         }

         /*** Maybe print datatype as child, but not datatype's datatype. ***/

         /*
          * if (vtyp) spewob(fp, cx_datatype(ob), lev + 1, verb);
          */

         /*** Spew children. ***/

         if (verb & CX_SPEW_CHILDREN) {
            kids = cx_stream(ob, CX_OB_ANY);
            while (NULL != (kid = cx_next(kids)))
               spewob(fp, kid, lev + 1, verb);
            cx_destroy(kids);
         }
      }
   }

   if (0 == lev) saydelim(fp, lev);

   return;
}

/*============================================================================
 *  cx_spewob -- public entry point
 */

void cx_e_spewob(FILE *fp, cx_Object ob, cx_Integer verb)
{
   spewob(fp, ob, 0, verb);
}
