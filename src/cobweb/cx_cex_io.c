/*****************************************************************************
* cx_cex_io.c -- read and write CEX's to/from streams (low-level functions)
*
*  This was adapted from the Daylight contributed program du_tdtio.c.
*
*  cx_Integer cx_e_cex_eof  (void)
*  cx_String  cx_e_cex_read (cx_Object ins)
*  cx_Integer cx_e_cex_write(cx_Object outs, cx_String cex)
*  cx_Object  cx_receive    (cx_Object table, cx_Object ins, cx_Object outs)
*
*  cx_String cx_e_cex_xtagdata(cx_String tree,  cx_Integer *pcxt,
*                              cx_String *ptag, cx_String  *pdat)
*
*  cx_Integer cx_cex_listfmt    (void)
*  void       cx_cex_set_listfmt(cx_Integer boo)
*
*----------------------------------------------------------------------------
*  tags and data are stored as strings tag<data>
*  the ';' character is special as content
*  data must be quoted if it contains >
*
*----------------------------------------------------------------------------
*  To do:
*  [ ] out of memory error weird
*  [ ] out of memory not detected for final character
*  [ ] how about comments, e.g., "   "
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

#define CX_OUT_INCREM 1024

#define START_STATE     0
#define COMMENT_STATE   1
#define TAG_STATE       2
#define LT_STATE        3
#define QUOTED_STATE    4
#define NOTQUOTED_STATE 5
#define GOTQUOTE_STATE  6
#define VERTBAR_STATE   7


/*** Define legal characters in a datatype tag ***/

#define TAG_CHAR(c) \
  (   c == '$'                \
   || c == '_'                \
   || (c >= 'A' && c <= 'Z')  \
   || (c >= 'a' && c <= 'z')  \
   || (c >= '0' && c <= '9'))

/*** Characters not legal in unquoted dataitems other than '>'. ***/

#define NEEDSQUOTES(c) \
  (c == '$' || c == '<' || c == '|')

/*** Comments may appear outside dataitems. ***/

#define COMMENT   '{'
#define UNCOMMENT '}'

/*** Characters that should never occur in a TDT ***/

#define BAD_CHAR(c) \
  (   c == 0        \
   || c == 6        \
   || c == 7        \
   || c == 8        \
   || c == 11       \
   || c == 12       \
   || (c >= 14 && c <= 31) \
   || (c == '\377'))

/*** Static variables for cx_cex_eof(). ***/

static cx_Integer ateof = FALSE;

/*** Static output buffer variables for cx_e_cex_read(). ***/

static char *out    = NULL;
static int   outsiz = 0;
static char *outptr = NULL;
static char *outend = NULL;

/*** Static value of outfmt (for *listfmt()) is initially LIST. ***/

static int  outfmt = CX_FMT_LIST;

/*============================================================================
 *  cx_cex_listfmt() and cx_cex_set_listfmt() -- manage listfmt property
 */

cx_Integer cx_e_cex_listfmt(void)
{
   return outfmt;
}

void cx_e_cex_set_listfmt(cx_Integer boo)
{
   outfmt = boo;
}

/*============================================================================
 *  addchar() -- add character to output buffer, resizing as needed.
 */

static int addchar(char c)
{
   int newsize, offset;
  
   /*** Time to resize buffer? ***/

   if (outptr == outend) {
      if (NULL == out) {
         newsize = 2 * CX_OUT_INCREM;
         out     = cx_malloc(newsize * sizeof(char));
         offset  = 0;
      } else {
         newsize = outsiz * 1.2;
         if (newsize < (outsiz +  2 * CX_OUT_INCREM))
            newsize = outsiz + 2 * CX_OUT_INCREM;
         offset = outptr - out;
         out    = (char *) cx_realloc(out, newsize * sizeof(char));
      }

      if (NULL == out) return FALSE;
      outptr = out + offset;
      outsiz = newsize;
      outend = out + newsize;
   }

   /*** Add character, advance current pointer, return successfully. ***/

   *outptr++ = c;
   return TRUE;
}

/*============================================================================
 *  cx_e_cex_eof() -- returns TRUE iff last cx_e_cex_read() call hit EOF
 */
 
cx_Integer cx_e_cex_eof(void)
{
   return ateof;
}

/*============================================================================
 *  cx_e_cex_read() -- read one CEX from input stream, return as cx_String
 *  
 *  This function is very forgiving about formatting -- it ignores all
 *  whitespace (newlines, carriage returns, tabs, spaces) that falls between
 *  dataitems, and makes no other assumptions about formatting.
 *
 *  On success, returns a cx_String (points to static zero-terminated string).
 *  If called with NULL, deallocates static buffer and returns NULL.
 *  On EOF, returns NULL; cx_e_cex_eof() will return TRUE;
 *  On error, returns NULL; error messages will be queued.
 */

cx_String cx_e_cex_read(cx_Object ins)
{
   char oop[80];
   int   c, ok, state;

   /* Avoid compiler warning! */
   c = 0;

   /*** Not at eof until eof encountered. ***/
  
   ateof = FALSE;

   /*** Deallocate static buffer if called with NULL. ***/
  
   if (NULL == ins) { cx_free(out); out = NULL; outsiz = 0; return NULL; }
  
   /*** Initialize for addchar() ***/

   outptr = out;
   outend = out + outsiz;
  
   /*** "State machine" loop ***/

   ok    = TRUE;
   state = START_STATE;
   while (state != VERTBAR_STATE && EOF != (c = cx_e_iogetc(ins)) && ok) {

      switch (state) {
      case START_STATE:
         switch (c) {
            case ' ':
            case '\t':
            case '\n':
            case '\r': ok = TRUE;                                break;
            case '{':  ok = TRUE;        state = COMMENT_STATE;  break;
            case '|':  ok = addchar(c);  state = VERTBAR_STATE;  break;
            default:   ok = addchar(c);  state = TAG_STATE;      break;
         }
         if (!ok) cx_error_save("out of memory", CX_ERR_ERROR, "cex_read");
         break;

      case COMMENT_STATE:
         switch(c) {
            case '}':  state = START_STATE;  /* fall through */
            default:   ok    = TRUE;  break;
         }
         break;

      case TAG_STATE:
         if (c == '<') {
	    state = LT_STATE;
	    if (!(ok = addchar(c)))
               cx_error_save("out of memory", CX_ERR_ERROR, "cex_read");
	 } else if (!TAG_CHAR(c)) {
	    sprintf(oop, "bad tag character '%c'", c);
            cx_error_save(oop, CX_ERR_ERROR, "cex_read");
	    ok = FALSE;
	 } else if (!(ok = addchar(c))){
            cx_error_save("out of memory", CX_ERR_ERROR, "cex_read");
         }
         break;
        
      case LT_STATE:
         ok = addchar(c);
         switch (c) {
            case '"':  state = QUOTED_STATE;     break;
            case '>':  state = START_STATE;      break;
            default:   state = NOTQUOTED_STATE;  break;
         }
         break;

      case QUOTED_STATE:
         ok = addchar(c);
         if (c == '"') state = GOTQUOTE_STATE;
         break;

      case NOTQUOTED_STATE:
         ok = addchar(c);
         switch (c) {
            case '"': state = QUOTED_STATE;  break;
            case '>': state = START_STATE;   break;
         }
         break;

      case GOTQUOTE_STATE:
         ok = addchar(c);
         switch (c) {
            case '"': state = QUOTED_STATE;     break;
            case '>': state = START_STATE;      break;
            default:  state = NOTQUOTED_STATE;  break;
         }
         break;

      default: /* shouldn't happen */
         ok = TRUE;
      }
   }

   /*** Mark eof. ***/

   if (EOF == c) ateof = TRUE;

   /*** Really at end. ***/

   if (outptr == out) return NULL;

   /*** Check for proper CEX termination. ***/

   if (ok && '|' != *(outptr - 1)) {
      cx_error_save("Incomplete CEX input.", CX_ERR_ERROR, "cex_read");
      return NULL;
   }

   /*** Zero-terminate then deal with out of memory condition. ***/

   if (ok) ok = addchar('\0');

   if (!ok) {
      cx_free(out);
      out    = NULL;
      outsiz = 0;
      cx_error_save("out of memory", CX_ERR_ERROR, "cex_read");
      return NULL;
   }

   /*** Return zero-terminated tree as static cx_String. ***/

   return (cx_String) out;
}


/*============================================================================
 *  cx_e_cex_write() -- write CEX to stream, return number of chars written.
 *
 *  If outfmt != CX_FMT_RAW,  adds `\n' after unquoted '|' if not there.
 *  If outfmt == CX_FMT_LIST, adds `\n' after unquoted '>' if not there.
 */
 
cx_Integer cx_e_cex_write(cx_Object outs, cx_String cex)
{
   char *p;
   int   count = 0, quoted = 0, doublequote;
  
   for (p = cex; *p; p++) {
      switch (*p) {

      /*** Deal with quote character. ***/

      case '"':
         cx_e_ioputc(*p, outs);
         count++;

         /*** Print 2nd quote if doublequote. ***/

         doublequote = ('"' == *(p+1));
         if (doublequote) { cx_e_ioputc(*(++p), outs); count++; }

         /*** Update quoted state. ***/

         quoted = (quoted ? doublequote : !doublequote);
         break;

      /*** Deal with characters generating possible end-of-line. ***/

      case '|':
      case '>':
         cx_e_ioputc(*p, outs);
         count++;

         /*** If unquoted, write newline if '|' or ('>' and list format) ***/

         if (!quoted && CX_FMT_RAW != outfmt) {
            if ('|' == *p || CX_FMT_LIST == outfmt) {
	       cx_e_ioputc('\n', outs);
	       count++;
	    }
         }

         /*** Skip trailing whitespace. ***/

         if (!quoted) {        
           while (*(p+1) && isspace(*(p+1)))
             p++;
         }
         break;

      /*** Normal characters. ***/

      default:
         cx_e_ioputc(*p, outs);
         count++;
         break;
      }
   }
   return count;
}

/*============================================================================
 *  is_special() -- is character treated specially in a datafield?
 */

static int is_special(char c)
{
   switch (c) {
      case '$':  case '<':  case ';':  case '>':  case '|':  return TRUE;
   }
   return FALSE;
}

/*============================================================================
 *  cx_needs_quotes -- does string require quoting?
 */

cx_Integer cx_needs_quotes(cx_String str)
{
   if (str) {
      for ( ; *str; str++)
         if (is_special(*str) || '"' == *str) return TRUE;
   }
   return FALSE;
}

/*============================================================================
 *  cx_find_char -- find a character in str ignoring quoted portions of string
 */

cx_String cx_find_char(cx_String str, cx_String sc)
{
   char *p;
   int   quoted = FALSE;
  
   /*** Return NULL if string is NULL. ***/

   if (NULL == str) return NULL;

   /*** Loop over NULL-terminated string. ***/

   for (p = str; *p; p++) {

      /*** If not quoted, either quote or test for character. ***/

      if (!quoted) {
         if      ('"' == *p     ) quoted = TRUE;
         else if (strchr(sc, *p)) return p;

      /*** Else ignore except for quoted quote. ***/

      } else if ('"' == *p) {

         /*** Switch on next character. ***/

         switch (*(p + 1)) {
	    case '"':  p++;  break;            /* skip double quote */
	    case '\0':
	    default:   quoted = FALSE; break;  /* end of string or end quote */
         }
      }
   }

   /*** Not found. ***/

   return NULL;
}

/*============================================================================
 *  cx_unquote_dup -- remove quotes around string and un-double-quote
 *
 *  Duplicates input string, undoubling quotes if sin begins with '"'.
 */

cx_String cx_unquote_dup(cx_String sin)
{
   char  c, *p, *q, *str;

   /*** Return NULL if input is NULL. ***/

   if (NULL == sin) return NULL;

   /*** Duplicate string, return it if input doesn't start '"'. ***/

   str = cx_strdup(sin);
   if ('"' != *str) return str;

   /*** Loop over string. ***/

   p = str + 1;
   q = str;
   while( *p ) {
      c = *p++;
      if      ('"'  != c ) *q++ = c;      /* copy normal char */
      else if ('\0' == *p) break;         /* skip final quote */
      else if ('"'  == *p) *q++ = *p++;   /* un-double-quote */
      else                 break;         /* end quote, oops */
   }
   *q = '\0';

   /*** Return unquoted string. ***/

   return str;
}

/*============================================================================
 *  cx_unquote_ndup -- remove quotes around string and un-double-quote
 *
 *  Duplicates input string, undoubling quotes if sin begins with '"'.
 */

cx_String cx_unquote_ndup(cx_String sin, int lens)
{
   char  c, *p, *q, *str;

   /*** Return NULL if input is NULL. ***/

   if (NULL == sin) return NULL;

   /*** Duplicate string, return it if input doesn't start '"'. ***/

   str = (char *) cx_malloc((lens + 1) * sizeof(char));
   if (NULL == str) return NULL;
   strncpy(str, sin, lens);
   str[lens] = '\0';
   if ('"' != *str) return str;

   /*** Loop over string. ***/

   p = str + 1;
   q = str;
   while ((lens--) && (c = *p++)) {
      if      ('"'  != c ) *q++ = c;      /* copy normal char */
      else if ('\0' == *p) break;         /* skip final quote */
      else if ('"'  == *p) *q++ = *p++;   /* un-double-quote */
      else                 break;         /* end quote, oops */
   }
   *q = '\0';

   /*** Return unquoted string. ***/

   return str;
}

/*============================================================================
 *  cx_quote_dup -- surround string with quotes, doubling existing quotes.
 *
 *  Returns newly malloc-ed string.
 */

cx_String cx_quote_dup(cx_String str)
{
   char *p, *q, *qout;
   int   size;

   /*** Treat NULL like empty string. ***/

   if (NULL == str) return cx_strdup("");

   /*** Count number of characters needed. ***/

   for (size = 3, p = str; *p; p++)
      size += ('"' == *p ? 2 : 1);

   /*** Allocate quoted string buffer and point to it. ***/

   q = qout = (char *) cx_malloc(size * sizeof(char));

   /*** Copy string, doubling existing quotes. ***/

   *q++ = '"';
   for (p = str; *p; p++) {
      if ('"' == *p) *q++ = '"';
      *q++ = *p;
   }
   *q++ = '"';
   *q++ = '\0';

   /*** Return quoted string. ***/

   return qout;
}

/*============================================================================
 *  cx_e_cex_xtagdata() -- parse next data item in tree
 *
 *  Skips leading whitespace and finds next tag and data.
 *  If `tag' and/or `data' are non-NULL, fills with newly-malloc-ed cx_String.
 *  If `cxt' is non-NULL, sets to context flag per cx_cex_io.h.
 *  Returns pointer just after end-of-data delimiter ">", or NULL.
 */

cx_String cx_e_cex_xtagdata(cx_String tree,  cx_Integer *pcxt,
			    cx_String *ptag, cx_String  *pdat)
{
    char *p, *lt, *gt, *t = NULL, *d = NULL;
    int  lens, x;

    /*** Initialize output. ***/

    if (pcxt) *pcxt = 0;
    if (ptag) *ptag = NULL;
    if (pdat) *pdat = NULL;
 
    /*** Skip leading whitespace. ***/

    for (p = tree; *p; p++)
       if (!isspace(*p)) break;

    /*** Look for data delimiters (BOD also delimits end-of-tag). ***/

    lt = cx_find_char(p, "<");
    gt = cx_find_char(p, ">");
    if (NULL == lt || NULL == gt || gt < lt) return NULL;

    /*** Determine context from first non-blank character. ***/

    switch (*p) {
       case '$':  x = CX_CXT_IDENTIFIER;  p++;  break;
       case '/':  x = CX_CXT_PROPERTY;    p++;  break;
       default:   x = CX_CXT_DATAITEM;          break;
    }

    /*** Copy tag if needed. ***/

    if (ptag) {
       lens = (lt - p);
       if (NULL == (t = cx_strndup(p, lens))) return NULL;
/*
 * t    = (cx_String) cx_malloc((lens + 1) * sizeof(char));
 * if (NULL == t) return NULL;
 * strncpy(t, p, lens);
 * t[lens] = '\0';
 */
    }

    /*** Copy data and unquote as needed. ***/

    if (pdat) {
       lens = (gt - lt) - 1;
       d = cx_unquote_ndup(lt + 1, lens); /* DO DW */
       if (NULL == d) { cx_free(t); return NULL; }
       /*
       if (NULL == (d = cx_strndup(lt + 1, lens))) { cx_free(t); return NULL; }
       */
    }

    /*** Copy pointers to output and return happily. ***/

    if (pcxt) *pcxt = x;
    if (ptag) *ptag = t;
    if (pdat) *pdat = d;

    return (gt + 1);
}

/*============================================================================
 *  cx_e_register_root_type() and root_tag2type() -- record root tag vs type
 *
 *  This determines which root datatypes are mapped to objects.
 *  Not a public function ... this is called by package initialization.
 */

typedef struct _cx_root_type {
   cx_String             tag;
   cx_Integer            type;
   struct _cx_root_type *next;
} CX_ROOT_CLASS;

static CX_ROOT_CLASS *rootclasses = NULL;

static CX_ROOT_CLASS *root_tag2rootclass(cx_String tag)
{
   CX_ROOT_CLASS *rc;
   for (rc = rootclasses; rc; rc = rc->next)
      if (0 == cx_strcmp(tag, rc->tag)) return rc;
   return NULL;
}

static cx_Integer root_tag2type(cx_String tag)
{
   CX_ROOT_CLASS *rc = root_tag2rootclass(tag);
   return (rc ? rc->type : CX_OB_INVALID);
}

/*** EXPERIMENTAL SEMI-PUBLIC FUNCTION FOR 033. ***/

cx_Integer cx_tag2roottype(cx_String tag)
{
   return (tag ? root_tag2type(tag) : CX_OB_INVALID);
}

cx_Integer cx_e_register_root_type(cx_String tag, cx_Integer type)
{
   CX_ROOT_CLASS *rc;
   cx_String      func = "regroottyp";

   /*** Reject NULL tags and INVALID types. ***/
       
   if (NULL == tag || CX_OB_INVALID == type) return FALSE;

   /*** If root class with given tag doesn't exist, make it. ***/
       
   if (NULL == (rc = root_tag2rootclass(tag))) {
      rc          = (CX_ROOT_CLASS *) cx_malloc(sizeof(CX_ROOT_CLASS));
      rc->tag     = cx_strdup(tag);
      rc->type    = type;
      rc->next    = rootclasses;
      rootclasses = rc;

   /*** If extant root class is not identical, update it and note. ***/
       
   } else if (type != rc->type) {
      rc->type = type;
      cx_error_save("redefining root type for:", CX_ERR_NOTE, func);
      cx_error_save(tag,                         CX_ERR_NOTE, func);
   }

   /*** Return successfully. ***/

   return TRUE;
}

/*============================================================================
 *  cx_e_deregister_root_types() -- private clean up function
 */

void cx_e_deregister_root_types( void )
{
   CX_ROOT_CLASS *rc, *next;

   for (rc = rootclasses; rc; rc = next) {
      next = rc->next;
      cx_free(rc->tag);
      cx_free(rc);
   }
   rootclasses = NULL;
}

/*============================================================================
 *  cx_e_receive() -- interpret generic cex datatree, return NULL on error
 *
 *  This returns the first object of a registered type (but not datatypes).
 *  Datatypes on input are installed in the specified datatype table.
 *  If 'outs' is not NULL, copies unknown CEX objects to output
 *  This uses the polymorphic `cx_e_cexin' messages to do the real work.
 */

cx_Object  cx_e_receive(cx_Object tabin, cx_Object ins, cx_Object outs)
{
   cx_Object  dt, rootob, table;
   cx_Integer cxt, obtype;
   cx_String  cex, tag, data;
   cx_String func = "cx_receive";

   /*** If tabin is NULL, use default table (ensures method init). ***/
       
   table  = (tabin ? tabin : cx_default_datatypetable());

   /*** Read input until first non-datatype interpreted into rootob. ***/
 
   rootob = NULL;

   do {

      /*** Read next CEX entry, break on EOF. ***/
 
      cex = cx_e_cex_read(ins);
      if (cx_e_cex_eof()) break;

      /*** Return in error if CEX entry failed (but not EOF). ***/
 
      if (!cex) {
         cx_error_save("error reading cex", CX_ERR_ERROR, func);
         break;
      }

      /*** Interpret first context-tag-data, dispatch on type. ***/
 
      if (NULL != cx_e_cex_xtagdata(cex, &cxt, &tag, &data)) {
 
         /*** Root dataitem must be an identifier. ***/
 
         if (CX_CXT_IDENTIFIER != cxt) {
            cx_error_save("non-identifier root dataitem:", CX_ERR_ERROR, func);
            cx_error_save(tag,                             CX_ERR_ERROR, func);
            cx_error_save(data,                            CX_ERR_ERROR, func);
 
         /*** Consume datatypes. ***/
 
         } else if (0 == strcmp("D", tag)) {
            cx_e_cexin(cex, CX_OB_DATATYPE, table);
 
         /*** Find datatype. ***/
 
	 } else if (NULL == (dt = cx_tag2datatype(table, tag))) {
            cx_error_save("unknown root datatype:", CX_ERR_ERROR, func);
            cx_error_save(tag,                      CX_ERR_ERROR, func);
            cx_error_save(data,                     CX_ERR_ERROR, func);
 
         /*** Find language. ***/
 
	 } else if (NULL == cx_sprop(dt, "language")) {
            cx_error_save("datatype missing language:", CX_ERR_ERROR, func);
            cx_error_save(tag,                          CX_ERR_ERROR, func);
            cx_error_save(data,                         CX_ERR_ERROR, func);
 

         /*** Parse as registered type. ***/
         } else if (CX_OB_INVALID != (obtype = root_tag2type(tag))) {
            rootob = cx_e_cexin(cex, obtype, table);

         /*** If outs is specified, copy out unregistered trees and note. ***/
 
         } else if (outs) {
            cx_e_cex_write(outs, cex);
            cx_error_save("CEX object of unregistered type copied to output:",
			  CX_ERR_NOTE, func);
            cx_error_save(tag, CX_ERR_NOTE, func);

         /*** Else, generate a warning. ***/
 
         } else {
            cx_error_save("CEX object of unregistered type encountered:",
			  CX_ERR_WARN, func);
            cx_error_save(tag, CX_ERR_WARN, func);
         }
	 cx_free(tag);
	 cx_free(data);
      }
   } while( rootob == NULL );

   /*** Return root object or NULL at EOF. ***/

   return rootob;
}
