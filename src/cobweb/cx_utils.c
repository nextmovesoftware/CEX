/*****************************************************************************
*  cx_utils.c -- simple CX support utilities
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
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <memory.h>

#include "cx_limits.h"
#include "cx_iostream.h"
#include "cx_utils.h"

/*============================================================================
 *  cx_e_panic() -- write a panic message
 *
 *  This writes a message using only low level calls, i.e., doesn't use the
 *  error facility (since the message might be "out of memory").
 */

void cx_e_panic(cx_String msg, cx_String src)
{
   if (src) fprintf(stderr, "PANIC! (%s) %s\n", src, msg ? msg : "?");
   else     fprintf(stderr, "PANIC! %s\n", msg ? msg : "?");
}

/*============================================================================
 *  cx_e_malloc() -- like malloc() but generates error if out of memory
 *
 *  If one needs to use a different memory allocator, here's the place for it.
 */

void *cx_e_malloc(cx_Integer size)
{
    register void *ptr;

    ptr = malloc(size);
    if( !ptr ) cx_e_panic("out of memory", "cx_malloc");
    return ptr;
}

/*============================================================================
 *  cx_e_realloc() -- like realloc() but generates error if out of memory
 */

void *cx_e_realloc(void *ptr, cx_Integer size)
{
    register void *newptr;

    newptr = realloc(ptr, size);
    if( !newptr ) cx_e_panic("out of memory", "cx_realloc");
    return newptr;
}

/*============================================================================
 *  cx_e_free() -- like free() but successful no-op if passed NULL
 */

void cx_e_free(void *ptr)
{
    if (ptr) free(ptr);
}

/*============================================================================
 *  cx_e_atoi() -- like atoi(), but returns 0 when called with NULL
 */

cx_Integer cx_e_atoi(cx_String str)
{
    if( !str ) return 0;
    return atoi(str);
}

/*============================================================================
 *  cx_e_atof() -- like atof(), but returns 0.0 when called with NULL
 */

cx_Real cx_e_atof(cx_String str)
{
    if( !str ) return 0.0;
    return atof((char *)str);
}

/*============================================================================
 *  cx_e_strlen() -- like strlen(), but returns 0 when called with NULL
 */

cx_Integer cx_e_strlen(cx_String str)
{
    if( !str ) return 0;
    return strlen(str);
}

/*============================================================================
 *  cx_e_strdup() -- like strdup(), but returns NULL when called with NULL
 */

cx_String cx_e_strdup(cx_String str)
{
    register cx_Integer lens;
    register cx_String copy;

    if( !str ) return (cx_String)0;

    lens = strlen(str);
    copy = (cx_String)cx_malloc(lens+1);
    memcpy(copy, str, lens);
    copy[lens] = '\0';
    return copy;
}

/*============================================================================
 *  cx_e_strndup() -- you guessed it: returns NULL when called with NULL
 *  Always returns a null-terminate string.
 */

cx_String cx_e_strndup(cx_String str, cx_Integer lens)
{
   cx_String copy;
   if (NULL == str) return NULL;
   copy = (cx_String) cx_malloc(lens + 1);
   memcpy(copy, str, lens);
   copy[lens] = '\0';
   return copy;
}

/*============================================================================
 *  cx_e_strcat() -- like strcat(), handles NULL arguments gracefully
 */

cx_String cx_e_strcat(cx_String s1, cx_String s2)
{
   if (NULL == s1) return NULL;   /* not much we can do about that */
   if (NULL == s2) return s1;     /* like concatenating an empty string */
   return strcat(s1, s2);
}

/*============================================================================
 *  cx_e_strcmp() -- like strcmp(), but handles NULL arguments
 *
 *  A NULL string is treated as equivalent to an empty string.
 */

cx_Integer cx_e_strcmp(cx_String s1, cx_String s2)
{
   /*** Normal string comparison. ***/

   if (s1 && s2) return (cx_Integer) strcmp((char *) s1, (char *) s2);

   /*** Return 0 if both are NULL, else first char of non-NULL string. ***/

   return (cx_Integer) (s1 ? *s1 : (s2 ? *s2 : 0));
}

/*============================================================================
 *  cx_e_strncmp() -- like strncmp(), but handles NULL arguments
 *
 *  A NULL string is treated as equivalent to an empty string.
 */

cx_Integer cx_e_strncmp(cx_String s1, cx_String s2, cx_Integer lens)
{
   /*** Normal string comparison. ***/

   if (s1 && s2)
      return (cx_Integer) strncmp((char *) s1, (char *) s2, (int) lens);

   /*** Return 0 if both are NULL, else first char of non-NULL string. ***/

   return (cx_Integer) (s1 ? *s1 : (s2 ? *s2 : 0));
}

/*============================================================================
 *  cx_e_strqbrk -- like strpbrk, but ignores quoted portions of string
 *                  ... and, of course, handles NULL arguments gracefully
 */

cx_String cx_e_strqbrk(cx_String str, cx_String sc)
{
   char *p;
   int   quoted = FALSE;
  
   /*** Return NULL if string is NULL. ***/

   if (NULL == str || NULL == sc) return NULL;

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
 *  cx_e_scratchpad() -- a more efficient approach to strcat()
 *
 *  cx_scratchpad(NULL) .... reset scratch pad, return NULL
 *  cx_scratchpad(str) ..... append str to scratchpad, return it
 */

cx_String cx_e_scratchpad(cx_String str)
{
   static char *buf   = NULL;
   static int   room  = 0;
   static int   lenb  = 0;
   static int   armed = 0;
   int          lens;

   /*** If called with NULL once, clear; if twice, reset everything. ***/

   if (NULL == str) {
      if (armed) { cx_free(buf); room = lenb = armed = 0; }
      else       { lenb = 0; armed = 1; }
      return NULL;
   }
   armed = 0;

   /*** Else make room if needed. ***/

   lens = strlen(str);
   if (lenb + lens + 1 > room) {
      room = CX_MAX(1024, 2 * room + 2 * lens + 1);
      if (NULL == buf) buf  = (char *) cx_malloc(room * sizeof(char));
      else             buf  = (char *) cx_realloc(buf, room * sizeof(char));
      if (NULL == buf) cx_e_panic("out of memory", "cx_scratchpad");
   }

   /*** Append string to end of buf, zero-terminate, and return buf. ***/

   if (0 < lens) memcpy(buf + lenb, str, lens);
   lenb      += lens;
   buf[lenb]  = '\0';
   return buf;
}

