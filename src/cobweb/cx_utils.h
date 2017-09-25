/*****************************************************************************
*  cx_utils.h -- declarations for CX utility package
*
*  The "utils" package doesn't support an object class, it just provides some
*  handy utilities.
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

#ifndef CX_UTILS_INCLUDED
#define CX_UTILS_INCLUDED 1

#include "cx_basics.h"

/*** C-wrappers. ***/

#define cx_panic           cx_e_panic
#define cx_malloc          cx_e_malloc
#define cx_realloc         cx_e_realloc
#define cx_free            cx_e_free
#define cx_atof            cx_e_atof
#define cx_atoi            cx_e_atoi
#define cx_strlen          cx_e_strlen
#define cx_strdup          cx_e_strdup
#define cx_strndup         cx_e_strndup
#define cx_strcat          cx_e_strcat
#define cx_strcmp          cx_e_strcmp
#define cx_strncmp         cx_e_strncmp
#define cx_strqbrk         cx_e_strqbrk
#define cx_scratchpad      cx_e_scratchpad

/*** Utility function declarations. ***/

void        cx_e_panic     (cx_String   msg, cx_String  src                );
void       *cx_e_malloc    (cx_Integer  size                               );
void       *cx_e_realloc   (void       *ptr, cx_Integer size               );
void        cx_e_free      (void       *ptr                                );
cx_Real     cx_e_atof      (cx_String   str                                );
cx_Integer  cx_e_atoi      (cx_String   str                                );
cx_Integer  cx_e_strlen    (cx_String   str                                );
cx_String   cx_e_strdup    (cx_String   str                                );
cx_String   cx_e_strndup   (cx_String   str, cx_Integer lens               );
cx_String   cx_e_strcat    (cx_String   s1,  cx_String  s2                 );
cx_Integer  cx_e_strcmp    (cx_String   s1,  cx_String  s2                 );
cx_Integer  cx_e_strncmp   (cx_String   s1,  cx_String  s2, cx_Integer lens);
cx_String   cx_e_strqbrk   (cx_String   str, cx_String  sc                 );
cx_String   cx_e_scratchpad(cx_String   str                                );

#endif /* CX_UTILS_INCLUDED */
