/*****************************************************************************
*  cx_types.h -- CX object type definitions, all in one place
*
*----------------------------------------------------------------------------
*
*  Object numbers have to be unique.
*  The numbers    0 -  999 are reserved for core system use.
*  The numbers 1000 - 1999 are reserved for core application use.
*  The numbers 2000 - 9999 are reserved for contractor use.
*
*  If you need to make a new object class, give it a number higher than
*  9999 so it won't conflict with others which may be contributed. 
*
*  Valid user-defined object numbers are 10000 to 2147483648 (2^31).
*  To avoid conflicts (e.g., everyone using 10000 and 10001), multiply your
*  phone number (no area code, just the last 6 or 7 digits) by 100 and use
*  this as the first number in a block of 100 numbers.  If your phone number
*  is 988-7934 extension 13, use the numbers 879341300 - 879341399, e.g.,
*  you might add the following lines to your copy of this file:
*
*     #define CX_OB_SPECTRUM 879341300
*     #define CX_OB_UVSPEC   879341301
*     #define CX_OB_NMRSPEC  879341302
*
*  If you contribute the code, the next "offical" release might contain:
*
*     #define CX_OB_SPECTRUM 107
*     #define CX_OB_UVSPEC   108
*     #define CX_OB_NMRSPEC  109
*
*  Always, always use symbolic names (e.g., CX_OB_SPECTRUM) in your code.
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

#ifndef CX_TYPES_INCLUDED
#define CX_TYPES_INCLUDED 1

/*** Primitive definitions. ***/

typedef int               cx_Integer;
typedef double            cx_Real;
typedef unsigned char *   cx_Binary;
typedef char *            cx_String;
typedef void *            cx_Object;
typedef void            (*cx_Method)();
typedef cx_Object         cx_IOStream;

/*** Conversion definitions (for scanf). ***/

typedef double cx_Real64;

/*** Base classes. ***/

#define CX_OB_INVALID        0
#define CX_OB_ANY            1
#define CX_OB_STRING         2
#define CX_OB_ERRORQUEUE     3
#define CX_OB_STREAM         4
#define CX_OB_SEQUENCE       5
#define CX_OB_PROPERTY       6
#define CX_OB_DATATYPE       7
#define CX_OB_DATATYPE_TABLE 8
#define CX_OB_BINARY         9

/*** Molecule package classes. ***/

#define CX_OB_MOLECULE       10
#define CX_OB_ATOM           11
#define CX_OB_BOND           12
#define CX_OB_ATOMTUPLE      13
#define CX_OB_BONDTUPLE      14
#define CX_OB_SMIATOMS       15
#define CX_OB_SMIBONDS       16

/*** IO Stream package ***/

#define CX_OB_IOSTREAM       20

/*** Surface package classes. ***/

#define CX_OB_SURFACE        30
#define CX_OB_VERTEX         31
#define CX_OB_FACE           32
#define CX_OB_VERTEXTUPLE    33
#define CX_OB_FACETUPLE      34

/*** Message package classes. ***/

#define CX_OB_MESSAGE        40

/*** Exotic package classes. ***/

#define	CU_OB_CAMERA         476041500

/*** Reserved "shapes" (mainly for tuples). ***/

#define CX_SHAPE_MOLECULE 'M'
#define CX_SHAPE_ATOM     'A'
#define CX_SHAPE_BOND     'B'
#define CX_SHAPE_SURFACE  'S'
#define CX_SHAPE_VERTEX   'V'
#define CX_SHAPE_FACE     'F'

#define CX_TUPLESHAPE(c) ('A' == c || 'B' == c || 'V' == c || 'F' == c)

#endif /* CX_TYPES_INCLUDED */
