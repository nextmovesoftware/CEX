/*****************************************************************************
*  cx_binary.c -- support for binary objects
*
*  This was adapted from the internal Daylight CIS utility strbinary.c,
*  written by Craig James.
*
*----------------------------------------------------------------------------
*  Contributing author and institution:
*     Dave Weininger and Craig James, Daylight CIS, Inc.
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

#include "cx_cobweb.h"

/*** Binary struct: pointer and length. ***/

typedef struct {
   cx_Binary  ptr;  /* pointer to value */
   cx_Integer len;  /* length */
} CI_BINARY;

/*** Are we initialized? ***/

static int initialized = FALSE;

/*
 *  The following macros & array provide fast lookup to translate binary data
 *  into printable ascii.  Each 6 bits of binary (range 0-63) is converted to
 *  one of 64 characters in the set [,.0-9A-Za-z]; each 3-byte triplet thus
 *  converts to a 4-byte ASCII string.
 *  
 *  The table is a simple lookup table, but serves both as a "to" and "from"
 *  table.  The left column is index-to-ascii, the right ascii-to-binary.
 *  
 *  Every binary array is padded to a multiple of 3 bytes for the conversion;
 *  once the conversion is done you can't tell whether the last two bytes are
 *  pad bytes or real bytes containing zero.  To remedy this, an extra
 *  character is tacked on the ASCII representation; it will always be one of
 *  the characters '3', '2', or '1', indicating how many of the bytes in the
 *  last triplet are genuine.  An ASCII-to-binary conversion will always
 *  produce an array whose length is a multiple of 3, but the last one or two
 *  bytes might be pad bytes; the last ascii character indicates this.
 */

#define BIN2ASCII(s,c,d,e) { \
 (s)[0] = lookup[ (int) (c & 0xFC) >> 2].asc;                            \
 (s)[1] = lookup[((int) (c & 0x03) << 4) | ((int) (d & 0xF0) >> 4)].asc; \
 (s)[2] = lookup[((int) (d & 0x0F) << 2) | ((int) (e & 0xC0) >> 6)].asc; \
 (s)[3] = lookup[ (int) (e & 0x3F)].asc;                                 \
}

#define ASCII2BIN(s,c,d,e) {                  \
 c  = ((int)  lookup[(s)[0]].bin         << 2); \
 c |= ((int) (lookup[(s)[1]].bin & 0x30) >> 4); \
 d  = ((int) (lookup[(s)[1]].bin & 0x0F) << 4); \
 d |= ((int) (lookup[(s)[2]].bin & 0x3C) >> 2); \
 e  = ((int) (lookup[(s)[2]].bin & 0x03) << 6); \
 e |=  (int)  lookup[(s)[3]].bin;               \
}

static struct { char asc, bin; } lookup[] = {
/*  0   */ { '.',  0 },
/*  1   */ { ',',  0 },
/*  2   */ { '0',  0 },
/*  3   */ { '1',  0 },
/*  4   */ { '2',  0 },
/*  5   */ { '3',  0 },
/*  6   */ { '4',  0 },
/*  7   */ { '5',  0 },
/*  8   */ { '6',  0 },
/*  9   */ { '7',  0 },
/* 10   */ { '8',  0 },
/* 11   */ { '9',  0 },
/* 12   */ { 'A',  0 },
/* 13   */ { 'B',  0 },
/* 14   */ { 'C',  0 },
/* 15   */ { 'D',  0 },
/* 16   */ { 'E',  0 },
/* 17   */ { 'F',  0 },
/* 18   */ { 'G',  0 },
/* 19   */ { 'H',  0 },
/* 20   */ { 'I',  0 },
/* 21   */ { 'J',  0 },
/* 22   */ { 'K',  0 },
/* 23   */ { 'L',  0 },
/* 24   */ { 'M',  0 },
/* 25   */ { 'N',  0 },
/* 26   */ { 'O',  0 },
/* 27   */ { 'P',  0 },
/* 28   */ { 'Q',  0 },
/* 29   */ { 'R',  0 },
/* 30   */ { 'S',  0 },
/* 31   */ { 'T',  0 },
/* 32   */ { 'U',  0 },
/* 33 ! */ { 'V',  0 },
/* 34 " */ { 'W',  0 },
/* 35 # */ { 'X',  0 },
/* 36 $ */ { 'Y',  0 },
/* 37 % */ { 'Z',  0 },
/* 38 & */ { 'a',  0 },
/* 39 ' */ { 'b',  0 },
/* 40 ( */ { 'c',  0 },
/* 41 ) */ { 'd',  0 },
/* 42 * */ { 'e',  0 },
/* 43 + */ { 'f',  0 },
/* 44 , */ { 'g',  1 },
/* 45 - */ { 'h',  0 },
/* 46 . */ { 'i',  0 },
/* 47 / */ { 'j',  0 },
/* 48 0 */ { 'k',  2 },
/* 49 1 */ { 'l',  3 },
/* 50 2 */ { 'm',  4 },
/* 51 3 */ { 'n',  5 },
/* 52 4 */ { 'o',  6 },
/* 53 5 */ { 'p',  7 },
/* 54 6 */ { 'q',  8 },
/* 55 7 */ { 'r',  9 },
/* 56 8 */ { 's', 10 },
/* 57 9 */ { 't', 11 },
/* 58 : */ { 'u',  0 },
/* 59 ; */ { 'v',  0 },
/* 60 < */ { 'w',  0 },
/* 61 = */ { 'x',  0 },
/* 62 > */ { 'y',  0 },
/* 63 ? */ { 'z',  0 },
/* 64 @ */ { '-',  0 },
/* 65 A */ { '-', 12 },
/* 66 B */ { '-', 13 },
/* 67 C */ { '-', 14 },
/* 68 D */ { '-', 15 },
/* 69 E */ { '-', 16 },
/* 70 F */ { '-', 17 },
/* 71 G */ { '-', 18 },
/* 72 H */ { '-', 19 },
/* 73 I */ { '-', 20 },
/* 74 J */ { '-', 21 },
/* 75 K */ { '-', 22 },
/* 76 L */ { '-', 23 },
/* 77 M */ { '-', 24 },
/* 78 N */ { '-', 25 }, 
/* 79 O */ { '-', 26 }, 
/* 80 P */ { '-', 27 }, 
/* 81 Q */ { '-', 28 }, 
/* 82 R */ { '-', 29 }, 
/* 83 S */ { '-', 30 },
/* 84 T */ { '-', 31 },
/* 85 U */ { '-', 32 },
/* 86 V */ { '-', 33 },
/* 87 W */ { '-', 34 },
/* 88 X */ { '-', 35 },
/* 89 Y */ { '-', 36 },
/* 90 Z */ { '-', 37 },
/* 91 [ */ { '-',  0 },
/* 92 \ */ { '-',  0 },
/* 93 ] */ { '-',  0 },
/* 94 ^ */ { '-',  0 },
/* 95 _ */ { '-',  0 },
/* 96 ` */ { '-',  0 },
/* 97 a */ { '-', 38 },
/* 98 b */ { '-', 39 },
/* 99 c */ { '-', 40 },
/*100 d */ { '-', 41 },
/*101 e */ { '-', 42 },
/*102 f */ { '-', 43 },
/*103 g */ { '-', 44 },
/*104 h */ { '-', 45 },
/*105 i */ { '-', 46 },
/*106 j */ { '-', 47 },
/*107 k */ { '-', 48 },
/*108 l */ { '-', 49 },
/*109 m */ { '-', 50 },
/*110 n */ { '-', 51 },
/*111 o */ { '-', 52 },
/*112 p */ { '-', 53 },
/*113 q */ { '-', 54 },
/*114 r */ { '-', 55 },
/*115 s */ { '-', 56 },
/*116 t */ { '-', 57 },
/*117 u */ { '-', 58 },
/*118 v */ { '-', 59 },
/*119 w */ { '-', 60 },
/*120 x */ { '-', 61 },
/*121 y */ { '-', 62 },
/*122 z */ { '-', 63 },
/*123 { */ { '-',  0 },
/*124 | */ { '-',  0 },
/*125 } */ { '-',  0 },
/*126 ~ */ { '-',  0 },
/*127 */ { '-',  0 }
};

/*============================================================================
 *  bin2str() -- encode binary data as zero-terminated ASCII string
 *
 *  Encodes length-delimited binary data as zero-terminated printable ASCII
 *  using the 6-bits-to-8 expansion.  Returns a newly-malloc'ed string.
 *  Returns NULL on NULL input or malloc failure.
 */

static cx_String bin2str(cx_Integer blen, cx_Binary b)
{
   char *a, *p, left[3];
   int   i, j, alen;
   int   ntrips = blen / 3;
   int   nleft  = blen - (ntrips * 3);

   /*** Return NULL on NULL input. ***/

   if (NULL == b) return NULL;

   /*** Allocate return array; multiples of 4 + "pad" + zero ***/

   alen = ntrips * 4 + 1;
   if (nleft != 0) alen += 4;
   if (NULL == (a = (char *) cx_malloc(alen + 1))) return NULL;
   p = a;
  
   /*** Convert every 3 bytes to 4 ASCII characters. ***/

   for (i = 0; i < ntrips * 3; i += 3, p += 4)
      BIN2ASCII(p, b[i], b[i+1], b[i+2]);

   /*** Convert the trailing 1 or 2 bytes. ***/

   if (nleft > 0) {
      left[0] = left[1] = left[2] = 0;
      for (j = 0; i < blen; )
         left[j++] = b[i++];
      BIN2ASCII(p, left[0], left[1], left[2]);
      p += 4;
   }

   /*** Append with 3, 2, or 1 indicate # valid bytes in last triplet. ***/

   if (nleft == 0) nleft = 3;
   *(p++) = '0' + nleft;
   *p     = '\0';

   /*** Return encoded zero-terminated string. ***/

   return a;
}

/*============================================================================
 *  str2bin() -- convert encoded zero-terminated string to binary data
 *
 *  Converts encoded ASCII string to newly-malloced binary array.  Each 4
 *  bytes of ASCII is converted to 3 bytes of binary.  The last byte of ASCII
 *  will be either 0, 1, or 2, indicating how many of the last 3 bytes of
 *  binary to chop off to get the correct length.
 */

static cx_Binary str2bin(cx_Integer *plen, unsigned char *str)
{
   cx_Binary  bin;
   int        i, lens;

   if (NULL == str) return NULL;
   lens = strlen((char*)str);

   if ((lens % 4) != 1)
      cx_error_save("Invalid encoded string length", CX_ERR_ERROR, "str2bin");

   /*** Compute binary array's length and allocate it. ***/
  
   *plen  = (lens - 1) / 4;
   *plen *= 3;
   if (NULL == (bin = (cx_Binary) cx_malloc(*plen))) return NULL;

   /**** Convert encoded ASCII to binary. ****/
  
   for (i = 0; i < *plen; i += 3, str += 4)
      ASCII2BIN(str, bin[i],  bin[i+1], bin[i+2]);

   /**** Trim the pad zeros off the binary string ****/
   
   i      = *str - '0';
   *plen -= 3 - i;

   return bin;
}

/*============================================================================
 *  binary_destroy() -- destroy a given binary object
 */
 
static void binary_destroy(cx_Object ob)
{
   CI_BINARY *bs;

   /*** Extract object content as CI_BINARY struct, deallocate it. ***/

   if (NULL != (bs = (CI_BINARY *) cx_e_base_content(ob))) {
      cx_free(bs->ptr);
      cx_free(bs);
   }

   /*** Destroy base object and any children. ***/

   cx_e_base_destroy(ob);
}

/*============================================================================
 *  binary_binaryvalue() -- return binaryvalue of a binary object
 */
 
static cx_Binary binary_binaryvalue(cx_Integer *plen, cx_Object ob)
{
   CI_BINARY *bs = (CI_BINARY *) cx_e_base_content(ob);
   if (NULL == bs || NULL == bs->ptr) { *plen = 0; return NULL; }
   *plen = bs->len;
   return bs->ptr;
}

/*============================================================================
 *  binary_stringvalue() -- return stringvalue of a binary object
 */
 
static cx_String binary_stringvalue(cx_Object ob)
{
   CI_BINARY *bs = (CI_BINARY *) cx_e_base_content(ob);
   return bin2str(bs->len, bs->ptr);
}

/*============================================================================
 *  binary_init() -- initialize binary-specific functions
 */
 
static cx_Integer binary_init(void)
{
   /*** Define binary object type. ***/

   cx_e_set_typename(CX_OB_BINARY, "Binary");

   /*** Binary-specific functions ***/

   cx_set_method(CX_OB_BINARY, "destroy",     binary_destroy    );
   cx_set_method(CX_OB_BINARY, "binaryvalue", binary_binaryvalue);
   cx_set_method(CX_OB_BINARY, "stringvalue", binary_stringvalue);

   /*** Use normal (base) count and stream methods. ***/

   cx_set_method(CX_OB_BINARY, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_BINARY, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_BINARY, "setproperty", cx_e_base_setproperty);

   /*** Binary objects aren't root objects: no cexin or send methods. ***/

   cx_set_method(CX_OB_BINARY, "cexin", NULL);
   cx_set_method(CX_OB_BINARY, "send",  NULL);

   /*** Mark initialized and return happy. ***/

   initialized = TRUE;
   return TRUE;
}

/*============================================================================
 *  cx_e_create_binary() -- create a binary object, set initial value
 */
 
cx_Object cx_e_create_binary(cx_Object parent, cx_Integer len, cx_Binary ptr)
{
   cx_Object  ob;
   CI_BINARY *bs;

   /*** Initialize polymorphic functions if needed. ***/

   if (!initialized) binary_init();

   /*** Make new CX_OB_BINARY object with given parent. ***/

   if (NULL == (ob = cx_e_base_create(parent, CX_OB_BINARY))) return NULL;

   /*** Create binary struct. ***/

   bs = (CI_BINARY *) cx_malloc(sizeof(CI_BINARY));
   if (NULL == bs) { cx_free(ob); return NULL; }
   cx_e_base_set_content(ob, (void *) bs);

   /*** Set content of new binary object to copy of given data, or NULL. ***/

   if (NULL == ptr) {
      bs->len = 0;
      bs->ptr = NULL;
   } else {
      bs->len = len;
      bs->ptr = (cx_Binary) cx_malloc(len + 1);
      memcpy((char *) bs->ptr, (char *) ptr, len);
      if (NULL == bs->ptr) { cx_free(bs);  cx_free(ob);  return NULL; }
   }

   /*** Return initialized binary object. ***/

   return (cx_Object) ob;
}

/*============================================================================
 *  cx_e_parse_binary() -- create a binary object from coded ASCII data
 */
 
cx_Object cx_e_parse_binary(cx_Object parent, cx_String str)
{
   cx_Object ob;
   cx_Binary ptr;
   int       len;

   /*** Convert string to binary. ***/

   ptr = str2bin(&len, (unsigned char*)str);

   /*** Create binary object. ***/

   ob = cx_e_create_binary(parent, len, ptr);

   /*** Deallocate temporary binary data. ***/

   cx_free(ptr);

   /*** Return binary object. ***/

   return ob;
}
