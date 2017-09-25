/*****************************************************************************
* testcexdt.c -- test program to parse CEX tag/data with quoting
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

#define OUCH printf("=== Ouch, error in: %s\n", func)

/*============================================================================
 *  testcextd -- test CEX parsing of tag data, including wierd quoted strings
 */

main(int argc, char **argv)
{
   cx_String  p, t, tag, data, func;
   cx_Integer cxt;
   int        spew = (2 == argc && 0 == strcmp("-v", argv[1]));

   /*** Test cx_e_cex_xtagdata(). ***/

   t = "$SMI<C> FOO<bar>\n QUOTED<\"$SMI<FOO\"\"BAR>|\"> /FOO<baz>FOO2<bar2>|";

   func = "cx_e_cex_xtagdata";

   if (NULL == (p = cx_e_cex_xtagdata(t, &cxt, &tag, &data))) OUCH;
   if (CX_CXT_IDENTIFIER != cxt) OUCH;
   if (0 != strcmp("SMI", tag )) OUCH;
   if (0 != strcmp("C",   data)) OUCH;

   if (spew) printf("   cxt: %d\n   tag: %s\n   dat: %s\n\n", cxt, tag, data);

   cx_free(tag);
   cx_free(data);

   if (NULL == (p = cx_e_cex_xtagdata(p, &cxt, &tag, &data))) OUCH;
   if (CX_CXT_DATAITEM != cxt   ) OUCH;
   if (0 != strcmp("FOO",  tag )) OUCH;
   if (0 != strcmp("bar",  data)) OUCH;

   if (spew) printf("   cxt: %d\n   tag: %s\n   dat: %s\n\n", cxt, tag, data);

   cx_free(tag);
   cx_free(data);

   if (NULL == (p = cx_e_cex_xtagdata(p, &cxt, &tag, &data))) OUCH;
   if (CX_CXT_DATAITEM != cxt              ) OUCH;
   if (0 != strcmp("QUOTED",          tag )) OUCH;
   if (0 != strcmp("$SMI<FOO\"BAR>|", data)) OUCH;

   if (spew) printf("   cxt: %d\n   tag: %s\n   dat: %s\n\n", cxt, tag, data);

   cx_free(tag);
   cx_free(data);

   if (NULL == (p = cx_e_cex_xtagdata(p, &cxt, &tag, &data))) OUCH;
   if (CX_CXT_PROPERTY != cxt  ) OUCH;
   if (0 != strcmp("FOO", tag )) OUCH;
   if (0 != strcmp("baz", data)) OUCH;

   if (spew) printf("   cxt: %d\n   tag: %s\n   dat: %s\n\n", cxt, tag, data);

   cx_free(tag);
   cx_free(data);

   if (NULL == (p = cx_e_cex_xtagdata(p, &cxt, &tag, &data))) OUCH;
   if (CX_CXT_DATAITEM != cxt  ) OUCH;
   if (0 != strcmp("FOO2", tag )) OUCH;
   if (0 != strcmp("bar2", data)) OUCH;

   if (spew) printf("   cxt: %d\n   tag: %s\n   dat: %s\n\n", cxt, tag, data);

   cx_free(tag);
   cx_free(data);

   if (NULL != (p = cx_e_cex_xtagdata(p, &cxt, &tag, &data))) OUCH;

   exit(0);
}
