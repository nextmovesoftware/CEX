/*****************************************************************************
*  helloworld.c -- a very basic test program
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
#include <ctype.h>

#include "cx_cobweb.h"

/*============================================================================
 *  main() for CX version of helloworld
 */

int main(int argc, char **argv)
{
   cx_Object sob, dt;

   /*** Create string object. ***/

   sob = cx_create_string(NULL, "Hello world!");

   /*** Create a datatype for it. ***/

   dt = cx_create_datatype(NULL, "HI", "hi", "Greetings","1", "STRING", "Hi");
   cx_set_datatype(sob, dt);
   cx_spewob(stdout, sob, CX_SPEW_DEFAULT);

   /*** Destroy string object. ***/

   cx_destroy(sob);
   cx_cleanup();
   return 0;
}

