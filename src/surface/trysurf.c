/*****************************************************************************
*  trysurf.c -- a non-standard test program
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
#include "cx_surface.h"

/*** This seems to be missing in 1.00! ***/

#define CX_PROP_SURF_COORDINATES "coordinates"

/*============================================================================
 *  main() non-standard test program
 */

int main(int argc, char **argv)
{
   int        verb = 7;
   cx_Object  surf, dt;
   cx_Object  vtuple, v1, v2, v3, v4;
   cx_Object  ftuple, face;

   /*** Initialize. ***/

   if (2 == argc && '-' == *argv[1]) {
      verb = cx_atoi(argv[1] + 1);
      printf("verb = %d\n");
   }

   cx_surface_pkg();
   cx_surface_create_datatypes(NULL);

   /*** Get datatype to attach to surface. ***/

   dt = cx_tag2datatype(NULL, CX_TAG_SURFACE);

   /*** Read input. ***/

   surf = cx_create_surface(NULL);
   if (surf) {

      /*** Create three vertices. ***/

      vtuple = cx_create_vertextuple(surf, CX_PROP_SURF_COORDINATES),
      cx_set_datatype(vtuple, cx_pname2datatype(NULL,
						CX_PROP_SURF_COORDINATES));
      v1 = cx_create_vertex(surf, NULL);
      cx_set_sprop(v1, CX_PROP_SURF_COORDINATES, "1,0,0");
      v2 = cx_create_vertex(surf, NULL);
      cx_set_sprop(v2, CX_PROP_SURF_COORDINATES, "0,1,0");
      v3 = cx_create_vertex(surf, NULL);
      cx_set_sprop(v3, CX_PROP_SURF_COORDINATES, "0,0,1");
      v4 = cx_create_vertex(surf, NULL);
      cx_set_sprop(v4, CX_PROP_SURF_COORDINATES, "0,0,0");

      /*** Create a face. ***/

      face = cx_create_face(v1, v2, v3);
      face = cx_create_face(v4, v2, v1);

      /*** Attach properties to surface for debugging and show it. ***/

      cx_set_datatype(surf, dt);
#if 0
      cx_spewob(stdout, surf, verb);
#else
      cx_send(surf, NULL, stdout);
#endif

      /*** Clean up. ***/

      cx_destroy(surf);
   }
   cx_error_spew(stdout, NULL);

   cx_cleanup();
   exit(0);
}
