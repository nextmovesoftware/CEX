/*****************************************************************************
*  testsurf.c -- a non-standard test program
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
#include "cx_molecule.h"
#include "cx_surface.h"

/*============================================================================
 *  main() non-standard test program
 */

int main(int argc, char **argv)
{
   cx_Object  s2, s3, mol, m2;
   cx_Object  surf, surfdt;
   cx_Object  vtuple, v1, v2, v3, v4, curves;
   cx_Object  fareas, face, faces;
   cx_Object  prop;
   cx_Real    area;
   cx_Object  ins, outs;

   /*** Initialize surface package and create datatypes. ***/

   if (!cx_surface_pkg())
      fprintf(stderr, ">> cx_surface_pkg() failed\n");

   if (!cx_surface_create_datatypes(NULL))
      fprintf(stderr, ">> cx_surface_create_datatypes() failed\n");

   /*** Read in datatypes ***/

   ins = cx_create_iostream("$CX_ROOT/data/datatypes.cex", CX_IO_READ);
   if (NULL == ins) {
      cx_error_save("can't open datatypes.cex", NULL, NULL);
      cx_error_spew(stderr, NULL);
      exit(1);
   } else {
      cx_next(ins);
      cx_destroy(ins);
   }
 
   /*** Get datatype to attach to surface. ***/

   if (NULL == (surfdt = cx_tag2datatype(NULL, CX_TAG_SURFACE)))
      fprintf(stderr, ">> cx_tag2datatype(0,%s) failed\n", CX_TAG_SURFACE);

   /*** Create stand-alone surface. ***/

   if (NULL == (surf = cx_create_surface(NULL)))
      fprintf(stderr, ">> cx_create_surface(NULL) failed\n");

   if (surf) {

      /*** Attach datatype to surface. ***/

      if (!cx_set_datatype(surf, surfdt))
         fprintf(stderr, ">> cx_set_datatype(%p,%p) failed\n", surf, surfdt);


      /*** Add surface properties. ***/

      cx_set_sprop(surf, "remark", "this is surface s1");
      cx_set_sprop(surf, CX_PROP_ISMAN,  "F");
      cx_set_sprop(surf, CX_PROP_ISWELL, "T");

      /*** Create three vertices. ***/

      v1 = cx_create_vertex(surf, "1.1,0.1,0.1");
      v2 = cx_create_vertex(surf, "0.2,1.2,0.2");
      v3 = cx_create_vertex(surf, "0.3,0.3,1.3");
      v4 = cx_create_vertex(surf, "0.4,0.4,0.4");


      /*** Add curvature vertextuple, curvature data to vertices. **/

      curves = cx_create_vertextuple(surf, CX_PROP_CURVE),
      cx_set_datatype(curves, cx_pname2datatype(NULL, CX_PROP_CURVE));
      cx_set_rprop(v1, CX_PROP_CURVE, 0.111);
      cx_set_rprop(v2, CX_PROP_CURVE, 0.222);
      cx_set_rprop(v3, CX_PROP_CURVE, 0.333);
      cx_set_rprop(v4, CX_PROP_CURVE, 0.444);

      /*** Add curvature property. ***/

      prop = cx_set_sprop(curves, "remark",
	                  "these are curvatures of vertices of surface s1");

      /*** Create face-tuple to represent area of individual faces. ***/

      fareas = cx_create_facetuple(surf, CX_PROP_FAREA);
      cx_set_datatype(fareas, cx_pname2datatype(NULL, CX_PROP_FAREA));

      /*** Add face-area property. ***/

      prop = cx_set_sprop(fareas, "remark",
	                  "these are areas of faces of surface s1");

      /*** Create face and set its area. ***/

      if (NULL == (face = cx_create_face(v1, v2, v3)))
         fprintf(stderr, ">> cx_create_face(%p,%p,%p) failed\n", v1,v2,v3);

      cx_set_rprop(face, CX_PROP_FAREA, 1.234);

      /*** Create another face and set its area. ***/

      if (NULL == (face = cx_create_face(v4, v2, v1)))
         fprintf(stderr, ">> cx_create_face(%p,%p,%p) failed\n", v4,v2,v1);

      cx_set_rprop(face, CX_PROP_FAREA, 2.345);

      /*** Compute total area of faces and add as surface property. ***/

      area  = 0.0;
      faces = cx_stream(surf, CX_OB_FACE);
      while (NULL != (face = cx_next(faces)))
         area += cx_rprop(face, CX_PROP_FAREA);
      cx_destroy(faces);

      cx_set_rprop(surf, CX_PROP_AREA, area);

      /*** Send surface, print errors to stderr. ***/

/* cx_send(surf, NULL, stdout); */
      outs = cx_create_iostream("-", CX_IO_WRITE);
      cx_append(outs, surf);
      cx_error_spew(stderr, NULL);

      /*** Create molecule, add surface to it, send it. ***/

      if (NULL == (mol = cx_smilin(NULL, "CC(=O)CCO"))) {
         fprintf(stderr, ">> cx_smilin(\"CC(=O)CCO\") failed\n");
         cx_destroy(surf);

      } else {
	 cx_set_datatype(mol, cx_pname2datatype(NULL, "molecule"));
	 cx_set_parent(surf, mol);

m2 = cx_smilin(mol, "C=N");
cx_set_datatype(m2, cx_pname2datatype(NULL, "molecule"));
cx_set_sprop(m2, "remark", "molecule m2 of molecule m1");

s2 = cx_create_surface(m2);
cx_set_datatype(s2, surfdt);
cx_set_sprop(s2, "name", "surface 2");
cx_set_sprop(s2, "remark", "surface s2 of molecule m2 of molecule m1");

s3 = cx_create_surface(mol);
cx_set_datatype(s3, surfdt);
cx_set_sprop(s3, "name", "surface 3");
cx_set_sprop(s3, "remark", "surface s3 of molecule m1");

curves = cx_create_vertextuple(s3, CX_PROP_CURVE),
cx_set_datatype(curves, cx_pname2datatype(NULL, CX_PROP_CURVE));
cx_set_sprop(curves, "remark",
	             "these are curvatures of vertices of surface s3");



         cx_append(outs, mol);
         cx_destroy(outs);
         cx_destroy(mol);
         cx_error_spew(stderr, NULL);
      }
   }

   cx_cleanup();
   exit(0);
}
