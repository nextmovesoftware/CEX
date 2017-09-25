/*****************************************************************************
*  cx_surface.h -- CX surface package definitions
*
*----------------------------------------------------------------------------
*  Authors: Anthony Nichols (Columbia),
*           Conrad Huang    (UCSF)
*           Dave Weininger  (Daylight)           
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#ifndef CX_SURFACE_INCLUDED
#define CX_SURFACE_INCLUDED 1

/*****************************************************
 *  Supported classes defined in cx_types.h:
 *
 *     CX_OB_SURFACE
 *     CX_OB_VERTEX
 *     CX_OB_FACE
 *     CX_OB_VERTEXTUPLE
 *     CX_OB_FACETUPLE
 */

#include "cx_types.h"

/*** Surface datatype tags. ***/

#define CX_TAG_SURFACE     "SURF"
#define CX_TAG_VERTEX      "VERTEX"
#define CX_TAG_FACE        "FACE"
#define CX_TAG_VERTEXTUPLE "VTUPLE"
#define CX_TAG_FACETUPLE   "FTUPLE"

/*** Surface property names. ***/

#define CX_PROP_SURF   "surface"
#define CX_PROP_SRC    "source"
#define CX_PROP_SRCALG "source-algorithm"
#define CX_PROP_ISMAN  "is-a-manifold"
#define CX_PROP_ISWELL "is-well-formed"
#define CX_PROP_VORD   "vertex-order"
#define CX_PROP_AREA   "area"
#define CX_PROP_VOL    "volume"
#define CX_PROP_REM    "remark"
#define CX_PROP_NAME   "name"

/*** Vertex property names. ***/

#define CX_PROP_CURVE  "curvature"

/*** Face property names. ***/

#define CX_PROP_FAREA "facearea"

/*** C-wrappers for all public functions in surface package. ***/

#define cx_vertextuple_name         cx_e_vertextuple_name
#define cx_face                     cx_e_face
#define cx_facetuple_name           cx_e_facetuple_name
#define cx_create_vertex            cx_e_create_vertex
#define cx_create_vertextuple       cx_e_create_vertextuple
#define cx_create_face              cx_e_create_face
#define cx_create_facetuple         cx_e_create_facetuple
#define cx_create_surface           cx_e_create_surface
#define cx_surface_pkg              cx_e_surface_pkg
#define cx_surface_create_datatypes cx_e_surface_create_datatypes
#define cx_prefix2vtuples           cx_e_prefix2vtuples
#define cx_prefix2ftuples           cx_e_prefix2ftuples
#define cx_surfin                   cx_e_surfin
#define cx_surface_set_stringvalue  cx_e_surface_set_stringvalue

/*** Declarations of public functions in cx_surface.c. ***/

cx_Object  cx_e_face(cx_Object v1, cx_Object v2, cx_Object v3);
cx_Object  cx_e_create_vertex(cx_Object surf, cx_String xyzstr);
cx_Integer cx_e_vertex_id(cx_Object vertex);
cx_Object  cx_e_vertex_from_id(cx_Object surf, cx_Integer id);
cx_Object  cx_e_create_face(cx_Object v1, cx_Object v2, cx_Object v3);
cx_Object  cx_e_create_surface(cx_Object parent);
cx_Integer cx_e_surface_pkg(void);
cx_Integer cx_e_surface_create_datatypes(cx_Object table);
cx_Object  cx_e_surfin(cx_Object parent, cx_String input);
void       cx_e_surface_set_stringvalue(cx_Object surf, cx_String name);


/*** Declarations of public functions in cx_vertextuple.c. ***/

cx_Object  cx_e_create_vertextuple(cx_Object surf, cx_String name);
cx_String  cx_e_vertextuple_name(cx_Object vtuple);
cx_Object  cx_e_prefix2vtuples(cx_Object surf, cx_String prefix);

/*** Declarations of public functions in cx_facetuple.c. ***/

cx_Object  cx_e_create_facetuple(cx_Object surf, cx_String name);
cx_String  cx_e_facetuple_name(cx_Object ftuple);
cx_Object  cx_e_prefix2ftuples(cx_Object surf, cx_String prefix);

#endif /* CX_SURFACE_INCLUDED */
