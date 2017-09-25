/*****************************************************************************
*  cx_surface.c -- support for surface, vertex, and face objects
*
*  The code in this file is designed to be robust and self-contained.
*
*  The C interface is defined in cx_mol.h.
*
*  This package uses the "cx_basics", "cx_util" and "cx_err" packages.
*
*  Limitations:
*    Maximum number of vertices per surface ......... 2^31 (2147483648)
*    Maximum number of faces per surface ............ 2^31 (2147483648)
*    Maximum number of faces per vertex ............. 2^31 (2147483648)
*    Number of vertices per face .................... 3
*
*  To do:
*   [ ] Use vertex indexes rather than pointers in face structure
*   [ ] Clean up spew function
*   [ ] Possibly add face array (deal with nb?)
*   [ ] Possibly supply faces from vertex
*   [ ] use error function
*   [ ] add create_datatypes to pkg?
*   [?] make cx_set_stringvalue() polymorphic
*   [-] use static datatype for creation functions
*
*----------------------------------------------------------------------------
*
* vertex object
*   header
*      parent .... surface
*   stream
*      faces  ..... stream of including faces
*   properties
*      inord ...... "input order"      "1"      - "2147483648"
*    > visit ...... "visit"            "0"      - "1"
*    > label ...... "vertex label"     e.g., "[13CH5+]"
*
* face object
*   header
*      parent ..... surface
*   stream
*      vertices ... stream of vertices
*   properties
*    > visit ...... "visit"           "0"      - "1"
*
*  surface object
*   header
*      parent ..... surface
*   children
*      vertices ... stream of vertices
*      faces  ..... stream of faces
*   properties
*
*----------------------------------------------------------------------------
*  REVISED FOR 033
*
* vertex object
*   header
*      parent .... surface
*   attributes
*      faces  ..... stream of faces (later?)
*   properties
*      inord ...... "input order"      "1"      - "2147483648"
*    > visit ...... "visit"            "0"      - "1"
*    > label ...... "vertex label"     e.g., "[13CH5+]"
*    curvature
*
* face object
*    header
*       parent ..... surface
*    attributes
*       vertices ... stream of vertices
*    properties
*     > visit ...... "visit"           "0"      - "1"
*    facearea
*
* surface object
*   header
*      parent ..... NULL or other, e.g., molecule?
*   attributes
*      vertices ... stream of vertices
*      faces  ..... stream of faces
*   properties .... area, is-a-manifold, is-well-formed, remark,
*                   source, source-algorithm, stringvalue, volume
*----------------------------------------------------------------------------
*  External form was:
*
*     $SURF<"1;2;3;4&1,2,3;4,2,1">
*     SURF_XYZ<"1,0,0;0,1,0;0,0,1;0,0,0">
*     |
*
*  External form expt'l:
*
*     $SURF<surfname>
*     SVERT<"1,0,0;0,1,0;0,0,1;0,0,0">
*     SFACE<"1,2,3;4,2,1">
*     |
*
*  External form here:
*
*     $SURF<"1,0,0;0,1,0;0,0,1;0,0,0&1,2,3;4,2,1">
*     |
*
*----------------------------------------------------------------------------
*  Authors: Anthony Nicholls, Conrad Huang, David Weininger
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

#include <memory.h>

#include "cx_cobweb.h"
#include "cx_surface.h"

/*** Internal definitions. ***/

#define MX_CHAR_PER_VLABEL 20
#define ID_ARRAY_SIZE      1024

/*** Internal struct contains name and "invisible" parts. ***/

typedef struct {
   cx_Integer nvert;                    /* number of vertices    */
   cx_Object  vertices[ID_ARRAY_SIZE];  /* hashed vertex seq's   */
} CI_SURF;

typedef struct {
   int        id;                       /* 1-origin index        */
   cx_String  xyzstr;                   /* coordinates, "0,0,0"  */
} CI_VERTEX;
 
/*** Handy macros. ***/

#define NOT_SURFACE(o) (NULL == o || CX_OB_SURFACE != cx_type(o))
#define NOT_VERTEX(o)  (NULL == o || CX_OB_VERTEX  != cx_type(o))
#define NOT_FACE(o)    (NULL == o || CX_OB_FACE    != cx_type(o))

/*** Are surface methods initialized?  Not initially. ***/

static int initialized = FALSE;

/*** Prototype ***/

/*** Handy non-object-oriented static utilities ***/

/*** Object-level support for local versions of polymorphic methods. ***/

/*** Local versions of polymorphic methods on vertices follow. ***/

/*============================================================================
 *  vertex_destroy() -- destroy an individual vertex object
 */

static void vertex_destroy(cx_Object vertex)
{
   cx_Object face, faces, surf, v, vlist, doomed;

   /*** Look in face list for faces which contain this vertex. ***/

   doomed = cx_create_sequence(NULL);
   surf   = cx_parent(vertex);
   faces  = cx_stream(surf, CX_OB_FACE);
   cx_reset(faces);
   while (NULL != (face = cx_next(faces))) {

      /*** Add faces containing this vertex to `doomed' sequence. ***/

      vlist = cx_stream(face, CX_OB_VERTEX);
      while (NULL != (v = cx_next(vlist)))
         if (v == vertex) { cx_append(doomed, face); break; }
   }

   /*** Destroy temporary stream of faces. ***/

   cx_destroy(faces);

   /*** Destroy faces from this vertex in a low-level manner. ***/

   cx_reset(doomed);
   while (NULL != (face = cx_next(doomed))) {
      cx_destroy(cx_e_base_content(face));
      cx_e_base_destroy(face);
   }
   cx_destroy(doomed);

   /*** Destroy this vertex's base structure and any remaining children. ***/

   cx_e_base_destroy(vertex);

   /*** TODO: mark surface as "not closed"
    *** and remove surface area and volume ***/
}

/*============================================================================
 *  vertex_count() -- count class over vertices (face count is special)
 *
 *  Caller (cx_stream dispatcher) guarantees that vertex is actually an vertex.
 */

static cx_Integer vertex_count(cx_Object vertex, cx_Integer type)
{
   cx_Object  surf, faces, face, vlist, v;
   cx_Integer count = 0;

   /*** A stream of faces is special, since they're not children of vertex. ***/

   if (CX_OB_FACE == type) {
      surf  = cx_parent(vertex);
      faces = cx_stream(surf, CX_OB_FACE);
      cx_reset(faces);
      while (NULL != (face = cx_next(faces))) {
         vlist = cx_stream(face, CX_OB_VERTEX);
         while (NULL != (v = cx_next(vlist)))
            if (v == vertex) { count++; break; }
      }
      return count;
   }

   /*** Otherwise, use base count function. ***/

   return cx_e_base_count(vertex, type);
}

/*============================================================================
 *  vertex_stream() -- return stream over vertices (face stream is special)
 *
 *  Caller (cx_stream dispatcher) guarantees that vertex is actually an vertex.
 */

static cx_Object vertex_stream(cx_Object vertex, cx_Integer type)
{
   cx_Object surf, face, faces, vlist, v, copy;

   /*** A stream of faces is special, since they're not children of vertex. ***/

   if (CX_OB_FACE == type) {
      copy  = cx_e_create_stream(vertex);
      surf  = cx_parent(vertex);
      faces = cx_stream(surf, CX_OB_FACE);
      cx_reset(faces);
      while (NULL != (face = cx_next(faces))) {
         vlist = cx_stream(face, CX_OB_VERTEX);
         while (NULL != (v = cx_next(vlist)))
            if (v == vertex) { cx_e_base_append(copy, face); break; }
      }
      cx_destroy(faces);
      cx_reset(copy);
      return copy;
   }

   /*** Otherwise, use base stream function. ***/

   return cx_e_base_stream(vertex, type);
}

/*** Local versions of polymorphic methods on faces follow. ***/

/*============================================================================
 *  face_stream() -- return stream over faces (vertex stream is special)
 *
 *  Caller (cx_stream dispatcher) guarantees that face is actually a face.
 */

static cx_Object face_stream(cx_Object face, cx_Integer type)
{
   cx_Object vertex, vertices, copy;

   /*** A vertex stream is special, since they're not children of face. ***/

   if (CX_OB_VERTEX == type) {
      copy     = cx_e_create_stream(face);
      vertices = cx_e_base_content(face);
      cx_reset(vertices);
      while (NULL != (vertex = cx_next(vertices)))
         cx_e_base_append(copy, vertex);
      cx_reset(copy);
      return copy;
   }

   /*** Otherwise, use base stream function. ***/

   return cx_e_base_stream(face, type);
}

/*** Local versions of polymorphic methods on surfaces follow. ***/

/*============================================================================
 *  surface_stringvalue()
 *
 *  Local function  -- use cx_stringvalue(surf) externally.
 */

static cx_String surface_stringvalue(cx_Object surf)
{
   CI_VERTEX *vs;
   cx_Object  vertices, vertex, faces, face;
   int        cont, cont2;
   char       buf[20];

   /*** Initialize scratchpad. ***/

   cx_scratchpad(NULL);
   cx_scratchpad( "\"" );

   /*** Start with vertex coordinates. ***/

   vertices = cx_stream(surf, CX_OB_VERTEX);
   for (cont = FALSE; NULL != (vertex = cx_next(vertices)); cont = TRUE) {
 
      /*** Possibly add tuple-delimiter, add stringvalue. ***/
 
      if (cont) cx_scratchpad( ";"  );
      vs = cx_e_base_content(vertex);
      if (vs && *vs->xyzstr) cx_scratchpad(vs->xyzstr);
      else                   cx_scratchpad(",,");
   }
   cx_destroy(vertices);
   cx_scratchpad("&");

   /*** Send the faces as a tuple of three vertex ids. ***/

   faces = cx_stream(surf, CX_OB_FACE);
   for (cont = FALSE; NULL != (face = cx_next(faces)); cont = TRUE) {
 
      /*** Possibly add tuple-delimiter, add stringvalue. ***/
 
      if (cont) cx_scratchpad( ";"  );
      vertices = cx_stream(face, CX_OB_VERTEX);
      for (cont2 = FALSE; NULL != (vertex = cx_next(vertices)); cont2 = TRUE) {
         if (cont2) cx_scratchpad( ","  );
         (void) sprintf(buf, "%d", cx_e_vertex_id(vertex));
         cx_scratchpad( buf );
      }
   }
 
   /*** Clean up and return static buffer. ***/
 
   cx_scratchpad( "\"" );
   return cx_scratchpad("");  /* duplicate? remove trailing commas? */
}

/*============================================================================
 *  surface_destroy() -- destroy a surface object
 */

static void surface_destroy(cx_Object surf)
{
   CI_SURF   *ss;
   cx_Object  ob, obs;
   int        i;

   /*** Get internal surface struct. ***/

   if (NULL == (ss = (CI_SURF *) cx_e_base_content(surf))) return;

   /*** Destroy contents and struct. ***/

   for (i = 0; i < ID_ARRAY_SIZE; i++)
      cx_destroy(ss->vertices[i]);
   cx_free(ss);

   /*** Destroy faces separately from other children. ***/

   obs = cx_stream(surf, CX_OB_ANY);
   while (NULL != (ob = cx_next(obs))) {
      if (CX_OB_FACE == cx_type(ob)) {
         cx_destroy(cx_e_base_content(ob));
         cx_e_base_destroy(ob);
      }
   }
   cx_destroy(obs);

   /*** Destroy base structure and any children. ***/

   cx_e_base_destroy(surf);
}

/*============================================================================
 *  setvertexprops() -- creates vertextuple and sets vertex properties
 *
 *  Returns vertextuple object.
 */

static cx_Object setvertexprops(cx_Object parent, cx_Object surf,
                                cx_String pname,  cx_String data,
                                cx_Object dt)
{
   cx_Object  vtuple, vertices, vertex;
   cx_String  p, q, shape, list = cx_strdup(data);
   int        dim;
   char       buf[80];   /* to hold unique property name, e.g., "color.1" */
   static int uid = 1;   /* should be on a surface basis */
 
   /*** Create unique property name. ***/

   if (NULL == pname) return NULL;
   sprintf(buf, "%s.%d", pname, uid++);

   /*** Create vertextuple with given property name. ***/

   if (NULL == (vtuple = cx_create_vertextuple(parent, buf))) return NULL;
   cx_set_datatype(vtuple, dt);

   /** Extract dimension from shape. ***/

   shape = cx_sprop(dt, "shape");
   dim   = ((isdigit(*shape) && '0' != *shape) ? (*shape - '0') : 1);
   dim   = CX_MAX(dim, 1);

   /*** Assign items in semicolon-delimited list to vertices in stream. ***/

   vertices = cx_stream(surf, CX_OB_VERTEX); /* input order */
   cx_reset(vertices);
   for (p = list; (vertex = cx_next(vertices)); p = (p ? q + 1 : NULL)) {

      /*** Extract semicolon-delimited items. ***/

      q = cx_strqbrk(p, ";");

      /*** Assign terminated item(s) to vertex as property. ***/

      if (q) *q = '\0';
      cx_set_sprop(vertex, buf, p);
      if (q) *q = ';';

      /*** Break if last item was really the last one. ***/

      if (NULL == q) break;
   }
   cx_destroy(vertices);
   cx_free(list);

   return vtuple;
}

/*============================================================================
 *  setfaceprops() -- creates facetuple and sets face properties
 *
 *  Returns facetuple object.
 */

static cx_Object setfaceprops(cx_Object parent, cx_Object surf,
                              cx_String pname,  cx_String data,
                              cx_Object dt)
{
   cx_Object  ftuple, faces, face;
   cx_String  p, q, shape, list = cx_strdup(data);
   int        dim;
   char       buf[80];  /* to hold unique property name, e.g., "color.1" */
   static int uid = 1;  /* should be on a surface basis */
 

   /*** Create unique property name. ***/

   if (NULL == pname) return NULL;
   sprintf(buf, "%s.%d", pname, uid++);

   /*** Create facetuple with given property name. ***/

   if (NULL == (ftuple = cx_create_facetuple(parent, buf))) return NULL;
   cx_set_datatype(ftuple, dt);

   /** Extract dimension from shape. ***/

   shape = cx_sprop(dt, "shape");
   dim   = ((isdigit(*shape) && '0' != *shape) ? (*shape - '0') : 1);
   dim   = CX_MAX(dim, 1);

   /*** Assign items in semicolon-delimited list to faces in stream. ***/

   faces = cx_stream(surf, CX_OB_FACE); /* input order */
   cx_reset(faces);
   for (p = list; NULL != (face = cx_next(faces)); p = q + 1) {

      /*** Extract semicolon-delimited items. ***/

      q = cx_strqbrk(p, ";");

      /*** Assign terminated item(s) to face as property. ***/

      if (q) *q = '\0';
      cx_set_sprop(face, buf, p);
      if (q) *q = ';';

      /*** Break if last item was really the last one. ***/

      if (NULL == q) break;
   }
   cx_destroy(faces);
   cx_free(list);

   return ftuple;
}

/*============================================================================
 *  surface_cexin() -- read surface as CEX tree, save in object
 *
 *  If table is NULL, the default datatype table is used.
 *  If successful, return the surface; if not, return NULL;
 */

static cx_Object surface_cexin(cx_String cex, cx_Object tabin)
{
   cx_Object  table, dt, id, di, ob, surf = NULL;
   cx_Integer cxt;
   cx_String  p, q, surftag, tag, data, lang, shape, pname;
   cx_String  func = "surface_cexin";
   char       errbuf[80];

   char       cold, *hold;
   cx_Integer rt, skip2id;

   /* Avoid uninitialized data! */
   dt = (cx_Object)0;

   /*** Reject NULL cex strings. ***/
 
   if (NULL == cex) return NULL;
 
   /*** If given table is NULL, get default table. ***/
 
   table = (tabin ? tabin : cx_default_datatypetable());
 
   /*** Extract root tag and content, return in error if unable. ***/
 
   if (NULL == (p = cx_e_cex_xtagdata(cex, &cxt, &surftag, &data))) {
      strncpy(errbuf, cex, 50);
      errbuf[50] = '\0';
      cx_error_save("Can't parse tree starting:", CX_ERR_ERROR, func);
      cx_error_save(errbuf,                       CX_ERR_ERROR, func);
 
   /*** Full datatrees must start with an identifier. ***/
 
   } else if (CX_CXT_IDENTIFIER != cxt && NULL != cx_strqbrk(cex, "|")) {
      cx_error_save("Expected root identifier:", CX_ERR_ERROR, func);
      cx_error_save(surftag,                     CX_ERR_ERROR, func);
      cx_error_save(cex,                         CX_ERR_ERROR, func);
 
   /*** Extract datatype; return in error if unable. ***/
 
   } else if (NULL == (dt = cx_tag2datatype(table, surftag))) {
      cx_error_save("Can't get datatype for:", CX_ERR_ERROR, func);
      cx_error_save(surftag,                   CX_ERR_ERROR, func);
 
   /*** Check for SURFEX language; return in error if not. ***/
 
   } else if (NULL == (lang = cx_sprop(dt, "language"))) {
      cx_error_save("language not defined for:",  CX_ERR_ERROR, func);
      cx_error_save(surftag,                      CX_ERR_ERROR, func);

   } else if (0 != cx_strcmp("SURFEX", lang)) {
      sprintf(errbuf, "root %s language %s, expected SURFEX\n", surftag, lang);
      cx_error_save(errbuf, CX_ERR_ERROR, func);

   /*** Should make up, and check, language. ***/
 
   /*** Create empty surface object; return in error if fails. ***/

   } else {
      surf = cx_surfin(NULL, data); /* fwd ref to public entry */
   }

   /*** We're done with tag/data in any case. ***/

   cx_free(surftag);

   /*** Return NULL if can't create surface for any reason. ***/

   if (NULL == surf) return NULL;

   /*** Attach datatype to surface. ***/

   cx_set_datatype(surf, dt);
 
   /*** Interpret remaining data. ***/

   skip2id = FALSE;
   id = di = surf;
   for ( ; NULL != (q = cx_e_cex_xtagdata(p, &cxt, &tag, &data)); p = q) {

      /*** Skip to next id if handled elsewhere. ***/

      if (skip2id) {
         if (CX_CXT_IDENTIFIER == cxt) skip2id = FALSE;
         else                          continue;
      }
   
      /*** Get datatype from tag, language & shape from datatype. ***/

      if (NULL == (dt = cx_tag2datatype(table, tag))) {
         cx_error_save("Can't get datatype for:",    CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (lang = cx_sprop(dt, "language"))) {
         cx_error_save("language not defined for:",  CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (pname = cx_sprop(dt, "property name"))) {
         cx_error_save("no property name for:",      CX_ERR_WARN, func);
         cx_error_save(tag,                          CX_ERR_WARN, func);
         cx_error_save("... skipping dataitem ...",  CX_ERR_WARN, func);
         continue;
      } else if (NULL == (shape = cx_sprop(dt, "shape"))) {
         cx_error_save("shape not defined for:",     CX_ERR_NOTE, func);
         cx_error_save(tag,                          CX_ERR_NOTE, func);
         cx_error_save("... using shape 1 (scalar)", CX_ERR_NOTE, func);
         shape = "1";
      }

      /*** Look for root type of this tag (or CX_OB_INVALID). ***/
 
      rt = cx_tag2roottype(tag);
 
      /*** Attach identifiers to surface. ***/

      if (CX_CXT_IDENTIFIER == cxt) {
 
         /*** If could-be root identifier, use its parser. ***/
 
         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, "$|");
            if (hold) {
                cold = *hold;
                *hold = '\0';
                cx_set_parent(cx_e_cexin(p, rt, table), surf);
                *hold = cold;
            } else
                cx_set_parent(cx_e_cexin(p, rt, table), surf);
            skip2id = TRUE;

         /*** Surface subtrees. ***/

         } else if (CX_SHAPE_VERTEX == shape[1]) {
            id = di = setvertexprops(surf, surf, pname, data, dt);
         } else if (CX_SHAPE_FACE == shape[1]) {
            id = di = setfaceprops(surf, surf, pname, data, dt);
         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("surf: vertex/face-tuples only", CX_ERR_ERROR, func);
            cx_error_save(shape,                           CX_ERR_ERROR, func);
            cx_error_save(tag,                             CX_ERR_ERROR, func);
         } else {
            id = di = ob = cx_create_string(surf, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach dataitems to vertices, faces, or last identifier. ***/

      } else if (CX_CXT_DATAITEM == cxt) {
 
         /*** If could-be root datatype, use its parser. ***/
 
         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, ">");
            if (hold) {
                hold++;
                cold = *hold;
                *hold = '\0';
                cx_set_parent(cx_e_cexin(p, rt, table), surf);
                *hold = cold;
            } else
                cx_set_parent(cx_e_cexin(p, rt, table), surf);
 
         /*** Surface dataitems. ***/
 
         } else if (CX_SHAPE_VERTEX == shape[1]) {
            di = setvertexprops(id, surf, pname, data, dt);
         } else if (CX_SHAPE_FACE == shape[1]) {
            di = setfaceprops(id, surf, pname, data, dt);
         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("surf: vertex/face-tuples only", CX_ERR_ERROR, func);
            cx_error_save(shape,                           CX_ERR_ERROR, func);
            cx_error_save(tag,                             CX_ERR_ERROR, func);
         } else {
            di = ob = cx_create_string(id, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach properties to last dataitem. ***/

      } else if (CX_CXT_PROPERTY == cxt) {
         if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("tuple-based props not ok", CX_ERR_ERROR, func);
            cx_error_save(shape,                      CX_ERR_ERROR, func);
            cx_error_save(tag,                        CX_ERR_ERROR, func);
         } else {
            cx_set_sprop(di, pname, data);
         }

      /*** Humm. ***/

      } else {
         cx_error_save("Unexpected context (ouch!)" , CX_ERR_ERROR, func);
         cx_error_save(tag,                           CX_ERR_ERROR, func);
         cx_error_save(data,                          CX_ERR_ERROR, func);
      }

      /*** Free this tag/data pair. ***/

      cx_free(tag);
      cx_free(data);
   }
 
   /*** return surface. ***/

   return surf;
}

/*============================================================================
 *  surface_send() -- read surface as CEX tree, save in object
 *
 *  Write given surface, children, properties, and datatypes to output in
 *  a def-before-ref fashion.  cx_dt_table_setmarks(table,FALSE) should be
 *  called before this function is called on a particular stream.
 *
 *  If table is NULL, the default datatype table is used.
 *
 *  Uses local directly recursive function sendmol().
 *
 *  Returns TRUE iff successful.
 */

static int has_visible_children(cx_Object ob)
{
   cx_Object kid, kids = cx_stream(ob, CX_OB_ANY);
   while (NULL != (kid = cx_next(kids)))
      if (CX_OB_PROPERTY != cx_type(kid) && cx_datatype(kid)) return TRUE;
   return FALSE;
}

static void surface_emit( cx_Object outs, cx_String pref,
                          cx_String tag, cx_String eod )
{
   cx_e_ioputs(pref, outs);
   cx_e_ioputc('<',  outs);
   cx_e_ioputs(tag,  outs);
   cx_e_ioputc('>',  outs);
   cx_e_ioputs(eod,  outs);
}

 
static cx_Integer sendsurf(cx_Object ob, cx_Object table, cx_String eod,
                           cx_Object outs, int pass, int lev)
{
   cx_Object dt, kid, kids, prop, props;
   cx_String pname;

   /*** Successful no-op if ob is NULL or has no associated datatype. ***/
 
   if (NULL == ob || NULL == (dt = cx_datatype(ob))) return TRUE;

   /*** Unsuccessful no-op if table, isn't. ***/

   if (CX_OB_DATATYPE_TABLE != cx_type(table)) return FALSE;
 
   /*** Send datatype on pass 0, print dataitem on pass 1. ***/

   if (0 == pass) {
      cx_append(outs, dt);

   } else if (0 == lev) {
      cx_e_ioputc('$', outs);
      surface_emit(outs, cx_sprop(dt,"tag"), cx_stringvalue(ob), eod);
 
   /*** EXPERIMENTAL, NEW IN 033, DEDUCE RECURSION ON OUTPUT ***/
 
   } else if (has_visible_children(ob)) {
      cx_e_ioputc('$', outs);
      surface_emit(outs, cx_sprop(dt, "tag"), cx_stringvalue(ob), eod);
 
   } else {
      cx_e_ioputc('%', outs);
      surface_emit(outs, cx_sprop(dt, "tag"), cx_stringvalue(ob), eod);
   }

   /*** Send property datatype on pass 0, print property on pass 1. ***/

   props = cx_stream(ob, CX_OB_PROPERTY);
   while (NULL != (prop = cx_next(props))) {
      pname = cx_prop_name(prop);
      if (NULL != (dt = cx_pname2datatype(table, pname))) {
         if (0 == pass) {
            cx_append(outs, dt);
         } else {
            cx_e_ioputc('/', outs);
            surface_emit(outs, cx_sprop(dt, "tag"), cx_stringvalue(prop), eod);
         }
      }
   }
   cx_destroy(props);

   /*** Print children recursively. ***/

   kids = cx_stream(ob, CX_OB_ANY);
   while (NULL != (kid = cx_next(kids)))
      if (CX_OB_PROPERTY != cx_type(kid))
         sendsurf(kid, table, eod, outs, pass, lev + 1);
   cx_destroy(kids);

   /*** Return successfully. ***/

   return TRUE;
} 
 
static cx_Integer surface_send(cx_Object ob, cx_Object table, cx_Object outs)
{
   cx_Integer ok;
   cx_String  eod, eot;

   /*** Process trivial or invalid requests. ***/

   if (NULL == ob  ) return TRUE;
   if (NULL == outs) return FALSE;

   /*** Set end-of-data and end-of-tree delimiters from listing format. ***/
 
   switch (cx_cex_listfmt()) {
      default:           /* Avoid compiler warnings! */
      case CX_FMT_DUMP:  eod = "";    eot = "\n";  break;
      case CX_FMT_RAW:   eod = "";    eot = "";    break;
      case CX_FMT_LIST:  eod = "\n";  eot = "\n";  break;
   }

   /*** Send datatypes, send datatree, return success. ***/

   ok = sendsurf(ob, table, eod, outs, 0, 0);
   if (ok) ok = sendsurf(ob, table, eod, outs, 1, 0);
   cx_e_ioputc('|', outs);
   cx_e_ioputs(eot, outs);
   return ok;
}

/*** Local auto-initialization function follows. ***/

/*============================================================================
 *  surface_init() -- initialize surface-specific functions
 */
 
void surface_init(void)
{
   /*** Define surface and surface-based types. ***/
 
   cx_e_set_typename(CX_OB_SURFACE, "surface");
   cx_e_set_typename(CX_OB_VERTEX,  "vertex");
   cx_e_set_typename(CX_OB_FACE,    "face");

   /*** Register root tag. ***/

   cx_e_register_root_type(CX_TAG_SURFACE, CX_OB_SURFACE);
 
   /*** vertex-specific functions. ***/

   cx_set_method(CX_OB_VERTEX, "destroy",     vertex_destroy    );
   cx_set_method(CX_OB_VERTEX, "stream",      vertex_stream     );
   cx_set_method(CX_OB_VERTEX, "count",       vertex_count      );
   cx_set_method(CX_OB_VERTEX, "setproperty", cx_e_base_setproperty);
   cx_set_method(CX_OB_VERTEX, "stringvalue", NULL);

   /*** Can't cexin or send send vertices. ***/

   cx_set_method(CX_OB_VERTEX, "cexin",   NULL);
   cx_set_method(CX_OB_VERTEX, "send",    NULL);


   /*** face-specific functions. ***/

   cx_set_method(CX_OB_FACE, "destroy",     cx_e_base_destroy    );
   cx_set_method(CX_OB_FACE, "stream",      face_stream          );
   cx_set_method(CX_OB_FACE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_FACE, "setproperty", cx_e_base_setproperty);
   cx_set_method(CX_OB_FACE, "stringvalue", NULL);


   /*** Can't create, stringvalue, cexin, or send faces. ***/

   cx_set_method(CX_OB_FACE, "cexin",       NULL);
   cx_set_method(CX_OB_FACE, "send",        NULL);


   /*** surface-specific functions. ***/

   cx_set_method(CX_OB_SURFACE, "cexin",       surface_cexin        );
   cx_set_method(CX_OB_SURFACE, "send",        surface_send         );
   cx_set_method(CX_OB_SURFACE, "setproperty", cx_e_base_setproperty);
   cx_set_method(CX_OB_SURFACE, "stringvalue", surface_stringvalue  );
   cx_set_method(CX_OB_SURFACE, "destroy",     surface_destroy      );
   cx_set_method(CX_OB_SURFACE, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_SURFACE, "stream",      cx_e_base_stream     );

   /*** Mark surface methods initialized and return successfully. ***/
       
   initialized = TRUE;
}
 
/*============================================================================
 *  cx_surface_pkg() -- public surface package initialization
 */

cx_Integer cx_e_surface_pkg(void)
{
   /*** Initialize surface methods and datatypes as needed. ***/

   if (!initialized) surface_init();
   return TRUE;
}

/*** Non-polymorphic external functions follow. ***/

/*============================================================================
 *  cx_e_surfin() -- construct a surface from vertex ids and face tuple
 */

static cx_String get_id(cx_String in, cx_Integer *id)
{
   cx_String start = in;
   int       n     = 0;

   while (isdigit(*in))
      n = n * 10 + (*in++ - '0');
   *id = ((in == start) ? -1 : n);
   return in;
}

cx_Object cx_e_surfin(cx_Object parent, cx_String input)
{
   cx_Object  surf, face, v[3];
   cx_Integer id;
   cx_String  pinface, func = "cx_e_surfin";
   cx_String  p, q, in;
   int        i;

   /*** Require a non-NULL, non-empty input string. ***/

   if (input == NULL || *input == '\0') return NULL;

   /*** Make temporary copy of input. ***/

   in = cx_strdup(input);
   if (NULL == in) return NULL;

   /*** Point to start of face section of input. ***/

   pinface = strchr(in, '&');
   if (pinface) *pinface++ = '\0';

   /*** Create an empty surface. ***/

   surf = cx_e_create_surface(parent);
   if (NULL == surf) return NULL;

   /*** First part contains vertex coordinate(s). ***/

   id = 0;
   for (p = in; *p; p=q ) {
      for( q=p; *q; q++ )
          if( *q == ';' ) {
              *q++ = '\0';
              break;
          }
             
      if (NULL == cx_e_create_vertex(surf, p)) {
         cx_error_save("Can't parse vertex input:", CX_ERR_ERROR, func);
         cx_error_save(p                          , CX_ERR_ERROR, func);
         cx_destroy(surf);
         return NULL;
      }
   }

   /*** Parse the face part ***/

   for (p = pinface; *p; p=q ) {
      for( q=p; *q; q++ )
          if( *q == ';' ) {
              *q++ = '\0';
              break;
          }

      /*** Expect three positive id's. ***/

      for (i = 0; i < 3; i++) {

         /*** Read positive integral id or die trying. ***/

         p = get_id(p, &id);

         if (0 >= id) {
            cx_error_save("Can't parse face input:", CX_ERR_ERROR, func);
            cx_error_save(input                    , CX_ERR_ERROR, func);
            cx_destroy(surf);
            return NULL;
         }

         /*** Find vertex from id or die trying. ***/

         v[i] = cx_e_vertex_from_id(surf, id);
         if (NULL == v[i]) {
            cx_error_save("Face has undefined vertex", CX_ERR_ERROR, func);
            cx_destroy(surf);
            return NULL;
         }

         if (',' == *p) p++;
      }

      /*** Create face from three vertices or die trying. ***/

      face = cx_e_create_face(v[0], v[1], v[2]);
      if (NULL == face) {
         cx_error_save("Can't create face (oom?)", CX_ERR_ERROR, func);
         cx_destroy(surf);
         return NULL;
      }
   }

   /*** Cleanup and return. ***/

   return surf;
}

/*============================================================================
 *  cx_e_create_vertex() -- return a fresh vertex, or NULL
 */

cx_Object cx_e_create_vertex(cx_Object surf, cx_String xyzstr)
{
   cx_Object  vertex;
   CI_VERTEX *vs;
   CI_SURF   *ss;

   /*** Validate parent and mark surface modified. ***/

   if (NOT_SURFACE(surf)) return NULL;

   /*** Make new vertex object. ***/

   vs = (CI_VERTEX *)malloc(sizeof(CI_VERTEX));
   if (NULL == vs) return NULL;

   vertex = (cx_Object) cx_e_base_create(surf, CX_OB_VERTEX);
   if (NULL == vertex) return NULL;

   cx_e_base_set_content(vertex, (void *) vs);

   /*** Give the vertex the next id. ***/

   ss         = cx_e_base_content(surf);
   ss->nvert += 1;
   vs->id     = ss->nvert;

   /*** Save initial coordinate string. ***/

   vs->xyzstr = cx_strdup(xyzstr);

   /*** Append to hashed vertices sequence. ***/

   cx_append(ss->vertices[vs->id % ID_ARRAY_SIZE], vertex);

   /*** Return vertex as (cx_Object) ***/

   return vertex;
}

/*============================================================================
 *  cx_e_vertex_id() -- return the integer id associated with a vertex
 */

cx_Integer cx_e_vertex_id(cx_Object vertex)
{
   CI_VERTEX *vs = cx_e_base_content(vertex);
   return (vs ? vs->id : -1);
}

/*============================================================================
 *  cx_e_vertex_from_id() -- return vertex with given integer id
 */

cx_Object cx_e_vertex_from_id(cx_Object surf, cx_Integer id)
{
   CI_SURF   *ss;
   cx_Object vertices, vertex;

   ss = cx_e_base_content(surf);
   if (ss == NULL) return NULL;

   vertices = ss->vertices[id % ID_ARRAY_SIZE];

   cx_reset(vertices);
   while (NULL != (vertex = cx_next(vertices)))
      if (cx_e_vertex_id(vertex) == id) return vertex;
   return NULL;
}

/*============================================================================
 *  alloc_face() -- return a newly allocated & initialized face, or NULL
 */
 
static cx_Object alloc_face(cx_Object surf)
{
   cx_Object face;
 
   /*** Make new face object. ***/
 
   face = (cx_Object) cx_e_base_create(surf, CX_OB_FACE);
   if (NULL == face) return NULL;
 
   /*** Store stream of vertices as face's (only) content. ***/
 
   cx_e_base_set_content(face, (void *) cx_e_create_sequence(face));
 
   /*** Return face as (cx_Object) ***/
 
   return face;
}
 
/*============================================================================
 *  cx_e_create_face() -- return a newly allocated & initialized face, or NULL
 */

cx_Object cx_e_create_face(cx_Object v1, cx_Object v2, cx_Object v3)
{
   cx_Object surf, face, vertices;

   /*** Must have three vertices. ***/

   if (NOT_VERTEX(v1) || NOT_VERTEX(v2) || NOT_VERTEX(v3)) return NULL;

   /*** Vertices must be different. ***/

   if (v1 == v2 || v1 == v3 || v2 == v3) return NULL;

   /*** All vertices must share common parent. ***/

   if (NULL == (surf = cx_parent(v1))) return NULL;
   if (surf != cx_parent(v2) || surf != cx_parent(v3)) return NULL;

   /*** Only make face if no face with given vertices already exists. ***/

#if 0
   if (NULL == (face = cx_face(v1, v2, v3))) {
#endif

      /*** Create face. ***/

      if (NULL == (face = alloc_face(surf))) return NULL;

      /*** Add vertices to face's vertex stream. ***/

      vertices = cx_e_base_content(face);
      cx_e_base_append(vertices, v1);
      cx_e_base_append(vertices, v2);
      cx_e_base_append(vertices, v3);
#if 0
   }
#endif

   /*** Return new face object. ***/

   return face;
}

/*============================================================================
 *  cx_e_face() -- return face between two vertices or NULL
 */

cx_Object cx_e_face(cx_Object v1, cx_Object v2, cx_Object v3)
{
   cx_Object surf, vertices, v, faces, face;

   /*** Validate end vertices. ***/

   if (NOT_VERTEX(v1) || NOT_VERTEX(v2) || NOT_VERTEX(v3)) return NULL;

   /*** Get faces of surface. ***/

   surf = cx_parent(v1);
   faces = cx_stream(surf, CX_OB_FACE);

   /*** Loop over faces, looking for matching vertices. ***/

   cx_reset(faces);
   while (NULL != (face = cx_next(faces))) {
      vertices = cx_stream(face, CX_OB_VERTEX);
      cx_reset(vertices);
      while (NULL != (v = cx_next(vertices)))
         if (v != v1 && v != v2 && v != v3) break;
      if (NULL == v) break;
   }

   /*** Return common face if found. ***/

   return face;
}

/*============================================================================
 *  cx_e_create_surface() -- return newly alloced & initialized surf, or NULL
 */

cx_Object cx_e_create_surface(cx_Object parent)
{
   CI_SURF    *ss;
   cx_Object  surf;
   int        i;

   /*** Initialize surface methods if needed. ***/

   if (!initialized) surface_init();

   /*** Create internal surface struct and and base object. ***/

   ss = (CI_SURF *)malloc(sizeof(CI_SURF));
   if (NULL == ss) return NULL;

   surf = cx_e_base_create(parent, CX_OB_SURFACE);
   if (NULL == surf) return NULL;

   /*** Initialize internal surface struct. ***/

   ss->nvert = 0;
   for (i = 0; i < ID_ARRAY_SIZE; i++)
      ss->vertices[i] = cx_create_sequence(NULL);

   /*** Set surface object content to struct and return object. ***/

   cx_e_base_set_content(surf, ss);

   return surf;
}

/*============================================================================
 *  cu_e_surface_create_datatypes() -- create surface-related datatypes
 *
 *  This return TRUE iff all datatypes are created successfully.
 */

static char *dts[] =  {

   "SURF",   CX_PROP_SURF,    "Surface",              "1",  "SURFEX",
   "Surface as vertex positions and optional triangular faces",

   "SRC",    CX_PROP_SRC,     "Source",               "1",  "STRING",
   "Source of object, e.g., program used to generate object",

   "SRCALG", CX_PROP_SRCALG, "Source algorithm",      "1",  "STRING",
   "Method used to generate object",

   "ISMAN",  CX_PROP_ISMAN,   "Is a manifold",        "1",  "STRING",
   "Is this (surface) a manifold? (T,F)",

   "ISWELL", CX_PROP_ISWELL,  "Is well-formed",       "1",  "STRING",
   "Are (surface) edges shared by at most two faces? (T,F)",

   "VORD",   CX_PROP_VORD,    "Vertex order",         "1",  "STRING",
   "Order of vertices in faces (CW, CCW, UNK)",

   "CURVE",  CX_PROP_CURVE,   "Curvature at vertex",  "1V", "REAL",
   "Curvature at vertex (unitless)",

   "FAREA",  CX_PROP_FAREA,   "Face area",            "1F", "REAL",
   "Surface area of face, square Angstroms",

   "AREA",   CX_PROP_AREA,    "Area",                 "1",  "REAL",
   "Area, square Angstroms",

   "VOL",    CX_PROP_VOL,     "Volume",               "1",  "REAL",
   "Volume, cubic Angstroms",

   "REM",    CX_PROP_REM,     "Remark",               "1",  "STRING",
   "General purpose ASCII text",

   "NAM",   CX_PROP_NAME,     "Name",                 "1",  "STRING",
   "Name as arbitrary ASCII text",

   NULL
};

cx_Integer cx_e_surface_create_datatypes(cx_Object table)
{
   static int defined = FALSE;
   static int rv      = FALSE;
   char **p;

   if (defined) return rv;

   if (table == NULL) table = cx_default_datatypetable();

   if (NULL != table) {
      rv = TRUE;
      for (p = dts; *p; p += 6)
         if (!cx_create_datatype(table, p[0], p[1], p[2], p[3], p[4], p[5]))
            rv = FALSE;
      defined = TRUE;
   }
   return rv;
}
