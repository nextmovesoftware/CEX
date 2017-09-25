/*****************************************************************************
*  cu_camera.c -- support for Camera objects
*
*  The code in this file is designed to be robust and self-contained.
*
*  The C interface is defined in cu_camera.h.
*
*  This package uses the "cx_basics", "cx_util" and "cx_err" packages.
*
*  To do:
*   [ ] Support reading from and writing to streams
*
*******************************************************************************
*
*  Camera object
*   header
*      parent ..... camera
*   attributes
*      none
*   properties
*      camera position ..... location of camera in space
*      scene center ........ center of scene
*      clip hither ......... distance from camera to hither clipping plane
*      clip yon ............ distance from camera to yon clipping plane
*      view area ........... horizontal and vertical clipping distances
*                            at the hither clipping plane
*      render size ......... size of photo that camera should produce
*      background color .... background color behind scene
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Conrad Huang, UCSF
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "cx_cobweb.h"
#include "cu_camera.h"

/*** Internal camera struct only contains "invisible" parts. ***/

typedef struct {
   cx_String   string;       /* string value of camera (not used for much)  */
} CI_CAMERA;

/*** Internal definitions. ***/

/*** Handy macros. ***/

#define NOT_CAMERA(o)	(NULL == o || CU_OB_CAMERA != cx_type(o))

/*** Are camera methods initialized?  Not initially. ***/

static int initialized = FALSE;

/*** Prototype ***/

/*** Handy non-object-oriented static utilities ***/

/*** Object-level support for local versions of polymorphic methods. ***/

/*** Local versions of polymorphic methods on cameras follow. ***/

/*============================================================================
 *  camera_stringvalue() -- return saved string, or NULL
 *
 *  This function is not public: use cx_stringvalue(camera).
 */

static cx_String camera_stringvalue(cx_Object camera)
{
   CI_CAMERA   *cs = cx_e_base_content(camera);

   return cs->string;
}

/*============================================================================
 *  camera_destroy() -- destroy a camera object
 */

static void camera_destroy(cx_Object camera)
{
   CI_CAMERA	*cs;

   /*** Get internal camera struct. ***/

   if (NULL == (cs = (CI_CAMERA *) cx_e_base_content(camera))) return;

   /*** Destroy contents and struct. ***/

   cx_free(cs->string);
   cx_free(cs);

   /*** Destroy base structure and any children. ***/

   cx_e_base_destroy(camera);
}

/*============================================================================
 *  camera_cexin() -- read camera as CEX tree, save in object
 *
 *  If table is NULL, the default datatype table is used.
 *  If successful, return the camera; if not, return NULL;
 */

static cx_Object camera_cexin(cx_String cex, cx_Object tabin)
{
   cx_Object  table, dt, id, di, ob, camera = NULL;
   CI_CAMERA  *cs;
   cx_Integer cxt;
   cx_String  p, q, roottag, rootdata, tag, data, lang, shape, pname;
   cx_String  func = "camera_cexin";
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
 
   if (NULL == (p = cx_e_cex_xtagdata(cex, &cxt, &roottag, &rootdata))) {
      strncpy(errbuf, cex, 50);
      errbuf[50] = '\0';
      cx_error_save("Can't parse tree starting:", CX_ERR_ERROR, func);
      cx_error_save(errbuf,                       CX_ERR_ERROR, func);
 
   /*** Full datatrees must start with an identifier. ***/
 
   } else if (CX_CXT_IDENTIFIER != cxt && NULL != cx_strqbrk(cex, "|")) {
      cx_error_save("Expected root identifier:", CX_ERR_ERROR, func);
      cx_error_save(roottag,                     CX_ERR_ERROR, func);
 
   /*** Extract datatype; return in error if unable. ***/
 
   } else if (NULL == (dt = cx_tag2datatype(table, roottag))) {
      cx_error_save("Can't get datatype for:", CX_ERR_ERROR, func);
      cx_error_save(roottag,                   CX_ERR_ERROR, func);
 
   /*** Check that language is available; return in error if not. ***/
 
   } else if (NULL == (lang = cx_sprop(dt, "language"))) {
      cx_error_save("language not defined for:",  CX_ERR_ERROR, func);
      cx_error_save(roottag,                      CX_ERR_ERROR, func);

   /*** Check that language is STRING; return in error if not. ***/
 
   } else if (0 != cx_strcmp("STRING", lang)) {
      sprintf(errbuf, "root %s language %s, expected STRING\n", roottag, lang);
      cx_error_save(errbuf, CX_ERR_ERROR, func);
 
   /*** Create the camera object; return in error if fails. ***/
 
   } else {
      camera = cu_e_create_camera(NULL);
   }

   /*** Clean up and exit if can't interpret camera for any reason. ***/

   if (NULL == camera) { cx_free(roottag); cx_free(rootdata); return NULL; }

   /*** Copy data string into camera. ***/

   cs = cx_e_base_content(camera);
   cs->string = cx_strdup(rootdata);

   /*** We're done with tag/data. ***/

   cx_free(roottag);
   cx_free(rootdata);

   /*** Attach datatype to camera. ***/

   cx_set_datatype(camera, dt);

   /*** Interpret remaining data. ***/

   skip2id = FALSE;
   id = di = camera;
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
 
      /*** Attach identifiers to camera. ***/
 
      if (CX_CXT_IDENTIFIER == cxt) {
 
         /*** If could-be root identifier, use its parser. ***/
 
         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, "$|");
            if (hold) {
                cold = *hold;
                *hold = '\0';
                cx_set_parent(cx_e_cexin(p, rt, table), camera);
                *hold = cold;
            } else
                cx_set_parent(cx_e_cexin(p, rt, table), camera);
            skip2id = TRUE;
 
         /*** Camera subids. ***/
 
         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("tuple-based identifier not ok", CX_ERR_ERROR, func);
            cx_error_save(shape,                           CX_ERR_ERROR, func);
            cx_error_save(tag,                             CX_ERR_ERROR, func);
         } else {
            id = di = ob = cx_create_string(camera, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach dataitems to atoms, bonds, or last identifier. ***/
 
      } else if (CX_CXT_DATAITEM == cxt) {
 
         /*** If could-be root datatype, use its parser. ***/
 
         if (CX_OB_INVALID != rt) {
            hold = cx_strqbrk(p + 1, ">");
            if (hold) {
                hold++;
                cold = *hold;
                *hold = '\0';
                cx_set_parent(cx_e_cexin(p, rt, table), camera);
                *hold = cold;
            } else
                cx_set_parent(cx_e_cexin(p, rt, table), camera);
 
         /*** Camera dataitems. ***/

         } else if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("tuple-based dataitem not ok", CX_ERR_ERROR, func);
            cx_error_save(shape,                         CX_ERR_ERROR, func);
            cx_error_save(tag,                           CX_ERR_ERROR, func);
         } else {
            di = ob = cx_create_string(id, data);
            cx_set_datatype(ob, dt);
         }

      /*** Attach properties to last dataitem. ***/

      } else if (CX_CXT_PROPERTY == cxt) {
         if (CX_TUPLESHAPE(shape[1])) {
            cx_error_save("tuple-based props not ok" , CX_ERR_ERROR, func);
            cx_error_save(shape,                       CX_ERR_ERROR, func);
            cx_error_save(tag,                         CX_ERR_ERROR, func);
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
 
   /*** return camera. ***/

   return camera;
}

/*============================================================================
 *  camera_send() -- write camera object as CEX tree
 *
 *  Write given camera, children, properties, and datatypes to output in
 *  a def-before-ref fashion.  cx_dt_table_setmarks(table,FALSE) should be
 *  called before this function is called on a particular stream.
 *
 *  If table is NULL, the default datatype table is used.
 *
 *  Uses local directly recursive function sendcamera().
 *
 *  Returns TRUE iff successful.
 */

static void camera_emit( cx_Object outs, cx_String tag,
		         cx_String obj, cx_String eod )
{
   cx_e_ioputs(tag, outs);
   cx_e_ioputc('<', outs);
   cx_e_ioputs(obj, outs);
   cx_e_ioputc('>', outs);
   cx_e_ioputs(eod, outs);
}

static cx_Integer sendcamera(cx_Object ob, cx_Object table, cx_String eod,
			     cx_Object outs, int pass, int lev)
{
   cx_Object dt, kid, kids, prop, props;
   cx_String pname;

   /*** Successful no-op if ob is NULL or has no associated datatype. ***/
 
   if (NULL == ob || NULL == (dt = cx_datatype(ob))) return TRUE;
 
   /*** Send datatype on pass 0, print dataitem on pass 1. ***/

   if (0 == pass) {
      cx_append(outs, dt);
   } else if (0 == lev) {
      cx_e_ioputc('$', outs);
      camera_emit(outs, cx_sprop(dt, "tag"), cx_stringvalue(ob), eod);
   } else {
      camera_emit(outs, cx_sprop(dt, "tag"), cx_stringvalue(ob), eod);
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
            camera_emit(outs, cx_sprop(dt, "tag"), cx_stringvalue(ob), eod);
         }
      }
   }
   cx_destroy(props);

   /*** Print children recursively. ***/

   kids = cx_stream(ob, CX_OB_ANY);
   while (NULL != (kid = cx_next(kids)))
      sendcamera(kid, table, eod, outs, pass, lev + 1);
   cx_destroy(kids);

   /*** Return successfully. ***/

   return TRUE;
} 
 
static cx_Integer camera_send(cx_Object ob, cx_Object table, cx_Object outs)
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

   ok = sendcamera(ob, table, eod, outs, 0, 0);
   if (ok) ok = sendcamera(ob, table, eod, outs, 1, 0);
   cx_e_ioputc('|', outs);
   cx_e_ioputs(eot, outs);
   return ok;
}

/*** Local auto-initialization function follows. ***/

/*============================================================================
 *  camera_init() -- initialize camera-specific functions
 */
 
void camera_init(void)
{
   /*** Define camera type. ***/
 
   cx_e_set_typename(CU_OB_CAMERA, "Camera");

   /*** Register root tag. ***/

   cx_e_register_root_type(CU_TAG_CAMERA, CU_OB_CAMERA);
 
   /*** camera-specific functions. ***/

   cx_set_method(CU_OB_CAMERA, "cexin",       camera_cexin         );
   cx_set_method(CU_OB_CAMERA, "send",        camera_send          );
   cx_set_method(CU_OB_CAMERA, "setproperty", cx_e_base_setproperty);
   cx_set_method(CU_OB_CAMERA, "stringvalue", camera_stringvalue   );
   cx_set_method(CU_OB_CAMERA, "destroy",     camera_destroy       );
   cx_set_method(CU_OB_CAMERA, "count",       cx_e_base_count      );
   cx_set_method(CU_OB_CAMERA, "stream",      cx_e_base_stream     );

   /*** Mark camera methods initialized and return successfully. ***/
       
   initialized = TRUE;
}
 
/*============================================================================
 *  cu_camera_pkg() -- public camera package initialization
 */

cx_Integer cu_e_camera_pkg(void)
{
   /*** Initialize camera methods if needed. ***/

   if (!initialized) camera_init();
   return TRUE;
}

/*** Non-polymorphic external functions follow. ***/

/*============================================================================
 *  cu_e_create_camera() -- return newly alloced & initialized camera, or NULL
 */

cx_Object cu_e_create_camera(cx_Object parent)
{
   CI_CAMERA	*cs;
   cx_Object	camera;

   /*** Initialize camera methods if needed. ***/

   if (!initialized) camera_init();

   /*** Create internal camera struct and and base object. ***/

   if (NULL == (cs = (CI_CAMERA *) cx_malloc(sizeof(CI_CAMERA)))) return NULL;
   if (NULL == (camera = cx_e_base_create(parent, CU_OB_CAMERA))) return NULL;

   /*** Initialize internal camera struct. ***/

   cs->string  = cx_strdup("unnamed");

   /*** Set camera object content to struct and return object. ***/

   cx_e_base_set_content(camera, cs);

   return camera;
}

/*============================================================================
 *  cu_e_camera_set_string() -- set string value associated with camera
 */

void cu_e_camera_set_string(cx_Object camera, cx_String s)
{
   CI_CAMERA	*cs = cx_e_base_content(camera);

   if (NULL != cs) {
      if (NULL != cs->string) cx_free(cs->string);
      cs->string = cx_strdup(s);
   }
}

/*============================================================================
 *  cu_e_camera_create_datatypes() -- create datatypes for object and property
 */

static char *dts[] =  {
  "CAMERA", CU_TAG_CAMERA,         "Camera",           "1", "STRING",
            "Named camera object",
  "CAMCX",  CU_PROP_CAMERA_X,      "Camera X",         "1", "REAL",
            "X-location of camera, angstroms",
  "CAMCY",  CU_PROP_CAMERA_Y,      "Camera Y",         "1", "REAL",
            "Y-location of camera, angstroms",
  "CAMCZ",  CU_PROP_CAMERA_Z,      "Camera Z",         "1", "REAL",
            "Z-location of camera, angstroms",
  "CAMSX",  CU_PROP_SCENE_X,       "Scene center X",   "1", "REAL",
            "X-location of camera, angstroms",
  "CAMSY",  CU_PROP_SCENE_Y,       "Scene center Y",   "1", "REAL",
            "Y-location of camera, angstroms",
  "CAMSZ",  CU_PROP_SCENE_Z,       "Scene center Z",   "1", "REAL",
            "Z-location of camera, angstroms",
  "CAMDF",  CU_PROP_FOCUS_DIST,    "Focus distance",   "1", "REAL",
            "Camera focus distance, angstroms",
  "CAMDH",  CU_PROP_CLIP_HITHER,   "Clip hither",      "1", "REAL",
	    "Distance from camera to hither clipping plane, angstroms",
  "CAMDY",  CU_PROP_CLIP_YON,      "Clip yon",         "1", "REAL",
	    "Distance from camera to yon clipping plane, angstroms",
  "CAMVX1", CU_PROP_VIEW_XMIN,     "View Xmin",        "1", "REAL",
	    "Distance from center to left clipping at hither plane",
  "CAMVX2", CU_PROP_VIEW_XMAX,     "View Xmax",        "1", "REAL",
	    "Distance from center to right clipping at hither plane",
  "CAMVY1", CU_PROP_VIEW_YMIN,     "View Ymin",        "1", "REAL",
	    "Distance from center to left clipping at hither plane",
  "CAMVY2", CU_PROP_VIEW_YMAX,     "View Ymax",        "1", "REAL",
	    "Distance from center to right clipping at hither plane",
  "CAMRW",  CU_PROP_RENDER_WIDTH,  "Render width",     "1", "REAL",
	    "Width of image when rendered, arbitrary units",
  "CAMRH",  CU_PROP_RENDER_HEIGHT, "Render height",    "1", "REAL",
	    "Height of image when rendered, arbitrary units",
  "CAMBG",  CU_PROP_BG_COLOR,      "Background color", "1", "INTEGER",
	    "Background color, RGB (orange = 255,165,0 = FFA500 = 16753920)",
  NULL
};

void cu_e_camera_create_datatypes(cx_Object table)
{
   static int defined = FALSE;
   char **p;

   if (!defined) {
      defined = TRUE;
      if (table == NULL) table = cx_default_datatypetable();
      for (p = dts; *p; p += 6)
         cx_create_datatype(table, p[0], p[1], p[2], p[3], p[4], p[5]);
   }
}
