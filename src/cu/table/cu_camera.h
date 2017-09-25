/*****************************************************************************
*  cu_camera.h -- CU camera package definitions
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Conrad Huang, UCSF
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CU code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#ifndef CU_CAMERA_INCLUDED
#define CU_CAMERA_INCLUDED 1

/*****************************************************
 *  Supported classes defined in cx_types.h:
 *
 *     CU_OB_CAMERA
 */

#include "cx_types.h"

/*** Camera datatype tag. ***/

#define	CU_TAG_CAMERA		"CAMERA"

/*** Camera property names. ***/

#define CU_PROP_CAMERA        "camera"
#define CU_PROP_CAMERA_X      "camera-x"
#define CU_PROP_CAMERA_Y      "camera-y"
#define CU_PROP_CAMERA_Z      "camera-z"
#define CU_PROP_SCENE_X       "scene-x"
#define CU_PROP_SCENE_Y       "scene-y"
#define CU_PROP_SCENE_Z       "scene-z"
#define CU_PROP_FOCUS_DIST    "focus"
#define CU_PROP_CLIP_HITHER   "clip-hither"
#define CU_PROP_CLIP_YON      "clip-yon"
#define CU_PROP_VIEW_XMIN     "view-xmin"
#define CU_PROP_VIEW_XMAX     "view-xmax"
#define CU_PROP_VIEW_YMIN     "view-ymin"
#define CU_PROP_VIEW_YMAX     "view-ymax"
#define CU_PROP_RENDER_WIDTH  "render-width"
#define CU_PROP_RENDER_HEIGHT "render-height"
#define CU_PROP_BG_COLOR      "bg-rgb"

/*** C-wrappers for all public functions in camera package. ***/

#define cu_create_camera		cu_e_create_camera
#define cu_camera_pkg			cu_e_camera_pkg
#define cu_camera_set_string		cu_e_camera_set_string
#define cu_camera_create_datatypes	cu_e_camera_create_datatypes

/*** Declarations of public functions in cu_camera.c. ***/

cx_Object  cu_e_create_camera(cx_Object parent);
cx_Integer cu_e_camera_pkg(void);
void       cu_e_camera_set_string(cx_Object camera, cx_String s);
void       cu_e_camera_create_datatypes(cx_Object table);


#endif /* CU_CAMERA_INCLUDED */
