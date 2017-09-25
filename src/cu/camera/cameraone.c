/*****************************************************************************
*  cameraone.c -- a non-standard test program
*----------------------------------------------------------------------------
*  Authors: Conrad Huang (UCSF) and Dave Weininger (Daylight)
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
#include "cu_camera.h"

/*============================================================================
 *  main() non-standard test program
 */

int main(int argc, char **argv)
{
   cx_Object  camera, camdt, outs;

   /*** Initialize. ***/

   cu_camera_pkg();
   cu_camera_create_datatypes(NULL);

   /*** Get CAMERA datatype to attach to camera. ***/

   camdt = cx_tag2datatype(NULL, CU_TAG_CAMERA);

   /*** Create a camera. ***/

   camera = cu_create_camera(NULL);

   if (NULL != camera) {

      /*** Attach datatype to camera and name it. ***/

      cx_set_datatype(camera, camdt);
      cu_camera_set_string(camera, "one");

      /*** Create camera properties. ***/
   
      cx_set_rprop(camera, CU_PROP_CAMERA_X,        0.0 );
      cx_set_rprop(camera, CU_PROP_CAMERA_Y,        0.0 );
      cx_set_rprop(camera, CU_PROP_CAMERA_Z,        0.0 );
      cx_set_rprop(camera, CU_PROP_SCENE_X,         1.0 );
      cx_set_rprop(camera, CU_PROP_SCENE_Y,         1.0 );
      cx_set_rprop(camera, CU_PROP_SCENE_Z,         1.0 );
      cx_set_rprop(camera, CU_PROP_FOCUS_DIST,     10.0 );
      cx_set_rprop(camera, CU_PROP_CLIP_HITHER,     1.0 );
      cx_set_rprop(camera, CU_PROP_CLIP_YON,       10.0 );
      cx_set_rprop(camera, CU_PROP_VIEW_XMIN,      -1.0 );
      cx_set_rprop(camera, CU_PROP_VIEW_XMAX,       1.0 );
      cx_set_rprop(camera, CU_PROP_VIEW_YMIN,      -1.0 );
      cx_set_rprop(camera, CU_PROP_VIEW_YMAX,       1.0 );
      cx_set_rprop(camera, CU_PROP_RENDER_WIDTH,  100.0 );
      cx_set_rprop(camera, CU_PROP_RENDER_HEIGHT, 100.0 );

      /*** Background: firebrick = 178,34,34 = B22222 = 11674146 ***/

      cx_set_iprop(camera, CU_PROP_BG_COLOR,   11674146 );

      /*** Create output stream and send camera on it. ***/

      outs = cx_create_iostream("-", CX_IO_WRITE);
      cx_append(outs, camera);

      /*** Clean up. ***/

      cx_destroy(camera);
      cx_destroy(outs  );
   }

   /*** Spew errors, clean up, and exit successfully. ***/

   cx_error_spew(stderr, NULL);
   cx_cleanup();
   exit(0);
}
