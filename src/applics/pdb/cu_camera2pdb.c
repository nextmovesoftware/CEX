/*****************************************************************************
*  cu_camera2pdb.c -- CU support for writing a camera in PDB format
*
*  This file provides functions to write PDB's using CU utilities:
*
*     cu_camera2pdb() ........... write camera to stream
*
*----------------------------------------------------------------------------
*
*  cx_Integer cu_camera2pdb(FILE *fp,
*                          cx_Object camera,);
*
*  which writes a PDB entry to given stream for the given camera.
* 
*  FALSE is returned on error, in which case error messages will be queued.
*
*----------------------------------------------------------------------------
*
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
#include "cx.h"
#include "cu.h"
#include "cx_pdb.h"
#include "pdb.h"

/*============================================================================
 *  cu_camera2pdb() - write a pdb entry to given stream for given camera
 */

cx_Integer cu_camera2pdb(FILE *fp,             /* output stream             */
                         cx_Object camera)    /* camera to output          */
{
   pdb_record	pdb;
   int          bg;

   pdb.record_type = PDB_USER_PDBRUN;
   pdb.pdb.user_pdbrun.version = 6;
   pdb_write_record(fp, &pdb, NULL, 0);

   /*** Write eye position record if camera x, y, and z are defined. ***/

   if (NULL != cx_sprop(camera, CU_PROP_CAMERA_X) &&
       NULL != cx_sprop(camera, CU_PROP_CAMERA_Y) &&
       NULL != cx_sprop(camera, CU_PROP_CAMERA_Z)) {
      pdb.record_type            = PDB_USER_EYEPOS;
      pdb.pdb.user_eyepos.xyz[0] = cx_rprop(camera, CU_PROP_CAMERA_X);
      pdb.pdb.user_eyepos.xyz[1] = cx_rprop(camera, CU_PROP_CAMERA_Y);
      pdb.pdb.user_eyepos.xyz[2] = cx_rprop(camera, CU_PROP_CAMERA_Z);
      pdb_write_record(fp, &pdb, NULL, 0);
       
   }

   /*** Write window geometry record if able. ***/

   if (NULL != cx_sprop(camera, CU_PROP_CLIP_HITHER) &&
       NULL != cx_sprop(camera, CU_PROP_CLIP_YON   ) &&
       NULL != cx_sprop(camera, CU_PROP_VIEW_XMIN  ) &&
       NULL != cx_sprop(camera, CU_PROP_VIEW_XMAX  ) &&
       NULL != cx_sprop(camera, CU_PROP_VIEW_YMIN  ) &&
       NULL != cx_sprop(camera, CU_PROP_VIEW_YMAX  )) {
      pdb.record_type = PDB_USER_WINDOW;
      pdb.pdb.user_window.left   = cx_rprop(camera, CU_PROP_VIEW_XMIN  );
      pdb.pdb.user_window.right  = cx_rprop(camera, CU_PROP_VIEW_XMAX  );
      pdb.pdb.user_window.bottom = cx_rprop(camera, CU_PROP_VIEW_XMIN  );
      pdb.pdb.user_window.top    = cx_rprop(camera, CU_PROP_VIEW_XMAX  );
      pdb.pdb.user_window.hither = cx_rprop(camera, CU_PROP_CLIP_HITHER);
      pdb.pdb.user_window.yon    = cx_rprop(camera, CU_PROP_CLIP_YON   );
      pdb_write_record(fp, &pdb, NULL, 0);
   }

   /*** Write focus distance (focal length) record if able. ***/

   if (NULL != cx_sprop(camera, CU_PROP_FOCUS_DIST)) {
      pdb.record_type = PDB_USER_FOCUS;
      pdb.pdb.user_focus.focus = cx_rprop(camera, CU_PROP_FOCUS_DIST);
      pdb_write_record(fp, &pdb, NULL, 0);
   }

   /*** Write scene center record if able. ***/

   if (NULL != cx_sprop(camera, CU_PROP_SCENE_X) &&
       NULL != cx_sprop(camera, CU_PROP_SCENE_Y) &&
       NULL != cx_sprop(camera, CU_PROP_SCENE_Z)) {
      pdb.record_type = PDB_USER_ATPOS;
      pdb.pdb.user_atpos.xyz[0] = cx_rprop(camera, CU_PROP_SCENE_X);
      pdb.pdb.user_atpos.xyz[1] = cx_rprop(camera, CU_PROP_SCENE_Y);
      pdb.pdb.user_atpos.xyz[2] = cx_rprop(camera, CU_PROP_SCENE_Z);
      pdb_write_record(fp, &pdb, NULL, 0);
   }

   /*** Write viewport record if able. ***/

   if (NULL != cx_sprop(camera, CU_PROP_RENDER_WIDTH ) &&
       NULL != cx_sprop(camera, CU_PROP_RENDER_HEIGHT)) {
      pdb.record_type            = PDB_USER_VIEWPORT;
      pdb.pdb.user_viewport.xmin = 0;
      pdb.pdb.user_viewport.ymin = 0;
      pdb.pdb.user_viewport.xmax = cx_rprop(camera, CU_PROP_RENDER_WIDTH );
      pdb.pdb.user_viewport.ymax = cx_rprop(camera, CU_PROP_RENDER_HEIGHT);
      pdb_write_record(fp, &pdb, NULL, 0);
   }

   /*** Write viewport record if able. ***/

   if (NULL != cx_sprop(camera, CU_PROP_BG_COLOR)) {
      bg                          = cx_iprop(camera, CU_PROP_BG_COLOR);
      pdb.record_type             = PDB_USER_BGCOLOR;
      pdb.pdb.user_bgcolor.rgb[0] = bg & 0xff0000 >> 8; /* red   */
      pdb.pdb.user_bgcolor.rgb[1] = bg & 0x00ff00 >> 4; /* green */
      pdb.pdb.user_bgcolor.rgb[2] = bg & 0x0000ff     ; /* blue  */
      pdb_write_record(fp, &pdb, NULL, 0);
   }

   pdb.record_type = PDB_USER_FILE;
   pdb.pdb.user_file.model = 0;
   (void) strcpy(pdb.pdb.user_file.filename, "none");
   pdb_write_record(fp, &pdb, NULL, 0);

   return TRUE;
}
