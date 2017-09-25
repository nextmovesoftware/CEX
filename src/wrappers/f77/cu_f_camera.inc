C-----------------------------------------------------------------------
C  cu_f_camera.h -- CU camera definitions and wrappers for Fortran
C
C  cx.h should be included before this file.
C-----------------------------------------------------------------------
C  Author and institution: Conrad Huang, UCSF
C
C  This source code is contributed to the public domain and may be
C  freely copied and redistributed for research, profit, fun or any
C  other reason, with these restrictions: (1) unmodified or
C  functionally equivalent code derived from CX code must contain this
C  notice, (2) all derived code must acknowledge the author and
C  institution, and (3) the functional definition of symbols starting
C  CX_ or cx_ may not be changed (if you need to change a function,
C  CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C  Camera datatype tag
C-----------------------------------------------------------------------
      CHARACTER*(*) CU_TAG_CAMERA
      PARAMETER ( CU_TAG_CAMERA           = 'CAMERA' )
C-----------------------------------------------------------------------
C  Camera property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CU_PROP_CAMERA_POSITION, CU_PROP_SCENE_CENTER,
     &              CU_PROP_FOCAL_LENGTH,    CU_PROP_CLIP_HITHER,
     &              CU_PROP_CLIP_YON,        CU_PROP_VIEW_AREA,
     &              CU_PROP_RENDER_SIZE,     CU_PROP_BG_COLOR
      PARAMETER ( CU_PROP_CAMERA_POSITION = 'camera position' )
      PARAMETER ( CU_PROP_SCENE_CENTER    = 'scene center' )
      PARAMETER ( CU_PROP_FOCAL_LENGTH    =  'focal length' )
      PARAMETER ( CU_PROP_CLIP_HITHER     =  'clip hither' )
      PARAMETER ( CU_PROP_CLIP_YON        =  'clip yon' )
      PARAMETER ( CU_PROP_VIEW_AREA       =  'view area' )
      PARAMETER ( CU_PROP_RENDER_SIZE     =  'render size' )
      PARAMETER ( CU_PROP_BG_COLOR        =  'background color' )
C-----------------------------------------------------------------------
C  Fortran wrappers for public molecule functions
C
C  Note: subroutines (void entry points) are not declared in Fortran.
C-----------------------------------------------------------------------
      INTEGER*4 CU_F_CREATE_CAMERA
      INTEGER*4 CU_F_CAMERA_PKG
C     (CALL)    CU_F_CAMERA_SET_STRING
C     (CALL)    CU_F_CAMERA_CREATE_DATATYPES
C-----------------------------------------------------------------------
