C-----------------------------------------------------------------------
C  cx_f_molecule.h -- CX molecule definitions and wrappers for Fortran
C
C  cx.h should be included before this file.
C-----------------------------------------------------------------------
C  Author and institution: Dave Weininger, Daylight CIS, Inc.
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
C  Surface datatype tags
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_TAG_SURFACE,     CX_TAG_VERTEX, CX_TAG_FACE,
     &              CX_TAG_VERTEXTUPLE, CX_TAG_FACETUPLE
      PARAMETER ( CX_TAG_SURFACE     = 'SURF'   )
      PARAMETER ( CX_TAG_VERTEX      = 'VERTEX' )
      PARAMETER ( CX_TAG_FACE        = 'FACE'   )
      PARAMETER ( CX_TAG_VERTEXTUPLE = 'VTUPLE' )
      PARAMETER ( CX_TAG_FACETUPLE   = 'FTUPLE' )
C-----------------------------------------------------------------------
C  Surface property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_SURF_GENERATION , CX_PROP_SURF_CLOSED,
     &              CX_PROP_SURF_WELL_FORMED, CX_PROP_SURF_AREA,
     &              CX_PROP_SURF_VOLUME     , CX_PROP_SURF_FACE_ORDER,
     &              CX_PROP_SURF_COORDINATES, CX_PROP_SURF_FACES,
     &              CX_PROP_SURF_REMARK     , CX_PROP_SURF_NAME
      PARAMETER ( CX_PROP_SURF_GENERATION  = 'generation method'    )
      PARAMETER ( CX_PROP_SURF_CLOSED      = 'completeness'         )
      PARAMETER ( CX_PROP_SURF_WELL_FORMED = 'well formed'          )
      PARAMETER ( CX_PROP_SURF_AREA        = 'surface area'         )
      PARAMETER ( CX_PROP_SURF_VOLUME      = 'volume enclosed'      )
      PARAMETER ( CX_PROP_SURF_FACE_ORDER  = 'vertex order in face' )
      PARAMETER ( CX_PROP_SURF_COORDINATES = 'coordinates'          )
      PARAMETER ( CX_PROP_SURF_FACES       = 'faces'                )
      PARAMETER ( CX_PROP_SURF_REMARK      = 'remark'               )
      PARAMETER ( CX_PROP_SURF_NAME        = 'surfname'             )
C-----------------------------------------------------------------------
C  Vertex property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_CURVATURE
      PARAMETER ( CX_PROP_CURVATURE = 'surface curvature' )
C-----------------------------------------------------------------------
C  Face property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_FACE_AREA
      PARAMETER ( CX_PROP_FACE_AREA = 'area of face' )
C-----------------------------------------------------------------------
C  Fortran wrappers for public molecule functions
C
C  Note: subroutines (void entry points) are not declared in Fortran.
C-----------------------------------------------------------------------
      INTEGER*4	CX_F_FACETUPLE_NAME
      INTEGER*4	CX_F_VERTEXTUPLE_NAME
      INTEGER*4	CX_F_VERTEX_FROM_ID
      INTEGER*4	CX_F_VERTEX_ID
      INTEGER*4 CX_F_CREATE_FACE
      INTEGER*4 CX_F_CREATE_FACETUPLE
      INTEGER*4 CX_F_CREATE_SURFACE
      INTEGER*4 CX_F_CREATE_VERTEX
      INTEGER*4 CX_F_CREATE_VERTEXTUPLE
      INTEGER*4 CX_F_FACE
      INTEGER*4 CX_F_PREFIX2FTUPLES
      INTEGER*4 CX_F_PREFIX2VTUPLES
      INTEGER*4 CX_F_SURFACE_PKG
C     (CALL)    CX_F_SURFACE_CREATE_DATATYPES
C-----------------------------------------------------------------------
