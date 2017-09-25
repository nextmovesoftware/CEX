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
C  Molecule property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_INSMI
      PARAMETER ( CX_PROP_INSMI = 'input smiles' )
C-----------------------------------------------------------------------
C  Atom and Bond property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_INORD, CX_PROP_VISIT
      PARAMETER ( CX_PROP_INORD = 'input order' )
      PARAMETER ( CX_PROP_VISIT = 'visit'       )
C-----------------------------------------------------------------------
C  Atom property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_ATNUMB, CX_PROP_ATSYMB, CX_PROP_ALABEL,
     &              CX_PROP_IMPH,   CX_PROP_MASS,   CX_PROP_CHARGE,
     &              CX_PROP_CHIRAL
      PARAMETER ( CX_PROP_ATNUMB = 'atomic number'   )
      PARAMETER ( CX_PROP_ATSYMB = 'atomic symbol'   )
      PARAMETER ( CX_PROP_ALABEL = 'atom label'      )
      PARAMETER ( CX_PROP_IMPH   = 'implicit hcount' )
      PARAMETER ( CX_PROP_MASS   = 'mass'            )
      PARAMETER ( CX_PROP_CHARGE = 'charge'          )
      PARAMETER ( CX_PROP_CHIRAL = 'chirality'       )
C-----------------------------------------------------------------------
C  Bond property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_BORDER, CX_PROP_BSYMB, CX_PROP_DBO,
     &              CX_PROP_BLABEL
      PARAMETER ( CX_PROP_BORDER = 'bond order'  )
      PARAMETER ( CX_PROP_BSYMB  = 'bond symbol' )
      PARAMETER ( CX_PROP_DBO    = 'dbo'         )
      PARAMETER ( CX_PROP_BLABEL = 'bond label'  )
C-----------------------------------------------------------------------
C  Chiral classes:
C    TetraHedral ALlene-like SquarePlanar TrigonalBiyramidal OctaHedral
C-----------------------------------------------------------------------
      INTEGER*4 CX_CHI_NONE, CX_CHI_TH, CX_CHI_AL,
     &          CX_CHI_SP,   CX_CHI_TB, CX_CHI_OH

      PARAMETER ( CX_CHI_NONE =  0 )
      PARAMETER ( CX_CHI_TH   =  3 )
      PARAMETER ( CX_CHI_AL   =  4 )
      PARAMETER ( CX_CHI_SP   = 16 )
      PARAMETER ( CX_CHI_TB   = 17 )
      PARAMETER ( CX_CHI_OH   = 18 )
C-----------------------------------------------------------------------
C  Symbolic names for double bond orientations
C-----------------------------------------------------------------------
      INTEGER*4 CX_CHI_NO_DBO, CX_CHI_CIS, CX_CHI_TRANS, CX_CHI_UP,
     &          CX_CHI_DOWN, CX_CHI_FORWARD, CX_CHI_REVERSE
      PARAMETER ( CX_CHI_NO_DBO  = 0 )
      PARAMETER ( CX_CHI_CIS     = 1 )
      PARAMETER ( CX_CHI_TRANS   = 2 )
      PARAMETER ( CX_CHI_UP      = 3 )
      PARAMETER ( CX_CHI_DOWN    = 4 )
      PARAMETER ( CX_CHI_FORWARD = 5 )
      PARAMETER ( CX_CHI_REVERSE = 6 )
C-----------------------------------------------------------------------
C  Fortran wrappers for public molecule functions
C
C  Note: subroutines (void entry points) are not declared in Fortran.
C-----------------------------------------------------------------------
      INTEGER*4 CX_F_ATOMTUPLE_NAME
      INTEGER*4 CX_F_BOND
      INTEGER*4 CX_F_BONDTUPLE_NAME
      INTEGER*4 CX_F_CREATE_ATOM
      INTEGER*4 CX_F_CREATE_ATOMTUPLE
      INTEGER*4 CX_F_CREATE_BOND
      INTEGER*4 CX_F_CREATE_BONDTUPLE
      INTEGER*4 CX_F_CREATE_MOLECULE
      INTEGER*4 CX_F_MOLECULE_PKG
C     (CALL)    CX_F_MOL_ADDHALL
      INTEGER*4 CX_F_MOL_COPY
      INTEGER*4 CX_F_MOL_HEAVYCOPY
C     (CALL)    CX_F_MOL_SETIMPH
C     (CALL)    CX_F_MOL_ZAPH
      INTEGER*4 CX_F_PREFIX2ATUPLES
      INTEGER*4 CX_F_PREFIX2BTUPLES
      INTEGER*4 CX_F_SMILIN
      INTEGER*4 CX_F_XATOM
C-----------------------------------------------------------------------
