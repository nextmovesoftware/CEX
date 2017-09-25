      BLOCK DATA RADIIDAT
      INCLUDE 'bondage.cmn'
C-----------------------------------------------------------------------
C Atomic symbols for default covalent (CRAD) and van der Waals (VRAD)
C radii. NTYPES = # atomic symbols and radii assigned
C Note: 'bondage.siz' contains parameter statement for MAXTYP.  There
C        must be MAXTYP total values initialized in each DATA statement.
C-----------------------------------------------------------------------
      DATA NTYPES /12/
      DATA SYM/  'C', 'N', 'O', 'S', 'P', 'F', 'Cl','Br','I', 'H','Du',
     .           'DU',38*' '/
      DATA CRAD/ 0.85,0.85,0.75,1.05,0.96,0.60,1.00,1.15,1.35,0.33,-9.0,
     .           -9.0,38*0.0/
      DATA VRAD/ 1.65,1.55,1.40,1.80,1.90,1.45,1.80,1.95,2.10,0.10, 0.0,
     .            0.0,38*0.0/

      END
