      SUBROUTINE AUTOBOND(IMOL,LUNOUT)
C=======================================================================
C Automatically generate double bonds in molecule IMOL from input 
C connectivity and coordinates:
C   1.  Set only cyclic bonds, run through BCKTRK to generate Kekule
C       structure.
C   2.  Set all bonds, run through BCKTRK again to pick up remaining
C       conjugated acyclic stuff.
C   3.  Finally, set all remaining bonds.
C-----------------------------------------------------------------------
      INCLUDE   'bondage.cmn'
      PARAMETER (MAXVAL=6)
      INTEGER    IBOND(MAXVAL,MAXAT),NBOND(MAXAT),DOTS(MAXAT)
      LOGICAL    CYCLIC,DODOTS

C-----------------------------------------------------------------------
C Set only cyclic bonds, run through BCKTRK to generate Kekule
C structure.
C-----------------------------------------------------------------------
      DO I = ISTC(IMOL),IENC(IMOL)
         DOTS(I) = 0
      END DO
      CYCLIC = .TRUE.
      DODOTS = .TRUE.
cdcs  print *,' First genbond : imol = ',imol,istj(imol),ienj(imol)
      CALL GENBOND(IMOL,DODOTS,CYCLIC,MAXVAL,IBOND,NBOND,DOTS,LUNOUT)
      CALL BCKTRK(IMOL,CYCLIC,MAXVAL,IBOND,NBOND,DOTS,LUNOUT)

C-----------------------------------------------------------------------
C Set all bonds, run through BCKTRK again to pick up remaining
C conjugated acyclic stuff.
C-----------------------------------------------------------------------
      DO I = ISTC(IMOL),IENC(IMOL)
         DOTS(I) = 0
      END DO
      CYCLIC = .FALSE.
      DODOTS = .TRUE.
cdcs  print *,' Second genbond : imol = ',imol,istj(imol),ienj(imol)
      CALL GENBOND(IMOL,DODOTS,CYCLIC,MAXVAL,IBOND,NBOND,DOTS,LUNOUT)
      CALL BCKTRK(IMOL,CYCLIC,MAXVAL,IBOND,NBOND,DOTS,LUNOUT)

C-----------------------------------------------------------------------
C Finally, set all remaining bonds.
C-----------------------------------------------------------------------
      CYCLIC = .FALSE.
      DODOTS = .FALSE.
cdcs  print *,' Last genbond : imol = ',imol,istj(imol),ienj(imol)
      CALL GENBOND(IMOL,DODOTS,CYCLIC,MAXVAL,IBOND,NBOND,DOTS,LUNOUT)
      
      RETURN
      END
