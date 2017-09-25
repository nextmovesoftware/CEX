      SUBROUTINE SETDOUBLE(DODOTS,MAXVAL,IBOND,NBOND,DOTS,IA,IB,LUNOUT)
C=======================================================================
C  Set double bond or DOTS on IA-IB.
C-----------------------------------------------------------------------
      INCLUDE   'bondage.cmn'
      INTEGER    IBOND(MAXVAL,NAT),NBOND(NAT),DOTS(NAT)
      LOGICAL    DODOTS

      IF (DODOTS) THEN
         DOTS(IA) = 1
         DOTS(IB) = 1
      ELSE
         CALL ADDBND(MAXVAL,IBOND,NBOND,IA,IB,'double',LUNOUT)
      END IF
      RETURN
      END
