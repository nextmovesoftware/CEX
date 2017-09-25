      INTEGER FUNCTION NXTATT(MAXVAL,IBOND,NBOND,ATOM,I)
C=======================================================================
C  This function returns the next atom after I that is attached to ATOM.
C  If there is no next atom, -1 is returned.
C-----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      INTEGER  IBOND(MAXVAL,NAT),NBOND(NAT)
      INTEGER  I,J,ATOM

      J = I+1
      IF (J.LE.NBOND(ATOM)) THEN
        NXTATT = IBOND(J,ATOM) 
        I = I+1
      ELSE
        NXTATT = -1
      END IF
      RETURN
      END
