      INTEGER FUNCTION GETBND(IMOL,ATOM,ATT)
C=======================================================================
C  Determine the sequence number of the bond between atoms ATOM and ATT
C  in molecule IMOL.  Return -1 if they are not bonded. 
C-----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      INTEGER ATOM,ATT

      GETBND = -1
      IF (ATOM.EQ.0 .OR. ATT.EQ.0) RETURN
      DO 10 L = ISTJ(IMOL),IENJ(IMOL)
         IF (((IANO(L).EQ.ATOM).AND.(IBNO(L).EQ.ATT)) .OR.
     .      ((IBNO(L).EQ.ATOM).AND.(IANO(L).EQ.ATT))) THEN
           GETBND = L
           RETURN
         END IF
10    CONTINUE
      RETURN
      END
