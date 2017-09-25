      SUBROUTINE SQUEEZE(IN,LIN,OUT,LOUT)
C======================================================================
C  Remove blanks from string IN(:LIN) and return as OUT(:LOUT).
C----------------------------------------------------------------------
      CHARACTER*(*) IN,OUT
      INTEGER       LIN,LOUT

      OUT=IN(:LIN)
      LENGTH=LIN
10    IPTR=INDEX(OUT(:LENGTH),' ')
      IF (IPTR.GT.1) THEN
         OUT = OUT(:IPTR-1) // OUT(IPTR+1:LENGTH)
         LENGTH = LENGTH-1
         GO TO 10
      ELSE IF (IPTR.EQ.1) THEN
         OUT = OUT(2:LENGTH)
         LENGTH = LENGTH-1
         GO TO 10
      END IF
      LOUT=LENGTH
      RETURN
      END
