      FUNCTION LENSTR( STRING )
C======================================================================
C  lenstr -- return length of string as defined by the last nonblank
C  (Returns 0 if all blank.)
C----------------------------------------------------------------------
      INTEGER    LENSTR, LAST, I
      CHARACTER*(*) STRING
      LAST = LEN( STRING )
      DO 10 I=LAST,1,-1
         IF (STRING(I:I).NE.' ') THEN
            LENSTR = I
            RETURN
         ENDIF
10    CONTINUE
      LENSTR = 0
      END
