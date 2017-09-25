      SUBROUTINE UPPER(S,U,LENGTH)
C=======================================================================
C Convert character string to upper case
C-----------------------------------------------------------------------
      CHARACTER*(*) S,U
      CHARACTER*26 LCASE,UCASE
      DATA LCASE /'abcdefghijklmnopqrstuvwxyz'/
      DATA UCASE /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

      DO 10 K = 1,LENGTH
         IF (S(K:K) .LT. 'a' .OR. S(K:K) .GT. 'z') THEN
            U(K:K) = S(K:K)
         ELSE
            I = INDEX(LCASE,S(K:K))
            U(K:K) = UCASE(I:I)
         END IF
10    CONTINUE
      U = U(:LENGTH)

      RETURN
      END
