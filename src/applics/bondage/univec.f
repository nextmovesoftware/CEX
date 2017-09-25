      SUBROUTINE UNIVEC(C1,C2)
C=======================================================================
C Generates unit vector C2 from C1
C-----------------------------------------------------------------------
      REAL   S,C1(3),C2(3),SMALL
      PARAMETER (SMALL=1.0E-5)

      S = 0.0
      DO 10 L = 1,3
         C2(L) = C1(L)
         S = S + C1(L)*C1(L)
10    CONTINUE
      S = SQRT(S)
      IF (S.GT.SMALL) THEN
         DO 20 L = 1,3
            C2(L) = C2(L) / S
20       CONTINUE
      END IF
      RETURN
      END
