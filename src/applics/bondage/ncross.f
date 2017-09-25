      SUBROUTINE NCROSS (A,B,AXB)
C=======================================================================
C  Returns normalized vector cross product AXB between vectors A, B
C-----------------------------------------------------------------------
      REAL   A(3),B(3),AXB(3),RS
      CALL CROSS(A,B,AXB)
      RS = 0.0
      DO 10 K = 1,3
         RS = RS + AXB(K)*AXB(K)
10    CONTINUE
      IF (RS.NE.0.0) THEN
         RS = SQRT(RS)
         DO 20 K = 1,3
            AXB(K) = AXB(K)/RS
20       CONTINUE
      ELSE
         DO 30 K = 1,3
            AXB(K) = 0.0
30       CONTINUE
      END IF
      RETURN
      END
