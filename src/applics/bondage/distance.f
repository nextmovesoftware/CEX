      SUBROUTINE DISTANCE(I,J,D)
C=======================================================================
C  Returns the distance D between atoms I, J
C-----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      REAL     D,DT

      D=0.0
      DO 10 N=1,3
         DT=CR(N,J)-CR(N,I)
         D=D+DT*DT
   10 CONTINUE
      D=SQRT(D)
      RETURN
      END
