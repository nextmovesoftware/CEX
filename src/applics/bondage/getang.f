      SUBROUTINE GETANG(IAT,JAT,KAT,THETA)
C=======================================================================
C Determines the angle defined by IAT-JAT-KAT
C-----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      REAL     V1(3),V2(3),THETA,CTHETA,DOT

      DO 10 K=1,3
          V1(K) = CR(K,IAT) - CR(K,JAT)
          V2(K) = CR(K,KAT) - CR(K,JAT)
10    CONTINUE
      CALL UNIVEC(V1,V1)
      CALL UNIVEC(V2,V2)
      CTHETA = DOT(V1,V2)
C.. Check for rounding errors...
      IF (CTHETA.GT. 1.0) CTHETA= 1.0
      IF (CTHETA.LT.-1.0) CTHETA=-1.0
      THETA = ACOS(CTHETA)
C.. Convert to degrees...
      THETA = THETA * 57.29578
      RETURN
      END
