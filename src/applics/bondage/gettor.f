      SUBROUTINE GETTOR(AT1,AT2,AT3,AT4,THETA)
C======================================================================
C   Determines the torsional angle defined by atoms AT1-AT2-AT3-AT4
C   in range 0-360.
C----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      REAL     V1(3),V2(3),V3(3),P(3),Q(3)
      REAL     ABSTH,XTHETA,S,THETA,DOT
      INTEGER  AT1,AT2,AT3,AT4

      DO 10 N=1,3
          V1(N) = CR(N,AT1) - CR(N,AT2)
          V2(N) = CR(N,AT2) - CR(N,AT3)
          V3(N) = CR(N,AT3) - CR(N,AT4)
10    CONTINUE
      CALL NCROSS(V2,V1,P)
      CALL NCROSS(V3,V2,Q)
      XTHETA = DOT(P,Q)
C.. Check for rounding errors
      IF (XTHETA.GT. 1.0) XTHETA= 1.0
      IF (XTHETA.LT.-1.0) XTHETA=-1.0
      THETA = ACOS(XTHETA)
C.. Convert to degrees...
      THETA = THETA * 57.29578
      ABSTH = ABS(THETA)
      IF (ABSTH.LT.0.001) THEN
         THETA = 0.0
      ELSE IF (ABS(ABSTH-180.0).LT.0.001) THEN
         THETA = 180.0
      ELSE
         S = DOT(V1,Q)
         IF (S.LT.0.0) THETA = 360.0 - THETA
      END IF
      IF (THETA.LT.0.0) THETA = THETA+360.0
      IF (THETA.GT.360.0) THETA = THETA-360.0
      RETURN
      END
