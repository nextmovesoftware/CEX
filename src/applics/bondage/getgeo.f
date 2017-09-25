      SUBROUTINE GETGEO(IMOL,MAXVAL,GEOM,IBOND,NBOND)
C=======================================================================
C Determines geometry of all atoms in molecule IMOL.  Returns GEOM(K)
C indicating geometry, as follows:
C   GEOM(K) = 0   indeterminate
C             1   end atom
C             2   linear
C             3   trigonal
C             4   tetrahedral
C-----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      INTEGER IBOND(MAXVAL,NAT),NBOND(NAT),GEOM(NAT)
      REAL    ANGMIN,ANGMAX,SUMANG,A

      DO I = ISTC(IMOL),IENC(IMOL)
         GEOM(I) = 0
      END DO
C-----------------------------------------------------------------------
C Check 5-membered rings and set their atoms' geometry by checking
C torsion angles.
C-----------------------------------------------------------------------
      CALL RING5(IMOL,MAXVAL,IBOND,NBOND,GEOM)
      DO 50  KAT = ISTC(IMOL), IENC(IMOL)
         IF (GEOM(KAT).NE.0) GO TO 50
         CALL BNDINF(MAXVAL,IBOND,NBOND,KAT,NB,MUL)
         NBKAT = NB-MUL+1
         IF(NBKAT .EQ. 1)  THEN
            IG = 1
         ELSE
            IG = 0
         END IF
         ANGMIN = 999.
         ANGMAX = 0.0
         NANG = 0
         SUMANG = 0.0
         DO 30  I = 1, NBOND(KAT)-1
            IAT = IBOND(I,KAT)
            DO 20  J = I+1, NBOND(KAT)
               JAT = IBOND(J,KAT)
               IF (JAT.EQ.IAT) GO TO 20
               CALL GETANG(IAT,KAT,JAT,A)
               NANG = NANG+1
               SUMANG = SUMANG+A
               IF(A .LT. ANGMIN)  ANGMIN = A
               IF(A .GT. ANGMAX)  ANGMAX = A
               IF(A.GT.160.)  THEN
                  IA = 2
c              ELSE IF(A.LT.135. .AND. A.GE.115.)  THEN
               ELSE IF(A.LT.135. .AND. A.GE.112.)  THEN
                  IA = 3
c              ELSE IF(A.LT.115. .AND. A.GT.100.)  THEN
               ELSE IF(A.LT.112. .AND. A.GT.100.)  THEN
                  IA = 4
               ELSE
                  IA = -1
               END IF
               IF(IG .EQ. 0)  THEN
                  IG = IA
               ELSE IF(IG .NE. IA)  THEN
                  IG = -1
               END IF
20          CONTINUE
30       CONTINUE

C-----------------------------------------------------------------------
C Additional checks, if above did not decide the case, for tetrahedral
C or trigonal geometry when proper number of substituents are present.
C These are a more relaxed test for tetrahedral angles, and planar
C geometry with angles distorted from 120 degrees...
C-----------------------------------------------------------------------
         IF(IG .LE. 0)  THEN
            IF(NBKAT .EQ. 4)  THEN
               IF(ANGMIN .GT. 90. .AND. ANGMAX .LT. 135.)  THEN
                  IG = 4
               END IF
            ELSE IF(NBKAT .EQ. 3)  THEN
               IF(ANGMIN .GT. 100. .AND. ANGMAX .LT. 140.)  THEN
                  IF(NANG .EQ. 3)  THEN
                     IF(ABS(360.-SUMANG) .LT. 3.5)  THEN
                        IG = 3
                     END IF
                  END IF
               END IF
            END IF
         END IF
         GEOM(KAT) = MAX(0,IG)
50    CONTINUE
      RETURN
      END
