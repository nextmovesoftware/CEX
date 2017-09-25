      SUBROUTINE BNDINF(MAXVAL,IBOND,NBOND,IAT,NB,MUL)
C=======================================================================
C Returns NB = number of bonds emanating from atom IAT, and
C MUL = highest bond multiplicity
C-----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      INTEGER  IBOND(MAXVAL,NAT),NBOND(NAT)

      MUL = 0
      NB  = NBOND(IAT)

cdcs  print *,'iat = ',iat,' nb = ',nb

      DO I=1,NB
         JAT = IBOND(I,IAT)
         M = 1
         DO J=I+1,NB
            IF (JAT.EQ.IBOND(J,IAT)) M=M+1
         END DO
         MUL = MAX(MUL,M)
      END DO

      RETURN
      END
