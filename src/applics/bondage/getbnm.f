      SUBROUTINE GETBNM(IMOL,MAX,IBOND,NBOND)
C=======================================================================
C Return list of bonds in molecule IMOL by atom
C-----------------------------------------------------------------------
      INTEGER IBOND(MAX,NAT),NBOND(NAT)
      INCLUDE 'bondage.cmn'

      DO 10 KAT = ISTC(IMOL),IENC(IMOL)
         NBOND(KAT) = 0
10    CONTINUE
      DO 20 KBN = ISTJ(IMOL),IENJ(IMOL)
         IA = IANO(KBN)
         IB = IBNO(KBN)
         NBOND(IA) = NBOND(IA) + 1
         IF (NBOND(IA).LE.MAX) IBOND(NBOND(IA),IA) = IB
         NBOND(IB) = NBOND(IB) + 1
         IF (NBOND(IB).LE.MAX) IBOND(NBOND(IB),IB) = IA
20    CONTINUE
      RETURN
      END
