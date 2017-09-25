      SUBROUTINE WRITPF(JCHN,IER)
C=======================================================================
C.. Write coordinates to JCHN in Protein Data Bank format.
C-----------------------------------------------------------------------
      INCLUDE      'bondage.cmn'
      PARAMETER    (MAXB = 7)
      INTEGER       NBNDS(MAXAT),KBNDS(MAXAT,MAXB)
      LOGICAL       COL13

      DO 40 K=1,NAT
         NBNDS(K) = 0
40    CONTINUE

      DO 200 KMOL = 1,NSEC
         IF (ISTJ(KMOL).GT.0) THEN
            DO 80 KBND = ISTJ(KMOL),IENJ(KMOL)
               IA = IANO(KBND)
               IB = IBNO(KBND)
               NBNDS(IA) = NBNDS(IA) + 1
               IF (NBNDS(IA).GT.MAXB) GOTO 800
               NBNDS(IB) = NBNDS(IB) + 1
               IF (NBNDS(IB).GT.MAXB) GOTO 800
               KBNDS(IA,NBNDS(IA)) = IB
               KBNDS(IB,NBNDS(IB)) = IA
80          CONTINUE
         END IF
         DO 100 KRES = 1,NSEG
            IF (ISCML(KRES).EQ.KMOL) THEN
               DO 90 KAT = ISTS(KRES),IENS(KRES)
                  IF ( COL13( ATNM(KAT) ) ) THEN
                     WRITE (JCHN,82,ERR=999) KAT+KMOL-1,
     .                  ATNM(KAT),SECNM(KRES)(:4),SECSQ(KRES),
     .                  CHAINID(KRES),(CR(L,KAT),L=1,3)
82                   FORMAT('ATOM',2X,I5,1X,A4,1X,A4,1X,I4,A1,3X,4F8.3)
                  ELSE
                     WRITE (JCHN,84,ERR=999) KAT+KMOL-1,
     .                  ATNM(KAT),SECNM(KRES)(:4),SECSQ(KRES),
     .                  CHAINID(KRES),(CR(L,KAT),L=1,3)
84                   FORMAT('ATOM',2X,I5,2X,A4,A4,1X,I4,A1,3X,4F8.3)
                  END IF
90             CONTINUE
            END IF
100      CONTINUE
         WRITE (JCHN,'(A,I5)') 'TER   ',KAT+KMOL-1
200   CONTINUE

      DO 300 KMOL = 1,NSEC
         DO 210 KAT=ISTC(KMOL),IENC(KMOL)
            WRITE(JCHN,'(A,11I5)',ERR=999) 'CONECT',
     .       KAT+KMOL-1,
     .       (KBNDS(KAT,K)+KMOL-1,K=1,NBNDS(KAT))
210      CONTINUE
300   CONTINUE
      WRITE (JCHN,'(A)',ERR=999) 'END'

      IER = 0
      CLOSE(JCHN)
      RETURN

800   WRITE(6,8001) MAXB-1
8001  FORMAT(' More than ',I2,' bonds from an atom')
      IER = 1
      RETURN
999   WRITE(6,9991)
9991  FORMAT(' Io error writing *PDB* file')
      IER = 1
      RETURN

      END
