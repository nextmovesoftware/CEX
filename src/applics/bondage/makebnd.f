      SUBROUTINE MAKEBND(N,IER,MAXB,IBOND,IB,MOL,NOJOIN,NLASTAT)
C=======================================================================
C    LOAD CONNECTIVITY ARRAY
C-----------------------------------------------------------------------
      INCLUDE    'bondage.cmn'
      INTEGER     IBOND(MAXB,MAXAT),IB(MAXB),MOL,NOJOIN,NLASTAT
      CHARACTER*1 CHID
cdcs  print *,'In Makebnd:',n,ierr,maxb
cdcs  print *, mol,nojoin,nlastat
      DO 180 I=1,N
          IA=I+NLASTAT
          DO 10 J=1,MAXB
             IF (IBOND(J,I).NE.0) THEN
                IF(J.GT.4) THEN
                   CALL VERIFYATM(IA,NM,NR,CHID)
                   WRITE(6,'('' Warning: > 4 bonds to '',2I4,A1,A4)')
     .              NM,NR,CHID,ATNM(IA)
                END IF
                IB(J)=IBOND(J,I)+NLASTAT
             ELSE
                IB(J)=0
             END IF
10        CONTINUE
          DO 150 KMOL = 1,NSEC
              IF (IA.GE.ISTC(KMOL).AND.IA.LE.IENC(KMOL)) GOTO 160
150       CONTINUE
          GOTO 410
160       IF (MOL.NE.KMOL) THEN
              IF (NOJ(KMOL).NE.0) GOTO 410
              ISTJ(KMOL) = NOJOIN + 1
              MOL = KMOL
          END IF
          DO 170 K = 1,MAXB
              IF (IB(K).LE.IA) GOTO 170
              IF (IB(K).LT.ISTC(MOL).OR.IB(K).GT.IENC(MOL)) THEN
C                 WRITE(6,'(A)') ' Warning - cross link omitted'
              ELSE
                  NOJOIN = NOJOIN + 1
                  IF(NOJOIN.GT.MAXBND) GO TO 420
                  NOJ(MOL) = NOJ(MOL) + 1
                  IENJ(MOL) = NOJOIN
                  IANO(NOJOIN) = IA
                  IBNO(NOJOIN) = IB(K)
              END IF
170       CONTINUE
180   CONTINUE

      IER = 0
      RETURN

410   WRITE(6,411)
411   FORMAT(' Bonding data not in acceptable format')
      IER = 1
      RETURN
420   WRITE(6,421) MAXBND
421   FORMAT(' System too large:  number of bonds limited to ',I4)
      IER = 1
      RETURN
      END
