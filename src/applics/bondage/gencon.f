      SUBROUTINE GENCON(IMOL,MAXB,JBND,IER)
C=======================================================================
C Generate connectivity for molecule IMOL based on covalent bond radii.
C-----------------------------------------------------------------------
      INCLUDE    'bondage.cmn'
      REAL        Z(MAXAT),RMAX,RTHIS,RLIM,XTHIS,YTHIS,ZTHIS,DX,DY,DZ,D
      REAL        R
      INTEGER     NEXT(MAXAT),NBND(MAXAT),JBND(MAXB,MAXAT)
      CHARACTER*1 CHID
cdcs  print *,' In gencon : NIN[1-4] = ',(nin(idcs),idcs=1,4)
C-----------------------------------------------------------------------
C Initialize bond arrays
C-----------------------------------------------------------------------
      DO K=1,NAT
         NBND(K) = 0
         DO J=1,MAXB
            JBND(J,K) = 0
         END DO
      END DO

C-----------------------------------------------------------------------
C Sort by Z-coordinate
C-----------------------------------------------------------------------
      K = 0
      DO J = ISTC(IMOL),IENC(IMOL)
         K = K+1
         Z(K) = CR(3,J)
      END DO
      CALL QUICKSORT(K,Z,NEXT)

C-----------------------------------------------------------------------
C Find max covalent bond radius
C-----------------------------------------------------------------------
      RMAX = 0.0
      DO N = 1,NTYPES
         RMAX = MAX(RMAX,CRAD(N))
      END DO

C-----------------------------------------------------------------------
C Loop to find bonded atoms: for each atom check all those within
C Z +/- CRAD+RMAX
C-----------------------------------------------------------------------
      K = 0
      INDX = ISTC(IMOL)-1
      DO 250 J = ISTC(IMOL),IENC(IMOL)
          K = K+1
          KAT = NEXT(K)
          RTHIS = CRAD(SYMNO(KAT+INDX))
          RLIM = RMAX + RTHIS
          XTHIS = CR(1,KAT+INDX)
          YTHIS = CR(2,KAT+INDX)
          ZTHIS = CR(3,KAT+INDX)
          K1 = K
260       K1 = K1 - 1
cdcs      print *,'@260',k,k1,xthis,ythis,zthis,rthis,rlim,kat
          IF (K1.LT.1) GOTO 265
          KAT2 = NEXT(K1)
          IF (KAT2.LT.KAT) GOTO 260
          DZ = ZTHIS - Z(KAT2)
          IF (ABS(DZ).GT.RLIM) GOTO 265

          DX = XTHIS - CR(1,KAT2+INDX)
          DY = YTHIS - CR(2,KAT2+INDX)
          D = SQRT(DX*DX + DY*DY + DZ*DZ)
          R = RTHIS + CRAD(SYMNO(KAT2+INDX))
cdcs      print *,' gencon@1stif',j,k,kat,kat2,rthis,indx,dx,dy
          IF (D.LE.R) THEN
              NBND(KAT) = NBND(KAT) + 1
              IF (NBND(KAT).GT.MAXB) THEN
                  KER = KAT
                  GOTO 994
              END IF
              NBND(KAT2) = NBND(KAT2) + 1
              IF (NBND(KAT2).GT.MAXB) THEN
                  KER = KAT2
                  GOTO 994
              END IF
              JBND(NBND(KAT),KAT) = KAT2
              JBND(NBND(KAT2),KAT2) = KAT
cdcs          print *,'Added bond: 1st',
cdcs .           kat,kat2,nbnd(kat),nbnd(kat2),
cdcs .           jbnd(nbnd(kat),kat),jbnd(nbnd(kat2),kat2)
          END IF
          GOTO 260

265       K1 = K
270       K1 = K1 + 1
cdcs      print *,'@270',k,k1,imol,nin(imol),next(k1)
          IF (K1.GT.NIN(IMOL)) GOTO 250
          KAT2 = NEXT(K1)
          IF (KAT2.LT.KAT) GOTO 270
          DZ = ZTHIS - Z(KAT2)
          IF (ABS(DZ).GT.RLIM) GOTO 250

          DX = XTHIS - CR(1,KAT2+INDX)
          DY = YTHIS - CR(2,KAT2+INDX)
          D = SQRT(DX*DX + DY*DY + DZ*DZ)
          R = RTHIS + CRAD(SYMNO(KAT2+INDX))
cdcs      print *,' gencon@2ndif',j,k,kat,kat2,rthis,indx,dx,dy
          IF (D.LE.R) THEN
              NBND(KAT) = NBND(KAT) + 1
              IF (NBND(KAT).GT.MAXB) THEN
                  KER = KAT
                  GOTO 994
              END IF
              NBND(KAT2) = NBND(KAT2) + 1
              IF (NBND(KAT2).GT.MAXB) THEN
                  KER = KAT2
                  GOTO 994
              END IF
              JBND(NBND(KAT),KAT) = KAT2
              JBND(NBND(KAT2),KAT2) = KAT
cdcs          print *,'Added bond: 2nd',
cdcs .           kat,kat2,nbnd(kat),nbnd(kat2),
cdcs .           jbnd(nbnd(kat),kat),jbnd(nbnd(kat2),kat2)
          END IF
          GOTO 270
250   CONTINUE

      RETURN

994   CALL VERIFYATM(KER,KMOL,KRES,CHID)
      WRITE (6,'(A,I3,I5,A1,A4)') ' Error - too many bonds for: ',
     . KMOL,KRES,CHID,ATNM(KER)
      DO 400 K=1,NBND(KER) - 1
         CALL VERIFYATM(JBND(K,KER),KMOL,KRES,CHID)
         WRITE(6,'(10X,A,I3,I5,A1,A4)') 
     .   ' connected to: ',KMOL,KRES,CHID,ATNM(JBND(K,KER))
400   CONTINUE
      IER = 1
      RETURN

      END
