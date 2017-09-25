      SUBROUTINE GENBOND(IMOL,DODOTS,CYCLIC,MAXVAL,IBOND,NBOND,DOTS,
     . LUNOUT)
C=======================================================================
C Generate double bonds from input connectivity and coordinates.
C If CYCLIC, then only check rings.  If DODOTS, only set DOTS - not
C bond order.
C Modified from Eric Swanson's "join -f23" feature in PSSHOW.
C-----------------------------------------------------------------------
      INCLUDE   'bondage.cmn'
      INTEGER    GEOM(MAXAT),IBOND(MAXVAL,NAT),NBOND(NAT),DOTS(NAT)
C.. Maximum bond lengths
      REAL       BLC2C2, BLC1C1, BLC2N2, BLC1N1, BLC2O2, BLN2N2, BNDLEN
      REAL       BLC2S2, BLN2O2, BLS2O2
      PARAMETER (BLC2C2=1.43, BLC1C1=1.25, BLC2N2=1.41, BLC1N1=1.22)
      PARAMETER (BLC2O2=1.31, BLN2N2=1.35, BLC2S2=1.73, BLN2O2=1.25)
      PARAMETER (BLS2O2=1.53)
      CHARACTER*2 ATYPE,ATIA,ATIB
      LOGICAL     CYCLIC,DODOTS,RING

C-----------------------------------------------------------------------
C Make bond list
C-----------------------------------------------------------------------
      CALL GETBNM(IMOL,MAXVAL,IBOND,NBOND)

C-----------------------------------------------------------------------
C Determine geometry
C-----------------------------------------------------------------------
      CALL GETGEO(IMOL,MAXVAL,GEOM,IBOND,NBOND)

C-----------------------------------------------------------------------
C Increase multiplicity of those bonds connecting atoms of apparent
C sp and sp2 hybridization, checking bondlengths in the process.
C Only C, N,  O, S are considered.  Three passes through each molecule:
C pass 1 finds carbonyls, pass 2 everything except planar sp2 N,
C pass 3 planar sp2 N.
C-----------------------------------------------------------------------
cdcs  print *,' In genbond : imol = ',imol
      DO 140 IPASS = 1,3
         DO 120 KBND = ISTJ(IMOL), IENJ(IMOL)
cdcs        print *,' In genbond : istj = ',istj(imol),ienj(imol)
            IA = IANO(KBND)
            IB = IBNO(KBND)
cdcs        print *,' In genbond : ia,ib = ',ia,ib
            ATIA = ATYPE(ATNM(IA))
            ATIB = ATYPE(ATNM(IB))
            IF (CYCLIC) THEN
               CALL FINDRING(IA,IB,IMOL,RING)
               IF (.NOT.RING) GO TO 120
            END IF
            IF (IPASS.EQ.1.AND.ATIA.NE.'O'.AND.ATIB.NE.'O') GO TO 120
            CALL BNDINF(MAXVAL,IBOND,NBOND,IA,NBA,MULA)
            IF (.NOT.DODOTS .AND. (NBA.GE.4 .OR. MULA.GT.1)) GO TO 120
            IF (IPASS.LE.2 .AND. ATIA.EQ.'N'.AND.NBA.EQ.3) GO TO 120
            CALL BNDINF(MAXVAL,IBOND,NBOND,IB,NBB,MULB)
            IF (.NOT.DODOTS .AND. (NBB.GE.4 .OR. MULB.GT.1)) GO TO 120
            IF (IPASS.LE.2 .AND. ATIB.EQ.'N'.AND.NBB.EQ.3) GO TO 120
            IF (IPASS.EQ.3 .AND. .NOT.
     .       ((ATIA.EQ.'N'.AND.NBA.EQ.3).OR.(ATIB.EQ.'N'.AND.NBB.EQ.3)))
     .       GO TO 120
            CALL DISTANCE(IA,IB,BNDLEN)
            IF (ATIA.EQ.'C'.AND.ATIB.EQ.'C') THEN
               IF((GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.3) .OR.
     .            (GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1) .OR.
     .            (GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3))  THEN
                  IF(BNDLEN .LT. BLC2C2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               ELSE IF(GEOM(IA).EQ.2 .AND. GEOM(IB).EQ.2)  THEN
                  IF(BNDLEN .LT. BLC1C1) 
     .             CALL ADDBND(MAXVAL,IBOND,NBOND,IA,IB,'triple',LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'C' .AND.  ATIB.EQ.'N')  THEN
               IF((GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.3) .OR.
     .            (GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1) .OR.
     .            (GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3))  THEN
                  IF(NBB .LE. 2 .AND. MULB .EQ. 1)  THEN
                     IF(BNDLEN .LT. BLC2N2) CALL SETDOUBLE(DODOTS,
     .                MAXVAL,IBOND,NBOND,DOTS,IA,IB,LUNOUT)
                  END IF
               ELSE IF(GEOM(IA).EQ.2 .AND. GEOM(IB).EQ.1)  THEN
                  IF(BNDLEN .LT. BLC1N1) 
     .             CALL ADDBND(MAXVAL,IBOND,NBOND,IA,IB,'triple',LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'N' .AND. ATIB.EQ.'C')  THEN
               IF((GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.3) .OR.
     .            (GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1) .OR.
     .            (GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3))  THEN
                  IF(NBA .LE. 2 .AND. MULA .EQ. 1)  THEN
                     IF(BNDLEN .LT. BLC2N2) CALL SETDOUBLE(DODOTS,
     .                MAXVAL,IBOND,NBOND,DOTS,IA,IB,LUNOUT)
                  END IF
               ELSE IF(GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.2)  THEN
                  IF(BNDLEN .LT. BLC1N1) 
     .             CALL ADDBND(MAXVAL,IBOND,NBOND,IA,IB,'triple',LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'N' .AND.  ATIB.EQ.'N')  THEN
               IF((GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.3) .OR.
     .            (GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1) .OR.
     .            (GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3))  THEN
                  IF(BNDLEN .LT. BLN2N2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'C' .AND. ATIB.EQ.'O')  THEN
               IF(GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1)  THEN
                  IF(BNDLEN .LT. BLC2O2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'O' .AND. ATIB.EQ.'C')  THEN
               IF(GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3)  THEN
                  IF(BNDLEN .LT. BLC2O2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'C' .AND. ATIB.EQ.'S')  THEN
               IF(GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1)  THEN
                  IF(BNDLEN .LT. BLC2S2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'S' .AND. ATIB.EQ.'C')  THEN
               IF(GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3)  THEN
                  IF(BNDLEN .LT. BLC2S2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'N' .AND. ATIB.EQ.'O')  THEN
               IF(GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1)  THEN
                  IF(NBA .LE. 2 .AND. MULA .EQ. 1)  THEN
                     IF(BNDLEN .LT. BLN2O2) CALL SETDOUBLE(DODOTS,
     .                MAXVAL,IBOND,NBOND,DOTS,IA,IB,LUNOUT)
                  END IF
               END IF
            ELSE IF (ATIA.EQ.'O' .AND. ATIB.EQ.'N')  THEN
               IF(GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3)  THEN
                  IF(NBB .LE. 2 .AND. MULB .EQ. 1)  THEN
                     IF(BNDLEN .LT. BLN2O2) CALL SETDOUBLE(DODOTS,
     .                MAXVAL,IBOND,NBOND,DOTS,IA,IB,LUNOUT)
                  END IF
               END IF
            ELSE IF (ATIA.EQ.'S' .AND. ATIB.EQ.'O')  THEN
               IF(GEOM(IA).EQ.3 .AND. GEOM(IB).EQ.1)  THEN
                  IF(BNDLEN .LT. BLS2O2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               END IF
            ELSE IF (ATIA.EQ.'O' .AND. ATIB.EQ.'S')  THEN
               IF(GEOM(IA).EQ.1 .AND. GEOM(IB).EQ.3)  THEN
                  IF(BNDLEN .LT. BLS2O2) CALL SETDOUBLE(DODOTS,MAXVAL,
     .             IBOND,NBOND,DOTS,IA,IB,LUNOUT)
               END IF
            END IF
120      CONTINUE
140   CONTINUE

      RETURN
      END
