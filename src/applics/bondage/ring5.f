      SUBROUTINE RING5(IMOL,MAX,IBOND,NBOND,GEOM)
C=======================================================================
C Assign geometry of atoms in 5-membered rings. First, search
C for 5-membered rings, then test each bond for planarity.
C-----------------------------------------------------------------------
      INCLUDE    'bondage.cmn'
      PARAMETER   (MAXVAL=6)
      REAL        IAIB,IBIC,ICID,IDIE,IEIA
      INTEGER     IBOND(MAX,NAT),NBOND(NAT),GEOM(NAT)
      INTEGER     ICSUB(MAXVAL**2),IESUB(MAXVAL**2),ICSUBX(MAXVAL**2)
      INTEGER     IESUBX(MAXVAL**2),IASUB(MAXVAL),IBSUB(MAXVAL)
      LOGICAL     FOUND

      DO KBND = ISTJ(IMOL),IENJ(IMOL)
         IA = IANO(KBND)
         IB = IBNO(KBND)
         IF (NBOND(IA).LT.2 .OR. NBOND(IB).LT.2) GO TO 99
         CALL FINDRING(IA,IB,IMOL,FOUND)
         IF (FOUND) THEN
C-----------------------------------------------------------------------
C Find all other cyclic bonds IASUB attached to IA.
C-----------------------------------------------------------------------
            NIASUB = 0
            DO I = 1,NBOND(IA)
               J = IBOND(I,IA)
               IF (J.NE.IB.AND.NBOND(J).GT.1) THEN
                  CALL FINDRING(IA,J,IMOL,FOUND)
                  IF (FOUND) THEN
                     NIASUB = NIASUB+1
                     IASUB(NIASUB) = J
                  END IF
               END IF
            END DO
            IF (NIASUB.EQ.0) GO TO 99
C-----------------------------------------------------------------------
C Find all other cyclic bonds IESUB attached to atoms in IASUB. IESUBX
C identifies which atom in IASUB is connected to IESUB
C-----------------------------------------------------------------------
            DO K = 1,NIASUB
               IE = IASUB(K)
               NIESUB = 0
               DO I = 1,NBOND(IE)
                  J = IBOND(I,IE)
                  IF (J.NE.IA.AND.NBOND(J).GT.1) THEN
                     CALL FINDRING(IE,J,IMOL,FOUND)
                     IF (FOUND) THEN
                        NIESUB = NIESUB+1
                        IESUB(NIESUB) = J
                        IESUBX(NIESUB) = IE
                     END IF
                  END IF
               END DO
            END DO
            IF (NIESUB.EQ.0) GO TO 99
C-----------------------------------------------------------------------
C Find all other cyclic bonds IBSUB attached to IB.
C-----------------------------------------------------------------------
            NIBSUB = 0
            DO I = 1,NBOND(IB)
               J = IBOND(I,IB)
               IF (J.NE.IA.AND.NBOND(J).GT.1) THEN
                  CALL FINDRING(IB,J,IMOL,FOUND)
                  IF (FOUND) THEN
                     NIBSUB = NIBSUB+1
                     IBSUB(NIBSUB) = J
                  END IF
               END IF
            END DO
            IF (NIBSUB.EQ.0) GO TO 99
C-----------------------------------------------------------------------
C Find all other cyclic bonds ICSUB attached to atoms in IBSUB. ICSUBX
C identifies which atom in IBSUB is connected to ICSUB
C-----------------------------------------------------------------------
            DO K = 1,NIBSUB
               IC = IBSUB(K)
               NICSUB = 0
               DO I = 1,NBOND(IC)
                  J = IBOND(I,IC)
                  IF (J.NE.IB.AND.NBOND(J).GT.1) THEN
                     CALL FINDRING(IC,J,IMOL,FOUND)
                     IF (FOUND) THEN
                        NICSUB = NICSUB+1
                        ICSUB(NICSUB) = J
                        ICSUBX(NICSUB) = IC
                     END IF
                  END IF
               END DO
            END DO
            IF (NICSUB.EQ.0) GO TO 99
C-----------------------------------------------------------------------
C Now, search for common atoms in ICSUB and IESUB lists
C-----------------------------------------------------------------------
            FOUND = .FALSE.
            I = 0
            DO WHILE (.NOT.FOUND .AND. I.LT.NICSUB)
               I = I+1
               ID = ICSUB(I)
               J = 0
               DO WHILE (.NOT.FOUND .AND. J.LT.NIESUB)
                  J = J+1
                  FOUND = IESUB(J).EQ.ID
               END DO
            END DO
            IF (FOUND) THEN
               IC = ICSUBX(I)
               IE = IESUBX(J)
C-----------------------------------------------------------------------
C Finally found a 5-membered ring: IA-IB-IC-ID-IE
C Test planarity by measuring torsion angles
C-----------------------------------------------------------------------
               CALL GETTOR(IE,IA,IB,IC,IAIB)
               IF (IAIB.GT.180.) IAIB = 360.-IAIB
               CALL GETTOR(IA,IB,IC,ID,IBIC)
               IF (IBIC.GT.180.) IBIC = 360.-IBIC
               CALL GETTOR(IB,IC,ID,IE,ICID)
               IF (ICID.GT.180.) ICID = 360.-ICID
               CALL GETTOR(IC,ID,IE,IA,IDIE)
               IF (IDIE.GT.180.) IDIE = 360.-IDIE
               CALL GETTOR(ID,IE,IA,IB,IEIA)
               IF (IEIA.GT.180.) IEIA = 360.-IEIA

c              write(6,'(a,4I3,F7.1)') '(ring5) ',IE,IA,IB,IC,IAIB 
c              write(6,'(a,4I3,F7.1)') '(ring5) ',IA,IB,IC,ID,IBIC
c              write(6,'(a,4I3,F7.1)') '(ring5) ',IB,IC,ID,IE,ICID
c              write(6,'(a,4I3,F7.1)') '(ring5) ',IC,ID,IE,IA,IDIE
c              write(6,'(a,4I3,F7.1)') '(ring5) ',ID,IE,IA,IB,IEIA
c              write(6,'(1x)')

               IF (IEIA + IAIB .LT. 11.) GEOM(IA) = 3
               IF (IAIB + IBIC .LT. 11.) GEOM(IB) = 3
               IF (IBIC + ICID .LT. 11.) GEOM(IC) = 3
               IF (ICID + IDIE .LT. 11.) GEOM(ID) = 3
               IF (IDIE + IEIA .LT. 11.) GEOM(IE) = 3
            END IF
         END IF
99       CONTINUE
      END DO
      RETURN
      END
