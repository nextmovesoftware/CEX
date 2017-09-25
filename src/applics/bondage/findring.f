      SUBROUTINE FINDRING(IST,IEND,IMOL,RING)
C=======================================================================
C  RING = .TRUE. if the bond defined by IST to IEND in molecule IMOL is
C  cyclic.
C-----------------------------------------------------------------------
      INCLUDE 'bondage.cmn'
      INTEGER FOUND(MAXBND),LOOK(MAXBND)
      LOGICAL RING
      SAVE    FOUND
      DATA    FOUND/MAXBND*0/

      DO 20 I=ISTJ(IMOL),IENJ(IMOL)
         IF((IST.EQ.IANO(I) .AND. IEND.EQ.IBNO(I)) .OR.
     .    (IEND.EQ.IANO(I) .AND. IST.EQ.IBNO(I))) THEN
               IBOND=0
               IF (IMOL.GT.1) THEN
                  DO J=1,IMOL-1
                     IBOND=NOJ(J)+IBOND
                  END DO
               END IF
               IBOND=IBOND+I
            IF(FOUND(IBOND).EQ.0) THEN
               GO TO 30
            ELSE IF(FOUND(IBOND).EQ.1) THEN
               RING=.FALSE.
               RETURN
            ELSE IF(FOUND(IBOND).EQ.2) THEN
               RING=.TRUE.
               RETURN
            END IF
         END IF
 20   CONTINUE

 30   ITHIS = 1
      NOTOL = 1
      LOOK(1) = IEND

 40   DO 60 K=ISTJ(IMOL),IENJ(IMOL)
         ICUR = LOOK(ITHIS)
         IBP1 = IANO(K)
         IBP2 = IBNO(K)
         IF (IBP1.EQ.ICUR) THEN
            IBPM = IBP2
         ELSE IF (IBP2.EQ.ICUR) THEN
            IBPM = IBP1
         ELSE
            GOTO 60
         END IF
C-----------------------------------------------------------------------
C Have we returned to the start of the bond? If so, we've got a ring.
C-----------------------------------------------------------------------
         IF (ITHIS.NE.1.AND.IBPM.EQ.IST) THEN
            FOUND(IBOND)=2
            RING=.TRUE.
            RETURN
         END IF
         IF (IBPM.NE.IST) THEN
            DO 50 J=1,NOTOL
               IF (J.EQ.ITHIS) GOTO 50
               IF (IBPM.EQ.LOOK(J)) GOTO 60
50          CONTINUE
            NOTOL = NOTOL + 1
            LOOK(NOTOL) = IBPM
         END IF
60    CONTINUE
      ITHIS = ITHIS + 1
      IF (ITHIS.LE.NOTOL) GOTO 40
      FOUND(IBOND)=1
      RING=.FALSE.
      RETURN
      END
