      SUBROUTINE QUICKSORT(N,DATA,NEXT)
C
      PARAMETER (MAXSTACK=100)
      REAL      DATA(N)       !DATA TO BE SORTED ON
      INTEGER   I             !GENERAL INDEX
      INTEGER   ID            !POINTER TO DATA
      INTEGER   J             !GENERAL INDEX
      INTEGER   JD            !POINTER TO DATA
      INTEGER   LV(MAXSTACK)  !LOWER VALUE STACK
      INTEGER   N             !NUMBER OF ITEMS IN DATA TO SORT
      INTEGER   NEXT(N)       !POINTER ARRAY WHICH RECIEVES ORDER
      INTEGER   P             !POINTER TO STACK
      INTEGER   UV(MAXSTACK)  !UPPER VALUE STACK

C  INITIALIZE POINTER ARRAY
      DO I=1,N
         NEXT(I)=I
      END DO
      LV(1)=1
      UV(1)=N
      P=1
      DO WHILE (P.GT.0)
        IF (LV(P).LT.UV(P)) THEN
          I=LV(P)
          J=UV(P)
          ID=NEXT(I)
          JD=NEXT(J)
          DO WHILE (I.LT.J)
            DO WHILE (DATA(ID).LT.DATA(JD))
              I=I+1
              ID=NEXT(I)
            END DO
            ID=NEXT(J)
            DO WHILE (J.GT.I.AND.DATA(ID).GE.DATA(JD))
              J=J-1
              ID=NEXT(J)
            END DO
            IF (I.LT.J) THEN
              ID=NEXT(J)
              NEXT(J)=NEXT(I)
              NEXT(I)=ID
            END IF
          END DO
          J=UV(P)
          ID=NEXT(J)
          NEXT(J)=NEXT(I)
          NEXT(I)=ID
          IF (P.LT.MAXSTACK) THEN
            IF ((I-LV(P)).GT.(UV(P)-I)) THEN
              UV(P+1)=UV(P)
              UV(P)=I-1
              LV(P+1)=I+1
            ELSE
              LV(P+1)=LV(P)
              LV(P)=I+1
              UV(P+1)=I-1
            END IF
            P=P+1
          ELSE
            LV(1)=1
            UV(1)=N
            P=1
          END IF
        ELSE
          P=P-1
        END IF
      END DO
      RETURN
      END
