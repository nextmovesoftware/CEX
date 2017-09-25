      SUBROUTINE CONJN(MAXVAL,IBOND,NBOND,BNDSTK,STKPTR,IAT,JAT,OK)
C-----------------------------------------------------------------------
C IAT is double bonded to JAT.  If either IAT or JAT are nitrogen, check
C whether IAT=JAT is conjugated by searching for *=*-IAT=JAT or 
C IAT=JAT-*=*. BNDSTK contains list of double bonds. Return OK if 
C conjugated.
C-----------------------------------------------------------------------
      INCLUDE    'bondage.cmn'
      INTEGER     IBOND(MAXVAL,NAT),NBOND(NAT),BNDSTK(MAXBND),STKPTR
      INTEGER     ATOM(3)
      CHARACTER*2 ATYPE
      LOGICAL     OK

      OK = ATYPE(ATNM(IAT)).NE.'N' .AND. ATYPE(ATNM(JAT)).NE.'N'
      IF (OK) RETURN
      ATOM(1) = IAT
      ATOM(2) = JAT
      ATOM(3) = IAT
      J = 0
      DO WHILE (.NOT.OK .AND. J.LT.2)
         J = J+1
         K = 0
         DO WHILE (.NOT.OK .AND. K.LT.NBOND(ATOM(J)))
            K = K+1
            KAT = IBOND(K,ATOM(J))
            IF (KAT.NE.ATOM(J+1)) THEN
               L = 0
               DO WHILE (.NOT.OK .AND. L.LT.STKPTR)
                  L = L+1
                  OK = IANO(BNDSTK(L)).EQ.KAT.OR.IBNO(BNDSTK(L)).EQ.KAT 
               END DO
            END IF
         END DO
      END DO
      RETURN
      END
