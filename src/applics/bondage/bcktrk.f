      SUBROUTINE BCKTRK(IMOL,CYCLIC,MAXVAL,IBOND,NBOND,DOTS,LUNOUT)
C=======================================================================
C This function sees whether the multiple bonds on the atoms in the set
C DOTS  can  be  consistently  connected  into  double bonds. If success 
C then the bonds formed by connecting the DOTS are made.
C-----------------------------------------------------------------------
      INCLUDE     'bondage.cmn'
      INTEGER     IBOND(MAXVAL,NAT),NBOND(NAT),DOTS(NAT)
      INTEGER     CHOICE(MAXBND),BNDSTK(MAXBND)
      INTEGER     ATOM1,ATOM2,ATOM,ATT,BOND,STKPTR,GETBND
      LOGICAL     CYCLIC,OK

      STKPTR =  0
C-----------------------------------------------------------------------
C Choose next candidate
C-----------------------------------------------------------------------

10    DO ATOM = ISTC(IMOL),IENC(IMOL)
         IF (DOTS(ATOM).NE.0) GO TO 16
      END DO
C-----------------------------------------------------------------------
C Found a match
C-----------------------------------------------------------------------
      GO TO 100
16    I=0
C-----------------------------------------------------------------------
C Now, choose a partner
C-----------------------------------------------------------------------
17    ATT=NXTATT(MAXVAL,IBOND,NBOND,ATOM,I)
      IF (ATT.EQ.-1) GO TO 50
      IF (DOTS(ATT).EQ.0) GO TO 17
C-----------------------------------------------------------------------
C Found a possibility; put it into a bond with atom
C-----------------------------------------------------------------------
      BOND=GETBND(IMOL,ATOM,ATT)
      STKPTR=STKPTR+1
      BNDSTK(STKPTR)=BOND
      IF (ATOM.NE.IANO(BOND)) I=-I
      CHOICE(STKPTR)=I
      DOTS(ATOM)=DOTS(ATOM)-1
      DOTS(ATT)=DOTS(ATT)-1
C-----------------------------------------------------------------------
C  Keep on trying
C-----------------------------------------------------------------------
      GO TO 10

C-----------------------------------------------------------------------
C  Backtrack to last choice point and try new way
C-----------------------------------------------------------------------
50    IF (STKPTR.EQ.0) GO TO 200
C-----------------------------------------------------------------------
C  Still are choice points to choose
C-----------------------------------------------------------------------
      BOND=BNDSTK(STKPTR)
      I=CHOICE(STKPTR)
      STKPTR=STKPTR-1
      ATOM1=IANO(BOND)
      ATOM2=IBNO(BOND)
      DOTS(ATOM1)=DOTS(ATOM1)+1
      DOTS(ATOM2)=DOTS(ATOM2)+1
      ATOM=ATOM1
      IF (I.LT.0) ATOM=ATOM2
      I=IABS(I)
      GO TO 17

C-----------------------------------------------------------------------
C  Success 
C-----------------------------------------------------------------------
100   CONTINUE
      DO J=1,STKPTR
         IAT = IANO(BNDSTK(J))
         JAT = IBNO(BNDSTK(J))
         OK = .TRUE.
C-----------------------------------------------------------------------
C If we're only setting double bonds for DOTS in cyclic system, check
C for conjugated ring nitrogen to avoid misassigning lactam C-N as
C imine C=N.
C-----------------------------------------------------------------------
         IF (CYCLIC) CALL CONJN(MAXVAL,IBOND,NBOND,BNDSTK,STKPTR,
     .    IAT,JAT,OK)
         IF (OK) CALL ADDBND(MAXVAL,IBOND,NBOND,IAT,JAT,'double',LUNOUT)
      END DO
200   CONTINUE
      RETURN
      END
