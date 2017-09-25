      SUBROUTINE ADDBND(MAXVAL,IBOND,NBOND,IAT,JAT,TYPE,LUNOUT)
C=======================================================================
C Convert bond between atoms IAT and JAT to double or triple bond.
C-----------------------------------------------------------------------
      INCLUDE      'bondage.cmn'
      INTEGER       IBOND(MAXVAL,NAT),NBOND(NAT)
      CHARACTER*(*) TYPE
      CHARACTER*1   CHIDI,CHIDJ

      IF (TYPE.EQ.'double') THEN
         INC = 2
      ELSE IF (TYPE.EQ.'triple') THEN
         INC = 3
      END IF

C-----------------------------------------------------------------------
C Check IAT-JAT bond multiplicity
C-----------------------------------------------------------------------
      MUL  = 0
      DO I = 1,NBOND(IAT)
         IF (JAT.EQ.IBOND(I,IAT)) MUL=MUL+1
      END DO
      IF (MUL.GE.INC) RETURN
      INC = INC - MUL

C-----------------------------------------------------------------------
C Check IAT and JAT multiplicities - note this check will prevent
C allenes, etc. (X-C=C=C-Y), but these are rare and this check is
C necessary to prevent setting ALL bounds double in some aromatic
C systems.
C-----------------------------------------------------------------------
cdcs  print *,' Call in addbnd : iat = ',iat
      CALL BNDINF(MAXVAL,IBOND,NBOND,IAT,NB,MUL)
      IF (MUL.GT.1) RETURN
cdcs  print *,' Call in addbnd : jat = ',jat
      CALL BNDINF(MAXVAL,IBOND,NBOND,JAT,NB,MUL)
      IF (MUL.GT.1) RETURN

      CALL VERIFYATM(IAT,KMOL,IRES,CHIDI)
      CALL VERIFYATM(JAT,KMOL,JRES,CHIDJ)
      L = LENSTR(TYPE)
c     WRITE(LUNOUT,'(A,2(I3,I5,A1,A4),2A)') ' Converted bond: ',
c    . KMOL,IRES,CHIDI,ATNM(IAT),KMOL,JRES,CHIDJ,ATNM(JAT),
c    . ' to ', TYPE(:L)
      IF (KMOL.LT.NSEC) THEN
         DO KBND = IENJ(NSEC),ISTJ(KMOL+1),-1
            IANO(KBND+INC) = IANO(KBND)
            IBNO(KBND+INC) = IBNO(KBND)
         END DO
         DO KSEC = KMOL+1,NSEC
            ISTJ(KSEC) = ISTJ(KSEC) + INC
            IENJ(KSEC) = IENJ(KSEC) + INC
         END DO
      END IF
      NOJ(KMOL) = NOJ(KMOL) + 1
      IENJ(KMOL) = IENJ(KMOL) + 1
      IANO(IENJ(KMOL)) = IAT
      IBNO(IENJ(KMOL)) = JAT
      NBOND(IAT) = NBOND(IAT)+1
      NBOND(JAT) = NBOND(JAT)+1
      IF (NBOND(IAT).LE.MAXVAL) IBOND(NBOND(IAT),IAT) = JAT
      IF (NBOND(JAT).LE.MAXVAL) IBOND(NBOND(JAT),JAT) = IAT
      IF (INC.EQ.2) THEN
         NOJ(KMOL) = NOJ(KMOL) + 1
         IENJ(KMOL) = IENJ(KMOL) + 1
         IANO(IENJ(KMOL)) = IAT
         IBNO(IENJ(KMOL)) = JAT
         NBOND(IAT) = NBOND(IAT)+1
         NBOND(JAT) = NBOND(JAT)+1
         IF (NBOND(IAT).LE.MAXVAL) IBOND(NBOND(IAT),IAT) = JAT
         IF (NBOND(JAT).LE.MAXVAL) IBOND(NBOND(JAT),JAT) = IAT
      END IF
      
      RETURN
      END
