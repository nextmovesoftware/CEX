      SUBROUTINE GETRADII(ICHN,IMOL,OK)
C-----------------------------------------------------------------------
C Assign van der Waals and covalent bond radii for molecule IMOL. Default
C radii are assigned in block data routine RADIIDAT.f
C If ICHN > 0,  read user-defined radii from file.
C-----------------------------------------------------------------------
      INCLUDE     'bondage.cmn'
      PARAMETER   (MAXARGS=10)
      REAL         RAD1,RAD2
      INTEGER      ARGPTR(2,MAXARGS)
      CHARACTER*80 STRING
      CHARACTER*2  ATOM,ATYPE
      LOGICAL      FIRST,FOUND,OK
      SAVE         FIRST
      DATA FIRST /.TRUE./

      OK = .TRUE.
      IF (FIRST) THEN
         FIRST = .FALSE.
         DO I=1,MAXAT
            SYMNO(I) = 0
         END DO
      END IF

C-----------------------------------------------------------------------
C Read in user-defined symbols and radii if ICHN > 0
C    Free format: 
C     1st field = atomic symbol
C     2nd field = van der Waals radius
C     3rd field = covalent bond radius (optional)
C-----------------------------------------------------------------------
      IF (ICHN.GT.0) THEN
10       READ(ICHN,'(A)',END=90,ERR=990) STRING
         CALL PARSE(STRING,NARGS,ARGPTR,MAXARGS)
         IF (NARGS.GE.2) THEN
            IF (ARGPTR(2,1)-ARGPTR(1,1).GT.1) THEN
               GO TO 999
            ELSE
               ATOM = STRING(ARGPTR(1,1):ARGPTR(2,1))
               CALL GETREAL(STRING(ARGPTR(1,2):ARGPTR(2,2)),RAD1,OK)
               IF (.NOT.OK) GO TO 999
               IF (NARGS.EQ.3) THEN
                  CALL GETREAL(STRING(ARGPTR(1,3):ARGPTR(2,3)),RAD2,OK)
                  IF (.NOT.OK) GO TO 999
               END IF
               FOUND = .FALSE.
               I = 0
               DO WHILE (.NOT.FOUND .AND. I.LT.NTYPES)
                  I = I+1
                  FOUND = ATOM.EQ.SYM(I)
               END DO
               IF (FOUND) THEN
C-----------------------------------------------------------------------
C Matched default atomic symbol; replace default radii with 
C user-defined values
C-----------------------------------------------------------------------
                  VRAD(I) = RAD1
                  IF (NARGS.EQ.3) CRAD(I) = RAD2
                  WRITE(STRING,'(A)') ' Reassign radii for:'
               ELSE
                  NTYPES = NTYPES+1
                  IF (NTYPES.GT.MAXTYP) GO TO 1000
                  SYM(NTYPES)  = ATOM
                  VRAD(NTYPES) = RAD1
                  IF (NARGS.EQ.3) CRAD(NTYPES) = RAD2
                  WRITE(STRING,'(A)') ' Assign new atom type:'
               END IF
               LSTRING = LENSTR(STRING)
               IF (NARGS.EQ.3) THEN
                  WRITE(6,'(A,1X,A,2F5.2)') STRING(:LSTRING),ATOM,RAD1,
     .             RAD2
               ELSE
                  WRITE(6,'(A,1X,A,F5.2)') STRING(:LSTRING),ATOM,RAD1
               END IF
            END IF
         ELSE
            GO TO 999
         END IF
         GO TO 10
90       CONTINUE
      END IF
      IF (IMOL.EQ.0) RETURN

C-----------------------------------------------------------------------
C Assign atomic symbol number SYMNO for all atoms in molecule IMOL
C-----------------------------------------------------------------------
      IF (SYMNO(ISTC(IMOL)).GT.0) RETURN
      OK = .TRUE.
      DO J = ISTC(IMOL),IENC(IMOL)
         ATOM = ATYPE(ATNM(J))
         FOUND = .FALSE.
         N = 0
         DO WHILE (.NOT.FOUND .AND. N.LT.NTYPES)
            N = N+1
            FOUND =  ATOM.EQ.SYM(N)
         END DO
         IF (FOUND) THEN
            SYMNO(J) = N
         ELSE
            WRITE(6,'(2A)')' VDW and covalent bond radii unknown for: ',
     .       ATNM(J)
            OK = .FALSE.
         END IF
      END DO

      RETURN

990   WRITE(6,'(A)') ' Error reading *radii* file'
      OK = .FALSE.
      RETURN

999   LSTRING = LENSTR(STRING)
      WRITE(6,'(2A)')' Error reading *radii* file at ', STRING(:LSTRING)
      OK = .FALSE.
      RETURN

1000  WRITE(6,'(A)') ' (GETRADII) Exceeded max number of atom types'
      OK = .FALSE.
      RETURN

      END
