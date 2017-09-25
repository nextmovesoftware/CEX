      CHARACTER*2 FUNCTION ATYPE(ATNAME)
C===================================================================
C Left justify atom name, and return atomic symbol as atom type.
C If the first character of the atom name is a number (e.g. 1H2), 
C get the atom type from the next character. Returns '?' if atom 
C type is not found.  Default atomic symbols are assigned in block
C data routine RADIIDAT.f
C-------------------------------------------------------------------
      INCLUDE      'bondage.cmn'
      CHARACTER*(*) ATNAME
      LOGICAL       DIGIT,FOUND

      IF (ATNAME(:1).EQ.' ') THEN
         CALL GETINT(ATNAME(2:2),I,DIGIT)
         IF (DIGIT) THEN
            ATYPE = ATNAME(3:4)
         ELSE
            ATYPE = ATNAME(2:3)
         END IF
      ELSE
         CALL GETINT(ATNAME(:1),I,DIGIT)
         IF (DIGIT) THEN
            ATYPE = ATNAME(2:3)
         ELSE
            ATYPE = ATNAME(:2)
         END IF
      END IF

C-------------------------------------------------------------------
C  Two-character atomic symbols
C-------------------------------------------------------------------
      FOUND=.FALSE.
      I = 0
      DO WHILE(.NOT.FOUND .AND. I.LT.NTYPES)
         I = I+1
         FOUND = ATYPE(:2).EQ.SYM(I)(:2)
      END DO

C-------------------------------------------------------------------
C  One-character atomic symbols
C-------------------------------------------------------------------
      IF (.NOT.FOUND) THEN
         I = 0
         DO WHILE(.NOT.FOUND .AND. I.LT.NTYPES)
            I = I+1
            FOUND = ATYPE(:1).EQ.SYM(I)
         END DO
         IF (FOUND) ATYPE = ATYPE(:1)
      END IF

      IF (.NOT.FOUND) ATYPE='?'

      RETURN
      END
