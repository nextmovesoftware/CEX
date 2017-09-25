      CHARACTER*80 FUNCTION NEWARG(ARG, K, IV, PREFIX)
C==================================================================
C   CREATE A FILE NAME FOR THE K-TH STRUCTURE.  IF AN EXTENSION WAS
C   SUPPLIED, USE IT.  OTHERWISE USE THE DEFAULT EXTENSION .pdb.
C   THIS RESULTS IN A FILE NAMED <NAME><K>.<EXT> OR <NAME><K>.pdb if
C   PREFIX is false, and <K><NAME>.<EXT> OR <K><NAME>.pdb if PREFIX 
C   is true. IV IS THE VERSION NUMBER (SIMULATES VMS VERSIONS ON UNIX).
C------------------------------------------------------------------
      CHARACTER*(*) ARG
      CHARACTER*6   NUMB
      CHARACTER*1   VERS
      LOGICAL       PREFIX

      L1 = INDEX(ARG,']')
      L2 = INDEX(ARG(L1 + 1:),'.')
      L3 = INDEX(ARG(L1 + 1:),' ')
      NDIG = MAX(1.,(ALOG10(FLOAT(K)) + 1))
      WRITE(NUMB,'(I6)') K
      IF (.NOT.PREFIX) THEN
         IF (L2.EQ.0) THEN
            NEWARG = (ARG(:(L1+L3)-1) // NUMB((6-NDIG) + 1:)) // '.pdb'
         ELSE
            NEWARG = (ARG(:(L1+L2)-1) // NUMB((6-NDIG) + 1:)) //
     .                ARG(L1+L2:(L1+L3)-1)
         END IF
      ELSE
         IF (L2.EQ.0) THEN
            NEWARG = (NUMB((6-NDIG) + 1:) // ARG(:(L1+L3)-1)) // '.pdb'
         ELSE
            NEWARG = (NUMB((6-NDIG) + 1:) // ARG(:(L1+L2)-1)) //
     .                ARG(L1+L2:(L1+L3)-1)
         END IF
      END IF
      IF (IV.GT.1) THEN
         WRITE(VERS,'(I1)') IV
         LENGTH = LENSTR(NEWARG)
         NEWARG = NEWARG(:LENGTH) // ',' // VERS
      END IF

      RETURN
      END
