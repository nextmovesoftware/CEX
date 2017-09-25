      FUNCTION COL13(ATOM)
C======================================================================
C Check if ATOM needs to start in column 13 of a PDB format file.
C We check for two letter atomic symbols, for 4-character atom names,
C and for atom names which begin with a number (e.g. 1H2).  
C----------------------------------------------------------------------
      CHARACTER*(*) ATOM
      CHARACTER*2   ATYPE
      LOGICAL       COL13

      CALL GETINT(ATOM(:1),I,COL13)
      COL13 = COL13 .OR. LENSTR(ATYPE(ATOM)).EQ.2 .OR. LENSTR(ATOM).EQ.4
      RETURN
      END
