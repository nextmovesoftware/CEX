      SUBROUTINE GETINT(STRING,I,OK)
C======================================================================
C  Convert character string STRING to integer I.  Returns OK = .FALSE.
C  if string has characters other than digits.
C----------------------------------------------------------------------
      CHARACTER*(*)  STRING
      CHARACTER*80   FMT
      CHARACTER*10   DIGITS
      LOGICAL        OK
      DATA           DIGITS/'0123456789'/

C----------------------------------------------------------------------
C  Ensure that STRING contains only digits
C----------------------------------------------------------------------
      LENGTH = LENSTR(STRING)
      IF (LENGTH.EQ.0) THEN
         OK = .FALSE.
         RETURN
      END IF
      IF ( STRING(1:1).EQ.'-') THEN
         ISTART=2
      ELSE
         ISTART=1
      END IF
      DO 40 N=ISTART,LENGTH
         OK = INDEX( DIGITS,STRING(N:N) ).GT.0
         IF (.NOT.OK) RETURN
40    CONTINUE
C----------------------------------------------------------------------
C  Read STRING into INTEGER variable.
C----------------------------------------------------------------------
      CALL SQUEEZE(STRING,LENGTH,STRING,LENGTH)
      WRITE(FMT,'(A,I9,A)',ERR=99) '(I',LENGTH,')'
      LIN = LENSTR(FMT)
      CALL SQUEEZE(FMT,LIN,FMT,LOUT)
      READ(STRING(:LENGTH),FMT,ERR=99) I
      OK = .TRUE.
      RETURN

99    OK = .FALSE.
      RETURN
      END
