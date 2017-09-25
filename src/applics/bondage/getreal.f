      SUBROUTINE GETREAL(STRING,R,OK)
C======================================================================
C  Convert character string STRING to real R.  Returns OK = .FALSE.
C  if string has characters other than digits or a single decimal point.
C  If no decimal point is found, converts integer to real.
C----------------------------------------------------------------------
      REAL             R
      CHARACTER*(*)    STRING
      CHARACTER*12     STRING2
      CHARACTER*11     DIGITS
      INTEGER          POINT
      LOGICAL          OK
      DATA             DIGITS/'0123456789.'/
      DATA             POINT/11/

      LENGTH = LENSTR(STRING)
      IF (LENGTH.EQ.0) THEN
         OK = .FALSE.
         RETURN
      END IF
C----------------------------------------------------------------------
C  Ensure that STRING contains only digits
C----------------------------------------------------------------------
      IF ( STRING(1:1).EQ.'-') THEN
         ISTART=2
      ELSE
         ISTART=1
      END IF
      NPOINTS=0
      DO 40 N=ISTART,LENGTH
         IPTR = INDEX( DIGITS,STRING(N:N) )
         IF (IPTR.EQ.POINT) NPOINTS = NPOINTS+1
         OK = IPTR.GT.0 .AND. NPOINTS.LT.2
         IF (.NOT.OK) RETURN
40    CONTINUE
C----------------------------------------------------------------------
C  Add decimal point if one isn't provided
C----------------------------------------------------------------------
      IF ( NPOINTS.EQ.0) STRING = STRING(:LENGTH) // '.'
C----------------------------------------------------------------------
C  Read STRING into REAL variable.
C----------------------------------------------------------------------
      STRING2 = STRING
      READ(STRING2,'(F12.6)') R
      RETURN
      END
