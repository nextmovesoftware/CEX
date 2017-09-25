      SUBROUTINE NEWVERS(IV,FILNAM,LFIL,FILTYP)
C----------------------------------------------------------------------
C  Create next highest version number file name for UNIX systems.
C  IV = current version number; FILNAM = filename; LFIL = length
C  of original filename, FILTYP = text description of file type.
C----------------------------------------------------------------------
      CHARACTER*(*) FILNAM,FILTYP
      CHARACTER*80  STRING
      CHARACTER*1   VERS
      
      IV = IV+1
      IF (IV.EQ.10) THEN
         LTYP = LENSTR(FILTYP)
         WRITE(STRING,'(4A)') ' Unable to open *', FILTYP(:LTYP) , 
     .    '* file: ', FILNAM(:LFIL) 
         LENGTH = LENSTR(STRING)
         WRITE(6,'(/,A)') STRING(:LENGTH)
         STOP
      END IF
      WRITE(VERS,'(I1)') IV
      FILNAM = FILNAM(:LFIL) // ',' // VERS
      RETURN
      END
