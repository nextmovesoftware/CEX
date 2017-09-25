      SUBROUTINE VERIFYATM(N,NMOL,NRES,CHID)
C=======================================================================
C Verify assignment - determine molecule, residue number, and chain
C identifier from atom number.
C-----------------------------------------------------------------------
      INCLUDE    'bondage.cmn'
      CHARACTER*1 CHID

      DO 10 K=1,NSEC
         IF (N.GE.ISTC(K) .AND. N.LE.IENC(K)) THEN
            NMOL=K
            GO TO 20
         END IF
10    CONTINUE
20    DO 30 K=1,NSEG
         IF (N.GE.ISTS(K) .AND. N.LE.IENS(K)) THEN
            NRES=SECSQ(K)
            CHID=CHAINID(K)
            RETURN
         END IF
30    CONTINUE
      RETURN

      END
