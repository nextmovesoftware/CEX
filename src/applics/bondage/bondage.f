C======================================================================
C                          BONDAGE
C                       A bondage program
C
C        Authors:  J.M. Blaney,  J. Scott Dixon, E. Swanson
C                  D. Weininger, D. Spellmeyer
C----------------------------------------------------------------------
C  Converted receive/send to next/append for 1.22 13 Jul 1998, DW.
C----------------------------------------------------------------------

      PROGRAM         BONDAGE

      INTEGER*4       NULL
      PARAMETER      (NULL = 0)
      INTEGER*4      MAXB
      PARAMETER      (MAXB = 6)
      INCLUDE        'bondage.cmn'
      INCLUDE        '../../../include/cx_f.inc'
      INCLUDE        '../../../include/cx_f_molecule.inc'
      LOGICAL        OK
      INTEGER*4      mol, natoms, lens
      INTEGER*4      CEXMOL
      INTEGER*4      OUTS
      INTEGER*4      IOK

      CHARACTER*(80) PDBIN
      CHARACTER*(1024) BUF

      INTEGER*4      I, IER, IMOL, NOJO, LAST
      INTEGER        IBOND(MAXB,MAXAT), IB(MAXB)

C----------------------------------------------------------------------
C  Initialize some parameters
C----------------------------------------------------------------------

      LUNOUT = 6
      DO K=1,MAXSEC
         NIN(K)  = 0
         ISTJ(K) = 0
         IENJ(K) = 0
         NOJ(K)  = 0
      ENDDO

C----------------------------------------------------------------------
C  Get molecule from CEX stream.
C----------------------------------------------------------------------
      MOL = CEXMOL()
      IF (MOL.EQ.NULL)  STOP
      OUTS = CX_F_CREATE_IOSTREAM('-', CX_IO_WRITE)
      IF (NULL.EQ.OUTS) STOP

C----------------------------------------------------------------------
C  Flag to determine whether to automatically assign double bonds
C----------------------------------------------------------------------

      AUTOBND = .TRUE.

C----------------------------------------------------------------------
C  Do it
C----------------------------------------------------------------------
      CALL GETRADII(0,NSEC,OK)
      IF (.NOT.OK) STOP

      CALL GENCON(NSEC, MAXB, IBOND, IER)

C----------------------------------------------------------------------
C  Make the assumption that there is only one molecule on the
C  Stream at this point.
C----------------------------------------------------------------------
      IMOL = 1
      NMOL  = 0
      NOJO = 0
      LAST = 0

      CALL MAKEBND(NAT, IER, MAXB, IBOND, IB, NMOL, NOJO, LAST)

      IF (AUTOBND) THEN
         CALL AUTOBOND(IMOL,LUNOUT)
         NOJOIN = IENJ(IMOL)
      END IF

      IF(IER.NE.0) STOP

      IF (0.LT.NOJ(1)) ISTJ(1) = 1

      LENS = CX_F_STRINGVALUE(MOL, BUF)

      CALL NEWBONDS(MOL)

      LENS = CX_F_STRINGVALUE(MOL, BUF)
      IOK  = CX_F_APPEND(OUTS, MOL)
      STOP
      END


      INTEGER*4 FUNCTION CEXMOL()
C======================================================================
C  CEXMOL -- get molecule object from CEX stream, interpret it
C
C  Returns molecule handle if successful, NULL (0) if not.
C  In any case, CEX stream might be at EOF, test with CX_F_CEX_EOF().
C  If successful, bondage.cmn COMMON blocks are filled.
C  This ignores extant bonds (and parts), puts everything into one
C  molecule, residue and chain.
C----------------------------------------------------------------------
      INTEGER*4    NULL
      PARAMETER  ( NULL = 0 )
      INCLUDE     'bondage.cmn'
      INCLUDE     '../../../include/cx_f.inc'
      INCLUDE     '../../../include/cx_f_molecule.inc'
      LOGICAL        OK
      INTEGER*4      ATOMS, ATOM, AN, TUPLES, TUPLE, XYZLEN, LENS, RV
      CHARACTER*(80) XYZNAM, COORD
      INTEGER*4      INS
      INTEGER*4      IOK
      SAVE           INS
      LOGICAL        PASS0
      SAVE           PASS0
      DATA PASS0 / .TRUE. /
C----------------------------------------------------------------------
C  Initialize return value to NULL for early exits.
C----------------------------------------------------------------------
      CEXMOL = NULL
C----------------------------------------------------------------------
C  Initialize CEX molecule package once.
C----------------------------------------------------------------------
      IF (PASS0) THEN
         IOK = CX_F_MOLECULE_PKG()
         IF (NULL.EQ.IOK) RETURN
         INS = CX_F_CREATE_IOSTREAM('-', CX_IO_READ)
         IF (NULL.EQ.INS) RETURN
         PASS0 = .FALSE.
      ENDIF
C----------------------------------------------------------------------
C  Get molecule object from CEX stream.
C----------------------------------------------------------------------
      DO WHILE (NULL.EQ.CEXMOL)
         CEXMOL = CX_F_NEXT(INS)
         IF (CX_OB_MOLECULE .NE. CX_F_TYPE(CEXMOL)) CEXMOL = NULL
      END DO
C----------------------------------------------------------------------
C  Make sure that there is really a molecule
C----------------------------------------------------------------------
      IF(CEXMOL.EQ.NULL) THEN
         PRINT *, 'Molecule Not Found'
         RETURN
      ENDIF
C----------------------------------------------------------------------
C  Require a coordinates property.
C----------------------------------------------------------------------
      TUPLES = CX_F_PREFIX2ATUPLES(CEXMOL, "coordinates")
      IF (NULL.EQ.TUPLES) THEN
         PRINT *, 'Coordinates not found in molecule'
         RETURN
      ENDIF
      TUPLE  = CX_F_NEXT(TUPLES)
      XYZLEN = CX_F_ATOMTUPLE_NAME(TUPLE, XYZNAM)
      CALL CX_F_DESTROY(TUPLES)
C----------------------------------------------------------------------
C  NSEC (number of disconnected components) is artifically set to 1.
C  THIS IS A CRITICAL COMPONENT TO FIX LATER!
C  WILL CHAINS HELP? DCS 10/9/95
C----------------------------------------------------------------------

      NSEC = 1

C----------------------------------------------------------------------
C  Initialize components.
C----------------------------------------------------------------------
      IENC(1) = 0
      IENJ(1) = 0
      ISTC(1) = 0
      ISTJ(1) = 0
      NIN(1)  = 0
      NOJ(1)  = 0
C----------------------------------------------------------------------
C  Loop over atoms (ignore bonds).
C  Should this accomodate bonds that are already present?
C  If so, the code will have to be modified to read in the
C  previously defined bonds and add to them.
C----------------------------------------------------------------------
      NAT     = 0
      ATOMS   = CX_F_STREAM(CEXMOL, CX_OB_ATOM)
      ATOM    = CX_F_NEXT(ATOMS)
      DO WHILE (NULL.NE.ATOM)
         IF (0.EQ.NIN(1)) ISTC(1) = NAT + 1
         NAT     = NAT    + 1
         IF (NAT.GT.MAXAT) THEN
            WRITE(6,'(a)') 'CEXMOL: Maximum number of atoms exceeded'
            CEXMOL = NULL
            RETURN
         END IF
         NIN(1)  = NIN(1) + 1
         IENC(1) = NAT
C----------------------------------------------------------------------
C  Assign atomic symbol numbers.  Du is used for *, DU isn't used.
C  These are found as the atomic number "integer" property
C  in the atom-tuple on the molecule defined by "cexmol"
C
C  'C', 'N', 'O', 'S', 'P', 'F', 'Cl','Br','I', 'H','Du','DU'
C   1    2    3    4    5    6    7    8    9   10   11
C
C----------------------------------------------------------------------
         AN = CX_F_IPROP(ATOM, "atomic number")
         IF (6.EQ.AN) THEN
            SYMNO(NAT) = 1
            ATNM(NAT)  = 'C'
         ELSE IF (7.EQ.AN) THEN
            SYMNO(NAT) = 2
            ATNM(NAT)  = 'N'
         ELSE IF (8.EQ.AN) THEN
            SYMNO(NAT) = 3
            ATNM(NAT)  = 'O'
         ELSE IF (16.EQ.AN) THEN
            SYMNO(NAT) = 4
            ATNM(NAT)  = 'S'
         ELSE IF (15.EQ.AN) THEN
            SYMNO(NAT) = 5
            ATNM(NAT)  = 'P'
         ELSE IF (9.EQ.AN) THEN
            SYMNO(NAT) = 6
            ATNM(NAT)  = 'F'
         ELSE IF (17.EQ.AN) THEN
            SYMNO(NAT) = 7
            ATNM(NAT)  = 'Cl'
         ELSE IF (35.EQ.AN) THEN
            SYMNO(NAT) = 8
            ATNM(NAT)  = 'Br'
         ELSE IF (53.EQ.AN) THEN
            SYMNO(NAT) = 9
            ATNM(NAT)  = 'I'
         ELSE IF (1.EQ.AN) THEN
            SYMNO(NAT) = 10
            ATNM(NAT)  = 'H'
         ELSE IF (0.EQ.AN) THEN
            SYMNO(NAT) = 11
            ATNM(NAT)  = 'Du'
         ELSE
            PRINT *, 'Element not found', AN
         END IF
C----------------------------------------------------------------------
C  Get coordinates
C  These are found on the atom-tuple with property name
C  determined above on the line with "XYZLEN = ..."
C  Replaced format '(F,F,F)' with * ... 1.22 13 Jul 1998, DW.
C----------------------------------------------------------------------
         LENS = CX_F_SPROP(ATOM, XYZNAM(:XYZLEN), COORD)
	 IF (10 .GT. LENS) THEN
	   NAT = NAT - 1
	 ELSE
           READ(COORD(:LENS), *) CR(1,NAT), CR(2,NAT), CR(3,NAT)
C----------------------------------------------------------------------
C  Save Fortran 1-origin index in atom object as 'ford' property
C  This creates an integer property that is the ordinal number
C  of the atom upon reading the molecule in. This will change once the
C  bonds have been determined.  "Ford" is the fortran ordinal-number.
C  It has nothing to do with the autmobiles...
C----------------------------------------------------------------------
           RV = CX_F_SET_IPROP(ATOM, 'ford', NAT)
C----------------------------------------------------------------------
C  Advance to next atom
C----------------------------------------------------------------------
	 END IF
         ATOM  = CX_F_NEXT(ATOMS)
      END DO
      CALL CX_F_DESTROY(ATOMS)
C----------------------------------------------------------------------
C  Fake residues: NSEG (number of residues) is artifically set to 1.
C  THIS IS A CRITICAL COMPONENT TO FIX LATER!
C  WILL CHAINS HELP? DCS 10/9/95
C----------------------------------------------------------------------
      NSEG       = 1
      ISTS(1)    = ISTC(1)
      IENS(1)    = IENC(1)
      ISCML(1)   = 1
      SECSQ(1)   = 1
      SECNM(1)   = 'UNK'
      CHAINID(1) = 'A'
C----------------------------------------------------------------------
C  Return with CEXMOL set or NULL.
C----------------------------------------------------------------------
      RETURN
      END

      SUBROUTINE NEWBONDS(MOL)
C======================================================================
C  NEWBONDS -- update bonds in cex-molecule "mol"
C----------------------------------------------------------------------
      INTEGER*4    NULL
      PARAMETER    ( NULL = 0 )
      INCLUDE     'bondage.cmn'
      INCLUDE     '../../../include/cx_f.inc'
      INCLUDE     '../../../include/cx_f_molecule.inc'
      INTEGER*4    MOL, ATOMS, ATOM, BOND
C     INTEGER*4    BONDS, BENDS, BEND1, BEND2
      INTEGER*4    molats(maxat)

      INTEGER*4 I,IBONDOR(MAXBND)

C----------------------------------------------------------------------
C  Create array of bond orders.
C----------------------------------------------------------------------

C----------------------------------------------------------------------
C LOOP OVER BONDS TO THIS POINT. FIND IF THIS IS A UNIQUE ENTRY.
C IF SO, ASSIGN IT A BOND ORDER OF 1, ELSE, INCREMENT THE
C PREVIOUS ENTRY, AND ASSIGN THIS A BOND-ORDER OF -1 (AS A FLAG)
C----------------------------------------------------------------------
      DO I = 1, NOJ(1)
         IAT = IANO(I)
         JAT = IBNO(I)
         IBONDOR(I) = 1
         DO J = 1 , I-1
            IPREV = IANO(J)
            JPREV = IBNO(J)
            IF (IAT.EQ.IPREV .AND. JAT .EQ. JPREV) THEN
               IF(IBONDOR(J).GT.0) IBONDOR(J) = IBONDOR(J) + 1
               IBONDOR(I) = -1
            ENDIF
         ENDDO
      END DO

      INDX = 1
      ATOMS   = CX_F_STREAM(MOL, CX_OB_ATOM)
      ATOM    = CX_F_NEXT(ATOMS)
      MOLATS(INDX) = ATOM
      DO WHILE (NULL.NE.ATOM)
         INDX = INDX + 1
         ATOM = CX_F_NEXT(ATOMS)
         MOLATS(INDX) = ATOM
      END DO
      CALL CX_F_DESTROY(ATOMS)

      DO I = 1, NOJ(1)
         IAT = IANO(I)
         JAT = IBNO(I)
         IF(IBONDOR(I).GE.1) THEN
            BOND=CX_F_CREATE_BOND(MOLATS(IAT),MOLATS(JAT),IBONDOR(I))
         ENDIF
      END DO
C----------------------------------------------------------------------
C This code is here to enumerate the bonds on the molecule.
C It is useful only for debugging purposes. It has been left
C as an example of how to cycle over the bonds.
C----------------------------------------------------------------------

C     BONDS = CX_F_STREAM(MOL, CX_OB_BOND)
C     BOND  = CX_F_NEXT(BONDS)
C     DO WHILE (NULL.NE.BOND)
C        BENDS = CX_F_STREAM(BOND, CX_OB_ATOM)
C        BEND1 = CX_F_NEXT(BENDS)
C        BEND2 = CX_F_NEXT(BENDS)
C        CALL CX_F_DESTROY(BENDS)
C        BOND  = CX_F_NEXT(BONDS)
C     END DO
C     CALL CX_F_DESTROY(BONDS)

      RETURN
      END

