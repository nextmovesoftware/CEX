/****************************************************************************
*  chains.c -- CEX polypeptide recognition
*----------------------------------------------------------------------------
*  Contributing authors and institutions: Roger Sayle, Glaxo Wellcome R&D.
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "cx.h"
#include "cx_molecule.h"

#ifndef True
#define True  1
#define False 0
#endif


#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE, ProgName)

#define PROP_MNAME   "molname"
#define PROP_ALABS   "atomname"
#define PROP_RESID   "resname"
#define PROP_RESNO   "resno"
#define PROP_CHAIN   "chain"
#define PROP_HETA    "hetatm"
#define PROP_ORDER   "pdborder"


#define BitN      0x0001
#define BitNTer   0x0002
#define BitNPro   0x0004
#define BitNPT    0x0008
#define BitCA     0x0010
#define BitCAGly  0x0020
#define BitC      0x0100
#define BitCTer   0x0200
#define BitCOXT   0x0400
#define BitO      0x1000
#define BitOXT    0x2000

#define BitNAll   0x000F
#define BitCAAll  0x0030
#define BitCAll   0x0700
#define BitOAll   0x3000

typedef struct {
    int flag;
    short elem, count;
    int n1, n2, n3;
        } Template;

#define MAXPEPTIDE  11
static Template Peptide[MAXPEPTIDE] = {
    /* N     */    {  0x0001, 7, 2, 0x0030, 0x0100,      0 },
    /* NTer  */    {  0x0002, 7, 1, 0x0030,      0,      0 },
    /* NPro  */    {  0x0004, 7, 3, 0x0030, 0x0100,     -6 },
    /* NPT   */    {  0x0008, 7, 2, 0x0030,     -6,      0 },
    /* CA    */    {  0x0010, 6, 3, 0x000F, 0x0700,     -6 },
    /* CAGly */    {  0x0020, 6, 2, 0x0003, 0x0700,      0 },
    /* C     */    {  0x0100, 6, 3, 0x0030, 0x1000, 0x0005 },
    /* CTer  */    {  0x0200, 6, 2, 0x0030, 0x1000,      0 },
    /* COXT  */    {  0x0400, 6, 3, 0x0030, 0x1000, 0x2000 },
    /* O     */    {  0x1000, 8, 1, 0x0700,      0,      0 },
    /* OXT   */    {  0x2000, 8, 1, 0x0400,      0,      0 }
        };


#define ATOMMAX   38
static char AtomName[ATOMMAX][4] = {
    /*  0 */  { ' ', 'N', ' ', ' ' },
    /*  1 */  { ' ', 'C', 'A', ' ' },
    /*  2 */  { ' ', 'C', ' ', ' ' },
    /*  3 */  { ' ', 'O', ' ', ' ' },
    /*  4 */  { ' ', 'C', 'B', ' ' },
    /*  5 */  { ' ', 'S', 'G', ' ' },
    /*  6 */  { ' ', 'O', 'G', ' ' },
    /*  7 */  { ' ', 'C', 'G', ' ' },
    /*  8 */  { ' ', 'O', 'G', '1' },
    /*  9 */  { ' ', 'C', 'G', '1' },
    /* 10 */  { ' ', 'C', 'G', '2' },
    /* 11 */  { ' ', 'C', 'D', ' ' },
    /* 12 */  { ' ', 'O', 'D', ' ' },
    /* 13 */  { ' ', 'S', 'D', ' ' },
    /* 14 */  { ' ', 'C', 'D', '1' },
    /* 15 */  { ' ', 'O', 'D', '1' },
    /* 16 */  { ' ', 'N', 'D', '1' },
    /* 17 */  { ' ', 'C', 'D', '2' },
    /* 18 */  { ' ', 'O', 'D', '2' },
    /* 19 */  { ' ', 'N', 'D', '2' },
    /* 20 */  { ' ', 'C', 'E', ' ' },
    /* 21 */  { ' ', 'N', 'E', ' ' },
    /* 22 */  { ' ', 'C', 'E', '1' },
    /* 23 */  { ' ', 'O', 'E', '1' },
    /* 24 */  { ' ', 'N', 'E', '1' },
    /* 25 */  { ' ', 'C', 'E', '2' },
    /* 26 */  { ' ', 'O', 'E', '2' },
    /* 27 */  { ' ', 'N', 'E', '2' },
    /* 28 */  { ' ', 'C', 'E', '3' },
    /* 29 */  { ' ', 'C', 'Z', ' ' },
    /* 30 */  { ' ', 'N', 'Z', ' ' },
    /* 31 */  { ' ', 'C', 'Z', '2' },
    /* 32 */  { ' ', 'C', 'Z', '3' },
    /* 33 */  { ' ', 'O', 'H', ' ' },
    /* 34 */  { ' ', 'N', 'H', '1' },
    /* 35 */  { ' ', 'N', 'H', '2' },
    /* 36 */  { ' ', 'C', 'H', '2' },
    /* 37 */  { ' ', 'O', 'X', 'T' }
        };

static char ResName[24][4] = {
    /*  0 */ "UNK",
    /*  1 */ "ALA",
    /*  2 */ "ARG",
    /*  3 */ "ASN",
    /*  4 */ "ASP",
    /*  5 */ "CYS",
    /*  6 */ "GLU",
    /*  7 */ "GLN",
    /*  8 */ "GLY",
    /*  9 */ "HIS",
    /* 10 */ "ILE",
    /* 11 */ "LEU",
    /* 12 */ "LYS",
    /* 13 */ "MET",
    /* 14 */ "PHE",
    /* 15 */ "PRO",
    /* 16 */ "SER",
    /* 17 */ "THR",
    /* 18 */ "TRP",
    /* 19 */ "TYR",
    /* 20 */ "VAL",
    /* 21 */ "HOH",
    /* 22 */ "LIG",
    /* 23 */ "HYP"
        };

#define INT_BITMASK  "_bitmask"
#define INT_ATOMID   "_atomid"
#define INT_RESID    "_resid"
#define INT_HCOUNT   "_hcount"

static char *AtmNameProp;
static char *ResNameProp;
static char *ResNoProp;
static char *ChainProp;
static char *OrderProp;
static char *HetaProp;

static char *ProgName;
static char *OutFile;
static char *InFile;


static int BondCount( cx_Object atom )
{
    register int count;
    cx_Object bonds,bond;
    cx_Object other;

    count = 0;
    bonds = cx_stream(atom, CX_OB_BOND);
    while( (bond=cx_next(bonds)) != (cx_Object)0 )
    {    other = cx_xatom(atom,bond);
         if( cx_iprop(other,"atomic number") > 1 )
             count++;
    }
    cx_destroy(bonds);
    return count;
}


static void DetermineHetaAtoms( cx_Object mol )
{
    register int elem;
    cx_Object atoms, atom;

    atoms = cx_stream(mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {   elem = cx_iprop(atom,"atomic number");
        if( elem > 1 )
        {   if( !BondCount(atom) )
            {   cx_set_sprop(atom,HetaProp,"T");
                if( elem == 8 )
                {      cx_set_iprop(atom,INT_RESID,21);
                } else cx_set_iprop(atom,INT_RESID,22);
            } else cx_set_sprop(atom,HetaProp,"F");
        }
    }
    cx_destroy(atoms);
}


static int RecurseChain( cx_Object atom, char *c )
{
    register char *ptr;
    register int result;
    cx_Object bonds, bond;
    cx_Object other;

    result = 1;
    cx_set_sprop(atom,ChainProp,c);
    bonds = cx_stream(atom, CX_OB_BOND);
    while( (bond=cx_next(bonds)) != (cx_Object)0 )
    {   other = cx_xatom(atom,bond);
        ptr = cx_sprop(other,ChainProp);
        if( !ptr || (*ptr==' ') )
            result += RecurseChain( other, c );
    }
    cx_destroy(bonds);
    return result;
}


static void ClearChain( cx_Object mol, int ch )
{
    cx_Object atoms, atom;
    register char *ptr;

    atoms = cx_stream( mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {   ptr = cx_sprop(atom,ChainProp);
        if( ptr && (*ptr==ch) )
        {   cx_set_sprop(atom,HetaProp,"T");
            cx_set_sprop(atom,ChainProp," ");
            cx_set_iprop(atom,INT_RESID,22);
        }
    }
    cx_destroy(atoms);
}


static void DetermineConnectedChains( cx_Object mol )
{
    cx_Object atoms, atom;
    register char *chain;
    register char *heta;
    register int count;
    register int size;
    static char ch[2];

    count = 0;
    ch[1] = '\0';
    atoms = cx_stream( mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {    chain = cx_sprop(atom,ChainProp);
         heta = cx_sprop(atom,HetaProp);
         if( chain && (*chain==' ') && heta && (*heta=='F') )
         {   ch[0] = 'A'+count;
             size = RecurseChain(atom,ch);
             if( size < 10 ) 
             {   ClearChain( mol, ch[0] );
             } else count++;
         }
    }

    if( count == 1 )
    {   cx_reset(atoms);
        while( (atom=cx_next(atoms)) != (cx_Object)0 )
            cx_set_sprop(atom,ChainProp," ");
    }
    cx_destroy(atoms);
}


static int MatchConstraint( cx_Object atom, int mask )
{
    if( mask < 0 )
        return( cx_iprop(atom,"atomic number") == -mask );
    return( cx_iprop(atom,INT_BITMASK) & mask );
}


static int MatchConstraints( Template *pep, cx_Object na,
                             cx_Object nb, cx_Object nc )
{
    return( MatchConstraint(na,pep->n1) &&
            MatchConstraint(nb,pep->n2) &&
            MatchConstraint(nc,pep->n3) );
}


static void ConstrainPeptides( cx_Object mol )
{
    cx_Object bonds, bond;
    cx_Object atoms, atom;
    cx_Object neighbour[4];
    cx_Object na,nb,nc;
    cx_Object other;

    register Template *pep;
    register int orig,change;
    register int elem,mask;
    register int count;
    register int i,j;

    /* First Pass */
    atoms = cx_stream( mol, CX_OB_ATOM );
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {   elem = cx_iprop(atom,"atomic number");
        count = BondCount(atom);
        mask = 0;

        for( i=0; i<MAXPEPTIDE; i++ )
            if( (Peptide[i].elem==elem) && (Peptide[i].count==count) )
                mask |= Peptide[i].flag;
        cx_set_iprop(atom,INT_BITMASK,mask);
    }

    /* Second Pass */
    do {
        change = False;

        cx_reset(atoms);
        while( (atom=cx_next(atoms)) != (cx_Object)0 )
        {   mask = cx_iprop(atom,INT_BITMASK);
            if( mask )
            {   /* Determine Neighbours */
                i = 0;
                bonds = cx_stream(atom, CX_OB_BOND);
                while( (bond=cx_next(bonds)) != (cx_Object)0 )
                {   other = cx_xatom(atom,bond);
                    if( cx_iprop(other,"atomic number") > 1 )
                        neighbour[i++] = other;
                }
                cx_destroy(bonds);

                na = neighbour[0];
                nb = neighbour[1];
                nc = neighbour[2];

                orig = mask;
                for( j=0; j<MAXPEPTIDE; j++ )
                {   pep = &Peptide[j];
                    if( pep->flag & mask )
                    {   if( i == 3 )
                        {   if( !MatchConstraints(pep,na,nb,nc) &&
                                !MatchConstraints(pep,na,nc,nb) &&
                                !MatchConstraints(pep,nb,na,nc) &&
                                !MatchConstraints(pep,nb,nc,na) &&
                                !MatchConstraints(pep,nc,na,nb) &&
                                !MatchConstraints(pep,nc,nb,na) )
                                mask &= ~pep->flag;
                        } else if( i == 2 )
                        {   if( !(MatchConstraint(na,pep->n1) &&
                                  MatchConstraint(nb,pep->n2)) &&
                                !(MatchConstraint(nb,pep->n1) &&
                                  MatchConstraint(na,pep->n2)) )
                                mask &= ~pep->flag;
                        } else if( i == 1 )
                        {   if( !MatchConstraint(na,pep->n1) )
                                mask &= ~pep->flag;
                        }
                    }
                }

                if( mask != orig )
                {   cx_set_iprop(atom,INT_BITMASK,mask);
                    change = True;
                }
            }
            
        }
    } while( change );
    cx_destroy(atoms);
}


static void TracePeptideChain( cx_Object atom, int r )
{
    cx_set_iprop(atom,ResNoProp,r);
}


static void DeterminePeptideBackbone( cx_Object mol )
{
    cx_Object atoms, atom;
    register int atomid;
    register int mask;

    ConstrainPeptides( mol );

    /* OrderPeptideBackbone */
    atoms = cx_stream( mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {    atomid = cx_iprop(atom,INT_ATOMID);
         if( atomid == -1 )
         {   mask = cx_iprop(atom,INT_BITMASK);
             if( mask & BitNTer )
             {   cx_set_iprop(atom,INT_ATOMID,0);
                 TracePeptideChain(atom,1);
             } else if( (mask&BitNPT) && !(mask&BitN) )
             {   cx_set_iprop(atom,INT_ATOMID,0);
                 TracePeptideChain(atom,1);
             }
         }
    }
    cx_destroy(atoms);
}


static void DetermineHydrogens( cx_Object mol )
{
    register int num;
    register char *ptr;
    cx_Object atoms,atom;
    cx_Object bonds,bond;
    cx_Object other;

    atoms = cx_stream( mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
        if( cx_iprop(atom,"atomic number") == 1 )
        {   bonds = cx_stream( atom, CX_OB_BOND);
            while( (bond=cx_next(bonds)) != (cx_Object)0 )
            {   other = cx_xatom(atom,bond);
                if( cx_iprop(other,"atomic number") != 1 )
                {   ptr = cx_sprop(other,HetaProp);
                    cx_set_sprop(atom,HetaProp,ptr);

                    num = cx_iprop(other,ResNoProp);
                    if( num ) cx_set_iprop(atom,ResNoProp,num);

                    num = cx_iprop(other,INT_ATOMID);
                    cx_set_iprop(atom,INT_ATOMID,num);

                    num = cx_iprop(other,INT_RESID);
                    cx_set_iprop(atom,INT_RESID,num);

                    /* Maintain hcount */
                    num = cx_iprop(other,INT_HCOUNT);
                    cx_set_iprop(other,INT_HCOUNT,num+1);
                    cx_set_iprop(atom,INT_HCOUNT,num+1);
                    break;
                }
            }
            cx_destroy(bonds);
        }

    cx_reset(atoms);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
        if( cx_iprop(atom,"atomic number") == 1 )
        {   bonds = cx_stream( atom, CX_OB_BOND);
            while( (bond=cx_next(bonds)) != (cx_Object)0 )
            {   other = cx_xatom(atom,bond);
                if( cx_iprop(other,"atomic number") != 1 )
                {   num = cx_iprop(other,INT_HCOUNT);
                    if( num == 1 )
                        cx_set_iprop(atom,INT_HCOUNT,0);
                    break;
                }
            }
        }
    cx_destroy(atoms);
}


static void PreparePDBRecords( cx_Object mol )
{
    static char name[5];
    cx_Object atoms,atom;
    register int atomid;
    register int resid;
    register char *ptr;
    register int i;

    atoms = cx_stream(mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {   resid = cx_iprop(atom,INT_RESID);
        cx_set_sprop(atom,ResNameProp,ResName[resid]);

        atomid = cx_iprop(atom,INT_ATOMID);
        if( atomid == -1 )
        {   ptr = cx_sprop(atom,"atomic symbol");
            if( ptr[1] )
            {   name[0] = ptr[0];
                name[1] = toupper(ptr[1]);
            } else
            {   name[0] = ' ';
                name[1] = ptr[0];
            }
            name[2] = ' ';
            name[3] = ' ';
        } else if( cx_iprop(atom,"atomic number") == 1 )
        {   i = cx_iprop(atom,INT_HCOUNT);
            if( i )
            {   name[0] = '0'+i;
            } else name[0] = ' ';

            name[1] = 'H';
            name[2] = AtomName[atomid][2];
            name[3] = AtomName[atomid][3];
        } else for( i=0; i<4; i++ )
            name[i] = AtomName[atomid][i];

        name[4] = '\0';
        cx_set_sprop(atom,AtmNameProp,name);
    }
    cx_destroy(atoms);
}


static void InitialiseMol( cx_Object mol )
{
    cx_Object atoms, atom;

    atoms = cx_stream(mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {   cx_set_sprop(atom,ChainProp," ");

        cx_set_iprop(atom,INT_HCOUNT,0);
        cx_set_iprop(atom,INT_ATOMID,-1);
    }
    cx_destroy(atoms);
}


static void cx_connectivity_to_peptide( cx_Object mol )
{
    InitialiseMol( mol );
    DetermineHetaAtoms( mol );
    DetermineConnectedChains( mol );
    DeterminePeptideBackbone( mol );
    /* DeterminePeptideSidechains( mol ); */
    DetermineHydrogens( mol );
    PreparePDBRecords( mol );
}


static void useatomtuple( cx_Object mol, char *pname )
{
    cx_Object tuples,tuple;

    if( (tuples = cx_prefix2atuples(mol,pname)) )
    {   tuple = cx_next(tuples);
    } else tuple = (cx_Object)NULL;
    cx_destroy(tuples);

    if( tuple ) return;

    tuple = cx_create_atomtuple(mol,pname);
    cx_set_datatype(tuple, cx_pname2datatype(NULL,pname));
}


static void prepare_atomtuples( cx_Object mol )
{
    useatomtuple(mol,AtmNameProp);
    useatomtuple(mol,ResNameProp);
    useatomtuple(mol,ResNoProp);
    useatomtuple(mol,OrderProp);
    useatomtuple(mol,ChainProp);
    useatomtuple(mol,HetaProp);
}


static void define_datatypes( void )
{
#ifdef ORIG
    register cx_IOStream *fpdts;

    if( (fp=cx_fopen("$CX_ROOT/data/datatypes.cex","r")) )
    {   cx_recieve(NULL,fp,NULL);
        cx_fclose(fp);
        return;
    }
#endif
    cx_create_datatype(NULL, "ANAM", AtmNameProp, "Atom name",
                       "1A", "STRING", "Atom name as arbitrary ASCII text");
    cx_create_datatype(NULL, "CHAIN", ChainProp, "Atom chain ID",
                       "1A", "STRING", "Atom chain membership as ID");
    cx_create_datatype(NULL, "RESNO", ResNoProp, "Atom residue number",
                       "1A", "INTEGER", "Atom residue number");
    cx_create_datatype(NULL, "HETA", HetaProp, "Atom PDB hetatom flag",
                       "1A", "STRING", "Atom PDB hetatom flag, T or F");
    cx_create_datatype(NULL, "PDBORD", OrderProp, "Atom PDB serial number",
                       "1A", "INTEGER", "Atom PDB serial number (order)");
    cx_create_datatype(NULL, "RESNA", ResNameProp, "Atom residue name",
                       "1A", "STRING", 
                       "Atom residue name, typically three upper case letters");
}


static void DisplayUsage( void )
{
    NOTE("Usage:   chains [-<propname> <string>] [<infile> [<outfile>]]");
    cx_error_spew(stderr,NULL);
    exit(1);
}


static void ProcessCommandLine( int argc, char *argv[] )
{
    register int i,j;

    j = 0;
    for( i=1; i<argc; i++ )
        if( (argv[i][0]=='-') && argv[i][1] )
        {   /* All options take one argument! */
            if( i+1 == argc ) DisplayUsage();
            if( !strcmp(argv[i],"-atomname") )
            {   AtmNameProp = argv[++i];
            } else if( !strcmp(argv[i],"-resname") )
            {   ResNameProp = argv[++i];
            } else if( !strcmp(argv[i],"-resno") )
            {   ResNoProp = argv[++i];
            } else if( !strcmp(argv[i],"-hetatm") )
            {   HetaProp = argv[++i];
            } else if( !strcmp(argv[i],"-chain") )
            {   ChainProp = argv[++i];
            } else if( !strcmp(argv[i],"-pdborder") )
            {   OrderProp = argv[++i];
            } else DisplayUsage();
        } else switch( j++ )
        {   case(0):  InFile = argv[i];  break;
            case(1):  OutFile = argv[i]; break;
            default:  DisplayUsage();
        }
}


int main( int argc, char *argv[] )
{
    register cx_Object ifp, ofp;
    register cx_Object obj;

    ProgName = argv[0];
    InFile = (char*)NULL;
    OutFile = (char*)NULL;

    AtmNameProp = PROP_ALABS;
    ResNameProp = PROP_RESID;
    ResNoProp   = PROP_RESNO;
    ChainProp   = PROP_CHAIN;
    OrderProp   = PROP_ORDER;
    HetaProp    = PROP_HETA;

    ProcessCommandLine( argc, argv );

    if( !InFile ) InFile = "-";
    ifp = cx_create_iostream(InFile,CX_IO_READ);

    if( !ifp )
    {   cx_error_save("Unable to open input file",CX_ERR_FATAL,*argv);
        cx_error_save(InFile,CX_ERR_FATAL,*argv);
        cx_error_spew(stderr,NULL);
        exit(1);
    }

    if( !OutFile ) OutFile = "-";
    ofp = cx_create_iostream(OutFile,CX_IO_WRITE);

    if( !ofp )
    {   cx_error_save("Unable to open output file",CX_ERR_FATAL,*argv);
        cx_error_save(InFile,CX_ERR_FATAL,*argv);
        cx_error_spew(stderr,NULL);
        exit(1);
    }

    cx_molecule_pkg();
    define_datatypes();

    while( (obj = cx_next(ifp)) ) 
    {   if( cx_type(obj) == CX_OB_MOLECULE )
        {   prepare_atomtuples( obj );
            cx_connectivity_to_peptide(obj);
        }
        cx_append(ofp,obj);
        cx_error_spew(stderr, NULL);
        cx_destroy(obj);
    }

    /* cx_send(cx_default_datatypetable(), NULL, ofp); */
    cx_error_spew(stderr, NULL);
    cx_destroy(ofp);
    cx_destroy(ifp);
    cx_cleanup();
    return 0;
}
