/*****************************************************************************
*  connect.c -- CEX simple connectivity determination
*----------------------------------------------------------------------------
*  Contributing authors and institutions: Roger Sayle, Glaxo Wellcome R&D.
*                                         Dave Weininger, Daylight CIS, Inc.
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "cx.h"
#include "cx_molecule.h"

#ifndef True
#define True  1
#define False 0
#endif


#define MAXELEM   103
#define MAXCOVAL  2.0
#define COVALERR  0.56
#define MAXDIST   (MAXCOVAL+MAXCOVAL+COVALERR)

static double Coval[MAXELEM+1] = {
    0.680, 0.320, 1.600, 0.680, 0.352, 0.832, 0.720, 0.680, 0.680, 0.640,
    1.120, 0.972, 1.100, 1.352, 1.200, 1.048, 1.020, 1.000, 1.568, 1.328,
    0.992, 1.440, 1.472, 1.328, 1.352, 1.352, 1.340, 1.328, 1.500, 1.520,
    1.448, 1.220, 1.168, 1.208, 1.220, 1.208, 1.600, 1.472, 1.120, 1.780,
    1.560, 1.480, 1.472, 1.352, 1.400, 1.448, 1.500, 1.592, 1.688, 1.632,
    1.460, 1.460, 1.472, 1.400, 1.700, 1.672, 1.340, 1.872, 1.832, 1.820,
    1.808, 1.800, 1.800, 1.992, 1.792, 1.760, 1.752, 1.740, 1.728, 1.720,
    1.940, 1.720, 1.568, 1.432, 1.368, 1.352, 1.368, 1.320, 1.500, 1.500,
    1.700, 1.552, 1.540, 1.540, 1.680, 1.208, 1.900, 1.800, 1.432, 1.180,
    1.020, 0.888, 0.968, 0.952, 0.928, 0.920, 0.912, 0.900, 0.888, 0.880,
    0.872, 0.860, 0.848, 0.840 };

#define NOTE(msg) cx_error_save(msg, CX_ERR_NOTE, ProgName)
#define WARN(msg) cx_error_save(msg, CX_ERR_WARN, cx_sprop(mol,NameProp))

typedef struct _ObjList {
        struct _ObjList *next;
        cx_Object *atom;
        float x, y, z;
    } ObjList;

#define LISTPOOL 32
static ObjList *FreeList;
static ObjList *AtomList;

#define HASHSIZE 8
static ObjList *HashTable[HASHSIZE][HASHSIZE][HASHSIZE];


static float MinX, MinY, MinZ;
static float MaxX, MaxY, MaxZ;

static char *CoordProp;
static char *NameProp;

static char *ProgName;
static char *OutFile;
static char *InFile;


static ObjList *AllocateListItem( void )
{
    register ObjList *ptr;
    register int i;

    if( !(ptr = FreeList) )
    {   ptr = (ObjList*)malloc(LISTPOOL*sizeof(ObjList));
        for( i=1; i<LISTPOOL; i++ )
        {   ptr->next = FreeList;
            FreeList = ptr++;
        }
    } else FreeList = FreeList->next;
    return( ptr );
}

static void DeallocateList( ObjList *list )
{
    register ObjList *ptr;

    while( list )
    {   ptr = list;
        list = ptr->next;
        ptr->next = FreeList;
        FreeList = ptr;
    }
}


static void InitialiseHashTable( void )
{
    register int i,j,k;

    for( i=0; i<HASHSIZE; i++ )
        for( j=0; j<HASHSIZE; j++ )
            for( k=0; k<HASHSIZE; k++ )
                HashTable[i][j][k] = NULL;
    FreeList = (ObjList*)NULL;
    AtomList = (ObjList*)NULL;
}


static void ResetHashTable( void )
{
    register int i,j,k;

    for( i=0; i<HASHSIZE; i++ )
        for( j=0; j<HASHSIZE; j++ )
            for( k=0; k<HASHSIZE; k++ )
            {   DeallocateList(HashTable[i][j][k]);
                HashTable[i][j][k] = NULL;
            }
}


static cx_String GetCoordName( cx_Object mol, char *prop )
{
    cx_Object tupleseq, tuple;

    if( (tupleseq = cx_prefix2atuples(mol,prop)) )
    {   tuple = cx_next(tupleseq);
    } else tuple = (cx_Object)NULL;

    if( !tuple )
    {   WARN("No coordinate property");
        cx_destroy( tupleseq );
        return( (cx_String)NULL );
    }

    /* cx_next(tupleseq) */
    if( !cx_atend(tupleseq) )  
        WARN("Ambiguous coordinate property");
    return( cx_atomtuple_name(tuple) );
}


static void TestBonded( ObjList *atm1, ObjList *atm2 )
{
    register int elem1, elem2;
    register float dx,dy,dz;
    register float max;

    elem1 = cx_iprop(atm1->atom,"atomic number");
    if( (elem1<0) || (elem1>MAXELEM) ) elem1 = 0;

    elem2 = cx_iprop(atm2->atom,"atomic number");
    if( (elem2<0) || (elem2>MAXELEM) ) elem2 = 0;

    max = Coval[elem1] + Coval[elem2] + COVALERR;

    dx = atm1->x - atm2->x;
    dy = atm1->y - atm2->y;
    dz = atm1->z - atm2->z;
    if( (dx*dx+dy*dy+dz*dz) < (max*max) )
        cx_create_bond(atm1->atom,atm2->atom,1);
}


static void SimpleConnect( void )
{
    register ObjList *atm1, *atm2;

    for( atm1=AtomList; atm1; atm1=atm1->next )
        for( atm2=atm1->next; atm2; atm2=atm2->next )
            TestBonded( atm1, atm2 );
    DeallocateList(AtomList);
    AtomList = (ObjList*)NULL;
}


static void ComplexConnect( void )
{
    register ObjList *atm1, *atm2;
    register float sx,sy,sz;
    register int lx,ly,lz;
    register int hx,hy,hz;
    register int mx,my,mz;
    register int x,y,z;

    sx = (float)HASHSIZE/((MaxX+0.1)-MinX);
    sy = (float)HASHSIZE/((MaxY+0.1)-MinY);
    sz = (float)HASHSIZE/((MaxZ+0.1)-MinZ);

    for( atm1=AtomList; atm1; atm1=atm2 )
    {    lx = (int)(sx*((atm1->x-MAXDIST)-MinX));
         ly = (int)(sy*((atm1->y-MAXDIST)-MinY));
         lz = (int)(sz*((atm1->z-MAXDIST)-MinZ));

         hx = (int)(sx*((atm1->x+MAXDIST)-MinX));
         hy = (int)(sy*((atm1->y+MAXDIST)-MinY));
         hz = (int)(sz*((atm1->z+MAXDIST)-MinZ));

         if( lx < 0 ) lx = 0;   if( hx >= HASHSIZE ) hx = HASHSIZE-1;
         if( ly < 0 ) ly = 0;   if( hy >= HASHSIZE ) hy = HASHSIZE-1;
         if( lz < 0 ) lz = 0;   if( hz >= HASHSIZE ) hz = HASHSIZE-1;

         for( x=lx; x<=hx; x++ )
             for( y=ly; y<=hy; y++ )
                 for( z=lz; z<=hz; z++ )
                     for( atm2=HashTable[x][y][z]; atm2; atm2=atm2->next )
                         TestBonded(atm1,atm2);
         mx = (int)(sx*(atm1->x-MinX));
         my = (int)(sy*(atm1->y-MinY));
         mz = (int)(sz*(atm1->z-MinZ));

         atm2 = atm1->next;
         atm1->next = HashTable[mx][my][mz];
         HashTable[mx][my][mz] = atm1;
    }
    AtomList = (ObjList*)NULL;
    ResetHashTable();
}



static void cx_3d_to_connectivity( cx_Object mol )
{
    cx_String cprop;
    cx_Object atoms, atom;
    register ObjList *ptr;
    register char *cptr;
    register int count;
    float x,y,z;

    cprop = GetCoordName(mol,CoordProp);
    if( !cprop ) return;

    count = 0;
    atoms = cx_stream(mol, CX_OB_ATOM);
    while( (atom=cx_next(atoms)) != (cx_Object)0 )
    {   cptr = cx_sprop(atom,cprop);
        if( cptr && sscanf(cptr,"%g,%g,%g",&x,&y,&z) == 3 )
        {   if( count )
            {   if( x < MinX ) { MinX = x; } else if( x > MaxX ) MaxX = x;
                if( y < MinY ) { MinY = y; } else if( y > MaxY ) MaxY = y;
                if( z < MinZ ) { MinZ = z; } else if( z > MaxZ ) MaxZ = z;
            } else
            {   MinX = MaxX = x;
                MinY = MaxY = y;
                MinZ = MaxZ = z;
            }
            ptr = AllocateListItem();
            ptr->next = AtomList;
            ptr->atom = atom;
            AtomList = ptr;
            ptr->x = x;
            ptr->y = y;
            ptr->z = z;
            count++;
        }
    }

    if( count < 12 )
    {   SimpleConnect();
    } else ComplexConnect();
    cx_destroy(atoms);
}


static void DisplayUsage( void )
{
    NOTE("Usage:   connect [-pcoord <prop>] [<infile> [<outfile>]]");
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
            if( !strcmp(argv[i],"-pcoord") )
            {   CoordProp = argv[++i];
            } else if( !strcmp(argv[i],"-pname") )
            {   NameProp = argv[++i];
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
    cx_Object obj;

    ProgName = argv[0];
    InFile = (char*)NULL;
    OutFile = (char*)NULL;

    CoordProp = "coordinates";
    NameProp = "molname";

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

    InitialiseHashTable();
    while( (obj = cx_next(ifp)) ) 
    {   if( cx_type(obj) == CX_OB_MOLECULE )
            cx_3d_to_connectivity(obj);
       
        cx_append(ofp,obj);
        cx_error_spew(stderr, NULL);
        cx_destroy(obj);
    }

    /* cx_send(tab, tab, ofp); */
    cx_error_spew(stderr, NULL);
    cx_destroy(ofp);
    cx_destroy(ifp);
    cx_cleanup();
    return 0;
}

