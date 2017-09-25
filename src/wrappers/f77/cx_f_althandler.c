#include <stdio.h>
#include <string.h>
#include <memory.h>
#include "cx.h"

typedef cx_Integer  FLENS;
typedef cx_Integer *FPLEN;

/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Some compilers append a "_" to fortran function names, but others
* do not.  Because this was written first for Sun (which does append
* "_"), the code has the "_" in the names.  For machines that don't
* use this convention, we just redefine these names back to their
* "real" values.
======================================================================*/

#if defined(HPUX) || defined(NEXT)

#define cx_f_smilin_	cx_f_smilin
#define cx_f_count_	cx_f_count
#define althandler_	althandler
#define erk_		erk

#endif

/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Linux-ppc appends "__" to each Fortran function name!
======================================================================*/

#if defined(LINUX)

#define cx_f_smilin_	cx_f_smilin__
#define cx_f_count_	cx_f_count__
#define althandler_	althandler__
#define erk_		erk__

#endif


/*============================================================================
 *  c2fstr -- copy C char array to fortran string
 *
 *  This copies MIN(lens,mxout) chars of str to out and returns the number of
 *  characters copied; if less than mxout, out is blank-padded to mxout - 1.
 */

static cx_Integer c2fstr(cx_Integer lens, cx_String str, cx_String out,
                         cx_Integer mxout )
{
   if (NULL == out  ) return 0;
   if (lens >  mxout) lens = mxout;
   if (NULL == str  ) lens = 0;
   if (0    <  lens ) memcpy(out, str, lens);
   if (lens <  mxout) memset(out + lens, ' ', mxout - lens);
   return lens;
}

/*============================================================================
 *  f2cstr -- copy Fortran string to C string
 */

static cx_String f2cstr(cx_Integer lens, cx_String str)
{
   cx_String string;
   if (0    > lens) lens = 0;
   if (NULL == str) str = "";
   string = (cx_String) malloc(lens + 1);
   strncpy(string, str, lens);
   string[lens] = '\0';
   return string;
}

/*============================================================================
 *  cx_f_smilin_
  { return dt_smilin(mx, str); }
 */

cx_Integer cx_f_smilin_(cx_Object *par, cx_String smi, FLENS lens)
{
   cx_String str = f2cstr(lens, smi);
cx_Object mol;
   fprintf(stderr, "lens %d\n", lens);
   fprintf(stderr, "par %p, smi `%s'\n", *par, str);
   /* return (cx_Integer) cx_smilin(*par, str); */
   mol = cx_smilin(*par, str);
   fprintf(stderr, "mol %p (%d), CX_OB_ATOM %d\n", mol, mol, CX_OB_ATOM);
   return (cx_Integer) mol;
}

/*============================================================================
 *  cx_f_count_
 */

cx_Integer cx_f_count_(cx_Object *ob, cx_Integer *type)
{
   fprintf(stderr, "ob %p, type %d (%d)\n", *ob, *type, type);
   return cx_count(*ob, *type);
}


/* #include "/opt/SUNWspro/SC3.0/include/cc/sunmath.h" */
#include <sunmath.h>
#include <siginfo.h>
#include <ucontext.h>

static void fpehdl(int sig, siginfo_t *sip, ucontext_t *uap) 
{
   fprintf(stderr, "fp exception, sig: %d\n", sig);
   return;
}

void althandler_()
{
   ieee_handler("set", "all", fpehdl);
}

void erk_()
{
   int   rv;
   char *action = "get";
   char *mode   = "exception";
   char *in     = "";
   char *out;
   rv = ieee_flags(action, mode, in, &out);
   fprintf(stderr, "rv %0X: %s\n", rv, out);
}

#ifdef NOPE
C-----------------------------------------------------------------------
C  cx_f.h -- CX definitions and wrappers for Fortran
C-----------------------------------------------------------------------
C  Author and institution: Dave Weininger, Daylight CIS, Inc.
C
C  This source code is contributed to the public domain and may be
C  freely copied and redistributed for research, profit, fun or any
C  other reason, with these restrictions: (1) unmodified or
C  functionally equivalent code derived from CX code must contain this
C  notice, (2) all derived code must acknowledge the author and
C  institution, and (3) the functional definition of symbols starting
C  CX_ or cx_ may not be changed (if you need to change a function,
C  CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C  Limits
C-----------------------------------------------------------------------
      INTEGER*4 CX_PATH_MAX;
      PARAMETER ( CX_PATH_MAX = 1024 )
C-----------------------------------------------------------------------
C  Symbolic names for pseudo-object classes
C-----------------------------------------------------------------------
      INTEGER*4 CX_OB_INVALID, CX_OB_ANY
      PARAMETER ( CX_OB_INVALID = 0 )
      PARAMETER ( CX_OB_ANY     = 1 )
C-----------------------------------------------------------------------
C  Symbolic names for basic object classes
C-----------------------------------------------------------------------
      INTEGER*4 CX_OB_STRING,         CX_OB_ERRORQUEUE, CX_OB_STREAM,
     &          CX_OB_SEQUENCE,       CX_OB_PROPERTY,   CX_OB_DATATYPE,
     &          CX_OB_DATATYPE_TABLE, CX_OB_BINARY
      PARAMETER ( CX_OB_STRING         = 2  )
      PARAMETER ( CX_OB_ERRORQUEUE     = 3  )
      PARAMETER ( CX_OB_STREAM         = 4  )
      PARAMETER ( CX_OB_SEQUENCE       = 5  )
      PARAMETER ( CX_OB_PROPERTY       = 6  )
      PARAMETER ( CX_OB_DATATYPE       = 7  )
      PARAMETER ( CX_OB_DATATYPE_TABLE = 8  )
      PARAMETER ( CX_OB_BINARY         = 9  )
C-----------------------------------------------------------------------
C  Symbolic names for molecule object classes
C-----------------------------------------------------------------------
      INTEGER*4 CX_OB_MOLECULE,  CX_OB_ATOM, CX_OB_BOND,
     &          CX_OB_ATOMTUPLE, CX_OB_BONDTUPLE,
     &          CX_OB_SMIATOMS,  CX_OB_SMIBONDS
      PARAMETER ( CX_OB_MOLECULE       = 10 )
      PARAMETER ( CX_OB_ATOM           = 11 )
      PARAMETER ( CX_OB_BOND           = 12 )
      PARAMETER ( CX_OB_ATOMTUPLE      = 13 )
      PARAMETER ( CX_OB_BONDTUPLE      = 14 )
      PARAMETER ( CX_OB_SMIATOMS       = 15 )
      PARAMETER ( CX_OB_SMIBONDS       = 16 )
C-----------------------------------------------------------------------
C  Symbolic names for FALSE and TRUE
C-----------------------------------------------------------------------
      INTEGER*4 FALSE, TRUE
      PARAMETER ( FALSE = 0 )
      PARAMETER ( TRUE  = 1 )
C-----------------------------------------------------------------------
C CEX IO quote character.
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_QCEX_CHAR, CX_QCEX_STR
      PARAMETER ( CX_QCEX_CHAR = '"' )
      PARAMETER ( CX_QCEX_STR  = '"' )
C-----------------------------------------------------------------------
C Tuple separator, as character and string constants.
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_TUPLE_CHAR, CX_TUPLE_STR
      PARAMETER ( CX_TUPLE_CHAR = ';' )
      PARAMETER ( CX_TUPLE_STR  = ';' )
C-----------------------------------------------------------------------
C  Flags used by cx_cex_listfmt()
C-----------------------------------------------------------------------
      INTEGER*4 CX_FMT_RAW, CX_FMT_DUMP, CX_FMT_LIST
      PARAMETER ( CX_FMT_RAW  = 0 )
      PARAMETER ( CX_FMT_DUMP = 1 )
      PARAMETER ( CX_FMT_LIST = 2 )
C-----------------------------------------------------------------------
C  Flags used by cx_e_cex_xtagdata()
C-----------------------------------------------------------------------
      INTEGER*4 CX_CXT_UNKNOWN,  CX_CXT_IDENTIFIER, CX_CXT_DATAITEM,
     &          CX_CXT_PROPERTY
      PARAMETER ( CX_CXT_UNKNOWN    =    0 )
      PARAMETER ( CX_CXT_IDENTIFIER = 4201 )
      PARAMETER ( CX_CXT_DATAITEM   = 4202 )
      PARAMETER ( CX_CXT_PROPERTY   = 4203 )
C-----------------------------------------------------------------------
C  Symbolic and property names for standard properties
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_TAG,   CX_PROP_PNAME, CX_PROP_VNAME,
     &              CX_PROP_SHAPE, CX_PROP_LANG,  CX_PROP_DESC,
     &              CX_PROP_MARK
      PARAMETER ( CX_PROP_TAG   = 'tag'           )
      PARAMETER ( CX_PROP_PNAME = 'property name' )
      PARAMETER ( CX_PROP_VNAME = 'verbose name'  )
      PARAMETER ( CX_PROP_SHAPE = 'shape'         )
      PARAMETER ( CX_PROP_LANG  = 'language'      )
      PARAMETER ( CX_PROP_DESC  = 'description'   )
      PARAMETER ( CX_PROP_MARK  = '_mark'         )
C-----------------------------------------------------------------------
C  Standard severity flags.
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_ERR_NONE,  CX_ERR_NOTE, CX_ERR_WARN,
     &              CX_ERR_ERROR, CX_ERR_FATAL
      PARAMETER ( CX_ERR_NONE  = 'NOERROR' )
      PARAMETER ( CX_ERR_NOTE  = 'NOTE'    )
      PARAMETER ( CX_ERR_WARN  = 'WARNING' )
      PARAMETER ( CX_ERR_ERROR = 'ERROR'   )
      PARAMETER ( CX_ERR_FATAL = 'FATAL'   )
C-----------------------------------------------------------------------
C  Output level flags for cu_spewob().
C-----------------------------------------------------------------------
      INTEGER*4 CX_SPEW_NONE,  CX_SPEW_DATA,     CX_SPEW_CHILDREN,
     &          CX_SPEW_PROPS, CX_SPEW_PROPNAME, CX_SPEW_DATATAG,
     &          CX_SPEW_CLASS, CX_SPEW_SUMTUPLE, CX_SPEW_DEFAULT
      PARAMETER ( CX_SPEW_NONE     =  0 )
      PARAMETER ( CX_SPEW_DATA     =  1 )
      PARAMETER ( CX_SPEW_CHILDREN =  2 )
      PARAMETER ( CX_SPEW_PROPS    =  4 )
      PARAMETER ( CX_SPEW_PROPNAME =  8 )
      PARAMETER ( CX_SPEW_DATATAG  = 16 )
      PARAMETER ( CX_SPEW_CLASS    = 32 )
      PARAMETER ( CX_SPEW_SUMTUPLE = 64 )
      PARAMETER ( CX_SPEW_DEFAULT  =  7 )
C-----------------------------------------------------------------------
C  Function declarations take room in Fortran, so these are commented
C  out here: declare them in program units where they are used.
C  Subroutines (void entry points) are not declared in Fortran
C-----------------------------------------------------------------------
C     INTEGER*4 cx_f_ancestor
C     INTEGER*4 cx_f_append
C     INTEGER*4 cx_f_atend
C     INTEGER*4 cx_f_binaryvalue
C     INTEGER*4 cx_f_cex_eof
C     INTEGER*4 cx_f_cex_listfmt
C     INTEGER*4 cx_f_count
C     INTEGER*4 cx_f_create_binary
C     INTEGER*4 cx_f_create_datatype
C     INTEGER*4 cx_f_create_datatypetable
C     INTEGER*4 cx_f_create_sequence
C     INTEGER*4 cx_f_create_sequence
C     INTEGER*4 cx_f_create_string
C     INTEGER*4 cx_f_datatype
C     INTEGER*4 cx_f_default_datatypetable
C     INTEGER*4 cx_f_delete
C     INTEGER*4 cx_f_dt_mark
C     INTEGER*4 cx_f_dt_setmark
C     INTEGER*4 cx_f_dt_table_setmarks
C     INTEGER*4 cx_f_error_count
C     INTEGER*4 cx_f_error_save
C     INTEGER*4 cx_f_error_spew
C     INTEGER*4 cx_f_errorqueue
C     INTEGER*4 cx_f_iprop
C     INTEGER*4 cx_f_next
C     INTEGER*4 cx_f_parent
C     INTEGER*4 cx_f_parse_binary
C     INTEGER*4 cx_f_pname2datatype
C     INTEGER*4 cx_f_prefix2props
C     INTEGER*4 cx_f_prop_name
C     INTEGER*4 cx_f_realformat
C     INTEGER*4 cx_f_receive
C     INTEGER*4 cx_f_reset
C     REAL*4    cx_f_rprop
C     INTEGER*4 cx_f_send
C     INTEGER*4 cx_f_set_datatype
C     INTEGER*4 cx_f_set_iprop
C     INTEGER*4 cx_f_set_method
C     INTEGER*4 cx_f_set_parent
C     INTEGER*4 cx_f_set_rprop
C     INTEGER*4 cx_f_set_sprop
C     INTEGER*4 cx_f_set_typename
C     INTEGER*4 cx_f_sprop
C     INTEGER*4 cx_f_stream
C     INTEGER*4 cx_f_stringvalue
C     INTEGER*4 cx_f_tag2datatype
C     INTEGER*4 cx_f_type
C     INTEGER*4 cx_f_type2typename
C     INTEGER*4 cx_f_typename
C     INTEGER*4 cx_f_typename2type
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C  Don't quite know what do to with utilities yet, most can't be called
C  from Fortran...
C-----------------------------------------------------------------------
C
C void        cx_f_panic     (cx_String   msg, cx_String  src  );
C void       *cx_f_malloc    (cx_Integer  size                 );
C void       *cx_f_realloc   (void       *ptr, cx_Integer size );
C void        cx_f_free      (void       *ptr                  );
C cx_Real     cx_f_atof      (cx_String   str                  );
C cx_Integer  cx_f_atoi      (cx_String   str                  );
C cx_IOStream*cx_f_fopen     (cx_String   fn,  cx_String perm  );
C cx_Integer  cx_f_strlen    (cx_String   str                  );
C cx_String   cx_f_strdup    (cx_String   str                  );
C cx_String   cx_f_strndup   (cx_String   str, cx_Integer lens );
C cx_String   cx_f_strcat    (cx_String   s1,  cx_String  s2   );
C cx_Integer  cx_f_strcmp    (cx_String   s1,  cx_String  s2   );
C cx_Integer  cx_f_strncmp   (cx_String   s1,  cx_String  s2, cx_Integer lens);
C cx_String   cx_f_strqbrk   (cx_String   str, cx_String  sc   );
C cx_String   cx_f_scratchpad(cx_String   str                  );
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C  These need documentation fixes
C     cx_e_stringvalue()
C     cx_e_binaryvalue()
C     cx_e_typename()
C     cx_e_type2typename()
C     CHARACTER*(*) cx_e_prop_name()
C     CHARACTER*(*) cx_e_sprop()
C     CHARACTER*(*) cx_e_realformat()
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  cx_f_molecule.h -- CX molecule definitions and wrappers for Fortran
C
C  cx.h should be included before this file.
C-----------------------------------------------------------------------
C  Author and institution: Dave Weininger, Daylight CIS, Inc.
C
C  This source code is contributed to the public domain and may be
C  freely copied and redistributed for research, profit, fun or any
C  other reason, with these restrictions: (1) unmodified or
C  functionally equivalent code derived from CX code must contain this
C  notice, (2) all derived code must acknowledge the author and
C  institution, and (3) the functional definition of symbols starting
C  CX_ or cx_ may not be changed (if you need to change a function,
C  CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C  Molecule property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_INSMI
      PARAMETER ( CX_PROP_INSMI = 'input smiles' )
C-----------------------------------------------------------------------
C  Atom and Bond property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_INORD, CX_PROP_VISIT
      PARAMETER ( CX_PROP_INORD = 'input order' )
      PARAMETER ( CX_PROP_VISIT = 'visit'       )
C-----------------------------------------------------------------------
C  Atom property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_ATNUMB, CX_PROP_ATSYMB, CX_PROP_ALABEL,
     &              CX_PROP_IMPH,   CX_PROP_MASS,   CX_PROP_CHARGE,
     &              CX_PROP_CHIRAL
      PARAMETER ( CX_PROP_ATNUMB = 'atomic number'   )
      PARAMETER ( CX_PROP_ATSYMB = 'atomic symbol'   )
      PARAMETER ( CX_PROP_ALABEL = 'atom label'      )
      PARAMETER ( CX_PROP_IMPH   = 'implicit hcount' )
      PARAMETER ( CX_PROP_MASS   = 'mass'            )
      PARAMETER ( CX_PROP_CHARGE = 'charge'          )
      PARAMETER ( CX_PROP_CHIRAL = 'chirality'       )
C-----------------------------------------------------------------------
C  Bond property names
C-----------------------------------------------------------------------
      CHARACTER*(*) CX_PROP_BORDER, CX_PROP_BSYMB, CX_PROP_DBO,
     &              CX_PROP_BLABEL
      PARAMETER ( CX_PROP_BORDER = 'bond order'  )
      PARAMETER ( CX_PROP_BSYMB  = 'bond symbol' )
      PARAMETER ( CX_PROP_DBO    = 'dbo'         )
      PARAMETER ( CX_PROP_BLABEL = 'bond label'  )
C-----------------------------------------------------------------------
C  Chiral classes:
C    TetraHedral ALlene-like SquarePlanar TrigonalBiyramidal OctaHedral
C-----------------------------------------------------------------------
      INTEGER*4 CX_CHI_NONE, CX_CHI_TH, CX_CHI_AL,
     &          CX_CHI_SP,   CX_CHI_TB, CX_CHI_OH

      PARAMETER ( CX_CHI_NONE =  0 )
      PARAMETER ( CX_CHI_TH   =  3 )
      PARAMETER ( CX_CHI_AL   =  4 )
      PARAMETER ( CX_CHI_SP   = 16 )
      PARAMETER ( CX_CHI_TB   = 17 )
      PARAMETER ( CX_CHI_OH   = 18 )
C-----------------------------------------------------------------------
C  Symbolic names for double bond orientations
C-----------------------------------------------------------------------
      INTEGER*4 CX_CHI_NO_DBO, CX_CHI_CIS, CX_CHI_TRANS, CX_CHI_UP,
     &          CX_CHI_DOWN, CX_CHI_FORWARD, CX_CHI_REVERSE
      PARAMETER ( CX_CHI_NO_DBO  = 0 )
      PARAMETER ( CX_CHI_CIS     = 1 )
      PARAMETER ( CX_CHI_TRANS   = 2 )
      PARAMETER ( CX_CHI_UP      = 3 )
      PARAMETER ( CX_CHI_DOWN    = 4 )
      PARAMETER ( CX_CHI_FORWARD = 5 )
      PARAMETER ( CX_CHI_REVERSE = 6 )
C-----------------------------------------------------------------------
C  Fortran wrappers for public molecule functions
C
C  Function declarations take room in Fortran, so these are commented
C  out here: declare them in program units where they are used.
C  Note: subroutines (void entry points) are not declared in Fortran.
C-----------------------------------------------------------------------
C     INTEGER*4 cx_f_atomtuple_name
C     INTEGER*4 cx_f_bond
C     INTEGER*4 cx_f_bondtuple_name
C     INTEGER*4 cx_f_create_atom
C     INTEGER*4 cx_f_create_atomtuple
C     INTEGER*4 cx_f_create_bond
C     INTEGER*4 cx_f_create_bondtuple
C     INTEGER*4 cx_f_create_molecule
C     INTEGER*4 cx_f_mol_copy
C     INTEGER*4 cx_f_mol_heavycopy
C     INTEGER*4 cx_f_molecule_pkg
C     INTEGER*4 cx_f_prefix2atuples
C     INTEGER*4 cx_f_smilin
C     INTEGER*4 cx_f_xatom
C-----------------------------------------------------------------------
C  These require documentation fixes
C     CHARACTER*(*) cx_f_atomtuple_name
C     CHARACTER*(*) cx_f_bondtuple_name
C-----------------------------------------------------------------------
#endif
