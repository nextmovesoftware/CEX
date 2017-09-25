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
      INTEGER*4 CX_PATH_MAX
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
C  Symbolic names for surface object classes
C-----------------------------------------------------------------------
      INTEGER*4 CX_OB_SURFACE,     CX_OB_VERTEX, CX_OB_FACE,
     &          CX_OB_VERTEXTUPLE, CX_OB_FACETUPLE
      PARAMETER (CX_OB_SURFACE         = 30 )
      PARAMETER (CX_OB_VERTEX          = 31 )
      PARAMETER (CX_OB_FACE            = 32 )
      PARAMETER (CX_OB_VERTEXTUPLE     = 33 )
      PARAMETER (CX_OB_FACETUPLE       = 34 )
C-----------------------------------------------------------------------
C  Symbolic names for camera object classes
C-----------------------------------------------------------------------
      INTEGER*4 CU_OB_CAMERA
      PARAMETER ( CX_OB_CAMERA       = 476041500 )
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
C  Flags used by cx_create_iostream
C-----------------------------------------------------------------------
      INTEGER*4 CX_IO_READ, CX_IO_WRITE
      PARAMETER ( CX_IO_READ  = 0 )
      PARAMETER ( CX_IO_WRITE = 1 )
C-----------------------------------------------------------------------
C  Subroutines (void functions) are not declared in Fortran, but are
C  listed here for convenience
C-----------------------------------------------------------------------
      INTEGER*4 CX_F_ANCESTOR
      INTEGER*4 CX_F_APPEND
      INTEGER*4 CX_F_ATEND
      REAL*8    CX_F_ATOF
      INTEGER*4 CX_F_ATOI
      INTEGER*4 CX_F_BINARYVALUE
      INTEGER*4 CX_F_CEX_EOF
      INTEGER*4 CX_F_CEX_LISTFMT
C     (CALL)    CX_F_CLEANUP
      INTEGER*4 CX_F_COUNT
      INTEGER*4 CX_F_CREATE_BINARY
      INTEGER*4 CX_F_CREATE_DATATYPE
      INTEGER*4 CX_F_CREATE_DATATYPETABLE
      INTEGER*4 CX_F_CREATE_IOSTREAM
      INTEGER*4 CX_F_CREATE_SEQUENCE
      INTEGER*4 CX_F_CREATE_STRING
      INTEGER*4 CX_F_DATATYPE
      INTEGER*4 CX_F_DEFAULT_DATATYPETABLE
      INTEGER*4 CX_F_DELETE
C     (CALL)    CX_F_DESTROY
      INTEGER*4 CX_F_ERROR_COUNT
      INTEGER*4 CX_F_ERROR_SAVE
      INTEGER*4 CX_F_ERROR_SPEW
      INTEGER*4 CX_F_ERRORQUEUE
      INTEGER*4 CX_F_IPROP
      INTEGER*4 CX_F_NEXT
      INTEGER*4 CX_F_PARENT
      INTEGER*4 CX_F_PARSE_BINARY
      INTEGER*4 CX_F_PNAME2DATATYPE
      INTEGER*4 CX_F_PREFIX2PROPS
      INTEGER*4 CX_F_PROP_NAME
      INTEGER*4 CX_F_REALFORMAT
      INTEGER*4 CX_F_RESET
      REAL*8    CX_F_RPROP
C     (CALL)    CX_F_CEX_SET_LISTFMT
      INTEGER*4 CX_F_SET_DATATYPE
      INTEGER*4 CX_F_SET_IPROP
      INTEGER*4 CX_F_SET_METHOD
      INTEGER*4 CX_F_SET_PARENT
C     (CALL)    CX_F_SET_REALFORMAT
      INTEGER*4 CX_F_SET_RPROP
      INTEGER*4 CX_F_SET_SPROP
C     (CALL)    CX_F_SPEWOB
      INTEGER*4 CX_F_SPROP
      INTEGER*4 CX_F_STDIN
      INTEGER*4 CX_F_STDOUT
      INTEGER*4 CX_F_STDERR
      INTEGER*4 CX_F_STREAM
      INTEGER*4 CX_F_STRINGVALUE
      INTEGER*4 CX_F_TAG2DATATYPE
      INTEGER*4 CX_F_TYPE
      INTEGER*4 CX_F_TYPE2TYPENAME
      INTEGER*4 CX_F_TYPENAME
      INTEGER*4 CX_F_TYPENAME2TYPE
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C  Don't quite know what do to with utilities yet, most can't be called
C  from Fortran...
C-----------------------------------------------------------------------
C Yes
C cx_Real     cx_f_atof      (cx_String   str                  );
C cx_Integer  cx_f_atoi      (cx_String   str                  );
C-----------------------------------------------------------------------
C void        cx_f_panic     (cx_String   msg, cx_String  src  );
C void       *cx_f_malloc    (cx_Integer  size                 );
C void       *cx_f_realloc   (void       *ptr, cx_Integer size );
C void        cx_f_free      (void       *ptr                  );
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
