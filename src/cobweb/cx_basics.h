/*****************************************************************************
*  cx_basics.h -- CX basic definitions
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Dave Weininger, Daylight CIS, Inc.
*
*  This source code is contributed to the public domain and may be freely
*  copied and redistributed for research, profit, fun or any other reason,
*  with these restrictions: (1) unmodified or functionally equivalent code
*  derived from CX code must contain this notice, (2) all derived code must
*  acknowledge the author and institution, and (3) the functional definition
*  of symbols starting CX_ or cx_ may not be changed (if you need to change
*  a function, CHANGE THE NAME: prefixes CU_ and cu_ are suggested).
*****************************************************************************/

#ifndef CX_BASICS_INCLUDED
#define CX_BASICS_INCLUDED 1

#include "cx_types.h"


/*** Constants. ***/

#define FALSE  0
#define TRUE   1


/*** Handy definitions. ***/

#define CX_ABS(n)    ((n) > 0 ? (n) : -(n))
#define CX_MIN(a, b) ((a) < (b) ? (a) : (b))
#define CX_MAX(a, b) ((a) > (b) ? (a) : (b))


/*** Base polymorph functions. ***/

cx_Object  cx_e_base_create      (cx_Object par, cx_Integer type );
void       cx_e_base_destroy     (cx_Object ob                   );
void      *cx_e_base_content     (cx_Object ob                     );
cx_Object  cx_e_base_datatype    (cx_Object ob                     );
cx_Integer cx_e_base_set_content (cx_Object ob, void *content      );
cx_Integer cx_e_base_set_datatype(cx_Object ob, cx_Object dt       );

cx_Object  cx_e_child            (cx_Object ob);
cx_Object  cx_e_sibling          (cx_Object ob);
cx_Object  cx_e_child_safe       (cx_Object ob);
cx_Object  cx_e_sibling_safe     (cx_Object ob);
cx_Integer cx_e_type_safe        (cx_Object ob);


/*** Non-public polymorphic functions ***/

cx_Object  cx_e_cexin      (cx_String cex,   cx_Integer type, cx_Object table);
cx_Object  cx_e_setproperty(cx_Object ob,    cx_String pname,  cx_String pval);
cx_Integer cx_e_send       (cx_Object ob,  cx_Object table,  cx_Object outs  );


/*** Public polymorphic functions ***/

cx_Integer cx_e_count      (cx_Object ob,  cx_Integer type                   );
void       cx_e_destroy    (cx_Object ob                                     );
cx_Integer cx_e_reset      (cx_Object ob                                     );
cx_Object  cx_e_stream     (cx_Object ob,  cx_Integer stype                  );
cx_String  cx_e_stringvalue(cx_Object ob                                     );
cx_Binary  cx_e_binaryvalue(cx_Integer *plen, cx_Object ob                   );


/*** Other public functions ***/

cx_Object  cx_e_ancestor     (cx_Object  ob,   cx_Integer type               );
void       cx_e_cleanup      (void                                           );
cx_Object  cx_e_parent       (cx_Object  ob                                  );
cx_Integer cx_e_set_parent   (cx_Object  ob,   cx_Object  parent             );
cx_Integer cx_e_set_typename (cx_Integer type, cx_String  typename           );
cx_Integer cx_e_type         (cx_Object  ob                                  );
cx_String  cx_e_typename     (cx_Object  ob                                  );
cx_Integer cx_e_typename2type(cx_String  typename                            );
cx_String  cx_e_type2typename(cx_Integer type                                );
char      *cx_e_version      (void                                           );


/*** C-wrappers for all public functions ***/

#define cx_ancestor       cx_e_ancestor
#define cx_binaryvalue    cx_e_binaryvalue
#define cx_cleanup        cx_e_cleanup
#define cx_count          cx_e_count
#define cx_destroy        cx_e_destroy
#define cx_parent         cx_e_parent
#define cx_reset          cx_e_reset
#define cx_set_parent     cx_e_set_parent
#define cx_stream         cx_e_stream
#define cx_stringvalue    cx_e_stringvalue
#define cx_type           cx_e_type
#define cx_type2typename  cx_e_type2typename
#define cx_typename       cx_e_typename
#define cx_typename2type  cx_e_typename2type
#define cx_version        cx_e_version

/*** C-wrapper and declaration for method registration is special. ***/
 
#define cx_set_method(type,meth,func) \
	cx_e_set_method(type,meth,(cx_Method) func)
	  
cx_Integer cx_e_set_method(cx_Integer type, cx_String meth, cx_Method gfunc);


#endif /* CX_BASICS_INCLUDED */

