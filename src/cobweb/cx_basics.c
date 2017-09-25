/*****************************************************************************
*  cx_basics.c -- support for basic CX types
*
*----------------------------------------------------------------------------
*  To do:
*  [ ] provide means of blowing away classes
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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "cx_cobweb.h"

/* cx_e_iostream_cleanup */
#include "cx_iostream.h"


/*** Class struct contains ALL class-specific info (mainly methods). ***/

typedef cx_Object  (*cx_CexInMethod)(cx_String,cx_Object);
typedef cx_Integer (*cx_CountMethod)(cx_Object,cx_Integer);
typedef void       (*cx_DestroyMethod)(cx_Object);
typedef cx_Integer (*cx_ResetMethod)(cx_Object);
typedef cx_Integer (*cx_SendMethod)(cx_Object,cx_Object,cx_Object);
typedef cx_Object  (*cx_SetPropertyMethod)(cx_Object,cx_String,cx_String);
typedef cx_Object  (*cx_StreamMethod)(cx_Object,cx_Integer);
typedef cx_Object  (*cx_StringValueMethod)(cx_Object);
typedef cx_Binary  (*cx_BinaryValueMethod)(cx_Integer*,cx_Object);


typedef struct _ci_class {
   cx_Integer type;
   cx_String  typename;
   cx_CexInMethod       cexin;
   cx_CountMethod       count;
   cx_DestroyMethod     destroy;
   cx_ResetMethod       reset;
   cx_SendMethod        send;
   cx_SetPropertyMethod setproperty;
   cx_StreamMethod      stream;
   cx_StringValueMethod stringvalue;
   cx_BinaryValueMethod binaryvalue;
   struct _ci_class  *nextclass;
} CI_CLASS;
 
/*** Static list of classes. ***/
 
static CI_CLASS *class_list = NULL;

/*** Base object contains class, header and pointer to content. ***/

typedef struct {
   CI_CLASS  *class;                 /* object class info */
   cx_Object  datatype;              /* datatype */
   cx_Object  parent;                /* parent object or NULL */
   cx_Object  children;              /* head of children queue */
   cx_Object  baby;                  /* tail of children queue */
   cx_Object  sibling;               /* next in linked list of children */
   void      *content;               /* object-specific info */
} CI_BASE;


/*============================================================================
 *  type2class() -- return pointer to CI_CLASS struct for given type
 *
 *  NULL is returned on failure.
 */

static CI_CLASS *type2class(cx_Integer type)
{
    register CI_CLASS *class;

    for( class = class_list; class; class = class->nextclass )
        if( class->type == type ) return class;
    return (CI_CLASS*)0;
}


/*============================================================================
 *  getclass() -- return pointer to fresh CI_CLASS struct, or NULL on error
 */

static CI_CLASS *getclass(cx_Integer type)
{
    register CI_CLASS *class;

    for( class = class_list; class; class = class->nextclass )
        if( class->type == type ) return class;

    class = (CI_CLASS *)malloc(sizeof(CI_CLASS));
    if( class == (CI_CLASS*)0 ) return (CI_CLASS*)0;

    class->type        = type;
    class->typename    = (cx_String)0;
    class->cexin       = NULL;
    class->count       = NULL;
    class->destroy     = NULL;
    class->reset       = NULL;
    class->send        = NULL;
    class->setproperty = NULL;
    class->stream      = NULL;
    class->stringvalue = NULL;
    class->binaryvalue = NULL;

    /*** Add to linked list of known classes. ***/
    class->nextclass = class_list;
    class_list       = class;

    /*** Return new methods struct. ***/

    return class;
}

/*============================================================================
 *  cx_typename2type() -- public function accesses class_list
 *
 *  CX_OB_INVALID is returned on failure (not found or typename is NULL).
 */

cx_Integer cx_e_typename2type(cx_String typename)
{
    register CI_CLASS *class;
    register char ch;

    /*** Validate. ***/
    if( !typename ) return CX_OB_INVALID;

    /*** Look through linked list of clasess. ***/
    ch = typename[0];
    for( class=class_list; class; class=class->nextclass )
        if( class->typename && class->typename[0]==ch &&
            !strcmp(class->typename,typename) )
            return class->type;
   
    /*** Return CX_OB_INVALID if not found. ***/
    return CX_OB_INVALID;
}


/*============================================================================
 *  cx_e_type2typename() -- public function accesses class_list
 *
 *  NULL is returned on failure (class not found).
 */

cx_String cx_e_type2typename(cx_Integer type)
{
    register CI_CLASS *class;

    /*** Look through linked list of clasess. ***/
    for( class = class_list; class; class=class->nextclass )
        if( class->type == type ) return class->typename;

    /*** Return NULL if not found. ***/
    return (cx_String)0;
}


/*============================================================================
 *  cx_e_type() -- return object type or CX_OB_INVALID if unable
 */

cx_Integer cx_e_type(cx_Object ob)
{
    register CI_CLASS *class;
    register CI_BASE  *base;

    base = (CI_BASE*)ob;
    if( !base ) return CX_OB_INVALID;

    class = base->class;
    if( !class ) return CX_OB_INVALID;
    return class->type;
}


/*============================================================================
 *  cx_e_type_safe() -- return object type or CX_OB_INVALID if unable
 */

cx_Integer cx_e_type_safe(cx_Object ob)
{
    register CI_CLASS *class;

    class = ((CI_BASE*)ob)->class;
    return class? class->type : CX_OB_INVALID;
}


/*============================================================================
 *  cx_e_typename() -- return object typename or "Invalid" if unable
 */

cx_String cx_e_typename(cx_Object ob)
{
    register CI_CLASS *class;
    register CI_BASE  *base;

    base = (CI_BASE*)ob;
    if( !base ) return "Invalid";

    class = base->class;
    if( !class ) return "Invalid";
    return class->typename;
}


/*============================================================================
 *  cx_e_set_typename() -- set class' typename, return success
 */

cx_Integer cx_e_set_typename(cx_Integer type, cx_String typename)
{
    register CI_CLASS *class;

    /*** Validate. ***/
    if( type==CX_OB_INVALID || type==CX_OB_ANY )
        return FALSE;

    /*** Look up class; if not found, make one. ***/
    class = getclass(type);
    if( !class ) return FALSE;
   
    /*** Record typename and return successfully. ***/
    if( class->typename )
        free(class->typename);
    class->typename = cx_strdup(typename);
    return TRUE;
}

/*============================================================================
 *  cx_e_set_method() -- register/create methods
 *
 *  Known method types and functionalities:
 *
 *    "cexin" ........ parse root object from CEX-datatree
 *    "count" ........ return count of given class over given object
 *    "destroy" ...... destroy given object and children
 *    "reset" ........ reset stream, sequence, or datatypetable
 *    "send" ......... write object to cex stream
 *    "setproperty" .. create/set stringvalued property
 *    "stream" ....... return stream of given class over given object
 *    "stringvalue" .. return stringvalue of given object
 *    "binaryvalue" .. return binaryvalue of given object
 *
 *  NULL is returned on error (meth is unknown, out of memory).
 */

cx_Integer cx_e_set_method(cx_Integer type, cx_String meth, cx_Method gfunc)
{
    register CI_CLASS *class;
    register char ch;

    /*** Validate. ***/
    if( !meth || type==CX_OB_INVALID || type==CX_OB_ANY )
        return FALSE;

    /*** Look up class; if not found, make one. ***/
    class = getclass(type);
    if( !class ) return FALSE;

    /*** Set appropriate method. ***/
    ch = meth[0];
    if( ch == 's' ) {
        ch = meth[1];
        if( ch == 'e' ) {
            if( !strcmp(meth,"send") ) {
                class->send = (cx_SendMethod)gfunc;
                return TRUE;
            } else if( !strcmp(meth,"setproperty") ) {
                class->setproperty = (cx_SetPropertyMethod)gfunc;
                return TRUE;
            }
        } else if( ch == 't' ) {
            if( !strcmp(meth,"stream") ) {
                class->stream = (cx_StreamMethod)gfunc;
                return TRUE;
            } else if( !strcmp(meth,"stringvalue") ) {
                class->stringvalue = (cx_StringValueMethod)gfunc;
                return TRUE;
            }
        }
    } else if( ch == 'c' ) {
        if( !strcmp(meth,"cexin") ) {
            class->cexin = (cx_CexInMethod)gfunc;
            return TRUE;
        } else if( !strcmp(meth,"count") ) {
            class->count = (cx_CountMethod)gfunc;
            return TRUE;
        }
    } else if( ch == 'd' ) {
        if( !strcmp(meth,"destroy") ) {
            class->destroy = (cx_DestroyMethod)gfunc;
            return TRUE;
        }
    } else if( ch == 'r' ) {
        if( !strcmp(meth,"reset") ) {
            class->reset = (cx_ResetMethod)gfunc;
            return TRUE;
        }
    } else if( ch == 'b' ) {
        if( !strcmp(meth,"binaryvalue") ) {
            class->binaryvalue = (cx_BinaryValueMethod)gfunc;
            return TRUE;
        }
    }
    return FALSE;
}

/*============================================================================
 *  cx_e_cexin() -- polymorphic cex-reader returns root object or NULL
 *
 *  If `table' is NULL, the default datatype table is used.
 */

cx_Object cx_e_cexin(cx_String cex, cx_Integer type, cx_Object table)
{
    register CI_CLASS *class;

    class = type2class(type);
    if( !class || !class->cexin )
        return (cx_Object)0;
    return class->cexin(cex, (table ? table : cx_default_datatypetable()));
}


/*============================================================================
 *  cx_e_count() -- polymorphic counter returns NULL on error
 */

cx_Integer cx_e_count(cx_Object ob, cx_Integer type)
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return 0;

    class = base->class;
    if( !class || !class->count )
        return 0;
    return class->count(ob, type);
}


/*============================================================================
 *  cx_e_destroy() -- polymorphic object destruction is a no-op on error
 */

void cx_e_destroy(cx_Object ob)
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return;

    class = base->class;
    if( class && class->destroy )
        class->destroy(ob);
}


/*============================================================================
 *  cx_e_reset() -- polymorphic reset is a no-op returning FALSE on error
 */

cx_Integer cx_e_reset(cx_Object ob)
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return FALSE;

    class = base->class;
    if( !class || !class->reset )
        return FALSE;
    return class->reset(ob);
}

/*============================================================================
 *  cx_e_send() -- polymorphic cex-writer returns success
 *
 *  If `table' is NULL, the default datatype table is used.
 */

cx_Integer cx_e_send(cx_Object ob, cx_Object table, cx_Object fp )
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return FALSE;

    class = base->class;
    if( !class || !class->send )
        return FALSE;

    return class->send(ob, (table?table:cx_default_datatypetable()), fp);
}


/*============================================================================
 *  cx_e_setproperty() -- polymorphic setproperty returns NULL on error
 */

cx_Object cx_e_setproperty(cx_Object ob, cx_String pname, cx_String pval)
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_Object)0;

    class = base->class;
    if( !class || !class->setproperty )
        return (cx_Object)0;
    return class->setproperty(ob, pname, pval);
}


/*============================================================================
 *  cx_e_stream() -- polymorphic stream generator returns NULL on error
 */

cx_Object cx_e_stream(cx_Object ob, cx_Integer type)
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_Object)0;

    class = base->class;
    if( !class || !class->stream )
        return (cx_Object)0;
    return class->stream(ob, type);
}


/*============================================================================
 *  cx_e_stringvalue() -- polymorphic stringvalue returns NULL on error
 */

cx_String cx_e_stringvalue(cx_Object ob)
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_String)0;

    class = base->class;
    if( !class || !class->stringvalue )
        return (cx_String)0;
    return class->stringvalue(ob);
}


/*============================================================================
 *  cx_e_binaryvalue() -- polymorphic binaryvalue returns NULL on error
 */

cx_Binary cx_e_binaryvalue(cx_Integer *plen, cx_Object ob)
{
    register CI_CLASS *class;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_Binary)0;

    class = base->class;
    if( !class || !class->binaryvalue )
        return (cx_Binary)0;
    return class->binaryvalue(plen, ob);
}


/*============================================================================
 *  cx_e_child() -- return object's (1st) child or NULL if unable
 */

cx_Object cx_e_child(cx_Object ob)
{
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_Object)0;
    return base->children;
}


/*============================================================================
 *  cx_e_child_safe() -- return object's (1st) child or NULL if unable
 */

cx_Object cx_e_child_safe(cx_Object ob)
{
    return ((CI_BASE*)ob)->children;
}


/*============================================================================
 *  cx_e_sibling() -- return object's sibling or NULL if unable
 */

cx_Object cx_e_sibling(cx_Object ob)
{
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_Object)0;
    return ((CI_BASE*)ob)->sibling;
}


/*============================================================================
 *  cx_e_sibling_safe() -- return object's sibling or NULL if unable
 */

cx_Object cx_e_sibling_safe(cx_Object ob)
{
    return ((CI_BASE*)ob)->sibling;
}


/*============================================================================
 *  cx_e_parent() -- return object's parent or NULL if unable
 */

cx_Object cx_e_parent(cx_Object ob)
{
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_Object)0;
    return base->parent;
}


/*============================================================================
 *  cx_e_set_parent() -- set object's parent object, return success
 *
 *  This unlinks child from parent if object's parent is not NULL, allowing
 *  reparenting (setting new parent) and unparenting (setting NULL parent).
 *  Normally an object should be unparented before being destroyed.
 */

cx_Integer cx_e_set_parent(cx_Object ob, cx_Object parent)
{
    register CI_BASE *base;
    register CI_BASE *pbase;
    register CI_BASE *child;
    register CI_BASE *babe;

    /*** Validate ***/
    base = (CI_BASE*)ob;
    if( !base ) return FALSE;

    /*** Return successfully if extant parent is same as new one. ***/
    if( base->parent == parent ) return TRUE;

    /*** Remove ob from parent's childlist if needed. ***/
    pbase = (CI_BASE*)base->parent;
    if( pbase ) {
        if( pbase->children == ob ) {
            pbase->children = base->sibling;
            if( !pbase->children )
                pbase->baby = (cx_Object)0;
        } else {
            child = (CI_BASE*)pbase->children;
            while( child->sibling != ob )
                child = (CI_BASE*)child->sibling;
            child->sibling = base->sibling;
            if( !child->sibling )
                pbase->baby = (cx_Object)child;
        }
    }

    /*** Set parent, possibly to NULL. ***/
    base->sibling = (cx_Object)0;
    base->parent = parent;

    /*** If not NULL, add child to parent's children. ***/
    pbase = (CI_BASE*)parent;
    if( pbase ) {
        /*** If first child, make head of child list. ***/
        if( !pbase->children) {
            pbase->children = ob;
            pbase->baby     = ob;

        /*** Else append to end of child list. ***/
        } else {
	    babe = (CI_BASE*)pbase->baby;
	    babe->sibling = ob;
	    pbase->baby   = ob;
        }
   }

   /*** Return successfully. ***/
   return TRUE;
}


/*============================================================================
 *  cx_e_ancestor() -- return object's ancestor of given type or NULL
 *
 *  If desired type is CX_OB_ANY, returns cx_parent(ob)
 *  If desired type is CX_OB_INVALID, returns ultimate ancestor.
 *  If object `ob' is of desired type, returns ob.
 */

cx_Object cx_e_ancestor(cx_Object ob, cx_Integer type)
{
    register CI_BASE *ancestor;

    /*** Validate. ***/
    ancestor = (CI_BASE*)ob;
    if( !ancestor ) return (cx_Object)0;

    if( type == CX_OB_ANY ) {
        return ancestor->parent;
    } else if( type == CX_OB_INVALID ) {
        while( ancestor->parent )
            ancestor = (CI_BASE*)ancestor->parent;
        return (cx_Object)ancestor;
    }

    do {
        if( ancestor->class && ancestor->class->type == type )
            return (cx_Object)ancestor;
        ancestor = (CI_BASE*)ancestor->parent;
    } while( ancestor );
    return (cx_Object)0;
}


/*============================================================================
 *  cx_e_base_content() -- return object's content or NULL if unable
 */

void *cx_e_base_content(cx_Object ob)
{
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (void*)0;
    return base->content;
}


/*============================================================================
 *  cx_e_base_set_content() -- set object's content, return success
 */

cx_Integer cx_e_base_set_content(cx_Object ob, void *content)
{
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return FALSE;
    base->content = content;
    return TRUE;
}


/*============================================================================
 *  cx_e_base_datatype() -- return object's datatype or NULL if unable
 */

cx_Object cx_e_base_datatype(cx_Object ob)
{
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return (cx_Object)0;
    return base->datatype;
}


/*============================================================================
 *  cx_e_base_set_datatype() -- set object's datatype, return success
 */

cx_Integer cx_e_base_set_datatype(cx_Object ob, cx_Object dt)
{
    register CI_CLASS *class;
    register CI_BASE *dtbase;
    register CI_BASE *base;

    base = (CI_BASE*)ob;
    if( !base ) return FALSE;

    dtbase = (CI_BASE*)dt;
    if( dtbase ) {
        class = dtbase->class;
        if( !class || class->type != CX_OB_DATATYPE )
            return FALSE;
    }
    base->datatype = dt;
    return TRUE;
}


/*============================================================================
 *  cx_e_base_create() -- create new base object
 */

cx_Object cx_e_base_create(cx_Object parent, cx_Integer type)
{
    register CI_BASE *pbase;
    register CI_BASE *babe;
    register CI_BASE *base;

    /*** Allocate new base object, return NULL if unable. ***/
    base = (CI_BASE*)malloc(sizeof(CI_BASE));
    if( !base ) return (cx_Object)0;

    /*** Initialize as empty object of given typename. ***/
    base->class    = type2class(type);
    base->datatype = (cx_Object)0;
    base->children = (cx_Object)0;
    base->baby     = (cx_Object)0;
    base->sibling  = (cx_Object)0;
    base->content  = (void*)0;
    base->parent   = parent;

    /*** Update parent. ***/
    pbase = (CI_BASE*)parent;
    if( pbase ) {
        /*** If first child, make head of child list. ***/
        if( !pbase->children) {
            pbase->children = (cx_Object)base;
            pbase->baby     = (cx_Object)base;

        /*** Else append to end of child list. ***/
        } else {
            babe = (CI_BASE*)pbase->baby;
            babe->sibling = (cx_Object)base;
            pbase->baby   = (cx_Object)base;
        }
   }

   /*** Return initialized base object. ***/
   return (cx_Object)base;
}


/*============================================================================
 *  cx_e_base_destroy() -- destroy base object (but not content) and children
 */

void cx_e_base_destroy(cx_Object ob)
{
    register CI_BASE *pbase;
    register CI_BASE *base;
    register CI_BASE *sib;

    /*** Check object, return if NULL. ***/
    base = (CI_BASE*)ob;
    if( !base ) return;

    /*** Destroy object's children. ***/
    while( base->children )
        cx_destroy(base->children);

    /*** Unlink object from parent. ***/
    pbase = (CI_BASE*)base->parent;
    if( pbase ) {
        if( pbase->children == ob ) {
            pbase->children = base->sibling;
            if( !pbase->children )
                pbase->baby = (cx_Object)0;
        } else {
            sib = (CI_BASE*)pbase->children;
            while( sib->sibling != ob )
                sib = (CI_BASE*)sib->sibling;
            sib->sibling = base->sibling;
            if( !sib->sibling )
                pbase->baby = (cx_Object)sib;
        }
    }

    /*** Free object-specific storage. ***/
    free(base);
}


/*============================================================================
 *  cx_e_cleanup() -- deinitialize cx stuff
 */

void cx_e_cleanup(void)
{
    /*** Deallocate scratchpad. ***/
    cx_scratchpad(NULL);
    cx_scratchpad(NULL);

    /*** Deallocate cx_e_cex_read() buffer. ***/
    cx_e_cex_read(NULL);

    /*** This shouldn't be here! ***/
    cx_e_iostream_cleanup();

    /*** Deregister root types. ***/
    cx_e_deregister_root_types();

    /*** Destroy default datatype table. ***/
    cx_destroy(cx_default_datatypetable());
}

