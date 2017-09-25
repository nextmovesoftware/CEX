/*****************************************************************************
*  cx_property.c -- support for CX property objects
*
*----------------------------------------------------------------------------
*
*  Properties are strings with "stringvalue" and "property name" properties.
*
*  The low-level interface consists of:
*
*     cx_e_base_setproperty(ob, pname, pval) => property object
*     cx_e_property(ob, pname)               => property object
*     cx_prop_name(prop)                     => property name
*     cx_stringvalue(prop)                   => property value
*     cx_destroy(ob)                         => void
*
*  The low-level CX properties are very flexible and powerful but somewhat
*  cumbersome to use, e.g., to get the value of a property as an integer,
*  one needs to get property then get its stringvalue then interpret it.
*
*  This file also provides an interface to CX properties that is much simpler
*  to use, although limited in flexibility.  Specifically:
*
*  There can be at most one property of a given name per parent.  Speciically,
*
*     o  All object properties are ultimately string objects.
*
*     o  All object properties are owned by the propertied object
*        (therefore they can't be shared between objects).
*
*     o  Access is provided only to the property value (not to the object).
*
*  All functions are compatible with CX-level properties and each other.
*  For instance, one might use cx_sprop() to determine if a property
*  exists then cx_rprop() to retrive that property's real value.
*
*  cx_String cx_sprop(cx_Object ob, cx_String pname)
*
*     Get object's string-valued property with given property name `pname'.
*     Returns NULL if property is not found or if either argument is NULL.
*
*  cx_Integer cx_set_sprop(cx_Object ob, cx_String pname, cx_String str);
*
*     Destroys property `pname' of object `ob' if extant.  Then, if `str'
*     is not NULL, creates a new string property with value `str'.
*     Returns TRUE if successful, FALSE if not (e.g., ob or pname are NULL).
*
*  cx_Integer cx_iprop(cx_Object ob, cx_String pname)
*
*     Get object's integer-valued property with given property name `pname'.
*     Returns 0 if property is not found or if either argument is NULL.
*     Limited to the range of a cx_Integer, i.e., -2147483648 to 2147483647.
*
*  cx_Integer cx_set_iprop(cx_Object ob, cx_String pname, cx_Integer ival);
*
*     Destroys property `pname' of object `ob' if extant.  Then creates
*     a new integer property with value `ival'.  Returns success.
*
*  cx_Real cx_rprop(cx_Object ob, cx_String pname)
*
*     Get object's real-valued property with given property name `pname'.
*     Returns 0.0 if property is not found or if either argument is NULL.
*     Limited to the range of a cx_Real, ca. +/- 1.4e-45 to 3.4e+38.
*
*  cx_Integer cx_set_rprop(cx_Object ob, cx_String pname, cx_Real rval);
*
*     Destroys property `pname' of object `ob' if extant.  Then create a
*     a new integer property with value `rval'.  Returns success.
*
*  cx_String cx_realformat(void);
*  void      cx_set_realformat(cx_String fmt);
*
*     Get/set the format that cx_set_rprop() uses to write cx_Real numbers.
*     The format is interpreted as per C's printf(3), and is initially "%g".
*     Calling cx_set_realformat(NULL) resets "%g" and recovers memory.
*
*  Hint: For purposes of data exchange, it is usually best to treat real
*  numbers as strings to preserve known precision, rather than converting
*  them to and from cx_Real numbers (not always possible, unfortunately).
*
*  Hint: Any property can be destroyed with cx_set_sprop(ob, pname, NULL);
*
*
*   cexin ......... no
*   send .......... no
*   destroy ....... yes
*   stringvalue ... yes
*   count ......... yes
*   stream ........ yes
* 
*   cx_strprop (ob, pname);  cx_set_strprop (ob, pname, sval);
*   cx_intprop (ob, pname);  cx_set_intprop (ob, pname, ival);
*   cx_realprop(ob, pname);  cx_set_realprop(ob, pname, rval);
*   cx_binprop (ob, pname);  cx_set_binprop (ob, pname, bval);   ?
*
*----------------------------------------------------------------------------
*
*  Current implementation, not too efficient but flexible and simple:
*
*    o  Properties are normal objects owned by a parent as a normal child
*    o  Property object content is a simple struct containing pname/pval
*    o  Property objects don't form a linked list
*    o  Property objects may be enumerated by cx_stream(ob, CX_OB_PROPERTY)
*
*                  +--------+                         +--------+
*                  |CI_PROP |<-+                      |CI_PROP |<-+
*                  +--------+  |                      +--------+  |
*                  | pname  |  |                      | pname  |  |
*                  +--------+  |                      +--------+  |
*                  | pstr   |  |                      | pstr   |  |
*                  +--------+  |                      +--------+  |
*                              |                                  |
*  +---------+    +----------+ |    +----------+     +----------+ |
*  |cx_Object| ,->|cx_Object | | ,->|cx_Object |  ,->|cx_Object | |
*  +---------+ |  +----------+ | |  +----------+  |  +----------+ |
*  |any class| |  |"Property"| | |  |otherclass|  |  |"Property"| |
*  +---------+ |  +----------+ | |  +----------+  |  +----------+ |
*  |content  | |  |CI_PROP * |-' |  | content  |  |  |CI_PROP * |-'
*  +---------+ |  +----------+   |  +----------+  |  +----------+
*  |children |-'  | sibling  |---'  | sibling  |--'  | sibling  |->
*  +---------+    +----------+      +----------+     +----------+
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
#include <ctype.h>

#include "cx_cobweb.h"


/*** Property struct: named objects. ***/

typedef struct {
    cx_String pname;          /* property name */
    cx_String pval;           /* property value */
} CI_PROP;


/*** Static real format. ***/
static char *realformat = (char*)0;

/*** Are property methods initialized? ***/
static int initialized = FALSE;



/*============================================================================
 *  prop_stringvalue() -- return string value of property
 */
 
static cx_String prop_stringvalue(cx_Object prop)
{
    register CI_PROP *ps;

    ps = (CI_PROP*)cx_e_base_content(prop);
    return ps ? ps->pval : (cx_String)0;
}


/*============================================================================
 *  prop_destroy() -- destroy given property
 */
 
static void prop_destroy(cx_Object prop)
{
    register CI_PROP *ps;

    /*** Extract object content as CI_PROP struct, free strings. ***/
    ps = (CI_PROP*)cx_e_base_content(prop);
    if( ps ) {
        if( ps->pval )
            free(ps->pval);
        free(ps->pname);
        free(ps);
    }

    /*** Destroy base object and any children. ***/
    cx_e_base_destroy(prop);
}


/*============================================================================
 *  property_init() -- initialize property-specific functions
 */
 
static void property_init(void)
{
   /*** Define property object type. ***/
   cx_e_set_typename(CX_OB_PROPERTY,"Property");

   /*** Stream-specific methods for destroy and stringvalue. ***/
   cx_set_method(CX_OB_PROPERTY, "destroy",     prop_destroy    );
   cx_set_method(CX_OB_PROPERTY, "stringvalue", prop_stringvalue);

   /*** Use normal (base) count and stream methods. ***/
   cx_set_method(CX_OB_PROPERTY, "count",       cx_e_base_count      );
   cx_set_method(CX_OB_PROPERTY, "stream",      cx_e_base_stream     );
   cx_set_method(CX_OB_PROPERTY, "setproperty", cx_e_base_setproperty);
 
   initialized = TRUE;
}


/*============================================================================
 *  cx_e_prop_name() -- return name of property object or NULL if unable
 *
 *  Note: this operates only on a "Property" object.
 */

cx_String cx_e_prop_name(cx_Object prop)
{
    register CI_PROP *ps;

    /*** Check that valid property object is provided. ***/
    if( cx_type(prop) != CX_OB_PROPERTY )
        return (cx_String)0;

    /*** Fetch object content. ***/
    ps = (CI_PROP*)cx_e_base_content(prop);
    if( !ps ) return (cx_String)0;

    /*** Return property name. ***/
    return ps->pname;
}


/*============================================================================
 *  cx_e_prefix2props() -- return object's properties w/ name starting prefix
 */

cx_Object cx_e_prefix2props(cx_Object ob, cx_String prefix)
{
    register cx_Object prop;
    register cx_Object seq;
    register CI_PROP *ps;
    register int lens;
    register char ch;

    /*** Validate ***/
    if( !ob ) return (cx_Object)0;

    if( prefix ) {
        lens = strlen(prefix);
    } else lens = 0;

    if( !lens ) {
        /* return cx_stream(ob,CX_OB_PROPERTY); */
        seq = cx_create_sequence((cx_Object)0);

        prop = cx_e_child(ob);
        while( prop ) {
            if( cx_e_type_safe(prop) == CX_OB_PROPERTY )
                cx_e_base_append(seq,prop);
            prop = cx_e_sibling_safe(prop);
        }
        cx_reset(seq);
        return seq;
    }

    /*** Add properties with given prefix to sequence. ***/

    seq = cx_create_sequence((cx_Object)0);

    ch = prefix[0];
    prop = cx_e_child(ob);
    while( prop ) {
        if( cx_e_type_safe(prop) == CX_OB_PROPERTY ) {
            ps = (CI_PROP*)cx_e_base_content(prop);
            if( ps && ps->pname[0]==ch && !strncmp(ps->pname,prefix,lens) )
                cx_e_base_append(seq,prop);
        }
        prop = cx_e_sibling_safe(prop);
    }

    cx_reset(seq);
    return seq;
}


/*============================================================================
 *  cx_e_property() -- return named property of an object or NULL if unable
 *
 *  Note: this returns the property object, not the property value.
 */

cx_Object cx_e_property(cx_Object ob, cx_String pname)
{
    register cx_Object prop;
    register CI_PROP *ps;
    register char ch;

    /*** Initialize property methods if needed. ***/
    if( !initialized ) property_init();

    /*** Validate. ***/
    if( !pname ) return (cx_Object)0;

    /*** Look for property with given name. ***/
    ch = pname[0];
    prop = cx_e_child(ob);
    while( prop ) {
        if( cx_e_type_safe(prop) == CX_OB_PROPERTY ) {
            ps = cx_e_base_content(prop);
            if( ps && ps->pname[0]==ch && !strcmp(ps->pname,pname) )
                return prop;
        }
        prop = cx_e_sibling_safe(prop);
    }

    /*** Return NULL if not found. ***/
    return (cx_Object)0;
}


/*============================================================================
 *  cx_e_property_content() -- return named property base content of an
 *                             object or NULL if unable
 *
 *  Note: this returns the property content, not the property value.
 */

static CI_PROP *cx_e_property_content(cx_Object ob, cx_String pname)
{
    register cx_Object prop;
    register CI_PROP *ps;
    register char ch;

    /*** Initialize property methods if needed. ***/
    if( !initialized ) property_init();

    /*** Validate. ***/
    if( !pname ) return (CI_PROP*)0;

    /*** Look for property with given name. ***/
    ch = pname[0];
    prop = cx_e_child(ob);
    while( prop ) {
        if( cx_e_type_safe(prop) == CX_OB_PROPERTY ) {
            ps = cx_e_base_content(prop);
            if( ps && ps->pname[0]==ch && !strcmp(ps->pname,pname) )
                return ps;
        }
        prop = cx_e_sibling_safe(prop);
    }

    /*** Return NULL if not found. ***/
    return (CI_PROP*)0;
}


/*============================================================================
 *  cx_e_base_setproperty() -- set/create property of an object
 *
 *  Returns property object on success (not really needed), NULL on error.
 *  This is now a polymorphic function (e.g., molecule marks modified).
 */

cx_Object cx_e_base_setproperty(cx_Object ob, cx_String pname, cx_String pval)
{
    register cx_Object prop;
    register CI_PROP *ps;
    register char ch;

    /*** Initialize property methods if needed. ***/
    if( !initialized ) property_init();

    /*** Validate. ***/
    if( !pname ) return (cx_Object)0;

    /*** Look for property with given name. ***/
    ch = pname[0];
    prop = cx_e_child(ob);
    while( prop ) {
        if( cx_e_type_safe(prop) == CX_OB_PROPERTY ) {
            ps = cx_e_base_content(prop);
            if( ps && ps->pname[0]==ch && !strcmp(ps->pname,pname) ) {
                if( ps->pval ) free(ps->pval);
                ps->pval  = cx_strdup(pval);
                return prop;
            }
        }
        prop = cx_e_sibling_safe(prop);
    }

    /*** Require a valid object. ***/
    if( cx_type(ob) == CX_OB_INVALID )
        return (cx_Object)0;

    /*** Create and fill content. ***/
    ps = (CI_PROP*)malloc(sizeof(CI_PROP));
    if( !ps ) return (cx_Object)0;

    ps->pval  = cx_strdup(pval);
    ps->pname = cx_strdup(pname);
    if( !ps->pname ) {
        free(ps);
        return (cx_Object)0;
    }

    /*** If given property is not extant, create one with parent ob. ***/
    prop = cx_e_base_create(ob,CX_OB_PROPERTY);
    if( !prop ) {
        if( ps->pval )
            free(ps->pval);
        free(ps->pname);
        free(ps);
        return (cx_Object)0;
    }
    cx_e_base_set_content(prop, ps);

    /*** Return property object. ***/
    return prop;
}



/*****************************************************************************
*
*   higher level property support follows
*
*****************************************************************************/

/*============================================================================
 *  cx_sprop() -- return property by name as string
 *
 *  Get object's string-valued property with given property name `pname'.
 *  Returns NULL if property is not found or if either argument is NULL.
 */

cx_String cx_e_sprop(cx_Object ob, cx_String pname)
{
    register CI_PROP *ps;

    ps = cx_e_property_content(ob,pname);
    return ps? ps->pval : (cx_String)0;
}


/*============================================================================
 *  cx_set_sprop() -- set (or reset) string property by name
 *
 *  Creates/resets integer property with value `sval'.  Returns property.
 *
 *  Note: synonymous with cx_e_setproperty() in this implementation.
 *  Note: cx_set_sprop currently directly invokes cx_e_setpropty!
 */

cx_Object cx_e_set_sprop(cx_Object ob, cx_String pname, cx_String sval)
{
   return cx_e_setproperty(ob, pname, sval);
}


/*============================================================================
 *  cx_iprop() -- return property by name as an integer
 *
 *  Get object's integer-valued property with given property name `pname'.
 *  Returns 0 if property is not found or if either argument is NULL.
 *  Limited to the range of a cx_Integer, i.e., -2147483648 to 2147483647.
 */

cx_Integer cx_e_iprop(cx_Object ob, cx_String pname)
{
    register CI_PROP *ps;
    register int result;
    register char *ptr;

    ps = cx_e_property_content(ob,pname);
    if( !ps || !ps->pval ) return 0;

    ptr = ps->pval;
    if( *ptr == '-' ) {
        ptr++;
        if( !isdigit(*ptr) )
            return 0;
        result = (*ptr++)-'0';
        while( isdigit(*ptr) )
            result = 10*result + ((*ptr++)-'0');
        return -result;
    }

    if( !isdigit(*ptr) )
        return 0;

    result = (*ptr++)-'0';
    while( isdigit(*ptr) )
        result = 10*result + ((*ptr++)-'0');
    return result;
}


/*============================================================================
 *  cx_e_set_iprop() -- set (or reset) integer property by name
 *
 *  Creates/resets integer property with value `ival'.  Returns property.
 */

cx_Object cx_e_set_iprop(cx_Object ob, cx_String pname, cx_Integer ival)
{
    auto char buf[20];
    register char *ptr;

    ptr = &buf[18];
    *ptr-- = '\0';

    if( ival >= 0 ) {
        while( ival > 9 ) {
            *ptr-- = (ival%10)+'0';
            ival = ival/10;
        }
        *ptr = ival+'0';
    } else {
        ival = -ival;
        while( ival > 9 ) {
            *ptr-- = (ival%10)+'0';
            ival = ival/10;
        }
        *ptr-- = ival+'0';
        *ptr = '-';
    }
    return cx_e_setproperty(ob, pname, ptr);
}


/*============================================================================
 *  cx_e_rprop() -- return property by name as a real
 *
 *  Get object's real-valued property with given property name `pname'.
 *  Returns 0.0 if property is not found or if either argument is NULL.
 *  Limited to the range of a cx_Real, ca. +/- 1.4e-45 to 3.4e+38.
 */

cx_Real cx_e_rprop(cx_Object ob, cx_String pname)
{
    register CI_PROP *ps;

    ps = cx_e_property_content(ob,pname);
    if( !ps || !ps->pval ) return 0;
    return (cx_Real)atof(ps->pval);
}

/*============================================================================
 *  cx_e_realformat(), cx_e_set_realformat() -- get/set real format
 *
 *  Get or set the format that cx_set_rprop() uses to write cx_Real numbers.
 *  The format is interpreted as per C's printf(3), and is initially "%g".
 *  Calling cx_set_realformat(NULL) resets "%g" and recovers any memory used.
 */

cx_String cx_e_realformat(void)
{
   return realformat ? realformat : "%g";
}


void cx_e_set_realformat(cx_String fmt)
{
    if( realformat ) free(realformat);
    realformat = cx_strdup(fmt);
}


/*============================================================================
 *  cx_e_set_rprop() -- set (or reset) real property by name
 *
 *  Creates/resets real property with value `rval'.  Returns property.
 */

cx_Object cx_e_set_rprop(cx_Object ob, cx_String pname, cx_Real rval)
{
    char buf[128];

    if( realformat ) {
        sprintf(buf,realformat,rval);
    } else sprintf(buf,"%g",rval);
    return cx_e_setproperty(ob, pname, buf);
}

