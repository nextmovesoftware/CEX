/*****************************************************************************
*  cx_stream.c -- support for sequence and stream objects
*
*----------------------------------------------------------------------------
*  Contributing author and institution: Dave Weininger, Daylight CIS, Inc.
*  CEX IOSTREAM extensions author:      Roger Sayle, GlaxoWellcome R&D, UK.
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

/*** An OBLIST is a linked list of cx_Objects, ie., (void *) pointers. ***/

typedef struct _ci_oblist {
   cx_Object          ob;
   struct _ci_oblist *next;
} CI_OBLIST;

/*** Stream struct: linked list of objects with a current position. ***/

typedef struct {
   CI_OBLIST *list;
   CI_OBLIST *last;
   CI_OBLIST *cur;
} CI_SITEM;

/*** Are we initialized? ***/
static int initialized = FALSE;



/*************************************
*
*  Common method initialization
*
*************************************/


/*============================================================================
 *  seq_destroy() -- destroy a given sequence (or stream)
 */
 
static void seq_destroy(cx_Object ob)
{
    register CI_OBLIST *list;
    register CI_OBLIST *next;
    register CI_SITEM *sit;

    /*** Extract stream content as CI_SITEM struct, if able. ***/
    sit = (CI_SITEM*)cx_e_base_content(ob);
    if( sit ) {
        /*** Destroy linked list of CI_OBLIST's (but not listed objects). ***/
        for( list=sit->list; list; list=next ) {
            next = list->next;
            free(list);
        }

        /*** Destroy CI_SITEM struct. ***/
        free(sit);
    }

    /*** Destroy base object and any children. ***/
    cx_e_base_destroy(ob);
}


/*============================================================================
 *  seq_reset() -- reset stream or sequence, return success
 */

static cx_Integer seq_reset(cx_Object ob)
{
    register CI_SITEM  *sit;

    /*** Extract stream content as CI_SITEM struct, if able. ***/
    sit = (CI_SITEM *)cx_e_base_content(ob);
    if( !sit ) return FALSE;

    /*** Set current position to start of list, return successfully. ***/
    sit->cur = sit->list;
    return TRUE;
}


/*============================================================================
 *  stream_class_init( int type ) -- Initialize stream/sequence class
 */

static void stream_class_init( int type )
{
    cx_set_method(type, "destroy", seq_destroy);
    cx_set_method(type, "reset",   seq_reset);

    /*** Use normal (base) count and stream methods. ***/
    cx_set_method(type, "count",       cx_e_base_count);
    cx_set_method(type, "stream",      cx_e_base_stream);
    cx_set_method(type, "setproperty", cx_e_base_setproperty);
}


/*============================================================================
 *  stream_init() -- initialize sequence- and stream-specific functions
 */
 
static void stream_init(void)
{
    /*** Define Sequence and Stream object types. ***/
    cx_e_set_typename(CX_OB_STREAM,   "Stream"  );
    stream_class_init(CX_OB_STREAM);
 
    cx_e_set_typename(CX_OB_SEQUENCE, "Sequence");
    stream_class_init(CX_OB_SEQUENCE);

    initialized = TRUE;
}


/*************************************
*
*  Sequence support follows
*
*************************************/

/*============================================================================
 *  cx_e_create_sequence() -- create and return an empty sequence
 */

cx_Object cx_e_create_sequence(cx_Object parent)
{
    register cx_Object  sob;
    register CI_SITEM  *sit;

    /*** Are methods initialized? ***/
    if( !initialized ) stream_init();

    /*** Make and initialize CI_SITEM struct. ***/
    sit = (CI_SITEM *)malloc(sizeof(CI_SITEM));
    if( !sit ) return (cx_Object)0;

    sit->list = (CI_OBLIST*)0;
    sit->last = (CI_OBLIST*)0;
    sit->cur  = (CI_OBLIST*)0;

    /*** Make new CX_OB_SEQUENCE object with given parent. ***/
    sob = cx_e_base_create(parent, CX_OB_SEQUENCE);
    if( !sob ) { free(sit); return (cx_Object)0; }

    /*** Set content of new sequence object. ***/
    cx_e_base_set_content(sob,(void*)sit);

    /*** Return initialized sequence object. ***/
    return sob;
}


/*============================================================================
 *  cx_e_base_append() -- append object to given sequence, return success
 *
 *  This isn't a true base function (i.e., base for a polymorphic function),
 *  but if it becomes one, here's where it goes.  This does not do object
 *  class checking and is NOT a public function -- use cx_append() instead.
 *
 *  It is provided so new packages can provide non-standard stream messages.
 */

cx_Integer cx_e_base_append(cx_Object ob, cx_Object addob)
{
    register CI_OBLIST *obitem;
    register CI_SITEM  *sit;

    /*** Extract sequence content as CI_SITEM struct, if able. ***/
    sit = (CI_SITEM *) cx_e_base_content(ob);
    if( !sit ) return FALSE;

    /*** Create sequence entry. ***/
    obitem = (CI_OBLIST*)malloc(sizeof(CI_OBLIST));
    if( !obitem ) return FALSE;

    obitem->next = (CI_OBLIST*)0;
    obitem->ob   = addob;

    if( sit->list ) {
        /*** Make current item, if current item is undefined. ***/
        if( !sit->cur ) sit->cur = obitem;

        sit->last->next = obitem;
        sit->last = obitem;
    } else {
        sit->list = obitem;
        sit->last = obitem;
        sit->cur = obitem;
    }

    /*** Return successfully. ***/
    return TRUE;
}


/*============================================================================
 *  cx_e_append() -- append object to given sequence, return success
 */

cx_Integer cx_e_append(cx_Object ob, cx_Object addob)
{
    register cx_Integer type;

    type = cx_type(ob);

    if( type == CX_OB_IOSTREAM )
        return cx_e_iostream_append(ob,addob);
    if( type == CX_OB_SEQUENCE )
        return cx_e_base_append(ob,addob);
    return FALSE;
}


/*============================================================================
 *  cx_e_base_delete() -- delete object from given sequence, return success
 *
 *  This doesn't check arguments: caller guarantees that ob is a sequence or
 *  stream and zapob is non-NULL.  Returns FALSE if object isn't in list.
 *  "Current object" in the sequence (the next one to be returned) is not
 *  changed unless it was the object being deleted, in which case the it is
 *  set to the next one.
 *
 *  Slightly flawed interface in that it's only possibly to zap the first
 *  NULL object in the sequence or stream.
 *
 *  This isn't a true base function (i.e., base for a polymorphic function),
 *  but if it becomes one, here's where it goes.  This does not do object
 *  class checking and is NOT a public function -- use cx_delete() instead.
 *
 *  It is provided so new packages can provide non-standard stream messages.
 */

cx_Integer cx_e_base_delete(cx_Object ob, cx_Object zapob)
{
    register CI_SITEM  *sit;  /* sequence item */
    register CI_OBLIST *zit;  /* object list item */
    register CI_OBLIST *tmp;

    /*** Extract sequence content as CI_SITEM struct, if able. ***/
    sit = (CI_SITEM*)cx_e_base_content(ob);
    if( !sit ) return FALSE;

    zit = sit->list;
    if( !zit ) return FALSE;

    /*** If zapob is root of list, make its `next' the root. ***/
    if( zit->ob == zapob ) {
        if( sit->last == zit ) {
            sit->list = (CI_OBLIST*)0;
            sit->last = (CI_OBLIST*)0;
            sit->cur  = (CI_OBLIST*)0;
        } else {
            if( sit->cur == zit )
                sit->cur = zit->next;
            sit->list = zit->next;
        }
        free(zit);
        return TRUE;
    }

    /*** Else find zapob, remove from list; fail if unable. ***/
    tmp=zit;
    zit=zit->next;
    while( zit ) {
        if( zit->ob == zapob ) {
            if( sit->last == zit ) {
                tmp->next = (CI_OBLIST*)0;
                sit->last = tmp;
            } else tmp->next = zit->next;
            free(zit);
            return TRUE;
        }
        tmp = zit;
        zit = zit->next;
    }
    return FALSE;
}


/*============================================================================
 *  cx_e_delete() -- delete object from given sequence, return success
 */

cx_Integer cx_e_delete(cx_Object ob, cx_Object zapob)
{
    /*** Return NULL if ob is not a sequence object. ***/
    if( cx_type(ob) != CX_OB_SEQUENCE ) return FALSE;

    /*** Else, return local pseudo-base function. ***/
    return cx_e_base_delete(ob, zapob);
}


/*============================================================================
 *  cx_e_next() -- return next object in sequence/stream or NULL
 */

cx_Object cx_e_next(cx_Object ob)
{
    register CI_SITEM  *sit;
    register cx_Object  out;
    register cx_Integer type;

    type = cx_type(ob);

    /*** Handle IOSTREAM objects as a special case ***/
    if( type == CX_OB_IOSTREAM ) return cx_e_iostream_next(ob);

    /*** Return NULL if ob is not a stream or sequence object. ***/
    if( type != CX_OB_SEQUENCE && type != CX_OB_STREAM )
        return (cx_Object)0;

    /*** Extract object content as CI_SITEM struct, if able. ***/
    sit = (CI_SITEM*)cx_e_base_content(ob);
    if( !sit ) return (cx_Object)0;

    /*** Return NULL if at end of list. ***/
    if( !sit->cur ) return (cx_Object)0;

    /*** Return current object, advance for next. ***/
    out      = sit->cur->ob;
    sit->cur = sit->cur->next;
    return out;
}


/*============================================================================
 *  cx_e_atend() -- return TRUE iff stream/sequence is positioned at the end
 *
 *  This returns FALSE on error, e.g., ob isn't a stream or sequence.
 */

cx_Integer cx_e_atend(cx_Object ob)
{
    register CI_SITEM  *sit;
    register cx_Integer type;

    type = cx_type(ob);

    /*** Handle IOSTREAM objects as a special case ***/
    if( type == CX_OB_IOSTREAM ) return cx_e_ioeof(ob);

    /*** Return TRUE if ob is not a stream or sequence object. ***/
    if( type != CX_OB_SEQUENCE && type != CX_OB_STREAM )
        return TRUE;

    /*** Extract content as CI_SITEM struct, if able. ***/
    sit = (CI_SITEM*)cx_e_base_content(ob);
    if( !sit ) return TRUE;

    /*** Return TRUE iff next cx_next() would return NULL due to EOS. ***/
    return !sit->cur;
}


/*************************************
*
*  Stream support follows
*
*************************************/

/*============================================================================
 *  cx_e_create_stream() -- create and return an empty stream
 *
 *  This is NOT a public function.
 */

cx_Object cx_e_create_stream(cx_Object parent)
{
    register cx_Object  sob;
    register CI_SITEM  *sit;

    /*** Are methods initialized? ***/
    if( !initialized ) stream_init();

    /*** Make and initialize CI_SITEM struct. ***/
    sit = (CI_SITEM *)malloc(sizeof(CI_SITEM));
    if( !sit ) return (cx_Object)0;

    sit->list = (CI_OBLIST*)0;
    sit->last = (CI_OBLIST*)0;
    sit->cur  = (CI_OBLIST*)0;

    /*** Make new CX_OB_STREAM object with given parent. ***/
    sob = cx_e_base_create(parent, CX_OB_STREAM);
    if( !sob ) { free(sit); return (cx_Object)0; }

    /*** Set content of new stream object. ***/
    cx_e_base_set_content(sob,(void*)sit);

    /*** Return initialized stream object. ***/
    return sob;
}


/*============================================================================
 *  cx_e_base_count() -- count object's children of given class
 *
 *  This base function is coded here because of the parallel with cx_stream().
 */

cx_Integer cx_e_base_count(cx_Object ob, cx_Integer type)
{
    register cx_Object child;
    register int       count;

    child = cx_e_child(ob);
    if( !child ) return 0;

    if( type != CX_OB_ANY ) {
        count = 0;
        do {
            if( cx_e_type_safe(child) == type )
                count++;
            child = cx_e_sibling_safe(child);
        } while( child );

    } else {
        count = 1;
        child = cx_e_sibling_safe(child);
        while( child ) {
            child = cx_e_sibling_safe(child);
            count++;
        }
    }
    return (cx_Integer)count;
}


/*============================================================================
 *  cx_e_base_stream() -- return stream of object's children of given class
 *
 *  This returns NULL if object has no children of given class.
 */

cx_Object cx_e_base_stream(cx_Object ob, cx_Integer type)
{
    register cx_Object   child;
    register cx_Object   sob;
    register CI_SITEM   *stream;
    register CI_OBLIST  *oblist;

    /*** Are methods initialized? ***/
    if( !initialized ) stream_init();

    /*** Get first child object, return NULL if unable. ***/
    child = cx_e_child(ob);
    if( !child ) return (cx_Object)0;

    if( type == CX_OB_ANY ) {
        stream = (CI_SITEM*)malloc(sizeof(CI_SITEM));
        if( !stream ) return (cx_Object)0;

        sob = cx_e_base_create(ob, CX_OB_STREAM);
        if( !sob ) { free(stream); return (cx_Object)0; }
        cx_e_base_set_content(sob, (void *)stream);

        oblist = (CI_OBLIST*)malloc(sizeof(CI_OBLIST));
        if( !oblist ) {
            stream->list = (CI_OBLIST*)0;
            stream->last = (CI_OBLIST*)0;
            stream->cur  = (CI_OBLIST*)0;
            return sob;
        }

        oblist->ob = child;
        stream->list = oblist;
        stream->last = oblist;
        stream->cur  = oblist;

        child = cx_e_sibling_safe(child);
        while( child ) {
            oblist = (CI_OBLIST*)malloc(sizeof(CI_OBLIST));
            if( oblist ) {
                oblist->ob = child;
                stream->last->next = oblist;
                stream->last = oblist;
            }
            child = cx_e_sibling_safe(child);
        }
        stream->last->next = (CI_OBLIST*)0;
        return sob;
    }

    stream = (CI_SITEM*)0;
    sob = (cx_Object)0;

    do {
        if( cx_e_type_safe(child) == type ) {
            if( !stream ) {
                stream = (CI_SITEM*)malloc(sizeof(CI_SITEM));
                if( !stream ) return (cx_Object)0;

                sob = cx_e_base_create(ob, CX_OB_STREAM);
                if( !sob ) { free(stream); return (cx_Object)0; }
                cx_e_base_set_content(sob, (void *)stream);

                oblist = (CI_OBLIST*)malloc(sizeof(CI_OBLIST));
                if( !oblist ) {
                    stream->list = (CI_OBLIST*)0;
                    stream->last = (CI_OBLIST*)0;
                    stream->cur  = (CI_OBLIST*)0;
                    return sob;
                }

                oblist->next = (CI_OBLIST*)0;
                oblist->ob = child;

                stream->list = oblist;
                stream->last = oblist;
                stream->cur  = oblist;

            } else {
                oblist = (CI_OBLIST*)malloc(sizeof(CI_OBLIST));
                if( oblist ) {
                    oblist->next = (CI_OBLIST*)0;
                    oblist->ob   = child;

                    stream->last->next = oblist;
                    stream->last       = oblist;
                }
            }
        }
        child = cx_e_sibling_safe(child);
    } while( child );
    return sob;
}

