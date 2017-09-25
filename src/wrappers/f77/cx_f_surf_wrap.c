/*****************************************************************************
*  cx_f_surf_wrap.c
*
*
*****************************************************************************/

#include <string.h>
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>

#include "cx.h"
#include "cx_surface.h"

/*** Variable type used for implicit Fortran string length ***/

typedef cx_Integer  FLENS;

/*** Fortran true and false. ***/

#define FTRUE  1
#define FFALSE 0

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

#define cx_f_prefix2vtuples_	cx_f_prefix2vtuples
#define cx_f_create_facetuple_	cx_f_create_facetuple
#define cx_f_prefix2ftuples_	cx_f_prefix2ftuples

#endif

/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Linux-ppc appends "__" to each Fortran function name!
======================================================================*/

#if defined(LINUX)

#define cx_f_prefix2vtuples_	cx_f_prefix2vtuples__
#define cx_f_create_facetuple_	cx_f_create_facetuple__
#define cx_f_prefix2ftuples_	cx_f_prefix2ftuples__

#endif


/*============================================================================
 *  c2fstr -- copy C char array to fortran string
 *
 *  This copies MIN(lens,mxout) chars of str to out and returns the number of
 *  characters copied; if less than mxout, out is blank-padded to mxout - 1.
 */

static cx_Integer c2fstr(cx_Integer lens, cx_String str, cx_String out,
                         FLENS mxout)
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
 *  cx_f_face -- find face from vertices
 */

cx_Integer cx_f_face_(cx_Object *v1, cx_Object *v2, cx_Object *v3)
{
   return (cx_Integer) cx_face(*v1, *v2, *v3);
}

/*============================================================================
 *  cx_f_create_vertex -- create new vertex in surface
 */

cx_Integer cx_f_create_vertex_(cx_Object *surf, cx_String xyzs, FLENS lenxyzs)
{
   cx_String s  = f2cstr(lenxyzs, xyzs);
   cx_Object ob = cx_create_vertex(*surf, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_vertex_id -- return id of given vertex
 */

cx_Integer cx_f_vertex_id_(cx_Object *vertex)
{
   return cx_e_vertex_id(*vertex);
}

/*============================================================================
 *  cx_f_vertex_from_id -- find vertex with given id
 */

cx_Integer cx_f_vertex_from_id_(cx_Object *surf, cx_Integer *id)
{
   return (cx_Integer) cx_e_vertex_from_id(*surf, *id);
}

/*============================================================================
 *  cx_f_create_face -- create new face from vertices
 */

cx_Integer cx_f_create_face_(cx_Object *v1, cx_Object *v2, cx_Object *v3)
{
   return (cx_Integer) cx_create_face(*v1, *v2, *v3);
}

/*============================================================================
 *  cx_f_create_surface -- create new surface
 */

cx_Integer cx_f_create_surface_(cx_Object *parent)
{
   return (cx_Integer) cx_create_surface(*parent);
}

/*============================================================================
 *  cx_f_surface_pkg -- initialize surface package
 */

cx_Integer cx_f_surface_pkg_(void)
{
   return cx_surface_pkg();
}

/*============================================================================
 *  cx_f_surface_create_datatypes -- create datatypes for surface package
 */

void cx_f_surface_create_datatypes_(cx_Object *table)
{
   cx_surface_create_datatypes(*table);
}

/*============================================================================
 *  cx_f_create_vertextuple -- create vertex tuple with given name
 */

cx_Integer cx_f_create_vertextuple_(cx_Object *surf, cx_String str, FLENS lens)
{
   cx_String s  = f2cstr(lens, str);
   cx_Object ob = cx_create_vertextuple(*surf, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_vertextuple_name -- get vertex tuple name
 */

cx_Integer cx_f_vertextuple_name_(cx_Object *tuple, cx_String buf, FLENS mx)
{
   cx_String str = cx_vertextuple_name(*tuple);
   return c2fstr(strlen(str), str, buf, mx);
}

/*============================================================================
 *  cx_f_prefix2vtuples -- get stream of vertex tuples with given prefix
 */

cx_Integer cx_f_prefix2vtuples_(cx_Object *surf, cx_String str, FLENS lens)
{
   cx_String s  = f2cstr(lens, str);
   cx_Object ob = cx_prefix2vtuples(*surf, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_create_facetuple -- create face tuple with given name
 */

cx_Integer cx_f_create_facetuple_(cx_Object *surf, cx_String str, FLENS lens)
{
   cx_String s  = f2cstr(lens, str);
   cx_Object ob = cx_create_facetuple(*surf, s);
   cx_free(s);
   return (cx_Integer) ob;
}

/*============================================================================
 *  cx_f_facetuple_name -- get name of face tuple
 */

cx_Integer cx_f_facetuple_name_(cx_Object *tuple, cx_String buf, FLENS mx)
{
   cx_String str = cx_facetuple_name(*tuple);
   return c2fstr(strlen(str), str, buf, mx);
}

/*============================================================================
 *  cx_f_prefix2ftuples -- get stream of face tuples with given prefix
 */

cx_Integer cx_f_prefix2ftuples_(cx_Object *surf, cx_String str, FLENS lens)
{
   cx_String s  = f2cstr(lens, str);
   cx_Object ob = cx_prefix2ftuples(*surf, s);
   cx_free(s);
   return (cx_Integer) ob;
}
