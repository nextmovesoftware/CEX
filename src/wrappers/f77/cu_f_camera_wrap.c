/*****************************************************************************
*  cu_f_camera_wrap.c
*
*
*****************************************************************************/

#include <string.h>
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>

#include "cx.h"
#include "cu_camera.h"

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

#define cu_f_create_camera_		cu_f_create_camera
#define cu_f_camera_pkg_		cu_f_camera_pkg
#define cu_f_camera_set_string_		cu_f_camera_set_string
#define cu_f_camera_create_datatypes_	cu_f_camera_create_datatypes

#endif

/*======================================================================
* MACHINE-DEPENDENT FORTRAN NAMING CONVENTIONS
*
* Linux-ppc appends "__" to each Fortran function name!
======================================================================*/

#if defined(LINUX)

#define cu_f_create_camera_		cu_f_create_camera__
#define cu_f_camera_pkg_		cu_f_camera_pkg__
#define cu_f_camera_set_string_		cu_f_camera_set_string__
#define cu_f_camera_create_datatypes_	cu_f_camera_create_datatypes__

#endif

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
 *  cu_f_create_camera_
 */

cx_Integer cu_f_create_camera_(cx_Object *parent)
{
   return (cx_Integer) cu_create_camera(*parent);
}

/*============================================================================
 *  cu_f_camera_pkg_
 */

cx_Integer cu_f_camera_pkg_(void)
{
   return (cx_Integer) cu_camera_pkg();
}

/*============================================================================
 *  cu_f_camera_set_string_
 */

void cu_f_camera_set_string_(cx_Object *camera, cx_String str, FLENS lens)
{
   cx_String s  = f2cstr(lens, str);
   cu_camera_set_string(*camera, s);
   cx_free(s);
}

/*============================================================================
 *  cu_f_camera_create_datatypes_
 */

void cu_f_camera_create_datatypes_(cx_Object *table)
{
   cu_e_camera_create_datatypes(*table);
}
