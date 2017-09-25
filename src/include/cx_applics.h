/*****************************************************************************
*  $CX_ROOT/src/include/cx_applics.h -- CX application interface definitions
*
*  This defines any public entry points provided by applics code (indirectly).
*
*  There is no need to add anything here for new applications which don't
*  provide a functional interface (i.e., just executable(s)).  This is a
*  convenient place to define functions which have code under the applics
*  directory, e.g., the applics/pdb provides functional interfaces
*  cx_pdb_read() and cx_pdb_write() corresponding to the functionality of
*  the programs programs pdb2cex and cex2pdb.
*
*  The entry points should be prefixed "cx_" (if officially supported entry
*  in libcx.a) or "cu_" (if not officially supported, in libcu.a).
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

#ifndef CX_APPLICS_INCLUDED
#define CX_APPLICS_INCLUDED 1

/*** Include applics interface header file here. ***/

#include "cx_pdb.h"

#endif /* CX_APPLICS_INCLUDED */
