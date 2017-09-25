/*****************************************************************************
*  $CX_ROOT/src/include/cx.h -- all public CX definitions
*
*----------------------------------------------------------------------------
*
*  This file is intended to indirectly define all public cx_ and cu_ entry
*  points so programs only need to include "cx.h".  Programs in the source
*  tree should NOT include this file directly.
*
*----------------------------------------------------------------------------
*
*  cx.h
*  |
*  +-------- cx_limits.h (lowest level hardcoded limits)
*  |
*  +-------- cx_types.h (primitive types and object types)
*  |
*  +-------- cx_cobweb.h (core object support)
*  |         |
*  |         +-------- cx_basics.h (base object & methods)
*  |         |
*  |         +-------- cx_cex_io.h (file i/o)
*  |         |
*  |         +-------- cx_utils.h (mainly string utilites)
*  |         |
*  |         +-------- cx_stream.h (streams and sequences)
*  |         |
*  |         +-------- cx_iostream.h (iostreams)
*  |         |
*  |         +-------- cx_string.h (string objects)
*  |         |
*  |         +-------- cx_property.h (properties)
*  |         |
*  |         +-------- cx_errorqueue.h (error queue & support)
*  |         |
*  |         +-------- cx_datatype.h (datatypes & datatype tables)
*  |         |
*  |         +-------- cx_spewob.h (object display utility)
*  |
*  +-------- cx_message.h (message)
*  |
*  +-------- cx_molecule.h (molecule, atom, bond, & tuples)
*  |
*  +-------- cx_exotic.h (unsupported object classes)
*  |
*  +-------- cx_applics.h (application interfaces)
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

#ifndef CX_ALL_INCLUDED
#define CX_ALL_INCLUDED 1

/*** Lowest level definitions only (no function prototypes). ***/

#include "cx_limits.h"

/*** Definitions of primitive and object types. ***/

#include "cx_types.h"

/*** cobweb (chemical object web) core package ***/

#include "cx_cobweb.h"

/*** message package ***/

#include "cx_message.h"

/*** molecule package ***/

#include "cx_molecule.h"

/*** surface package ***/

#include "cx_surface.h"

/*** exotic packages (unsupported objects)***/

#include "cx_exotic.h"

/*** applications NOT ***/

/* #include "cx_applics.h" */

#endif /* CX_ALL_INCLUDED */
