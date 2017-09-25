/*****************************************************************************
*  chain.c -- add disconnected component number as given property to molecules
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

#include <stdio.h>
#include <string.h>
#include "cx.h"
#include "cx_molecule.h"

/*============================================================================
 *  main()  for parts
 */

int main(int ac, char **av)
{
  cx_Object   dt, ob, ins, outs;

  /*** No options or arguments are allowed. ***/

  if (1 != ac) {
    fprintf(stderr, "usage:  %s < in.cex > out.cex\n", *av);
    exit(1);
  }

  /*** Initialize molecule package. ***/

  cx_molecule_pkg();

  /*** Create and publish chain datatype. ***/

  dt = cx_create_datatype(NULL, "CHAIN", "chain", "Atom chain ID",
                          "1A", "STRING", "Atom chain membership as ID");

  /*** Create input and output streams. ***/

  ins  = cx_create_iostream("-", CX_IO_READ );
  outs = cx_create_iostream("-", CX_IO_WRITE);


  /*** Operate on molecules in input stream. ***/

  while (NULL != (ob = cx_next(ins))) {
    if (CX_OB_MOLECULE == cx_type(ob)) {

      /*** Add parts as chain atomtuple property. ***/

      cx_set_datatype(cx_create_atomtuple(ob, "chain"), dt);
      cu_mol_setpart(ob, "chain");
    }

    /*** Append all objects to output then zap'em. ***/

    cx_append(outs, ob);
    cx_destroy(ob);
  }

  /*** Show errors, close files, exit. ***/

  cx_error_spew(stderr, CX_ERR_ERROR);
  cx_destroy(ins);
  cx_destroy(outs);
  exit(0);
}
