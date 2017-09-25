/*****************************************************************************
*  printmol.c -- print molecule debugging information
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

#include "cx.h"
#include "cx_molecule.h"

/*** Output flags. ***/

#define CX_OUT_NORMAL   1
#define CX_OUT_TERSE    2
#define CX_OUT_VERBOSE  3

/*** Handy macros. ***/

#define NOTE(msg)  cx_error_save(msg, CX_ERR_NOTE,  *argv)
#define FATAL(msg) cx_error_save(msg, CX_ERR_FATAL, *argv)

/*============================================================================
 *  proginit() -- interpret arguments and initialize program
 *
 *  On error, writes usage summary and exits; any return is successful.
 */

static void proginit(int argc, char **argv, cx_Object *ins, FILE **ppout,
                     int *verb, int *spew, char **elev)
{
   char  *oops, *oops2, *fnin, *fnout, *pok;
   int    ia;

   /*** Set default values. ***/

    fnin  = (char*)0;
    fnout = (char*)0;
   *spew  = CX_SPEW_DEFAULT;
   *verb  = CX_OUT_NORMAL;
   *elev  = CX_ERR_ERROR;
    oops  = (char*)0;
    oops2 = (char*)0;

   /*** Initialize molecule package. ***/
    
   cx_molecule_pkg();

   /*** Deal with command line arguments. ***/

   for (ia = 1; ia < argc; ia++) {

      /*** Extract input, output file names; error if more than two. ***/

      if ('-' != *argv[ia]) {
         if      (NULL == fnin ) fnin  = argv[ia];
         else if (NULL == fnout) fnout = argv[ia];
         else { oops = "too many file names specified"; oops2 = argv[ia]; }

      /*** Deal with options. ***/

      } else {
         if      (0 == cx_strcmp("-om", argv[ia]))  *verb = CX_OUT_NORMAL;
         else if (0 == cx_strcmp("-ot", argv[ia]))  *verb = CX_OUT_TERSE;
         else if (0 == cx_strcmp("-ov", argv[ia]))  *verb = CX_OUT_VERBOSE;
         else if (0 == cx_strcmp("-me", argv[ia]))  *elev = CX_ERR_ERROR;
         else if (0 == cx_strcmp("-mn", argv[ia]))  *elev = CX_ERR_NOTE;
         else if (0 == cx_strcmp("-mw", argv[ia]))  *elev = CX_ERR_WARN;

         /*** Interpret argument as number if it starts with a digit. ***/

         else if (isdigit(argv[ia][1])) {
            *spew = -1 * strtol(argv[ia], &pok, 10);
            if (pok == argv[ia] || 0 > *spew || 127 < *spew) {
               oops  = "bad -<#> value, expected 0 - 127:";
               oops2 = argv[ia];
            }

         /*** Else, punt. ***/
         
         } else { oops  = "unknown option:"; oops2 = argv[ia]; }
      }
   }

   /*** Open input and output files. ***/

   if (NULL == oops) {
      if (NULL == fnin) fnin = "-";
      *ins  = cx_create_iostream(fnin, CX_IO_READ);
      if (NULL == *ins) {
         oops = "can't open input stream"; oops2 = fnin;
      } else {
         *ppout = ((NULL == fnout) ? stdout : fopen(fnout, "w"));
         if (NULL == ppout) { oops = "can't open output file"; oops2 = fnout; }
      }
   }

   /*** Exit on syntax error. ***/

   if (oops) {
      NOTE ( "printmol -- print molecules on cex stream"           );
      NOTE ( ""                                                    );
      FATAL(oops                                                   );
      FATAL(oops2                                                  );
      NOTE (""                                                     );
      NOTE ("Usage:  printmol [options] [in.cex [out.txt]]"        );
      NOTE (""                                                     );
      NOTE ("Input is from standard input if `in.cex'  is omitted.");
      NOTE ("Output is to standard output if `out.txt' is omitted.");
      NOTE ("Error messages are always written to standard error." );
      NOTE (""                                                     );
      NOTE ("Error message control options (at most one of):"      );
      NOTE ("  -me: show ERROR messages only (default)"            );
      NOTE ("  -mn: show NOTE, WARNING and ERROR messages"         );
      NOTE ("  -mw: show WARNING and ERROR messages"               );
      NOTE (""                                                     );
      NOTE ("Output content control options (at most one of):"     );
      NOTE ("  -om: print (m)olecule only (default)"               );
      NOTE ("  -ot: (t)erse, print just smiles and name"           );
      NOTE ("  -ov: (v)erbose, print everything else, too"         );
      NOTE (""                                                     );
      NOTE ("Formatted output control option (if not -ot):"        );
      NOTE ("  -<#>: print data at level #,"                       );
      NOTE ("    where # is bitwise print level (sum of):"         );
      NOTE ("      1 = data           16 = raw datatags"           );
      NOTE ("      2 = children       32 = object classes"         );
      NOTE ("      4 = properties     64 = summarize tuples"       );
      NOTE ("      8 = property names                      "       );
      NOTE (""                                                     );
      NOTE ("Default option settings are:  -se -om -7"             );
      cx_error_spew(stderr, NULL);
      exit(1);
   }
}

/*============================================================================
 *  invent_moltypes() -- invent datatypes for internal molecule properties
 */

static char *tm[] =  {
  "_MI", CX_PROP_INSMI,  "Input SMILES",    "1",  "STRING", "insmi",
  NULL
};

static char *ta[] =  {
  "_AI", CX_PROP_IMPH,   "Implicit hcount", "1A", "INTEGER", "atom imph",
  "_AN", CX_PROP_ATNUMB, "Atomic number"  , "1A", "INTEGER", "atom number",
  "_AS", CX_PROP_ATSYMB, "Atomic symbol"  , "1A", "STRING" , "atom symbol",
  "_AM", CX_PROP_MASS,   "Atom mass"      , "1A", "INTEGER", "atom mass",
  "_AC", CX_PROP_CHARGE, "Atom charge"    , "1A", "INTEGER", "atom charge",
  "_AL", CX_PROP_CHIRAL, "Atom chirality" , "1A", "STRING" , "atom chirality",
  "_Al", "label",        "Label"          , "1A", "INTEGER", "label",
  NULL
};

static char *tb[] =  {
  "_BO", CX_PROP_BORDER, "Bond order",      "1B", "INTEGER", "bond order",
  "_BS", CX_PROP_BSYMB,  "Bond symbol",     "1B", "STRING",  "bond symbol",
  "_BS", CX_PROP_DBO,    "Bond dbo",        "1B", "STRING",  "bond dbo",
  NULL
};

static void invent_moltypes(cx_Object table)
{
   char **p;

   /*** Create datatypes for internal molecule properties. ***/

   for (p = tm; *p; p += 6)
      cx_create_datatype(table, p[0], p[1], p[2], p[3], p[4], p[5]);

   /*** Create datatypes for internal molecule atomtuples. ***/

   for (p = ta; *p; p += 6)
      cx_create_datatype(table, p[0], p[1], p[2], p[3], p[4], p[5]); 

   /*** Create datatypes for internal molecule bondtuples. ***/

   for (p = tb; *p; p += 6)
      cx_create_datatype(table, p[0], p[1], p[2], p[3], p[4], p[5]);
}

/*============================================================================
 *  molbug() -- attach datatypes to molecule innards (molecule and its tuples)
 */

static cx_Integer molbug(cx_Object mol, cx_Object table)
{
   char      **p;
   cx_Object   dt;

   /*** Attach "MOL" datatype to molecule. ***/

   if (NULL == (dt = cx_tag2datatype(table, "MOL"))) return FALSE;
   cx_set_datatype(mol, dt);

   /*** Create tuples for internal props with attached datatypes. ***/

   for (p = ta; *p; p += 6)
      cx_set_datatype(
         cx_create_atomtuple(mol, p[1]), cx_tag2datatype(table, p[0]));

   for (p = tb; *p; p += 6)
      cx_set_datatype(
         cx_create_bondtuple(mol, p[1]), cx_tag2datatype(table, p[0]));

   return TRUE;
}

/*============================================================================
 *  main() for printmol
 */

int main(int argc, char **argv)
{
   int        verb, spew;
   char      *elev, *name;
   cx_Object  ob, orig, table, ins;
   FILE      *fpout;

   cx_Object cx_e_mol_copy(cx_Object);

   /*** Initialize program (consumes option arguments). ***/

   proginit(argc, argv, &ins, &fpout, &verb, &spew, &elev);

   /*** Get default datatype table. ***/

   table = cx_default_datatypetable();

   /*** Invent datatypes for internal molecule properties. ***/

   invent_moltypes(table);

   /*** Loop over objects on input stream. ***/

   while (NULL != (ob = cx_next(ins))) {

      /*** Terse output. ***/

      if (CX_OUT_TERSE == verb) {
         if (CX_OB_MOLECULE == cx_type(ob)) {
            if (NULL == (name = cx_sprop(ob, "molname")))
               name = cx_sprop(ob, "name");
            if (name) fprintf(fpout, "%s %s\n", cx_stringvalue(ob), name);
            else      fprintf(fpout, "%s\n",    cx_stringvalue(ob));
         }

      /*** Verbose output: spew everything including structure. ***/

      } else if (CX_OUT_VERBOSE == verb) {
         if (CX_OB_MOLECULE == cx_type(ob)) molbug(ob, table);
         if (0 < spew) cx_spewob(fpout, ob, spew);

      /*** Normal output: spew molecule (copies). ***/

      } else {
         if (CX_OB_MOLECULE == cx_type(ob)) {
            orig = ob;
            if (NULL != (ob = cx_e_mol_copy(ob))) {
               molbug(ob, table);
               if (0 < spew) cx_spewob(fpout, ob, spew);
            }
            cx_destroy(orig);
         }
      }

      /*** Clean up. ***/

      cx_error_spew(stderr, elev);
      cx_destroy(ob);
   }

   /*** Dump errors to stderr, clean up, close files and exit with 0. ***/

   cx_error_spew(stderr, elev);
   cx_destroy(ins);
   cx_cleanup();
   fclose(fpout);
   return 0;
}
