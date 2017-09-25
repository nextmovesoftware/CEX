/*****************************************************************************
*  cx_pdb.h -- CX PDB package definitions
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

#ifndef CX_PDB_INCLUDED
#define CX_PDB_INCLUDED 1

/*** Function declarations. ***/

cx_Integer cx_pdb_eof (void);

cx_Object  cx_pdb_read(FILE *fp);

cx_Integer cx_pdb_set_atomnames(cx_Object mol, cx_Object *aa);

cx_Integer cx_pdb_write(FILE *fp,              /* output stream            */
                        cx_Object mol,         /* molecule to output       */
                        cx_String name,        /* for COMPND record        */
                        cx_String remark,      /* for REMARK record        */
                        cx_String alabs_prop,  /* atom label prop name     */
                        cx_String resid_prop,  /* residue atomprop name    */
                        cx_String chain_prop,  /* chain atomprop name      */
                        cx_String resno_prop,  /* resnum atomprop name     */
                        cx_String coord_prop,  /* coordinate atomprop name */
                        cx_String occup_prop,  /* occupancy atomprop name  */
                        cx_String bvalu_prop,  /* bvalue atomprop name     */
                        cx_String acolor_prop, /* color atomprop name      */
                        cx_String arad_prop);  /* radius atomprop name     */

cx_Integer cu_camera2pdb(FILE *fp,             /* output stream            */
			 cx_Object camera);    /* camera to output         */

#endif /* CX_PDB_INCLUDED */
