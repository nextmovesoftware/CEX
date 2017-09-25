/*****************************************************************************
*  cx_wrl.h -- CX support for VRML (virtual reality markup language)
*****************************************************************************/

/*** Standard "models" for cx_wrl_write(). ***/

#define CX_WRL_NONE        0
#define CX_WRL_WIREFRAME   1
#define CX_WRL_STICKS      2
#define CX_WRL_BALLSTICK   3
#define CX_WRL_SPACEFILL   4

/*** Internal property names. ***/

#define CX_WRL_REMARK       "wrl_remark"
#define CX_WRL_COORDINATES  "wrl_coordinates"

#define CX_WRL_MOLNAME      "wrl_molname"
#define CX_WRL_ATOMNAME     "wrl_atomname"
#define CX_WRL_BONDNAME     "wrl_bondname"

#define CX_WRL_ATOMCOLOR    "wrl_atomcolor"
#define CX_WRL_BONDCOLOR    "wrl_bondcolor"

#define CX_WRL_ATOMRADIUS   "wrl_atomradius"
#define CX_WRL_BONDRADIUS   "wrl_bondradius"

/*** Functions in cx_wrl_write.c ***/

cx_Integer cx_wrl_setatomcolors(cx_Object mol                            );
cx_Integer cx_wrl_setatomradii (cx_Object mol, cx_Real factor            );
cx_Integer cx_wrl_setbondradii (cx_Object mol, cx_Real radius            );
cx_Integer cx_wrl_write        (FILE *fp, cx_Object mol, cx_Integer model);
