include $(CX_ROOT)/include/makefile.mdep

TARGET  = libcx_f.a incs
LIBDIR  = $(CX_ROOT)/lib
INCDIR  = $(CX_ROOT)/include
OBJS	= cx_f_wrappers.o cx_f_mol_wrap.o cx_f_surf_wrap.o \
	  cu_f_camera_wrap.o
HDRS    = cx_f.inc cx_f_molecule.inc cx_f_surface.inc \
	  cu_f_camera.inc

install all:	$(TARGET)

libcx_f.a:	$(OBJS) incs
		$(AR) $(AR_PUTFLAGS) libcx_f.a $(OBJS) && cp libcx_f.a $(LIBDIR)

incs:
		cp $(HDRS) $(INCDIR)


testprogs:	felloworlds

LIBS	= $(CX_ROOT)/lib/libcx.a \
	  $(CX_ROOT)/lib/libcu.a

cx_f_wrappers.o:	cx_f_wrappers.c
			$(CC) $(CFLAGS) -c -o cx_f_wrappers.o \
			-I$(CX_ROOT)/include \
			cx_f_wrappers.c

cx_f_mol_wrap.o:	cx_f_mol_wrap.c
			$(CC) $(CFLAGS) -c -o cx_f_mol_wrap.o \
			-I$(CX_ROOT)/include \
			cx_f_mol_wrap.c

fellow:		fellow.o cx_f_wrappers.o
		@$(LINKER) $(FFLAGS) -o fellow \
		fellow.o cx_f_wrappers.o $(LIBS) $(SYSLIBS)

felloworlds:	felloworlds.o cx_f_wrappers.o
		@$(LINKER) $(FFLAGS) -o felloworlds \
		felloworlds.o cx_f_wrappers.o $(LIBS) $(SYSLIBS)

clean:
		rm -rf *.o *.a core
