MAKE = pmake
all: strip-pdb idem

strip-pdb: strip-pdb.c
	${CC} -o strip-pdb strip-pdb.c

idem: idem.c ../pdb.h ../obj/libpdb.a
	${CC} -I.. -o idem idem.c ../obj/libpdb.a

pdbdiff: pdbdiff.c ../pdb.h ../obj/libpdb.a
	${CC} -I.. -o pdbdiff pdbdiff.c ../obj/libpdb.a

idem2: idem.c
	${CC} -o idem idem.c -L/usr/local/lib -lpdb

clean:
	rm -f *.o strip-pdb idem
