# include	<stdio.h>
# include	<stdlib.h>
# include	<string.h>
# include	<ctype.h>
# include	<pdb.h>

int	do_file(const char *name);

int
main(int argc, char **argv)
{
	FILE	*input;
	int	result;

	if (argc <= 1) {
		fprintf(stderr, "usage: compare file(s)\n");
		return 1;
	}

	result = 0;
	for (argc--, argv++; argc > 0; argc--, argv++)
		result |= do_file(*argv);
	return result;
}

int
do_file(const char *name)
{
	FILE	*input;
	char	buf[BUFSIZ];
	FILE	*orig, *new;

	if ((input = fopen(name, "r")) == NULL) {
		fprintf(stderr, "unable to open %s for reading\n", name);
		return 128;
	}
	orig = new = NULL;
	fprintf(stderr, "%s\n", name);
	while (fgets(buf, BUFSIZ, input) != NULL) {
		char		*s, *t;
		pdb_record	r;
		char		outbuf[PDB_BUFSIZ];

		/* strip trailing blanks */
		t = NULL;
		for (s = buf; *s != '\0' && s < &buf[72]; s++)
			if (!isspace(*s))
				t = s + 1;
		if (t == NULL)
			*buf = '\0';
		else
			*t = '\0';

		/* see if libpdb produces the same string that was read in */
		r = pdb_read_string(buf);
		pdb_write_string(outbuf, &r, NULL, 0);
		if (strcmp(buf, outbuf) == 0)
			continue;

		if (orig == NULL) {
			orig = fopen("/usr/tmp/pdb_orig", "w");
			new = fopen("/usr/tmp/pdb_new", "w");
		}
		fprintf(orig, "%s\n", buf);
		fprintf(new, "%s\n", outbuf);
	}
	if (orig != NULL) {
		fclose(orig);
		fclose(new);
		system("/usr/local/bin/spiff -q /usr/tmp/pdb_new /usr/tmp/pdb_orig");
		unlink("/usr/tmp/pdb_orig");
		unlink("/usr/tmp/pdb_new");
	}
	fclose(input);
	return 0;
}
