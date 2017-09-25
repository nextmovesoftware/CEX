/* $Id: strip-pdb.c,v 1.1.1.1 2000/01/13 22:24:14 scott Exp $ */
# include	<stdio.h>
# include	<ctype.h>

main()
{
	char	buffer[BUFSIZ];
	char	*s, *t;

	while (fgets(buffer, BUFSIZ, stdin) != NULL) {
		t = NULL;
		for (s = buffer; *s != '\0' && s < &buffer[72]; s++)
			if (!isspace(*s))
				t = s + 1;
		if (t == NULL)
			*buffer = '\0';
		else
			*t = '\0';
		puts(buffer);
	}
}
