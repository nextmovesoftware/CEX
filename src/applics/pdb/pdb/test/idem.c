/* $Id: idem.c,v 1.1.1.1 2000/01/13 22:24:14 scott Exp $ */
# include	<stdio.h>
# include	<string.h>
# include	"../pdb.h"

# ifdef __STDC__
extern	int	getopt(int, char **, char *);
extern	void	do_file(FILE *, char *);
# else
extern	int	getopt();
extern	void	do_file();
# endif
extern	int	optind;

main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	renumber;
	int	c;
	FILE	*pdb_file;
	char	*file_name;

	renumber = 0;
	while ((c = getopt(argc, argv, "r")) != EOF) switch (c) {

	case 'r':
		renumber = 1;
		break;

	default:
		(void) fprintf(stderr, "usage: %s [-s] [file(s)]\n", argv[0]);
		exit(1);
		/* NOTREACHED */
	}

	if (optind == argc)
		do_file(stdin, NULL);
	else for ( ; optind < argc; optind += 1) {
		if ((pdb_file = fopen(argv[optind], "r")) == NULL) {
			(void) fprintf(stderr,
					"unable to open %s for reading.\n",
					argv[optind]);
			exit(2);
		}
		if (!renumber)
			file_name = NULL;
		else if ((file_name = strrchr(argv[optind], '/')) != NULL)
			file_name += 1;
		else	
			file_name = argv[optind];
		do_file(pdb_file, file_name);
		(void) fclose(pdb_file);
	}
}

void
do_file(f, fn)
	FILE	*f;
	char	*fn;
{
	int		line_num;
	pdb_record	r;

	line_num = 0;
	do {
		r = pdb_read_record(f);
		pdb_write_record(stdout, &r, fn, ++line_num);
	} while (r.record_type != PDB_END);
}
