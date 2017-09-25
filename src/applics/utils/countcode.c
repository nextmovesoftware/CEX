/*****************************************************************************
*  countcode.c -- count lines in C code
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
#include <ctype.h>

#define CODE    0
#define COMCODE 1
#define COMMENT 2
#define EMPTY   3

/*============================================================================
 *  linekind: returns CODE, COMCODE, COMMENT, EMPTY
 */

int linekind(char *line, int *incomment)
{
   char *p, c;
   int hascom = 0, hascode = 0;

   /*** Skip to first non-whitespace. ***/

   for (p = line; *p && isspace(*p); p++)
      ;

   /*** If whitespace through end-of-line, the line is EMPTY or COMMENT. ***/

   if (0 == *p) return (*incomment ? COMMENT : EMPTY);

   /*** Parse for comment, count comment and code characters. ***/

   while ( *p ) {
      c = *p++;
      /*** Check for start-of-comment. ***/

      if ('/' == c && '*' == *p) *incomment = 1;

      /*** Increment counters. ***/

      if      (*incomment)  hascom++;
      else if (!isspace(c)) hascode++;

      /*** Check for end-of-comment. ***/

      if ('*' == c && '/' == *p) { p++; hascom++; *incomment = 0; }
   }

   /*** Return line type: code, comment, or both. ***/

   if (hascom && hascode) return COMCODE;
   if (hascom)            return COMMENT;
   if (hascode)           return CODE;

   fprintf(stderr, "oops: %s\n", line);
   return EMPTY;
}

/*============================================================================
 *  countcode main()
 */

int main(int argc, char *argv[])
{
   char  *p, buf[2000];
   int    i, lens, kind, iarg, ncod, ncom, ntot, counts[4], totals[4];
   int    incomment = 0;      /* not in comment mode initially */
   int    verb      = 0;      /* not verbose until -v option encountered */
   int    nin       = 0;      /* number of input files processed ***/
   FILE  *fp       = stdin;  /* use standard input until file opened */

   /*** Initialize total. ***/

   for (i = 0; i < 4; i++)
      totals[i] = 0;

   /*** Loop over files if provided, else read standard input. ***/

   for (iarg = 0; iarg < argc; iarg++) {
      if (0 == iarg && 1 < argc) continue;
      if (0 < iarg) {
	 if (0 == strcmp("-v", argv[iarg])) {
	    verb = 1;
            continue;
	 } else if (0 == strcmp("-t", argv[iarg])) {
	    verb = 0;
            continue;
	 } else {
            fp = fopen(argv[iarg], "r");
            if (NULL == fp) {
               fprintf(stderr, "can't open %s\n", argv[iarg]);
               continue;
            }
         }
      }

      /*** Initialize. ***/

      for (i = 0; i < 4; i++)
         counts[i] = 0;

      /*** Loop over input lines, characterizing them. ***/

      while (NULL != fgets(buf, 2000, fp)) {
         kind = linekind(buf, &incomment);
         counts[kind]++;

	 /*** Spew if needed. ***/

	 if (verb) {

	    /*** Measure string length, converting \n & adjusting for \t. ***/

	    for (lens = 0, p = buf; *p; p++) {
	       if      ('\t' == *p) { do { } while (++lens % 8); }
	       else if ('\n' == *p) { *p = '\0'; break; }
	       else                 { lens++; }
	    }

	    /*** Print kind. ***/

	    if (55 < lens) buf[55] = '\0';
            switch(kind) {
               case CODE:    printf("Code:    "); break;
               case COMCODE: printf("ComCode: "); break;
               case COMMENT: printf("Comment: "); break;
               case EMPTY:   printf("Empty:   "); break;
            }

	    /*** Print up to first 55 characters of line. ***/

	    for (i = 0, p = buf; i < 55; i++) {
	       if (0 == *p) {
		  printf(" ");
	       } else if ('\t' == *p) {
		  printf(" ");
		  while ((i + 1) % 8) { printf(" "); i++; }
	       } else                 {
		  printf("%c", *p);
               }
	       if (*p) p++;
	    }

	    /*** Print last 12 characters of line. ***/

	    if (55 >= lens) {
	       printf(" |");
	    } else if (p >= buf + lens - 12) {
	       printf(" | %s", p + 1);
	    } else {
	       printf(" | %s", buf + lens - 12);
	    }
	    printf("\n");
         }
      }

      fclose(fp);

      /*** Accumulate totals. ***/
   
      nin++;

      for (i = 0; i < 4; i++)
         totals[i] += counts[i];

      /*** Show counts. ***/

      printf("%-16s %6d code, %6d commented, %6d comments, %6d empty\n",
             argv[iarg], counts[0], counts[1], counts[2], counts[3]);

   }

   /*** If more than one file processed, show totals. ***/

   if (1 < nin) {
      printf("                 ------       ------"
	     "            ------           ------\n");

      printf("%-16s %6d code, %6d commented, %6d comments, %6d empty\n",
             "total", totals[0], totals[1], totals[2], totals[3]);
   }

   /*** Show summary. ***/

   ncod = totals[0] + totals[1];
   ncom = totals[1] + totals[2] + totals[3];
   ntot = totals[0] + ncom;
   printf("\nsummary: %6d code lines (%.1f%%), %6d comment lines (%.1f%%)\n",
          ncod, (100.0 * ncod) / (float) ntot,
          ncom, (100.0 * ncom) / (float) ntot );
   return 0;
}
