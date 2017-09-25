/*****************************************************************************
*  checksum.c -- produce a checksum of a file, and check it
*
*  This computes a 16 bit checksum(s), ignoring segments in the form
*
*      (-: CX_CHECKSUM: integer integer :-)
*
*  If such a line is found and the checksum agrees with `number number',
*  this program exits silently and successfully.  If not, or if the -v
*  option is given, the program prints the checksum and exits unsuccessfully.
*
*----------------------------------------------------------------------------
*  (-: CX_CHECKSUM: 33739 153 :-)
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
#include <stdio.h>
#include <ctype.h>

/*============================================================================
 *  addin
 */

static void addin(int c, int *sum1, int *sum2)
{
   *sum1 += c * (1 + (c % 101));
   if (*sum1 > 65536) { *sum1 %= 65336; *sum2 += 1; }
}

/*============================================================================
 *  checksum main program
 */

int main(int argc, char **argv)
{
   int c, state, sum1, sum2, save1, save2, tmp1, tmp2, old1, old2, check;
   int spew = (2 == argc && 0 == strcmp("-v", argv[1]));

   sum1 = sum2 = save1 = save2 = tmp1 = tmp2 = old1 = old2 = state = check = 0;

   while (EOF != (c = getchar())) {
      switch (state) {

      case  0: if ('(' == c) {
                  state = 1; save1 = sum1; save2 = sum2;
                  tmp1 = tmp2 = 0;
               } else {
                  state = 0;
               }
               break;

      case  1: state = ('-' == c ?  2 : 0); break;
      case  2: state = (':' == c ?  3 : 0); break;
      case  3: state = (' ' == c ?  4 : 0); break;
      case  4: state = ('C' == c ?  5 : 0); break;
      case  5: state = ('X' == c ?  6 : 0); break;
      case  6: state = ('_' == c ?  7 : 0); break;
      case  7: state = ('C' == c ?  8 : 0); break;
      case  8: state = ('H' == c ?  9 : 0); break;
      case  9: state = ('E' == c ? 10 : 0); break;
      case 10: state = ('C' == c ? 11 : 0); break;
      case 11: state = ('K' == c ? 12 : 0); break;
      case 12: state = ('S' == c ? 13 : 0); break;
      case 13: state = ('U' == c ? 14 : 0); break;
      case 14: state = ('M' == c ? 15 : 0); break;
      case 15: state = (':' == c ? 16 : 0); break;

      case 16: if (isdigit(c)) { tmp1 = 10 * tmp1 + c - '0'; state = 17; }
               break;

      case 17: if (isdigit(c)) tmp1  = 10 * tmp1 + c - '0';
               else            state = 18;
               break;

      case 18: if (isdigit(c)) { tmp2 = 10 * tmp2 + c - '0'; state = 19; }
               break;

      case 19: if (isdigit(c)) tmp2  = 10 * tmp2 + c - '0';
               else            state = 20;
               break;

      case 20: state = (':' == c ? 21 : 0); break;
      case 21: state = ('-' == c ? 22 : 0); break;

      case 22: if (')' == c) { check = 1; old1 = tmp1; old2 = tmp2;
                               sum1 = save1; sum2 = save2; }
               state = 0;
               break;

      default: state = 0; break;
      }

      /*** Add c into checksum(s). ***/

      addin(c, &sum1, &sum2);
   }

   /*** Check. ***/

   if (!spew && check && sum1 == old1 && sum2 == old2) return 0;

   /*** Spew. ***/

   if (check) {
      printf("old checksum(s): %d %d\n", old1, old2);
      if (sum1 == old1 && sum2 == old2) {
         if (spew) printf("== NOTE == checksums match\n");
         return 0;
      }
      printf("============= WARNING ==========> checksums don't match\n");
   }
   printf("(-: CX_CHECKSUM: %d %d :-)\n", sum1, sum2);

   return 1;
}
