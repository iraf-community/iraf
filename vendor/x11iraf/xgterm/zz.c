#include <stdio.h>

main (int argc, char **argv)
{
  char s[128];

  if (argc) {
    printf ("\033[?38h\035\033\033\014");	/* XTerm OW plus GS/ESC/FF */

/*    printf ("\033[?38h\035^_");		/* XTerm OW plus XGterm OW */

  } else
    printf ("\035^_");				/* XGterm OW		*/
}
