/**
 *  SGIUTIL.H -- Declarations for the SGI utility routines.
 */

void  bswap2 (unsigned char *a, unsigned char *b, int nbytes);
void  bswap4 (unsigned char *a, unsigned char *b, int nbytes);

int   isSwapped (void);

int   get_iarg (char argp, char **argv, int argno, int def_val);
