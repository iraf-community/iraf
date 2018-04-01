#include <stdio.h>
#include <ctype.h>
#define	import_spp
#define	NOKNET
#define	import_knames
#include <iraf.h>

#define	SZ_FBUF		512		/* File i/o buffer size		*/

#include <sys/time.h>


# ifdef FINIT
int	bdebug = 0;			/* print debug stuff		*/
int	osfiletype;			/* type of single output file	*/
XCHAR	text[SZ_FBUF];			/* output text line if textfile	*/
XCHAR	*txop;				/* next char in output buf	*/
# else
extern	int bdebug;
extern	int osfiletype;
extern	XCHAR text[];
extern	XCHAR *txop;
# endif

char	*vfn2osfn(char *vfn, int new);
char	*osfn2vfn(char *osfn);
char	*os_strpak(short int *sppstr, char *cstr, int maxch);
XCHAR	*os_strupk(char *str, short int *outstr, int maxch);
