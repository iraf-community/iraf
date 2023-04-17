#include <stdio.h>
#include <ctype.h>
#define	import_spp
#define	NOKNET
#define	import_knames
#define import_kernel
#include <iraf.h>

#include "../bootProto.h"

char *_os_getenv (char *envvar, char *outstr, int maxch);

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
