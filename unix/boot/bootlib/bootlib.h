#include <stdio.h>
#include <ctype.h>
#define	import_spp
#define	NOKNET
#define	import_knames
#include <iraf.h>

#define	SZ_FBUF		512		/* File i/o buffer size		*/

#ifdef VMS
#define	rindex	strrchr 
struct	timeval {
	long	tv_sec;
	long	tv_usec;
};
#else
#include <sys/time.h>
#endif


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

char	*vfn2osfn();
char	*osfn2vfn();
char	*os_strpak();
XCHAR	*os_strupk();
