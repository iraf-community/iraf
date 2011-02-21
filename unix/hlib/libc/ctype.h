#define	_U	01
#define	_L	02
#define	_N	04
#define	_S	010
#define _P	020
#define _C	040
#define _X	0100

#ifdef vms
globalvalue   vms_ctype_defs;
#endif
extern	char	u_ctype_[];

#define	isalpha(c)	((u_ctype_+1)[(unsigned int)(c)]&(_U|_L))
#define	isupper(c)	((u_ctype_+1)[(unsigned int)(c)]&_U)
#define	islower(c)	((u_ctype_+1)[(unsigned int)(c)]&_L)
#define	isdigit(c)	((u_ctype_+1)[(unsigned int)(c)]&_N)
#define	isxdigit(c)	((u_ctype_+1)[(unsigned int)(c)]&(_N|_X))
#define	isspace(c)	((u_ctype_+1)[(unsigned int)(c)]&_S)
#define ispunct(c)	((u_ctype_+1)[(unsigned int)(c)]&_P)
#define isalnum(c)	((u_ctype_+1)[(unsigned int)(c)]&(_U|_L|_N))
#define isprint(c)	((u_ctype_+1)[(unsigned int)(c)]&(_P|_U|_L|_N))
#define iscntrl(c)	((u_ctype_+1)[(unsigned int)(c)]&_C)
#define isascii(c)	((unsigned)((int)(c))<=0177)

#define toupper(c)	((c)-'a'+'A')
#define tolower(c)	((c)-'A'+'a')
#define toascii(c)	((c)&0177)
#define	tointeg(c)	((c)-'0')
#define	todigit(c)	((c)+'0')

#define D_ctype
