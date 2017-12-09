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

int isalpha(int c);
int isupper(int c);
int islower(int c);
int isdigit(int c);
int isxdigit(int c);
int isspace(int c);
int ispunct(int c);
int isalnum(int c);
int isprint(int c);
int iscntrl(int c);

#define isascii(c)	((unsigned)((int)(c))<=0177)
#define toupper(c)	((c)-'a'+'A')
#define tolower(c)	((c)-'A'+'a')
#define toascii(c)	((c)&0177)
#define	tointeg(c)	((c)-'0')
#define	todigit(c)	((c)+'0')

#define D_ctype
