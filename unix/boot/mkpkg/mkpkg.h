/* MKPKG.H -- Global definitions for MKPKG.
 */

#define	SZ_SBUF		10240	/* string buffer size (fixed)		*/
#define	SZ_PBSTK	50	/* push back stack 			*/
#define	SZ_PBBUF	2048	/* push back buffer			*/
#define	SZ_CMD		2048	/* buf for os escape			*/
#define	SZ_IFSTACK	50	/* max $IF nesting			*/
#define	SZ_PREDBUF	1024	/* largest $IF predicate		*/
#define	SZ_PKGENV	256	/* pkgenv package list buffer		*/
#define	MAX_ARGS	50	/* max args to a $IF			*/
#define	MAX_FILES	512	/* max files in a module list		*/
#define MAX_LIBFILES    8192    /* max files in a library index         */
#define	MAX_DEPFILES	100	/* max dependency files			*/
#define	MAX_SYMBOLS	256	/* max macros				*/
#define	MAX_SFDIRS	128	/* max dirs containing special 	files	*/
#define	MAX_SFFILES	1024	/* max special files			*/
#define	MAX_PKGENV	20	/* max package environments		*/

#define	INTERRUPT	SYS_XINT
#define	MKPKGFILE	"mkpkg"
#define	MKPKGINC	"hlib$mkpkg.inc"
#define	PKGENV		"PKGENV"
#define	LFLAGS		"lflags"
#define	XFLAGS		"xflags"
#define	XVFLAGS		"xvflags"
#define	DEBUGSYM	"debug"
#define	XC		"xc"
#define	GENERIC		"generic"
#define	GFLAGS		"gflags"
#define	BACK		".."

#define	BEGIN_CHAR	':'
#define	END_CHAR	';'
#define	SUBDIR_CHAR	'@'
#define COMMENT		'#'
#define PREPROCESSOR	'$'
#define	SYSCMD		'!'
#define SYSFILE_BEGIN	'<'
#define SYSFILE_END	'>'
#define	ESCAPE		'\\'

#define	PASS		1
#define	STOP		0
#define	TOK_FNAME	1
#define	TOK_NEWLINE	2
#define	TOK_BEGIN	3
#define	TOK_END		4
#define	TOK_WHITESPACE	5

/* Pushback structure, used to implement macro expansion.
 */
struct pushback {
	char	*ip;			/* next char to return		*/
	char	*op;			/* next avail char in buffer	*/
	char	*otop;			/* top of buffer		*/
	int	npb;			/* number of pushed ips		*/
	char	*pbstk[SZ_PBSTK];	/* save pushed ips		*/
	char	pbbuf[SZ_PBBUF+1];	/* push back buffer		*/
};

/* Mkpkg context descriptor.
 */
struct context {
	FILE	*fp;			/* mkpkg file descriptor	*/
	long	fpos;			/* saved file pointer		*/
	struct	pushback *pb;		/* pushback descriptor		*/
	int	pbchar;			/* single char pushback		*/
	int	pushback;		/* flag that is pushback	*/
	struct	context *prev;		/* previous mkpkg context	*/
	int	totfiles;		/* total library files updated	*/
	int	nfiles;			/* nfiles last updated		*/
	int	nrfiles;		/* nrfiles last updated		*/
	int	lineno;			/* lineno in mkpkg file		*/
	int	level;			/* subdirectory level		*/
	int	sublib;			/* called from lib module list	*/
	char	*old_cp;		/* old cp when pushing new ctx	*/
	int	old_nsymbols;		/* old nsymbols			*/
	int	old_iflev;		/* old IF stack pointer		*/
	char	*flist[MAX_FILES];	/* file list			*/
	char	*rflist[MAX_FILES];	/* remote file list		*/
	char	curdir[SZ_PATHNAME+1];	/* cwd for printed output	*/ 
	char	dirpath[SZ_PATHNAME+1];	/* os path of cwd		*/
	char	library[SZ_PATHNAME+1];	/* library being updated	*/
	char	libpath[SZ_PATHNAME+1];	/* pathname of library		*/
	char	mkpkgfile[SZ_FNAME+1];	/* mkpkg file being scanned	*/
};

/* Macros.
 */
struct symbol {
	char	*s_name;		/* symbol name			*/
	char	*s_value;		/* symbol value			*/
};

/* Special file list.
 */
struct	sfile {
	char	*sf_stname;		/* standard filename		*/
	char	*sf_sfname;		/* special filename		*/
	char	*sf_mkobj;		/* MKPKG command to make object	*/
	struct	sfile *sf_next;		/* next file in directory	*/
};


/* External functions.
 */
struct	sfile	*sf_dirsearch(), *sf_filesearch();
struct	context *push_context();
struct	context *pop_context();
char	*vfn2osfn();
char	*os_getenv();
char	*mklower();
char	*getargs();
char	*makeobj();
char	*getsym();
char	*putstr();
char	*malloc();
char	*calloc();
long	os_fdate();
long	m_fdate();
char	*index();
char	*k_fgets();
