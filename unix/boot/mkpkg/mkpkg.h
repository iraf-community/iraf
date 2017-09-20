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
struct	sfile	*sf_dirsearch(char *dirname), *sf_filesearch(struct sfile *sflist, char *stname);
struct	context *push_context(register struct context *cx, char *module, char *newdir, char *fname);
struct	context *pop_context(register struct context *cx);
char	*vfn2osfn(char *, int);
char	*os_getenv(char *);
char	*mklower(char *s);
char	*getargs(register struct context *cx);
char	*makeobj(char *fname);
char	*getsym(char *name);
char	*putstr(char *s);
long	os_fdate(char *);
long	m_fdate(char *fname);
char	*k_fgets(char *op, int maxch, register struct context *cx);


/*****************************************************************************/

/*  main.c  */
void warns (char *fmt, char *arg);
void fatals (char *fmt, char *arg);


/*  char.c  */
int   m_getc (register struct context *cx);
int   m_rawgetc (register struct context *cx);
void  m_ungetc (int ch, struct context *cx);
void  m_pushstr (struct context *cx, char *str);
void  mk_pbbuf (register struct context *cx);
void  pb_cancel (register struct context *cx);
char *putstr (char *s);

int   k_getc (register struct context *cx);
char *k_fgets (char *obuf, int maxch, register struct context *cx);
int   k_fseek (register struct context *cx, long offset, int type);
long  k_ftell (register struct context *cx);


/*  fdcache.c  */
long  m_fdate (char *fname);
void  m_fdinit (int debug);
int   fd_chksum (char *s);


/*  fncache.c  */
int   m_sysfile (char *lname, char *fname, int maxch);
void  m_fninit (int debug);
int   fn_chksum (char *s);
int   fn_strncpy (char *out, char *in, int maxch);


/*  host.c  */ 
int   h_updatelibrary (char *library, char *flist[], int totfiles, 
		char *xflags, char *irafdir);
int   h_rebuildlibrary (char *library);
int   h_incheck (char *file, char *dir);
int   h_outcheck (char *file, char *dir, int clobber);
void  h_getlibname (char *file, char *fname);
int   h_xc (char *cmd);
int   h_purge (char *dir);
int   h_copyfile (char *oldfile, char *newfile);

int   u_fcopy (char *old, char *new);
int   h_movefile (char *old, char *new);
int   u_fmove (char *old, char *new );

int   add_sources (char *cmd, int maxch, char *flist[], 
		int totfiles, int hostnames, int  *nsources);
int   add_objects (char *cmd, int maxch, char *flist[], 
		int totfiles, int hostnames);

char *makeobj (char *fname);
char *mkpath (char *module, char *directory, char *outstr);
char *resolvefname (char *fname);
int   h_direq (char *dir1, char *dir2);


/*  pkg.c  */
int   do_mkpkg (struct context *cx, int islib);
int   scan_modlist (struct context *cx, int islib);
void  parse_modname (char *modname, char *module, char *subdir, char *fname);
void  parse_fname (char *path, char *dname, char *fname);
struct context *push_context (register struct context *cx, char *module,
                char *newdir, char *fname);
struct context *pop_context (register struct context *cx);
void  get_dependency_list (struct context *cx, char *module, 
                char *dflist[], int maxfiles);
int   up_to_date (struct context *cx, char *module, char *lname, 
                char *dflist[], int *useobj);
int   open_mkpkgfile (register struct context *cx);
void  close_mkpkgfile (register struct context *cx);
struct context *find_mkpkgfile ( struct context *head_cx, 
                char *mkpkgfile, int level);
int   search_mkpkgfile (register struct context *cx);


/*  tok.c  */
int   gettok (register struct context *cx, char *outstr, int maxch );

void  do_osescape (register struct context *cx);
void  do_ppdir (struct context *cx, char *token);
void  do_if (struct context *cx, char *keyword);
void  do_else (struct context *cx);
void  do_endif (struct context *cx);
void  do_end (struct context *cx);
void  do_call (struct context *cx, char *program, int  islib);
void  do_echo (struct context *cx, char *msg);
int   do_goto (struct context *cx, char *symbol);
int   do_include (struct context *cx, char *fname);
void  do_omake (struct context *cx, char *fname);
int   do_xc (struct context *cx);
int   do_link (struct context *cx);
int   do_generic (struct context *cx);
void  do_set (struct context *cx);
int   do_incheck (struct context *cx);
int   do_outcheck (struct context *cx);
int   do_copyfile (struct context *cx);
int   do_movefile (struct context *cx);
void  do_delete (struct context *cx);
void  do_purge (struct context *cx, char *dname);

int   getcmd (register struct context *cx, char *prefix, char *cmd, int maxch);
char *getargs (register struct context *cx);
int   getstr (register struct context *cx, char *outstr, int maxch, int delim);
int   getkwvpair (register struct context *cx, char *symbol, char *value);
int   getword (char **str, char *outstr, int maxch);
void  putsym (char *name, char *value);
char *getsym (char *name);
char *mklower (char *s);


/*  sflist.c  */
int    sf_scanlist (struct context *cx);
struct sfile *sf_dirsearch (char *dirname);
struct sfile *sf_filesearch (struct sfile *sflist, char	*stname);
void   sf_prune (register char *cp);


/*  scanlib.c  */
int   h_scanlibrary (char *library);
long  h_ardate (char *fname);
int   mlb_setdate (char *modname, long fdate);
long  mlb_getdate (char *modname);
