/* EXTERN.H -- External static variables.
 */
extern	char	sbuf[];			/* string buffer		*/
extern	struct	symbol symtab[];	/* symbol table (macros)	*/
extern	struct	context *topcx;		/* currently active context	*/
extern	char	*cp;			/* pointer into sbuf		*/
extern	char	*ctop;			/* top of sbuf			*/
extern	char	irafdir[];		/* iraf root directory		*/
extern	int	nsymbols;		/* number of defined symbols	*/
extern	int	ifstate[];		/* $IF stack			*/
extern	int	iflev;			/* $IF stack pointer		*/
extern	int	debug;			/* print debug messages		*/
extern	int	dbgout;			/* compile for debugging	*/
extern	int	verbose;		/* print informative messages	*/
extern	int	ignore;			/* ignore warns			*/
extern	int	execute;		/* think but don't act?		*/
extern	int	exit_status;		/* exit status of last syscall	*/
extern	int	forceupdate;		/* foribly update libmod dates	*/

/* Public functions.
 */
/* char.c */
extern int m_getc ( struct context * );
extern int m_rawgetc ( struct context * );
extern void m_ungetc ( int, struct context * );
extern void m_pushstr ( struct context *, const char * );
extern int k_getc ( struct context * );
extern char *k_fgets ( char *, size_t, struct context * );
extern int k_fseek ( struct context *, long, int );
extern long k_ftell ( struct context * );
extern char *putstr ( const char * );
/* fdcache.c */
extern void m_fdinit ( int );
extern long m_fdate ( const char * );
/* fncache.c */
extern int m_sysfile ( const char *, char *, size_t );
extern void m_fninit ( int );
/* main.c */
extern void fatals ( const char *, const char * );
extern void warns ( const char *, const char * );
/* scanlib.c */
extern int h_scanlibrary ( const char * );
extern long h_ardate ( const char * );
/* sflist.c */
extern int sf_scanlist ( struct context * );
extern struct sfile *sf_filesearch ( struct sfile *, const char * );
extern struct sfile *sf_dirsearch ( const char * );
extern void sf_prune ( char * );
/* pkg.c */
extern int do_mkpkg ( struct context *, int );
extern void parse_modname ( const char *, char *, char *, char *, size_t );
extern void parse_fname ( const char *, char *, char *, size_t );
extern struct context *push_context ( struct context *, const char *, 
				      const char *, const char * );
extern struct context *pop_context ( struct context * );
extern void get_dependency_list ( struct context *, 
				  const char *, const char *[], int );
/* host.c */
extern int h_updatelibrary ( const char *, char *const [], int, 
			     const char *, const char * );
extern int h_rebuildlibrary ( const char * );
extern int h_incheck ( const char *, const char * );
extern int h_outcheck ( const char *, const char *, int );
extern void h_getlibname ( const char *, char *, size_t );
extern int h_xc ( const char * );
extern int h_purge ( const char * );
extern int h_copyfile ( const char *, const char * );
extern int h_movefile ( const char *, const char * );
extern int h_direq ( const char *, const char * );
extern const char *makeobj ( const char * );
/* tok.c */
extern int gettok ( struct context *, char *, size_t );
extern int do_goto ( struct context *, const char * );
extern int do_include ( struct context *, const char * );
extern void putsym ( const char *, const char * );
extern char *getsym ( const char * );
