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
