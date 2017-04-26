/*
 * TASK.H -- Each time a new task is run, a task struct is pushed onto the top
 * of the control stack. The struct is popped off when the task dies.
 * This allows recursive task calling.
 *
 * Each TASK directive creates a new ltask struct at the top of the
 *   dictionary and gets linked in at the head of the current package, curpack.
 * Each PACKAGE directive creates a new package struct at the top of the
 *   dictionary and gets linked at pachead.
 *
 * ASSUMES config.h, param.h and stdio.h already include'd.
 */


extern struct task *firstask;	/* pointer to original cl task		*/
extern struct task *newtask;	/* new task being prepared for execing;
				 * not linked in to task list nor does it
				 * become currentask until run.
				 */
extern struct task *currentask;	/* the currently running task		*/
extern struct package *curpack;	/* current package			*/


/* prevtask may be used as a pointer to the previous, ie, parent, task.
 * exploiting c's ability to do pointer arithmetic, it is simple one
 * task up from currentask on the control stack.
 * this is used alot in the builtin commands to gain access to their parent.
 * note that if currentask == firstask, prevtask will point beyond the
 * control stack and should not be used.
 */

/* Added because tp++ will not always be the next task structure. (FJR).
 * NOTE -- Must explicitly coerce to char pointer for correct byte arithmetic
 * on word (rather than byte) addessed machines.
 */
#define next_task(tp)	((struct task *)((char *)tp + (TASKSIZ*BPI)))

#define	prevtask	next_task(currentask)


/* ----------
 * info that is needed about a task as it appears on the control stack
 * while it is running.
 */
struct task {
	FILE	*t_stdin,	/* where xmit/xfer to stdin/out/err go	*/
		*t_stdout,
	     	*t_stderr,
		*t_stdgraph,	/* standard graphics streams		*/
		*t_stdimage,
		*t_stdplot;
	FILE	*t_in,		/* pipe read and write connections	*/
		*t_out;
	char	*ft_in;		/* stdin  file for foreign task		*/
	char	*ft_out;	/* stdout file for foreign task		*/
	char	*ft_err;	/* stderr file for foreign task		*/
	struct ltask *t_ltp;	/* link back to fostering ltask		*/
	unsigned XINT
		t_topd,		/* topd when this task was last pushed	*/
		t_pc,		/* pc			"		*/
		t_topos,	/* topos		"		*/
		t_basos,	/* basos		"		*/
		t_topcs;	/* topcs		"		*/
	XINT	t_envp;		/* environment stack pointer		*/
	int	t_pno;		/* mark package load time in prcache	*/
	struct package *t_curpack;/* curpack		"		*/
	unsigned t_bascode;	/* base addr of currently running code	*/
	int	t_pid;		/* process id of this ptask		*/
	int	t_scriptln;	/* script line number while parsing	*/
	struct	param *t_modep;	/* pointer to this task's `mode' param	*/
	struct	pfile *t_pfp;	/* pointer to pfile			*/
	int	t_flags;	/* see T_XXX flags below		*/
};


/* A leading underscore in the ltask name is used to flag tasks which
 * should not appear in the menus.
 */
#define	CH_INVIS	'_'

/* t_flags */
#define	T_SCRIPT	00000001 /* means t_ltp->lt_flags & LT_SCRIPT >0*/
#define	T_CL		00000002 /* means that t_ltp == firstask->t_ltp	*/
#define	T_INTERACTIVE	00000004 /* T_CL && t_stdio == real stdio	*/
#define	T_BUILTIN	00000010 /* task is built in; see builtin.c	*/
#define	T_FOREIGN	00000020 /* host task, a type of builtin	*/
#define	T_PSET		00000040 /* pset (parameter set) task		*/
#define	T_PKGCL		00000100 /* task is name of a loaded package	*/
#define	T_CLEOF		00000200 /* cl() with EOF on current stream	*/
#define	T_TIMEIT	00000400 /* print time consumed by task		*/


/* These flags are set by the opcodes that change a newtask's pseudofile,
 * such as SETSTDOUT.  Only when the flag is set will the file then be
 * closed by a "bye" or eof from the ltask by clbye().
 */
#define	T_MYOUT		00001000 /* t_stdout was set to exec this task	*/
#define	T_MYIN		00002000 /* t_stdin             "		*/
#define	T_MYERR		00004000 /* t_stderr	        "		*/
#define	T_MYSTDGRAPH	00010000 /* t_stdgraph	        "		*/
#define	T_MYSTDIMAGE	00020000 /* t_stdimage	        "		*/
#define	T_MYSTDPLOT	00040000 /* t_stdplot	        "		*/
#define	T_IPCIO		00100000 /* t_stdout redirected to t_out	*/
#define	T_STDINB	00200000 /* stdin is binary			*/
#define	T_STDOUTB	00400000 /* stdout is binary			*/
#define	T_APPEND	01000000 /* append output of foreign task	*/

/* This flag is set by execnewtask() when a task begins running, and is
 * cleared by iofinish() when the task's i/o is closed down.  Provided so
 * that we can call iofinish at several points during error recovery without
 * trying to close files more than once.
 */
#define	T_RUNNING	02000000

/* When this bit is set we are running unattended as a background cl.
 * Seeing this bit on will prevent pfile writes and all errors and signals
 * will cause immediate io flushing and exit.
 */
#define	T_BATCH		04000000

/* IPCIO definitions. */
#define	IPCOUT		"IPC$IPCIO-OUT"
#define	IPCDONEMSG	"# IPC$IPCIO-FINISHED\n"


/* Struct LTASK -- One of these is created at the top of the dictionary and
 *   gets linked in to its package by each ltask named (or implied) in a TASK
 *   directive.  We need the name of the ltask, filename of the ptask, pointer
 *   to next in list of ltasks on this package, pointer to the parent package
 *   and misc flags.
 * The pointer to the parent package is used to get the prefix for the
 *   ltask's param file when writing it out locally.  Lname is built into the
 *   directionary right after the structure; pname is re-used if possible by
 *   looking to see if another ltask exists in the same package with the same
 *   name.  This is more than a savings of core as its the way connect()
 *   decides if a new ltask is in the currently running ptask (by comparing
 *   currentask->t_ltp->lt_pname with newtask->t_ltp->lt_pname).
 * Note that the ftprefix string cannot be included in the union lt_u as
 *   a foreign task is a builtin and the ltu_f field is already used to point
 *   to the builtin to be run to issue the host command.
 */

struct ltask {
	char *lt_lname;		/* name of this logical task		*/
	union {
		char *ltu_pname;/* name of this ltask's physical file	*/
		void (*ltu_f)();/* function to run for this builtin	*/
	} lt_u;
	char	*lt_ftprefix;	/* OSCMD command prefix for foreign tsk	*/
	struct	ltask *lt_nlt;	/* ptr to next ltask in this package	*/
	struct	package *lt_pkp;/* pointer to parent package		*/
	int	lt_flags;	/* see LT_XXX flags below		*/
};

/* alias's for fields in union lt_u.
 */
#define	lt_pname	lt_u.ltu_pname
#define	lt_f		lt_u.ltu_f


/* lt_flags */
#define	LT_SCRIPT	000001	/* this task is just a script and so is	*/
				/*   the only one in this ptask		*/
#define	LT_PFILE	000002	/* this task has a pfile (some don't!).	*/
#define	LT_STDINB	000004	/* set if task's stdin is binary stream	*/
#define	LT_STDOUTB	000010	/*      "        stdout       "		*/
#define	LT_BUILTIN	000020	/* task is built into CL		*/
#define	LT_FOREIGN	000040	/* host task, called with c_oscmd()	*/
#define	LT_PSET		000100	/* pset (parameter set) task		*/
#define	LT_INVIS	000200	/* don't show this task in menu		*/
#define	LT_PACCL	000400	/* changing packages; see callnewtask()	*/
#define LT_CL		001000	/* task is some variant of cl()		*/
#define LT_CLEOF	002000	/* task is cl with EOF (cleof())	*/
#define	LT_DEFPCK	004000	/* the task def'd a pkg with same name	*/
#define	LT_UPFOK	010000	/* user pfile exists and is valid	*/


/* ----------
 * A package consists of its name, a pointer to next package (maintained in
 * a LIFO fashion off pachead), pointer to first in a list of ltasks in
 * this package, pointer to its in-core pfile, and misc flags (not used so far).
 * the name string is built into the dictionary directly after the struct.
 */
 
struct package {
	char	*pk_name;		/* name of package		*/
	char	*pk_bin;		/* package BIN directory	*/
	struct	package *pk_npk;	/* ptr to next package		*/
	struct	ltask *pk_ltp;		/* ptr to first ltask in pkg	*/
	struct	pfile *pk_pfp;		/* ptr to pkg pfile, if loaded	*/
	int	pk_flags;		/* package flags		*/
};
 
/* pk_flags */
    	/* none at present */


/* ----------
 * size of of the task, ltask, and package structs IN INTS.
 * this is to properly increment pointers within dictionary.
 */
 
#define	TASKSIZ		btoi (sizeof (struct task))
#define	LTASKSIZ	btoi (sizeof (struct ltask))
#define	PACKAGESIZ	btoi (sizeof (struct package))

struct	package *newpac(), *pacfind();
struct	ltask *addltask(), *newltask(), *ltaskfind(), *cmdsrch();
struct	ltask *ltasksrch(), *_ltasksrch();
struct	task *pushtask(), *poptask();
int	deftask(), defpac();
