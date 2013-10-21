/*
 * CONFIG.H -- Configuration parameters for the IRAF Command Language.
 */

#define SHARELOG	YES	/* share logfile with other processes	*/

/* ----------
 * Total size of combined control and operand stack, in ints.
 *   Note that operands are more than 1 int big, see operand.h for OPSIZ,
 *   and that tasks certainly are too, see task.h.
 * Also, number of INT's dictionary is grown each time topd reaches maxd.
 * NOTE: at present, malloc() calls (such as for fio) will fragment the
 *   dictionary, a fatal error.  We have a static sized dictionary until
 *   this can be fixed.
 */
#define	STACKSIZ	256000
#define	DICTSIZE	1024000
#define	MEMINCR		4096

typedef unsigned long memel;	/* type for dictionary, stack, etc.	*/

/* History and command block buffer dimensions.  The command block buffer
 * must be at least one line in size, and should be large enough to hold
 * most interactively entered multiline command blocks.  The history buffer
 * must be at least as large as the command block buffer.
 */
#define SZ_CMDBLK	2048
#define	SZ_HISTBUF	8192

/* ----------
 * char buffers sizes.
 */

#define	MAXMENU		256	/* largest menu than ? can print	*/
#define	FAKEPARAMLEN	(24)	/* see newfakeparam(). 			*/
#define	LEN_PKPREFIX	3	/* length of package prefix string	*/
#define	LEN_PFILENAME	6	/* length of pfilename in uparm		*/

#define	NBKG		32	/* max number of active background jobs	*/
#define	MAXSUBPROC	10	/* max number cached subprocesses	*/
#define	MAXPIPES	20	/* max pipes in a command		*/

#define	forever		while (!0)
#define	until(x)	while (!(x))

/* Specify the names of the default cl param file and the startup file.
 * All files are assumed to reside in iraf$lib.
 *
 * CLPROCESS is used as the process name to be used to spawn background
 *   processes, and to get the directory where the default cl.par file
 *   may be found.
 * CLSTARTUP is executed, as a script, to set up the initial
 *   evironment defn's, commands, and other stuff. when it starts, the package
 *   "clpackage" and one task, "cl", are the only things defined.
 * used in main().
 * LOGINFILE is the name of the file which, if found in the current directory
 *   when the cl starts, will also be run as a script, after CLSTARTUP.
 * CLLOGOUT is the name of the system logout file, executed when the user
 *   logs off.
 * UPARM is the environment name whose value is used as the directory
 *   for working copies of param files. see pfileread() and pfilewrite().
 */

#define	LOGINFILE	"login.cl"
#define	UPARM		"uparm"
#define	CLPROCESS	"ecl.e"
#define	CLSTARTUP	"clpackage.cl"
#define CLLOGOUT	"cllogout.cl"
#define ROOTPACKAGE	"language"
#define CLPACKAGE	"clpackage"

/* Indefinite valued numbers.
 */

#define INDEFSTR	undefval	/* mode of the param structure.	*/
extern char	*undefval;
