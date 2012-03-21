/* Define variables used during compilation of loop constructs. */
#define MAX_LOOP	50
#define N_OPEN_ARR	15

/* The LABEL structure is used to store the linked list of LABEL names.
 */
struct	label {
	char	*l_name;	/* Pointer to label name. */
	int	l_loc;		/* Location of label. */
	int	l_defined;	/* Has actual label been seen. */
	struct label *l_next;	/* Pointer to next in list. */
	};

/* Pointers to the names of the parameters in a PROCEDURE statement.
 * These are used in positional references to params within a script.
 */

#define MAX_PROC_PARAMS	100

extern int	nextdest[MAX_LOOP];	/* Destinations for NEXT's 	*/
extern int	brkdest[MAX_LOOP];	/* Destinations for BREAK's 	*/

extern int	nestlevel;		/* Loop nesting level		*/
extern int	ncaseval;		/* Number of cases in switch	*/

extern int	n_oarr;			/* Number of open array indices	*/
extern int	i_oarr;			/* Current open array index	*/

extern int	oarr_beg[N_OPEN_ARR];	/* Open index limits.		*/
extern int	oarr_end[N_OPEN_ARR];
extern int	oarr_curr[N_OPEN_ARR];	/* Current value for index.	*/
extern int	imloopset;		/* Loop inited at run time?	*/
extern int	n_indexes;		/* Number of indexes on stack.	*/

extern int	maybeindex;		/* Could last constant be index */
				/* range?			*/

extern struct	label	*label1; /* Pointer to first top of label list. */
extern int	igoto1;		/* Head of list of indirect GOTO's */


extern struct	operand	*parlist[MAX_PROC_PARAMS];
extern struct	param *last_parm;/* Last parameter before compilation.	*/
extern int	n_procpar;		/* Number of params in proc stmt.	*/
