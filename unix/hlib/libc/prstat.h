/* PRSTAT parameters.
 */
#define	PR_STATUS	1	/* process status (OK, P_DEAD)		*/
#define	PR_INCHAN	2	/* input channel			*/
#define	PR_INFD		3	/* FD of input stream			*/
#define	PR_OUTCHAN	4	/* output channel			*/
#define	PR_OUTFD	5	/* FD of output stream			*/
#define	PR_STDIN	7	/* FD's assigned to pseudofile streams	*/
#define	PR_STDERR	8
#define	PR_STDOUT	9
#define	PR_STDGRAPH	10
#define	PR_STDIMAGE	11

#define	P_RUNNING	0	/* process status			*/
#define	P_BUSY		1	/* process is busy			*/
#define	P_DONE		2	/* process terminated normally		*/
#define	P_DEAD		3	/* process died and sent us X_IPC	*/

#define	D_prstat
