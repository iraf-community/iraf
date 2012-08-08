/*
 * EPARAM.H -- Definition of the string editing capabilities.  The mapping
 * of the commands is defined by the *.ed files in DEV.
 */

#define FIRST_CMD	3	/* first command escape sequence	*/
#define	NUM_COMMANDS	35	/* number of recognized commands	*/
#define	MAX_COMMANDS	50	/* max commands recognized by edcap	*/
#define	SZ_ESCAPE	10	/* terminal escape sequence		*/
#define	SZ_KEYSTROKE	12	/* keystroke command name		*/

#define	G_TOPLINE	6	/* top of eparam scrolling region	*/
#define	G_BOTLINE	22	/* bottom of eparam scrolling region	*/
#define	G_STARTCOL	11	/* start of eparam edit area		*/
#define	G_CMDLINE	24	/* command line for messages & exit	*/

#define	G_MAXPARAM	100	/* maximum number of parameters		*/
#define	G_MAXPAGES	12	/* maximum number of pages		*/
#define	G_MAXSTRING      80	/* maximum size of the edit string	*/
#define	G_BIGSIZE	2048	/* sum of sizes of value fields		*/
#define	MAXPROMPT	2048	/* maximum characters in multiline pr.	*/
#define	PROMPTOFFSET	32	/* where the prompt starts		*/
#define	VALUEOFFSET	11	/* where the value field starts		*/
#define	MAX_ON_ROW	6	/* the number of %10.10s fields		*/

#define	FWD		1
#define	AFT		0

/* eparam() context structure.
 */
struct ep_context {
	int	e_init;			/* set on first call		*/
	XINT	e_topd;			/* save top of dictionary	*/
	int	e_topkey;		/* saved context variables	*/
	int	e_line;			/* 		"		*/
	int	e_col;			/* 		"		*/
	int	e_nextkey;		/* 		"		*/
	int	e_nextline;		/* 		"		*/
	struct	pfile *e_mpfp;		/* master pfile descriptor	*/
	struct	pfile *e_cpfp;		/* pfilecopy descriptor		*/
	char	e_pset[SZ_FNAME+1];	/* pset name (task or file)	*/
};

/* eparam() colon commands and exit status codes.
 */
#define	EP_EOF		1		/* update pfile and pop context	*/
#define	EP_EDIT		2		/* discard context and edit	*/
#define	EP_DESCEND	3		/* push context and edit pfile	*/
#define	EP_RUN		4		/* exit and run task		*/

/* Editor initialization and termination sequences (these have to be first
 * in case a 'define key' capability is added).
 */
#define EDITOR_ID	0	/* editor's name		 	*/
#define EDIT_INIT	1	/* editor initialization sequence	*/
#define EDIT_TERM	2	/* editor termination sequence		*/

/* edit commands	*/

#define	MOVE_UP		3	/* move the cursor up one line */
#define MOVE_DOWN	4	/* move the cursor down one line */
#define MOVE_RIGHT	5	/* move the cursor one char to the right */
#define MOVE_LEFT	6	/* move the cursor one char to the left */
#define NEXT_WORD	7	/* move the cursor one word to the right */
#define PREV_WORD	8	/* move the cursor one word to the left */
#define MOVE_EOL	9	/* move the cursor to the end of line */
#define MOVE_BOL	10	/* move the cursor to the beginning */
#define NEXT_PAGE	11	/* move to the next page */
#define PREV_PAGE	12	/* move to the previous page */
#define MOVE_START	13	/* move to the start of the text */
#define MOVE_END	14	/* move to the end of the text */

/* these commands are for EDT type editors */
#define SET_FWD		15	/* set the direction forwards */
#define SET_AFT		16	/* set the direction aftwards */
#define TOGGLE_DIR	17	/* change the direction */

#define DEL_LEFT	18	/* delete the character to the left */
#define DEL_CHAR	19	/* delete the character under the cursor */
#define DEL_WORD	20	/* delete up to and including next delimiter */
#define DEL_LINE	21	/* delete up to the end of line */
#define UNDEL_CHAR	22	/* undelete the character */
#define UNDEL_WORD	23	/* undelete the word */
#define UNDEL_LINE	24	/* undelete the line */

#define FIND_FWD	25	/* find forward	*/
#define FIND_AFT	26	/* find aftward	*/
#define FIND_NEXT	27	/* find next */
#define GET_HELP	28	/* display help information */
#define REPAINT		29	/* clear and repaint the screen */
#define EXIT_UPDATE	30	/* exit the editor */
#define EXIT_NOUPDATE	31	/* exit the editor with no update */

#define NEXT_LINE	32	/* move to the next line */
#define NOMORE_COMMANDS 99	/* last command terminator */

struct edit_commands {
	int  cmd;
	char escape[SZ_ESCAPE+1];
	char keystroke[SZ_KEYSTROKE+1];
};

extern struct edit_commands command[MAX_COMMANDS];
extern char *cmdnames[MAX_COMMANDS];
extern int numcommands;

char	*enumin(), *minmax();
char	*host_editor();
