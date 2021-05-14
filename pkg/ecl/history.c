/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_fset
#define import_ctype
#include <iraf.h>

#include "config.h"
#include "errs.h"
#include "mem.h"
#include "operand.h"
#include "param.h"
#include "task.h"
#include "clmodes.h"
#include "grammar.h"
#include "proto.h"


/*
 * HISTORY.C -- Routines for character input to the parser (actually,
 * the lexical analyser).  Includes the history mechanism, the logfile,
 * and prompting.
 */

extern	int cldebug;

#define	HISTCHAR	'^'	/* primary history metacharacter	*/
#define	FIRSTARG	'^'	/* first argument macro ("^^")		*/
#define	LASTARG		'$'	/* last argument macro ("^$")		*/
#define	ALLARGS		'*'	/* all arguments macro ("^*")		*/
#define	ARGCHARS	"$^*"	/* argument substitution chars		*/
#define	MATCH_ANYWHERE	'?'	/* match string anywhere in cmd		*/
#define	MATCH_ALL	'g'	/* match all occurrences		*/
#define	NO_EXECUTE	":p"	/* print but do not execute command	*/
#define	MAXCOL		80	/* form width for formatting output	*/
#define	SZ_LOGBUF	1024	/* putlog buffer size			*/

#define	EOS		'\0'
#define	NOCLOSURE	">>"	/* parser needs more input (pprompt)	*/
#define	MAX_SHOWHIST	800	/* maximum history cmds to show		*/

/* History, command block, yy_getc, logfile database.
 */
char	raw_cmdblk[SZ_CMDBLK+1];/* saves raw command for history (for scripts)*/
char	cmdblk[SZ_CMDBLK+1];	/* command block buffer			*/
char	prompt[SZ_CMDBLK+1];	/* command prompt			*/
char	*op_cmdblk=cmdblk;	/* next output line in cmdblk		*/
char	*ip_cmdblk=cmdblk;	/* next input char in cmdblk		*/
int	cmdblk_line=0;		/* line number within cmd block		*/
int	cmdblk_save=0;		/* set if cmdblk filled interactively	*/

char	histbuf[SZ_HISTBUF+1];	/* history buffer			*/
char	*op_hist=histbuf;	/* next location in history buffer	*/
int	histbuf_full=0;		/* set when buffer wraps around		*/
int	share_logfile=SHARELOG;	/* share logfile with other processes?	*/

FILE	*logfp=NULL;		/* file pointer for command logfile	*/
int	histnum = 0;		/* history command block number		*/
int	history_number;		/* the current history record		*/

extern	int _lexmodes;		/* enable lexical mode switching	*/
extern	char *ifseen;		/* Processing an IF statement?		*/

extern	int do_error;		/* Are we processing errors?		*/

extern	void *memset();
char   *freadline (char *prompt);
int     add_history (char *buf);


/* YY_GETC -- Called by the modified yylex() "input" macro in the lexical
 * analysis stage of the parser to get the next character from the input
 * stream.  When EOF is reached on the stream, add the "bye" command to
 * the logfile.
 */
int
yy_getc (FILE *fp)
{
	register char ch;

	while ((ch = *ip_cmdblk++) == EOS)
	    if (get_command (fp) == EOF) {
		if (currentask->t_flags & T_INTERACTIVE)
		    if (log_commands())
		        put_logfile ("bye\n");
		return (EOF);
	    }

	return (ch);
}


/* YY_STARTBLOCK -- Terminate the last command block and start a new one.
 * Save old command block in history (if interactive) and in logfile (if
 * interactive, logging is enabled, and logflag argument is true).  Even
 * if logging is enabled, a command will not be logged which aborts or is
 * interrupted.
 */
void
yy_startblock (int logflag)
{
	register char *ip;

	if (cldebug)
	    eprintf ("startblock (%d)\n", logflag);

	/* Log cmdblk only if it was filled by an interactive task.  We must
	 * make the test when the new block is initialized since the write is
	 * delayed.
	 */
	if (cmdblk_save) { 
	    /* Do not record commands which consist only of whitespace.
	     */
	    for (ip=cmdblk;  isspace (*ip);  ip++)
		;
	    if (*ip != EOS) {
		/* Use the raw_cmdblk, saved in get_command().
		 */
		put_history (raw_cmdblk);
		if (logflag && log_commands())
		    put_logfile (raw_cmdblk);
	    }
	}

	if (cldebug) 
	    eprintf ("startblock: ifseen=%d\n", ifseen);

	if (!ifseen) {
	    ip_cmdblk = op_cmdblk = cmdblk;
	    *ip_cmdblk = EOS;
	}
	cmdblk_line = 0;
	cmdblk_save = (currentask->t_flags & T_INTERACTIVE);

	/* Mode switching of the lexical analyzer is enabled by this call
	 * if the CL parameter lexmodes is set.  Called between blocks
	 * entered interactively and also during error recovery.
	 */
	lexinit();
}


/* CURCMD -- Return a pointer to the command block currently being interpreted.
 */
char *
curcmd (void)
{
	return (cmdblk);
}


/* GET_COMMAND -- Get command line from the input stream.  If not interactive,
 *   all we do is read the line into the cmdblk buffer.  If called when parsing
 *   command input to an interactive task, we must output a prompt before
 *   reading in the command line.  The prompt changes depending on whether or
 *   not the command is the first in a command block (whether or not we have
 *   closure).  After reading the command, we check if it is a history directive
 *   and process it if so.  Otherwise we must still process it to expand any
 *   history macros.  Ignore all blank or comment lines.  These are
 *   any line in which the first non-blank character is a newline or a
 *   '#'.  This will make some things a bit more efficient, but is
 *   actually to allow the if/else parsing to work properly.
 *
 * N.B.: We must directly or indirectly set ip_cmdblk so that yy_getc takes
 *   the next character from the right place.  This is either done directly
 *   or by a call to yy_startblock.
 */
int
get_command (FILE *fp)
{
	register char *ip, *op;
	char	raw_cmd[SZ_LINE+1];	/* buffer for raw command line	*/
	char	new_cmd[SZ_CMDBLK+1];	/* temporary for processed cmd	*/
	int	execute=1, temp, status;

	if (!(currentask->t_flags & T_INTERACTIVE)  ||
	    parse_state == PARSE_PARAMS) {

	    /* Ensure that searches through string terminate. */
	    cmdblk[SZ_LINE] = '\0';
	    ip_cmdblk = cmdblk;

	    while (YES) {
 		currentask->t_scriptln++;	/* noninteractive mode	*/

		status = (fgets (cmdblk, SZ_LINE, fp) == NULL ? EOF : OK);
		if (status == EOF) {
		    cmdblk[0] = '\0';
		    break;
		}

		/* Check if this is a blank line. */
		for (ip = cmdblk;  *ip == ' ' || *ip == '\t';  ip++)
		    ;
		if (*ip == '\n' || *ip == '\0')
		    continue;

		/* Check for the #{ ... #} lexmode toggle sequences.  These
		 * are matched only at the beginning of a line.  #{ sets
		 * command mode on the command input stream and #} clears it.
		 */
		if (*ip == '#') {
		    if (ip == cmdblk) {
			if (*(ip+1) == '{') {
			    lex_setcpumode (fp);
			    lexinit();
			} else if (*(ip+1) == '}') {
			    lex_clrcpumode (fp);
			    lexinit();
			}
		    }
		    continue;
		}

		break;
	    }

	    if (cldebug || echocmds())
		eprintf ("%s", status == EOF ? "bye\n" : cmdblk);

	    return (status);
	}

	raw_cmd[SZ_LINE] = '\0';
	while (YES) {
	    /* Prompt the user for a new command if the input buffer is empty.
	     * The CL prompt clears raw mode in case it is left in effect by a
	     * program abort.
	     */
input_:
	    /* Read the next command line. 
	     */

	    if (eh_readline == NO) {
	        if (c_fstati (fileno(fp), F_UNREAD) == 0) {
		    if (c_fstati ((XINT)STDIN, F_RAW) == YES)
		        c_fseti ((XINT)STDIN, F_RAW, NO);
		    if (cmdblk_line == 0)
		        pprompt (curpack->pk_name);
		    else
		        pprompt (NOCLOSURE);
	        }

	    	if (fgets (raw_cmd, SZ_LINE, fp) == NULL)
		    return (EOF);

	    } else {
		extern char epar_cmdbuf[];

		/* If the epar/ehist command buffer is full, process that
		 * rather than taking input from the terminal.
		 */
    		c_fseti ((XINT)STDIN, F_CANCEL, OK);
    		c_fseti ((XINT)fileno(fp), F_CANCEL, OK);

		if (*epar_cmdbuf) {
    		    strcpy (raw_cmd, epar_cmdbuf);
		    epar_cmdbuf[0] = '\0';

		} else {
		    char *cmd = (char *)NULL;

		    get_prompt((cmdblk_line==0) ? curpack->pk_name : NOCLOSURE);
    		    if ((cmd = freadline (prompt)) == (char *)NULL)
			return (EOF);
    		    strcpy (raw_cmd, cmd);
    		    strcat (raw_cmd, "\n");
		}
	    }

	    /* Check for the #{ ... #} lexmode toggle sequences.  These
	     * are matched only at the beginning of a line.  #{ sets
	     * command mode on the command input stream and #} clears it.
	     */
	    if (*(ip=raw_cmd) == '#') {
		if (*(ip+1) == '{') {
		    lex_setcpumode (fp);
		    lexinit();
		} else if (*(ip+1) == '}') {
		    lex_clrcpumode (fp);
		    lexinit();
		}
	    }

	    /* Skip leading whitespace. */
	    for (ip=raw_cmd;  *ip == ' ' || *ip == '\t';  ip++)
		;

	    /* For interactive comments, make sure we store them in the
	     * history and the logfile.  This is so that users can add
	     * comments into the logfile interactively.
	     */
	    if (*ip == '#') {
		put_history (raw_cmd);
	  	if (log_commands())
		    put_logfile (raw_cmd);
	    } else if (*ip != '\n' && *ip != '\0') {
		cmdblk_line++;
		break;
	    }
	}

	/* If history directive, transform the directive into an executable
	 * command block using the history data.  Echo the new command as
	 * if the user had typed it, for verification.
	 */
	if (*raw_cmd == HISTCHAR) {
	    /* Use screen style history editing only if the CL parameter
	     * "ehinit" contains the boolean variable "verify" (or if the 
	     * cmd is "ehistory", below).
	     */
	    if (eh_verify)
		execute = edit_history_directive (raw_cmd+1, new_cmd);
	    else {
		execute = process_history_directive (raw_cmd, new_cmd);
		fputs (new_cmd, currentask->t_stdout);
	    }

	} else if (expand_history_macros (raw_cmd, new_cmd)) {
	    fputs (new_cmd, currentask->t_stdout);

	} else {
	    static  char ehist[] = "ehistory";
	    int     n;

	    for (n=0, ip=raw_cmd, op=ehist;  (*ip == *op);  ip++, op++)
		n++;
	    if (n > 0 && isspace (*ip)) {
		while (isspace (*ip))
		    ip++;
		execute = edit_history_directive (ip, new_cmd);
	    }
	}

	/* If user deletes entire line go back and get another command.  */
	for (ip=new_cmd;  isspace (*ip);  ip++)
	    ;
	if (*ip == EOS) {
	    cmdblk_line = 0;
	    execute = 1;
	    goto input_;
	}

	/* Now move the processed command into the cmdblk buffer.  If there
	 * is not enough storage remaining in the cmdblk buffer, we have to
	 * break the actual (large) command block up, calling yy_startblock to
	 * start a new block, but without changing the line number within the
	 * block.  We must not let the history mechanism limit the size of a
	 * command block.
	 */
	op_cmdblk = ip_cmdblk - 1;		/* back up to EOS	*/
	if (strlen (new_cmd) > (cmdblk + SZ_CMDBLK - op_cmdblk)) {
	    temp = cmdblk_line;
	    yy_startblock (LOG);
	    cmdblk_line = temp;
	}
	ip_cmdblk = op = op_cmdblk;
	for (ip=new_cmd;  (*op++ = *ip++) != EOS;  )
	    ;

	/* Save the "raw command" here for use in yy_startblock.  This is
	 * to handle the problem of procedure script parsing overwriting
	 * the raw command in cmdblk.  Save immediate mode and escapes, 
	 * but don't save newlines.
	 */
	strcpy (raw_cmdblk, cmdblk);
    	if (isalpha (cmdblk[0]) || 
	    cmdblk[0] == '=' || cmdblk[0] == '!' || cmdblk[0] == '$') {
	        int len = strlen (cmdblk);
	        char buf[SZ_CMDBLK];

	        memset (buf, 0, SZ_CMDBLK);
	        strncpy (buf, cmdblk, len-1);	/* trounce the NL we do have */
	        add_history (buf);
	}

	if (!execute)
	    yy_startblock (NOLOG);

	fflush (currentask->t_stdout);
	return (OK);
}


/* Dummy procedures for platforms where we don't have GNU readline().
*/
#ifdef NO_READLINE
char *
freadline (char *prompt) { }
int 
add_history (char *buf)    { }
#endif



/* PROCESS_HISTORY_DIRECTIVE -- Transform a history directive into an
 * executable command or command block.  There are two classes of
 * directives: (1) string substitution editing of the last command block,
 * and (2) search for an earlier command by some means and return that.
 * If ":p" follows a directive, we generate the command and return false
 * (no execute) as the function value.  Any text which follows the directive
 * is appended to the new command block.
 */
int
process_history_directive (char *directive, char *new_command_block)
{
	register char *ip, *op, *p;
	char	last_command_block[SZ_CMDBLK+1];
	int	execute=1, edit=0;
	int	record;
	char	*rindex();

	ip = directive + 1;			/* skip the '^'		*/
	op = new_command_block;

	/* Chop the newline. */
	if ((p = rindex (ip, '\n')) != NULL)
	    *p = EOS;

	/* Scan the directive string to determine whether or not we have
	 * an edit directive.  We have an edit directive if there is a second
	 * (unescaped) history metacharacter in the directive.
	 */
	for (p=ip, edit=0;  *p != EOS;  p++)
	    if (*p == '\\' && *(p+1) != EOS)
		p++;
	    else if (*p == HISTCHAR) {
		edit = 1;
		break;
	    }

	/* Directives "^^", "^str1^str2^", and "^str1^str2^g". */
	if (edit) {
	    /* Get last command and edit it */
	    if (get_history (1, last_command_block, SZ_CMDBLK) == ERR)
		cl_error (E_UERR, "Nothing in history buffer to edit");
	    ip = directive +
		stredit (directive, last_command_block, new_command_block);

	/* Directives "^absnum" and "-relnum". */
	} else if ((*ip == '-' && isdigit (*(ip+1))) || isdigit (*ip)) {
	    if (*ip == '-')
		record = -atoi(ip++);
	    else
		record = histnum - atoi(ip) + 1;
	    if (get_history (record, new_command_block, SZ_CMDBLK) == ERR)
		cl_error (E_UERR, "History record not found");
	    while (isdigit (*ip))
		ip++;

	/* Directives "^", "^str", and "^?str".  */
	} else
	    ip = directive + search_history (directive, new_command_block);

	/* Check for the ":p" no execute suffix */
	execute = (strncmp (ip, NO_EXECUTE, strlen(NO_EXECUTE)) != 0);
	if (!execute)
	    ip += strlen (NO_EXECUTE);
	
	/* Append any text remaining in the history directive to the new
	 * command block, BEFORE the final newline.
	 */
	op += strlen (new_command_block);
	while (isspace (*(op-1)))
	    --op;
	expand_history_macros (ip, op);

	/* Make sure the new command line ends with a newline. */
	while (*op != EOS)
	    op++;
	while (isspace (*(op-1)))
	    --op;
	*op++ = '\n';
	*op = EOS;

	return (execute);
}


/* SEARCH_HISTORY -- Search for the occurrence of the given string in the
 * history buffer, leaving the corresponding command in the output buffer
 * if it matches the pattern.  Return the number of directive characters used.
 * The "repeat last command" directive "^" is a special case: the null string
 * matches anything.
 */
int
search_history (char *directive, char *new_command_block)
{
	register char *ip, *op, *p;
	char	pattern[SZ_FNAME];
	int	match_only_at_bol=1, record, patlen;

	ip = directive + 1;			/* skip the '^'		*/

	if (*ip == '\\' && *(ip+1) == MATCH_ANYWHERE)
	    ip++;
	else if (*ip == MATCH_ANYWHERE) {
	    ip++;
	    match_only_at_bol = 0;
	}

	/* Extract pattern, delimited by whitespace, EOS, ?, or ":p",
	 * depending on whether we have ?? delimiters.
	 */
	patlen = strlen (NO_EXECUTE);
	for (op=pattern;  (*op = *ip) != EOS;  op++, ip++)
	    if (match_only_at_bol) {
		if (isspace (*ip))
		    break;
		else if (strncmp (ip, NO_EXECUTE, patlen) == 0)
		    break;
	    } else if (*ip == '\\' && *(ip+1) == MATCH_ANYWHERE) {
		*op = *++ip;
	    } else if (*ip == MATCH_ANYWHERE) {
		ip++;
		break;
	    }
	*op++ = EOS;

	/* Search backwards in history buffer until command is found
	 * which matches the pattern.  The null pattern matches anything.
	 */
	patlen = strlen (pattern);
	record = 1;

	while (get_history (record++, new_command_block, SZ_CMDBLK) != ERR) {
	    if (patlen == 0) {
		break;
	    } else if (match_only_at_bol) {
		if (strncmp (new_command_block, pattern, patlen) == 0)
		    break;
	    } else {
		for (p=new_command_block;  *p != EOS;  p++) {
		    if (*p == *pattern && strncmp(p,pattern,patlen) == 0)
			break;
		}
		if (*p != EOS)
		    break;
	    }
	}

	if (strlen (new_command_block) == 0)
	    cl_error (E_UERR, "Event not found");

	return (ip - directive);
}


/* STREDIT -- Edit string "in_text" according to the editing directive
 *   string given as the first argument, placing the edited string in the
 *   buffer "out_text".  Return the number of characters used in the
 *   edit directive string.
 * This is actually a general purpose string editor.  For the history code,
 *   the edit directives are "^^", "^str", and "^?str".  The directive "^^"
 *   is actually an edit directive wherein the match and substitute strings
 *   are both null, causing the last command to be repeated without change.
 * The first character in the edit directive is taken to be the edit
 *   metacharacter (i.e., "^", "/", etc.).
 */
int
stredit (
    char *edit_directive,		/* e.g., "^str1^str2^"		*/
    char *in_text,			/* text to be edited		*/
    char *out_text			/* buffer for output text	*/
)
{
	register char *ip, *op, *pp;
	char	metacharacter;
	char	pattern[SZ_LINE+1], text[SZ_LINE+1];
	int	replace_all_occurrences=0;
	int	patlen, len_directive, nmatches;

	/* Extract pattern and substitution strings.  The history metacharacter
	 * may be included in a string if escaped.  Otherwise, we leave
	 * escape sequences completely alone.
	 */
	ip = edit_directive;
	metacharacter = *ip++;

	for (op=pattern;  (*op = *ip) != EOS;  ip++, op++)
	    if (*ip == '\\' && *(ip+1) == metacharacter)
		*op = *++ip;
	    else if (*ip == metacharacter) {
		ip++;
		break;
	    }
	*op = EOS;
	patlen = strlen (pattern);

	/* If the directive is "^^", we do not permit a substitution string
	 * so that the directive may be used to append text to the previous
	 * command.  We interpret the sequences "^\n" and "^\t" as newline
	 * and tab, respectively.
	 */
	if (patlen > 0) {
	    for (op=text;  (*op = *ip) != EOS;  ip++, op++)
		if ((*ip == metacharacter && *(ip+1) == '\\') &&
		    (*(ip+2) == 'n' || *(ip+2) == 't')) {
		    ip += 2;
		    *op = (*ip == 'n') ? '\n' : '\t';
		} else if (*ip == '\\' && *(ip+1) == metacharacter) {
		    *op = *++ip;
		} else if (*op == '\n' || *op == metacharacter) {
		    ip++;
		    break;
		}
	    *op = EOS;
	    if (*ip == MATCH_ALL) {
		replace_all_occurrences = 1;
		ip++;
	    }
	} else
	    *text = EOS;

	/* All done processing edit directive; get nchars processed. */
	len_directive = ip - edit_directive;


	/* Edit the command, unless directive is "^^" (null pattern). */
	nmatches = 0;

	for (ip=in_text, op=out_text;  *ip != EOS;  ) {
	    /* Advance to next match */
	    for (pp=pattern;  (*op = *ip) != EOS;  op++, ip++)
		if (*ip == *pp && strncmp (ip, pattern, patlen) == 0) {
		    nmatches++;
		    break;
		}
	    if (patlen == 0)
		break;
	    else if (nmatches == 0)
		cl_error (E_UERR, "No match");

	    /* Copy replacement string, advance input pointer past the
	     * matched string, if we have a match.
	     */
	    if (*ip == *pp) {
		for (pp=text;  (*op = *pp++) != EOS;  op++)
		    ;
		ip += patlen;
	    }

	    if (!replace_all_occurrences) {
		while ((*op = *ip++) != EOS)
		    op++;
		break;
	    }
	}

	*op = EOS;
	return (len_directive);
}


/* EXPAND_HISTORY_MACROS -- Copy the input string to the output string,
 *   replacing all occurrences of "^$" by the final argument the last command,
 *   all occurrences of "^^" by the first argument of the last command, and
 *   all occurrences of "^*" by the full argument list of the last command.
 * If the command block contains more than one line, we assume that the
 *   argument list spans several lines.  If this is not true, the expansion
 *   will not be what the user wanted (but then they probably screwed up).
 * The function returns true if any macros were expanded.
 */
int
expand_history_macros (char *in_text, char *out_text)
{
	register char *ip, *op, *ap;
	char	cmdblk[SZ_CMDBLK+1], *argp[100];
	int	nargs=0, nrep=0, argno=0, have_arg_strings=0;
	char	*index();

	/* Copy the command text.  Fetch argument strings from history only
	 * if a history macro is found.  Otherwise the copy is very fast.
	 */
	for (ip=in_text, op=out_text;  (*op = *ip) != EOS;  ip++, op++) {
            if (*ip == '"') {                   /* span literal strings */
                while (1) {
                   *op++ = *ip++;
                   if (*ip == '"' && *(ip+1) != '"') {
                       *op = *ip;
                        break;
                   }
                }
                continue;
            } else if (*ip == HISTCHAR) {
		if (ip > in_text && *(ip-1) == '\\') {
		    *(--op) = HISTCHAR;				/* \^	*/
		    continue;
		} else if (!isdigit(*(ip+1)) && index(ARGCHARS,*(ip+1)) == NULL)
		    continue;

		/* Parse the argument list of the previous command if have not
		 * already done so.
		 */
		if (!have_arg_strings++) {
		    if (get_history (1, cmdblk, SZ_CMDBLK) == ERR)
			cl_error (E_UERR, "Nothing in history buffer");
		    nargs = get_arglist (cmdblk, argp);
		}

		/* Set up the substitution.
		 */
		switch (*(ip+1)) {
		case FIRSTARG:
		    argno = 1;
		    nrep = 1;
		    break;
		case LASTARG:
		    argno = nargs;
		    nrep = 1;
		    break;
		case ALLARGS:
		    argno = 1;
		    nrep = nargs;
		    break;
		default:
		    argno = *(ip+1) - '0';
		    nrep = 1;
		    break;
		}

		/* Copy the arguments to the output command, overwriting the
		 * history metacharacter (*op).
		 */
		while (--nrep >= 0 && argno <= nargs) {
		    for (ap=argp[argno++];  (*op = *ap++);  op++)
			;
		    if (nrep > 0)
			*op++ = ' ';
		}

		--op;		/* leave pointing at last char output	*/
		ip++;		/* skip the macro type metacharacter	*/
	    }
	}

	return (have_arg_strings > 0);
}


/* GET_ARGLIST -- Fetch the last command line and return an array of
 * pointers to the whitespace delimited argument strings.  If parsing a
 * full command line, argument "zero" is the task name (the first token),
 * and argp[1] the first real argument.  The number of arguments
 * (excluding the task name) is returned as the function value.
 *
 * NOTE -- The input argument list is modified (the argp[i] point into it).
 * NOTE -- This procedure is used elsewhere in the CL to parse argument lists.
 */
int
get_arglist (
    char *cmdblk,		/* buffer to store argument list in	*/
    char *argp[]		/* receives argument pointers		*/
)
{
	register char	*cp;
	register int	nargs;

	for (cp=cmdblk, nargs=0;  *cp != EOS;  ) {
	    /* Advance to next token; convert newline to EOS. */
	    while (*cp == ' ' || *cp == '\t')
		cp++;
	    if (*cp == '\n' || *cp == EOS) {
		*cp = EOS;
		break;
	    }

	    /* Set argument pointer and bump argument count. */
	    argp[nargs++] = cp;

	    /* Mark the end of the token. */
	    while (*cp && !isspace (*cp))
		cp++;
	    if (*cp == ' ' || *cp == '\t')
		*cp++ = EOS;
	}

	return (nargs - 1);
}


/* PUT_HISTORY -- Add a new record to the history buffer.  Record cannot
 * be larger than SZ_CMDBLK, which must be smaller than SZ_HISTBUF.  Copy
 * chars into histbuf in circular buffer fashion, overwriting old history
 * data.  EOS delimits records in the history buffer.
 */
void
put_history (char *command)
{
	register char *ip, *op, *otop;

	/* Make sure there is exactly one newline at the end of the command. */
	for (ip = command + strlen(command) - 1;  ip >= command;  --ip)
	    if (!isspace (*ip))
		break;
	*++ip = '\n';
	*++ip = EOS;

	otop = histbuf + SZ_HISTBUF;
	ip = command;
	op = op_hist;

	do {
	    *op++ = *ip;
	    if (op >= otop) {
		op = histbuf;
		histbuf_full++;
	    }
	} while (*ip++ != EOS);

	op_hist = op;
	histnum++;
}


/* GET_HISTORY -- Fetch the indicated command from the history buffer,
 * returning OK if found, ERR otherwise.
 */
int
get_history (int record, char *command, int maxch)
{
	char	*recptr;
	char	*find_history();

	if ((recptr = find_history (record)) == NULL) {
	    *command = EOS;
	    return (ERR);
	} else {
	    fetch_history (recptr, command, maxch);
	    return (OK);
	}
}


/* FETCH_HISTORY -- Extract the command pointed to by the first argument
 * from the history buffer into the user buffer (the latter is a nice,
 * well behaved linear rather than circular buffer).
 */
void
fetch_history (char *recptr, char *command, int maxch)
{
	register char	*ip, *op, *itop;
	register int	n;

	itop = histbuf + SZ_HISTBUF;
	ip   = recptr;
	op   = command;
	n    = ((maxch < SZ_HISTBUF) ? maxch : SZ_HISTBUF) - 1;

	while (--n >= 0 && ((*op = *ip++) != EOS) ) {
	    op++;
	    if (ip >= itop)
		ip = histbuf;
	}

	*op = EOS;
}


/* FIND_HISTORY -- Locate the indicated command record in the history buffer,
 * returning a pointer to the first char or NULL.  Commands are referenced
 * by number, where 1 is the most recent command, 2 the one before that, and
 * so on.  We are done when we search so far back that we reach the location
 * op_hist.  To speed up linear searches of the history buffer, we keep track
 * of where we are on successive calls, provided the buffer has not been
 * written into between calls.  We can detect this by saving a copy of
 * op_hist in a static variable between calls.
 */
char *
find_history (int record)
{
	register char *ip, *op, *bufptr;
	static	int current_record = 0;
	static	char *recptr, *old_ophist = NULL;

	if (histnum == 0 || record <= 0)
	    return (NULL);

	/* We only search backwards into history: if desired record is
	 * more recent than the "current record", or if the buffer has
	 * been written into, reset and search from the beginning.  The
	 * "current record" is the record pointed to by recptr.
	 */
	if (old_ophist != op_hist || record < current_record) {
	    current_record = 0;
	    old_ophist = recptr = op_hist;
	}

	ip = recptr;				/* start here		*/
	op = op_hist;				/* not found if get here */
	bufptr = histbuf;			/* wrap around if get here */

	/* Search backwards into history for the record, starting from the
	 * current position (initially record number "0", the next record to
	 * be filled).  Each time through the loop, set recptr for the new
	 * "current record".
	 */
	while (current_record < record) {
	    if (--ip < bufptr) {		/* backup to EOS	*/
		if (!histbuf_full)
		    return (NULL);
		ip = histbuf + SZ_HISTBUF - 1;
	    }
	    do {
		if (--ip < bufptr) {
		    /* Initially, before the buffer fill up, there is no EOS
		     * preceeding the first record.
		     */
		    if (!histbuf_full)
			break;
		    ip = histbuf + SZ_HISTBUF - 1;
		}
		if (ip == op)
		    return (NULL);		/* cannot find record	*/
	    } while (*ip != EOS);

	    /* Advance to first char of next record */
	    if (++ip >= histbuf + SZ_HISTBUF)
		ip = bufptr;
	    recptr = ip;
	    current_record++;
	}
	history_number = current_record;	/* save this globally  */
	return (recptr);
}


/* SHOW_HISTORY -- Print the contents of the history buffer on the output
 * stream, preceeding each command block with a 3 digit command number.
 * Show at most min (max_commands, MAX_SHOWHIST) command blocks.
 */
void
show_history (FILE *fp, int max_commands)
{
	char	*recptr[MAX_SHOWHIST];
	char	cmdblk[SZ_CMDBLK+1];
	int	record;
	char	*find_history();

	/* Flush the "history" command so that it shows up in the history. */
	yy_startblock (LOG);

	/* Determine the number of records to show. */
	for (record=0;  record < MAX_SHOWHIST;  record++)
	    if ((recptr[record] = find_history (record+1)) == NULL)
		break;
	if (max_commands > 0)
	    record = (record < max_commands) ? record : max_commands;

	/* Print the records with the 3 digit record number plus a blank
	 * on the first line and 4 blanks at the beginning of each successive
	 * line of the block.
	 */
	while (record > 0) {
	    fprintf (fp, "%3d ", (histnum - (--record)) % 1000);
	    fetch_history (recptr[record], cmdblk, SZ_CMDBLK+1);
	    print_command (fp, cmdblk, "", "    ");
	    fflush (fp);
	}
}


/* PPROMPT -- Print prompt as first two chars of prompt string plus "> ", i.e.,
 * "pk> ".  If null prompt string (NOCLOSURE), print the continuation prompt
 * ">>> ".  Also print, before the prompt, all ltasks in current package
 * if menus() are enabled and a new package has been invoked.
 */
void
pprompt (register char *string)
{
	static	struct package *lastpack = NULL;
	extern	long int run_level;

	if (menus() && curpack != lastpack) {
	    listhelp (curpack, NO);
	    lastpack = curpack;
	    printf ("\n");
	    fflush (stdout);
	}

	if (strncmp ("clpackage", string, 9) == 0)
	    printf ("ecl> ");
	else
	    printf ((eh_longprompt == YES) ? "%s> " : "%2.2s> ", string);
	fflush (stdout);

	run_level = 0;
}


/* GET_PROMPT -- Get prompt as first two chars of prompt string plus "> ", i.e.,
 * "pk> ".  If null prompt string (NOCLOSURE), print the continuation prompt
 * ">>> ".  Also print, before the prompt, all ltasks in current package
 * if menus() are enabled and a new package has been invoked.
 */
void 
get_prompt (register char *string)
{
	static	struct package *lastpack = NULL;
	extern	long int run_level;

        if (menus() && curpack != lastpack) {
            listhelp (curpack, NO);
            lastpack = curpack;
	    printf ("\n");
	    fflush (stdout);
        }

	if (strncmp ("clpackage", string, 9) == 0)
	    strcpy (prompt, "ecl> ");
	else
	    sprintf (prompt,
		(eh_longprompt == YES) ? "%s> " : "%2.2s> ", string);

	run_level = 0;
}


/* PUT_LOGFILE -- Put a command into the logfile, if logging is enabled.
 * Otherwise check if the logfile is open and close it, in case user has
 * just turned logging off.  If the "share_logfile" switch is set the logfile
 * is opened and closed each time a record is appended to the file, allowing
 * other processes to access the same file.
 */
void
put_logfile (char *command)
{
	FILE	*fp;

	if (keeplog()) {
	    if (logfp == NULL)
	        if (open_logfile (logfile()) == ERR)
		    /* Do not abort by calling cl_error().  We could be a
		     * background job accessing a shared logfile.  Also, we
		     * want to avoid error recursion when logging an error.
		     */
		    return;

  	    if (share_logfile) {
		if ((fp = fopen (logfile(), "a"))) {
		    print_command (fp, command, "", "");
		    fclose (fp);
		}
	    } else
	        print_command (logfp, command, "", "");

	} else if (logfp != NULL)
	    close_logfile (logfile());
}


/* OPEN_LOGFILE -- Open the named command logging file for appending,
 * timestamp new session.  The logfile grows without bounds unless the
 * user deletes it or starts a new one.
 */
int
open_logfile (char *fname)
{
	if (logfp != NULL)
	    close_logfile (fname);

	if ((logfp = fopen (fname, "a")) == NULL) {
	    eprintf ("cannot open logfile\n");
	    return (ERR);
	}

	if (!(firstask->t_flags & T_BATCH))
	    fprintf (logfp, "\n# LOGIN %s\n", today());
	
	if (share_logfile)
	    fclose (logfp);

	return (OK);
}


/* CLOSE_LOGFILE -- Print termination message and close logfile.
 */
void
close_logfile (char *fname)
{
	register FILE *fp;

	if (logfp != NULL) {
	    if (share_logfile) {
		if ((fp = fopen (fname, "a")) == NULL) {
		    eprintf ("cannot open logfile\n");
		    return;
		}
	    } else
		fp = logfp;

	    if (!(firstask->t_flags & T_BATCH))
		fprintf (fp, "# Logout %s\n", today());

	    fclose (fp);
	    logfp = NULL;
	}
}


/* RESET_LOGFILE -- The name of the logfile has been reset by the user.
 * Close and reopen the logfile, but only if share_logfile option is off.
 */
void
reset_logfile (void)
{
	if (!share_logfile) {
	    close_logfile ("");
	    open_logfile (logfile());
	}
}


/* PRINT_COMMAND -- Print a (possibly multiline) command to the same left
 * margin as when it was entered.
 */
void
print_command (
    register FILE *fp,
    char *command,
    char *marg1,
    char *marg2		/* margin strings of first and subseq. cmds */
)
{
	register char *ip;

	fprintf (fp, marg1);
	for (ip=command;  *ip != EOS;  ip++) {
	    fputc (*ip, fp);
	    if (*ip == '\n' && *(ip+1) != EOS)
		fprintf (fp, marg2);
	}
}


/* TODAY -- Get todays date as a string, for datestamping the logfile.
 */
char *
today (void)
{
	static	char datebuf[64];

	c_cnvtime (c_clktime(0L), datebuf, 64);
	return (datebuf);
}


/* WHAT_RECORD -- Return the record number of the last edited history
 */
int
what_record (void)
{
	return (history_number);
}


/* PUTLOG -- Format and write a message to the logfile.  This is called by
 * the putlog builtin (clputlog() in builtin.c) and in some places in the
 * CL (e.g., exec.c).
 */
void
putlog (
    struct task *tp,		/* pointer to task or NULL */
    char *usermsg
)
{
	register char	*ip, *op, *otop;
	register int	n;
	char	msg[SZ_LOGBUF], job[5];
	char	*pkg, *tname, *today();
	extern  int  bkgno;			/* job number if bkg job */

	if (!keeplog())
	    return;

	/* If background job, format job number, but only if background
	 * logging is enabled.
	 */
	if (firstask->t_flags & T_BATCH) {
	    if (log_background())
	    	sprintf (job, "[%d] ", bkgno);
	    else
		return;
	} else
	    job[0] = EOS;

	/* If a valid task pointer is given, get the package and task name.
	 * Otherwise, assume it's an internal (cl) logging message.
	 */
	if (tp) {
	    pkg   = tp->t_ltp->lt_pkp->pk_name;
	    tname = tp->t_ltp->lt_lname;
	} else {
	    pkg   = "cl";
	    tname = "";
	}

	/* Format the message.  Only use time, no day and date.  Break long
	 * messages into several lines.
	 */
	sprintf (msg, "# %8.8s %s%s%s %s- ",
		(today() + 4), pkg, (tp ? "." : ""), tname, job);
	otop = &msg[SZ_LOGBUF];
	for (op=msg, n=0;  *op && op < otop;  op++)
	    n++;
	for (ip=usermsg;  (*op++ = *ip++) && op < otop;  n++)
	    if (n + 2 >= MAXCOL) {
		*op++ = '\\';
		*op++ = '\n';
		n = 0;
	    }
	*(op-1) = '\n';
	*op = EOS;
	    
	put_logfile (msg);
}
