/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "mkpkg.h"
#include "extern.h"
#include "../bootProto.h"




/*
 * TOK.C -- Preprocessor functions.
 */

/* GETTOK -- Get the next token from the make file currently being scanned.
 * Conditional interpretation is provided via the $IFxxx directives.
 */
int
gettok (
  register struct context *cx,		/* current context	*/
  char	*outstr,			/* receives token	*/
  int	maxch 
)
{
	register int	ch;
	register char	*op;
	char	tokbuf[SZ_COMMAND+1];
	int	token, delim;

	if (debug > 1) {
	    printf ("gettok:\n");
	    fflush (stdout);
	}

again:
	/* Skip whitespace */
	for (ch = m_getc(cx);  ch == ' ';  ch = m_getc(cx))
	    ;
	if (ch == EOF) {
	    outstr[0] = EOS;
	    return (TOK_END);
	}
	outstr[0] = ch;
	outstr[1] = EOS;

	/* First nonwhite character identifies each token.
	 */
	switch (ch) {
	case COMMENT:
	    /* Ignore a comment.
	     */
	    while ((ch = m_rawgetc(cx)) != '\n' && ch != EOF)
		;
	    m_ungetc ('\n', cx);
	    goto again;

	case PREPROCESSOR:
	    /* Preprocessor directive.
	     */
	    for (op=tokbuf, *op++ = ch;  (ch = m_getc(cx)) != EOF;  )
		if (islower (ch))
		    *op++ = ch;
		else if (isupper (ch))
		    *op++ = tolower (ch);
		else {
		    m_ungetc (ch, cx);
		    break;
		}

	    *op = EOS;
	    if (strncmp (tokbuf, "$exit", 5) == 0)
		return (TOK_END);

	    do_ppdir (cx, tokbuf);
	    goto again;

	case SYSCMD:
	    /* Send a command to host system.
	     */
	    do_osescape (cx);
	    goto again;

	case BEGIN_CHAR:
	    /* Start of program.
	     */
	    token = TOK_BEGIN;
	    break;

	case END_CHAR:
	    /* End of program.
	     */
	    token = TOK_END;
	    break;

	case '\n':
	    token = TOK_NEWLINE;
	    break;

	case SYSFILE_BEGIN:
	    /* Replace '<' by system library pathname, concatentate
	     * filename and exit.
	     */
	    getstr (cx, tokbuf, SZ_COMMAND, SYSFILE_END);
	    if (m_sysfile (tokbuf, outstr, maxch) <= 0)
		sprintf (outstr, "<%s>", tokbuf);

	    if (debug) {
		/* Don't print diagnostic if the file was found to be
		 * in the usual place, i.e., the system library lib$.
		 */
		if (strncmp (outstr, "iraf$lib/", 9) != 0) {
		    printf ("<%s> matched with `%s'\n", tokbuf, outstr);
		    fflush (stdout);
		}
	    }

	    token = TOK_FNAME;
	    break;

	case '\'':
	case '"':
	    /* Quoted strings are treated as fname tokens, permitting
	     * optional quoting of filenames in module lists.
	     */
	    getstr (cx, outstr, maxch, delim = ch);
	    token = TOK_FNAME;
	    break;

	case '\\':
	    if ((ch = m_getc(cx)) == '\n')
		goto again;
	    /* fall through */

	default:
	    /* Unquoted filename token.
	     */
	    m_ungetc (ch, cx);
	    getstr (cx, outstr, maxch, delim = ' ');
	    token = TOK_FNAME;
	    break;
	}

	/* Discard token? */
	if (ifstate[iflev] == STOP)
	    goto again;

	if (debug > 1) {
	    if (outstr[0] <= 040)
		printf ("token = char 0%o\n", outstr[0]);
	    else
		printf ("token = `%s'\n", outstr);
	    fflush (stdout);
	}

	return (token);
}


/* DO_OSESCAPE -- Send a command to host system.  If the first char after
 * the ! is a left paren or quote then the matching char is taken to terminate
 * the command, otherwise an (unescaped) newline terminates the command.
 * The parenthesized form permits additional directives on the same line.
 */
void
do_osescape (register struct context *cx)
{
	register int	ch;
	char	cmd[SZ_CMD+1];

	if (debug > 1) {
	    printf ("do_osescape:\n");
	    fflush (stdout);
	}

	ch = m_getc (cx);
	if (ch == '(' || ch == '\'' || ch == '"') {
	    getstr (cx, cmd, SZ_CMD, (ch == '(' ? ')' : ch));
	} else if (ch == '\n') {
	    return;

	} else {
	    char    *op, *otop;

	    op    = cmd;
	    *op++ = ch;
	    otop  = &cmd[SZ_CMD];

	    while (op < otop && (ch = m_getc(cx)) != EOF)
		if (ch == ESCAPE) {
		    ch = m_getc (cx);
		    if (ch != '\n') {
			*op++ = ESCAPE;
			*op++ = ch;
		    }
		} else if (ch == '\n') {
		    break;
		} else
		    *op++ = ch;

	    *op = EOS;
	}

	if (ifstate[iflev] == STOP)
	    return;
	if (verbose) {
	    printf ("!%s\n", cmd);
	    fflush (stdout);
	}

	if (execute)
	    exit_status = os_cmd (cmd);
	if (exit_status == INTERRUPT)
	    fatals ("<ctrl/c> interrupt %s", cx->library);
}


/* DO_PPDIR -- Execute a preprocessor directive.  A hash table would be more
 * efficient, but the complexity is not warranted since this is only called
 * when a $ prefixed preprocessor directive has already been recognized in
 * the input.
 */
void
do_ppdir (
    struct context *cx,		/* current context		*/
    char	*token		/* directive to be executed	*/
)
{
	int	islib;

	if (debug > 1) {
	    printf ("do_ppdir: %s\n", token);
	    fflush (stdout);
	}

	if (     strncmp (token, "$if",		3) == 0)
	    do_if (cx, token);
	else if (strncmp (token, "$else",	5) == 0)
	    do_else (cx);
	else if (strncmp (token, "$endif",	6) == 0)
	    do_endif (cx);
	else if (strncmp (token, "$end",	4) == 0)
	    do_end (cx);

	else if (strncmp (token, "$call",	5) == 0)
	    do_call (cx, getargs(cx), islib=NO);
	else if (strncmp (token, "$echo",	5) == 0)
	    do_echo (cx, getargs(cx));
	else if (strncmp (token, "$goto",	5) == 0)
	    do_goto (cx, getargs(cx));
	else if (strncmp (token, "$include",	8) == 0)
	    do_include (cx, getargs(cx));
	else if (strncmp (token, "$set",	4) == 0)
	    do_set (cx);
	else if (strncmp (token, "$special",	8) == 0)
	    sf_scanlist (cx);
	else if (strncmp (token, "$update",	7) == 0)
	    do_call (cx, getargs(cx), islib=YES);

	else if (strncmp (token, "$checkin",	8) == 0)
	    do_incheck (cx);
	else if (strncmp (token, "$checkout",	9) == 0)
	    do_outcheck (cx);
	else if (strncmp (token, "$copy",	5) == 0)
	    do_copyfile (cx);
	else if (strncmp (token, "$delete",	7) == 0)
	    do_delete (cx);
	else if (strncmp (token, "$generic",	8) == 0)
	    do_generic (cx);
	else if (strncmp (token, "$link",	5) == 0)
	    do_link (cx);
	else if (strncmp (token, "$move",	5) == 0)
	    do_movefile (cx);
	else if (strncmp (token, "$omake",	6) == 0)
	    do_omake (cx, getargs(cx));
	else if (strncmp (token, "$purge",	6) == 0)
	    do_purge (cx, getargs(cx));
	else if (strncmp (token, "$xc",		3) == 0)
	    do_xc (cx);

	else if (strncmp (token, "$debug",	6) == 0) {
	    if ((debug = (strcmp (getargs(cx), "off")) != 0))
		verbose++; }
	else if (strncmp (token, "$verbose",	8) == 0)
	    verbose = (strcmp (getargs(cx), "off") != 0);

	else
	    warns ("illegal preprocessor directive `%s'", token);
}


/* DO_IF -- Called when a "$if.." token is seen in the input stream.  Read in
 * the predicate and set the state of the ifcode accordingly.
 */
void
do_if (struct context *cx, char	*keyword)
{
	register int	ch;
	register char	*op;
	char	tokbuf[SZ_COMMAND+1];
	char	buf[SZ_PREDBUF], *argv[MAX_ARGS];
	long    fdate, altdate, os_fdate(char *);
	int	argc, negate, bval, i;
	char	*key;
 
	if (debug > 1) {
	    printf ("do_if: %s\n", keyword);
	    fflush (stdout);
	}

	/* Set the negate flag for the "$ifn" form of the if.  Leave key
	 * pointing to the first char of whatever follows.  Watch out for
	 * "$ifnewer".
	 */
	key = &keyword[3];	/* "$if^" */
	negate = (*key == 'n' && strncmp(key,"newer",5) != 0);
	if (negate)
	    key++;

	/* Extract the paren delimited list of predicate strings.  This may
	 * extend over multiple lines if the newlines are escaped.
	 */
	while ((ch = m_getc(cx)) != '(')
	    if (ch == '\n')
		warns ("illegal `%s' predicate", keyword);
	    else if (ch == EOF)
		warns ("unexpected EOF in `%s'", keyword);

	argv[0] = buf;
	op      = buf;
	argc    = 0;

	while ((ch = m_getc(cx)) != ')') {
	    if (ch == ESCAPE) {
		ch = m_getc (cx);
		if (ch == '\n')
		    continue;
		else
		    *op++ = ch;
	    } else if (ch == '\n') {
		warns ("missing right paren in `%s'", keyword);
	    } else if (ch == EOF) {
		warns ("unexpected EOF in `%s'", keyword);
	    } else if (ch == ' ') {
		continue;
	    } else if (ch == SYSFILE_BEGIN && op == argv[argc]) {
		getstr (cx, tokbuf, SZ_COMMAND, SYSFILE_END);
		if (m_sysfile (tokbuf, op, SZ_PREDBUF+buf-op) <= 0)
		    sprintf (op, "<%s>", tokbuf);
		while (*op)
		    op++;
		continue;
	    } else if (ch == ':' || ch == ',') {
		*op++ = EOS;
		if (op - buf >= SZ_PREDBUF)
		    warns ("predicate too large in `%s'", keyword);
		if (++argc >= MAX_ARGS)
		    warns ("too many arguments in `%s' predicate", keyword);
		argv[argc] = op;
	    } else
		*op++ = ch;
	}

	*op = EOS;
	argc++;

	if (++iflev > SZ_IFSTACK)
	    warns ("$IFs nested too deeply (%s)", keyword);

	/* If the $IF is encountered while scanning the tokens in a false-IF
	 * clause, do not "execute" the $IF.  We still have to push the IF
	 * onto the control stack, because the matching $ENDIF is going to
	 * pop the stack.
	 */
	if (ifstate[iflev-1] == STOP) {
	    ifstate[iflev] = STOP;
	    return;
	}

	/* Execute the $IF statement.
	 */
	bval = 0;
	if (strcmp (key, "def") == 0) {
	    /* $IFDEF.  If the named symbol exists execute the true clause,
	     * else go to the else clause.
	     */
	    if (argc > 0) {
		bval = (getsym (argv[0]) != NULL);
		if (!bval)
		    bval = (os_getenv (argv[0]) != NULL);
	    }

	} else if (strcmp (key, "eq") == 0) {
	    /* $IFEQ.  Test if the named environment variable has one of the
	     * indicated values.
	     */
	    char   *valstr;

	    if (argc > 0) {
		if ((valstr = getsym (argv[0])) == NULL &&
		    (valstr = os_getenv (argv[0])) == NULL) {

		    warns ("symbol `%s' not found", argv[0]);
		    bval = 0;

		} else {
		    if (argc == 1)
			bval = 1;
		    else {
			for (i=1;  i < argc;  i++)
			    if (strcmp (valstr, argv[i]) == 0) {
				bval = 1;
				break;
			    }
		    }
		}
	    }

	} else if (strcmp (key, "file") == 0) {
	    /* $IFFILE.  Check for the existence of any of the named files.
	     */
	    for (i=0;  i < argc;  i++)
		if (os_access (argv[i], 0,0) == YES) {
		    bval = 1;
		    break;
		}

	} else if (strcmp (key, "older") == 0) {
	    /* $IFOLDER.  Check if the named file is older than any of the
	     * listed files.  If the named file does not exist the result
	     * is true.  If any of the listed files do not exist a warning
	     * is printed and they are ignored.
	     */
	    if (os_access (argv[1], 0,0) == NO) {
		warns ("file `%s' not found", argv[1]);
		bval = 1;
	    } else if ((fdate = os_fdate(argv[0])) <= 0) {
		warns ("file `%s' not found", argv[0]);
		bval = 1;
	    } else {
		for (i=1;  i < argc;  i++) {
		    altdate = m_fdate (argv[i]);
		    if (altdate <= 0) {
			warns ("file `%s' not found", argv[i]);
			bval = 1;
			break;
		    } else if (fdate < altdate) {
			bval = 1;
			break;
		    }
		}
	    }

	} else if (strcmp (key, "newer") == 0) {
	    /* $IFNEWER.  Check if the named file is newer than any of the
	     * listed files.  If the named file does not exist the result
	     * is false.  If any of the listed files do not exist a warning
	     * is printed and they are ignored.
	     */
	    if (os_access (argv[1], 0,0) == NO) {
		warns ("file `%s' not found", argv[1]);
		bval = 1;
	    } else if ((fdate = os_fdate(argv[0])) <= 0) {
		warns ("file `%s' not found", argv[0]);
		bval = 1;
	    } else {
		for (i=1;  i < argc;  i++) {
		    altdate = m_fdate (argv[i]);
		    if (altdate <= 0)
			warns ("file `%s' not found", argv[i]);
		    else if (fdate > altdate) {
			bval = 1;
			break;
		    }
		}
	    }
	    
	} else if (strcmp (key, "err") == 0) {
	    /* $IFERR.  Test the exit status of the last command executed.
	     */
	    bval = (exit_status != OK);

	} else
	    warns ("unrecognized $if statement `%s'", keyword);

	if (negate)
	    bval = !bval;
	ifstate[iflev] = bval;

	if (debug) {
	    printf ("%s (", keyword);
	    if (argc > 0)
		printf ("%s", argv[0]);
	    for (i=1;  i < argc;  i++)
		printf (", %s", argv[i]);
	    printf (") -> %s\n", bval ? "YES" : "NO");
	    fflush (stdout);
	}
}


/* DO_ELSE -- Called when the token "$else" is seen in the input stream.
 * Toggle the if state.  Do nothing if the state one level down in STOP,
 * indicating that this $ELSE is nested inside the false clause of an
 * outer $IF.
 */
void
do_else (struct	context *cx)
{
	if (debug > 1) {
	    printf ("do_else:\n");
	    fflush (stdout);
	}

	if (iflev < 1)
	    warns ("%s with no matching $if", "$else");
	else if (iflev > 1 && ifstate[iflev-1] == STOP)
	    return;
	else
	    ifstate[iflev] = (ifstate[iflev] == PASS) ? STOP : PASS;
}


/* DO_ENDIF -- Called when the token "$endif" is seen in the input stream.
 * Pop the if stack.
 */
void
do_endif (struct context *cx)
{
	if (debug > 1) {
	    printf ("do_endif:\n");
	    fflush (stdout);
	}

	if (--iflev < 0)
	    warns ("unmatched %s", "$endif");
}


/* DO_END -- Called when the token "$end" is seen in the input stream.
 * Clear the if stack and reenable pass-token.
 */
void
do_end (struct context *cx)
{
	if (debug > 1) {
	    printf ("do_end:\n");
	    fflush (stdout);
	}

	if (cx->prev && cx->prev->old_iflev >= 0)
	    iflev = cx->prev->old_iflev;
	else
	    iflev = 0;
}


/* DO_CALL -- Call a "subroutine", i.e., named entry in a mkpkg file.  The call
 * may include definitions for any temporary symbols (arguments) to be passed
 * to the subroutine.  The subroutine is assumed to be in the current mkpkg
 * file unless otherwise indicated.
 *
 * Syntax:
 *	$call module
 *	$call module (sym1=value, sym2=value, ...)
 *	$call module@subdir/file
 *	$call module@subdir/file (sym1=value, ...)
 *		(etc.)
 *
 * Note that the statements are interpreted (as is everything in mkpkg), hence
 * mkpkg subroutines should not be used for trivial things.
 */
void
do_call (
  struct context *cx,		/* current context		*/
  char	*program,		/* module to be called		*/
  int	 islib 			/* module list for a library	*/
)
{
	struct	context *ncx;
	char	module[SZ_FNAME+1], subdir[SZ_FNAME+1], fname[SZ_FNAME+1];
	char	symbol[SZ_FNAME+1], value[SZ_COMMAND+1];
	char	modspec[SZ_FNAME+1];
	char	*old_cp;
	int	old_nsymbols;

	strcpy (modspec, program);
	if (debug && ifstate[iflev] == PASS) {
	    printf ("$call %s\n", modspec);
	    fflush (stdout);
	}

	old_cp = cp;
	old_nsymbols = nsymbols;

	/* Process the argument list, if any, into the symbol table.
	 */
	while (getkwvpair (cx, symbol, value) != ERR)
	    if (ifstate[iflev] == PASS)
		putsym (symbol, value);

	if (ifstate[iflev] == STOP)
	    return;

	/* Parse the module name, push a new context, and execute the
	 * subroutine.
	 */
	parse_modname (modspec, module, subdir, fname);
	if ((ncx = push_context (cx, module, subdir, fname)) == NULL)
	    exit_status = ERR;
	else {
	    exit_status = do_mkpkg (ncx, islib);
	    cx = pop_context (ncx);
	}

	/* Restore the old context and discard the argument temporaries.
	 */
	if (exit_status != OK)
	    warns ("module `%s' not found or returned error", modspec);

	cp = old_cp;
	nsymbols = old_nsymbols;
}


/* DO_ECHO -- Print a message on the standard output.
 */
void
do_echo (struct context *cx, char *msg)
{
	if (ifstate[iflev] == PASS) {
	    printf ("%s\n", msg);
	    fflush (stdout);
	}
}


/* DO_GOTO -- Advance the file pointer to the named symbol in the current
 * file, without changing the current context.
 */
int
do_goto (struct context *cx, char *symbol)
{
	register char	*ip;
	char	match[SZ_FNAME+1];
	char	lbuf[SZ_LINE+1];
	int	len_matchstr;
	long	fpos;

	if (ifstate[iflev] == STOP)
	    return (OK);

	if (debug) {
	    printf ("goto %s\n", symbol);
	    fflush (stdout);
	}

	sprintf (match, "%s:", symbol);
	len_matchstr = strlen (match);

	fpos = k_ftell (cx);
	if (cx->fp != stdin)
	    k_fseek (cx, 0L, 0);

	while (k_fgets (lbuf, SZ_LINE, cx) != NULL) {
	    cx->lineno++;
	    for (ip=lbuf;  isspace (*ip);  ip++)
		;
	    if (strncmp (ip, match, len_matchstr) == 0) {
		/* GOTO clears the IF stack back to where it whatever it was
		 * upon entry to the module.
		 */
		if (cx->prev && cx->prev->old_iflev >= 0)
		    iflev = cx->prev->old_iflev;
		return (OK);
	    }
	}

	warns ("could not find mkpkg module or label `%s'", symbol);
	if (cx->fp != stdin)
	    k_fseek (cx, fpos, 0);

	return (ERR);
}


/* DO_INCLUDE -- Open a file and execute any preprocessor directives therein.
 * Macros defined in an include are retained after the context of the include
 * is popped.
 */
int
do_include (
  struct context *cx,		/* current context	*/
  char *fname 			/* include file name	*/
)
{
	struct	context *ncx;
	int	islib;

	if (ifstate[iflev] == STOP)
	    return (OK);

	if (debug > 1) {
	    printf ("do_include: %s\n", fname);
	    fflush (stdout);
	}

	ncx = push_context (cx, "BOF", "", fname);
	do_mkpkg (ncx, islib=NO);
	cx->old_cp = cp;			/* keep symbols */
	cx->old_nsymbols = nsymbols;
	cx = pop_context (ncx);

	return (OK);
}


/* DO_OMAKE -- Generate the object module for the named source module, if
 * the object does not exist or is older than the source module.
 */
void
do_omake (
  struct context *cx,
  char	*fname
)
{
	char	cmd[SZ_COMMAND+1];
	char	xflags[SZ_LINE+1];
	char	*dflist[MAX_DEPFILES+1];
	char	*s_xflags, *dfile;
	long	sourcedate, objdate, date;
	int	recompile, i;


	if (ifstate[iflev] == STOP)
	    return;

	if (debug) {
	    printf ("omake %s\n", fname);
	    fflush (stdout);
	}

	if ((sourcedate = os_fdate (fname)) <= 0) {
	    warns ("file `%s' not found", fname);
	    exit_status = ERR;
	    return;

	} else {
	    get_dependency_list (cx, fname, dflist, MAX_DEPFILES);
	    objdate = os_fdate (makeobj (fname));
	    recompile = 0;

	    if (sourcedate > objdate)
		recompile++;
	    else {
		for (i=0;  (dfile = dflist[i]) != NULL;  i++)
		    if ((date = m_fdate (dfile)) == 0)
			warns ("dependency file `%s' not found", dfile);
		    else if (date > objdate) {
			recompile++;
			break;
		    }
	    }
	}

	if (recompile) {
	    /* Get XFLAGS. */
	    s_xflags = getsym (XFLAGS);
	    xflags[0] = EOS;
	    if (debug)
		strcat (xflags, "-d ");
	    if (dbgout)
		strcat (xflags, "-x ");
	    strcat (xflags, s_xflags);

	    if (irafdir[0])
		sprintf (cmd, "%s %s -r %s %s", XC, xflags, irafdir, fname);
	    else
		sprintf (cmd, "%s %s %s", XC, xflags, fname);

	    if (verbose) {
		printf ("%s\n", cmd);
		fflush (stdout);
	    }

	    if (execute)
		exit_status = h_xc (cmd);
	    if (exit_status == INTERRUPT)
		fatals ("<ctrl/c> interrupt %s", cx->library);

	} else if (verbose) {
	    printf ("Object %s is up to date\n", makeobj(fname));
	    fflush (stdout);
	}
}


/* DO_XC -- Call XC.  Note that the current default xflags are not
 * automatically included in the generated command.
 */
int
do_xc (struct context *cx)
{
	char	cmd[SZ_CMD+1];


	if (debug > 1) {
	    printf ("do_xc:\n");
	    fflush (stdout);
	}

	if (irafdir[0])
	    sprintf (cmd, "%s -r %s", XC, irafdir);
	else
	    sprintf (cmd, "%s", XC);

	if (debug)
	    strcat (cmd, " -d");
	if (dbgout)
	    strcat (cmd, " -x");

	getcmd (cx, cmd, cmd, SZ_CMD);

	if (ifstate[iflev] == STOP)
	    return 0;

	if (verbose) {
	    printf ("%s\n", cmd);
	    fflush (stdout);
	}

	if (execute)
	    exit_status = h_xc (cmd);
	if (exit_status == INTERRUPT)
	    fatals ("<ctrl/c> interrupt %s", cx->library);

	return (exit_status);
}


/* DO_LINK -- Call XC to link a list of objects and/or libraries.  This is
 * equivalent to $XC, except that the LFLAGS are used instead of the XFLAGS.
 */
int
do_link (struct context *cx)
{
	register struct sfile *sflist, *sfp=NULL;
	static int skip_sf = 0;
	char *ip, token[SZ_FNAME+1];
	char linkline[SZ_CMD+1];
	char cmdbuf[SZ_CMD+1];
	char *cmd = cmdbuf;
	int lflags_set = 0;
	char *lflags;


	if (debug > 1) {
	    printf ("do_link:\n");
	    fflush (stdout);
	}

	/* Get the link command from the input stream. */
	getcmd (cx, "", linkline, SZ_CMD);

	/* Check whether the executable being generated is on the special
	 * file list.
	 */
	if (!skip_sf && (sflist = sf_dirsearch (cx->dirpath))) {
	    for (ip=linkline;  getword(&ip,token,SZ_FNAME);  )
		if (strcmp (token, "-o") == 0)
		    if (getword (&ip, token, SZ_FNAME))
			if ((sfp = sf_filesearch (sflist, token)))
			    break;
	}

	/* Check if LFLAGS is being substituted for this file. */
	if (sfp && strncmp (sfp->sf_mkobj, "LFLAGS", 6) == 0) {
	    for (ip=sfp->sf_mkobj;  *ip && *ip != '=';  ip++)
		;
	    lflags = (*ip == '=') ? ip + 1 : ip;
	    lflags_set++;
	} else
	    lflags = getsym (LFLAGS);

	if (irafdir[0])
	    sprintf (cmd, "%s %s -r %s", XC, lflags, irafdir);
	else
	    sprintf (cmd, "%s %s", XC, lflags);

	if (debug)
	    strcat (cmd, " -d");
	if (dbgout)
	    strcat (cmd, " -x");

	strcat (cmd, linkline);

	if (ifstate[iflev] == STOP)
	    return 0;

	/* Check whether a special $link command or other build command
	 * should be executed.
	 */
	if (sfp && !lflags_set) {
	    /* Push back the special link command. */
	    m_pushstr (cx, "\n");
	    m_pushstr (cx, sfp->sf_mkobj);

	    /* Avoid recursion if $link is pushed back. */
	    if (strncmp (sfp->sf_mkobj, "$link", 5) == 0)
		skip_sf++;
	    return (OK);
	}

	if (verbose) {
	    printf ("%s\n", cmd);
	    fflush (stdout);
	}

	if (execute)
	    exit_status = h_xc (cmd);
	if (exit_status == INTERRUPT)
	    fatals ("<ctrl/c> interrupt %s", cx->library);

	skip_sf = 0;
	return (exit_status);
}


/* DO_GENERIC -- Call the generic preprocessor.
 */
int
do_generic (struct context *cx)
{
	char	cmd[SZ_CMD+1];

	if (debug > 1) {
	    printf ("do_generic:\n");
	    fflush (stdout);
	}

	getcmd (cx, GENERIC, cmd, SZ_CMD);

	if (ifstate[iflev] == STOP)
	    return 0;

	if (verbose) {
	    printf ("%s\n", cmd);
	    fflush (stdout);
	}

	if (execute)
	    exit_status = os_cmd (cmd);
	if (exit_status == INTERRUPT)
	    fatals ("<ctrl/c> interrupt %s", cx->library);

	return (exit_status);
}


/* DO_SET -- Enter the name and value of a symbol (macro) into the symbol
 * table.
 */
void
do_set (struct context *cx)
{
	char	symbol[SZ_FNAME+1];
	char	value[SZ_PBBUF+1];

	if (debug > 1) {
	    printf ("do_set:\n");
	    fflush (stdout);
	}

	if (getkwvpair (cx, symbol, value) != ERR) {
	    if (ifstate[iflev] == STOP)
		return;

	    if (debug) {
		printf ("set %s = `%s'\n", symbol, value);
		fflush (stdout);
	    }
	    putsym (symbol, value);
	}
}


/* DO_INCHECK -- Check a file (e.g, library) back into the named directory.
 * (the "in" is first to make the external function name unique on systems
 * which truncate external names).
 */
int
do_incheck (struct context *cx)
{
	char	fname[SZ_FNAME+1];
	char	dname[SZ_FNAME+1];

	if (debug > 1) {
	    printf ("do_checkin:\n");
	    fflush (stdout);
	}

	strcpy (fname, getargs (cx));
	strcpy (dname, getargs (cx));

	exit_status = h_incheck (fname, dname);
	if (exit_status != OK)
	    warns ("error during checkin of %s", fname);

	return (exit_status);
}


/* DO_OUTCHECK -- Check a file (e.g, library) out of the named directory.
 */
int
do_outcheck (struct context *cx)
{
	char	fname[SZ_FNAME+1];
	char	dname[SZ_FNAME+1];
	int	clobber;

	if (debug > 1) {
	    printf ("do_checkout:\n");
	    fflush (stdout);
	}

	strcpy (fname, getargs (cx));
	strcpy (dname, getargs (cx));

	exit_status = h_outcheck (fname, dname, clobber=YES);
	if (exit_status != OK)
	    warns ("error during checkout of %s", fname);

	return (exit_status);
}


/* DO_COPYFILE -- Copy a file.
 */
int
do_copyfile (struct context *cx)
{
	char	old[SZ_FNAME+1];
	char	new[SZ_FNAME+1];

	if (debug > 1) {
	    printf ("do_copyfile:\n");
	    fflush (stdout);
	}

	strcpy (old, getargs (cx));
	strcpy (new, getargs (cx));

	if (ifstate[iflev] == STOP)
	    return 0;

	if (verbose) {
	    printf ("copy `%s' to `%s'\n", old, new);
	    fflush (stdout);
	}
	
	exit_status = h_copyfile (old, new);
	if (exit_status != OK)
	    warns ("error making copy of %s", old);

	return (exit_status);
}


/* DO_MOVEFILE -- Move a file to another directory, or rename the file in the
 * current directory.
 */
int
do_movefile (struct context *cx)
{
	register char *ip, *op;
	char	old[SZ_FNAME+1];
	char	new[SZ_PATHNAME+1];

	if (debug > 1) {
	    printf ("do_movefile:\n");
	    fflush (stdout);
	}

	strcpy (old, getargs (cx));
	strcpy (new, getargs (cx));

	if (ifstate[iflev] == STOP)
	    return 0;

	/* If NEW is a directory, concatenate the filename.  Always pass a
	 * filename to h_movefile.
	 */
	for (op=new;  *op;  op++)
	    ;
	if (*(op-1) == '$' || *(op-1) == '/')
	    for (ip=old;  (*op++ = *ip++);  )
		;

	if (verbose) {
	    printf ("move `%s' to `%s'\n", old, new);
	    fflush (stdout);
	}
	
	exit_status = h_movefile (old, new);
	if (exit_status != OK)
	    warns ("error moving file %s", old);

	return (exit_status);
}


/* DO_DELETE -- Delete a file or list of files.
 */
void
do_delete (struct context *cx)
{
	char	fname[SZ_PATHNAME+1];


	if (debug > 1) {
	    printf ("do_delete:\n");
	    fflush (stdout);
	}

	for (;;) {
	    strcpy (fname, getargs (cx));
	    if (fname[0] == EOS)
		return;

	    if (ifstate[iflev] == STOP)
		return;

	    if (execute) {
		if (verbose) {
		    printf ("delete file %s\n", vfn2osfn(fname,0));
		    fflush (stdout);
		}

		exit_status = os_delete (fname);
		if (exit_status != OK)
		    warns ("cannot delete file %s", fname);
	    }
	}
}


/* DO_PURGE -- Purge all files in a directory.  This is a no-op on systems
 * that do not support multiple file versions.
 */
void
do_purge (
  struct context *cx,		/* not used			*/
  char	*dname			/* logical directory name	*/
)
{
	if (debug > 1) {
	    printf ("do_purge: %s\n", dname);
	    fflush (stdout);
	}

	if (ifstate[iflev] == STOP)
	    return;

	exit_status = h_purge (dname);
	if (exit_status != OK)
	    warns ("error during purge of %s", dname);
}


/* GETCMD -- Extract a possibly multiline command from the input stream
 * into a buffer, with macro replacement in the process.
 */
int
getcmd (
  register struct context *cx,
  char	*prefix,		/* first part of command	*/
  char	*cmd,			/* receives the command		*/
  int	maxch
)
{
	register char	*op, *otop;
	register int	ch;


	otop = &cmd[maxch];

	if (cmd != prefix)
	    strcpy (cmd, prefix);
	
	for (op=cmd;  *op;  op++)
	    ;

	while (op < otop && (ch = m_getc(cx)) != EOF)
	    if (ch == ESCAPE) {
		ch = m_getc (cx);
		if (ch != '\n') {
		    *op++ = ESCAPE;
		    *op++ = ch;
		}
	    } else if (ch == '\n') {
		*op = EOS;
		break;
	    } else if (ch == PREPROCESSOR && *(op-1) == ' ') {
		/* $ is only recognized as a command delimiter if it occurs
		 * at the start of a new token.
		 */
		m_ungetc (ch, cx);
		*op = EOS;
		break;
	    } else
		*op++ = ch;

	return (op - cmd);
}


/* GETARGS -- Accumulate the argument list of a preprocessor macro.
 * The argument list may optionally be enclosed in parens or quotes,
 * otherwise we look for whitespace or newline as the delimiter.
 */
char *
getargs (
  register struct context *cx	/* current context	*/
)
{
	register int	ch;
	static	char	args[SZ_PBBUF+1];
	char	tokbuf[SZ_COMMAND+1];
	int	delim;


	while ((ch = m_getc(cx)) == ' ')
	    ;

	if (ch == '(')
	    delim = ')';
	else if (ch == '"' || ch == '\'')
	    delim = ch;
	else if (ch == SYSFILE_BEGIN)
	    delim = SYSFILE_END;
	else {
	    delim = ' ';
	    m_ungetc (ch, cx);
	}

	getstr (cx, tokbuf, SZ_COMMAND, delim);
	strcpy (args, tokbuf);

	if (delim == SYSFILE_END)
	    if (m_sysfile (tokbuf, args, SZ_PBBUF) <= 0)
		sprintf (args, "<%s>", tokbuf);

	return (args);
}


/* GETSTR -- Accumulate a string from the input stream, stopping when the
 * specified delimiter character is seen.  Note that macros are expanded
 * even within quoted strings, as in MAKE (macros are defined at the character
 * level, rather than at the token level).
 */
int
getstr (
  register struct context *cx,	/* current context	*/
  char	*outstr,		/* receives string	*/
  int	maxch,			/* max chars out	*/
  int	delim 			/* delimiter character	*/
)
{
	register char	*op;
	register int	ch, n;

	for (op=outstr, n=maxch;  --n >= 0 && (ch = m_getc(cx)) != delim;  )
	    if (ch == '\\') {
		ch = m_getc(cx);
		if (ch == '\n')
		    ;
		else if (ch == delim)
		    *op++ = ch;
		else {
		    *op++ = '\\';
		    *op++ = ch;
		}
	    } else if (ch == '\n' || ch == EOF) {
		*op = EOS;
		if (delim != ' ')
		    warns ("missing closing quote in string `%s'", outstr);
		m_ungetc ('\n', cx);
		break;
	    } else
		*op++ = ch;

	*op = EOS;
	return (op - outstr);
}


/* GETKWVPAIR -- Extract the keyword and value fields from a "keyword=value"
 * construct in the input stream.
 */
int
getkwvpair (
  register struct context *cx,	/* current context		*/
  char	*symbol,		/* receives name of symbol	*/
  char	*value			/* receives value of symbol	*/
)
{
	register char	*op;
	register int	ch;

	while ((ch = m_getc(cx)) == ' ')
	    ;
	if (!isalpha(ch)) {
	    m_ungetc (ch, cx);
	    return (ERR);
	}

	/* Extract module name */
	for (op=symbol, *op++ = ch;  (ch = m_getc(cx)) != '=';  ) {
	    if (ch == ' ') {
		continue;
	    } else if (ch == '\n') {
		warns ("missing `=' in $set statement `%s'", symbol);
		m_ungetc ('\n', cx);
		return (ERR);
	    } else
		*op++ = ch;
	}
	*op = EOS;

	/* Extract symbol value */
	strcpy (value, getargs(cx));
	return (OK);
}


/* GETWORD -- Extract a whitespace delimited substring from a string.
 * The input pointer is left pointing to the first character following
 * the extracted string.
 */
int
getword (
  char **str,
  char *outstr,
  int maxch
)
{
	register char *ip=(*str), *op=outstr;
	register char *otop = outstr + maxch;
	register int ch;

	while (*ip && isspace (*ip))
	    ip++;

	while (op < otop && (ch = *ip++))
	    if (isspace (ch))
		break;
	    else
		*op++ = ch;

	*op = EOS;
	*str = ip;

	return (op - outstr);
}


/* PUTSYM -- Add a symbol (macro definition) to the symbol table.  Symbol
 * storage is in the string buffer, with all symbols defined local to a
 * module being discarded when the module exits.  All symbols are globally
 * accessible, with local symbols possibly redefining (temporarily) existing
 * external symbols (e.g., the value of "xflags" might be reset locally,
 * but should not affect outer level code once the module has exited).
 * Symbol names are treated in a case insensitive fashion to simplify use
 * on systems that do not preserve case, e.g., in the MKPKG argument list.
 */
void
putsym (
  char	*name,		/* symbol name		*/
  char	*value		/* symbol value		*/
)
{
	char	*symbol;

	if (debug) {
	    printf ("put symbol %s = `%s'\n", name, value);
	    fflush (stdout);
	}

	symbol = mklower (name);
	symtab[nsymbols].s_name  = putstr (symbol);
	symtab[nsymbols].s_value = putstr (value);

	if (++nsymbols >= MAX_SYMBOLS)
	    fatals ("too many symbols (`%s')", name);
}


/* GETSYM -- Lookup a symbol in the symbol table.  Return the symbol value
 * as the function value if the symbol is found, else return NULL.  The symbol
 * table is searched most-recently-defined symbols first, permitting symbols
 * to be redefined locally.  Note that the full table is searched, hence the
 * outer symbols are globally accessible.  The number of symbols tends to be
 * quite small and symbol lookup only occurs when a macro is explicitly
 * referenced as $(NAME), hence a simple linear search is best.
 */
char *
getsym (
  char	*name		/* symbol name		*/
)
{
	register struct	symbol *sp, *stop;
	register int	ch;
	char	*symbol;

	symbol = mklower (name);
	stop = &symtab[0];
	sp   = &symtab[nsymbols];
	ch   = symbol[0];

	/* Search the symbol table.
	 */
	while (--sp >= stop)
	    if (sp->s_name[0] == ch)
		if (strcmp (sp->s_name, symbol) == 0)
		    return (sp->s_value);
	
	return (NULL);
}


/* MKLOWER -- Convert a small string to lower case and return a pointer to
 * a local copy of the new string.
 */
char *
mklower (char *s)
{
	register char	*ip, *op;
	register int	n, ch;
	static	char lstr[SZ_FNAME+1];

	for (ip=s, op=lstr, n=SZ_FNAME;  --n >= 0 && (ch = *ip++);  )
	    if (isupper (ch))
		*op++ = tolower (ch);
	    else
		*op++ = ch;
	*op = EOS;

	return (lstr);
}
