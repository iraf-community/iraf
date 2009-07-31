/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define import_spp
#define import_error
#include <iraf.h>

#include "../bootlib/bootlib.h"
#include "mkpkg.h"


/*
 * TOK.C -- Preprocessor functions.
 */

static int do_osescape ( struct context * );
static int do_ppdir ( struct context *, const char * );
static int getstr ( struct context *, char *, size_t, int );
static void do_if ( struct context *, const char * );
static void do_else ( struct context * );
static void do_endif ( struct context * );
static void do_end ( struct context * );
static int do_call ( struct context *, const char *, int );
static char *getargs ( struct context * );
static void do_echo ( struct context *, const char * );
static void do_set ( struct context * );
static int do_incheck ( struct context * );
static int do_outcheck ( struct context * );
static int do_copyfile ( struct context * );
static int do_delete ( struct context * );
static int do_generic ( struct context * );
static int do_link ( struct context * );
static int do_movefile ( struct context * );
static int do_omake ( struct context *, const char * );
static int do_purge ( struct context *, const char * );
static int do_xc ( struct context * );
static int getkwvpair ( struct context *, char *, size_t, char *, size_t );
static int getcmd ( struct context *, const char *, char *, size_t );
static int getword ( const char **, char *, size_t );
static const char *mklower ( const char * );

/* GETTOK -- Get the next token from the make file currently being scanned.
 * Conditional interpretation is provided via the $IFxxx directives.
 */
/* cx     : current context	*/
/* outstr : receives token	*/
int gettok ( struct context *cx, char *outstr, size_t bufsize )
{
	char *op, *maxop;
	char tokbuf[SZ_COMMAND+1];
	int token, delim, ch;

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
	    maxop = tokbuf + SZ_COMMAND+1 -1;
	    for (op=tokbuf, *op++ = ch;  (ch = m_getc(cx)) != EOF;  )
		if (islower (ch)) {
		    if ( op < maxop ) *op++ = ch;
		}
		else if (isupper (ch)) {
		    if ( op < maxop ) *op++ = tolower (ch);
		}
		else {
		    m_ungetc (ch, cx);
		    break;
		}

	    *op = EOS;
	    if (strncmp (tokbuf, "$exit", 5) == 0)
		return (TOK_END);

	    if ( do_ppdir (cx, tokbuf) != OK ) {
		fatals ("%s", "xc returns error.");
	    }
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
	    getstr (cx, tokbuf, SZ_COMMAND+1, SYSFILE_END);
	    if (m_sysfile (tokbuf, outstr, bufsize) <= 0)
		snprintf (outstr, bufsize, "<%s>", tokbuf);

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
	    getstr (cx, outstr, bufsize, delim = ch);
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
	    getstr (cx, outstr, bufsize, delim = ' ');
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
static int do_osescape ( struct context *cx )
{
	char cmd[SZ_CMD+1];
	int ch;

	if (debug > 1) {
	    printf ("do_osescape:\n");
	    fflush (stdout);
	}

	ch = m_getc (cx);
	if (ch == '(' || ch == '\'' || ch == '"') {
	    getstr (cx, cmd, SZ_CMD+1, (ch == '(' ? ')' : ch));
	} else if (ch == '\n') {
	    exit_status = OK;
	    return exit_status;	/* OK ??? */

	} else {
	    char    *op, *otop;

	    op    = cmd;
	    *op++ = ch;
	    otop  = &cmd[SZ_CMD];

	    while ( (ch = m_getc(cx)) != EOF ) {
		if (ch == ESCAPE) {
		    ch = m_getc (cx);
		    if (ch != '\n') {
			if ( op < otop ) *op++ = ESCAPE;
			if ( op < otop ) *op++ = ch;
		    }
		} else if (ch == '\n') {
		    break;
		} else {
		    if ( op < otop ) *op++ = ch;
		}
	    }
	    *op = EOS;
	}

	if (ifstate[iflev] == STOP)
	    return OK;
	if (verbose) {
	    printf ("!%s\n", cmd);
	    fflush (stdout);
	}

	if (execute) {
	    exit_status = os_cmd (cmd);
	    if (exit_status == INTERRUPT) {
		fatals ("<ctrl/c> interrupt %s", cx->library);
	    }
	}
	else {
	    exit_status = OK;
	}

	return exit_status;
}


/* DO_PPDIR -- Execute a preprocessor directive.  A hash table would be more
 * efficient, but the complexity is not warranted since this is only called
 * when a $ prefixed preprocessor directive has already been recognized in
 * the input.
 */
/* cx    : current context		*/
/* token : directive to be executed	*/
static int do_ppdir ( struct context *cx, const char *token )
{
	int	islib;
	int	status = OK;

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
	    status = do_link (cx);
	else if (strncmp (token, "$move",	5) == 0)
	    do_movefile (cx);
	else if (strncmp (token, "$omake",	6) == 0)
	    status = do_omake (cx, getargs(cx));
	else if (strncmp (token, "$purge",	6) == 0)
	    do_purge (cx, getargs(cx));
	else if (strncmp (token, "$xc",		3) == 0)
	    status = do_xc (cx);

	else if (strncmp (token, "$debug",	6) == 0) {
	    if (debug = (strcmp (getargs(cx), "off") != 0))
		verbose++; }
	else if (strncmp (token, "$verbose",	8) == 0)
	    verbose = (strcmp (getargs(cx), "off") != 0);

	else
	    warns ("illegal preprocessor directive `%s'", token);

	return status;
}


/* DO_IF -- Called when a "$if.." token is seen in the input stream.  Read in
 * the predicate and set the state of the ifcode accordingly.
 */
static void do_if ( struct context *cx, const char *keyword )
{
	char tokbuf[SZ_COMMAND+1];
	char buf[SZ_PREDBUF];
	char *argv[MAX_ARGS];
	char *op, *maxop;
	const char *key;
	long fdate, altdate;
	int argc, negate, bval, ch, i;
 
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
	maxop	= buf + SZ_PREDBUF -1;
	argc    = 0;

	while ( (ch = m_getc(cx)) != ')' ) {
	    if (ch == ESCAPE) {
		ch = m_getc (cx);
		if (ch == '\n')
		    continue;
		else {
		    if ( op < maxop ) *op++ = ch;
		}
	    } else if (ch == '\n') {
		warns ("missing right paren in `%s'", keyword);
	    } else if (ch == EOF) {
		warns ("unexpected EOF in `%s'", keyword);
	    } else if (ch == ' ') {
		continue;
	    } else if (ch == SYSFILE_BEGIN && op == argv[argc]) {
		getstr (cx, tokbuf, SZ_COMMAND+1, SYSFILE_END);
		if (m_sysfile (tokbuf, op, SZ_PREDBUF-(op-buf)) <= 0)
		    snprintf (op, SZ_PREDBUF-(op-buf), "<%s>", tokbuf);
		while (*op)
		    op++;
		continue;
	    } else if (ch == ':' || ch == ',') {
		if ( op <= maxop ) *op = EOS;
		if ( maxop <= op )
		    warns ("predicate too large in `%s'", keyword);
		argc++;
		if ( MAX_ARGS <= argc ) {
		    warns ("too many arguments in `%s' predicate", keyword);
		    argc--;
		}
		if ( op < maxop ) op++;
		argv[argc] = op;
	    } else {
		if ( op < maxop ) *op++ = ch;
	    }
	}

	if ( op <= maxop ) *op = EOS;
	argc++;

	if (++iflev >= SZ_IFSTACK) {
	    warns ("$IFs nested too deeply (%s)", keyword);
	    iflev--;
	}

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
	    const char *valstr;

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
	    if ((fdate = os_fdate(argv[0])) <= 0) {
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
	    if ((fdate = os_fdate(argv[0])) <= 0)
		warns ("file `%s' not found", argv[0]);
	    else {
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
static void do_else ( struct context *cx )
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
static void do_endif ( struct context *cx )
{
	if (debug > 1) {
	    printf ("do_endif:\n");
	    fflush (stdout);
	}

	if ( iflev <= 0 ) {
	    warns ("unmatched %s", "$endif");
	}
	iflev--;
}


/* DO_END -- Called when the token "$end" is seen in the input stream.
 * Clear the if stack and reenable pass-token.
 */
static void do_end ( struct context *cx )
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
/* cx      : current context		*/
/* program : module to be called	*/
/* islib   : module list for a library	*/
static int do_call ( struct context *cx, const char *program, int islib )
{
	struct	context *ncx;
	char	module[SZ_FNAME+1], subdir[SZ_FNAME+1], fname[SZ_FNAME+1];
	char	symbol[SZ_FNAME+1], value[SZ_COMMAND+1];
	char	modspec[SZ_FNAME+1];
	char	*old_cp;
	int	old_nsymbols;

	safe_strcpy (modspec, SZ_FNAME+1, program);
	if (debug && ifstate[iflev] == PASS) {
	    printf ("$call %s\n", modspec);
	    fflush (stdout);
	}

	old_cp = cp;
	old_nsymbols = nsymbols;

	/* Process the argument list, if any, into the symbol table.
	 */
	while (getkwvpair (cx, symbol, SZ_FNAME+1, value, SZ_COMMAND+1) != ERR)
	    if (ifstate[iflev] == PASS)
		putsym (symbol, value);

	if (ifstate[iflev] == STOP)
	    return OK;

	/* Parse the module name, push a new context, and execute the
	 * subroutine.
	 */
	parse_modname (modspec, module, subdir, fname, SZ_FNAME+1);
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

	return exit_status;
}


/* DO_ECHO -- Print a message on the standard output.
 */
static void do_echo ( struct context *cx, const char *msg )
{
	if (ifstate[iflev] == PASS) {
	    printf ("%s\n", msg);
	    fflush (stdout);
	}
}


/* DO_GOTO -- Advance the file pointer to the named symbol in the current
 * file, without changing the current context.
 */
int do_goto ( struct context *cx, const char *symbol )
{
	char match[SZ_FNAME+1];
	char lbuf[SZ_LINE+1];
	const char *ip;
	long fpos;
	int len_matchstr;

	if (ifstate[iflev] == STOP)
	    return (OK);

	if (debug) {
	    printf ("goto %s\n", symbol);
	    fflush (stdout);
	}

	snprintf (match, SZ_FNAME+1, "%s:", symbol);
	len_matchstr = strlen (match);

	fpos = k_ftell (cx);
	if (cx->fp != stdin)
	    k_fseek (cx, 0L, 0);

	while (k_fgets (lbuf, SZ_LINE+1, cx) != NULL) {
	    cx->lineno++;
	    for (ip=lbuf;  isspace (*ip);  ip++)
		;
	    if (strncmp (ip, match, len_matchstr) == 0) {
		/* GOTO clears the IF stack back to where it whatever it was
		 * upon entry to the module.
		 */
		if (cx->prev && cx->prev->old_iflev >= 0)
		    iflev = cx->prev->old_iflev;
		exit_status = OK;
		return (OK);
	    }
	}

	warns ("could not find mkpkg module or label `%s'", symbol);
	if (cx->fp != stdin)
	    k_fseek (cx, fpos, 0);

	exit_status = ERR;
	return (ERR);
}


/* DO_INCLUDE -- Open a file and execute any preprocessor directives therein.
 * Macros defined in an include are retained after the context of the include
 * is popped.
 */
/* cx    : current context	*/
/* fname : include file name	*/
int do_include ( struct context *cx, const char *fname )
{
	struct context *ncx;
	int islib;

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

	exit_status = OK;
	return (OK);
}


/* DO_OMAKE -- Generate the object module for the named source module, if
 * the object does not exist or is older than the source module.
 */
static int do_omake ( struct context *cx, const char *fname )
{
	char cmd[SZ_COMMAND+1];
	char xflags[SZ_LINE+1];
	const char *dflist[MAX_DEPFILES+1];
	const char *s_xflags, *dfile;
	long sourcedate, objdate, date;
	int recompile, i;

	if (ifstate[iflev] == STOP)
	    return OK;

	if (debug) {
	    printf ("omake %s\n", fname);
	    fflush (stdout);
	}

	if ((sourcedate = os_fdate (fname)) <= 0) {
	    warns ("file `%s' not found", fname);
	    exit_status = ERR;
	    return exit_status;	/* OK ??? */

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

	exit_status = OK;

	if (recompile) {
	    /* Get XFLAGS. */
	    s_xflags = getsym (XFLAGS);
	    if ( s_xflags == NULL ) s_xflags = "";
	    xflags[0] = EOS;
	    if (debug)
		safe_strcat (xflags, SZ_LINE+1, "-d ");
	    if (dbgout)
		safe_strcat (xflags, SZ_LINE+1, "-x ");
	    safe_strcat (xflags, SZ_LINE+1, s_xflags);

	    if (irafdir[0])
		snprintf (cmd, SZ_COMMAND+1, "%s %s -r %s %s", XC, xflags, irafdir, fname);
	    else
		snprintf (cmd, SZ_COMMAND+1, "%s %s %s", XC, xflags, fname);

	    if (verbose) {
		printf ("%s\n", cmd);
		fflush (stdout);
	    }

	    if (execute) {
		exit_status = h_xc (cmd);
		if (exit_status == INTERRUPT) {
		    fatals ("<ctrl/c> interrupt %s", cx->library);
		}
	    }

	} else if (verbose) {
	    printf ("Object %s is up to date\n", makeobj(fname));
	    fflush (stdout);
	}

	return exit_status;	/* OK ??? */
}


/* DO_XC -- Call XC.  Note that the current default xflags are not
 * automatically included in the generated command.
 */
static int do_xc ( struct context *cx )
{
	char	cmd[SZ_CMD+1];

	if (debug > 1) {
	    printf ("do_xc:\n");
	    fflush (stdout);
	}

	if (irafdir[0])
	    snprintf (cmd, SZ_CMD+1, "%s -r %s", XC, irafdir);
	else
	    snprintf (cmd, SZ_CMD+1, "%s", XC);

	if (debug)
	    safe_strcat (cmd, SZ_CMD+1, " -d");
	if (dbgout)
	    safe_strcat (cmd, SZ_CMD+1, " -x");

	getcmd (cx, cmd, cmd, SZ_CMD+1);

	if (ifstate[iflev] == STOP)
	    return OK;

	if (verbose) {
	    printf ("%s\n", cmd);
	    fflush (stdout);
	}

	if (execute) {
	    exit_status = h_xc (cmd);
	    if (exit_status == INTERRUPT) {
		fatals ("<ctrl/c> interrupt %s", cx->library);
	    }
	}
	else {
	    exit_status = OK;
	}

	return (exit_status);
}


/* DO_LINK -- Call XC to link a list of objects and/or libraries.  This is
 * equivalent to $XC, except that the LFLAGS are used instead of the XFLAGS.
 */
static int do_link ( struct context *cx )
{
	static int skip_sf = 0;
	struct sfile *sfp=NULL;
	struct sfile *sflist;
	const char *lflags;
	const char *ip;
	char linkline[SZ_CMD+1];
	char token[SZ_FNAME+1];
	char cmdbuf[SZ_CMD+1];
	char *cmd = cmdbuf;
	int lflags_set = 0;

	if (debug > 1) {
	    printf ("do_link:\n");
	    fflush (stdout);
	}

	/* Get the link command from the input stream. */
	getcmd (cx, "", linkline, SZ_CMD+1);

	/* Check whether the executable being generated is on the special
	 * file list.
	 */
	if (!skip_sf && (sflist = sf_dirsearch (cx->dirpath))) {
	    for (ip=linkline;  getword(&ip,token,SZ_FNAME+1);  )
		if (strcmp (token, "-o") == 0)
		    if (getword (&ip, token, SZ_FNAME+1))
			if (sfp = sf_filesearch (sflist, token))
			    break;
	}

	/* Check if LFLAGS is being substituted for this file. */
	if (sfp && strncmp (sfp->sf_mkobj, "LFLAGS", 6) == 0) {
	    for (ip=sfp->sf_mkobj;  *ip && *ip != '=';  ip++)
		;
	    lflags = (*ip == '=') ? ip + 1 : ip;
	    lflags_set++;
	} else {
	    lflags = getsym (LFLAGS);
	    if ( lflags == NULL ) lflags = "";
	}

	if (irafdir[0])
	    snprintf (cmd, SZ_CMD+1, "%s %s -r %s", XC, lflags, irafdir);
	else
	    snprintf (cmd, SZ_CMD+1, "%s %s", XC, lflags);

	if (debug)
	    safe_strcat (cmd, SZ_CMD+1, " -d");
	if (dbgout)
	    safe_strcat (cmd, SZ_CMD+1, " -x");

	safe_strcat (cmd, SZ_CMD+1, linkline);

	if (ifstate[iflev] == STOP)
	    return OK;

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
	    exit_status = OK;	/* OK ??? */
	    return (OK);
	}

	if (verbose) {
	    printf ("%s\n", cmd);
	    fflush (stdout);
	}

	if (execute) {
	    exit_status = h_xc (cmd);
	    if (exit_status == INTERRUPT) {
		fatals ("<ctrl/c> interrupt %s", cx->library);
	    }
	}
	else {
	    exit_status = OK;
	}

	skip_sf = 0;
	return (exit_status);
}


/* DO_GENERIC -- Call the generic preprocessor.
 */
static int do_generic ( struct context *cx )
{
	char	cmd[SZ_CMD+1];

	if (debug > 1) {
	    printf ("do_generic:\n");
	    fflush (stdout);
	}

	getcmd (cx, GENERIC, cmd, SZ_CMD+1);

	if (ifstate[iflev] == STOP)
	    return OK;

	if (verbose) {
	    printf ("%s\n", cmd);
	    fflush (stdout);
	}

	if (execute) {
	    exit_status = os_cmd (cmd);
	    if (exit_status == INTERRUPT) {
		fatals ("<ctrl/c> interrupt %s", cx->library);
	    }
	}
	else {
	    exit_status = OK;
	}

	return (exit_status);
}


/* DO_SET -- Enter the name and value of a symbol (macro) into the symbol
 * table.
 */
static void do_set ( struct context *cx )
{
	char	symbol[SZ_FNAME+1];
	char	value[SZ_PBBUF+1];

	if (debug > 1) {
	    printf ("do_set:\n");
	    fflush (stdout);
	}

	if (getkwvpair (cx, symbol, SZ_FNAME+1, value, SZ_PBBUF+1) != ERR) {
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
static int do_incheck ( struct context *cx )
{
	char	fname[SZ_FNAME+1];
	char	dname[SZ_FNAME+1];

	if (debug > 1) {
	    printf ("do_checkin:\n");
	    fflush (stdout);
	}

	safe_strcpy (fname, SZ_FNAME+1, getargs (cx));
	safe_strcpy (dname, SZ_FNAME+1, getargs (cx));

	exit_status = h_incheck (fname, dname);
	if (exit_status != OK)
	    warns ("error during checkin of %s", fname);

	return (exit_status);
}


/* DO_OUTCHECK -- Check a file (e.g, library) out of the named directory.
 */
static int do_outcheck ( struct context *cx )
{
	char	fname[SZ_FNAME+1];
	char	dname[SZ_FNAME+1];
	int	clobber;

	if (debug > 1) {
	    printf ("do_checkout:\n");
	    fflush (stdout);
	}

	safe_strcpy (fname, SZ_FNAME+1, getargs (cx));
	safe_strcpy (dname, SZ_FNAME+1, getargs (cx));

	exit_status = h_outcheck (fname, dname, clobber=YES);
	if (exit_status != OK)
	    warns ("error during checkout of %s", fname);

	return (exit_status);
}


/* DO_COPYFILE -- Copy a file.
 */
static int do_copyfile ( struct context *cx )
{
	char	old[SZ_FNAME+1];
	char	new[SZ_FNAME+1];

	if (debug > 1) {
	    printf ("do_copyfile:\n");
	    fflush (stdout);
	}

	safe_strcpy (old, SZ_FNAME+1, getargs (cx));
	safe_strcpy (new, SZ_FNAME+1, getargs (cx));

	if (ifstate[iflev] == STOP)
	    return OK;

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
static int do_movefile ( struct context *cx )
{
	const char *ip;
	char *op, *maxop;
	char old[SZ_FNAME+1];
	char new[SZ_PATHNAME+1];

	if (debug > 1) {
	    printf ("do_movefile:\n");
	    fflush (stdout);
	}

	safe_strcpy (old, SZ_FNAME+1, getargs (cx));
	safe_strcpy (new, SZ_PATHNAME+1, getargs (cx));

	if (ifstate[iflev] == STOP)
	    return OK;

	/* If NEW is a directory, concatenate the filename.  Always pass a
	 * filename to h_movefile.
	 */
	maxop = new + SZ_PATHNAME+1 -1;
	for (op=new;  *op;  op++)
	    ;
	if ( new < op && (*(op-1) == '$' || *(op-1) == '/') ) {
	    for ( ip=old ; op < maxop && (*ip) ; op++, ip++ )
		*op = *ip;
	    *op = EOS;
	}

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
static int do_delete ( struct context *cx )
{
	char	fname[SZ_PATHNAME+1];

	if (debug > 1) {
	    printf ("do_delete:\n");
	    fflush (stdout);
	}

	exit_status = OK;

	for (;;) {
	    safe_strcpy (fname, SZ_PATHNAME+1, getargs (cx));
	    if (fname[0] == EOS)
		return exit_status;

	    if (ifstate[iflev] == STOP)
		return OK;

	    if (execute) {
		int status;
		if (verbose) {
		    printf ("delete file %s\n", vfn2osfn(fname,0));
		    fflush (stdout);
		}

		status = os_delete (fname);
		if (status != OK) {
		    exit_status = status;
		    warns ("cannot delete file %s", fname);
		}
	    }
	}
}


/* DO_PURGE -- Purge all files in a directory.  This is a no-op on systems
 * that do not support multiple file versions.
 */
/* cx    : not used			*/
/* dname : logical directory name	*/
static int do_purge ( struct context *cx, const char *dname )
{
	if (debug > 1) {
	    printf ("do_purge: %s\n", dname);
	    fflush (stdout);
	}

	if (ifstate[iflev] == STOP)
	    return OK;

	exit_status = h_purge (dname);
	if (exit_status != OK)
	    warns ("error during purge of %s", dname);

	return exit_status;
}


/* GETCMD -- Extract a possibly multiline command from the input stream
 * into a buffer, with macro replacement in the process.
 */
/* prefix : first part of command	*/
/* cmd    : receives the command	*/
static int getcmd ( struct context *cx, const char *prefix, 
		    char *cmd, size_t bufsize )
{
	char *op, *otop;
	int ch;

	otop = cmd + bufsize -1;
	safe_strcpy (cmd, bufsize, prefix);
	for (op=cmd;  *op;  op++)
	    ;

	while ( (ch = m_getc(cx)) != EOF ) {
	    if (ch == ESCAPE) {
		ch = m_getc (cx);
		if (ch != '\n') {
		    if ( op < otop ) *op++ = ESCAPE;
		    if ( op < otop ) *op++ = ch;
		}
	    } else if (ch == '\n') {
		if ( op <= otop ) *op = EOS;
		break;
	    } else if (ch == PREPROCESSOR && cmd < op && *(op-1) == ' ') {
		/* $ is only recognized as a command delimiter if it occurs
		 * at the start of a new token.
		 */
		m_ungetc (ch, cx);
		if ( op <= otop ) *op = EOS;
		break;
	    } else {
		if ( op < otop ) *op++ = ch;
	    }
	}
	if ( op <= otop ) *op = EOS;
	return (op - cmd);
}


/* GETARGS -- Accumulate the argument list of a preprocessor macro.
 * The argument list may optionally be enclosed in parens or quotes,
 * otherwise we look for whitespace or newline as the delimiter.
 */
/* cx : current context	*/
static char *getargs ( struct context *cx )
{
	static char args[SZ_PBBUF+1];
	char tokbuf[SZ_COMMAND+1];
	int delim, ch;

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

	getstr (cx, tokbuf, SZ_COMMAND+1, delim);
	safe_strcpy (args, SZ_PBBUF+1, tokbuf);

	if (delim == SYSFILE_END)
	    if (m_sysfile (tokbuf, args, SZ_PBBUF+1) <= 0)
		snprintf (args, SZ_PBBUF+1, "<%s>", tokbuf);

	return (args);
}


/* GETSTR -- Accumulate a string from the input stream, stopping when the
 * specified delimiter character is seen.  Note that macros are expanded
 * even within quoted strings, as in MAKE (macros are defined at the character
 * level, rather than at the token level).
 */
/* cx      : current context		*/
/* outstr  : receives string		*/
/* bufsize : max chars out		*/
/* delim   : delimiter character	*/
static int getstr ( struct context *cx, char *outstr, size_t bufsize, 
		    int delim)
{
	char *op, *maxop;
	int ch;

	maxop = outstr + bufsize -1;
	for ( op=outstr ; (ch = m_getc(cx)) != delim ; ) {
	    if (ch == '\\') {
		ch = m_getc(cx);
		if (ch == '\n')
		    ;
		else if (ch == delim) {
		    if ( op < maxop ) *op++ = ch;
		}
		else {
		    if ( op < maxop ) *op++ = '\\';
		    if ( op < maxop ) *op++ = ch;
		}
	    } else if (ch == '\n' || ch == EOF) {
		if ( op <= maxop ) *op = EOS;
		if (delim != ' ')
		    warns ("missing closing quote in string `%s'", outstr);
		m_ungetc ('\n', cx);
		break;
	    } else {
		if ( op < maxop ) *op++ = ch;
	    }
	}
	if ( op <= maxop ) *op = EOS;
	return (op - outstr);
}


/* GETKWVPAIR -- Extract the keyword and value fields from a "keyword=value"
 * construct in the input stream.
 */
/* cx     : current context		*/
/* symbol : receives name of symbol	*/
/* value  : receives value of symbol	*/
static int getkwvpair( struct context *cx, char *symbol, size_t bufsize_symbol,
		       char *value, size_t bufsize_value)
{
	char *op, *maxop;
	int ch;

	while ((ch = m_getc(cx)) == ' ')
	    ;
	if (!isalpha(ch)) {
	    m_ungetc (ch, cx);
	    return (ERR);
	}

	/* Extract module name */
	maxop = symbol + bufsize_symbol -1;
	op = symbol;
	if ( op < maxop ) *op++ = ch;
	for ( ; (ch = m_getc(cx)) != '=' ; ) {
	    if (ch == ' ') {
		continue;
	    } else if (ch == '\n') {
		if ( op <= maxop ) *op = EOS;
		warns ("missing `=' in $set statement `%s'", symbol);
		m_ungetc ('\n', cx);
		return (ERR);
	    } else {
		if ( op < maxop ) *op++ = ch;
	    }
	}
	if ( op <= maxop ) *op = EOS;

	/* Extract symbol value */
	safe_strcpy (value, bufsize_value, getargs(cx));
	return (OK);
}


/* GETWORD -- Extract a whitespace delimited substring from a string.
 * The input pointer is left pointing to the first character following
 * the extracted string.
 */
static int getword ( const char **str, char *outstr, size_t bufsize )
{
	const char *ip=(*str);
	char *op = outstr;
	char *otop = outstr + bufsize -1;

	while (*ip && isspace (*ip))
	    ip++;

	while ( *ip ) {
	    if (isspace (*ip))
		break;
	    else {
		if ( op < otop ) *op++ = *ip;
	    }
	    ip++;
	}

	if ( op <= otop ) *op = EOS;
	if ( *ip == EOS ) *str = ip;
	else *str = ip+1;

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
/* name  : symbol name		*/
/* value : symbol value		*/
void putsym ( const char *name, const char *value )
{
	const char *symbol;

	if (debug) {
	    printf ("put symbol %s = `%s'\n", name, value);
	    fflush (stdout);
	}

	symbol = mklower (name);

	if ( MAX_SYMBOLS <= nsymbols )
	    fatals ("too many symbols (`%s')", name);

	symtab[nsymbols].s_name  = putstr (symbol);
	symtab[nsymbols].s_value = putstr (value);

	nsymbols++;
}


/* GETSYM -- Lookup a symbol in the symbol table.  Return the symbol value
 * as the function value if the symbol is found, else return NULL.  The symbol
 * table is searched most-recently-defined symbols first, permitting symbols
 * to be redefined locally.  Note that the full table is searched, hence the
 * outer symbols are globally accessible.  The number of symbols tends to be
 * quite small and symbol lookup only occurs when a macro is explicitly
 * referenced as $(NAME), hence a simple linear search is best.
 */
/* name : symbol name		*/
char *getsym ( const char *name )
{
	struct symbol *sp, *stop;
	const char *symbol;
	int ch;

	symbol = mklower (name);
	stop = &symtab[0];
	sp   = &symtab[nsymbols];
	ch   = symbol[0];

	/* Search the symbol table.
	 */
	while ( sp > stop ) {
	    sp--;
	    if (sp->s_name[0] == ch)
		if (strcmp (sp->s_name, symbol) == 0)
		    return (sp->s_value);
	}
	
	return (NULL);
}


/* MKLOWER -- Convert a small string to lower case and return a pointer to
 * a local copy of the new string.
 */
static const char *mklower ( const char *s )
{
	static char lstr[SZ_FNAME+1];
	char *op, *maxop;
	const char *ip;

	maxop = lstr + SZ_FNAME+1 -1;
	for ( ip=s, op=lstr ; op < maxop && (*ip) ; op++, ip++ ) {
	    if (isupper (*ip))
		*op = tolower (*ip);
	    else
		*op = *ip;
	}
	*op = EOS;

	return (lstr);
}
