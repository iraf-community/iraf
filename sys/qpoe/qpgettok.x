# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<ctype.h>
include	<fset.h>
include	"qpoe.h"

.help gettok
.nf --------------------------------------------------------------------------
GETTOK -- Lexical input routines for QPOE.  Used to return tokens from input
text; this is where all macro expansion and file expansion takes place.

	       gt = qp_opentext (qp, text)
	      token = qp_gettok (gt, tokbuf, maxch)
		    qp_ungettok (gt, tokbuf)
	      token = qp_rawtok (gt, tokbuf, maxch)
	     token = qp_nexttok (gt)
		   qp_closetext (gt)

Access to the package is gained by opening a text string with QP_OPENTEXT.
This returns a descriptor which is passed to QP_GETTOK to read successive
tokens, which may come from the input text string or from any macros,
include files, etc., referenced in the text or in any substituted text.
QP_UNGETTOK pushes a token back into the QP_GETTOK input stream, to be
returned in the next QP_GETTOK call (following macro expansion).

QP_RAWTOK returns the next physical token from an input stream (without
macro expansion), and QP_NEXTTOK returns the type of the next *physical*
token (no macro expansion) without actually fetching it (for look ahead
decision making).

The tokens that can be returned are as follows:

	TOK_IDENTIFIER		[a-zA-Z][a-zA-Z0-9_]*
	TOK_NUMBER		[0-9][0-9a-zA-Z.]*(e|E)?(+|-)?[0-9]*
	TOK_STRING		if "abc" or 'abc', the abc
	'c'			other characters, e.g., =+-*/,;:()[] etc
	EOF			at end of input

Macro replacement syntax:

	macro			push macro with null arglist
	macro(arg,arg,...)	push macro with argument substitution
	@file			push contents of file
	@file(arg,arg,...)	push file with argument substitution
	`cmd`			substitute output of CL command "cmd"

where
	macro			is an identifier, the name of a global macro
				or a datafile local macro (parameter)

In all cases, occurences of $N in the replacement text are replaced by the
macro arguments if any, and macros are recursively expanded.  Whitespace,
including newline, equates to a single space, as does EOF (hence always
delimits tokens).  Comments (# to end of line) are ignored.  All identifiers
in scanned text are checked to see if they are references to predefined global
or local (datafile) macros.

A global macro is a symbol defined globally for QPOE, in effect for all poefile
accesses (see qpmacro.x).  A local macro is a macro defined as a string
parameter of type TY_MACRO in the poefile header (and hence affecting only
that one datafile).
.endhelp ---------------------------------------------------------------------

# General definitions.
define	MAX_LEVELS	20		# max include file nesting
define	MAX_ARGS	9		# max arguments to a macro
define	SZ_CMD		80		# `cmd`
define	SZ_IBUF		8192		# buffer for macro replacement
define	SZ_OBUF		8192		# buffer for macro replacement
define	SZ_ARGBUF	256		# argument list to a macro

# The gettok descriptor.
define	LEN_GTDES	45
define	GT_QP		Memi[$1]	# backpointer to QPOE descriptor
define	GT_FD		Memi[$1+1]	# current input stream
define	GT_NEXTCH	Memi[$1+2]	# lookahead character
define	GT_FTEMP	Memi[$1+3]	# file on stream is a temp file
define	GT_LEVEL	Memi[$1+4]	# current nesting level
define	GT_SVFD		Memi[$1+5+$2-1]	# stacked file descriptors
define	GT_SVFTEMP	Memi[$1+25+$2-1]# stacked ftemp flags


# QP_OPENTEXT -- Open the QP_GETTOK descriptor.  The descriptor is initially
# opened on the user supplied string buffer (which is opened as a file and
# which must remain intact while token input is in progress), but include file
# processing etc. may cause arbitrary nesting of file descriptors.

pointer procedure qp_opentext (qp, text)

pointer	qp			#I QPOE descriptor
char	text[ARB]		#I input text to be scanned

pointer	gt
int	sz_pbbuf
int	stropen(), strlen()
errchk	stropen, calloc

begin
	call calloc (gt, LEN_GTDES, TY_STRUCT)

	GT_QP(gt) = qp
	GT_FD(gt) = stropen (text, strlen(text), READ_ONLY)

	if (qp == NULL)
	    sz_pbbuf = DEF_MAXPUSHBACK
	else
	    sz_pbbuf = QP_SZPBBUF(qp)
	call fseti (GT_FD(gt), F_PBBSIZE, sz_pbbuf)

	return (gt)
end


# QP_GETTOK -- Return the next token from the input stream.  The token ID
# (a predefined integer code or the character value) is returned as the
# function value.  The text of the token is returned as an output argument.
# Any macro references, file includes, etc., are performed in the process
# of scanning the input stream, hence only fully resolved tokens are output.

int procedure qp_gettok (gt, tokbuf, maxch)

pointer	gt			#I gettok descriptor
char	tokbuf[maxch]		#O receives the text of the token
int	maxch			#I max chars out

pointer	sp, bp, qp, cmd, ibuf, obuf, argbuf, fname, sym, textp
int	fd, token, level, nargs, nchars, i_fd, o_fd, ftemp

bool	streq()
pointer	qp_gmsym()
int	strmac(), open(), stropen()
int	qp_rawtok(), qp_nexttok(), qp_arglist()
errchk	qp_rawtok,close,ungetci,ungetline,qp_arglist,clcmdw,stropen,syserr
define	pushfile_ 91


begin
	call smark (sp)

	# Allocate some buffer space.
	nchars = SZ_CMD + SZ_IBUF + SZ_OBUF + SZ_ARGBUF + SZ_FNAME + 5
	call salloc (bp, nchars, TY_CHAR)

	cmd = bp
	ibuf = cmd + SZ_CMD + 1
	obuf = ibuf + SZ_IBUF + 1
	argbuf = obuf + SZ_OBUF + 1
	fname = argbuf + SZ_ARGBUF + 1

	qp = GT_QP(gt)

	# Read raw tokens and push back macro or include file text until we
	# get a fully resolved token.

	repeat {
	    fd = GT_FD(gt)

	    # Get a raw token.
	    token = qp_rawtok (gt, tokbuf, maxch)

	    # Process special tokens.
	    switch (token) {
	    case EOF:
		# EOF has been reached on the current stream.
		level = GT_LEVEL(gt)
		if (GT_FTEMP(gt) == YES) {
		    call fstats (fd, F_FILENAME, Memc[fname], SZ_FNAME)
		    if (level > 0)
			call close (fd)
		    iferr (call delete (Memc[fname]))
			call erract (EA_WARN)
		} else if (level > 0)
		    call close (fd)

		if (level > 0) {
		    # Restore previous stream.
		    GT_FD(gt)     = GT_SVFD(gt,level)
		    GT_FTEMP(gt)  = GT_SVFTEMP(gt,level)
		    GT_LEVEL(gt)  = level - 1
		    GT_NEXTCH(gt) = NULL
		} else {
		    # Return EOF token to caller.
		    call strcpy ("EOF", tokbuf, maxch)
		    break
		}

	    case TOK_IDENTIFIER:
		# Lookup the identifier in the symbol table.
		sym = NULL
		if (qp != NULL)
		    sym = qp_gmsym (qp, tokbuf, textp)

		# Process a defined macro.
		if (sym != NULL) {
		    # If macro does not have any arguments, merely push back
		    # the replacement text.

		    if (and (S_FLAGS(sym), SF_MACARGS) == 0) {
			if (GT_NEXTCH(gt) > 0) {
			    call ungetci (fd, GT_NEXTCH(gt))
			    GT_NEXTCH(gt) = 0
			}
			call ungetline (fd, Memc[textp])
			next
		    }

		    # Extract argument list, if any, perform argument
		    # substitution on the macro, and push back the edited
		    # text to be rescanned.

		    if (qp_nexttok(gt) == '(') {
			nargs = qp_arglist (gt, Memc[argbuf], SZ_ARGBUF)

			# Pushback the text of a macro with arg substitution.
			nchars = strmac (Memc[textp], Memc[argbuf],
			    Memc[obuf], SZ_OBUF)
			if (GT_NEXTCH(gt) > 0) {
			    call ungetci (fd, GT_NEXTCH(gt))
			    GT_NEXTCH(gt) = 0
			}
			call ungetline (fd, Memc[obuf])
			next

		    } else {
			call eprintf ("macro `%s' called with no arguments\n")
			    call pargstr (tokbuf)
		    }
		}

		# Check for the builtin symbol $DFN, the datafile name.
		if (tokbuf[1] == '$') {
		    if (streq (tokbuf, "$DFN")) {
			call strcpy (QP_DFNAME(qp), tokbuf, maxch)
			token = TOK_STRING
			break
		    }
		}

		# Return a regular identifier.
		break

	    case TOK_COMMAND:
		# Send a command to the CL and push back the output.

		# Execute the command, spooling the output in a temp file.
		call mktemp ("tmp$co", Memc[fname], SZ_FNAME)
		call sprintf (Memc[cmd], SZ_LINE, "%s > %s")
		    call pargstr (tokbuf)
		    call pargstr (Memc[fname])
		call clcmdw (Memc[cmd])

		# Open the output file as input text.
		call strcpy (Memc[fname], tokbuf, maxch)
		nargs = 0
		ftemp = YES
		goto pushfile_

	    case '@':
		token = qp_rawtok (gt, tokbuf, maxch)
		if (token != TOK_IDENTIFIER && token != TOK_STRING) {
		    call eprintf ("expected a filename after the `@'\n")
		    next
		} else {
		    nargs = 0
		    if (qp_nexttok(gt) == '(')		# )
			nargs = qp_arglist (gt, Memc[argbuf], SZ_ARGBUF)
		    ftemp = NO
		}
pushfile_
		# Attempt to open the file; first try the given name, then
		# if that doesn't work, try adding the macro file extension.

		iferr (i_fd = open (tokbuf, READ_ONLY, TEXT_FILE)) {
		    call qp_mkfname (tokbuf,
			QPOE_MACROEXTN, Memc[fname], SZ_FNAME)
		    iferr (i_fd = open (Memc[fname],READ_ONLY,TEXT_FILE)) {
			call eprintf ("cannot open `%s'\n")
			    call pargstr (tokbuf)
			next
		    }
		}

		if (qp != NULL)
		    call fseti (i_fd, F_PBBSIZE, QP_SZPBBUF(qp))
		else
		    call fseti (i_fd, F_PBBSIZE, DEF_MAXPUSHBACK)

		# Cancel lookahead.
		if (GT_NEXTCH(gt) > 0) {
		    call ungetci (fd, GT_NEXTCH(gt))
		    GT_NEXTCH(gt) = 0
		}

		# If the macro was called with a nonnull argument list,
		# attempt to perform argument substitution on the file
		# contents.  Otherwise merely push the fd.

		if (nargs > 0) {
		    # Pushback file contents with argument substitution.
		    o_fd = stropen (Memc[ibuf], SZ_IBUF, NEW_FILE)

		    call fcopyo (i_fd, o_fd)
		    nchars = strmac (Memc[ibuf],Memc[argbuf],Memc[obuf],SZ_OBUF)
		    call ungetline (fd, Memc[obuf])

		    call close (o_fd)
		    call close (i_fd)

		} else {
		    # Push a new input stream.
		    level = GT_LEVEL(gt) + 1
		    if (level > MAX_LEVELS)
			call syserr (SYS_QPMRECUR)

		    GT_SVFD(gt,level) = GT_FD(gt)
		    GT_SVFTEMP(gt,level) = GT_FTEMP(gt)
		    GT_LEVEL(gt) = level

		    fd = i_fd
		    GT_FD(gt) = fd
		    GT_FTEMP(gt) = ftemp
		}

	    default:
		break
	    }
	}

	if (qp != NULL)
	    if (QP_DEBUG(qp) > 4) {
		call eprintf ("token=%d(%o), `%s'\n")
		    call pargi (token)
		    call pargi (max(0,token))
		    if (IS_PRINT(tokbuf[1]))
			call pargstr (tokbuf)
		    else
			call pargstr ("")
	    }

	call sfree (sp)
	return (token)
end


# QP_UNGETTOK -- Push a token back into the QP_GETTOK input stream, to be
# returned as the next token by QP_GETTOK.

procedure qp_ungettok (gt, tokbuf)

pointer	gt			#I gettok descriptor
char	tokbuf[ARB]		#I text of token

int	fd
pointer	qp
errchk	ungetci

begin
	fd = GT_FD(gt)
	qp = GT_QP(gt)

	if (qp != NULL)
	    if (QP_DEBUG(qp) > 4) {
		call eprintf ("unget token `%s'\n")
		    call pargstr (tokbuf)
	    }

	# Cancel lookahead.
	if (GT_NEXTCH(gt) > 0) {
	    call ungetci (fd, GT_NEXTCH(gt))
	    GT_NEXTCH(gt) = 0
	}

	# First push back a space to ensure that the token is recognized
	# when the input is rescanned.

	call ungetci (fd, ' ')

	# Now push the token text.
	call ungetline (fd, tokbuf)
end
 

# QP_RAWTOK -- Get a raw token from the input stream, without performing any
# macro expansion or file inclusion.  The text of the token in returned in
# tokbuf, and the token type is returened as the function value.

int procedure qp_rawtok (gt, outstr, maxch)

pointer	gt			#I gettok descriptor
char	outstr[maxch]		#O receives text of token.
int	maxch			#I max chars out

int	token, delim, fd, ch, op
define	again_ 91
int	getci()

begin
	fd = GT_FD(gt)
again_
	# Get lookahead char if we don't already have one.
	ch = GT_NEXTCH(gt)
	GT_NEXTCH(gt) = NULL
	if (ch <= 0 || IS_WHITE(ch) || ch == '\n') {
	    while (getci (fd, ch) != EOF)
		if (!(IS_WHITE(ch) || ch == '\n'))
		    break
	}

	# Output the first character.
	op = 1
	if (ch != EOF && ch != '"' && ch != '\'' && ch != '`') {
	    outstr[op] = ch
	    op = op + 1
	}

	# Accumulate token.  Some of the token recognition logic used here
	# (especially for numbers) is crude, but it is not clear that rigour
	# is justified for this application.

	if (ch == EOF) {
	    call strcpy ("EOF", outstr, maxch)
	    token = EOF

	} else if (ch == '#') {
	    # Ignore a comment.
	    while (getci (fd, ch) != '\n')
		if (ch == EOF)
		    break
	    goto again_

	} else if (IS_ALPHA(ch) || ch == '_' || ch == '$' || ch == '.') {
	    # Identifier.
	    token = TOK_IDENTIFIER
	    while (getci (fd, ch) != EOF)
		if (IS_ALNUM(ch) || ch == '_' || ch == '$' || ch == '.') {
		    outstr[op] = ch
		    op = min (maxch, op+1)
		} else
		    break

	} else if (IS_DIGIT(ch)) {
	    # Number.
	    token = TOK_NUMBER

	    # Get number.
	    while (getci (fd, ch) != EOF)
		if (IS_ALNUM(ch) || ch == '.') {
		    outstr[op] = ch
		    op = min (maxch, op+1)
		} else
		    break

	    # Get exponent if any.
	    if (ch == 'E' || ch == 'e') {
		outstr[op] = ch
		op = min (maxch, op+1)
		while (getci (fd, ch) != EOF)
		    if (IS_DIGIT(ch) || ch == '+' || ch == '-') {
			outstr[op] = ch
			op = min (maxch, op+1)
		    } else
			break
	    }

	} else if (ch == '"' || ch == '\'' || ch == '`') {
	    # Quoted string or command.

	    if (ch == '`')
		token = TOK_COMMAND
	    else
		token = TOK_STRING

	    delim = ch
	    while (getci (fd, ch) != EOF)
		if (ch==delim && (op>1 && outstr[op-1] != '\\') || ch == '\n')
		    break
		else {
		    outstr[op] = ch
		    op = min (maxch, op+1)
		}
	    ch = getci (fd, ch)

	} else if (ch == '+') {
	    # May be the += operator.
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = TOK_PLUSEQUALS
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '+'

	} else if (ch == ':') {
	    # May be the := operator.
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = TOK_COLONEQUALS
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = ':'

	} else {
	    # Other characters.
	    token = ch
	    ch = getci (fd, ch)
	}

	# Process the lookahead character.
	if (IS_WHITE(ch) || ch == '\n') {
	    repeat {
		ch = getci (fd, ch)
	    } until (!(IS_WHITE(ch) || ch == '\n'))
	}

	if (ch != EOF)
	    GT_NEXTCH(gt) = ch

	outstr[op] = EOS
	return (token)
end


# QP_NEXTTOK -- Determine the type of the next raw token in the input stream,
# without actually fetching the token.  TOK_PLUSEQUALS is not recognized at
# this level.  Note that this is at the same level as QP_RAWTOK, i.e., no
# macro expansion is performed, and the lookahead token is that which would
# be returned by the next qp_rawtok, which is not necessarily what qp_gettok
# would return after macro replacement.

int procedure qp_nexttok (gt)

pointer	gt			#I gettok descriptor

pointer	qp
int	token, fd, ch
int	getci()

begin
	fd = GT_FD(gt)
	qp = GT_QP(gt)

	# Get lookahead char if we don't already have one.
	ch = GT_NEXTCH(gt)
	if (ch <= 0 || IS_WHITE(ch) || ch == '\n')
	    while (getci (fd, ch) != EOF)
		if (!(IS_WHITE(ch) || ch == '\n'))
		    break

	if (ch == EOF)
	    token = EOF
	else if (IS_ALPHA(ch))
	    token = TOK_IDENTIFIER
	else if (IS_DIGIT(ch))
	    token = TOK_NUMBER
	else if (ch == '"' || ch == '\'') 
	    token = TOK_STRING
	else if (ch == '`')
	    token = TOK_COMMAND
	else
	    token = ch

	if (qp != NULL)
	    if (QP_DEBUG(qp) > 4) {
		call eprintf ("nexttok=%d(%o) `%c'\n")
		    call pargi (token)
		    call pargi (max(0,token))
		    if (IS_PRINT(ch))
			call pargi (ch)
		    else
			call pargi (0)
	    }

	return (token)
end


# QP_CLOSETEXT -- Close the gettok descriptor and any files opened thereon.

procedure qp_closetext (gt)

pointer	gt			#I gettok descriptor

int	level, fd
pointer	sp, fname

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	for (level=GT_LEVEL(gt);  level >= 0;  level=level-1) {
	    fd = GT_FD(gt)
	    if (GT_FTEMP(gt) == YES) {
		call fstats (fd, F_FILENAME, Memc[fname], SZ_FNAME)
		call close (fd)
		iferr (call delete (Memc[fname]))
		    call erract (EA_WARN)
	    } else
		call close (fd)

	    if (level > 0) {
		GT_FD(gt)    = GT_SVFD(gt,level)
		GT_FTEMP(gt) = GT_SVFTEMP(gt,level)
	    }
	}

	call mfree (gt, TY_STRUCT)
	call sfree (sp)
end


# QP_ARGLIST -- Extract a paren and comma delimited argument list to be used
# for substitution into a macro replacement string.  Since the result will be
# pushed back and rescanned, we do not have to perform macro substitution on
# the argument list at this level.

int procedure qp_arglist (gt, argbuf, maxch)

pointer	gt			#I gettok descriptor
char	argbuf[maxch]		#O receives parsed arguments
int	maxch			#I max chars out

int	level, quote, nargs, op, ch, fd
int	getci()

begin
	fd = GT_FD(gt)

	# Get lookahead char if we don't already have one.
	ch = GT_NEXTCH(gt)
	if (ch <= 0 || IS_WHITE(ch) || ch == '\n')
	    while (getci (fd, ch) != EOF)
		if (!(IS_WHITE(ch) || ch == '\n'))
		    break

	quote = 0
	level = 1
	nargs = 0
	op = 1

	if (ch == '(') {
	    while (getci (fd, ch) != EOF) {
		if (ch == '"' || ch == '\'') {
		    if (quote == 0)
			quote = ch
		    else if (quote == ch)
			quote = 0

		} else if (ch == '(' && quote == 0) {
		    level = level + 1
		} else if (ch == ')' && quote == 0) {
		    level = level - 1
		    if (level <= 0) {
			if (op > 1 && argbuf[op-1] != EOS)
			    nargs = nargs + 1
			break
		    }

		} else if (ch == ',' && level == 1 && quote == 0) {
		    ch = EOS
		    nargs = nargs + 1
		} else if (ch == '\n') {
		    ch = ' '
		} else if (ch == '\\' && quote == 0) {
		    ch = getci (fd, ch)
		    next
		} else if (ch == '#' && quote == 0) {
		    while (getci (fd, ch) != EOF)
			if (ch == '\n')
			    break
		    next
		}

		argbuf[op] = ch
		op = min (maxch, op + 1)
	    }

	    GT_NEXTCH(gt) = NULL
	}

	argbuf[op] = EOS
	return (nargs)
end
