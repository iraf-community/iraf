# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<ctype.h>
include	<fset.h>
include	"gettok.h"

.help gettok
.nf --------------------------------------------------------------------------
GETTOK -- Lexical input routines.  Used to return tokens from input text,
performing macro expansion and file expansion.  The input text may be either
an open file descriptor or a text string.

	 nchars = gt_expandtext (text, obuf, len_obuf, gsym, gsym_data)

	           gt = gt_open (fd, gsym, gsym_data, pbblen, flags)
	       gt = gt_opentext (text, gsym, gsym_data, pbblen, flags)
		       gt_close (gt)

	     nchars = gt_expand (gt, obuf, len_obuf)
	      token = gt_gettok (gt, tokbuf, maxch)
		    gt_ungettok (gt, tokbuf)
	      token = gt_rawtok (gt, tokbuf, maxch)
	     token = gt_nexttok (gt)

The client get-symbol routine has the following calling sequence, where
"nargs" is an output argument which should be set to the number of macro
arguments, if any.  Normally this routine will call SYMTAB to do the
symbol lookup, but this is not required.  GSYM may be set to NULL if no
macro replacement is desired.

		   textp = gsym (gsym_data, symbol, &nargs)

PBBLEN is the size of the pushback buffer used for macro expansion, and
determines the size of the largest macro replacement string that can be
pushed back.  FLAGS may be used to disable certain types of pushback.
Both PBBLEN and FLAGS may be given as zero if the client is happy with the
builtin defaults.

Access to the package is gained by opening a text string with GT_OPENTEXT.
This returns a descriptor which is passed to GT_GETTOK to read successive
tokens, which may come from the input text string or from any macros,
include files, etc., referenced in the text or in any substituted text.
GT_UNGETTOK pushes a token back into the GT_GETTOK input stream, to be
returned in the next GT_GETTOK call (following macro expansion).  GT_EXPAND
will process the entire input text string, expanding any macro references
therein, returning the fully resolved text in the output buffer.  A more
macroscopic version of this is GT_EXPANDTEXT, which does the opentext,
expand, and close operations internally, using the builtin defaults.

GT_RAWTOK returns the next physical token from an input stream (without
macro expansion), and GT_NEXTTOK returns the type of the next *physical*
token (no macro expansion) without actually fetching it (for look ahead
decision making).

The tokens that can be returned are as follows:

	GT_IDENT		[a-zA-Z][a-zA-Z0-9_]*
	GT_NUMBER		[0-9][0-9a-zA-Z.]*(e|E)?(+|-)?[0-9]*
	GT_STRING		if "abc" or 'abc', the abc
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
in scanned text are checked to see if they are references to predefined
macros, using the client supplied symbol lookup routine.
.endhelp ---------------------------------------------------------------------

# General definitions.
define	MAX_LEVELS	20		# max include file nesting
define	MAX_ARGS	9		# max arguments to a macro
define	SZ_CMD		80		# `cmd`
define	SZ_IBUF		8192		# buffer for macro replacement
define	SZ_OBUF		8192		# buffer for macro replacement
define	SZ_ARGBUF	256		# argument list to a macro
define	SZ_TOKBUF	1024		# token buffer
define	DEF_MAXPUSHBACK	16384		# max pushback, macro replacement
define	INC_TOKBUF	4096		# increment if expanded text fills

# The gettok descriptor.
define	LEN_GTDES	50
define	GT_FD		Memi[$1]	# current input stream
define	GT_UFD		Memi[$1+1]	# user (client) input file
define	GT_FLAGS	Memi[$1+2]	# option flags
define	GT_PBBLEN	Memi[$1+3]	# pushback buffer length
define	GT_DEBUG	Memi[$1+4]	# for debug messages
define	GT_GSYM		Memi[$1+5]	# get symbol routine
define	GT_GSYMDATA	Memi[$1+6]	# client data for above
define	GT_NEXTCH	Memi[$1+7]	# lookahead character
define	GT_FTEMP	Memi[$1+8]	# file on stream is a temp file
define	GT_LEVEL	Memi[$1+9]	# current nesting level
define	GT_SVFD		Memi[$1+10+$2-1]# stacked file descriptors
define	GT_SVFTEMP	Memi[$1+30+$2-1]# stacked ftemp flags

# Set to YES to enable debug messages.
define	DEBUG		NO


# GT_EXPANDTEXT -- Perform macro expansion on a text string returning the
# fully resolved text in the client's output buffer.  The number of chars
# in the output string is returned as the function value.

int procedure gt_expandtext (text, obuf, len_obuf, gsym, gsym_data)

char	text[ARB]		#I input text to be expanded
pointer	obuf			#U output buffer
int	len_obuf		#U size of output buffer
int	gsym			#I epa of client get-symbol routine
int	gsym_data		#I client data for above

pointer	gt
int	nchars
int	gt_expand()
pointer	gt_opentext()
errchk	gt_opentext

begin
	gt = gt_opentext (text, gsym, gsym_data, 0, 0)
	nchars = gt_expand (gt, obuf, len_obuf)
	call gt_close (gt)

	return (nchars)
end


# GT_EXPAND -- Perform macro expansion on a GT text stream returning the
# fully resolved text in the client's output buffer.  The number of chars
# in the output string is returned as the function value.

int procedure gt_expand (gt, obuf, len_obuf)

pointer	gt			#I gettok descriptor
pointer	obuf			#U output buffer
int	len_obuf		#U size of output buffer

int	token, nchars
pointer	sp, tokbuf, op, otop
int	gt_gettok(), strlen(), gstrcpy()
errchk	realloc

begin
	call smark (sp)
	call salloc (tokbuf, SZ_TOKBUF, TY_CHAR)

	# Open input text for macro expanded token input.
	otop = obuf + len_obuf
	op = obuf

	# Copy tokens to the output, inserting a space after every token.
	repeat {
	    token = gt_gettok (gt, Memc[tokbuf], SZ_TOKBUF)
	    if (token != EOF) {
		if (op + strlen(Memc[tokbuf]) + 3 > otop) {
		    nchars = op - obuf
		    len_obuf = len_obuf + INC_TOKBUF
		    call realloc (obuf, len_obuf, TY_CHAR)
		    otop = obuf + len_obuf
		    op = obuf + nchars
		}

		if (token == GT_STRING) {
		    Memc[op] = '"'  
		    op = op + 1
		}
		op = op + gstrcpy (Memc[tokbuf], Memc[op], otop-op)
		if (token == GT_STRING) {
		    Memc[op] = '"'  
		    op = op + 1
		}
		Memc[op] = ' '
		op = op + 1
	    }
	} until (token == EOF)

	# Cancel the trailing blank and add the EOS.
	if (op > 1 && op < otop)
	    op = op - 1
	Memc[op] = EOS

	call sfree (sp)
	return (op - 1)
end


# GT_OPEN -- Open the GETTOK descriptor on a file descriptor.

pointer procedure gt_open (fd, gsym, gsym_data, pbblen, flags)

int	fd			#I input file
int	gsym			#I epa of client get-symbol routine
int	gsym_data		#I client data for above
int	pbblen			#I pushback buffer length
int	flags			#I option flags

pointer	gt
int	sz_pbbuf
errchk	calloc

begin
	call calloc (gt, LEN_GTDES, TY_STRUCT)

	GT_GSYM(gt) = gsym
	GT_GSYMDATA(gt) = gsym_data
	GT_FLAGS(gt) = flags
	GT_DEBUG(gt) = DEBUG

	GT_FD(gt) = fd
	GT_UFD(gt) = fd

	if (pbblen <= 0)
	    sz_pbbuf = DEF_MAXPUSHBACK
	else
	    sz_pbbuf = pbblen
	call fseti (GT_FD(gt), F_PBBSIZE, sz_pbbuf)
	GT_PBBLEN(gt) = sz_pbbuf

	return (gt)
end


# GT_OPENTEXT -- Open the GT_GETTOK descriptor.  The descriptor is initially
# opened on the user supplied string buffer (which is opened as a file and
# which must remain intact while token input is in progress), but include file
# processing etc. may cause arbitrary nesting of file descriptors.

pointer procedure gt_opentext (text, gsym, gsym_data, pbblen, flags)

char	text[ARB]		#I input text to be scanned
int	gsym			#I epa of client get-symbol routine
int	gsym_data		#I client data for above
int	pbblen			#I pushback buffer length
int	flags			#I option flags

pointer	gt
int	sz_pbbuf
int	stropen(), strlen()
errchk	stropen, calloc

begin
	call calloc (gt, LEN_GTDES, TY_STRUCT)

	GT_GSYM(gt) = gsym
	GT_GSYMDATA(gt) = gsym_data
	GT_FLAGS(gt) = flags
	GT_DEBUG(gt) = DEBUG

	GT_FD(gt) = stropen (text, strlen(text), READ_ONLY)
	GT_UFD(gt) = 0

	if (pbblen <= 0)
	    sz_pbbuf = DEF_MAXPUSHBACK
	else
	    sz_pbbuf = pbblen
	call fseti (GT_FD(gt), F_PBBSIZE, sz_pbbuf)
	GT_PBBLEN(gt) = sz_pbbuf

	return (gt)
end


# GT_GETTOK -- Return the next token from the input stream.  The token ID
# (a predefined integer code or the character value) is returned as the
# function value.  The text of the token is returned as an output argument.
# Any macro references, file includes, etc., are performed in the process
# of scanning the input stream, hence only fully resolved tokens are output.

int procedure gt_gettok (gt, tokbuf, maxch)

pointer	gt			#I gettok descriptor
char	tokbuf[maxch]		#O receives the text of the token
int	maxch			#I max chars out

pointer	sp, bp, cmd, ibuf, obuf, argbuf, fname, textp
int	fd, token, level, margs, nargs, nchars, i_fd, o_fd, ftemp

int	strmac(), open(), stropen()
int	gt_rawtok(), gt_nexttok(), gt_arglist(), zfunc3()
errchk	gt_rawtok, close, ungetci, ungetline, gt_arglist, 
errchk	clcmdw, stropen, syserr, zfunc3
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

	# Read raw tokens and push back macro or include file text until we
	# get a fully resolved token.

	repeat {
	    fd = GT_FD(gt)

	    # Get a raw token.
	    token = gt_rawtok (gt, tokbuf, maxch)

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

	    case GT_IDENT:
		# Lookup the identifier in the symbol table.
		textp = NULL
		if (GT_GSYM(gt) != NULL)
		    textp = zfunc3 (GT_GSYM(gt), GT_GSYMDATA(gt), tokbuf, margs)

		# Process a defined macro.
		if (textp != NULL) {
		    # If macro does not have any arguments, merely push back
		    # the replacement text.

		    if (margs == 0) {
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

		    if (gt_nexttok(gt) == '(') {
			nargs = gt_arglist (gt, Memc[argbuf], SZ_ARGBUF)
			if (nargs != margs) {
			    call eprintf ("macro `%s' called with ")
				call pargstr (tokbuf)
			    call eprintf ("wrong number of arguments\n")
			}

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

		# Return a regular identifier.
		break

	    case GT_COMMAND:
		# Send a command to the CL and push back the output.
		if (and (GT_FLAGS(gt), GT_NOCOMMAND) != 0)
		    break

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
		# Pushback the contents of a file.
		if (and (GT_FLAGS(gt), GT_NOFILE) != 0)
		    break

		token = gt_rawtok (gt, tokbuf, maxch)
		if (token != GT_IDENT && token != GT_STRING) {
		    call eprintf ("expected a filename after the `@'\n")
		    next
		} else {
		    nargs = 0
		    if (gt_nexttok(gt) == '(')		# )
			nargs = gt_arglist (gt, Memc[argbuf], SZ_ARGBUF)
		    ftemp = NO
		}
pushfile_
		# Attempt to open the file.
		iferr (i_fd = open (tokbuf, READ_ONLY, TEXT_FILE)) {
		    call eprintf ("cannot open `%s'\n")
			call pargstr (tokbuf)
		    next
		}

		call fseti (i_fd, F_PBBSIZE, GT_PBBLEN(gt))

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
			call syserr (SYS_FPBOVFL)

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

	if (GT_DEBUG(gt) > 0) {
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


# GT_UNGETTOK -- Push a token back into the GT_GETTOK input stream, to be
# returned as the next token by GT_GETTOK.

procedure gt_ungettok (gt, tokbuf)

pointer	gt			#I gettok descriptor
char	tokbuf[ARB]		#I text of token

int	fd
errchk	ungetci

begin
	fd = GT_FD(gt)

	if (GT_DEBUG(gt) > 0) {
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
 

# GT_RAWTOK -- Get a raw token from the input stream, without performing any
# macro expansion or file inclusion.  The text of the token in returned in
# tokbuf, and the token type is returened as the function value.

int procedure gt_rawtok (gt, outstr, maxch)

pointer	gt			#I gettok descriptor
char	outstr[maxch]		#O receives text of token.
int	maxch			#I max chars out

int	token, delim, fd, ch, last_ch, op
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
	    token = GT_IDENT
	    while (getci (fd, ch) != EOF)
		if (IS_ALNUM(ch) || ch == '_' || ch == '$' || ch == '.') {
		    outstr[op] = ch
		    op = min (maxch, op+1)
		} else
		    break

	} else if (IS_DIGIT(ch)) {
	    # Number.
	    token = GT_NUMBER

	    # Get number.
	    while (getci (fd, ch) != EOF)
		if (IS_ALNUM(ch) || ch == '.') {
		    outstr[op] = ch
		    last_ch = ch
		    op = min (maxch, op+1)
		} else
		    break

	    # Get exponent if any.
	    if (last_ch == 'E' || last_ch == 'e') {
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
		token = GT_COMMAND
	    else
		token = GT_STRING

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
		    token = GT_PLUSEQ
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '+'

	} else if (ch == ':') {
	    # May be the := operator.
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = GT_COLONEQ
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = ':'

	} else if (ch == '*') {
	    if (getci (fd, ch) != EOF)
		if (ch == '*') {
		    token = GT_EXPON
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '*'

	} else if (ch == '/') {
	    if (getci (fd, ch) != EOF)
		if (ch == '/') {
		    token = GT_CONCAT
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '/'

	} else if (ch == '?') {
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = GT_SE
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '?'

	} else if (ch == '<') {
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = GT_LE
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '<'

	} else if (ch == '>') {
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = GT_GE
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '>'

	} else if (ch == '=') {
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = GT_EQ
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '='

	} else if (ch == '!') {
	    if (getci (fd, ch) != EOF)
		if (ch == '=') {
		    token = GT_NE
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '!'

	} else if (ch == '&') {
	    if (getci (fd, ch) != EOF)
		if (ch == '&') {
		    token = GT_LAND
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '&'

	} else if (ch == '|') {
	    if (getci (fd, ch) != EOF)
		if (ch == '|') {
		    token = GT_LOR
		    outstr[op] = ch
		    op = op + 1
		    ch = getci (fd, ch)
		} else
		    token = '|'

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


# GT_NEXTTOK -- Determine the type of the next raw token in the input stream,
# without actually fetching the token.  Operators such as GT_EQ etc. are not
# recognized at this level.  Note that this is at the same level as
# GT_RAWTOK, i.e., no macro expansion is performed, and the lookahead token
# is that which would be returned by the next gt_rawtok, which is not
# necessarily what gt_gettok would return after macro replacement.

int procedure gt_nexttok (gt)

pointer	gt			#I gettok descriptor

int	token, fd, ch
int	getci()

begin
	fd = GT_FD(gt)

	# Get lookahead char if we don't already have one.
	ch = GT_NEXTCH(gt)
	if (ch <= 0 || IS_WHITE(ch) || ch == '\n')
	    while (getci (fd, ch) != EOF)
		if (!(IS_WHITE(ch) || ch == '\n'))
		    break

	if (ch == EOF)
	    token = EOF
	else if (IS_ALPHA(ch) || ch == '_' || ch == '$' || ch == '.')
	    token = GT_IDENT
	else if (IS_DIGIT(ch))
	    token = GT_NUMBER
	else if (ch == '"' || ch == '\'') 
	    token = GT_STRING
	else if (ch == '`')
	    token = GT_COMMAND
	else
	    token = ch

	if (GT_DEBUG(gt) > 0) {
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


# GT_CLOSE -- Close the gettok descriptor and any files opened thereon.

procedure gt_close (gt)

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
	    } else if (fd != GT_UFD(gt))
		call close (fd)

	    if (level > 0) {
		GT_FD(gt)    = GT_SVFD(gt,level)
		GT_FTEMP(gt) = GT_SVFTEMP(gt,level)
	    }
	}

	call mfree (gt, TY_STRUCT)
	call sfree (sp)
end


# GT_ARGLIST -- Extract a paren and comma delimited argument list to be used
# for substitution into a macro replacement string.  Since the result will be
# pushed back and rescanned, we do not have to perform macro substitution on
# the argument list at this level.

int procedure gt_arglist (gt, argbuf, maxch)

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
