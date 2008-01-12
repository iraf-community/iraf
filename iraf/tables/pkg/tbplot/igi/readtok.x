include	<lexnum.h>
include	<ctype.h>
include "igi.h"

#  READTOK -- Lexical analyzer.  Returns the token code as the function
#  value.  If the token is an operand (identifier or constant) the operand
#  value is returned in tokvals. 

#  8/20/91 Removed ^Ls. ZGL
#  10/9/91 Changed getc() while{} loops to repeat{}.  ZGL
#  10/15/91 Fixed a bad until() condition causing escapes to crash igi.  ZGL

int procedure readtok (in, tokvals)

int	in		# Input command line
pointer	tokvals		# Token value structure

char	ch, chn
long	lval
double	dval
int	ip
int	nchars, token, junk
pointer	ctoken
int	maxch

int	getc(), lexnum(), gctod(), gctol(), strlen()

begin
	while (getc (in, ch) != EOF)
	    if (!IS_WHITE(ch))
	        break

	if (ch == EOF)
	    return (EOF)

	maxch = SZ_LINE
	call malloc (ctoken, maxch, TY_CHAR)
	Memc[ctoken] = EOS
	nchars = 1

	if (ch == '\'' || ch == '"') {
	    # Return a string constant
	    token = STRING

	    while (getc (in, chn) != EOF) {
		if (chn == '\n' || chn == ch)
		    break
		else if (chn == '&')
		    call getarg (chn, in, tokvals, Memc[ctoken], maxch, nchars)
		else
		    call chdeposit (chn, Memc[ctoken], maxch, nchars)
	    }

	    if (nchars == 1) {
		call eprintf ("Null String ")
		token = ch
	    } else if (chn == ch) {
		nchars = nchars - 1
		Memc[ctoken+nchars] = EOS
	    } else if (chn == '\n') {
		call eprintf ("Missing close quote in string constant ")
		nchars = nchars - 1
		Memc[ctoken+nchars] = EOS

		call ungetc (in, chn)
	    }

	    LOP_TYPE(tokvals) = TY_CHAR
	    call tkalop (tokvals, nchars)
	    call strcpy (Memc[ctoken], LOP_VALC(tokvals), nchars)

	} else if (ch == '#') {
	    # Comment line;  ends with next newline

	    while (getc (in, ch) != EOF)
		if (ch == '\n')
		    break

	    token = ch

	} else if (ch == '^' || ch == '?' || ch == '$') {
	    token = ch

	} else if (ch == '!') {
	    # Escape the command to the cl;  accumulate command to NEWLINE
	    call chdeposit (ch, Memc[ctoken], maxch, nchars)
	    chn = ch

	    repeat {
		junk = getc (in, ch)
		if (ch != '\n') {
		    if (ch == '&')
			call getarg (ch, in, tokvals, Memc[ctoken],
				     maxch, nchars)
		    else
			call chdeposit (ch, Memc[ctoken], maxch, nchars)
		}

	    } until (junk == EOF || ch == '\n')

	    call ungetc (in, ch)

	    call chdeposit (EOS, Memc[ctoken], maxch, nchars)
	    nchars = strlen (Memc[ctoken])
	    call tkalop (tokvals, nchars)
	    call strcpy (Memc[ctoken], LOP_VALC(tokvals), nchars)
	    token = chn

	} else if (IS_PRINT(ch) && !IS_DELIM(ch)) {

	    #  Anything else -- command or argument
	    call ungetc (in, ch)
	    repeat {
		junk = getc (in, ch)
		if (!IS_DELIM(ch)) {

		    # Check for argument in string.
		    if (ch == '&')
			call getarg (ch, in, tokvals, Memc[ctoken],
				     maxch, nchars)
		    else
			call chdeposit (ch, Memc[ctoken], maxch, nchars)
		}

	    } until (junk == EOF || IS_DELIM(ch))
		
	    call ungetc (in, ch)

	    call chdeposit (EOS, Memc[ctoken], maxch, nchars)
	    nchars = strlen (Memc[ctoken])
	    call tkalop (tokvals, nchars)
	    call strcpy (Memc[ctoken], LOP_VALC(tokvals), nchars)

	    ip = 1
	    token = lexnum (Memc[ctoken], ip, nchars)

	    ip = 1
	    switch (token) {
	    case LEX_OCTAL:
		junk = gctol (Memc[ctoken], ip, lval, 8)
		LOP_TYPE(tokvals) = TY_INT
		LOP_VALI(tokvals) = lval
		token = CONSTANT

	    case LEX_DECIMAL:
		junk = gctol (Memc[ctoken], ip, lval, 10)
		LOP_TYPE(tokvals) = TY_INT
		LOP_VALI(tokvals) = lval
		token = CONSTANT

	    case LEX_HEX:
		junk = gctol (Memc[ctoken], ip, lval, 16)
		LOP_TYPE(tokvals) = TY_INT
		LOP_VALI(tokvals) = lval
		token = CONSTANT

	    case LEX_REAL:
		junk = gctod (Memc[ctoken], ip, dval)
		LOP_TYPE(tokvals) = TY_REAL
		if (IS_INDEFD (dval)) {
		    LOP_VALR(tokvals) = INDEFR
		} else {
		    LOP_VALR(tokvals) = dval
		}
		token = CONSTANT

	    default:
		token = IDENTIFIER
	    }

	} else
	    token = ch

	call mfree (ctoken, TY_CHAR)
	return (token)
end


#  TKALOP -- Allocate a buffer to return string token value

procedure tkalop (tokvals, nchars)

pointer	tokvals		# Token value structure
int	nchars

begin
	if (LOP_LEN(tokvals) > 0)
	    call mfree (LOP_VALP(tokvals), TY_CHAR)

	if (nchars > 0) {
	    call malloc (LOP_VALP(tokvals), nchars, TY_CHAR)
	    LOP_LEN(tokvals) = nchars
	}
end
#---------------------------------------------------------------------------
# End of tkalop
#---------------------------------------------------------------------------
procedure getarg (ch, in, tokvals, l, maxch, nchars)

char	ch			# I:  Input character.
int	in			# I:  Input command line.
pointer	tokvals			# I:  Token value structure.
char	l[maxch]		# IO: Line being constructed.
int	maxch			# I:  Maximum length of line.
int	nchars			# IO: Number of valid chars in line.

# Declarations
int	argnum			# Argument number.
int	ctoi()			# Character to integer.
int	getc()			# Read character.
int	ip			# Character pointer.
int	junk			# Exactly that.
int	ns			# # chars in s.
pointer	s			# Temporary s.
pointer	sp			# Stack pointer.
pointer	sym			# Symbol table.

common	/symcom/sym

begin
	call smark (sp)
	call salloc (s, maxch, TY_CHAR)
	ns = 1
	
	call chdeposit (ch, Memc[s], maxch, ns)

	# If escaped, return.
	if (l[max(nchars-1,1)] == '\\') {
	    call chdeposit (ch, l, maxch, nchars)
	    call sfree (sp)
	    return
	}

	# Get and decode rest of argument.
	repeat {
	    junk = getc (in, ch)
	    if (junk != EOF && IS_DIGIT(ch))
		call chdeposit (ch, Memc[s], maxch, ns)
	} until (junk == EOF || !IS_DIGIT(ch))

	call ungetc (in, ch)

	call chdeposit (EOS, Memc[s], maxch, ns)

	# Decode the argument index
	ip = 2
	if (ctoi (Memc[s], ip, argnum) > 0) {
	    SYM_NMACARG(sym) = max (SYM_NMACARG(sym), argnum)
	} else {
	    call eprintf ("Invalid macro argument:  %s ")
	    call pargstr (Memc[s])
	}

	# Put everything in the output string.
	ns = 0
	while (Memc[s+ns] != EOS) {
	    call chdeposit (Memc[s+ns], l, maxch, nchars)
	    ns = ns + 1
	}

	# That's all folks.
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of getarg
#---------------------------------------------------------------------------
