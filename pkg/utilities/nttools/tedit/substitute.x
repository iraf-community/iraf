.help substitute
.nf____________________________________________________________________________

This procedure searches for and replaces text patterns in a string.
The text patterns are passed to the procedure as arguments, so this
procedure can be used to perform a variety of text processing tasks.
The procedure has four arguments: a target pattern string (from), a
replacement pattern string (to), the string to be modified (str), and
a maximum length for this string (maxch). The syntax for the target
and replacement pattern strings largely follows that used in the
substitute command by the Unix text editors `ed' and `ex'. The pattern
consists of a sequence of ordinary characters, which match themselves,
and metacharacters, which match a set of characters. A metacharacter
can be matched as if it were an ordinary character by preceding it
with the escape character, `\'. For example, the escape character
itself is indicated in a pattern by `\\'. The metacharacters which can
be used in the target pattern are:

	beginning of string	^	end of string		$
	white space		#	escape character	\
	ignore case		{	end ignore case		}
	begin character class	[	end character class	]
	not, in char class	^	range, in char class	-
	one character		?	zero or more occurences *
	begin tagged string	\(	end tagged string	\)


A set of characters is indicated in the target string by the character
class construct. For example, punctuation could be indicated by
`[,;.!]'.  A range of characters contiguous in the underlying
character set can be abbreviated by the range construct. For example,
`[a-z]' matches any lower case character. The complement of a
character set is indicated by making `^' the first character in a
class. For example, `[^0-9]' matches any non-digit. Repetition of a
character or character class is indicated by the following it with the
`*' metacharacter. Thus, zero or more occurences of a lower case
character is indicated by `[a-z]*'. The tagged string metacharacters
have no effect on the match, they only serve to identify portions of
the matched string for the replacement pattern. The metacharacters
which are used in the replacement pattern are the following:

	entire string		&	tagged string		\n
	capitalize		\u	upper case		\U
	lower case		\L	end case conversion	\e \E

The ditto metacharacter, `&', indicates that the entire portion of the
string that was matched by the target pattern. The tag metacharacter
indicates that the n-th tagged string.  For example, `\1' indicates
the first tagged string and `\2' the second. The remaining
metacharacters affect the case of the output string. The
capitalization metacharacter only affects the immediately following
metacharacter, but the upper and lower case metacharacters must be
turned off explicitly with `\e' or `\E'. The following are a few
examples of the results that can be obtained with this subroutine:

	from			to			action
	----			--	       		------
	IRAF			SDAS			convert all mentions
							of IRAF to SDAS
	[a-z][A-Za-z]*		\u&			capitalize all words
	"\([^"]*\)"		'\1'			convert double quoted
							strings to single
							quoted strings
	\([^,]*\),\(?*\)	\2,\1			reverse two fields
							separated by commas

.endhelp_______________________________________________________________________

include <ctype.h>

define	DITTO		-1	# substitute matched expression
define	TAG		-2	# substitute tagged part of matched expression
define	CAP		-3	# capitalize next char
define	UCASE		-4	# convert to upper case
define	LCASE		-5	# convert to lower case
define	ENDCASE		-6	# end case conversion

define	CH_ESCAPE	'\\'
define	CH_DITTO	'&'
define	CH_LTAG		'('
define	CH_RTAG		')'
define	CH_INDEX	'%'

#* HISTORY *
#* B.Simon	08-Dec-87	First code
#* B.Simon	05-Jan-93	Modified for substitute command

# SUBSTITUTE -- Substitute characters in second pattern for first pattern

bool procedure substitute (from, to, str, maxch)

char	from[ARB]	# i: Target pattern
char	to[ARB]		# i: Replacement pattern 
char	str[ARB]	# u: String to be modified
int	maxch		# i: Maximum length of string
#--
bool	match
int	maxpat, ic, jc, nc
pointer	sp, pat, sub, temp

int	pat_amatch()

begin
	# Allocate memory for temporary strings

	maxpat = maxch + SZ_LINE

	call smark (sp)
	call salloc (pat, maxpat, TY_CHAR)
	call salloc (sub, maxpat, TY_CHAR)
	call salloc (temp, maxch, TY_CHAR)

	# Encode target and replacement patterns

	call code_pat (from, Memc[pat], maxpat)
	call code_sub (to, Memc[sub], maxpat)

	# Perform an anchored match at each character of the string.
	# If there is a match, substitute the replacement pattern for
	# the target. Otherwise move the character to the output unchanged

	ic = 1
	jc = 1
	match = false

	while (str[ic] != EOS) {
	    nc = pat_amatch (str, ic, Memc[pat])
	    if (nc > 0) {
		match = true
		call make_sub (Memc[pat], Memc[sub], str, ic, ic+nc-1, 
			       Memc[temp], jc, maxch)

	    } else {
		nc = 1
		if (jc <= maxch) {
		    Memc[temp+jc-1] = str[ic]
		    jc = jc + 1
		}
	    }

	    ic = ic + nc
	}

	# Copy from temporary output string back to the original string

	Memc[temp+jc-1] = EOS
	call strcpy (Memc[temp], str, maxch)

	# Return status indicates if there were any matches

	call sfree (sp)
	return (match)

end

# CODE_PAT -- Encode the target pattern

procedure code_pat (from, pat, maxch)

char	from[ARB]	# i: Target string
char	pat[ARB]	# o: Encoded target pattern
int	maxch		# i: Maximum length of pattern
#--
char	ch
int	ic, jc, nc
pointer	sp, temp

int	patmake()

begin
	# Allocate memory for temporary string

	call smark (sp)
	call salloc (temp, maxch, TY_CHAR)

	# Convert target string to a form acceptable to the IRAF pattern 
	# matcher by converting tagged strings to index characters. Also
	# escape any index characters which might already be in the string.

	ic = 1
	jc = 1
	while (from[ic] != EOS) {
	    if (from[ic] == CH_ESCAPE) {
		if (from[ic+1] == CH_LTAG || from[ic+1] == CH_RTAG) {
		    ch = CH_INDEX
		    ic = ic + 1
		} else {
		    ch = from[ic]
		}

	    } else if (from[ic] == CH_INDEX) {
		if (jc <= maxch) {
		    Memc[temp+jc-1] = CH_ESCAPE
		    jc = jc + 1
		}
		ch = from[ic]

	    } else {
		ch = from[ic]
	    }

	    if (jc <= maxch) {
		Memc[temp+jc-1] = ch
		jc = jc + 1
	    }

	    ic = ic + 1
	}

	# Call the IRAF pattern encoder to encode the converted string

	Memc[temp+jc-1] = EOS
	nc = patmake (Memc[temp], pat, maxch)

	call sfree (sp)
end

# CODE_SUB -- Encode the replacement pattern

procedure code_sub (to, sub, maxch)

char	to[ARB]		# i: Replacement string
char	sub[ARB]	# o: Encoded replacement pattern
int	maxch		# i: Maximum length of encoded pattern
#--
char	ch
int	ic, jc

int	cctoc()

begin
	# Convert special characters in replacement pattern to codes
	# Also convert escape sequences to single characters

	ic = 1
	jc = 1

	while (to[ic] != EOS) {
	    if (to[ic] == CH_DITTO) {
		ch = DITTO

	    } else if (to[ic] == CH_ESCAPE) {
		switch (to[ic+1]) {
		case 'u':
		    ch = CAP
		    ic = ic + 1
		case 'U':
		    ch = UCASE
		    ic = ic + 1
		case 'L':
		    ch = LCASE
		    ic = ic + 1
		case 'e', 'E':
		    ch = ENDCASE
		    ic = ic + 1
		default:
		    if (IS_DIGIT(to[ic+1])) {
			if (jc <= maxch) {
			    sub[jc] = TAG
			    jc = jc + 1
			}
			ch = TO_INTEG(to[ic+1])
			ic = ic + 1

		    } else if (cctoc (to, ic, ch) == 1) {
			ch = to[ic]

		    } else {
			ic = ic - 1
		    }
		}

	    } else {
		ch = to[ic]
	    }

	    if (jc <= maxch) {
		sub[jc] = ch
		jc = jc + 1
	    }

	    ic = ic + 1
	}

	sub[jc] = EOS

end

# COPY_SUB -- Move input characters to the output string

procedure copy_sub (str1, first, last, caseflag, str2, len, maxch)

char	str1[ARB]	# i: Input string
int	first		# i: First character to be moved
int	last		# i: Last character to be moved
int	caseflag	# u: Case conversion flag
char	str2[ARB]	# u: Output string
int	len		# u: Length of output string
int	maxch		# i: Maximum length of output string
#--
char	ch
int	ic

begin                   
	do ic = first, last {
	    switch (caseflag) {
	    case ENDCASE:
		ch = str1[ic]
	    case LCASE:
		ch = str1[ic]
		if (IS_UPPER (ch))
	     	    ch = TO_LOWER (ch)
	    case UCASE,CAP:
		ch = str1[ic]
		if (IS_LOWER (ch))
		    ch = TO_UPPER (ch)
	    default:
		ch = str1[ic]
	    }

	    if (len <= maxch) {
		str2[len] = ch
		len = len + 1
	    }

	    if (caseflag == CAP)
		caseflag = ENDCASE
	}
end

# MAKE_SUB Substitute for the chars matched by the target pattern

procedure make_sub (pat, sub, in, first, last, out, oc, maxch)

char	pat[ARB]	# i: Target pattern
char	sub[ARB]	# i: Replacement pattern
char	in[ARB]		# i: Input string
int	first		# i: First character matched in input string
int	last		# i: Last character matched in input string
char	out[ARB]	# u: Output string
int	oc		# u: Last character in output string
int	maxch		# i: Maximum length of output string
#--
int	caseflag, ic, index, ltag, rtag

int	patindex()

begin
	caseflag = ENDCASE
	for (ic = 1; sub[ic] != EOS; ic = ic + 1) {
	    switch (sub[ic]) {
	    case ENDCASE:
		caseflag = ENDCASE
	    case LCASE:
		caseflag = LCASE
	    case UCASE:
		caseflag = UCASE
	    case CAP:
		caseflag = CAP
	    case TAG:
		ic = ic + 1
		index = (sub[ic] - 1) * 2 + 1
		ltag = patindex (pat, index)
		rtag = patindex (pat, index+1) - 1
		call copy_sub (in, ltag, rtag, caseflag, out, oc, maxch)
	    case DITTO:
		call copy_sub (in, first, last, caseflag, out, oc, maxch)
	    default:
		call copy_sub (sub, ic, ic, caseflag, out, oc, maxch)
	    }
	}
end
