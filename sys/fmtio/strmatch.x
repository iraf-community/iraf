# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include <pattern.h>

.help strmatch, gstrmatch
.nf ________________________________________________________________________
STRMATCH -- Find the first occurrence of the string A in the string B.
If not found, return zero, else return the index of the first character
following the matched substring.

GSTRMATCH -- More general version of strmatch.  The indices of the
first and last characters matched are returned as arguments.  The function
value is the same as for STRMATCH.

STRMATCH recognizes the metacharacters BOL, EOL, ANY, WHITESPACE, IGNORECASE,
and MATCHCASE (BOL and EOL are special only as the first and last chars
in the pattern).  The null pattern matches any string.  Metacharacters
can be escaped.
.endhelp ___________________________________________________________________


# STRMATCH -- Search string STR for pattern PAT.  Return the index of the
# next character following the matched substring, or 0.

int procedure strmatch (str, pat)

char	pat[ARB], str[ARB]
int	first_char, last_char
int	gstrmatch()

begin
	return (gstrmatch (str, pat, first_char, last_char))
end


# GSTRMATCH -- Generalized string match.  Returns the indices of the first and
# last characters in the matched substring if a match occurs.

int procedure gstrmatch (str, pat, first_char, last_char)

char	pat[ARB], str[ARB]
int	first_char, last_char
bool	ignore_case, bolflag
char	ch, pch
int	i, ip, initial_pp, pp

begin
	ignore_case = false
	bolflag = false
	first_char = 1
	initial_pp = 1

	if (pat[1] == CH_BOL) {			# match at beginning of line?
	    bolflag = true
	    initial_pp = 2
	}
	    
	# Try to match pattern starting at each character offset in string.
	do ip = 1, ARB {
	    if (str[ip] == EOS)
		break
	    i = ip

	    # Compare pattern to string str[ip].
	    for (pp=initial_pp;  pat[pp] != EOS;  pp=pp+1) {
		switch (pat[pp]) {
		case CH_WHITESPACE:
		    while (IS_WHITE (str[i]))
			i = i + 1
		case CH_ANY:
		    if (str[i] != '\n')
			i = i + 1
		case CH_IGNORECASE:
		    ignore_case = true
		case CH_MATCHCASE:
		    ignore_case = false
		
		default:
		    pch = pat[pp]
		    if (pch == CH_ESCAPE && pat[pp+1] != EOS) {
			pp = pp + 1
			pch = pat[pp]
		    } else if (pch == CH_EOL)
		        if (pat[pp+1] == EOS && (str[i]=='\n' || str[i]==EOS)) {
			    first_char = ip
			    last_char = i
			    if (str[i] == EOS)
				last_char = last_char - 1
			    return (last_char + 1)
			}

		    ch = str[i]
		    i = i + 1

		    # Compare ordinary characters.  The comparison is trivial
		    # unless case insensitivity is required.

		    if (ignore_case) {
	                if (IS_UPPER (ch)) {
			    if (IS_UPPER (pch)) {
				if (pch != ch)
				    break
			    } else if (pch != TO_LOWER (ch))
		                    break
	                } else if (IS_LOWER (ch)) {
			    if (IS_LOWER (pch)) {
		                if (pch != ch)
		                    break
			    } else if (pch != TO_UPPER (ch))
				    break
	                } else {
			    if (pch != ch)
				break
			}
		    } else {
			if (pch != ch)
			    break
		    }
	        }
	    }

	    # If the above loop was exited before the end of the pattern
	    # was reached, the pattern did not match.

	    if (pat[pp] == EOS) {
		first_char = ip
		last_char = i-1
		return (i)

	    } else if (bolflag || str[i] == EOS)
		break
	}

	return (0)				# no match
end
