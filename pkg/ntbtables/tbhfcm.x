include <ctype.h>		# defines IS_WHITE
include <tbset.h>

# tbhfcm -- find a comment in a header parameter string
# This locates the comment, if present, in a text string containing a
# header parameter.
# The returned value of index will be zero if no comment was found.
# If a comment was found, par[index] will be the first character of
# the comment.
# The input string should be the complete parameter record; that is,
# it includes the keyword name, data type, value, and optional comment.
#
# Phil Hodge,  6-Mar-1995  Subroutine created.
# Phil Hodge, 12-May-1995  Check for both ' and " as string delimiter.

procedure tbhfcm (par, index)

char	par[ARB]	# i: string containing header parameter
int	index		# o: index of beginning of comment, or zero
#--
pointer sp
pointer word		# scratch for the parameter value
int	ip, nchar, ctowrd()
int	strlen()

begin
	index = 0				# initial value

	if (strlen (par) < START_OF_VALUE)
	    return

	# If a parameter of type text does not begin with a quote,
	# it doesn't have an associated comment.
	if (par[LOCN_DTYPE] == 't') {
	    if (par[START_OF_VALUE] != '"' && par[START_OF_VALUE] != '\'')
		return
	}

	call smark (sp)
	call salloc (word, SZ_PARREC, TY_CHAR)

	# Skip over the value.
	ip = START_OF_VALUE
	nchar = ctowrd (par, ip, Memc[word], SZ_PARREC)

	# Check whether anything follows the value.  Skip whitespace.
	while (IS_WHITE(par[ip]))
	    ip = ip + 1

	# If there is a comment, set index to the first character
	# of the comment.
	if (par[ip] != EOS)
	    index = ip

	call sfree (sp)
end
