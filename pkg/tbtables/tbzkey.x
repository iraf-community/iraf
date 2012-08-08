include "tbtables.h"
include "tbltext.h"

# tbzkey -- append to list of keywords
# This routine takes a line of text containing a keyword definition for
# a text table, allocates memory for a new keyword entry, and adds it to
# the list of keywords.
#
# The input string (str) must begin with "#k " or "#K "; this is not
# checked here.
#
# The parnum argument can either be zero or a specific keyword number.
# parnum = 0 means that the string contains a new keyword which is to be
# appended at the end of the current list of keywords; TB_NPAR will be
# incremented in this case.  If parnum is greater than zero, it is the
# number of a keyword that is to be replaced, so it must be within the
# range of existing keywords; however, parnum = TB_NPAR + 1 is allowed,
# and it just means append a new keyword.
#
# If the keyword buffer is not long enough it will be reallocated.

# Phil Hodge,  7-Jun-1999  Subroutine created.

procedure tbzkey (tp, str, parnum)

pointer tp		# i: pointer to table descriptor
char	str[ARB]	# i: string containing keyword definition
int	parnum		# i: parameter number, or zero to append a new one
#--
int	keynum		# = parnum or TB_NPAR + 1
int	in_len		# length of input string
int	strlen()
errchk	tbtchs

begin
	if (str[1] == EOS)
	    return

	if (parnum > 0) {
	    if (parnum > TB_NPAR(tp) + 1)
		call error (1, "tbzkey:  keyword number is out of range")
	    keynum = parnum		# write a specific one
	} else {
	    keynum = TB_NPAR(tp) + 1	# append a new one
	}

	# Allocate or reallocate the array of keywords, if necessary.
	if (TB_KEYLIST_PTR(tp) == NULL || keynum > TB_MAXPAR(tp))
	    call tbtchs (tp, TB_NPAR(tp) + INCR_N_KEYWORDS, -1, -1, -1)

	# If we're replacing an existing keyword, free the previous memory.
	if (keynum <= TB_NPAR(tp)) {
	    if (TB_KEYWORD(tp,keynum) != NULL)
		call mfree (TB_KEYWORD(tp,keynum), TY_CHAR)
	}

	# Allocate space for a new entry, and copy the input string.
	in_len = strlen (str)
	call malloc (TB_KEYWORD(tp,keynum), in_len, TY_CHAR)
	call strcpy (str, Memc[TB_KEYWORD(tp,keynum)], in_len)

	# chop off newline, if present
	if (str[in_len] == '\n')
	    Memc[TB_KEYWORD(tp,keynum)+in_len-1] = EOS

	if (keynum > TB_NPAR(tp))
	    TB_NPAR(tp) = keynum

	TB_MODIFIED(tp) = true
end
