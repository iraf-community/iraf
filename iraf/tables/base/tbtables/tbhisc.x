include <ctype.h>
include <tbset.h>

# tbhisc -- is the keyword a comment?
# If the input keyword is blank or is "history" or "comment" then this
# procedure returns true.  Actual blanks and tabs are both considered
# to be blank, and the search for non-blank characters ends with EOS
# or with the end of the keyword.

bool procedure tbhisc (keyword)

char	keyword[ARB]		# Name of parameter

char	uckey[SZ_KEYWORD]	# keyword converted to upper case
int	k			# loop index
bool	streq()

begin
	call strcpy (keyword, uckey, SZ_KEYWORD)
	call strupr (uckey)

	if (streq (uckey, "HISTORY"))
	    return (true)
	else if (streq (uckey, "COMMENT"))
	    return (true)
	else {
	    do k = 1, SZ_KEYWORD {
		if (uckey[k] == EOS)
		    return (true)
		else if (!IS_WHITE(uckey[k]))
		    return (false)
	    }
	    return (true)
	}
end
