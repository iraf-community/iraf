include	<pkg/xtanswer.h>

# SET_INTERACTIVE -- Set the interactive flag.  Query the user if necessary.
#
# This procedure initializes the interactive flag if there is no query.
# If there is a query it is issued by XT_ANSWER.   The four valued
# interactive flag is returned.

procedure set_interactive (query, interactive)

char	query[ARB]		# Query prompt
int	interactive		# Fit overscan interactively?  (returned)

int	interact		# Saves last value of interactive flag
bool	clgetb()

begin
	# If the query is null then initialize from the CL otherwise
	# query the user.  This response is four valued to allow the user
	# to turn off the query when processing multiple images.

	if (query[1] == EOS) {
	    if (clgetb ("interactive"))
	        interact = YES
	    else
	        interact = ALWAYSNO
	} else
	    call xt_answer (query, interact)

	interactive = interact
end
