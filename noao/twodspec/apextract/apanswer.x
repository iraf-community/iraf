include	<pkg/xtanswer.h>

# AP_ANSWER -- Map boolean CL parameters to four valued parameters.
# If the CL parameter is NO then we want no queries and the action to
# be ALWAYSNO.  If the task is not interactive then we want no queries
# and the action to be either ALWAYSNO or ALWAYSYES depending on the
# value of the answer.

int procedure ap_answer (prompt, interactive)

char	prompt[ARB]	# CL prompt
int	interactive	# Interactive switch

int	answer, btoi()
bool	clgetb()

begin
	answer = btoi (clgetb (prompt))
	if (answer == NO)
	    answer = ALWAYSNO
	else if (interactive == NO)
	    answer = ALWAYSYES

	return (answer)
end
