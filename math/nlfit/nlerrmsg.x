include	<math/nlfit.h>

# NLERRMSG -- Convert NLFIT error code into an error message.

procedure nlerrmsg (ier, errmsg, maxch)

int	ier			# NLFIT error code
char	errmsg[maxch]		# output error message 
int	maxch			# maximum number of chars

begin
	switch (ier) {
	case DONE:
	    call strcpy ("Solution converged", errmsg, maxch)
	case SINGULAR:
	    call strcpy ("Singular matrix", errmsg, maxch)
	case NO_DEG_FREEDOM:
	    call strcpy ("Too few points", errmsg, maxch)
	case NOT_DONE:
	    call strcpy ("Solution did not converge", errmsg, maxch)
	default:
	    call strcpy ("Unknown error code", errmsg, maxch)
	}
end
