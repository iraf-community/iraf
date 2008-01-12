include	"../lib/parser.h"
include	"../lib/prdefs.h"


# PR_ERROR - Issue an error message to the standard output, and take an
# error action acording to the severity code. Error messages can be disabled
# if the error flag is set to NO.

procedure pr_error (msg, severity)

char	msg[ARB]		# error message
int	severity		# severity code

include	"lexer.com"

#bool	clgetb()
int	pr_geti()

begin
	# Debug ?
	#if (clgetb ("debug.parcode")) {
	    #call eprintf ("pr_error (msg=%s) (sev=%d)\n")
		#call pargstr (msg)
		#call pargi (severity)
	#}

	# Test whether to process errors, or not
	if (pr_geti (FLAGERRORS) == NO)
	    return

	# Branch on error severity code
	switch (severity) {
	case PERR_WARNING:
	    call pr_inci (NWARNINGS, 1)
	    call printf ("** Warning near line %d: %s%s\n")
	        call pargi (nlines)
	        call pargstr (line)
	        call pargstr (msg)
	case PERR_SYNTAX:
	    call pr_inci (NERRORS, 1)
	    call printf ("** Error near line %d: %s%s at '%s'\n")
	        call pargi (nlines)
	        call pargstr (line)
	        call pargstr (msg)
		call pargstr (id)
	case PERR_SEMANTIC:
	    call pr_inci (NERRORS, 1)
	    call printf ("** Error near line %d: %s%s\n")
	        call pargi (nlines)
	        call pargstr (line)
	        call pargstr (msg)
	case PERR_POSTPROC:
	    call pr_inci (NERRORS, 1)
	    call printf ("** Error: %s\n")
	        call pargstr (msg)
	}
end
