include <ttyset.h>
include <fset.h>

define	QUERY "[Hit return to continue, n next image, q quit, w quit and  save parameters]"

# APGQVERIFY -- Print a message in the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure apgqverify (task, ap, ch)

char	task[ARB]	# name of the apphot task
pointer	ap		# pointer to apphot structure
int	ch		# character keystroke command

pointer	tty
int	getci(), strmatch()
pointer	ttyodes()

begin
	tty = ttyodes ("terminal")
	call ttyclearln (STDOUT, tty)
	call ttyso (STDOUT, tty, YES)

	call printf (QUERY)
	call flush (STDOUT)
	call fseti (STDIN, F_RAW, YES)
	if (getci (STDIN, ch) == EOF)
	    ;
	call fseti (STDIN, F_RAW, NO)
	call ttyso (STDOUT, tty, NO)
	call ttyclearln (STDOUT, tty)
	call printf ("\n")
	call flush (STDOUT)

	call ttycdes (tty)

	if (ch == 'q') {
	    return (YES)
	} else if (ch == 'w') {
	    if (strmatch ("^center", task) > 0) {
	        call ap_pcpars (ap)
	    } else if (strmatch ("^fitsky", task) > 0) {
		call ap_pspars (ap)
	    } else if (strmatch ("^phot", task) > 0) {
		call ap_ppars (ap)
	    } else if (strmatch ("^wphot", task) > 0) {
		call ap_wpars (ap)
	    } else if (strmatch ("^qphot", task) > 0) {
		call ap_qppars (ap)
	    } else if (strmatch ("^polyphot", task) > 0) {
		call ap_pypars (ap)
	    } else if (strmatch ("^radprof", task) > 0) {
		call ap_rpars (ap)
	    } else if (strmatch ("^fitpsf", task) > 0) {
		call ap_ppfpars (ap)
	    } else if (strmatch ("^daofind", task) > 0) {
		call ap_fdpars (ap)
	    } else if (strmatch ("^polymark", task) > 0) {
		;
	    }
	    return (YES)
	} else if (ch == 'n') {
	    return (YES)
	} else {
	    return (NO)
	}

end
