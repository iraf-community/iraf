include <fset.h>

define	QUERY "[Hit return to continue, q to quit, w to force update of PSF]"

# DP_QVERIFY -- Print a message in the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure dp_qverify (dao, im, psfim, opst, psfgr, psf_new, psf_computed,
	psf_written)

pointer	dao		# pointer to the daophot structure
pointer	im		# the input image descriptor
pointer	psfim		# the output psf image descriptor
int	opst		# the output psf star list file descriptor
int	psfgr		# the output psf group file descriptor
bool	psf_new		# has the psf star list been defined ?
bool	psf_computed	# is the psf fit defined ?
bool	psf_written	# has the psf been saved ?

int	ch
pointer	tty
bool	dp_updatepsf()
int	getci()
pointer	ttyodes()

begin
	# Print status warning message.
	if (psf_new)
	     call printf ("Warning: The PSF star list is undefined.\n")
	else if (! psf_computed)
	    call printf ("Warning: The PSF fit is not current.\n")
	else if (! psf_written)
	    call printf ("Warning: The PSF has not been saved.\n")

	# Open terminal and print query.
	tty = ttyodes ("terminal")
	call ttyclearln (STDOUT, tty)
	call ttyso (STDOUT, tty, YES)
	call printf (QUERY)
	call flush (STDOUT)

	# Get character.
	call fseti (STDIN, F_RAW, YES)
	if (getci (STDIN, ch) == EOF)
	    ;

	# Reset and close terminal.
	call fseti (STDIN, F_RAW, NO)
	call ttyso (STDOUT, tty, NO)
	call ttyclearln (STDOUT, tty)
	call printf ("\n")
	call flush (STDOUT)
	call ttycdes (tty)

	# Return YES for the quit command, otherwise NO.
	if (ch == 'q') {
	    return (YES)
	} else if (ch == 'w') {
	    if (dp_updatepsf (dao, im, psfim, opst, psfgr, psf_new,
	        psf_computed, psf_written)) {
		psf_computed = true
		psf_written = true
	    }
	    return (NO)
	} else {
	    return (NO)
	}
end
