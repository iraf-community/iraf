include <imhdr.h>
include <ttyset.h>
include <fset.h>

define	QUERY "[Hit return to continue, q to quit, w to force update of PSF]"

# DPGQVERIFY -- Print a message in the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure dpgqverify (dao, im, psfim, psfgr, psf_written, ch)

pointer	dao		# pointer to the daophot structure
pointer	im		# pointer to the input image
pointer	psfim		# pointer to the output PSF
int	psfgr		# psf group file descriptor
bool	psf_written	# is the psf written
int	ch		# character keystroke command

pointer	tty
int	getci(), dp_writepsf()
pointer	ttyodes()

begin
	if (! psf_written)
	    call printf ("Warning: The PSF has not been updated.\n")

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
	    if (dp_writepsf (dao, im, psfim) != ERR) {
		call printf ("Writing PSF %s for image %s.\n")
		    call pargstr (IM_HDRFILE(psfim))
		    call pargstr (IM_HDRFILE(im))
		psf_written = true
	    } else {
		call dp_rmpsf (dao, psfim, psfgr)
		call printf ("The PSF is undefined.\n")
	    }
	    return (NO)
	} else {
	    return (NO)
	}
end
