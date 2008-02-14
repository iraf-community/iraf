include	<math/nlfit.h>
include	<pkg/inlfit.h>


# ING_ERRORS -- Compute error diagnostic information and print it on the
# screen.

procedure ing_errorsr (in, file, nl, x, y, wts, npts, nvars)

pointer	in		# INLFIT pointer
char	file[ARB]	# Output file name
pointer	nl		# NLFIT pointer
real	x[ARB]		# Ordinates (npts * nvars)
real	y[ARB]		# Abscissas
real	wts[ARB]	# Weights
int	npts		# Number of data points
int	nvars		# Number of variables

bool	isfit
int	i, j, deleted, rejected, nparams, fd
real	chisqr, variance, rms
pointer	sp, fit, wts1, params, errors, rejpts, plist
pointer	name, pvnames, labels

int	open(), nlstati(), inlstrwrd(), in_geti()
pointer	in_getp()
real	in_rmsr(), nlstatr()
errchk	open()

begin
	# Open the output file.
	if (file[1] == EOS)
	    return
	fd = open (file, APPEND, TEXT_FILE)

	# Determine the number of coefficients.
	nparams = nlstati (nl, NLNPARAMS)

	# Allocate memory for parameters, errors, and parameter list.
	call smark (sp)
	call salloc (params, nparams, TY_REAL)
	call salloc (errors, nparams, TY_REAL)
	call salloc (labels, SZ_LINE + 1, TY_CHAR)

	# Allocate memory for the fit and strings.
	call salloc (fit, npts, TY_REAL)
	call salloc (wts1, npts, TY_REAL)
	call salloc (name, SZ_LINE + 1, TY_CHAR)
	call salloc (pvnames, SZ_LINE + 1, TY_CHAR)

	# Get number of rejected points and rejected point list.
	rejected = in_geti (in, INLNREJPTS)
	rejpts   = in_getp (in, INLREJPTS)

	# Count deleted points.
	deleted = 0
	do i = 1, npts {
	    if (wts[i] == real (0.0))
		deleted = deleted + 1
	}

	# Assign a zero weight to rejected points.
	call amovr (wts, Memr[wts1], npts)
	if (rejected > 0) {
	    do i = 1, npts {
		if (Memi[rejpts+i-1] == YES)
		    Memr[wts1+i-1] = real (0.0)
	    }
	}

	# Get the parameter values and errors.
	call nlvectorr (nl, x, Memr[fit], npts, nvars)
	call nlpgetr (nl, Memr[params], nparams)
	call nlerrorsr (nl, y, Memr[fit], Memr[wts1], npts, variance,
	    chisqr, Memr[errors])

	# Compute the RMS.
	rms = in_rmsr (y, Memr[fit], Memr[wts1], npts)

	# Print the error analysis.
	call fprintf (fd, "\nniterations        %d\n")
	    call pargi (nlstati (nl, NLITER))
	call fprintf (fd, "total_points       %d\n")
	    call pargi (npts)
	call fprintf (fd, "rejected           %d\n")
	    call pargi (in_geti (in, INLNREJPTS))
	call fprintf (fd, "deleted            %d\n")
	    call pargi (deleted)
	call fprintf (fd, "standard deviation %10.7g\n")
	    call pargr (sqrt (variance))
	call fprintf (fd, "reduced chi        %10.7g\n")
	    call pargr (sqrt (chisqr))
	call fprintf (fd, "average error      %10.7g\n")
	if (chisqr <= 0)
	    call pargr (real(0.0))
	else
	    call pargr (sqrt (max (variance, real (0.0)) / chisqr))
	call fprintf (fd, "average scatter    %10.7g\n")
	    call pargr (sqrt (nlstatr (nl, NLSCATTER)))
	call fprintf (fd, "RMS                %10.7g\n")
	    call pargr (rms)

	# Print parameter values and errors.
	call in_gstr (in, INLPLABELS, Memc[labels], SZ_LINE)
	call strcpy (Memc[labels], Memc[pvnames], SZ_LINE)
	call fprintf (fd, "\n%-10.10s  %14.14s  %14.14s\n")
	    call pargstr ("parameter")
	    call pargstr ("value")
	    call pargstr ("error")
	plist = in_getp (in, INLPLIST)
	do i = 1, nparams {
	    if (inlstrwrd (i, Memc[name], SZ_LINE, Memc[pvnames]) != 0) {
	        call fprintf (fd, "%-10.10s  ")
		    call pargstr (Memc[name])
	    } else {
	        call fprintf (fd, "%-10.2d  ")
		    call pargi (i)
	    }
	    call fprintf (fd, "%14.7f  %14.7f    (%s)\n")
		call pargr (Memr[params+i-1])
		call pargr (Memr[errors+i-1])
	    isfit = false
	    do j = 1, nparams {
		if (Memi[plist+j-1] == i) {
		    isfit = true
		    break
		}
	    }
	    if (isfit)
		call pargstr ("fit")
	    else
		call pargstr ("constant")
	}
	call fprintf (fd, "\n")

	# Free allocated memory.
	call sfree (sp)
	call close (fd)
end
