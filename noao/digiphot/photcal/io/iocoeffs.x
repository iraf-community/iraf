include "../lib/io.h"
include "../lib/fitparams.h"
include	"../lib/parser.h"

# IO_GCOEFFS - Get fit coefficients from a text database file

int procedure io_gcoeffs (fname, sym, stat, chisqr, rms, params, errors,
	nparams)

char	fname[ARB]		# output database name
int	sym			# equation symbol
int	stat			# fit error code (output)
real	chisqr			# reduced chi-squared of the fit
real	rms			# RMS of the fit
real	params[nparams]		# parameter values (output)
real	errors[nparams]		# parameter errors (output)
int	nparams			# number of parameters

#int	i
int	nread, rec
pointer	dt

#bool	clgetb()
int	dtlocate(), dtgeti()
pointer	dtmap(), pr_xgetname()
real	dtgetr()
errchk	dtmap()

begin
	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_gcoeffs.in: (fname=%s) (sym=%d) (npar=%d)\n")
		#call pargstr (fname)
		#call pargi (sym)
		#call pargi (nparams)
	#}

	# Map database.
	dt = dtmap (fname, READ_ONLY)

	# Locate record for the equation.
	iferr (rec = dtlocate (dt, Memc[pr_xgetname (sym)])) {
	    call dtunmap (dt)
	    return (0)
	}

	# Get fit status code, chisqr, and rms.
	iferr (stat   = dtgeti (dt, rec, STATUS))
	    stat = INDEFI
	iferr (chisqr = dtgetr (dt, rec, CHISQR))
	    chisqr = INDEFR
	iferr (rms = dtgetr (dt, rec, RMS))
	    rms = INDEFR

	# Get parameter values and errors.
	iferr (call dtgar (dt, rec, VALUES, params, nparams, nread))
	    nread = 0
	iferr (call dtgar (dt, rec, ERRORS, errors, nparams, nread))
	    nread = 0

	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf (
	        #"io_gcoeffs.out: (stat=%d) (chisqr=%g) (rms=%g) (nread=%s)")
		#call pargi (stat)
		#call pargr (chisqr)
		#call pargr (rms)
		#call pargi (nread)
	    #call eprintf ("\nvalues:")
	    #do i = 1, nread {
		#call eprintf (" (%g)")
		    #call pargr (params[i])
	    #}
	    #call eprintf ("\nerrors:")
	    #do i = 1, nread {
		#call eprintf (" (%g)")
		    #call pargr (errors[i])
	    #}
	    #call eprintf ("\n")
	#}

	# Unmap the database.
	call dtunmap (dt)

	# Return number of values read.
	return (nread)
end


# IO_PCOEFFS - Put fit coefficients in the output file

procedure io_pcoeffs (fname, sym, stat, wtflag, variance, chisqr, scatter,
	rms, params, errors, plist, nparams)

char	fname[ARB]		# output database name
int	sym			# equation symbol
int	stat			# fit error code
int	wtflag			# type of weighting
real	variance		# variance of the fit
real	chisqr			# reduced chi-squared of the fit
real	scatter			# additional scatter squared in the fit
real	rms			# RMS of the fit
real	params[nparams]		# parameter values
real	errors[nparams]		# parameter errors
int	plist[nparams]		# parameter list
int	nparams			# number of parameters

bool	isfit
char	str[SZ_LINE]
int	i, j
pointer	dt
real	rval

#bool	clgetb()
int	pr_gpari()
pointer	pr_xgetname(), pr_gsymc(), pr_gsymp(), pr_gderc(), dtmap(), pr_gderp()
real	pr_gsymr()
errchk	dtmap()

begin
	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_pcoeffs: (fname=%s) (sym=%d) (stat=%d) "
		#call pargstr (fname)
		#call pargi (sym)
		#call pargi (stat)
	    #call eprintf ("(chisqr=%g) (rms=%g) (npar=%d)\n")
		#call pargr (chisqr)
		#call pargr (rms)
		#call pargi (nparams)
	#}

	# Map the database.
	dt = dtmap (fname, APPEND)

	# Put time stamp and record identification.
	call dtptime (dt)
	call dtput (dt, "begin\t%s\n")
	    call pargstr (Memc[pr_xgetname (sym)])

	# Write fit status code and message.
	call nlerrmsg (stat, str, SZ_LINE)
	call dtput (dt, "\t%s\t%d\t(%s)\n")
	    call pargstr (STATUS)
	    call pargi (stat)
	    call pargstr (str)

	# Write the variance and standard deviation.
	call dtput (dt, "\t%s\t%g\n")
	    call pargstr (VARIANCE)
	    call pargr (variance)
	call dtput (dt, "\t%s\t%g\n")
	    call pargstr (STDEV)
	if (variance > 0.0)
	    call pargr (sqrt (variance))
	else
	    call pargr (0.0)

	# Write the average square error and the average error.
	call dtput (dt, "\t%s\t%g\n")
	    call pargstr (AVSQERROR)
	if (chisqr <= 0.0)
	    rval = 0.0
	else
	    rval = variance / chisqr
	    call pargr (rval)
	call dtput (dt, "\t%s\t\t%g\n")
	    call pargstr (AVERROR)
	    call pargr (sqrt (rval))

	# Write out the average square scatter and the average scatter.
	call dtput (dt, "\t%s\t%g\n")
	    call pargstr (AVSQSCATTER)
	    if (scatter <= 0.0)
		rval = 0.0
	    else
		rval = scatter
	    call pargr (rval)
	call dtput (dt, "\t%s\t%g\n")
	    call pargstr (AVSCATTER)
	    call pargr (sqrt (rval))

	# Write reduced chi-squared.
	call dtput (dt, "\t%s\t\t%g\n")
	    call pargstr (CHISQR)
	    call pargr (chisqr)

	# Write RMS.
	call dtput (dt, "\t%s\t\t%g\n")
	    call pargstr (MSQ)
	    call pargr (rms * rms)
	call dtput (dt, "\t%s\t\t%g\n")
	    call pargstr (RMS)
	    call pargr (rms)

	# Write reference equation. 
	call dtput (dt, "\t%s\t%s\n")
	    call pargstr (REFERENCE)
	    call pargstr (Memc[pr_gsymc (sym, PTEQREF)])

	# Write the fitting equation. 
	call dtput (dt, "\t%s\t\t%s\n")
	    call pargstr (FITTING)
	    call pargstr (Memc[pr_gsymc (sym, PTEQFIT)])

	# Write the weighting information.
	call dtput (dt, "\t%s\t\t%s\n")
	switch (wtflag) {
	case FWT_UNIFORM:
	    call pargstr (WEIGHTING)
	    call pargstr ("uniform")
	case FWT_PHOTOMETRIC:
	    call pargstr (WEIGHTING)
	    call pargstr ("photometric")
	case FWT_EQUATIONS:
	    call pargstr (WEIGHTING)
	    if (pr_gsymp (sym, PTEQRPNWEIGHT) == NULL)
	        call pargstr ("uniform")
	    else
	        call pargstr (Memc[pr_gsymc (sym, PTEQWEIGHT)])
	default:
	    call pargstr (WEIGHTING)
	    call pargstr ("uniform")
	}

	# Write the parameter names.
	call dtput (dt, "\t%s\t%d\n")
	    call pargstr (PARAMETERS)
	    call pargi (nparams)
	do i = 1, nparams {
	    call dtput (dt, "\t\t%s\t(%s)\n")
		call pargstr (Memc[pr_xgetname (pr_gpari (sym, i, PTEQPAR))])
	    isfit = false
	    do j = 1, nparams
		if (plist[j] == i) {
		    isfit = true
		    break
		}
	    if (isfit)
		call pargstr ("fit")
	    else
		call pargstr ("constant")
	}

	# Write the derivatives.
	call dtput (dt, "\t%s\t%d\n")
	    call pargstr (DERIVATIVES)
	    call pargi (nparams)
	do i = 1, nparams {
	    if (pr_gderp (sym, i, PTEQRPNDER) != NULL) {
	        call dtput (dt, "\t\t%s\n")
		    call pargstr (Memc[pr_gderc (sym, i, PTEQDER)])
	    } else {
	        call dtput (dt, "\t\t%g\n")
		    call pargr (pr_gsymr (pr_gpari (sym, i, PTEQPAR),
		        PFITDELTA))
	    }
	}

	# Write the parameter values.
	call dtput (dt, "\t%s\t%d\n")
	    call pargstr (VALUES)
	    call pargi (nparams)
	do i = 1, nparams {
	    call dtput (dt, "\t\t%g\n")
		call pargr (params[i])
	}

	# Write the parameter errors.
	call dtput (dt, "\t%s\t%d\n")
	    call pargstr (ERRORS)
	    call pargi (nparams)
	do i = 1, nparams {
	    call dtput (dt, "\t\t%g\n")
		call pargr (errors[i])
	}

	call dtput (dt,"\n")

	# Close the database.
	call dtunmap (dt)
end
