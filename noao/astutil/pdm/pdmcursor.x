include <mach.h>
include <ctype.h>
include <error.h>
include	<gset.h>
include	<fset.h>
include	<math/curfit.h>
include "pdm.h"

define	PROMPT		"pdm options"

# PDM_CURSOR -- Get the next command from the user in a graphics cursor loop.
# Perform the requested function.

procedure pdm_cursor (pdmp, ptype, infile, flip)

pointer	pdmp			# pointer to PDM data structure
int	ptype			# type of plot on the screen
char	infile[SZ_FNAME]	# input file name
bool	flip			# flip the y-axis scale

real	x, y			# cursor coordinates
real	xmax, xmin
real	period
int	wcs			# wcs to which coordinates belong
int	key			# keystroke value of cursor event
int	stridxs(), gqverify(), ier
int	clgcur(), i
int	pdm_delete(), index
int	pdm_undelete(), pdm_findmin()
real	rx1, rx2, cveval()
real	pdm_signif(), signif
pointer	weights			# pointer to temporary weights array for icfit
pointer	command			# string value, if any
pointer	sp, sptemp
errchk	pdm_colon, icg_addfitr, pdm_fitphase, pdm_alltheta
errchk	pdm_phase, pdm_signif, pdm_ampep, pdm_findmin

begin
	call smark (sp)
	call salloc (command, SZ_LINE, TY_CHAR)

	while (clgcur ("cursor", x, y, wcs, key, Memc[command],
	    SZ_LINE) != EOF) {

	    # Switch on command, take appropriate action.
	    switch (key) {
	    case '?':
	        # List options.
		call gpagefile (PDM_GP(pdmp), HELP, PROMPT)
	    case ':':
		# Colon command.
		call pdm_colon (pdmp, Memc[command], ptype, infile,
		    period, flip)
	    case 'h':
		# Graph the data.
		call pdm_dplot (pdmp, infile, flip)
		ptype = DATAPLOT
	    case 'f':
		# Call icfit on the data.
		if (ptype == DATAPLOT) {
		    # Set the min/max ordinate values for icfit.
		    call alimr (PDM_X(pdmp,1), PDM_NPT(pdmp), xmin, xmax)
		    call ic_putr (PDM_ICD(pdmp), "xmin", xmin)
		    call ic_putr (PDM_ICD(pdmp), "xmax", xmax)

		    # Allocate a temporary weights array and fill it with the
		    # in-use array values.

		    call smark (sptemp)
		    call salloc (weights, PDM_NPT(pdmp), TY_REAL)
		    do i = 1, PDM_NPT(pdmp)
			Memr[weights+i-1] = PDM_INUSE(pdmp,i)

		    # Call icfit.
		    if (PDM_NPT(pdmp) >= 2)
	    		call icg_addfitr (PDM_ICD(pdmp), PDM_GP(pdmp), "cursor",
			    PDM_GT(pdmp), PDM_CVD(pdmp), PDM_X(pdmp,1),
			    PDM_DY(pdmp,1), Memr[weights], PDM_NPT(pdmp),
			    PDM_NPT(pdmp))

		    # Recover the weights array back into the in-use array.
		    do i = 1, PDM_NPT(pdmp)
			 PDM_INUSE(pdmp,i) = Memr[weights+i-1]

		    # Replot.
		    call pdm_dplot (pdmp, infile, flip)
		    call sfree (sptemp)
		} else if (ptype == PHASEPLOT) {
		    # Call icfit on the phases.
		    call pdm_fitphase (pdmp)

		    # Replot.
		    call pdm_pplot (pdmp, period, infile, flip)
		} else
		    call printf ("Can't fit a THETA plot \007\n")
	    case 'i':
		# Theta vs. frequency plot.
		if (PDM_PMIN(pdmp) < EPSILONR && PDM_PMAX(pdmp) < EPSILONR)
		    call pdm_minmaxp(pdmp)

		# Calculate theta.
		call pdm_alltheta (pdmp, THETAFPLOT)

		# Graph theta vs frequency.
		call pdm_tplot (pdmp, THETAFPLOT, infile)
		ptype = THETAFPLOT
	    case 'k':
		# Theta vs. period plot.
		if (PDM_PMIN(pdmp) < EPSILONR && PDM_PMAX(pdmp) < EPSILONR)
		    call pdm_minmaxp(pdmp)

		# Calculate theta.
		call pdm_alltheta (pdmp, THETAPPLOT)

		# Graph theta vs frequency.
		call pdm_tplot (pdmp, THETAPPLOT, infile)
		ptype = THETAPPLOT
	    case 'p':
		# Phase curve plot.
		if (ptype == THETAPPLOT) {
		    # Find the epoch, calculate the phases w.r.t. this epoch.
		    call pdm_ampep (pdmp, x)
		    call pdm_phase(pdmp, x, PDM_EPOCH(pdmp))
		    call pdm_pplot (pdmp, x, infile, flip)
		    ptype = PHASEPLOT
		    period = x
		} else if (ptype == THETAFPLOT) {
		    # Find the epoch, calculate the phases w.r.t. this epoch.
		    call pdm_ampep (pdmp, 1./x)
		    call pdm_phase(pdmp, 1./x, PDM_EPOCH(pdmp))
		    call pdm_pplot (pdmp, 1./x, infile, flip)
		    ptype = PHASEPLOT
		    period = 1./x
		} else
		    call printf ("Wrong type of plot on screen for p key\007\n")
	    case 'd':
		# Delete the point nearest the cursor.
		index = pdm_delete (pdmp, x, y, ptype)
	    case 'u':
		# Undelete the point nearest the cursor.
		index = pdm_undelete (pdmp, x, y, ptype)
	    case 'j':
		# Subtract the fit from the data and use residuals.
		if (PDM_CVD(pdmp) == NULL)
		    call printf ("Fit has not been done. \007\n")
		else if (PDM_RESID(pdmp) == YES)
		    call printf ("Already using residuals. \007\n")
		else {
		    # For each point, calculate the fit function and subtract
		    # it from the data.

		    do i = 1, PDM_NPT(pdmp) {
			PDM_DY(pdmp,i) = PDM_DY(pdmp,i) -
			    cveval (PDM_CVD(pdmp), PDM_X(pdmp,i))
		    }
		    PDM_RESID(pdmp) = YES
		    if (ptype == DATAPLOT)
		        call pdm_dplot (pdmp, infile, flip)
		}
	    case 's':
		# Set sample regions with the cursor.
		if (ptype == DATAPLOT) {
		    if (stridxs ("*", PDM_SAMPLE(pdmp)) > 0)
		        PDM_SAMPLE(pdmp) = EOS

		    rx1 = x
		    call printf ("again:\n")
		    if (clgcur ("cursor", x, y, wcs, key, Memc[command],
			SZ_LINE) == EOF)
		        break
		    rx2 = x

		    call sprintf (Memc[command], SZ_LINE, " %g:%g")
		        call pargr (rx1)
		        call pargr (rx2)

		    call strcat (Memc[command], PDM_SAMPLE(pdmp), SZ_LINE)
		    call rg_gxmarkr (PDM_GP(pdmp), PDM_SAMPLE(pdmp),
			PDM_X(pdmp,1), PDM_NPT(pdmp), 1)
		    call printf (" \n")
		} else
		    call printf ("Wrong type of plot for s key \007\n")
	    case 't':
		# Initialize the sample string and erase from the graph.
		if (ptype == DATAPLOT) {
		    call rg_gxmarkr (PDM_GP(pdmp), PDM_SAMPLE(pdmp),
			PDM_X(pdmp,1), PDM_NPT(pdmp), 0)
		}
		call strcpy ("*", PDM_SAMPLE(pdmp), SZ_LINE)
	    case 'g':
		# Significance of theta at cursor x position.
		if (ptype == THETAPPLOT) {
	    	    # Calculate significance at cursor x position (per).
		        signif = pdm_signif (pdmp, x)
	    	    # Print at bottom of screen.
		    call printf ("Significance at cursor = %g\n")
			call pargr (signif)
		} else if (ptype == THETAFPLOT) {
	    	    # Calculate significance at cursor x position (per).
		        signif = pdm_signif (pdmp, 1./x)
		    # Print at bottom of screen.
		    call printf ("Significance at cursor = %g\n")
			call pargr (signif)
		} else if (ptype == PHASEPLOT) {
		    # Calculate significance at current period/frequency
		        signif = pdm_signif (pdmp, period)
		    # Print at bottom of screen
		    call printf ("Significance of this period = %g\n")
			call pargr (signif)
		} else {  
		    # Data plot.
		    call printf ("Wrong type of plot for g key \007\n")
		}
	    case 'a':
		# Amplitude and epoch at cursor x position.
		if (ptype == THETAPPLOT) {
		    # Calculate ampl & epoch at cursor x position.
		    call pdm_ampep (pdmp, x)
		    # Print at bottom of screen.
		    call printf ("amplitude of data = %g, epoch = %g\n")
			call pargr (PDM_AMPL(pdmp))
			call pargr (PDM_EPOCH(pdmp))
		} else if (ptype == THETAFPLOT) {
		    # Calculate ampl & epoch at cursor x position.
		    call pdm_ampep (pdmp, 1./x)
		    # Print at bottom of screen.
		    call printf ("amplitude of data = %g, epoch = %g\n")
			call pargr (PDM_AMPL(pdmp))
			call pargr (PDM_EPOCH(pdmp))
		} else if (ptype == PHASEPLOT) {
		    # Calculate ampl & epoch at current period/frequency
		    call pdm_ampep (pdmp, period)
		    # Print at bottom of screen.
		    call printf ("amplitude of data = %g, epoch = %g\n")
			call pargr (PDM_AMPL(pdmp))
			call pargr (PDM_EPOCH(pdmp))
		} else {
		    # Data plot.
		    call printf ("Wrong type of plot for a key \007\n")
		}
	    case 'm':
		# Mark range and find minimum in this range.
		if (ptype == THETAFPLOT || ptype == THETAPPLOT) {
		    rx1 = x
		    call printf ("again:\n")
		    if (clgcur ("cursor", x, y, wcs, key, Memc[command],
			SZ_LINE) == EOF)
		        break
		    rx2 = x
		    index = pdm_findmin(pdmp, ptype, rx1, rx2, 1,
			PDM_NTHPT(pdmp))
	    	    PDM_MINR(pdmp) = PDM_XTH(pdmp,index)
		    call printf ("period at minimum = %g, frequency = %g\n")
			call pargr (PDM_XTH(pdmp,index))
			call pargr (1./PDM_XTH(pdmp,index))

		} else
		    call printf ("Wrong type of plot for m key. \007\n")
	    case 'r':
		# Check type of plot and replot.
		if (ptype == DATAPLOT)
		    call pdm_dplot (pdmp, infile, flip)
		if (ptype == THETAPPLOT)
		    call pdm_tplot (pdmp, THETAPPLOT, infile)
		if (ptype == THETAFPLOT)
		    call pdm_tplot (pdmp, THETAFPLOT, infile)
		if (ptype == PHASEPLOT)
		    call pdm_pplot (pdmp, period, infile, flip)
	    case 'x':
		# Remove a trend from the data by fitting a straight line to
		# the data and removing it.

		# Set the min/max ordinate values for icfit.
		call alimr (PDM_X(pdmp,1), PDM_NPT(pdmp), xmin, xmax)
		call cvinit (PDM_CVD(pdmp), SPLINE1, 1, xmin, xmax)

		# Allocate a weights array and fill it.
		call smark (sptemp)
		call salloc (weights, PDM_NPT(pdmp), TY_REAL)
		do i = 1, PDM_NPT(pdmp)
		    Memr[weights+i-1] = PDM_INUSE(pdmp,i)

		call cvfit (PDM_CVD(pdmp), PDM_X(pdmp,1), PDM_DY(pdmp,1),
		    Memr[weights], PDM_NPT(pdmp), WTS_USER, ier)
		if (ier != 0) {
		    call eprintf ("error in cvfit\n")
		    call erract (EA_WARN)
		}

		# Subtract the fit from the data.
		if (PDM_RESID(pdmp) == YES) {
		    call printf ("Already using residuals. \007\n")
		    next
		} else {
		    # For each point, calculate the fit function and subtract
		    # it from the data.

		    do i = 1, PDM_NPT(pdmp) {
			PDM_DY(pdmp,i) = PDM_DY(pdmp,i) -
			    cveval (PDM_CVD(pdmp), PDM_X(pdmp,i))
		    }
		    PDM_RESID(pdmp) = YES
		}

		call cvfree (PDM_CVD(pdmp))
		call sfree (sptemp)

		# Replot.
		call pdm_dplot (pdmp, infile, flip)
		ptype = DATAPLOT
	    case 'z':
		# Flip the y-axis scale.
		flip = !flip
	    case 'q':
		# Quit. Exit PDM.
		if (gqverify() == YES)
		    break
	    default:
		# Error: unknown command: ring bell.
		call printf ("\007\n")
		call printf ("? for help or (h,f,i,k,p")
		call printf (",d,u,j,s,t,g,a,m,r,x,q,:)\n")
	    }
	}

	call flush (STDOUT)
	call sfree (sp)
end
