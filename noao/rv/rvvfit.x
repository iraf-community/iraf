include <gset.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"

# RV_VERBOSE_FIT - Write a verbose description of the fit and correlation.

procedure rv_verbose_fit (rv, ofd)

pointer	rv				#I RV struct pointer
int	ofd				#I Output file descriptor

pointer	sp, fname
int	fd
int	open()

errchk	mktemp,	open

begin
	if (ofd != STDOUT) {
	    if (RV_VERBOSE(rv) == OF_TXTONLY || 
		RV_VERBOSE(rv) == OF_NOLOG ||
	 	RV_VERBOSE(rv) == OF_STXTONLY) {
	            return
	    }
	}

	# Allocate some space
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Make a temporary file name and open it up
	if (ofd == STDOUT) {
	    call mktemp ("uparm$tmp", Memc[fname], SZ_PATHNAME)
	    iferr (fd = open (Memc[fname], NEW_FILE, TEXT_FILE))
	        call error (0, "Error opening temp file.")

	    call wrt_fit (rv, fd) 		# Start printing it out
	    call close (fd) 			# Close the file

	    # Page the file
	    call gpagefile (RV_GP(rv), Memc[fname], "")
	    call delete (Memc[fname]) 		# Clean up

	} else if (ofd != NULL) {
	    call wrt_fit (rv, ofd)
	    call fprintf (ofd, "\f")
	}

	call sfree (sp)
end


# RV_MEAN_RESID - Compute the median and sigma of the residuals of the fit.

procedure rv_mean_resid (rv, mean, sigma)

pointer	rv				#I RV struct pointer
real	mean				#O Mean of residuals
real	sigma				#O Sigma of residuals

pointer	sp, resx, resy
real	x, y
int	npts, i

real	model()

begin
	if (RV_FITDONE(rv) == NO) {
	    if (RV_INTERACTIVE(rv) == YES)
	        call rv_errmsg ("Error: No fit yet done to the data.")
	    return
	}
	if (RV_FITFUNC(rv) == SINC) {
	    mean = 0.0
	    sigma = 0.0
	    return
	}
	npts = RV_IEND(rv) - RV_ISTART(rv) + 1

	call smark (sp)
	call salloc (resx, npts, TY_REAL)
	call salloc (resy, npts, TY_REAL)

	# Compute the residuals or ratio of the fit
	x =  WRKPIXX(rv,RV_ISTART(rv))
	do i = 1, npts {
	    Memr[resx+i-1] =  x
            if (IS_DBLSTAR(rv) == NO) {
                switch (RV_FITFUNC(rv)) {
                case GAUSSIAN:
                    call cgauss1d (x, 1, COEFF(rv,1), 4, y)
                case LORENTZIAN:
                    call lorentz (x, 1, COEFF(rv,1), 4, y)
                case PARABOLA:
                    call polyfit (x, 1, COEFF(rv,1), 3, y)
                 }
            } else {
                y = model (x, DBL_COEFFS(rv,1), 3*DBL_NSHIFTS(rv)+2)
                y = DBL_SCALE(rv) * y +
                    (DBL_Y1(rv)+DBL_SLOPE(rv)*(x-DBL_X1(rv)))
            }

	    Memr[resy+i-1] = WRKPIXY(rv,i+RV_ISTART(rv)-1) - y
	    x = x + 1.
	}
	call aavgr (Memr[resy], npts, mean, sigma)
		
	call sfree (sp)
end


# WRT_FIT - Write a verbose description of the fit and correlation

procedure wrt_fit (rv, fd)

pointer	rv				#I RV struct pointer
int	fd				#I Tmp file descriptor

pointer	sp, ffunc, orange, rrange, system_id, title
bool	itob()

include "fitcom.com"

begin
	# Allocate some space
	call smark (sp)
	call salloc (ffunc, SZ_FNAME, TY_CHAR)
	call salloc (orange, SZ_LINE, TY_CHAR)
	call salloc (rrange, SZ_LINE, TY_CHAR)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (title, 2*SZ_LINE, TY_CHAR)

	# Get those string valued parameters
	call rv_make_range_string (RV_OSAMPLE(rv), Memc[orange])
	call rv_make_range_string (RV_RSAMPLE(rv), Memc[rrange])
	if (IS_DBLSTAR(rv) == NO)
	    call nam_fitfunc (rv, Memc[ffunc])
	else
	    call strcpy ("deblend", Memc[ffunc], SZ_FNAME)
	call sysid (Memc[system_id], SZ_LINE)

	call sprintf (Memc[title], 2*SZ_LINE, "\n%14t%s\n\t   %s\n\n")
	        call pargstr (
		    "Description of Fit to CCF Peak and Cross-Correlation")
		call pargstr (Memc[system_id])
	call fprintf (fd, Memc[title])

	# Write out the image stuff
	#call fprintf (fd, "Obj  = `%.24s[%.4d]'%40tstar = `%.24s'\n")
	call fprintf (fd, "Obj  = `%24s[%.4d]'%40tstar = `%.24s'\n")
	    call pargstr (IMAGE(rv))
	    call pargi (RV_APNUM(rv))
	    call pargstr (OBJNAME(rv))
	#call fprintf (fd, "Temp = `%.24s[%.4d]'%40tstar = `%.24s'\n")
	call fprintf (fd, "Temp = `%24s[%.4d]'%40tstar = `%.24s'\n")
	    call pargstr (RIMAGE(rv))
	    call pargi (RV_APNUM(rv))
	    call pargstr (TEMPNAME(rv))
	call fprintf (fd, "Deltav = %.3f Km/sec%40tTempvel = %.3f Km/sec\n\n")
	    call pargr (RV_DELTAV(rv))
	    call pargr (TEMPVEL(rv,RV_TEMPNUM(rv)))

	# Now do the fitting parameters
	call fprintf (fd, "Fit Parameters:\n")
	call fprintf (fd, "%10tFunction = `%s'%46tWidth = %g\n")
	    call pargstr (Memc[ffunc])
	    call pargr (RV_FITWIDTH(rv))
	call fprintf (fd, "%12tHeight = %g%43tMinwidth = %g\n")
	    call pargr (RV_FITHGHT(rv))
	    call pargr (RV_MINWIDTH(rv))
	call fprintf (fd, "%14tPeak = %g%43tMaxwidth = %g\n")
	    call pargb (itob(RV_PEAK(rv)))
	    call pargr (RV_MAXWIDTH(rv))
	call fprintf (fd, "%11tWeights = %b%41tBackground = %g\n")
	    call pargr (RV_WEIGHTS(rv))
	    call pargr (RV_BACKGROUND(rv))
	call fprintf (fd, "%9tWincenter = %g%45tWindow = %d\n")
	    call pargr (RV_WINCENPAR(rv))
	    call pargr (RV_WINPAR(rv))

	# Write out some more fitting information
	call fprintf (fd, "\n\tNumber of points fit = %d\n")
	    call pargi (nfit)
	if (RV_FITFUNC(rv) != SINC && RV_FITFUNC(rv) != CENTER1D) {
	    call fprintf (fd, "\tNumber of iterations = %d\n")
	        call pargi (niter)
	    call fprintf (fd, "\tNumber of coeffs fit = 1 - %d\n")
	        call pargi (nfitpars)
	    call fprintf (fd, "\tChi Squared of fit = %.4g\n")
	        call pargr (chisqr)
	    call fprintf (fd, "\tFit Coefficients:\n")
	    call wrt_coeffs (rv, fd)
	}

	call rv_mean_resid (rv, mresid, sresid)
	call fprintf (fd, "\n\tMean Residual = %f\n\tSigma of Residuals = %f\n")
	    call pargr (mresid)
	    call pargr (sresid)
	call fprintf (fd, "\tMaximum of cross-correlation is in bin = %d.\n")
	    call pargi (binshift)
	call fprintf (fd, "\tVariance of cross-correlation = %g\n")
	    call pargr (ccfvar)
	call fprintf (fd, "\tHJD of observation = %.5f %50tMJD = %.5f\n")
	    if (RV_DCFLAG(rv) == -1) {
	        call pargd (INDEFD)
	        call pargd (INDEFD)
	    } else {
	        call pargd (RV_HJD(rv))
	        call pargd (RV_MJD_OBS(rv))
	    }
	call fprintf (fd, "\tObject sample used in correlation = `%s'\n")
	    call pargstr (Memc[orange])
	call fprintf (fd, "\tTemplate sample used in correlation = `%s'\n")
	    call pargstr (Memc[rrange])
	call fprintf (fd, "\tTonry&Davis R value = %g\n\n")
	    call pargr (RV_R(rv))

	# Now print out some velocity information
	call fprintf (fd, "Velocity Results:\n")
	call wrt_velocity (rv, fd)

	# Lastly print out any error comments
	call fprintf (fd, "\nComments:\n")
	if (RV_ERRCOMMENTS(rv) != NULL) {
	    call fprintf (fd, "%s\n")
	        call pargstr (ERRCOMMENTS(rv))
	}
	call fprintf (fd, "\n")

	# Clean up
	call flush (fd)
	call sfree (sp)
end


# WRT_VELOCITY - Write out the velocity information.

procedure wrt_velocity (rv, fd)

pointer	rv					#I RV struct pointer
pointer	fd					#I Output file descriptor

int	i
double	rv_shift2vel()

begin
	if (IS_DBLSTAR(rv) == NO) {
	    call fprintf (fd, "\tShift of peak = %.4f pixels\n")
	        call pargr (RV_SHIFT(rv))
	    call fprintf (fd, "\tCorrelation height = %.3f\n")
	        call pargr (RV_HEIGHT(rv))
	    call fprintf (fd, "\tFWHM of peak = %g Km/sec\t(=%g pixels)\n\n")
	        if (RV_DCFLAG(rv) == -1 || IS_INDEF(RV_FWHM(rv)))
	            call pargr (INDEF)
	        else
	            call pargr (RV_FWHM(rv)*RV_DELTAV(rv))
	        call pargr (RV_FWHM(rv))
	    call fprintf (fd, "\tVelocity computed from shift = %.4f Km/sec\n")
	        call pargr (RV_VREL(rv))
	    call fprintf (fd, "\tObserved velocity = %.4f Km/sec\n")
	        call pargd (RV_VOBS(rv))
	    call fprintf (fd,"\tHeliocentric velocity = %.4f +/- %.3f Km/sec\n")
	        call pargd (RV_VCOR(rv))
		if (RV_DCFLAG(rv) != -1)
	            call pargd (RV_ERROR(rv))
		else
	            call pargd (INDEFD)
	} else {
	    call fprintf (fd, "\tShift of peak[1] = %.4f pixels\n")
	        call pargr (DBL_SHIFT(rv,1))
	    do i = 2, DBL_NSHIFTS(rv) {
		call fprintf (fd, "%22t[%d] = %.4f pixels\n")
		    call pargi (i)
		    call pargr (DBL_SHIFT(rv,i))
	    }
	    call fprintf (fd, "\tCorrelation height[1] = %.3f\n")
	        call pargr (DBL_HEIGHT(rv,1))
	    do i = 2, DBL_NSHIFTS(rv) {
		call fprintf (fd, "%27t[%d] = %.3f\n")
		    call pargi (i)
		    call pargr (DBL_HEIGHT(rv,i))
	    }
	    call fprintf (fd, "\tFWHM of peak[1] = %f Km/s\n")
	        call pargr (DBL_FWHM(rv,1))
	    do i = 2, DBL_NSHIFTS(rv) {
		call fprintf (fd, "%21t[%d] = %f Km/s\n")
		    call pargi (i)
		    call pargr (DBL_FWHM(rv,i))
	    }

	    call fprintf (fd, 
		"\n\tVelocity computed from shift[1] = %.4f Km/s\n")
	            call pargd (rv_shift2vel(rv,DBL_SHIFT(rv,1)))
	    do i = 2, DBL_NSHIFTS(rv) {
		call fprintf (fd, "%37t[%d] = %.4f Km/s\n")
		    call pargi (i)
	            call pargd (rv_shift2vel(rv,DBL_SHIFT(rv,i)))
	    }
	    call fprintf (fd, "\tObserved velocity[1] = %.4f Km/s\n")
	        call pargr (DBL_VOBS(rv,1))
	    do i = 2, DBL_NSHIFTS(rv) {
		call fprintf (fd, "%26t[%d] = %.4f Km/s\n")
		    call pargi (i)
		    call pargr (DBL_VOBS(rv,i))
	    }
	    call fprintf(fd,"\tHeliocentric velocity[1] = %.4f +/- %.3f Km/s\n")
	        call pargr (DBL_VHELIO(rv,1))
	        call pargr (DBL_VERR(rv,1))
	    do i = 2, DBL_NSHIFTS(rv) {
		call fprintf (fd, "%30t[%d] = %.4f +/- %.3f Km/s\n")
		    call pargi (i)
		    call pargr (DBL_VHELIO(rv,i))
		    call pargr (DBL_VERR(rv,i))
	    }
	}
	call flush (fd)
end


# WRT_COEFFS - Write the fit coefficients and errors

procedure wrt_coeffs (rv, fd)

pointer	rv					#I RV struct pointer
int	fd					#I File descriptor

begin
	if (fd == NULL)
	    return

	if (IS_DBLSTAR(rv) == YES) {
	    call wrt_debl_coeffs (rv, fd)

	} else if (RV_FITFUNC(rv) == GAUSSIAN || RV_FITFUNC(rv) == LORENTZIAN) {
	    call fprintf (fd, "\t\tc[1] = %8.4f  +/- %6.4f%65t# Amplitude\n")
		call pargr (COEFF(rv,1))
		call pargr (ECOEFF(rv,1))
	    call fprintf (fd, "\t\tc[2] = %8.4f  +/- %6.4f%65t# Center\n")
		call pargr (COEFF(rv,2))
		call pargr (ECOEFF(rv,2))
	    if (RV_FITFUNC(rv) == GAUSSIAN)
	        call fprintf (fd, "\t\tc[3] = %8.4f  +/- %6.4f%65t# Sigma^2\n")
	    else
	        call fprintf (fd, "\t\tc[3] = %8.4f  +/- %6.4f%65t# FWHM\n")
		    call pargr (COEFF(rv,3))
		    call pargr (ECOEFF(rv,3))
	    if (IS_INDEF(RV_BACKGROUND(rv))) {
	        call fprintf (fd, 
		    "\t\tc[4] = %8.4f  +/- %6.4f%65t# Background\n")
		        call pargr (COEFF(rv,4))
		        call pargr (ECOEFF(rv,4))
	    } else {
	        call fprintf (fd, 
		    "\t\tc[4] = %8.4f  (fixed)%65t# Background\n")
		        call pargr (RV_BACKGROUND(rv))
	    }

	} else if (RV_FITFUNC(rv) == PARABOLA) {
	    call fprintf (fd, "\t\tc[1] = %8.4f  +/- %6.4f\n")
		call pargr (COEFF(rv,1))
		call pargr (ECOEFF(rv,1))
	    call fprintf (fd, "\t\tc[2] = %8.4f  +/- %6.4f\n")
		call pargr (COEFF(rv,2))
		call pargr (ECOEFF(rv,2))
	    call fprintf (fd, "\t\tc[3] = %8.4f  +/- %6.4f\n")
		call pargr (COEFF(rv,3))
		call pargr (ECOEFF(rv,3))
	}
	call flush (fd)
end


# WRT_DEBL_COEFFS - Write the fit coefficients and errors for a deblended fit.

procedure wrt_debl_coeffs (rv, fd)

pointer	rv					#I RV struct pointer
int	fd					#I File descriptor

int	i

begin
	if (fd == NULL)
	    return

	call fprintf (fd, "\t\tc[1] = %8.4f %45t# First line position\n")
	    call pargr (DBL_COEFFS(rv,1))
	call fprintf (fd, "\t\tc[2] = %8.4f %45t# First line sigma\n")
	    call pargr (DBL_COEFFS(rv,2))
	do i = 1, DBL_NSHIFTS(rv) {
	    call fprintf (fd, "\t\tc[%d] = %8.4f %45t# Line #%d Amplitude\n")
		call pargi (3*i)
	        call pargr (DBL_COEFFS(rv,3*i))
		call pargi (i)
	    call fprintf (fd, 
		"\t\tc[%d] = %8.4f %45t# Line #%d Center (relative)\n")
		    call pargi (3*i+1)
	            call pargr (DBL_COEFFS(rv,3*i+1))
		    call pargi (i)
	    call fprintf (fd, 
		"\t\tc[%d] = %8.4f %45t# Line #%d Sigma (relative)\n")
		    call pargi (3*i+2)
	            call pargr (DBL_COEFFS(rv,3*i+2))
		    call pargi (i)
	}
	call flush (fd)
end
