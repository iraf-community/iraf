include <time.h>
include <mach.h>
include <fset.h>
include "../lib/apfile.h"


# PH_AGROW - Compute the curve of growth using the data from the input
# photometry files, optional airmasses from the airmass file, and the
# initial values of the parameters.

procedure ph_agrow (apcat, magfd, logfd, mgd, imtable, id, x, y, nap, rap,
	mag, merr, naperts, params, lterms, smallap, largeap)

int	apcat			# the aperture correction file descriptor
int	magfd			# the output magnitude file descriptor
int	logfd			# the log file descriptor
pointer	mgd			# pointer to the plot metacode file
pointer	imtable			# pointer to the image symbol table
int	id[ARB]			# the star ids
real	x[ARB]			# the star x coordinates
real	y[ARB]			# the star y coordinates
int	nap[ARB]		# the array of aperture numbers
real	rap[naperts,ARB]	# input array of aperture radii
real	mag[naperts,ARB]	# input array of magnitudes / differences
real	merr[naperts,ARB]	# input array of magnitude errors
int	naperts			# the number of input apertures
double	params[ARB]		# the initial values of the parameters
int	lterms			# the number of terms to be fit
int	smallap			# the small aperture number
int	largeap			# the large aperture number

int	nimages
pointer	agr
int	stnsymbols()
int	ph_amfit()

begin
	if (logfd != NULL)
	    call ph_logtitle (logfd, apcat)

	nimages = stnsymbols (imtable, 0)
	if (nimages <= 0) {
	    call printf ("Error: There is no input data to fit\n")
	    if (logfd != NULL)
	        call fprintf (logfd, "There is no input data to fit\n")
	    return
	}

	# Allocate memory required for fitting.
	call ph_ameminit (agr, lterms, naperts)

	# Fit the seeing radii and the cog model parameters.
	if (ph_amfit (imtable, agr, nap, rap, mag, merr, naperts, params,
	    lterms) == ERR) {
	    call printf ("Error: The cog model fit did not converge\n")
	    if (logfd != NULL) {
		call ph_ishow (logfd, params, lterms)
	        call ph_pshow (logfd, agr, lterms)
	        call ph_rshow (logfd, imtable)
	        call fprintf (logfd,
		    "Error: The cog model fit did not converge\n")
	    }
	} else {
	    if (logfd != NULL) {
		call ph_ishow (logfd, params, lterms)
	        call ph_pshow (logfd, agr, lterms)
	        call ph_rshow (logfd, imtable)
	    }
	    call ph_aeval (imtable, agr, apcat, magfd, logfd, mgd, id, x, y,
	        nap, rap, mag, merr, naperts, smallap, largeap)
	    if (logfd != NULL)
		call ph_tfshow (logfd, agr, naperts)
	}

	# Free memory required for fitting.
	call ph_amemfree (agr)
end


# PH_LOGTITLE -- Print the title for the new log file entry

procedure ph_logtitle (logfd, apcat)

int	logfd			# the log file descriptor
int	apcat			# the aperture correction file descriptor

pointer	sp, afname, date
long	clktime()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (afname, SZ_FNAME, TY_CHAR)
	call salloc (date, SZ_TIME, TY_CHAR)

	# Get the file name and the times.
	call fstats (apcat, F_FILENAME, Memc[afname], SZ_FNAME)
	call cnvtime (clktime (long(0)), Memc[date], SZ_TIME)

	call fprintf (logfd, "NEW LOGFILE ENTRY AT: %s\n")
	    call pargstr (Memc[date])
	call fprintf (logfd, "APFILE: %s\n\n")
	    call pargstr (Memc[afname])

	call sfree (sp)
end


# PH_AMEMINIT -- Allocate memory for doing the fit.

procedure ph_ameminit (agr, lterms, naperts)

pointer	agr			# pointer to the fitting structure
int	lterms			# the number of terms to fit
int	naperts			# the number of apertures

begin
	call malloc (agr, LEN_AGRSTRUCT, TY_STRUCT)

	call malloc (AGR_DM(agr), naperts, TY_DOUBLE)
	call malloc (AGR_DDDR(agr), naperts, TY_DOUBLE)
	call malloc (AGR_T(agr), lterms * naperts, TY_DOUBLE)
	call malloc (AGR_U(agr), lterms * lterms, TY_DOUBLE)
	call malloc (AGR_V(agr), lterms, TY_DOUBLE)
	call malloc (AGR_POLD(agr), lterms, TY_DOUBLE)
	call malloc (AGR_DP(agr), lterms, TY_DOUBLE)
	call malloc (AGR_PARAMS(agr), MAX_MTERMS, TY_DOUBLE)
	call malloc (AGR_PERRORS(agr), MAX_MTERMS, TY_DOUBLE)
	call malloc (AGR_PCLAMPS(agr), MAX_MTERMS, TY_DOUBLE)

	call malloc (AGR_RBAR(agr), naperts, TY_REAL)
	call malloc (AGR_W(agr), naperts, TY_REAL)
	call malloc (AGR_THEO(agr), naperts, TY_REAL)
	call malloc (AGR_ADOPT(agr), naperts, TY_REAL)
	call malloc (AGR_WOBS(agr), naperts, TY_REAL)
	call malloc (AGR_OBS(agr), naperts, TY_REAL)
	call malloc (AGR_WADO(agr), naperts, TY_REAL)

	call malloc (AGR_CUM(agr), naperts + 1, TY_REAL)
	call malloc (AGR_TCUM(agr), naperts + 1, TY_REAL)
	call malloc (AGR_WCUM(agr), naperts + 1, TY_REAL)
	call malloc (AGR_MAGS(agr), naperts, TY_REAL)
	call malloc (AGR_CMAGS(agr), naperts, TY_REAL)
	call malloc (AGR_TMAGS(agr), naperts, TY_REAL)
	call malloc (AGR_WMAGS(agr), naperts, TY_REAL)

	call calloc (AGR_AVE(agr), naperts, TY_REAL)
	call calloc (AGR_RESID(agr), naperts, TY_REAL)
	call calloc (AGR_RESSQ(agr), naperts, TY_REAL)
	call calloc (AGR_WR(agr), naperts, TY_REAL)
	call calloc (AGR_TAVE(agr), naperts, TY_REAL)
	call calloc (AGR_TRESID(agr), naperts, TY_REAL)
	call calloc (AGR_TRESSQ(agr), naperts, TY_REAL)
	call calloc (AGR_TWR(agr), naperts, TY_REAL)
end


# PH_AMFIT -- Fit the seeing disk widths and the model parameters.

int procedure ph_amfit (imtable, agr, nap, rap, mag, merr, naperts, params,
	lterms)

pointer	imtable			# pointer to the image symbol table
pointer	agr			# pointer to the fitting structure
int	nap[ARB]		# the array of aperture numbers
real	rap[naperts,ARB]	# input array of aperture radii
real	mag[naperts,ARB]	# input array of magnitudes / differences
real	merr[naperts,ARB]	# input array of magnitude errors
int	naperts			# the number of input apertures
double	params[ARB]		# the parameters to be fit
int	lterms			# number of terms to fit

double	pold, sumd, sumw, sumn, sumr, sumy, old
int	niter, i, j, k, nimages, ipow, nterms, iflag, ntimes, ngood
pointer	sp, sym, symbol
real	gain, rold, rclamp, dr
int	stnsymbols()
pointer	sthead(), stnext()

begin
	nimages = stnsymbols (imtable, 0)
	if (nimages <= 0)
	    return (ERR)

	# Allocate temporary space
	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)

	# Order the symbol table.
	symbol = sthead (imtable)
	do k = nimages, 1, -1 {
	    Memi[sym+k-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	# Initialize some variables
	ipow = 1
	nterms = 0
	gain = 0.0
	pold = 0.0d0
	call aclrd (Memd[AGR_POLD(agr)], lterms)
	call ph_apinit (params, Memd[AGR_PARAMS(agr)], Memd[AGR_PERRORS(agr)],
	    Memd[AGR_PCLAMPS(agr)])

	# Compute an improved value for ro and for the stellar profile
	# model parameters.

	for (niter = 1; niter <= AGR_ITMAX; niter = niter + 1) {

	    sumd = 0.0d0
	    sumw = 0.0d0
	    sumn = 0.0d0

	    # Compute the number of terms to be fit this iteration.
	    call ph_anterms (niter, lterms, gain, nterms)

	    # Zero the accumulation matrix and vector
	    call aclrd (Memd[AGR_U(agr)], lterms * lterms)
	    call aclrd (Memd[AGR_V(agr)], lterms)

	    do k = 1, nimages {

		# Get the current symbol.
		symbol = Memi[sym+k-1]

		# Check to see if there is any data.
		if (IMT_NENTRIES(symbol) <= 0) {
		    IMT_RO(symbol) = INDEFR
		    next
		}

		# Check to see if there is any good data.
		ngood = 0
		do i = 1, IMT_NENTRIES(symbol) {
		    do j = 2, nap[i]
			ngood = ngood + 1
		}
		if (ngood <= 0) {
		    IMT_RO(symbol) = INDEFR
		    next
		}

		# Get better estimage of ro.
		if (niter == 1) {
		    if (k > 1 && ! IS_INDEFR(IMT_RO(Memi[sym+k-2]))) {
		        IMT_RO(symbol) = IMT_RO(Memi[sym+k-2])
		    } else
			IMT_RO(symbol) = 0.5 * rap[1,1]
		}

		# Set the model derivative vector
		call ph_adddr (rap[1,IMT_OFFSET(symbol)], Memd[AGR_DDDR(agr)],
		    naperts, Memd[AGR_PARAMS(agr)], IMT_RO(symbol),
		    IMT_NXAIRMASS(symbol))

		# Get the new estimate of ro
		rold = 0.0
		rclamp = IMT_RO(symbol) / 4.0
		ntimes = 0
		repeat {
		    
		    sumr = 0.0d0
		    sumy = 0.0d0

		    call ph_adddrdm (rap[1,IMT_OFFSET(symbol)], 
		        Memd[AGR_DM(agr)], Memd[AGR_DDDR(agr)], naperts,
			Memd[AGR_PARAMS(agr)], IMT_RO(symbol),
			IMT_NXAIRMASS(symbol), niter)

		    call ph_awsum (nap[IMT_OFFSET(symbol)],
		        mag[1,IMT_OFFSET(symbol)],
		        merr[1,IMT_OFFSET(symbol)], Memd[AGR_DM(agr)],
			Memr[AGR_W(agr)], Memd[AGR_DDDR(agr)], naperts,
			IMT_NENTRIES(symbol), ipow, sumr, sumy)

		    if (sumy > 0.0d0) {
			dr = sumr / sumy
			if (dr * rold < 0.0)
			    rclamp = 0.5 * rclamp
			IMT_RO(symbol) = IMT_RO(symbol) - dr / (1.0 + abs (dr /
			    rclamp))
			if (IMT_RO(symbol) <= EPSILONR) {
			    IMT_RO(symbol) = INDEFR
			    call sfree (sp)
			    return (ERR)
			}
			rold = dr
			if (abs (dr / IMT_RO(symbol)) <= 3.0e-4)
			    break
		    } else {
			IMT_RO(symbol) = INDEFR
		        break
		    }

		    ntimes = ntimes + 1
		    if (ntimes >= 100) {
			IMT_RO(symbol) = INDEFR
			call sfree (sp)
			return (ERR)
		    }
		}

		call ph_adp (rap[1,IMT_OFFSET(symbol)], Memd[AGR_DM(agr)],
		    Memd[AGR_T(agr)], naperts, lterms, nterms,
		    Memd[AGR_PARAMS(agr)], IMT_RO(symbol),
		    IMT_NXAIRMASS(symbol))

		call ph_aaccum (nap[IMT_OFFSET(symbol)],
		    mag[1,IMT_OFFSET(symbol)], merr[1,IMT_OFFSET(symbol)],
		    Memd[AGR_DM(agr)], Memr[AGR_W(agr)], naperts,
		    IMT_NENTRIES(symbol), Memd[AGR_T(agr)], Memd[AGR_U(agr)],
		    Memd[AGR_V(agr)], lterms, nterms, ipow, sumd, sumw, sumn)
	    }

	    # Invert the matrix.
	    call dinver (Memd[AGR_U(agr)], lterms, nterms, iflag)
	    if (iflag != 0) {
		call sfree (sp)
		return (ERR)
	    }

	    # Get new values for the parameters.
	    call dmvmul (Memd[AGR_U(agr)], lterms, nterms, Memd[AGR_V(agr)],
		Memd[AGR_DP(agr)])
	    call ph_apsolve (Memd[AGR_POLD(agr)], Memd[AGR_PARAMS(agr)],
	        Memd[AGR_DP(agr)], Memd[AGR_PCLAMPS(agr)], nterms, ipow)

	    # Test the fit.
	    #if (sumn > 6.0)
	    if (sumn > (nterms + 1.0))
	        #sumn = sqrt (sumd / (sumn - 6.0))
	        sumn = sqrt (sumd / (sumn - (nterms + 1.0)))
	    else
		sumn = 0.0
	    sumd = sqrt (sumd / sumw)
	    gain = 2.0 * abs (old - sumd) / (old + sumd)
	    old = sumd
	    if ((nterms < lterms) || (gain > 0.001)) {
		next
	    } else if (ipow <= 2) {
		ipow = 3
		call amovkd (0.1d0, Memd[AGR_PCLAMPS(agr)], 4)
		next
	    } else
		break
	}

	if (niter > AGR_ITMAX) {
	    call sfree (sp)
	    return (ERR)
	}

	# Compute the errors.
	do k = 1, nterms {
	    if (Memd[AGR_U(agr)+(k-1)*lterms+k-1] > 0.0)
	        Memd[AGR_PERRORS(agr)+k-1] =
	            sumn * sqrt (Memd[AGR_U(agr)+(k-1)*lterms+k-1])
	    else
	        Memd[AGR_PERRORS(agr)+k-1] = 0.0d0
	}


	call sfree (sp)
	return (OK)
end


# PH_ISHOW -- Show the initial values of the parameters.

procedure ph_ishow (fd, params, mterms)

int	fd			# pointer to the output file descriptor
double	params[ARB]		# the parameter values
int	mterms			# the number of terms to be fit

int	i

begin
	# Print out the best fit parameters.
	call fprintf (fd,
	    "\nThe initial cog model parameter values for nparams %d\n")
	    call pargi (mterms)
	do i = 1, MAX_MTERMS {
	    call fprintf (fd, "\t%10s: %15.7g\n")
		switch (i) {
		case 1:
		    call pargstr ("swings")
		case 2:
		    call pargstr ("pwings")
		case 3:
		    call pargstr ("pgauss")
		case 4:
		    call pargstr ("rgescale")
		case 5:
		    call pargstr ("xwings")
		}
		call pargd (params[i])
	}
	call fprintf (fd, "\n")
end


# PH_PSHOW -- Show the results of the parameter fitting.

procedure ph_pshow (fd, agr, mterms)

int	fd			# the output file descriptor
pointer	agr			# the pointer to the fitting descriptor
int	mterms			# the number of parameters to fit

begin
	call ph_pwrite (fd, Memd[AGR_PARAMS(agr)], Memd[AGR_PERRORS(agr)],
	    MAX_MTERMS, mterms)
end


# PH_RSHOW -- Show the computed seeing parameters as a function of image.

procedure ph_rshow (fd, imtable)

int	fd			# the output file descriptor
pointer	imtable			# pointer to the symbol table

int	i, nimages
pointer	sp, sym, symbol
int	stnsymbols()
pointer	sthead(), stnext()

begin
	nimages = stnsymbols (imtable, 0)
	if (nimages <= 0)
	    return

	# Allocate temporary space
	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)

	# Order the symbol table.
	symbol = sthead (imtable)
	do i = nimages, 1, -1 {
	    Memi[sym+i-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	call fprintf (fd,
	    "\nThe seeing radius and assumed airmass for each image\n")
	do i = 1, nimages {
	    symbol = Memi[sym+i-1]
	    call fprintf (fd, "%30s  %8.4f  %8.3f\n")
		call pargstr (IMT_IMNAME(symbol))
		call pargr (IMT_RO(symbol))
		call pargr (IMT_XAIRMASS(symbol))
	}
	call fprintf (fd, "\n")

	call sfree (sp)
end


# PH_AEVAL -- Evaluate the fit for all the images.

procedure ph_aeval (imtable, agr, apcat, magfd, logfd, mgd, id, x, y, nap,
        rap, mag, merr, naperts, smallap, largeap)

pointer	imtable			# pointer to the image symbol table
pointer	agr			# pointer to the fitting structure
int	apcat			# the aperture correction file descriptor
int	magfd			# the best magnitudes file descriptor
int	logfd			# the log file descriptor
pointer	mgd			# pointer to the plot metacode file
int	id[ARB]			# the star ids
real	x[ARB]			# the star x coordinates
real	y[ARB]			# the star y coordinates
int	nap[ARB]		# the array of aperture numbers
real	rap[naperts,ARB]	# input array of aperture radii
real	mag[naperts,ARB]	# input array of magnitudes / differences
real	merr[naperts,ARB]	# input array of magnitude errors
int	naperts			# the number of input apertures
int	smallap			# the small aperture number
int	largeap			# the large aperture number

int	k, nimages
pointer	sp, sym, symbol
int	stnsymbols()
pointer	sthead(), stnext()

begin
	nimages = stnsymbols (imtable, 0)
	if (nimages <= 0)
	    return

	# Initialize some vectors.
	call aclrr (Memr[AGR_TAVE(agr)], naperts)
	call aclrr (Memr[AGR_TRESID(agr)], naperts)
	call aclrr (Memr[AGR_TRESSQ(agr)], naperts)
	call aclrr (Memr[AGR_TWR(agr)], naperts)

	# Allocate temporary space
	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)

	# Order the symbol table.
	symbol = sthead (imtable)
	do k = nimages, 1, -1 {
	    Memi[sym+k-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	# Write the banner for the apfile.
	call fprintf (apcat,
        "# The aperture correction from apertures %d to %d in magnitudes\n\n")
	    call pargi (smallap)
	    call pargi (largeap)

	# Write the banner for the magfile.
	if (magfd != NULL) {
	    call fprintf (magfd, "# Magnitudes corrected to aperture %d\n\n")
	        call pargi (largeap)
	    call fprintf (magfd,
"#%19tImage%30tFilter%39tExptime%47tAirmass%62tOtime%70tXcenter%80tYcenter\
%93tMag%101tMerr%107tRadius\n\n")
	}

	# Compute the aperture corrections.
	do k = 1, nimages {

	    symbol = Memi[sym+k-1]

	    # Initialize some vectors.
	    call aclrr (Memr[AGR_AVE(agr)], naperts)
	    call aclrr (Memr[AGR_RESID(agr)], naperts)
	    call aclrr (Memr[AGR_RESSQ(agr)], naperts)
	    call aclrr (Memr[AGR_WR(agr)], naperts)

	    call aclrr (Memr[AGR_ADOPT(agr)], naperts)
	    call aclrr (Memr[AGR_WADO(agr)], naperts)
	    call aclrr (Memr[AGR_WOBS(agr)], naperts)

	    call ph_rinit (rap[1,IMT_OFFSET(symbol)], Memr[AGR_RBAR(agr)],
	        naperts)
	    call ph_tinit (rap[1,IMT_OFFSET(symbol)], Memr[AGR_THEO(agr)],
	        naperts, Memd[AGR_PARAMS(agr)], IMT_RO(symbol),
		IMT_NXAIRMASS(symbol))

	    # Accumulate the difference data.
	    call ph_taccum (nap[IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
		merr[1,IMT_OFFSET(symbol)], Memr[AGR_THEO(agr)],
		Memr[AGR_W(agr)], Memr[AGR_ADOPT(agr)],
		Memr[AGR_WOBS(agr)], Memr[AGR_AVE(agr)],
		Memr[AGR_RESID(agr)], Memr[AGR_RESSQ(agr)],
		Memr[AGR_WR(agr)], Memr[AGR_OBS(agr)], Memr[AGR_WADO(agr)],
		naperts, IMT_NENTRIES(symbol))

	    # Compute the cumulative differences.
	    call ph_tcum (rap[1,IMT_OFFSET(symbol)], Memr[AGR_ADOPT(agr)],
		Memr[AGR_WADO(agr)], Memr[AGR_CUM(agr)], Memr[AGR_TCUM(agr)],
		Memr[AGR_WCUM(agr)], naperts, Memd[AGR_PARAMS(agr)],
		IMT_RO(symbol), IMT_NXAIRMASS(symbol))

	    # Write the aperture photometry file.
	    call ph_wapcat (apcat, IMT_IMNAME(symbol), IMT_RO(symbol),
	        Memr[AGR_ADOPT(agr)], Memr[AGR_WADO(agr)], naperts, smallap,
		largeap)

	    if (logfd != NULL) {
		call ph_twrite (logfd, IMT_IMNAME(symbol), IMT_RO(symbol),
		    IMT_XAIRMASS(symbol), Memr[AGR_RBAR(agr)],
		    Memr[AGR_THEO(agr)], Memr[AGR_OBS(agr)],
		    Memr[AGR_WOBS(agr)], Memr[AGR_ADOPT(agr)],
		    Memr[AGR_WADO(agr)], rap[1,IMT_OFFSET(symbol)],
		    Memr[AGR_CUM(agr)], Memr[AGR_TCUM(agr)],
		    Memr[AGR_WCUM(agr)], naperts)
		call fprintf (logfd, "\n")
		call ph_tmags (logfd, id[IMT_OFFSET(symbol)],
		    x[IMT_OFFSET(symbol)], y[IMT_OFFSET(symbol)],
		    nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
		    mag[1,IMT_OFFSET(symbol)], merr[1,IMT_OFFSET(symbol)],
		    Memr[AGR_ADOPT(agr)], Memr[AGR_W(agr)],
		    Memr[AGR_CUM(agr)], Memr[AGR_TCUM(agr)],
		    Memr[AGR_WCUM(agr)], Memr[AGR_MAGS(agr)],
		    Memr[AGR_CMAGS(agr)], Memr[AGR_TMAGS(agr)],
		    Memr[AGR_WMAGS(agr)], naperts, IMT_NENTRIES(symbol))
		call ph_papcor (logfd, IMT_IMNAME(symbol), IMT_RO(symbol),
		    rap[1,IMT_OFFSET(symbol)], Memr[AGR_ADOPT(agr)],
		    Memr[AGR_WADO(agr)], naperts, smallap, largeap)
		call fprintf (logfd, "\n")
	    }

	    if (magfd != NULL) {
		call ph_wmags (magfd, symbol, x[IMT_OFFSET(symbol)],
		    y[IMT_OFFSET(symbol)], nap[IMT_OFFSET(symbol)],
		    rap[1,IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
		    merr[1,IMT_OFFSET(symbol)], Memr[AGR_ADOPT(agr)],
		    Memr[AGR_WADO(agr)], Memr[AGR_W(agr)], Memr[AGR_CUM(agr)],
		    Memr[AGR_WCUM(agr)], Memr[AGR_MAGS(agr)],
		    Memr[AGR_CMAGS(agr)], Memr[AGR_WMAGS(agr)], naperts,
		    IMT_NENTRIES(symbol), largeap)
	    }

	    if (mgd != NULL) {
		call ph_gimfit (mgd, agr, IMT_IMNAME(symbol), IMT_RO(symbol),
	            IMT_XAIRMASS(symbol), nap[IMT_OFFSET(symbol)],
		    nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
		    mag[1,IMT_OFFSET(symbol)], naperts,
		    IMT_NENTRIES(symbol), YES)
	        if (! IS_INDEFR(IMT_RO(symbol))) {
		    call ph_gaimres (mgd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			nap[IMT_OFFSET(symbol)], nap[IMT_OFFSET(symbol)],
			rap[1,IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
			naperts, IMT_NENTRIES(symbol), YES)
		    call ph_gbimres (mgd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			nap[IMT_OFFSET(symbol)], nap[IMT_OFFSET(symbol)],
			mag[1,IMT_OFFSET(symbol)], naperts,
			IMT_NENTRIES(symbol), YES)
		    call ph_gaximres (mgd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
		        nap[IMT_OFFSET(symbol)], nap[IMT_OFFSET(symbol)],
		        x[IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
			naperts, IMT_NENTRIES(symbol), YES)
		    call ph_gayimres (mgd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			nap[IMT_OFFSET(symbol)], nap[IMT_OFFSET(symbol)],
			y[IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
			naperts, IMT_NENTRIES(symbol), YES)
		    call ph_gacum (mgd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			nap[IMT_OFFSET(symbol)], nap[IMT_OFFSET(symbol)],
			rap[1,IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
			naperts, IMT_NENTRIES(symbol), smallap, largeap, YES)
		}
	    }

	    if (! IS_INDEFR(IMT_RO(symbol))) {
	        call aaddr (Memr[AGR_AVE(agr)], Memr[AGR_TAVE(agr)],
	            Memr[AGR_TAVE(agr)], naperts)
	        call aaddr (Memr[AGR_RESID(agr)], Memr[AGR_TRESID(agr)],
	            Memr[AGR_TRESID(agr)], naperts)
	        call aaddr (Memr[AGR_RESSQ(agr)], Memr[AGR_TRESSQ(agr)],
	            Memr[AGR_TRESSQ(agr)], naperts)
	        call aaddr (Memr[AGR_WR(agr)], Memr[AGR_TWR(agr)],
	            Memr[AGR_TWR(agr)], naperts)
	    }
	}

	call sfree (sp)
end


# PH_A1EVAL -- Evaluate the fit for all the images.

procedure ph_a1eval (agr, image, r0, xairmass, nap, rap, mag, merr,
	naperts, npts)

pointer	agr			# pointer to the fitting structure
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	xairmass		# the airmass value
int	nap[ARB]		# the number of apertures array 
real	rap[naperts,ARB]	# the list of aperture radii
real	mag[naperts,ARB]	# the magnitude difference array
real	merr[naperts,ARB]	# the magnitude error array
int	naperts			# the number of apertures
int	npts			# the number of points

begin
	# Initialize some vectors.
	call aclrr (Memr[AGR_AVE(agr)], naperts)
	call aclrr (Memr[AGR_RESID(agr)], naperts)
	call aclrr (Memr[AGR_RESSQ(agr)], naperts)
	call aclrr (Memr[AGR_WR(agr)], naperts)
	call aclrr (Memr[AGR_ADOPT(agr)], naperts)
	call aclrr (Memr[AGR_WOBS(agr)], naperts)

	# Compute the theoretical curve
	call ph_rinit (rap, Memr[AGR_RBAR(agr)], naperts)
	call ph_tinit (rap, Memr[AGR_THEO(agr)], naperts,
	    Memd[AGR_PARAMS(agr)], r0, xairmass)

	# Accumulate the difference data.
	call ph_taccum (nap, mag, merr, Memr[AGR_THEO(agr)], Memr[AGR_W(agr)],
	    Memr[AGR_ADOPT(agr)], Memr[AGR_WOBS(agr)], Memr[AGR_AVE(agr)],
	    Memr[AGR_RESID(agr)], Memr[AGR_RESSQ(agr)], Memr[AGR_WR(agr)],
	    Memr[AGR_OBS(agr)], Memr[AGR_WADO(agr)], naperts, npts)

	# Compute the cumulative differences.
	call ph_tcum (rap, Memr[AGR_ADOPT(agr)], Memr[AGR_WADO(agr)],
	    Memr[AGR_CUM(agr)], Memr[AGR_TCUM(agr)], Memr[AGR_WCUM(agr)],
	    naperts, Memd[AGR_PARAMS(agr)], r0, xairmass)
end


# PH_TFSHOW -- Compute and print the summary statistics.

procedure ph_tfshow (fd, agr, naperts)

int	fd			# the output file descriptor
pointer	agr			# the pointer to the fit structure
int	naperts			# the number of apertures

begin
	call ph_rwrite (fd, Memr[AGR_TAVE(agr)], Memr[AGR_TWR(agr)],
	    Memr[AGR_TRESID(agr)], Memr[AGR_TRESSQ(agr)], naperts)
end


# PH_AMEMFREE -- Free the memory used for doing the fit

procedure ph_amemfree (agr)

pointer	agr			# pointer to the fitting structure

begin
	call mfree (AGR_DM(agr), TY_DOUBLE)
	call mfree (AGR_DDDR(agr), TY_DOUBLE)
	call mfree (AGR_T(agr), TY_DOUBLE)
	call mfree (AGR_U(agr), TY_DOUBLE)
	call mfree (AGR_V(agr), TY_DOUBLE)
	call mfree (AGR_POLD(agr), TY_DOUBLE)
	call mfree (AGR_DP(agr), TY_DOUBLE)
	call mfree (AGR_PARAMS(agr), TY_DOUBLE)
	call mfree (AGR_PERRORS(agr), TY_DOUBLE)
	call mfree (AGR_PCLAMPS(agr), TY_DOUBLE)

	call mfree (AGR_RBAR(agr), TY_REAL)
	call mfree (AGR_W(agr), TY_REAL)
	call mfree (AGR_THEO(agr), TY_REAL)
	call mfree (AGR_WR(agr), TY_REAL)
	call mfree (AGR_ADOPT(agr), TY_REAL)
	call mfree (AGR_WOBS(agr), TY_REAL)
	call mfree (AGR_OBS(agr), TY_REAL)
	call mfree (AGR_WADO(agr), TY_REAL)

	call mfree (AGR_CUM(agr), TY_REAL)
	call mfree (AGR_TCUM(agr), TY_REAL)
	call mfree (AGR_WCUM(agr), TY_REAL)
	call mfree (AGR_MAGS(agr), TY_REAL)
	call mfree (AGR_CMAGS(agr), TY_REAL)
	call mfree (AGR_TMAGS(agr), TY_REAL)
	call mfree (AGR_WMAGS(agr), TY_REAL)

	call mfree (AGR_AVE(agr), TY_REAL)
	call mfree (AGR_RESID(agr), TY_REAL)
	call mfree (AGR_RESSQ(agr), TY_REAL)
	call mfree (AGR_TAVE(agr), TY_REAL)
	call mfree (AGR_TRESID(agr), TY_REAL)
	call mfree (AGR_TRESSQ(agr), TY_REAL)
	call mfree (AGR_TWR(agr), TY_REAL)

	call mfree (agr, TY_STRUCT)
end


# PH_AGETP -- Fetch the initial values of the parameters for the cog model.

procedure ph_agetp (params)

double	params[ARB]

real	clgetr()

begin
	params[1] = clgetr ("swings")
	params[2] = clgetr ("pwings")
	params[3] = clgetr ("pgauss")
	params[4] = clgetr ("rgescale")
	params[5] = clgetr ("xwings")
end


# PH_APINIT -- Set the initial values for the curve of growth model parameters.

procedure ph_apinit (iparams, oparams, perrors, pclamps) 

double	iparams[ARB]		# the input parameters array
double	oparams[ARB]		# the output parameters array
double	perrors[ARB]		# the parameter errors array
double	pclamps[ARB]		# array of parameter clamps

begin
	call amovd (iparams, oparams, MAX_MTERMS)
	call aclrd (perrors, MAX_MTERMS)
	call amovkd (0.2d0, pclamps, MAX_MTERMS)
	pclamps[4] = 0.5d0
end


# PH_ANTERMS -- Compute the number of terms to use in the fit.

procedure ph_anterms (niter, lterms, gain, nterms)

int	niter			# the current iteration
int	lterms			# the maximum number of terms to be fit
real	gain			# the current value of the gain
int	nterms			# the current number of terms to be fit

int	n

begin
	n = 1
	if (nterms >= 1 && gain <= 0.04) {
	    n = 2
	    if (nterms >= 2 && gain <= 0.016) {
	        n = 3
		if (nterms >= 3 && gain <= 0.006) {
	            n = 4
		    if (nterms >= 4 && gain <= 0.0025)
	    	        n = 5
		}
	    }
	}

	nterms = min (n, lterms)
end


# PH_ADDDR -- Compute the derivative wrt ro vector.

procedure ph_adddr (rap, dddr, naperts, params, r0, xairmass)

real	rap[ARB]		# array of aperture radii
double  dddr[ARB]		# the output derivatives array
int	naperts			# the number of apertures
double  params[ARB]		# the input parameter array
real	r0			# the current value of r0
real	xairmass		# the current value of the airmass

int	i
double	ph_dmag()

begin
	do i = 2, naperts {
	    dddr[i] = 500.0d0 * (ph_dmag (rap[i-1], rap[i], xairmass,
	        r0 - 0.001, params) - ph_dmag (rap[i-1], rap[i], xairmass,
		r0 + 0.001, params))
	}
end


# PH_ADDDRDM -- Compute the derivative wrt ro vector.

procedure ph_adddrdm (rap, dm, dddr, naperts, params, r0, xairmass, niter)

real	rap[ARB]		# array of aperture radii
double	dm[ARB]			# the output model differences array
double  dddr[ARB]		# the output derivatives array
int	naperts			# the number of apertures
double  params[ARB]		# the input parameter array
real	r0			# the current value of r0
real	xairmass		# the current value of the airmass
int	niter			# the current iteratiom

int	i
double	ph_dmag()

begin
	do i = 2, naperts {
	    dm[i] = ph_dmag (rap[i-1], rap[i], xairmass, r0, params)
	    #if (niter == 1)
	        dddr[i] = 500.0d0 * (ph_dmag (rap[i-1], rap[i], xairmass,
	            r0 - 0.001, params) - ph_dmag (rap[i-1], rap[i], xairmass,
		    r0 + 0.001, params))
	}
end


# PH_AWSUM -- Accumulate the weighted sums necessary to fit r0.

procedure ph_awsum (nap, mag, merr, dm, w, dddr, naperts, npts, ipow,
	sumr, sumy)

int	nap[ARB]			# array of aperture numbers
real	mag[naperts,ARB]		# array of magnitude difference
real	merr[naperts,ARB]		# array of magnitude errors
double	dm[ARB]				# array of model differences
real	w[ARB]				# array of weights
double	dddr[ARB]			# array of model derivatives
int	naperts				# number of apertures
int	npts				# number of data points
int	ipow				# weighting power factor
double	sumr, sumy			# the output sums

int	i, j
real	diff, wt

begin
	do i = 1, npts {
	    w[1] = 1.0d0
	    if (nap[i] <= 1)
		next
	    do j = 2, nap[i] {
		diff = mag[j,i] - dm[j]
		w[j] = 1.0 / (1.0 + abs (diff / (2.0 * merr[j,i])) **
		    ipow)
		w[j] = min (w[j], w[j-1])
		wt = w[j] / merr[j,i] ** 2
		sumr = sumr + wt * diff * dddr[j]
		sumy = sumy + wt * dddr[j] ** 2
	    }
	}
end


# PH_ADP -- Compute the parameter derivative vector with the new
# value of r0.

procedure ph_adp (rap, dm, t, naperts, lterms, nterms, params, r0, xairmass)

real	rap[ARB]		# array of aperture radii
double	dm[ARB]			# the new model differences
double	t[lterms,ARB]		# the parameter derivatives matrix
int	naperts			# the number of apertures
int	lterms			# the size of the derivatives matrix
int	nterms			# the number of terms to be fit
double	params[ARB]		# the current model parameter values
real	r0			# the current r0 value
real	xairmass		# the current airmass

int	i, j
double	ph_dmag()

begin
	do j = 2, naperts {
	    dm[j] = ph_dmag (rap[j-1], rap[j], xairmass, r0, params)
	    do i = 1, nterms {
		params[i] = params[i] - 0.001
		t[i,j] = ph_dmag (rap[j-1], rap[j], xairmass, r0, params)
		params[i] = params[i] + 0.002
		t[i,j] = 500.0d0 * (t[i,j] - ph_dmag (rap[j-1], rap[j],
		    xairmass, r0, params))
		params[i] = params[i] - 0.001
	    }
	}
end


# PH_AACCUM -- Accumulate the matrix and vector required to solve for the
# parameter increments

procedure ph_aaccum (nap, mag, merr, dm, w, naperts, npts, t, u, v, lterms,
	nterms, ipow, sumd, sumw, sumn)

int	nap[ARB]			# array of aperture numbers
real	mag[naperts,ARB]		# array of magnitude difference
real	merr[naperts,ARB]		# array of magnitude errors
double	dm[ARB]				# array of model differences
real	w[ARB]				# array of weights
int	naperts				# number of apertures
int	npts				# number of data points
double	t[lterms,ARB]			# the array of parameter derivatives
double  u[lterms,ARB]			# the matrix to be accumulated
double	v[ARB]				# the vector to be accumulated
int	lterms				# the maximum number of terms to fit
int	nterms				# the current number of terms to fit
int	ipow				# power factor for the weights
double	sumd				# sum of the differences
double	sumw				# sum of the scatter
double	sumn				# sum of the weights

int	i, j, l, m
real	diff, wt

begin
	do i = 1, npts {
	    w[1] = 1.0
	    if (nap[i] <= 1)
		next
	    do j = 2, nap[i] {
		diff = mag[j,i] - dm[j]
		w[j] = 1.0 / (1.0 + abs (diff / (2.0 * merr[j,i])) ** ipow)
		w[j] = min (w[j], w[j-1])
		wt = w[j] / merr[j,i] ** 2
		sumd = sumd + wt * diff ** 2
		sumw = sumw + wt
		sumn = sumn + w[j]
		do l = 1, nterms {
		    v[l] = v[l] + wt * diff * t[l,j]
		    do m = 1, nterms
			u[l,m] = u[l,m] + wt * t[l,j] * t[m,j]
		}
	    }
	}
end


# PH_APSOLVE -- Solve for new values of the parameters.

procedure ph_apsolve (pold, params, dp, pclamps, nterms, ipow)

double	pold[ARB]		# the previous parameter imcrement values
double	params[ARB]		# the current values of the parameters
double	dp[ARB]			# the parameter increment values
double	pclamps[ARB]		# the parameter clamp values
int	nterms			# the number of terms that were fit
int	ipow			# the current weighting factor

int	i

begin
	do i = 1, nterms {
	    if (dp[i] * pold[i] < 0.0d0)
		pclamps[i] = 0.5 * pclamps[i]
	    pold[i] = dp[i]
	}

	params[1] = params[1] - dp[1] / ( 1.0d0 + abs (dp[1] / (pclamps[1] *
	    (params[1] - 1.0d0))))
	if (nterms >= 2) {
	    params[2] = params[2] - dp[2] / (1.0d0 + abs (dp[2] / (pclamps[2] *
		params[2] * (1.0d0 - params[2]))))
	    if (nterms >= 3) {
		params[3] = params[3] - dp[3] / (1.0d0 + abs (dp[3] /
		    (pclamps[3] * params[3] * (1.0d0 - params[3]))))
		if (ipow == 1) {
		    ipow = 2
		    pclamps[1] = 0.1d0
		    pclamps[2] = 0.1d0
		}
		if (nterms >= 4) {
		    params[4] = params[4] - dp[4] / (1.0d0 + abs (dp[4] /
			(pclamps[4] * params[4])))
		    if (nterms >= 5)
			params[5] = params[5] - dp[5] / (1.0d0 + abs (dp[5] /
			    (pclamps[5] * params[2])))
		}
	    }
	}
end

# PH_PWRITE -- Write out the theoetical model parameters.

procedure ph_pwrite (fd, params, perrors, max_nterms, nterms)

int	fd			# the output file descriptor
double	params[ARB]		# the current model parameter values
double	perrors[ARB]		# the current parameter errors
int	max_nterms		# the number of terms
int	nterms			# the number of terms to fit

int	i

begin
	# Print out the best fit parameters.
	call fprintf (fd,
	"\nThe computed cog model parameters and their errors for nparams %d\n")
	    call pargi (nterms)
	do i = 1, max_nterms {
	    call fprintf (fd, "\t%10s: %15.7g +/- %15.7g\n")
		switch (i) {
		case 1:
		    call pargstr ("swings")
		case 2:
		    call pargstr ("pwings")
		case 3:
		    call pargstr ("pgauss")
		case 4:
		    call pargstr ("rgescale")
		case 5:
		    call pargstr ("xwings")
		}
		call pargd (params[i])
		call pargd (perrors[i])
	}
	call fprintf (fd, "\n")
end

# PH_RINIT -- Initialize the rbar vector.

procedure ph_rinit (rap, rbar, naperts)

real	rap[ARB]		# the array of aperture radii
real	rbar[ARB]		# the mean radius estimates
int	naperts			# the number of aperture radii

int	j

begin
	do j = 2, naperts 
	    rbar[j] = 0.5 * (rap[j-1] + rap[j])
end

# PH_TINIT -- Initialize the rbar vector and compute the theoretical estimates.

procedure ph_tinit (rap, theo, naperts, params, r0, airmass)

real	rap[ARB]		# the array of aperture radii
real	theo[ARB]		# the current model estimates
int	naperts			# the number of aperture radii
double	params[ARB]		# the current parameter estimates
real	r0			# the seeing radius
real	airmass			# the airmass value

int	j
double	ph_dmag()

begin
	if (IS_INDEFR(r0))
	    call amovkr (INDEFR, theo[2], naperts - 1)
	else {
	    do j = 2, naperts 
	        theo[j] = ph_dmag (rap[j-1], rap[j], airmass, r0, params)
	}
end


# PH_TACCUM -- Accumulate the data.

procedure ph_taccum (nap, mag, merr, theo, w, adopt, wobs, ave, resid, ressq,
	wr, obs, wado, naperts, npts)

int	nap[ARB]			# array of aperture numbers
real	mag[naperts,ARB]		# the array of magnitude differences
real	merr[naperts,ARB]		# the array of magnitude errors
real	theo[ARB]			# the current model values
real	w[ARB]				# the working weight array
real	adopt[ARB]			# the adopted weight differences
real	wobs[ARB]			# the sum of the weights
real	ave[ARB]			# the average model values
real	resid[ARB]			# the residuals
real	ressq[ARB]			# the residuals squared
real	wr[ARB]				# the sum of the weights
real	obs[ARB]			# the observations
real	wado[ARB]			# the error in the observations
int	naperts				# the number of apertures
int	npts				# the number of npts

int	i, j
real	diff, wt, scale

begin
	do i = 1, npts {
	    w[1] = 1.0
	    if (nap[i] <= 1)
		next
	    do j = 2, nap[i] {
		if (IS_INDEFR(theo[j])) {
		    wt = 1.0 / merr[j,i] ** 2
		    ave[1] = INDEFR
		    ave[j] = INDEFR
		    resid[1] = INDEFR
		    resid[j] = INDEFR
		    ressq[1] = INDEFR
		    ressq[j] = INDEFR
		    wr[1] = INDEFR
		    wr[j] = INDEFR
		} else {
		    diff = mag[j,i] - theo[j]
		    w[j] = 1.0 / (1.0 + abs (diff / (2.0 * merr[j,i])))
		    w[j] = min (w[j], w[j-1])
		    wt = w[j] / merr[j,i] ** 2
		    ave[1] = ave[1] + wt * theo[j]
		    ave[j] = ave[j] + wt * theo[j]
		    resid[1] = resid[1] + wt * diff
		    resid[j] = resid[j] + wt * diff
		    ressq[1] = ressq[1] + wt * diff ** 2
		    ressq[j] = ressq[j] + wt * diff ** 2
		    wr[1] = wr[1] + wt
		    wr[j] = wr[j] + wt
		}
		adopt[j] = adopt[j] + wt * mag[j,i]
		wobs[j] = wobs[j] + wt
	    }
	}

	do j = 2, naperts {
	    if (wobs[j] <= 0.0)
		next
	    obs[j] = adopt[j] / wobs[j]
	    adopt[j] = 0.0
	    wobs[j] = 0.0
	}

	do i = 1, npts {
	    w[1] = 1.0
	    if (nap[i] <= 1)
		next
	    do j = 2, nap[i] {
		diff = mag[j,i] - obs[j]
		w[j] = 1.0 / (1.0 + abs (diff / (2.0 * merr[j,i])) ** 2)
		w[j] = min (w[j], w[j-1])
		wt = w[j] / merr[j,i] ** 2
		adopt[j] = adopt[j] + wt * mag[j,i]
		wobs[j] = wobs[j] + wt
	    }
	}

	do j = 2, naperts {
	    if (wobs[j] <= 0.0)
		next
	    obs[j] = adopt[j] / wobs[j]
	    adopt[j] = 0.0
	    wobs[j] = 0.0
	}

	do i = 1, npts {
	    w[1] = 1.0
	    if (nap[i] <= 1)
		next
	    do j = 2, nap[i] {
		diff = mag[j,i] - obs[j]
		w[j] = 1.0 / (1.0 + abs (diff / (2.0 * merr[j,i])) ** 3)
		w[j] = min (w[j], w[j-1])
		wt = w[j] / merr[j,i] ** 2
		adopt[j] = adopt[j] + wt * mag[j,i]
		wobs[j] = wobs[j] + wt
	    }
	}

	scale = 0.1
	do j = 2, naperts {
	    if (wobs[j] > 0.0) {
		obs[j] = adopt[j] / wobs[j]
		if (IS_INDEFR(theo[j]))
		    wt = 0.0
		else {
		    wt = 1.0 / (scale * theo[j]) ** 2
		    adopt[j] = adopt[j] + wt * theo[j]
		}
		wt = 1.0 / (wobs[j] + wt)
		adopt[j] = wt * adopt[j]
		#wado[j] = sqrt (wt)
		wado[j] = sqrt (wt + (scale * adopt[j]) ** 2)
		wobs[j] = sqrt (1.0 / wobs[j])
	    } else {
		adopt[j] = theo[j]
		if (IS_INDEFR(theo[j])) {
		    wado[j] = INDEFR
		    obs[j] = INDEFR
		    wobs[j] = INDEFR
		} else
		    wado[j] = 2.0 * scale * abs (theo[j])
	    }
	}
end


# PH_TCUM -- Compute the cumulative differences.

procedure ph_tcum (rap, adopt, wado, cum, tcum, wcum, naperts, params,
	r0, airmass)

real	rap[ARB]		# the list of aperture radii
real	adopt[ARB]		# the adopted differences
real	wado[ARB]		# the errors in the adopted differences
real	cum[ARB]		# the accumulated differences
real	tcum[ARB]		# the total accumulated differences
real	wcum[ARB]		# the accumulated difference errors
int	naperts			# the number of aperture radii
double	params[ARB]		# the current parameter values
real	r0			# the seeing radius
real	airmass			# the airmass

int	i
double	ph_dmag()

begin
	if (IS_INDEFR(r0)) {
	    call amovkr (INDEFR, cum, naperts + 1)
	    call amovkr (INDEFR, tcum, naperts + 1)
	    call amovkr (INDEFR, wcum, naperts + 1)
	} else {
	    cum[naperts+1] = 0.0
	    tcum[naperts+1] = ph_dmag (rap[naperts], 2.0 * rap[naperts],
	        airmass, r0, params)
	    wcum[naperts+1] = 0.0
	    do i = naperts, 2, -1 {
	        cum[i] = adopt[i] + cum[i+1]
	        tcum[i] = adopt[i] + tcum[i+1]
	        wcum[i] = wado[i] ** 2 + wcum[i+1]
	    }
	}
end


# PH_WAPCAT -- Write the image entry to the aperture correction catalog.

procedure ph_wapcat (apcat, image, r0, adopt, wado, naperts, smallap, largeap)

int	apcat			# the aperture correction catalog descriptor
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	adopt[ARB]		# the adopted difference values
real	wado[ARB]		# the adopted difference errors
int	naperts			# the number of apertures
int	smallap			# the small aperture number
int	largeap			# the large aperture number

int	i
real	cum, wcum

begin
	if (IS_INDEFR(r0)) {
	    cum = INDEFR
	    wcum = INDEFR
	} else {
	    cum = 0.0
	    wcum = 0.0
	    do i = largeap, smallap + 1, -1 {
	        cum = cum + adopt[i]
	        wcum = wcum + wado[i] ** 2
	    }
	    wcum = sqrt (wcum)
	}
	call fprintf (apcat, "%s %g %g\n")
	    call pargstr (image)
	    call pargr (cum)
	    call pargr (wcum)
end


# PH_PAPCOR -- Print the aperture correction on the standard output.

procedure ph_papcor (fd, image, r0, rap, adopt, wado, naperts, smallap, largeap)

int	fd			# the output file descriptor
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	rap[ARB]		# the aperture radii
real	adopt[ARB]		# the adopted difference values
real	wado[ARB]		# the adopted difference errors
int	naperts			# the number of apertures
int	smallap			# the small aperture number
int	largeap			# the large aperture number

int	i
real	cum, wcum

begin
	if (IS_INDEFR(r0)) {
	    cum = INDEFR
	    wcum = INDEFR
	}  else {
	    cum = 0.0
	    wcum = 0.0
	    do i = largeap, smallap + 1, -1 {
	        cum = cum + adopt[i]
	        wcum = wcum + wado[i] ** 2
	    }
	    wcum = sqrt (wcum)
	}

	call fprintf (fd,
	    "Image: %s rin=%.2f rout=%.2f apercor=%.3f +/- %.4f\n")
	    call pargstr (image)
	    call pargr (rap[smallap])
	    call pargr (rap[largeap])
	    call pargr (cum)
	    call pargr (wcum)
end


define	NPERLINE	7

# PH_TWRITE -- Write the results to the output file.

procedure ph_twrite (fd, image, r0, xairmass, rbar, theo, obs, wobs, adopt,
	wado, rap, cum, tcum, wcum, naperts)

int	fd			# the file descriptor
char	image			# the iamge name
real	r0			# the seeing disk
real	xairmass		# the assumed airmass
real	rbar[ARB]		# the list of mean aperture radii
real	theo[ARB]		# the theoretical model differences
real	obs[ARB]		# the observed differences
real	wobs[ARB]		# the observed difference errors
real	adopt[ARB]		# the adopted differences
real	wado[ARB]		# the adopted difference errors
real	rap[ARB]		# the list of aperture radii
real	cum[ARB]		# the cumulative differences
real	tcum[ARB]		# the total cumulative differences
real	wcum[ARB]		# the errors in the cumulative differences
int	naperts			# the number of aperture

int	j
real	sqwcum

begin
	# Print the title.
	call fprintf (fd, "\nThe cog for image %s from radius %.2f to %.2f\n")
	    call pargstr (image)
	    call pargr (rbar[2])
	    call pargr (rbar[naperts])

	# Print the mean aperture radius.
	do j = 2, naperts {
	    if (j == 2) {
		call fprintf (fd, "  radius  %9.4f")
		    call pargr (rbar[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (rbar[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (rbar[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (rbar[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	# Print the theoretical model.
	do j = 2, naperts {
	    if (j == 2) {
		call fprintf (fd, "   model  %9.4f")
		    call pargr (theo[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (theo[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (theo[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (theo[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	# Print the observed cog.
	do j = 2, naperts {
	    if (j == 2) {
		call fprintf (fd, "observed  %9.4f")
		    call pargr (obs[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (obs[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (obs[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (obs[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	# Print the errors in the observed cog.
	do j = 2, naperts {
	    if (j == 2) {
		call fprintf (fd, "   sigma  %9.4f")
		    call pargr (wobs[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (wobs[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (wobs[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (wobs[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	# Print the adopted cog.
	do j = 2, naperts {
	    if (j == 2) {
		call fprintf (fd, " adopted  %9.4f")
		    call pargr (adopt[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (adopt[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (adopt[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (adopt[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	# Print the errors in the adopted cog.
	do j = 2, naperts {
	    if (j == 2) {
		call fprintf (fd, "   sigma  %9.4f")
		    call pargr (wado[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (wado[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (wado[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (wado[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	# Print the title.
	call fprintf (fd,
	    "\nThe aperture correction for image %s from radius %.2f to %.2f\n")
	    call pargstr (image)
	    call pargr (rap[1])
	    call pargr (rap[naperts])

	# Print the aperture radii.
	do j = 1, naperts {
	    if (j == 1) {
		call fprintf (fd, "  radius  %9.4f")
		    call pargr (rap[j])
	    } else if (mod (j, NPERLINE) == 0) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (rap[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "          %9.4f")
		    call pargr (rap[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (rap[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 0)
	    call fprintf (fd, "\n")

	# Print the aperture correction.
	do j = 2, naperts {
	    if (j == 2) {
		call fprintf (fd, " apercor  %9.4f")
		    call pargr (cum[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (cum[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (cum[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (cum[j])
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")


	# Print the error in the aperture correction.
	do j = 2, naperts {
	    sqwcum = wcum[j]
	    if (j == 2) {
		call fprintf (fd, "   sigma  %9.4f")
		    call pargr (sqwcum)
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (sqwcum)
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (sqwcum)
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (sqwcum)
	    }
	}
	if (mod (naperts, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	# Print the title.
	call fprintf (fd,
	    "\nThe aperture correction for image %s from radius %.2f to %.2f\n")
	    call pargstr (image)
	    call pargr (rap[1])
	    call pargr (2.0 * rap[naperts])

	# Print the total aperture correction.
	do j = 2, naperts + 1 {
	    if (j == 2) {
		call fprintf (fd, "tapercor  %9.4f")
		    call pargr (tcum[j])
	    } else if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "%9.4f\n")
		    call pargr (tcum[j])
	    } else if (mod (j, NPERLINE) == 2) {
		call fprintf (fd, "          %9.4f")
		    call pargr (tcum[j])
	    } else {
		call fprintf (fd, "%9.4f")
		    call pargr (tcum[j])
	    }
	}
	if (mod (naperts + 1, NPERLINE) != 1)
	    call fprintf (fd, "\n")

	call fprintf (fd, "\n")
end


# PH_TMAGS -- Compute the correction to the last computed magnitude for
# each star and the total magnitude as a function of aperture.

procedure ph_tmags (logfd, id, x, y, nap, rap, mag, merr, adopt, w, cum,
	tcum, wcum, tmpmags, obs, tobs, wobs, naperts, npts)

int	logfd			# the output file descriptor
int	id[ARB]			# stellar id numbers
real	x[ARB]			# stellar x coordinates
real	y[ARB]			# stellar y coordinates
int	nap[ARB]		# array of aperture numbers
real	rap[naperts,ARB]	# the list of aperture radii
real	mag[naperts,ARB]	# the input magnitude difference array
real	merr[naperts,ARB]	# the input magnitude error array
real	adopt[ARB]		# the adopted difference values
real	w[ARB]			# the working weight array
real	cum[ARB]		# the accumulated difference array
real	tcum[ARB]		# the total accumulated difference array
real	wcum[ARB]		# the errors in the accumulated diff array
real	tmpmags[ARB]		# temporary magnitude array
real	obs[ARB]		# the accumulated magnitude array
real	tobs[ARB]		# the total accumulated magnitude array
real	wobs[ARB]		# the observations array
int	naperts			# number of apertures
int	npts			# number of points

int	i, j
real	diff

begin
	# Print the logfile banner.
	if (logfd != NULL) {
	    call fprintf (logfd, "\nThe observed, and corrected to radii %.2f ")
		call pargr (rap[naperts,1])
	    call fprintf (logfd, "and %.2f, magnitudes\n")
		call pargr (2.0 * rap[naperts,1])
	}

	do i = 1, npts {

	    # Compute the observed, correctged and estimated total 
	    # magnitudes.
	    tmpmags[1] = mag[1,i]
	    do j = 1, nap[i] {
		if (j == 1)
		    w[j] = 1.0
		else {
		    diff = mag[j,i] - adopt[j]
		    tmpmags[j] = tmpmags[j-1] + mag[j,i]
		    w[j] = 1.0 / (1.0 + (diff / (2.0 * merr[j,i])) ** 2)
		    w[j] = min (w[j], w[j-1])
		}
		if (IS_INDEFR(cum[j+1])) {
		    obs[j]  = INDEFR
		    tobs[j]  = INDEFR
		    wobs[j] = INDEFR
		} else {
		    obs[j]  = tmpmags[j] + cum[j+1]
		    tobs[j]  = tmpmags[j] + tcum[j+1]
		    wobs[j] = sqrt (wcum[j+1] + merr[j,i] ** 2 / w[j])
		}
	    }
	    do j = nap[i] + 1, naperts {
		obs[j]  = INDEFR
		tobs[j]  = INDEFR
		wobs[j] = INDEFR
	    }

	    # Write out the results for the star to the log file.

	    # Print the banner of the star.
	    call fprintf (logfd, "Star: %d  x: %.3f y: %.3f\n")
	        call pargi (id[i])
	        call pargr (x[i])
	        call pargr (y[i])

	    # Print the aperture radii.
	    do j = 1, naperts {
	        if (j == 1) {
		    call fprintf (logfd, "  radius  %9.4f")
		        call pargr (rap[j,i])
	        } else if (mod (j, NPERLINE) == 0) {
		    call fprintf (logfd, "%9.4f\n")
		        call pargr (rap[j,i])
	        } else if (mod (j, NPERLINE) == 1) {
		    call fprintf (logfd, "          %9.4f")
		        call pargr (rap[j,i])
	        } else {
		    call fprintf (logfd, "%9.4f")
		        call pargr (rap[j,i])
	        }
	    }
	    if (mod (naperts, NPERLINE) != 0)
	        call fprintf (logfd, "\n")

	    # Print the observed magnitudes.
	    do j = 1, naperts {
	        if (j == 1) {
		    call fprintf (logfd, "    mags  %9.4f")
	                call pargr (tmpmags[j])
	        } else if (mod (j, NPERLINE) == 0) {
		    call fprintf (logfd, "%9.4f\n")
		        call pargr (tmpmags[j])
	        } else if (mod (j, NPERLINE) == 1) {
		    call fprintf (logfd, "          %9.4f")
		        call pargr (tmpmags[j])
	        } else {
		    call fprintf (logfd, "%9.4f")
		        call pargr (tmpmags[j])
	        }
	    }
	    if (mod (naperts, NPERLINE) != 0)
	        call fprintf (logfd, "\n")

	    # Print the corrected magnitudes.
	    do j = 1, naperts {
	        if (j == 1) {
		    call fprintf (logfd, "   cmags  %9.4f")
		        call pargr (obs[j])
	        } else if (mod (j, NPERLINE) == 0) {
		    call fprintf (logfd, "%9.4f\n")
		        call pargr (obs[j])
	        } else if (mod (j, NPERLINE) == 1) {
		    call fprintf (logfd, "          %9.4f")
		        call pargr (obs[j])
	        } else {
		    call fprintf (logfd, "%9.4f")
		        call pargr (obs[j])
	        }
	    }
	    if (mod (naperts, NPERLINE) != 0)
	        call fprintf (logfd, "\n")

	    # Print the estimated total corrected magnitudes.
	    do j = 1, naperts {
	        if (j == 1) {
		    call fprintf (logfd, "  tcmags  %9.4f")
		        call pargr (tobs[j])
	        } else if (mod (j, NPERLINE) == 0) {
		    call fprintf (logfd, "%9.4f\n")
		        call pargr (tobs[j])
	        } else if (mod (j, NPERLINE) == 1) {
		    call fprintf (logfd, "          %9.4f")
		        call pargr (tobs[j])
	        } else {
		    call fprintf (logfd, "%9.4f")
		        call pargr (tobs[j])
	        }
	    }
	    if (mod (naperts, NPERLINE) != 0)
	        call fprintf (logfd, "\n")

	    # Print the errors in the total magnitudes
	    do j = 1, naperts {
	        if (j == 1) {
		    call fprintf (logfd, "   sigma  %9.4f")
		        call pargr (wobs[j])
	        } else if (mod (j, NPERLINE) == 0) {
		    call fprintf (logfd, "%9.4f\n")
		        call pargr (wobs[j])
	        } else if (mod (j, NPERLINE) == 1) {
		    call fprintf (logfd, "          %9.4f")
		        call pargr (wobs[j])
	        } else {
		    call fprintf (logfd, "%9.4f")
		        call pargr (wobs[j])
	        }
	    }
	    if (mod (naperts, NPERLINE) != 0)
	        call fprintf (logfd, "\n")
	}

	call fprintf (logfd, "\n")
end


# PH_WMAGS -- Compute the correction to the last computed magnitude for
# each star and the total magnitude as a function of aperture.

procedure ph_wmags (magfd, imsymbol, x, y, nap, rap, mag, merr, adopt, wado,
    w, cum, wcum, tmpmags, obs, wobs, naperts, npts, largeap)

int	magfd			# the best magnitudes file descriptor
pointer	imsymbol		# the image symbol
real	x[ARB]			# stellar x coordinates
real	y[ARB]			# stellar y coordinates
int	nap[ARB]		# array of aperture numbers
real	rap[naperts,ARB]	# the input aperture radii
real	mag[naperts,ARB]	# the input magnitude difference array
real	merr[naperts,ARB]	# the input magnitude error array
real	adopt[ARB]		# the adopted difference values
real	wado[ARB]		# the adopted difference errors
real	w[ARB]			# the working weight array
real	cum[ARB]		# the accumulated difference array
real	wcum[ARB]		# the errors in the accumulated diff array
real	tmpmags[ARB]		# temporary magnitude array
real	obs[ARB]		# the accumulated magnitude array
real	wobs[ARB]		# the observations array
int	naperts			# number of apertures
int	npts			# number of points
int	largeap			# the largest aperture

int	i, j, jfinal
real	diff, sigmin, sigcor
real	assqr()

begin
	do i = 1, npts {

	    # Compute the observed, correctged and estimated total magnitudes.
	    tmpmags[1] = mag[1,i]
	    sigmin = MAX_REAL
	    #jfinal = min (nap[i], largeap)
	    jfinal = largeap
	    if (largeap < nap[i])
		sigcor = assqr (wado[largeap+1], nap[i] - largeap)
	    else
		sigcor = 0.0
	    do j = 1, min (nap[i], largeap) {
		if (j == 1)
		    w[j] = 1.0
		else {
		    diff = mag[j,i] - adopt[j]
		    tmpmags[j] = tmpmags[j-1] + mag[j,i]
		    w[j] = 1.0 / (1.0 + (diff / (2.0 * merr[j,i])) ** 2)
		    w[j] = min (w[j], w[j-1])
		}
		if (IS_INDEFR(cum[j+1])) {
		    obs[j] = INDEFR
		    wobs[j] = INDEFR
		} else {
		    obs[j]  = tmpmags[j] + cum[j+1] - cum[largeap+1]
		    wobs[j] = sqrt (wcum[j+1] - sigcor + merr[j,i] ** 2 / w[j])
		    if (wobs[j] < sigmin) {
		        jfinal = j
		        sigmin = wobs[j]
		    }
		}
	    }

	    # Write out the best magnitude.
	    if (magfd != NULL) {
		call fprintf (magfd,
"%23.23s  %10.10s  %8.2f  %6.3f  %11.1h  %8.2f  %8.2f  %7.3f  %7.3f  %6.3f\n")
		    call pargstr (IMT_IMNAME(imsymbol))
		    call pargstr (IMT_IFILTER(imsymbol))
		    call pargr (IMT_ITIME(imsymbol))
		    call pargr (IMT_XAIRMASS(imsymbol))
		    call pargr (IMT_OTIME(imsymbol))
		    call pargr (x[i])
		    call pargr (y[i])
		    call pargr (obs[jfinal])
		    call pargr (wobs[jfinal])
		    call pargr (rap[jfinal,i])
	    }
	}
end


# PH_ACHI -- Compute the chi statistic.

real procedure ph_achi (wr, resid, ressq, naperts)

real	wr[ARB]			# the weights
real	resid[ARB]		# the residuals
real	ressq[ARB]		# the residuals
int	naperts			# the number of apertures

double	sumwr, sumres, sumressq
real	chi
real	asumr()

begin
	sumwr = asumr (wr, naperts)
	sumres = asumr (resid, naperts)
	sumressq = asumr (ressq, naperts)
	if (sumwr <= 0.0d0)
	    chi = 0.0
	else
	    chi = sumressq / sumwr - (sumres / sumwr) ** 2
	if (chi <= 0.0 || chi > MAX_REAL)
	    return (INDEFR)
	else
	    return (chi)
end


# PH_RWRITE -- Write out the fit statistics for each aperture.

procedure ph_rwrite (fd, ave, wr, resid, ressq, naperts)

int	fd			# the output file descriptor
real	ave[ARB]		# the average values
real	wr[ARB]			# the weights
real	resid[ARB]		# the residuals
real	ressq[ARB]		# the residuals
int	naperts			# the number of apertures

int	j
real	rtmp

begin
	call fprintf (fd, "\nAverage model cog, residual, and rms residual ")
	call fprintf (fd, "for each aperture all images\n")
	do j = 1, naperts {
	    if (wr[j] <= 0.0) {
		ave[j] = INDEFR
		resid[j] = INDEFR
		ressq[j] = INDEFR
	    } else {
	        ave[j] = ave[j] / wr[j]
	        resid[j] = resid[j] / wr[j]
	        rtmp = ressq[j] / wr[j] - resid[j] ** 2
		if (rtmp <= 0.0)
		    ressq[j] = 0.0
		else
	            ressq[j] = sqrt (rtmp)
	    }
	    call fprintf (fd, "\t%3d  %9.5f  %9.5f  %9.5f\n")
		call pargi (j)
		call pargr (ave[j])
		call pargr (resid[j])
		call pargr (ressq[j])
	}
	call fprintf (fd, "\n")
end


# PH_DMAG -- Compute the integral of the magnitude between two radii
# assuming that the stellar profile can be approximated by a circular
# Moffat function.

double procedure ph_dmag (r1, r2, x, r0, params)

real	r1			# beginning radius for the integration
real	r2			# ending radius for the integration
real	x			# the airmass value
real	r0			# the hwhm of the psf
double	params[ARB]		# the model parameter array

double	bpex, pm1, x1, x2, x1sq, x2sq, d1, d2, i1, i2, dmag 

begin
	bpex = params[2] + params[5] * x
	pm1 = params[1] - 1.0d0
	x1 = (r1 / r0)
	x2 = (r2 / r0)
	x1sq = x1 ** 2
	x2sq = x2 ** 2
	x1 = x1 / params[4]
	x2 = x2 / params[4]
	d1 = 1.0d0 + r1 ** 2
	d2 = 1.0d0 + r2 ** 2
	i1 = bpex * (1.0d0 - 1.0d0 / d1 ** pm1) + (1.0d0 - bpex) *
	    (params[3] * (1.0d0 - exp (-0.5d0 * x1sq)) + (1.0d0 - params[3]) *
	    (1.0d0 - (1.0d0 + x1) * exp (-x1)))
	i2 = bpex * (1.0d0 - 1.0d0 / d2 ** pm1) + (1.0d0 - bpex) *
	    (params[3] * (1.0d0 - exp (-0.5d0 * x2sq)) + (1.0d0 - params[3]) *
	    (1.0d0 - (1.0d0 + x2) * exp (-x2)))
	dmag = -2.5d0 * log10 (i2 / i1)

	return (dmag)
end


# DMVMUL -- Multply a matrix (left-hand side) by a one dimensional vector
# (right-hand side) and return the resultant vector.

procedure dmvmul (matrix, maxdim, dim, vector, result)

double	matrix [maxdim, maxdim]	# input matrix
int	maxdim			# maximum size of input matrix
int	dim			# dimension of matrix and vectors
double	vector[maxdim]		# input vector
double	result[maxdim]		# iutput vector

double	sum
int	i, j

begin
	do i = 1, dim {
	    sum = 0.0d0
	    do j = 1, dim
		sum = sum + (matrix[j,i] * vector[j])
	    result[i] = sum
	}
end
