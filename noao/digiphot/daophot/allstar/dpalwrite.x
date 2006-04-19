include	<tbset.h>
include <time.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/allstardef.h"

# DP_TNEWAL -- Create an new ALLSTAR output ST table.

procedure dp_tnewal (dao, tp, colpoint)

pointer	dao		# pointer to the daophot strucuture
pointer	tp		# pointer to the output table
int	colpoint[ARB]	# array of column pointers

int	i
pointer	sp, colnames, colunits, colformat, col_dtype, col_len

begin
	# Allocate space for table definition.
	call smark (sp)
	call salloc (colnames, ALL_NOUTCOLUMN * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, ALL_NOUTCOLUMN * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, ALL_NOUTCOLUMN * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (col_dtype, ALL_NOUTCOLUMN, TY_INT)
	call salloc (col_len, ALL_NOUTCOLUMN, TY_INT)

	# Set up the column definitions.
	call strcpy (ID, Memc[colnames], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)
	call strcpy (MAGERR, Memc[colnames+4*SZ_COLNAME+4], SZ_COLNAME)
	call strcpy (SKY, Memc[colnames+5*SZ_COLNAME+5], SZ_COLNAME)
	call strcpy (NITER, Memc[colnames+6*SZ_COLNAME+6], SZ_COLNAME)
	call strcpy (SHARP, Memc[colnames+7*SZ_COLNAME+7], SZ_COLNAME)
	call strcpy (CHI, Memc[colnames+8*SZ_COLNAME+8], SZ_COLNAME)
	call strcpy (PIER, Memc[colnames+9*SZ_COLNAME+9], SZ_COLNAME)
	call strcpy (PERROR, Memc[colnames+10*SZ_COLNAME+10], SZ_COLNAME)

	# Define the column formats.
	call strcpy ("%6d", Memc[colformat], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)
	call strcpy ("%14.3f", Memc[colformat+4*SZ_COLFMT+4], SZ_COLFMT)
	call strcpy ("%15.7g", Memc[colformat+5*SZ_COLFMT+5], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+6*SZ_COLFMT+6], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+7*SZ_COLFMT+7], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+8*SZ_COLFMT+8], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+9*SZ_COLFMT+9], SZ_COLFMT)
	call strcpy ("%13s", Memc[colformat+10*SZ_COLFMT+10], SZ_COLFMT)

	# Define the column units.
	call strcpy ("NUMBER", Memc[colunits], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+SZ_COLUNITS+1], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+2*SZ_COLUNITS+2], SZ_COLUNITS)
	call strcpy ("MAGNITUDES", Memc[colunits+3*SZ_COLUNITS+3], SZ_COLUNITS)
	call strcpy ("MAGNITUDES", Memc[colunits+4*SZ_COLUNITS+4], SZ_COLUNITS)
	call strcpy ("ADC", Memc[colunits+5*SZ_COLUNITS+5], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+6*SZ_COLUNITS+6], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+7*SZ_COLUNITS+7], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+8*SZ_COLUNITS+8], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+9*SZ_COLUNITS+9], SZ_COLUNITS)
	call strcpy ("PERRORS", Memc[colunits+10*SZ_COLUNITS+10], SZ_COLUNITS)

	# Define the column types.
	Memi[col_dtype] = TY_INT
	Memi[col_dtype+1] = TY_REAL
	Memi[col_dtype+2] = TY_REAL
	Memi[col_dtype+3] = TY_REAL
	Memi[col_dtype+4] = TY_REAL
	Memi[col_dtype+5] = TY_REAL
	Memi[col_dtype+6] = TY_INT
	Memi[col_dtype+7] = TY_REAL
	Memi[col_dtype+8] = TY_REAL
	Memi[col_dtype+9] = TY_INT
	Memi[col_dtype+10] = -13

	# Define the column lengths.
	do i = 1, ALL_NOUTCOLUMN 
	    Memi[col_len+i-1] = 1
	
	# Define and create the table.
	call tbcdef (tp, colpoint, Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[col_dtype], Memi[col_len], ALL_NOUTCOLUMN)
	call tbtcre (tp)

	# Write out the header parameters.
	call dp_talpars (dao, tp)

	call sfree (sp)
	
end


define AL_NAME1STR "#N%4tID%10tXCENTER%20tYCENTER%30tMAG%42tMERR%56tMSKY%71t\
NITER%80t\\\n"
define AL_UNIT1STR "#U%4t##%10tpixels%20tpixels%30tmagnitudes%42tmagnitudes%56t\
counts%71t##%80t\\\n"
define AL_FORMAT1STR "#F%4t%%-9d%10t%%-10.3f%20t%%-10.3f%30t%%-12.3f%42t\
%%-14.3f%56t%%-15.7g%71t%%-6d%80t \n"

define AL_NAME2STR "#N%12tSHARPNESS%24tCHI%36tPIER%42tPERROR%80t\\\n"
define AL_UNIT2STR "#U%12t##%24t##%36t##%42tperrors%80t\\\n"
define AL_FORMAT2STR "#F%12t%%-23.3f%24t%%-12.3f%36t%%-6d%42t%%-13s%80t \n"


# DP_XNEWAL -- Initialize a new output ALLSTAR text file.

procedure dp_xnewal (dao, tp)

pointer	dao		# pointer to the daophot structure
int	tp		# the output file descriptor

begin
	# Write out the header parameters.
	call dp_xalpars (dao, tp)

	# Write out the banner.
	call fprintf (tp, "#\n")
	call fprintf (tp, AL_NAME1STR)
	call fprintf (tp, AL_UNIT1STR)
	call fprintf (tp, AL_FORMAT1STR)
	call fprintf (tp, "#\n")
	call fprintf (tp, AL_NAME2STR)
	call fprintf (tp, AL_UNIT2STR)
	call fprintf (tp, AL_FORMAT2STR)
	call fprintf (tp, "#\n")
end


# DP_TALPARS -- Add various parameters to the header of the ALLSTAR table.

procedure dp_talpars (dao, tp)

pointer	dao			# pointer to the daophot structure
pointer	tp			# pointer to the output table

pointer	sp, psffit, outstr, date, time
bool	itob()
int	envfind()

begin
	# Define some daophot pointers.
	psffit = DP_PSFFIT(dao)

	# Allocate workin space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	# Write the id.
	if (envfind ("version", Memc[outstr], SZ_LINE) <= 0)
	    call strcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call tbhadt (tp, "IRAF", Memc[outstr])
	if (envfind ("userid", Memc[outstr], SZ_LINE) <= 0)
	    call tbhadt (tp, "USER", Memc[outstr])
	call gethost (Memc[outstr], SZ_LINE)
	call tbhadt (tp, "HOST", Memc[outstr])
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call tbhadt (tp, "DATE", Memc[date])
	call tbhadt (tp, "TIME", Memc[time])
	call tbhadt (tp, "PACKAGE", "daophot")
	call tbhadt (tp, "TASK", "allstar")

	# Write out the files names.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "IMAGE", Memc[outstr])
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PHOTFILE", Memc[outstr])
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PSFIMAGE", Memc[outstr])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "ALLSTARFILE", Memc[outstr])
	if (DP_OUTREJFILE(dao) == EOS) {
	    call tbhadt (tp, "REJFILE", "\"\"")
	} else {
	    call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_LINE)
	    call tbhadt (tp, "REJFILE", Memc[outstr])
	}
	call dp_imroot (DP_OUTIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "SUBIMAGE", Memc[outstr])

	# Define the data characteristics.
	call tbhadr (tp, "SCALE", DP_SCALE(dao))
	call tbhadr (tp, "DATAMIN", DP_MINGDATA(dao))
	call tbhadr (tp, "DATAMAX", DP_MAXGDATA(dao))
	call tbhadr (tp, "GAIN", DP_PHOTADU(dao))
	call tbhadr (tp, "READNOISE", DP_READNOISE(dao))

	# Define the observing parameters.
	call tbhadt (tp, "OTIME", DP_OTIME(dao))
	call tbhadr (tp, "XAIRMASS", DP_XAIRMASS(dao))
	call tbhadt (tp, "IFILTER", DP_IFILTER(dao))

	# Define the fitting parameters.
	call tbhadb (tp, "RECENTER", itob (DP_RECENTER(dao)))
	call tbhadb (tp, "GRPSKY", itob (DP_GROUPSKY(dao)))
	call tbhadb (tp, "FITSKY", itob (DP_FITSKY(dao)))
	call tbhadr (tp, "PSFMAG", DP_PSFMAG(psffit))
	call tbhadr (tp, "PSFRAD", DP_SPSFRAD(dao))
	call tbhadr (tp, "FITRAD", DP_SFITRAD(dao))
	call tbhadr (tp, "ANNULUS", DP_SANNULUS(dao))
	call tbhadr (tp, "DANNULUS", DP_SDANNULUS(dao))
	call tbhadi (tp, "MAXITER", DP_MAXITER(dao))
	call tbhadi (tp, "MAXGROUP", DP_MAXGROUP(dao))
	#call tbhadi (tp, "MAXNSTAR", DP_MAXNSTAR(dao))
	call tbhadr (tp, "FLATERROR", DP_FLATERR(dao))
	call tbhadr (tp, "PROFERROR", DP_PROFERR(dao))
	call tbhadi (tp, "CLIPEXP", DP_CLIPEXP(dao))
	call tbhadr (tp, "CLIPRANGE", DP_CLIPRANGE(dao))
	call tbhadr (tp, "MERGERAD", DP_SMERGERAD(dao))

	call sfree(sp)
end


# DP_XALPARS -- Add various parameters to the header of the PEAK table

procedure dp_xalpars (dao, tp)

pointer	dao			# pointer to the daophot structure
int	tp			# the output file descriptor

pointer	sp, psffit, outstr, date, time, dummy
bool	itob()
int	envfind()

begin
	# Define some daophot pointers.
	psffit = DP_PSFFIT(dao)

	# Allocate workin space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (dummy, 2, TY_CHAR)
	Memc[dummy] = EOS

	# Write the id.
	if (envfind ("version", Memc[outstr], SZ_LINE) <= 0)
	    call strcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IRAF", Memc[outstr], "version", Memc[dummy])
	if (envfind ("userid", Memc[outstr], SZ_LINE) <= 0)
	    call dp_sparam (tp, "USER", Memc[outstr], "name", Memc[dummy])
	call gethost (Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "HOST", Memc[outstr], "computer", Memc[dummy])
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call dp_sparam (tp, "DATE", Memc[date], "yyyy-mm-dd", Memc[dummy])
	call dp_sparam (tp, "TIME", Memc[time], "hh:mm:ss", Memc[dummy])
	call dp_sparam (tp, "PACKAGE", "daophot", "name", Memc[dummy])
	call dp_sparam (tp, "TASK", "allstar", "name", Memc[dummy])

	# Write out the files names.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IMAGE", Memc[outstr], "imagename", Memc[dummy])
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PHOTFILE", Memc[outstr], "filename", Memc[dummy])
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PSFIMAGE", Memc[outstr], "imagename", Memc[dummy])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "ALLSTARFILE", Memc[outstr], "filename",
	    Memc[dummy])
	if (DP_OUTREJFILE(dao) == EOS)
	    call dp_sparam (tp, "REJFILE", "\"\"", "filename", Memc[dummy])
	else {
	    call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_LINE)
	    call dp_sparam (tp, "REJFILE", Memc[outstr], "filename",
	        Memc[dummy])
	}
	call dp_imroot (DP_OUTIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "SUBIMAGE", Memc[outstr], "imagename",
	    Memc[dummy])

	# Define the data characteristics.
	call dp_rparam (tp, "SCALE", DP_SCALE(dao), "units/pix", Memc[dummy])
	call dp_rparam (tp, "DATAMIN", DP_MINGDATA(dao), "counts", Memc[dummy])
	call dp_rparam (tp, "DATAMAX", DP_MAXGDATA(dao), "counts", Memc[dummy])
	call dp_rparam (tp, "GAIN", DP_PHOTADU(dao), "e-/adu", Memc[dummy])
	call dp_rparam (tp, "READNOISE", DP_READNOISE(dao), "electrons",
	    Memc[dummy])

	# Define the observing parameters.
	call dp_sparam (tp, "OTIME", DP_OTIME(dao), "timeunit", Memc[dummy])
	call dp_rparam (tp, "XAIRMASS", DP_XAIRMASS(dao), "number", Memc[dummy])
	call dp_sparam (tp, "IFILTER", DP_IFILTER(dao), "filter", Memc[dummy])

	# Define the fitting parameters.
	call dp_bparam (tp, "RECENTER", itob (DP_RECENTER(dao)), "switch",
	    Memc[dummy])
	call dp_bparam (tp, "GRPSKY", itob (DP_GROUPSKY(dao)), "switch",
	    Memc[dummy])
	call dp_bparam (tp, "FITSKY", itob (DP_FITSKY(dao)), "switch",
	    Memc[dummy])
	call dp_rparam (tp, "PSFMAG", DP_PSFMAG(psffit), "magnitude",
	    Memc[dummy])
	call dp_rparam (tp, "PSFRAD", DP_SPSFRAD(dao), "scaleunit", Memc[dummy])
	call dp_rparam (tp, "FITRAD", DP_SFITRAD(dao), "scaleunit", Memc[dummy])
	call dp_rparam (tp, "ANNULUS", DP_SANNULUS(dao), "scaleunit",
	    Memc[dummy])
	call dp_rparam (tp, "DANNULUS", DP_SDANNULUS(dao), "scaleunit",
	    Memc[dummy])
	call dp_iparam (tp, "MAXITER", DP_MAXITER(dao), "number", Memc[dummy])
	call dp_iparam (tp, "MAXGROUP", DP_MAXGROUP(dao), "number", Memc[dummy])
	#call dp_iparam (tp, "MAXNSTAR", DP_MAXNSTAR(dao), "number",
	    #Memc[dummy])
	call dp_rparam (tp, "FLATERROR", DP_FLATERR(dao), "percentage",
	    Memc[dummy])
	call dp_rparam (tp, "PROFERROR", DP_PROFERR(dao), "percentage",
	    Memc[dummy])
	call dp_iparam (tp, "CLIPEXP", DP_CLIPEXP(dao), "number", Memc[dummy])
	call dp_rparam (tp, "CLIPRANGE", DP_CLIPRANGE(dao), "sigma",
	    Memc[dummy])
	call dp_rparam (tp, "MERGERAD", DP_SMERGERAD(dao), "scaleunit",
	    Memc[dummy])

	call sfree (sp)
end


# DP_TALWRITE -- Write out a photometry record to an ALLSTAR ST table.

procedure dp_talwrite (tpout, tprout, colpoint, id, x, y, mag, magerr, sky,
	chi, numer, denom, skip, aier, niter, istar, lstar, star, rstar,
	psfmag, csharp)

pointer	tpout		# the output photometry file descriptor
pointer	tprout		# the output rejections file descriptor
int	colpoint[ARB]	# array of column pointers
int	id[ARB]		# array of ids
real	x[ARB]		# array of xpositions
real	y[ARB]		# array of y positions
real	mag[ARB]	# array of magnitude values
real	magerr[ARB]	# array of mangitudes
real	sky[ARB]	# array of sky values
real	chi[ARB]	# array of chi values
real	numer[ARB]	# array of first numerator values
real	denom[ARB]	# array of first denominator values
int	skip[ARB]	# array of skipped stars
int	aier[ARB]	# array of error codes
int	niter		# number of iterations
int	istar, lstar	# first and last stars
int	star		# photometry file row number
int	rstar		# rejections file row number
real	psfmag		# magnitude of the psf
real	csharp		# the sharpness constant

int	i, iter, pier, plen
pointer	sp, perror
real	err, sharp
int	dp_gallerr()

begin
	call smark (sp)
	call salloc (perror, SZ_FNAME, TY_CHAR)

	do i = istar, lstar {

	    # Skip the star ?
	    if (skip[i] == NO)
		next
	    if (IS_INDEFR(x[i]) || IS_INDEFR(y[i]))
		next

	    # Get the results.
	    if (IS_INDEFR(mag[i]) || mag[i] <= 0.0 || denom[i] == 0.) {
		mag[i] = INDEFR
		iter = 0
		err = INDEFR
		sharp = INDEFR
		chi[i] = INDEFR
	    } else {
		if (magerr[i] <= 0.0)
		    err = 0.0
		else
	            err = 1.085736 * magerr[i] / mag[i]
		sharp = csharp * numer[i] / (mag[i] * denom[i])
		sharp = max (MIN_SHARP, min (sharp, MAX_SHARP))
	        mag[i] = psfmag - 2.5 * log10 (mag[i])
		iter = niter
	    }
	    pier = aier[i]
	    plen = dp_gallerr (pier, Memc[perror], SZ_FNAME)

	    # Write the output row.
	    if ((tprout != NULL) && (pier != ALLERR_OK)) {
	        rstar = rstar + 1
	        call tbrpti (tprout, colpoint[1], id[i], 1, rstar)
	        call tbrptr (tprout, colpoint[2], x[i], 1, rstar)
	        call tbrptr (tprout, colpoint[3], y[i], 1, rstar)
	        call tbrptr (tprout, colpoint[4], mag[i], 1, rstar)
	        call tbrptr (tprout, colpoint[5], err, 1, rstar)
	        call tbrptr (tprout, colpoint[6], sky[i], 1, rstar)
	        call tbrpti (tprout, colpoint[7], iter, 1, rstar)
	        call tbrptr (tprout, colpoint[8], sharp, 1, rstar)
	        call tbrptr (tprout, colpoint[9], chi[i], 1, rstar)
	        call tbrpti (tprout, colpoint[10], pier, 1, rstar)
		call tbrptt (tprout, colpoint[11], Memc[perror], plen,
		    1, rstar)
	    } else {
	        star = star + 1
	        call tbrpti (tpout, colpoint[1], id[i], 1, star)
	        call tbrptr (tpout, colpoint[2], x[i], 1, star)
	        call tbrptr (tpout, colpoint[3], y[i], 1, star)
	        call tbrptr (tpout, colpoint[4], mag[i], 1, star)
	        call tbrptr (tpout, colpoint[5], err, 1, star)
	        call tbrptr (tpout, colpoint[6], sky[i], 1, star)
	        call tbrpti (tpout, colpoint[7], iter, 1, star)
	        call tbrptr (tpout, colpoint[8], sharp, 1, star)
	        call tbrptr (tpout, colpoint[9], chi[i], 1, star)
	        call tbrpti (tpout, colpoint[10], pier, 1, star)
		call tbrptt (tpout, colpoint[11], Memc[perror], plen,
		    1, star)
	    }
	}

	call sfree (sp)
end


define AL_DATA1STR "%-9d%10t%-10.3f%20t%-10.3f%30t%-12.3f%42t%-14.3f%56t%-15.7g%71t%-6d%80t\\\n"
define AL_DATA2STR "%12t%-12.3f%24t%-12.3f%36t%-6d%42t%-13.13s%80t \n"

# DP_XALWRITE -- Write out photometry record to an ALLSTAR text file.

procedure dp_xalwrite (tpout, tprout, id, x, y, mag, magerr, sky, chi, numer,
	denom, skip, aier, niter, istar, lstar, psfmag, csharp)

int	tpout		# the output photometry file descriptor
int	tprout		# the output rejections file descriptor
int	id[ARB]		# array of ids
real	x[ARB]		# array of xpositions
real	y[ARB]		# array of y positions
real	mag[ARB]	# array of magnitude values
real	magerr[ARB]	# array of magnitudes
real	sky[ARB]	# array of sky values
real	chi[ARB]	# array of chi values
real	numer[ARB]	# array of first numerator values
real	denom[ARB]	# array of first denominator values
int	skip[ARB]	# array of skipped stars
int	aier[ARB]	# array of error codes
int	niter		# number of iterations
int	istar, lstar	# first and last stars
real	psfmag		# magnitude of the psf
real	csharp		# sharpness constant

int	i, iter, pier, plen
pointer	sp, perror
real	err, sharp
int	dp_gallerr()

begin
	call smark (sp)
	call salloc (perror, SZ_FNAME, TY_CHAR)

	do i = istar, lstar {

	    # Skip the star ?
	    if (skip[i] == NO)
		next
	    if (IS_INDEFR(x[i]) || IS_INDEFR(y[i]))
		next

	    if (IS_INDEFR(mag[i]) || mag[i] <= 0.0 || denom[i] == 0.) {
		mag[i] = INDEFR
		iter = 0
		err = INDEFR
		sharp = INDEFR
		chi[i] = INDEFR
	    } else {
		if (magerr[i] <= 0.0)
		    err = 0.0
		else
	            err = 1.085736 * magerr[i] / mag[i]
		sharp = csharp * numer[i] / (mag[i] * denom[i])
		sharp = max (MIN_SHARP, min (sharp, MAX_SHARP))
	        mag[i] = psfmag - 2.5 * log10 (mag[i])
	        iter = niter
	    }
	    pier = aier[i]
	    plen = dp_gallerr (pier, Memc[perror], SZ_FNAME)

	    if ((tprout != NULL) && (pier != ALLERR_OK)) {
	        call fprintf (tprout, AL_DATA1STR)
	            call pargi (id[i])
	            call pargr (x[i])
	            call pargr (y[i])
	            call pargr (mag[i])
	            call pargr (err)
	            call pargr (sky[i])
	            call pargi (iter)
	        call fprintf (tprout, AL_DATA2STR)
	            call pargr (sharp)
	            call pargr (chi[i])
		    call pargi (pier)
		    call pargstr (Memc[perror])
	    } else {
	        call fprintf (tpout, AL_DATA1STR)
	            call pargi (id[i])
	            call pargr (x[i])
	            call pargr (y[i])
	            call pargr (mag[i])
	            call pargr (err)
	            call pargr (sky[i])
	            call pargi (iter)
	        call fprintf (tpout, AL_DATA2STR)
	            call pargr (sharp)
	            call pargr (chi[i])
		    call pargi (pier)
		    call pargstr (Memc[perror])
	    }
	}

	call sfree (sp)
end


# DP_GALLERR -- Set the ALLSTAR task error code string.
 
int procedure dp_gallerr (ier, perror, maxch)
 
int     ier             # the integer error code
char    perror          # the output error code string
int     maxch           # the maximum size of the error code string
 
int     plen
int     gstrcpy()
 
begin
        switch (ier) {
        case ALLERR_OK:
            plen = gstrcpy ("No_error", perror, maxch)
	case ALLERR_BIGGROUP:
            plen = gstrcpy ("Big_group", perror, maxch)
        case ALLERR_INDEFSKY:
            plen = gstrcpy ("Bad_sky", perror, maxch)
        case ALLERR_NOPIX:
            plen = gstrcpy ("Npix_too_few", perror, maxch)
        case ALLERR_SINGULAR:
            plen = gstrcpy ("Singular", perror, maxch)
        case ALLERR_FAINT:
            plen = gstrcpy ("Too_faint", perror, maxch)
        case ALLERR_MERGE:
            plen = gstrcpy ("Merged", perror, maxch)
        case ALLERR_OFFIMAGE:
            plen = gstrcpy ("Off_image", perror, maxch)
        default:
            plen = gstrcpy ("No_error", perror, maxch)
        }
 
        return (plen)
end
