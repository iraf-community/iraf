include <tbset.h>
include <time.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/nstardef.h"


# DP_TPNEWNSTAR -- Create a new NSTAR output ST table.

procedure dp_tpnewnstar (dao, nst, colpoint)

pointer	dao			# pointer to the daophot structure
pointer	nst			# pointer to output photometry file
pointer	colpoint[ARB]		# array  of column pointers


int	i
pointer	sp, colnames, colunits, colformat, col_dtype, col_len

begin
	# Allocate space for table definition.
	call smark (sp)
	call salloc (colnames, NST_NOUTCOL * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, NST_NOUTCOL * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, NST_NOUTCOL * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (col_dtype, NST_NOUTCOL, TY_INT)
	call salloc (col_len, NST_NOUTCOL, TY_INT)

	# Set up the column definitions.
	call strcpy (ID, Memc[colnames], SZ_COLNAME)
	call strcpy (GROUP, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+4*SZ_COLNAME+4], SZ_COLNAME)
	call strcpy (MAGERR, Memc[colnames+5*SZ_COLNAME+5], SZ_COLNAME)
	call strcpy (SKY, Memc[colnames+6*SZ_COLNAME+6], SZ_COLNAME)
	call strcpy (NITER, Memc[colnames+7*SZ_COLNAME+7], SZ_COLNAME)
	call strcpy (SHARP, Memc[colnames+8*SZ_COLNAME+8], SZ_COLNAME)
	call strcpy (CHI, Memc[colnames+9*SZ_COLNAME+9], SZ_COLNAME)
	call strcpy (PIER, Memc[colnames+10*SZ_COLNAME+10], SZ_COLNAME)
	call strcpy (PERROR, Memc[colnames+11*SZ_COLNAME+11], SZ_COLNAME)

	# Se up the column format definitions.
	call strcpy ("%6d", Memc[colformat], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+4*SZ_COLFMT+4], SZ_COLFMT)
	call strcpy ("%14.3f", Memc[colformat+5*SZ_COLFMT+5], SZ_COLFMT)
	call strcpy ("%15.7g", Memc[colformat+6*SZ_COLFMT + 6], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+7*SZ_COLFMT+7], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+8*SZ_COLFMT+8], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+9*SZ_COLFMT+9], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+10*SZ_COLFMT+10], SZ_COLFMT)
	call strcpy ("%13s", Memc[colformat+11*SZ_COLFMT+11], SZ_COLFMT)

	# Set up the column unit definitions.
	call strcpy ("NUMBER", Memc[colunits], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+SZ_COLUNITS+1], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+2*SZ_COLUNITS+2], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+3*SZ_COLUNITS+3], SZ_COLUNITS)
	call strcpy ("MAGNITIDES", Memc[colunits+4*SZ_COLUNITS+4], SZ_COLUNITS)
	call strcpy ("MAGNITIDES", Memc[colunits+5*SZ_COLUNITS+5], SZ_COLUNITS)
	call strcpy ("COUNTS", Memc[colunits+6*SZ_COLUNITS+6], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+7*SZ_COLUNITS+7], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+8*SZ_COLUNITS+8], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+9*SZ_COLUNITS+9], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+10*SZ_COLUNITS+10], SZ_COLUNITS)
	call strcpy ("PERRORS", Memc[colunits+11*SZ_COLUNITS+11], SZ_COLUNITS)

	# Define the column datatypes.
	Memi[col_dtype] = TY_INT
	Memi[col_dtype+1] = TY_INT
	Memi[col_dtype+2] = TY_REAL
	Memi[col_dtype+3] = TY_REAL
	Memi[col_dtype+4] = TY_REAL
	Memi[col_dtype+5] = TY_REAL
	Memi[col_dtype+6] = TY_REAL
	Memi[col_dtype+7] = TY_INT
	Memi[col_dtype+8] = TY_REAL
	Memi[col_dtype+9] = TY_REAL
	Memi[col_dtype+10] = TY_INT
	Memi[col_dtype+11] = -13

	# Define columnlengths.
	do i = 1, NST_NOUTCOL 
	    Memi[col_len+i-1] = 1
	
	# Define and create the table.
	call tbcdef (nst, colpoint, Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[col_dtype], Memi[col_len], NST_NOUTCOL)
	call tbtcre (nst)

	# Write out some header parameters.
	call dp_tnstarpars (dao, nst)

	call sfree (sp)
	
end


define NST_NAME1STR "#N%4tID%10tGROUP%16tXCENTER%26tYCENTER%36tMAG%48t\
MERR%62tMSKY%80t\\\n"
define NST_UNIT1STR "#U%4t##%10t##%16tpixels%26tpixels%36tmagnitudes%48t\
magnitudes%62tcounts%80t\\\n"
define NST_FORMAT1STR "#F%4t%%-9d%10t%%-6d%16t%%-10.3f%26t%%-10.3f%36t\
%%-12.3f%48t%%-14.3f%62t%%-15.7g%80t \n"

define NST_NAME2STR "#N%12tNITER%18tSHARPNESS%30tCHI%42tPIER%48tPERROR%80t\\\n"
define NST_UNIT2STR "#U%12t##%18t##%30t##%42t##%48tperrors%80t\\\n"
define NST_FORMAT2STR "#F%12t%%-17d%18t%%-12.3f%30t%%-12.3f%42t%%-6d\
%48t%%-13s%80t \n"

# DP_XPNEWNSTAR -- Create a new NSTAR output text file.

procedure dp_xpnewnstar (dao, nst)

pointer	dao			# pointer to the daophot structure
int	nst			# the output file descriptor


begin
	# Write out some header parameters.
	call dp_xnstarpars (dao, nst)

	# Write out the header banner.
	call fprintf (nst, "#\n")
	call fprintf (nst, NST_NAME1STR)
	call fprintf (nst, NST_UNIT1STR)
	call fprintf (nst, NST_FORMAT1STR)
	call fprintf (nst, "#\n")
	call fprintf (nst, NST_NAME2STR)
	call fprintf (nst, NST_UNIT2STR)
	call fprintf (nst, NST_FORMAT2STR)
	call fprintf (nst, "#\n")
end


# DP_XNSTARPARS -- Add parameters to the header of the output NSTAR text file.

procedure dp_xnstarpars (dao, nst)

pointer	dao			# pointer to the daophot structure
int	nst			# the output file descriptor

pointer	psffit, sp, outstr, date, time, comment
bool	itob()
int	envfind()

begin
	# Get pointers
	psffit = DP_PSFFIT(dao)

	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)
	Memc[comment] = EOS

	# Write the id.
	if (envfind ("version", Memc[outstr], SZ_LINE) <=0)
	    call strcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call dp_sparam (nst, "IRAF", Memc[outstr], "version", Memc[comment])
	if (envfind ("userid", Memc[outstr], SZ_LINE) > 0)
	    call dp_sparam (nst, "USER", Memc[outstr], "name", Memc[comment])
	call gethost (Memc[outstr], SZ_LINE)
	call dp_sparam (nst, "HOST", Memc[outstr], "computer", Memc[comment])
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call dp_sparam (nst, "DATE", Memc[date], "yyyy-mm-dd", Memc[comment])
	call dp_sparam (nst, "TIME", Memc[time], "hh:mm:ss", Memc[comment])
	call dp_sparam (nst, "PACKAGE", "daophot", "name", Memc[comment])
	call dp_sparam (nst, "TASK", "nstar", "name", Memc[comment])

	# Write out the file names.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_FNAME)
	call dp_sparam (nst, "IMAGE", Memc[outstr], "imagename",
	    Memc[comment])
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_FNAME)
	call dp_sparam (nst, "GRPFILE", Memc[outstr], "filename", Memc[comment])
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_FNAME)
	call dp_sparam (nst, "PSFIMAGE", Memc[outstr], "imagename",
	    Memc[comment])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_FNAME)
	call dp_sparam (nst, "NSTARFILE", Memc[outstr], "filename",
	    Memc[comment])
	if (DP_OUTREJFILE(dao) == EOS)
	    call dp_sparam (nst, "REJFILE", "\"\"", "filename", Memc[comment])
	else {
	    call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_FNAME)
	    call dp_sparam (nst, "REJFILE", Memc[outstr], "filename",
	        Memc[comment])
	}

	# Write out the data dependent parameters.
	call dp_rparam (nst, "SCALE", DP_SCALE(dao), "units/pix",
	    Memc[comment])
	call dp_rparam (nst, "DATAMIN", DP_MINGDATA(dao), "counts",
	    Memc[comment])
	call dp_rparam (nst, "DATAMAX", DP_MAXGDATA(dao), "counts",
	    Memc[comment])
	call dp_rparam (nst, "GAIN", DP_PHOTADU(dao), "number", Memc[comment])
	call dp_rparam (nst, "READNOISE", DP_READNOISE(dao), "electrons",
	    Memc[comment])

	# Write out the observing parameters.
	call dp_sparam (nst, "OTIME", DP_OTIME(dao), "timeunit", Memc[comment])
	call dp_rparam (nst, "XAIRMASS", DP_XAIRMASS(dao), "number",
	    Memc[comment])
	call dp_sparam (nst, "IFILTER", DP_IFILTER(dao), "filter",
	    Memc[comment])

	# Write out the fitting parameters.
	call dp_bparam (nst, "RECENTER", itob (DP_RECENTER(dao)), "switch",
	    Memc[comment])
	call dp_bparam (nst, "FITSKY", itob (DP_FITSKY(dao)), "switch",
	    Memc[comment])
	call dp_bparam (nst, "GRPSKY", itob (DP_GROUPSKY(dao)), "switch",
	    Memc[comment])
	call dp_rparam (nst, "PSFMAG", DP_PSFMAG(psffit), "magnitude",
	    Memc[comment])
	call dp_rparam (nst, "PSFRAD", DP_SPSFRAD(dao), "scaleunit",
	    Memc[comment])
	call dp_rparam (nst, "FITRAD", DP_SFITRAD(dao), "scaleunit",
	    Memc[comment])
	call dp_iparam (nst, "MAXITER", DP_MAXITER(dao), "number",
	    Memc[comment])
	call dp_iparam (nst, "MAXGROUP", DP_MAXGROUP(dao), "number",
	    Memc[comment])
	call dp_rparam (nst, "FLATERROR", DP_FLATERR(dao), "percentage",
	    Memc[comment])
	call dp_rparam (nst, "PROFERROR", DP_PROFERR(dao), "percentage",
	    Memc[comment])
	call dp_iparam (nst, "CLIPEXP", DP_CLIPEXP(dao), "number",
	    Memc[comment])
	call dp_rparam (nst, "CLIPRANGE", DP_CLIPRANGE(dao), "sigma",
	    Memc[comment])
	call dp_rparam (nst, "MERGERAD", DP_SMERGERAD(dao), "scaleunit",
	    Memc[comment])

	call sfree(sp)
end


# DP_TNSTARPARS -- Add parameters to the header of the output NSTAR ST table.

procedure dp_tnstarpars (dao, nst)

pointer	dao			# pointer to the daophot structure
pointer	nst			# pointer to the output photometry table

pointer	psffit, sp, outstr, date, time
bool	itob()
int	envfind()

begin
	# Get pointers
	psffit = DP_PSFFIT(dao)

	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	# Write the id.
	if (envfind ("version", Memc[outstr], SZ_LINE) <=0)
	    call strcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call tbhadt (nst, "IRAF", Memc[outstr])
	if (envfind ("userid", Memc[outstr], SZ_LINE) > 0)
	    call tbhadt (nst, "USER", Memc[outstr])
	call gethost (Memc[outstr], SZ_LINE)
	call tbhadt (nst, "HOST", Memc[outstr])
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call tbhadt (nst, "DATE", Memc[date])
	call tbhadt (nst, "TIME", Memc[time])
	call tbhadt (nst, "PACKAGE", "daophot")
	call tbhadt (nst, "TASK", "nstar")

	# Write out the file names.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_FNAME)
	call tbhadt (nst, "IMAGE", Memc[outstr])
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_FNAME)
	call tbhadt (nst, "GRPFILE", Memc[outstr])
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_FNAME)
	call tbhadt (nst, "PSFIMAGE", Memc[outstr])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_FNAME)
	call tbhadt (nst, "NSTARFILE", Memc[outstr])
	if (DP_OUTREJFILE(dao) == EOS)
	    call tbhadt (nst, "REJFILE", "\"\"")
	else {
	    call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_FNAME)
	    call tbhadt (nst, "REJFILE", Memc[outstr])
	}

	# Write out the data dependent parameters.
	call tbhadr (nst, "SCALE", DP_SCALE(dao))
	call tbhadr (nst, "DATAMIN", DP_MINGDATA(dao))
	call tbhadr (nst, "DATAMAX", DP_MAXGDATA(dao))
	call tbhadr (nst, "GAIN", DP_PHOTADU(dao))
	call tbhadr (nst, "READNOISE", DP_READNOISE(dao))

	# Write out the observing parameters.
	call tbhadt (nst, "OTIME", DP_OTIME(dao))
	call tbhadr (nst, "XAIRMASS", DP_XAIRMASS(dao))
	call tbhadt (nst, "IFILTER", DP_IFILTER(dao))

	# Write out the fitting parameters.
	call tbhadb (nst, "RECENTER", itob (DP_RECENTER(dao)))
	call tbhadb (nst, "FITSKY", itob (DP_FITSKY(dao)))
	call tbhadb (nst, "GRPSKY", itob (DP_GROUPSKY(dao)))
	call tbhadr (nst, "PSFMAG", DP_PSFMAG(psffit))
	call tbhadr (nst, "PSFRAD", DP_SPSFRAD(dao))
	call tbhadr (nst, "FITRAD", DP_SFITRAD(dao))
	call tbhadi (nst, "MAXITER", DP_MAXITER(dao))
	call tbhadi (nst, "MAXGROUP", DP_MAXGROUP(dao))
	call tbhadr (nst, "FLATERROR", DP_FLATERR(dao))
	call tbhadr (nst, "PROFERROR", DP_PROFERR(dao))
	call tbhadi (nst, "CLIPEXP", DP_CLIPEXP(dao))
	call tbhadr (nst, "CLIPRANGE", DP_CLIPRANGE(dao))
	call tbhadr (nst, "MERGERAD", DP_SMERGERAD(dao))

	call sfree(sp)
end


# DP_TNTWRITE -- Write out the NSTAR results to an ST table.

procedure dp_tntwrite (dao, im, nst, rej, niter, old_size, output_row,
	routput_row, colpoint) 

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
pointer	nst			# output photometry file descriptor
int	rej			# output rejections file descriptor
int	niter			# number of iterations
int	old_size		# original size of group
int	output_row		# output photometry file row number
int	routput_row		# output rejections file row number
pointer	colpoint[ARB]		# column pointer array

int	i, id, nkeep, nreject, pier, plen, iter
pointer	psffit, nstar, apsel, sp, perror
real	xcen, ycen, mag, errmag, sharp
int	dp_gnsterr()

begin
	# Get some daophot pointers.
	nstar = DP_NSTAR(dao)
	apsel = DP_APSEL(dao)
	psffit = DP_PSFFIT(dao)

	# Fill in the INDEFS.
	nkeep = DP_NNUM(nstar)
	nreject = old_size - nkeep
	if (nreject  > 0) {
	    call amovkr (INDEFR, Memr[DP_APMAG(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APERR(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APCHI(apsel)+nkeep], nreject)
	}

	call smark (sp)
	call salloc (perror, SZ_FNAME, TY_CHAR)

	# Now write out the results.
	do i = 1, old_size {

	    # Get the results.
	    id = Memi[DP_APID (apsel)+i-1]
	    xcen = Memr[DP_APXCEN (apsel)+i-1]
	    ycen = Memr[DP_APYCEN (apsel)+i-1]
	    if (IS_INDEFR(xcen) || IS_INDEFR(ycen))
		next
	    call dp_wout (dao, im, xcen, ycen, xcen, ycen, 1)
	    mag = Memr[DP_APMAG(apsel)+i-1]
	    errmag = Memr[DP_APERR(apsel)+i-1]
	    if (! IS_INDEFR(mag)) {
		if (! IS_INDEFR(errmag))
	    	    errmag = 1.085736 * errmag / mag
	        sharp = 1.4427 * Memr[DP_PSFPARS(psffit)] *
		    Memr[DP_PSFPARS(psffit)+1] * Memr[DP_NNUMER(nstar)+i-1] /
		    (mag * DP_PSFHEIGHT(psffit) * Memr[DP_NDENOM(nstar)+i-1])
	        if ((sharp < MIN_SHARP) || (sharp > MAX_SHARP))
		    sharp = INDEFR
	        mag = DP_PSFMAG (psffit) - 2.5 * log10 (mag)
	    } else
		sharp = INDEFR
	    pier = Memi[DP_NIER(nstar)+i-1]
	    if (pier == NSTERR_OK)
		iter = niter
	    else
		iter = 0
	    plen = dp_gnsterr (pier, Memc[perror], SZ_FNAME)

	    # Write the results to the standard output.
	    if (DP_VERBOSE (dao) == YES) {
		call printf (
		"\tID: %5d  XCEN: %8.2f  YCEN: %8.2f  MAG: %8.2f\n")
		    call pargi (id)
		    call pargr (xcen)
		    call pargr (ycen)
		    call pargr (mag)
	    }

	    # Write the output row to the proper table.
	    if ((rej != NULL) && (pier != NSTERR_OK)) {
	        routput_row = routput_row + 1
	        call tbrpti (rej, colpoint[1], id, 1, routput_row)
	        call tbrpti (rej, colpoint[2], DP_NGNUM(nstar), 1, routput_row)
	        call tbrptr (rej, colpoint[3], xcen, 1, routput_row)
	        call tbrptr (rej, colpoint[4], ycen, 1, routput_row)
	        call tbrptr (rej, colpoint[5], mag, 1, routput_row)
	        call tbrptr (rej, colpoint[6], errmag, 1, routput_row)
	        call tbrptr (rej, colpoint[7], Memr[DP_APMSKY(apsel)+i-1],
		    1, routput_row)
	        call tbrpti (rej, colpoint[8], iter, 1, routput_row)
	        call tbrptr (rej, colpoint[9], sharp, 1, routput_row)
	        call tbrptr (rej, colpoint[10], Memr[DP_APCHI(apsel)+i-1],
		    1, routput_row)
	        call tbrpti (rej, colpoint[11], pier, 1, routput_row)
		call tbrptt (rej, colpoint[12], Memc[perror], plen, 1,
		    routput_row)
	    } else {
	        output_row = output_row + 1
	        call tbrpti (nst, colpoint[1], id, 1, output_row)
	        call tbrpti (nst, colpoint[2], DP_NGNUM(nstar), 1, output_row)
	        call tbrptr (nst, colpoint[3], xcen, 1, output_row)
	        call tbrptr (nst, colpoint[4], ycen, 1, output_row)
	        call tbrptr (nst, colpoint[5], mag, 1, output_row)
	        call tbrptr (nst, colpoint[6], errmag, 1, output_row)
	        call tbrptr (nst, colpoint[7], Memr[DP_APMSKY(apsel)+i-1],
		    1, output_row)
	        call tbrpti (nst, colpoint[8], iter, 1, output_row)
	        call tbrptr (nst, colpoint[9], sharp, 1, output_row)
	        call tbrptr (nst, colpoint[10], Memr[DP_APCHI(apsel)+i-1],
		    1, output_row)
	        call tbrpti (nst, colpoint[11], pier, 1, output_row)
		call tbrptt (nst, colpoint[12], Memc[perror], plen, 1,
		    output_row)
	    }
	}

	call sfree (sp)
end


define NST_DATA1STR "%-9d%10t%-6d%16t%-10.3f%26t%-10.3f%36t%-12.3f%48t\
%-14.3f%62t%-15.7g%80t\\\n"
define NST_DATA2STR "%12t%-6d%18t%-12.3f%30t%-12.3f%42t%-6d%48t%-13.13s%80t \n"

# DP_XNTWRITE -- Write out the NSTAR results to a text file.

procedure dp_xntwrite (dao, im, nst, rej, niter, old_size) 

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
int	nst			# the output photometry file descriptor
int	rej			# the output rejections file descriptor
int	niter			# the number of the iteration
int	old_size		# old size of group

int	i, id, nkeep, nreject, pier, plen, iter
pointer	nstar, psffit, apsel, sp, perror
real	xcen, ycen, mag, errmag, sharp
int	dp_gnsterr()

begin
	# Get some daophot pointers.
	nstar = DP_NSTAR(dao)
	psffit = DP_PSFFIT(dao)
	apsel = DP_APSEL(dao)

	# Fill in the results for the rejected stars with INDEFS.
	nkeep = DP_NNUM(nstar)
	nreject = old_size - nkeep
	if (nreject  > 0) {
	    call amovkr (INDEFR, Memr[DP_APMAG(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APERR(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APCHI(apsel)+nkeep], nreject)
	}

	call smark (sp)
	call salloc (perror, SZ_FNAME, TY_CHAR)

	# Now write out the results.
	do i = 1, old_size {

	    # Get the results.
	    id = Memi[DP_APID (apsel)+i-1]
	    xcen = Memr[DP_APXCEN (apsel)+i-1]
	    ycen = Memr[DP_APYCEN (apsel)+i-1]
	    if (IS_INDEFR(xcen) || IS_INDEFR(ycen))
		next
	    call dp_wout (dao, im, xcen, ycen, xcen, ycen, 1)
	    mag = Memr[DP_APMAG(apsel)+i-1]
	    errmag = Memr[DP_APERR(apsel)+i-1]
	    if (! IS_INDEFR(mag)) {
		if (! IS_INDEFR(errmag))
	            errmag = 1.085736 * errmag / mag
	        sharp = 1.4427 * Memr[DP_PSFPARS(psffit)] *
		    Memr[DP_PSFPARS(psffit)+1] * Memr[DP_NNUMER(nstar)+i-1] /
		    (mag * DP_PSFHEIGHT(psffit) * Memr[DP_NDENOM(nstar)+i-1])
	        if ((sharp < MIN_SHARP) || (sharp > MAX_SHARP))
		    sharp = INDEFR
	        mag = DP_PSFMAG (psffit) - 2.5 * log10 (mag)
	    } else
		sharp = INDEFR
	    pier = Memi[DP_NIER(nstar)+i-1]
	    if (pier == NSTERR_OK)
		iter = niter
	    else
		iter = 0
	    plen = dp_gnsterr (pier, Memc[perror], SZ_FNAME)

	    # Write the results to the standard output.
	    if (DP_VERBOSE (dao) == YES) {
		call printf (
		"\tID: %5d  XCEN: %8.2f  YCEN: %8.2f  MAG: %8.2f\n")
	            call pargi (id)
		    call pargr (xcen)
		    call pargr (ycen)
		    call pargr (mag)
	    }

	    # Write the results to the output file.
	    if ((rej != NULL) && (pier != NSTERR_OK)) {
	        call fprintf (rej, NST_DATA1STR)
		    call pargi (id)
		    call pargi (DP_NGNUM(nstar))
		    call pargr (xcen)
		    call pargr (ycen)
	            call pargr (mag)
		    call pargr (errmag)
	            call pargr (Memr[DP_APMSKY(apsel)+i-1])
	        call fprintf (rej, NST_DATA2STR)
		    call pargi (iter)
	            call pargr (sharp)
	            call pargr (Memr[DP_APCHI(apsel)+i-1])
		    call pargi (pier)
		    call pargstr (Memc[perror])
	    } else {
	        call fprintf (nst, NST_DATA1STR)
		    call pargi (id)
		    call pargi (DP_NGNUM(nstar))
		    call pargr (xcen)
		    call pargr (ycen)
	            call pargr (mag)
		    call pargr (errmag)
	            call pargr (Memr[DP_APMSKY(apsel)+i-1])
	        call fprintf (nst, NST_DATA2STR)
		    call pargi (iter)
	            call pargr (sharp)
	            call pargr (Memr[DP_APCHI(apsel)+i-1])
		    call pargi (pier)
		    call pargstr (Memc[perror])
	    }
	}

	call sfree (sp)
end


# DP_GNSTERR -- Set the NSTAR task error code string.
 
int procedure dp_gnsterr (ier, perror, maxch)
 
int     ier             # the integer error code
char    perror          # the output error code string
int     maxch           # the maximum size of the error code string
 
int     plen
int     gstrcpy()
 
begin
        switch (ier) {
        case NSTERR_OK:
            plen = gstrcpy ("No_error", perror, maxch)
	case NSTERR_BIGGROUP:
            plen = gstrcpy ("Big_group", perror, maxch)
        case NSTERR_INDEFSKY:
            plen = gstrcpy ("Bad_sky", perror, maxch)
        case NSTERR_NOPIX:
            plen = gstrcpy ("Npix_too_few", perror, maxch)
        case NSTERR_SINGULAR:
            plen = gstrcpy ("Singular", perror, maxch)
        case NSTERR_FAINT:
            plen = gstrcpy ("Too_faint", perror, maxch)
        case NSTERR_MERGE:
            plen = gstrcpy ("Merged", perror, maxch)
        case NSTERR_OFFIMAGE:
            plen = gstrcpy ("Off_image", perror, maxch)
        default:
            plen = gstrcpy ("No_error", perror, maxch)
        }
 
        return (plen)
end
