include <tbset.h>
include <time.h>
include "../lib/daophotdef.h"
include "../lib/nstardef.h"
include "../lib/apsel.h"
include	"../lib/daophot.h"


define	MIN_SHARP	-99.99
define	MAX_SHARP	99.99

define	NCOLUMN 	10

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
	call salloc (colnames, NCOLUMN * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, NCOLUMN * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, NCOLUMN * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (col_dtype, NCOLUMN, TY_INT)
	call salloc (col_len, NCOLUMN, TY_INT)

	# Set up the column definitions.
	call strcpy (ID, Memc[colnames], SZ_COLNAME)
	call strcpy (GROUP, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+4*SZ_COLNAME+4], SZ_COLNAME)
	call strcpy ("MERR", Memc[colnames+5*SZ_COLNAME+5], SZ_COLNAME)
	call strcpy (APSKY, Memc[colnames+6*SZ_COLNAME+6], SZ_COLNAME)
	call strcpy (NITER, Memc[colnames+7*SZ_COLNAME+7], SZ_COLNAME)
	call strcpy ("SHARPNESS", Memc[colnames+8*SZ_COLNAME+8], SZ_COLNAME)
	call strcpy (CHI, Memc[colnames+9*SZ_COLNAME+9], SZ_COLNAME)

	# Se up the column format definitions.
	call strcpy ("%6d", Memc[colformat], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("%10.2f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("%10.2f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+4*SZ_COLFMT+4], SZ_COLFMT)
	call strcpy ("%14.3f", Memc[colformat+5*SZ_COLFMT+5], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+6*SZ_COLFMT + 6], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+7*SZ_COLFMT+7], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+8*SZ_COLFMT+8], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+9*SZ_COLFMT+9], SZ_COLFMT)

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

	# Define columnlengths.
	do i = 1, NCOLUMN 
	    Memi[col_len+i-1] = 1
	
	# Define and create the table.
	call tbcdef (nst, colpoint, Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[col_dtype], Memi[col_len], NCOLUMN)
	call tbtcre (nst)

	# Write out some header parameters.
	call dp_tnstarpars (dao, nst)

	call sfree (sp)
	
end


define NST_NAME1STR "#N%4tID%10tGROUP%16tXCENTER%26tYCENTER%36tMAG%48t\
MERR%62tMSKY%80t\\\n"
define NST_UNIT1STR "#U%4t##%10t##%16tpixels%26tpixels%36tmagnitudes%48t\
magnitudes%62tcounts%80t\\\n"
define NST_FORMAT1STR "#F%4t%%-9d%10t%%-6d%16t%%-10.2f%26t%%-10.2f%36t\
%%-12.3f%48t%%-14.3f%62t%%-12.3f%80t \n"

define NST_NAME2STR "#N%12tNITER%18tSHARPNESS%30tCHI%80t\\\n"
define NST_UNIT2STR "#U%12t##%18t##%30t##%80t\\\n"
define NST_FORMAT2STR "#F%12t%%-17d%18t%%-12.3f%30t%%-12.3f%80t \n"

# DP_XPNEWNSTAR -- Create a new NSTAR output text file.

procedure dp_xpnewnstar (dao, nst)

pointer	dao			# pointer to the daophot structure
pointer	nst			# pointer to output photometry file


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
	call dp_sparam (nst, "IRAF", Memc[outstr], "version", "")
	if (envfind ("userid", Memc[outstr], SZ_LINE) > 0)
	    call dp_sparam (nst, "USER", Memc[outstr], "name", "")
	call gethost (Memc[outstr], SZ_LINE)
	call dp_sparam (nst, "HOST", Memc[outstr], "computer", "")
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call dp_sparam (nst, "DATE", Memc[date], "mm-dd-yy", "")
	call dp_sparam (nst, "TIME", Memc[time], "hh:mm:ss", "")
	call dp_sparam (nst, "PACKAGE", "daophot", "name", "")
	call dp_sparam (nst, "TASK", "nstar", "name", "")

	# Write out the file names.
	call dp_sparam (nst, "IMAGE", DP_IMNAME(dao), "imagename", "")
	call dp_sparam (nst, "GRPFILE", DP_GRPFILE(dao), "filename", "")
	call dp_sparam (nst, "PSFIMAGE", DP_PSFIMAGE(dao), "imagename", "")
	call dp_sparam (nst, "NSTARFILE", DP_NSTARFILE(dao), "filename", "")

	# Write out the data dependent parameters.
	call dp_rparam (nst, "SCALE", DP_SCALE(dao), "units/pix", "")
	call dp_rparam (nst, "DATAMIN", DP_MINGDATA(dao), "counts", "")
	call dp_rparam (nst, "DATAMAX", DP_MAXGDATA(dao), "counts", "")
	call dp_rparam (nst, "GAIN", DP_PHOT_ADC(dao), "number", "")
	call dp_rparam (nst, "READNOISE", DP_READ_NOISE(dao), "electrons", "")

	# Write out the observing parameters.
	call dp_sparam (nst, "OTIME", DP_OTIME(dao), "timeunit", "")
	call dp_rparam (nst, "XAIRMASS", DP_XAIRMASS(dao), "number", "")
	call dp_sparam (nst, "IFILTER", DP_IFILTER(dao), "filter", "")

	# Write out the fitting parameters.
	call dp_rparam (nst, "PSFMAG", DP_PSFMAG(psffit), "magnitude", "")
	call dp_rparam (nst, "PSFRAD", DP_SPSFRAD(dao), "scaleunit", "")
	call dp_rparam (nst, "FITRAD", DP_SFITRAD(dao), "scaleunit", "")
	call dp_bparam (nst, "VARPSF", itob (DP_VARPSF(dao)), "switch", "")
	call dp_iparam (nst, "MAXITER", DP_MAXITER(dao), "number", "")
	call dp_iparam (nst, "MAXGROUP", DP_MAXGROUP(dao), "number", "")

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
	call tbhadt (nst, "IMAGE", DP_IMNAME(dao))
	call tbhadt (nst, "GRPFILE", DP_GRPFILE(dao))
	call tbhadt (nst, "PSFIMAGE", DP_PSFIMAGE(dao))
	call tbhadt (nst, "NSTARFILE", DP_NSTARFILE(dao))

	# Write out the data dependent parameters.
	call tbhadr (nst, "SCALE", DP_SCALE(dao))
	call tbhadr (nst, "DATAMIN", DP_MINGDATA(dao))
	call tbhadr (nst, "DATAMAX", DP_MAXGDATA(dao))
	call tbhadr (nst, "GAIN", DP_PHOT_ADC(dao))
	call tbhadr (nst, "READNOISE", DP_READ_NOISE(dao))

	# Write out the observing parameters.
	call tbhadt (nst, "OTIME", DP_OTIME(dao))
	call tbhadr (nst, "XAIRMASS", DP_XAIRMASS(dao))
	call tbhadt (nst, "IFILTER", DP_IFILTER(dao))

	# Write out the fitting parameters.
	call tbhadr (nst, "PSFMAG", DP_PSFMAG(psffit))
	call tbhadr (nst, "PSFRAD", DP_SPSFRAD(dao))
	call tbhadr (nst, "FITRAD", DP_SFITRAD(dao))
	call tbhadb (nst, "VARPSF", itob (DP_VARPSF(dao)))
	call tbhadi (nst, "MAXITER", DP_MAXITER(dao))
	call tbhadi (nst, "MAXGROUP", DP_MAXGROUP(dao))

	call sfree(sp)
end


# DP_TNTWRITE -- Write out the NSTAR results to an ST table.

procedure dp_tntwrite (dao, nst, niter, old_size, output_row, colpoint) 

pointer	dao			# pointer to the daophot structure
pointer	nst			# pointer to nstar table
int	niter			# number of iterations
int	old_size		# original size of group
int	output_row		# output row number
pointer	colpoint[ARB]		# column pointer array

int	i, id, nkeep, nreject
pointer	psffit, nstar, apsel
real	xcen, ycen, mag, errmag, sky, sharp, chi

begin
	# Get some daophot pointers.
	nstar = DP_NSTAR(dao)
	apsel = DP_APSEL(dao)
	psffit = DP_PSFFIT(dao)

	# Fill in the INDEFS.
	nkeep = DP_NUMBSTAR(nstar)
	nreject = old_size - nkeep
	if (nreject  > 0) {
	    call amovkr (INDEFR, Memr[DP_APMAG(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APERR(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APCHI(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APSHARP(apsel)+nkeep], nreject)
	}

	# Now write out the results.
	do i = 1, old_size {

	    # Increment the output row numbers.
	    output_row = output_row + 1

	    # Get the results.
	    id = Memi[DP_APID (apsel)+i-1]
	    xcen = Memr[DP_APXCEN (apsel)+i-1]
	    ycen = Memr[DP_APYCEN (apsel)+i-1]
	    mag = Memr[DP_APMAG(apsel)+i-1]
	    sky = Memr[DP_APMSKY(apsel)+i-1]
	    errmag = Memr[DP_APERR(apsel)+i-1]
	    if (! IS_INDEFR(mag)) {
		if (! IS_INDEFR(errmag))
	    	    errmag = 1.086 * sqrt (errmag) / mag
	        mag = DP_PSFMAG (psffit) - 2.5 * log10 (mag)
	    }
	    sharp = Memr[DP_APSHARP(apsel)+i-1]
	    if ((sharp < MIN_SHARP) || (sharp > MAX_SHARP))
		sharp = INDEFR
	    chi = Memr[DP_APCHI(apsel)+i-1]

	    # Write the results to the standard output.
	    if (DP_VERBOSE (dao) == YES) {
		call printf (
		"\tID: %5d  XCEN: %8.2f  YCEN: %8.2f  MAG: %8.2f\n")
		    call pargi (id)
		    call pargr (xcen)
		    call pargr (ycen)
		    call pargr (mag)
	    }

	    # Eliminate objects with undefined centers.
	    if (IS_INDEFR(xcen) || IS_INDEFR(ycen))
		next

	    # Write out the columns to the output table.
	    call tbrpti (nst, colpoint[1], id, 1, output_row)
	    call tbrpti (nst, colpoint[2], DP_GROUPNUM (nstar), 1, output_row)
	    call tbrptr (nst, colpoint[3], xcen, 1, output_row)
	    call tbrptr (nst, colpoint[4], ycen, 1, output_row)
	    call tbrptr (nst, colpoint[5], mag, 1, output_row)
	    call tbrptr (nst, colpoint[6], errmag, 1, output_row)
	    call tbrptr (nst, colpoint[7], sky, 1, output_row)
	    call tbrpti (nst, colpoint[8], niter, 1, output_row)
	    call tbrptr (nst, colpoint[9], sharp, 1, output_row)
	    call tbrptr (nst, colpoint[10], chi, 1, output_row)
	}
end


define NST_DATA1STR "%-9d%10t%-6d%16t%-10.2f%26t%-10.2f%36t%-12.3f%48t%-14.3f%62t%-12.3f%80t\\\n"
define NST_DATA2STR "%12t%-6d%18t%-12.3f%30t%-12.3f%80t \n"

# DP_XNTWRITE -- Write out the NSTAR results to a text file.

procedure dp_xntwrite (dao, nst, niter, old_size) 

pointer	dao			# pointer to the daophot structure
pointer	nst			# pointer to nstar table
int	niter			# the number of the iteration
int	old_size		# old size of group

int	i, id, nkeep, nreject
pointer	nstar, psffit, apsel
real	xcen, ycen, mag, errmag, sharp

begin
	# Get some daophot pointers.
	nstar = DP_NSTAR(dao)
	psffit = DP_PSFFIT(dao)
	apsel = DP_APSEL(dao)

	# Fill in the INDEFS.
	nkeep = DP_NUMBSTAR(nstar)
	nreject = old_size - nkeep
	if (nreject  > 0) {
	    call amovkr (INDEFR, Memr[DP_APMAG(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APERR(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APCHI(apsel)+nkeep], nreject)
	    call amovkr (INDEFR, Memr[DP_APSHARP(apsel)+nkeep], nreject)
	}

	# Now write out the results.
	do i = 1, old_size {

	    # Get the results.
	    id = Memi[DP_APID (apsel)+i-1]
	    xcen = Memr[DP_APXCEN (apsel)+i-1]
	    ycen = Memr[DP_APYCEN (apsel)+i-1]
	    mag = Memr[DP_APMAG(apsel)+i-1]
	    errmag = Memr[DP_APERR(apsel)+i-1]
	    if (! IS_INDEFR(mag)) {
		if (! IS_INDEFR(errmag))
	            errmag = 1.086 * sqrt (errmag) / mag
	        mag = DP_PSFMAG (psffit) - 2.5 * log10 (mag)
	    }
	    sharp = Memr[DP_APSHARP(apsel)+i-1]
	    if ((sharp < MIN_SHARP) || (sharp > MAX_SHARP))
		sharp = INDEFR

	    # Write the results to the standard output.
	    if (DP_VERBOSE (dao) == YES) {
		call printf (
		"\tID: %5d  XCEN: %8.2f  YCEN: %8.2f  MAG: %8.2f\n")
	            call pargi (id)
		    call pargr (xcen)
		    call pargr (ycen)
		    call pargr (mag)
	    }

	    # Eliminate object with undefined centers from the output.
	    if (IS_INDEFR(xcen) || IS_INDEFR(ycen))
		next

	    # Write the results to the output file.
	    call fprintf (nst, NST_DATA1STR)
		call pargi (id)
		call pargi (DP_GROUPNUM(nstar))
		call pargr (xcen)
		call pargr (ycen)
	        call pargr (mag)
		call pargr (errmag)
	        call pargr (Memr[DP_APMSKY(apsel)+i-1])

	    call fprintf (nst, NST_DATA2STR)
		call pargi (niter)
	        call pargr (sharp)
	        call pargr (Memr[DP_APCHI(apsel)+i-1])

	}
end
