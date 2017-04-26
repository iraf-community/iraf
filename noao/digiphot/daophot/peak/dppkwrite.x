include	<tbset.h>
include <time.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/peakdef.h"

# DP_TNEWPEAK -- Create a new output PEAK ST table.

procedure dp_tnewpeak (dao, tp, colpoint)

pointer	dao		# pointer to the daophot strucuture
pointer	tp		# pointer to the output table
int	colpoint[ARB]	# array of column pointers

int	i
pointer	sp, colnames, colunits, colformat, col_dtype, col_len

begin
	# Allocate space for the table definition.
	call smark (sp)
	call salloc (colnames, PK_NOUTCOL * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, PK_NOUTCOL * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, PK_NOUTCOL * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (col_dtype, PK_NOUTCOL, TY_INT)
	call salloc (col_len, PK_NOUTCOL, TY_INT)

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
	call strcpy ("%12.3f", Memc[colformat+6*SZ_COLFMT+6], SZ_COLFMT)
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
	call strcpy ("ADU", Memc[colunits+5*SZ_COLUNITS+5], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+6*SZ_COLUNITS+6], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+7*SZ_COLUNITS+7], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+8*SZ_COLUNITS+8], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+9*SZ_COLUNITS+9], SZ_COLUNITS)
	call strcpy ("PERRORS", Memc[colunits+10*SZ_COLUNITS+10], SZ_COLUNITS)

	# Define the column data types.
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
	do i = 1, PK_NOUTCOL 
	    Memi[col_len+i-1] = 1
	
	# Define and create the table.
	call tbcdef (tp, colpoint, Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[col_dtype], Memi[col_len], PK_NOUTCOL)
	call tbtcre (tp)

	# Write out some header parameters.
	call dp_tpeakpars (dao, tp)

	call sfree (sp)
end


define PK_NAME1STR "#N%4tID%10tXCENTER%20tYCENTER%30tMAG%42tMERR%56tMSKY%71t\
NITER%80t\\\n"
define PK_UNIT1STR "#U%4t##%10tpixels%20tpixels%30tmagnitudes%42t\
magnitudes%56tcounts%71t##%80t\\\n"
define PK_FORMAT1STR "#F%4t%%-9d%10t%%-10.3f%20t%%-10.3f%30t%%-12.3f%42t\
%%-14.3f%56t%%-15.7g%71t%%-6d%80t \n"

define PK_NAME2STR "#N%12tSHARPNESS%24tCHI%36tPIER%42tPERROR%80t\\\n"
define PK_UNIT2STR "#U%12t##%24t##%36t##%42tperrors%80t\\\n"
define PK_FORMAT2STR "#F%12t%%-23.3f%24t%%-12.3f%36t%%-6d%42t%%-13s%80t \n"


# DP_XNEWPEAK -- Create a new PEAK output text file.

procedure dp_xnewpeak (dao, tp)

pointer	dao		# pointer to the daophot structure
int	tp		# the output file descriptor

begin
	# Write out some header parameters.
	call dp_xpeakpars (dao, tp)

	# Print out the banner file.
	call fprintf (tp, "#\n")
	call fprintf (tp, PK_NAME1STR)
	call fprintf (tp, PK_UNIT1STR)
	call fprintf (tp, PK_FORMAT1STR)
	call fprintf (tp, "#\n")
	call fprintf (tp, PK_NAME2STR)
	call fprintf (tp, PK_UNIT2STR)
	call fprintf (tp, PK_FORMAT2STR)
	call fprintf (tp, "#\n")
end


# DP_TPEAKPARS -- Add parameters to the header of the PEAK ST table.

procedure dp_tpeakpars (dao, tp)

pointer	dao			# pointer to the daophot structure
pointer	tp			# pointer to the output table

pointer	sp, psffit, outstr, date, time
bool	itob()
int	envfind()

begin
	# Define some daophot pointers.
	psffit = DP_PSFFIT(dao)

	# Allocate working space.
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
	call tbhadt (tp, "TASK", "peak")

	# Write out the files names.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "IMAGE", Memc[outstr])
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PHOTFILE", Memc[outstr])
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PSFIMAGE", Memc[outstr])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PEAKFILE", Memc[outstr])
	if (DP_OUTREJFILE(dao) == EOS)
	    call tbhadt (tp, "REJFILE", "\"\"")
	else {
	    call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_LINE)
	    call tbhadt (tp, "REJFILE", Memc[outstr])
	}

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
	call tbhadb (tp, "FITSKY", itob (DP_FITSKY(dao)))
	call tbhadr (tp, "PSFRAD", DP_SPSFRAD(dao))
	call tbhadr (tp, "FITRAD", DP_SFITRAD(dao))
	call tbhadr (tp, "PSFMAG", DP_PSFMAG(psffit))
	call tbhadi (tp, "MAXITER", DP_MAXITER(dao))
	call tbhadr (tp, "FLATERROR", DP_FLATERR(dao))
	call tbhadr (tp, "PROFERROR", DP_PROFERR(dao))
	call tbhadi (tp, "CLIPEXP", DP_CLIPEXP(dao))
	call tbhadr (tp, "CLIPRANGE", DP_CLIPRANGE(dao))
	#call tbhadi (tp, "MAXNSTAR", DP_MAXNSTAR(dao))

	call sfree(sp)
end


# DP_XPEAKPARS -- Add parameters to the header of the output PEAK text file.

procedure dp_xpeakpars (dao, tp)

pointer	dao			# pointer to the daophot structure
int	tp			# the output file descriptor

pointer	sp, psffit, outstr, date, time
bool	itob()
int	envfind()

begin
	# Define some daophot pointers.
	psffit = DP_PSFFIT(dao)

	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	# Write the id.
	if (envfind ("version", Memc[outstr], SZ_LINE) <= 0)
	    call strcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IRAF", Memc[outstr], "version", "")
	if (envfind ("userid", Memc[outstr], SZ_LINE) <= 0)
	    call dp_sparam (tp, "USER", Memc[outstr], "name", "")
	call gethost (Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "HOST", Memc[outstr], "computer", "")
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call dp_sparam (tp, "DATE", Memc[date], "yyyy-mm-dd", "")
	call dp_sparam (tp, "TIME", Memc[time], "hh:mm:ss", "")
	call dp_sparam (tp, "PACKAGE", "daophot", "name", "")
	call dp_sparam (tp, "TASK", "peak", "name", "")

	# Write out the files names.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PHOTFILE", Memc[outstr], "filename", "")
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PSFIMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PEAKFILE", Memc[outstr], "filename", "")

	if (DP_OUTREJFILE(dao) == EOS)
	    call dp_sparam (tp, "REJFILE", "\"\"", "filename", "")
	else {
	    call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_LINE)
	    call dp_sparam (tp, "REJFILE", Memc[outstr], "filename", "")
	}

	# Define the data characteristics.
	call dp_rparam (tp, "SCALE", DP_SCALE(dao), "units/pix", "")
	call dp_rparam (tp, "DATAMIN", DP_MINGDATA(dao), "counts", "")
	call dp_rparam (tp, "DATAMAX", DP_MAXGDATA(dao), "counts", "")
	call dp_rparam (tp, "GAIN", DP_PHOTADU(dao), "e-/adu", "")
	call dp_rparam (tp, "READNOISE", DP_READNOISE(dao), "electrons", "")

	# Define the observing parameters.
	call dp_sparam (tp, "OTIME", DP_OTIME(dao), "timeunit", "")
	call dp_rparam (tp, "XAIRMASS", DP_XAIRMASS(dao), "number", "")
	call dp_sparam (tp, "IFILTER", DP_IFILTER(dao), "filter", "")

	# Define the fitting parameters.
	call dp_bparam (tp, "RECENTER", itob (DP_RECENTER(dao)), "switch", "")
	call dp_bparam (tp, "FITSKY", itob (DP_FITSKY(dao)), "switch", "")
	call dp_rparam (tp, "PSFRAD", DP_SPSFRAD(dao), "scaleunit", "")
	call dp_rparam (tp, "FITRAD", DP_SFITRAD(dao), "scaleunit", "")
	call dp_rparam (tp, "PSFMAG", DP_PSFMAG(psffit), "magnitudes", "")
	call dp_iparam (tp, "MAXITER", DP_MAXITER(dao), "number", "")
	call dp_rparam (tp, "FLATERROR", DP_FLATERR(dao), "percentage", "")
	call dp_rparam (tp, "PROFERROR", DP_PROFERR(dao), "percentage", "")
	call dp_iparam (tp, "CLIPEXP", DP_CLIPEXP(dao), "number", "")
	call dp_rparam (tp, "CLIPRANGE", DP_CLIPRANGE(dao), "sigma", "")
	#call dp_iparam (tp, "MAXNSTAR", DP_MAXNSTAR(dao), "number", "")

	call sfree(sp)
end


# DP_TPKWRITE -- Write the output photometry record to a PEAK ST table.

procedure dp_tpkwrite (tpout, colpoint, id, x, y, mag, errmag, sky, niter, chi,
	sharp, pier, perror, plen, star)

pointer	tpout		# pointer to the output table
int	colpoint[ARB]	# array of column pointers
int	id		# id of the star
real	x, y		# position of the star
real	mag		# magnitude of the star
real	errmag		# error magnitude of the star
real	sky		# value of sky
int	niter		# number of iterations
real	chi		# chi squared value
real	sharp		# sharpness characteristic
int	pier		# the error code
char	perror[ARB]	# the error string
int	plen		# the length of the error code string
int	star		# row number

begin
	call tbrpti (tpout, colpoint[1], id, 1, star)
	call tbrptr (tpout, colpoint[2], x, 1, star)
	call tbrptr (tpout, colpoint[3], y, 1, star)
	call tbrptr (tpout, colpoint[4], mag, 1, star)
	call tbrptr (tpout, colpoint[5], errmag, 1, star)
	call tbrptr (tpout, colpoint[6], sky, 1, star)
	call tbrpti (tpout, colpoint[7], niter, 1, star)
	call tbrptr (tpout, colpoint[8], sharp, 1, star)
	call tbrptr (tpout, colpoint[9], chi, 1, star)
	call tbrpti (tpout, colpoint[10], pier, 1, star)
	call tbrptt (tpout, colpoint[11], perror, plen, 1, star)
end


define PK_DATA1STR "%-9d%10t%-10.3f%20t%-10.3f%30t%-12.3f%42t%-14.3f%56t\
%-15.7g%71t%-6d%80t\\\n"
define PK_DATA2STR "%12t%-12.3f%24t%-12.3f%36t%-6d%42t%-13.13s%80t \n"

# DP_XPKWRITE -- Write the output photometry record to a PEAK text file.

procedure dp_xpkwrite (tpout, id, x, y, mag, errmag, sky, niter, chi, sharp,
	pier, perror, plen)

int 	tpout		# the output file descriptor
int	id		# id of the star
real	x, y		# position of the star
real	mag		# magnitude of the star
real	errmag		# error magnitude of the star
real	sky		# value of sky
int	niter		# number of iterations
real	chi		# chi squared value
real	sharp		# sharpness characteristic
int	pier		# the error code
char	perror[ARB]	# the error string
int	plen		# the length of the error code string

begin
	call fprintf (tpout, PK_DATA1STR)
	    call pargi (id)
	    call pargr (x)
	    call pargr (y)
	    call pargr (mag)
	    call pargr (errmag)
	    call pargr (sky)
	    call pargi (niter)
	call fprintf (tpout, PK_DATA2STR)
	    call pargr (sharp)
	    call pargr (chi)
	    call pargi (pier)
	    call pargstr (perror)
end
