include <time.h>
include	<tbset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"

define	ADD_NOUTCOLUMN 	4

# DP_TNADDSTAR -- Create an output ADDSTAR table.

procedure dp_tnaddstar (dao, tp, columns)

pointer	dao			# pointer to daophot structure
pointer	tp			# output table decscriptor
int columns[ARB]		# pointer to columns

int	i
pointer	sp, colnames, colunits, colformat, col_dtype, col_len

begin
	# Allocate space for table definition.
	call smark (sp)
	call salloc (colnames, ADD_NOUTCOLUMN * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, ADD_NOUTCOLUMN * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, ADD_NOUTCOLUMN * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (col_dtype, ADD_NOUTCOLUMN, TY_INT)
	call salloc (col_len, ADD_NOUTCOLUMN, TY_INT)

	# Set up the column definitions.
	call strcpy (ID, Memc[colnames], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)

	# Set up the column formats.
	call strcpy ("%5d", Memc[colformat], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)

	# Set up the units definitions.
	call strcpy ("NUMBER", Memc[colunits], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+SZ_COLUNITS+1], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+2*SZ_COLUNITS+2], SZ_COLUNITS)
	call strcpy ("MAGNITIDES", Memc[colunits+3*SZ_COLUNITS+3], 
	    SZ_COLUNITS)

	# Set up the data types.
	Memi[col_dtype] = TY_INT
	Memi[col_dtype+1] = TY_REAL
	Memi[col_dtype+2] = TY_REAL
	Memi[col_dtype+3] = TY_REAL

	do i = 1, ADD_NOUTCOLUMN 
	    Memi[col_len+i-1] = 1
	
	call tbcdef (tp, columns, Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[col_dtype], Memi[col_len], ADD_NOUTCOLUMN)
	call tbtcre (tp)

	# Write out the header parameters.
	call dp_tgadppars (dao, tp)

	call sfree (sp)
	
end


define ADD_NAMESTR "#N%4tID%10tXCENTER%20tYCENTER%30tMAG%80t\\\n"
define ADD_UNITSTR "#U%4t##%10tpixels%20tpixels%30tmagnitudes%80t\\\n"
define ADD_FORMATSTR "#F%4t%%-9d%10t%%-10.3f%20t%%-10.3f%30t%%-12.3f%80t \n"
define ADD_DATASTR "%4t%-6d%10t%-10.3f%20t%-10.3f%30t%-12.3f%80t \n"


# DP_XNADDSTAR -- Write out the ADDSTAR header parameters into a text file.

procedure dp_xnaddstar (dao, tp)

pointer	dao			# pointer to the daophot structure
int	tp			# group output file descriptor

begin
	# Add header parameters to the table.
	call dp_xgadppars (dao, tp)

	# Write out the banner.
	call fprintf (tp, "#\n")
	call fprintf (tp, ADD_NAMESTR)
	call fprintf (tp, ADD_UNITSTR)
	call fprintf (tp, ADD_FORMATSTR)
	call fprintf (tp, "#\n")
end


# DP_XWADD -- Procedure to write out the new star to the ADDSTAR output
# text file.

procedure dp_xwadd (tp, id, x, y, mag)

int	tp			# output file descriptor
int	id			# id number
real	x			# x value
real	y			# y value
real	mag			# magnitude

begin
	call fprintf (tp, ADD_DATASTR)
	    call pargi (id)
	    call pargr (x)
	    call pargr (y)
	    call pargr (mag)
end


# DP_TWADD -- Procedure to write out the new star to the ADDSTAR output
# table.

procedure dp_twadd (tp, colpoint, id, x, y, mag, row)

int	tp			# pointer to group output table
int	colpoint[ARB]		# column pointers
int	id			# id number
real	x			# x value
real	y			# y value
real	mag			# magnitude
int	row			# row number to be added

begin
	call tbrpti (tp, colpoint[1], id, 1, row)
	call tbrptr (tp, colpoint[2], x, 1, row)
	call tbrptr (tp, colpoint[3], y, 1, row)
	call tbrptr (tp, colpoint[4], mag, 1, row)
end


# DP_XGADPPARS -- Add various parameters to the header of the ADDSTAR text
# output file.

procedure dp_xgadppars (dao, tp)

pointer	dao			# pointer to the DAOPHOT structure
int	tp			# output file descriptor

pointer	sp, outstr, date, time
int	envfind()

begin
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

	if (envfind ("userid", Memc[outstr], SZ_LINE) > 0)
	    call dp_sparam (tp, "USER", Memc[outstr], "name", "")
	call gethost (Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "HOST", Memc[outstr], "computer", "")
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call dp_sparam (tp, "DATE", Memc[date], "yyyy-mm-dd", "")
	call dp_sparam (tp, "TIME", Memc[time], "hh:mm:ss", "")
	call dp_sparam (tp, "PACKAGE", "daophot", "name", "")
	call dp_sparam (tp, "TASK", "addstar", "name", "")

	# Write the file name parameters.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PHOTFILE", Memc[outstr], "filename", "")
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PSFIMAGE", Memc[outstr], "imagename", "")
	call dp_imroot (DP_OUTIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "ADDIMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "ADDFILE", Memc[outstr], "filename", "")

	# Write out relevant data parameters.
	call dp_rparam (tp, "SCALE", DP_SCALE(dao), "units/pix", "")
	call dp_rparam (tp, "DATAMIN", DP_MINGDATA(dao), "counts", "")
	call dp_rparam (tp, "DATAMAX", DP_MAXGDATA(dao), "counts", "")
	call dp_rparam (tp, "GAIN", DP_PHOTADU(dao), "number", "")
	call dp_rparam (tp, "READNOISE", DP_READNOISE(dao), "electrons", "")

	# Write out the observing parameters.
	call dp_sparam (tp, "OTIME", DP_OTIME(dao), "timeunit", "")
	call dp_rparam (tp, "XAIRMASS", DP_XAIRMASS(dao), "number", "")
	call dp_sparam (tp, "IFILTER", DP_IFILTER(dao), "filter", "")

	# Write out the daopars parameters.
	call dp_rparam (tp, "PSFRAD", DP_SPSFRAD(dao), "scaleunit", "")
	call dp_rparam (tp, "FITRAD", DP_SFITRAD(dao), "scaleunit", "")

	call sfree(sp)
end


# DP_TGADPPARS -- Add various parameters to the header of the ADDSTAR output
# table.

procedure dp_tgadppars (dao, tp)

pointer	dao			# pointer to the DAOPHOT structure
pointer	tp			# pointer to the output table

pointer	sp, outstr, date, time
int	envfind()

begin
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

	if (envfind ("userid", Memc[outstr], SZ_LINE) > 0)
	    call tbhadt (tp, "USER", Memc[outstr])
	call gethost (Memc[outstr], SZ_LINE)
	call tbhadt (tp, "HOST", Memc[outstr])
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call tbhadt (tp, "DATE", Memc[date])
	call tbhadt (tp, "TIME", Memc[time])
	call tbhadt (tp, "PACKAGE", "daophot")
	call tbhadt (tp, "TASK", "group")

	# Write the file name parameters.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "IMAGE", Memc[outstr])
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PHOTFILE", Memc[outstr])
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PSFIMAGE", Memc[outstr])
	call dp_imroot (DP_OUTIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "ADDIMAGE", Memc[outstr])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "ADDFILE", Memc[outstr])

	# Write out relevant data parameters.
	call tbhadr (tp, "SCALE", DP_SCALE(dao))
	call tbhadr (tp, "DATAMIN", DP_MINGDATA(dao))
	call tbhadr (tp, "DATAMAX", DP_MAXGDATA(dao))
	call tbhadr (tp, "GAIN", DP_PHOTADU(dao))
	call tbhadr (tp, "READNOISE", DP_READNOISE(dao))

	# Write out the observing parameters.
	call tbhadt (tp, "OTIME", DP_OTIME(dao))
	call tbhadr (tp, "XAIRMASS", DP_XAIRMASS(dao))
	call tbhadt (tp, "IFILTER", DP_IFILTER(dao))

	# Write out the daophot parameters.
	call tbhadr (tp, "PSFRAD", DP_SPSFRAD(dao))
	call tbhadr (tp, "FITRAD", DP_SFITRAD(dao))

	call sfree(sp)
end
