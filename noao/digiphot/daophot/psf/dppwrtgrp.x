include <time.h>
include	<tbset.h>
include	"../lib/apsel.h"
include	"../lib/daophot.h"
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_PWRTGRP --  Add a group to the PSF output group file.

procedure dp_pwrtgrp (dao, psfgr, top_star, new_table)

pointer	dao			# pointer to the daophot structure
pointer	psfgr			# group file descriptor
int	top_star		# first star in list not a neighbor
bool	new_table		# should a new table be created

int	group

begin
	# Create the table.
	if (new_table) {

	    # Initialize.
	    group = 1

	    # Make a new group.
	    if (DP_TEXT(dao) == YES)
		call dp_pxnewgrp (dao, psfgr, top_star, group)
	    else
	        call dp_ptnewgrp  (dao, psfgr, top_star, group) 

	} else  {

	    # Increment.
	    group = group + 1

	    # Add to the file.
	    if (DP_TEXT(dao) == YES)
	        call dp_pxaddgrp (dao, psfgr, top_star, group)
	    else
	        call dp_ptaddgrp (dao, psfgr, top_star, group)
	}

end


define PGR_NAMESTR "#N%4tID%10tGROUP%16tXCENTER%26tYCENTER%36tMAG%48t\
MSKY%80t\\\n"
define PGR_UNITSTR "#U%4t##%10t##%16tpixels%26tpixels%36tmagnitudes%48t\
counts%80t\\\n"
define PGR_FORMATSTR "#F%4t%%-9d%10t%%-6d%16t%%-10.2f%26t%%-10.2f%36t\
%%-12.3f%48t%%-14.3f%80t \n"
define	PGR_DATASTR "%-9d%10t%-6d%16t%-10.2f%26t%-10.2f%36t%-12.3f%48t\
%-14.3f%80t \n"

# DP_PXNEWGRP -- Create a new PSF output group text file and write the
# first group to it.

procedure dp_pxnewgrp (dao, tp, top_star, group)

pointer	dao			# pointer to the daophot structure.
pointer	tp			# pointer to table
int	top_star		# first non group result
int	group			# current group 

int	i
pointer	apsel

begin
	apsel = DP_APSEL(dao)

	# Write out the header parameters.
	call dp_pxgrppars (dao, tp)

	# Set up the column definitions.
	call fprintf (tp, "#\n")
	call fprintf (tp, PGR_NAMESTR)
	call fprintf (tp, PGR_UNITSTR)
	call fprintf (tp, PGR_FORMATSTR)
	call fprintf (tp, "#\n")

	do i = 1, top_star - 1 {
	    call fprintf (tp, PGR_DATASTR)
		call pargi (Memi[DP_APID(apsel)+i-1])
		call pargi (group)
	        call pargr (Memr[DP_APXCEN(apsel)+i-1])
	        call pargr (Memr[DP_APYCEN(apsel)+i-1])
	        call pargr (Memr[DP_APMAG(apsel)+i-1])
	        call pargr (Memr[DP_APMSKY(apsel)+i-1])
	}
end


# DP_PTNEWGRP -- Create a new PSF output group ST table and add to a new
# group to it.

procedure dp_ptnewgrp (dao, tp, top_star, group)

pointer	dao			# pointer to the daophot structure.
pointer	tp			# pointer to table
int	top_star		# first non group result
int	group			# current group 

int	i, j
pointer	apsel, psf
pointer	sp, colnames, colunits, colformat, coldtype, collen, colpoint

begin
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)
	colpoint = DP_COLPOINT(psf)

	# Allocate space for table definition.
	call smark (sp)
	call salloc (colnames, NAPCOLUMNS * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, NAPCOLUMNS * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, NAPCOLUMNS * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (coldtype, NAPCOLUMNS, TY_INT)
	call salloc (collen, NAPCOLUMNS, TY_INT)

	# Set up the column definitions.
	call strcpy (ID, Memc[colnames], SZ_COLNAME)
	call strcpy (GROUP, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+4*SZ_COLNAME+4], SZ_COLNAME)
	call strcpy (APSKY, Memc[colnames+5*SZ_COLNAME+5], SZ_COLNAME)

	# Set up the format definitions.
	call strcpy ("%6d", Memc[colformat], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("%10.2f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("%10.2f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+4*SZ_COLFMT+4], SZ_COLFMT)
	call strcpy ("%14.3f", Memc[colformat+5*SZ_COLFMT+5], SZ_COLFMT)

	# Define the column units.
	call strcpy ("NUMBER", Memc[colunits], SZ_COLUNITS)
	call strcpy ("NUMBER", Memc[colunits+SZ_COLUNITS+1], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+2*SZ_COLUNITS+2], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+3*SZ_COLUNITS+3], SZ_COLUNITS)
	call strcpy ("MAGNITUDES", Memc[colunits+4*SZ_COLUNITS+4], SZ_COLUNITS)
	call strcpy ("COUNTS", Memc[colunits+5*SZ_COLUNITS+5], SZ_COLUNITS)

	# Define the datatypes of the columns.
	Memi[coldtype] = TY_INT
	Memi[coldtype+1] = TY_INT
	Memi[coldtype+2] = TY_REAL
	Memi[coldtype+3] = TY_REAL
	Memi[coldtype+4] = TY_REAL
	Memi[coldtype+5] = TY_REAL

	# Initialize the column length parameter.
	do i = 1, NAPCOLUMNS {
	    j = i - 1
	    Memi[collen+j] = 1
	}
	
	# Define the table.
	call tbcdef (tp, Memi[colpoint], Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[coldtype], Memi[collen], NAPCOLUMNS)

	# Create the table.
	call tbtcre (tp)

	# Now write out the group.
	do i = 1, top_star - 1 {
	    call tbrpti (tp, Memi[colpoint], Memi[DP_APID(apsel)+i-1], 1, i)
	    call tbrpti (tp, Memi[colpoint+1], group, 1, i)
	    call tbrptr (tp, Memi[colpoint+2], Memr[DP_APXCEN(apsel)+i-1], 1, i)
	    call tbrptr (tp, Memi[colpoint+3], Memr[DP_APYCEN(apsel)+i-1], 1, i)
	    call tbrptr (tp, Memi[colpoint+4], Memr[DP_APMAG(apsel)+i-1], 1, i)
	    call tbrptr (tp, Memi[colpoint+5], Memr[DP_APMSKY(apsel)+i-1], 1, i)
	}

	# Add the header parameters to the table.
	call dp_ptgrppars (dao, tp)

	call sfree (sp)
	
end


# DP_PTGRPPARS -- Add parameters to the header of the output PSF group ST
# table.

procedure dp_ptgrppars (dao, tp)

pointer	dao			# pointer to the daophot structure
pointer	tp			# pointer to the table

pointer	sp, outstr, date, time
int	envfind()

begin
	# Allocate workin space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)


	# Write the task and date identifiers.
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

	# Define the package and task.
	call tbhadt (tp, "PACKAGE", "daophot")
	call tbhadt (tp, "TASK", "psf")

	# Define the input and output files.
	call tbhadt (tp, "IMAGE", DP_IMNAME(dao))
	call tbhadt (tp, "APFILE", DP_APFILE(dao))
	call tbhadt (tp, "PSFIMAGE", DP_PSFIMAGE(dao))
	call tbhadt (tp, "GRPSFILE", DP_GRPSFFILE(dao))

	# Data dependent parameters.
	call tbhadr (tp, "SCALE", DP_SCALE(dao))

	# Observing parameters.
	call tbhadt (tp, "OTIME", DP_OTIME(dao))
	call tbhadt (tp, "IFILTER", DP_IFILTER(dao))
	call tbhadr (tp, "XAIRMASS", DP_XAIRMASS(dao))

	# Grouping parameters.
	call tbhadr (tp, "PSFRAD", DP_SPSFRAD(dao))
	call tbhadr (tp, "FITRAD", DP_SFITRAD(dao))

	call sfree(sp)
end


# DP_PXGRPPARS -- Add parameters to the header of the output PSF group text
# file.

procedure dp_pxgrppars (dao, tp)

pointer	dao			# pointer to the daophot structure
pointer	tp			# pointer to the table

pointer	sp, outstr, date, time
int	envfind()

begin
	# Allocate workin space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)


	# Write the task and date identifiers.
	if (envfind ("version", Memc[outstr], SZ_LINE) <= 0)
	    call strcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IRAF", Memc[outstr], "version", "")
	if (envfind ("userid", Memc[outstr], SZ_LINE) > 0)
	    call dp_sparam (tp, "USER", Memc[outstr], "name", "")
	call gethost (Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "HOST", Memc[outstr], "computer", "")
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call dp_sparam (tp, "DATE", Memc[date], "mm-dd-yr", "")
	call dp_sparam (tp, "TIME", Memc[time], "hh:mm:ss", "") 

	# Define the package and task.
	call dp_sparam (tp, "PACKAGE", "daophot", "name", "")
	call dp_sparam (tp, "TASK", "psf", "name", "") 

	# Define the input and output files.
	call dp_sparam (tp, "IMAGE", DP_IMNAME(dao), "imagename", "")
	call dp_sparam (tp, "APFILE", DP_APFILE(dao), "filename", "")
	call dp_sparam (tp, "PSFIMAGE", DP_PSFIMAGE(dao), "imagename", "")
	call dp_sparam (tp, "GRPSFILE", DP_GRPSFFILE(dao), "filename", "")

	# Define the data dependent parameters.
	call dp_rparam (tp, "SCALE", DP_SCALE(dao), "units/pix", "")

	# Observing parameters.
	call dp_sparam (tp, "OTIME", DP_OTIME(dao), "timeunit", "")
	call dp_sparam (tp, "IFILTER", DP_IFILTER(dao), "filter", "")
	call dp_rparam (tp, "XAIRMASS", DP_XAIRMASS(dao), "number", "")

	# Grouping parameters.
	call dp_rparam (tp, "PSFRAD", DP_SPSFRAD(dao), "scaleunit", "")
	call dp_rparam (tp, "FITRAD", DP_SFITRAD(dao), "scaleunit", "")

	call sfree(sp)
end


# DP_PXADDGRP -- Add a new group to the existing PSF output group text file.

procedure dp_pxaddgrp (dao, tp, top_star, group)

pointer	dao			# pointer to daophot structure
pointer	tp			# pointer to Table
int	top_star		# first non-group result
int	group			# current group

int	i
pointer	apsel

begin
	# Get some daophot pointers.
	apsel = DP_APSEL(dao)

	# Now write out the group.
	do i = 1, top_star - 1 {
	    call fprintf (tp, PGR_DATASTR)
		call pargi (Memi[DP_APID(apsel)+i-1])
		call pargi (group)
	        call pargr (Memr[DP_APXCEN(apsel)+i-1])
	        call pargr (Memr[DP_APYCEN(apsel)+i-1])
	        call pargr (Memr[DP_APMAG(apsel)+i-1])
	        call pargr (Memr[DP_APMSKY(apsel)+i-1])
	}
end


# DP_PTADDGRP -- Add a new group to the existing PSF output group ST table.

procedure dp_ptaddgrp (dao, tp, top_star, group)

pointer	dao			# pointer to daophot structure
pointer	tp			# pointer to Table
int	top_star		# first non-group result
int	group			# current group

int	i, j, nrows
pointer	apsel, psf, colpoint
int	tbpsta()

begin
	# Get some daophot pointers.
	apsel = DP_APSEL(dao)

	# Find the number of rows in the table and add on at the end.
	nrows = tbpsta (tp, TBL_NROWS)

	# Now write out the group.
	psf = DP_PSF(dao)
	colpoint = DP_COLPOINT(psf)
	do i = 1, top_star -1 {
	    j = i + nrows
	    call tbrpti (tp, Memi[colpoint], Memi[DP_APID(apsel)+i-1], 1, j)
	    call tbrpti (tp, Memi[colpoint+1], group, 1, j)
	    call tbrptr (tp, Memi[colpoint+2], Memr[DP_APXCEN(apsel)+i-1], 1, j)
	    call tbrptr (tp, Memi[colpoint+3], Memr[DP_APYCEN(apsel)+i-1], 1, j)
	    call tbrptr (tp, Memi[colpoint+4], Memr[DP_APMAG(apsel)+i-1], 1, j)
	    call tbrptr (tp, Memi[colpoint+5], Memr[DP_APMSKY(apsel)+i-1], 1, j)
	}

end
