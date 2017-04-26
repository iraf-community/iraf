include <mach.h>
include <time.h>
include	<tbset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"

define	NCOLUMN 	6

# DP_TGNEWGRP -- Create a new GROUP output ST table.

procedure dp_tgnewgrp (tp, colpoint)

pointer	tp			# pointer to outpu ST table
pointer	colpoint[ARB]		# array of pointers to columns

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
	call strcpy (GROUP, Memc[colnames], SZ_COLNAME)
	call strcpy (ID, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+4*SZ_COLNAME+4], SZ_COLNAME)
	call strcpy (SKY, Memc[colnames+5*SZ_COLNAME+5], SZ_COLNAME)

	# Set up the format definitions.
	call strcpy ("%6d", Memc[colformat], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("%10.3f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+4*SZ_COLFMT+4], SZ_COLFMT)
	call strcpy ("%15.7g", Memc[colformat+5*SZ_COLFMT+5], SZ_COLFMT)

	# Set up the unit definitions.
	call strcpy ("##", Memc[colunits], SZ_COLUNITS)
	call strcpy ("##", Memc[colunits+SZ_COLUNITS+1], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+2*SZ_COLUNITS+2], SZ_COLUNITS)
	call strcpy ("PIXELS", Memc[colunits+3*SZ_COLUNITS+3], SZ_COLUNITS)
	call strcpy ("MAGNITUDES", Memc[colunits+4*SZ_COLUNITS+4], SZ_COLUNITS)
	call strcpy ("ADC", Memc[colunits+5*SZ_COLUNITS+5], SZ_COLUNITS)

	# Set up the data type definitions.
	Memi[col_dtype] = TY_INT
	Memi[col_dtype+1] = TY_INT
	Memi[col_dtype+2] = TY_REAL
	Memi[col_dtype+3] = TY_REAL
	Memi[col_dtype+4] = TY_REAL
	Memi[col_dtype+5] = TY_REAL

	# Define the column lengths.
	Memi[col_len] = 1
	Memi[col_len+1] = 1
	Memi[col_len+2] = 1
	Memi[col_len+3] = 1
	Memi[col_len+4] = 1
	Memi[col_len+5] = 1
	
	# Define and create the table.
	call tbcdef (tp, colpoint, Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[col_dtype], Memi[col_len], NCOLUMN)
	call tbtcre (tp)

	call sfree (sp)
end


# DP_XGGRPPARS -- Write out the parameters to the header of the GROUP text
# output file.

procedure dp_xggrppars (dao, tp)

pointer	dao			# pointer to the DAOPHOT structure
int	tp			# the output file descriptor

pointer	sp, outstr, date, time, psffit
int	envfind()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	psffit = DP_PSFFIT(dao)

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
	call dp_sparam (tp, "TASK", "group", "name", "")

	# Write the file name parameters.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PHOTFILE", Memc[outstr], "filename", "")
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PSFIMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "GRPFILE", Memc[outstr], "filename", "")

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

	# Write out the daophot parameters.
	call dp_rparam (tp, "PSFRAD", DP_SPSFRAD(dao), "scaleunit", "")
	call dp_rparam (tp, "FITRAD", DP_SFITRAD(dao), "scaleunit", "")
	call dp_rparam (tp, "PSFMAG", DP_PSFMAG(psffit), "magnitude", "")
	call dp_rparam (tp, "CRITSNRATIO", DP_CRITSNRATIO(dao), "sigma", "")
	call dp_iparam (tp, "MAXGROUP", DP_MAXGROUP(dao), "number", "")
	#call dp_iparam (tp, "MAXNSTAR", DP_MAXNSTAR(dao), "number", "")

	# Write out the group size parameters.
	# call dp_iparam (tp, "MINSZGROUP", 1, "number", "")
	# call dp_iparam (tp, "MAXSZGROUP", MAX_INT, "number", "")

	call sfree(sp)
end


# DP_TGGRPPARS -- Write out the parameters to the header of the GROUP output
# ST table file.

procedure dp_tggrppars (dao, tp)

pointer	dao			# pointer to the DAOPHOT structure
pointer	tp			# pointer to the output table

pointer	sp, outstr, date, time, psffit
int	envfind()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	psffit = DP_PSFFIT(dao)

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
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "GRPFILE", Memc[outstr])

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
	call tbhadr (tp, "PSFMAG", DP_PSFMAG(psffit))
	call tbhadr (tp, "CRITSNRATIO", DP_CRITSNRATIO(dao))
	call tbhadi (tp, "MAXGROUP", DP_MAXGROUP(dao))
	#call tbhadi (tp, "MAXNSTAR", DP_MAXNSTAR(dao))

	# call tbhadi (tp, "MINSZGROUP", 1)
	# call tbhadi (tp, "MAXSZGROUP", MAX_INT)

	call sfree(sp)
end


# DP_WRTGROUP -- Write out each group into a text file or an ST table.

procedure dp_wrtgroup (dao, im, grp, number, index, group_size, maxgroup)

pointer	dao			# pointer to daophot structure
pointer	im			# the input image descriptor
int	grp			# the output file descriptor
int	number[ARB]		# number in group of each size
int	index[ARB]		# index to results
int	group_size[ARB]		# size of groups
int	maxgroup		# maximum group size

begin
	if (DP_TEXT(dao) == YES)
	    call dp_xwrtgroup (dao, im, grp, number, index, group_size,
	        maxgroup)
	else
	    call dp_twrtgroup (dao, im, grp, number, index, group_size,
	        maxgroup)
end


define GR_NAMESTR "#N%4tGROUP%10tID%16tXCENTER%26tYCENTER%36tMAG%48tMSKY\
%80t\\\n"
define GR_UNITSTR "#U%4t##%10t##%16tpixels%26tpixels%36tmagnitudes%48tcounts\
%80t\\\n"
define GR_FORMATSTR "#F%4t%%-9d%10t%%-6d%16t%%-10.3f%26t%%-10.3f%36t%%-12.3f\
%48t%%-15.7g%80t \n"
define GR_DATASTR "%-9d%10t%-6d%16t%-10.3f%26t%-10.3f%36t%-12.3f%48t%-15.7g\
%80t \n"


# DP_XWRTGROUP -- Write each group into the GROUP output ST table.

procedure dp_xwrtgroup (dao, im, grp, number, index, group_size, maxgroup)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
int	grp			# the output file descriptor
int	number[ARB]		# number in group of each size
int	index[ARB]		# index to results
int	group_size[ARB]		# size of groups
int	maxgroup		# maximum group size

int	i, j, k, id, ngroup, nstars, first_ingrp
pointer	apsel
real	x, y, mag, sky

begin
	# Get the daophot pointer.
	apsel = DP_APSEL(dao)

	# Print results to the standard output.
	if (DP_VERBOSE(dao) == YES) {
	    call printf (" Size of     Number of\n")
	    call printf ("  group       groups\n\n")
	    do i = 1, maxgroup {
		if (number[i] <= 0)
		    next
	        call printf ("%8d    %9d\n")
	    	    call pargi (i)
	    	    call pargi (number[i])
	    }
	}

	# Add header parameters to the table.
	call dp_xggrppars (dao, grp)

	# Write out the banner.
	call fprintf (grp, "#\n")
	call fprintf (grp, GR_NAMESTR)
	call fprintf (grp, GR_UNITSTR)
	call fprintf (grp, GR_FORMATSTR)
	call fprintf (grp, "#\n")

	# Write out each group.
	ngroup = 1
	first_ingrp = 1
	while (first_ingrp <= DP_APNUM(apsel)) {

	    do j = first_ingrp, first_ingrp + group_size[first_ingrp] - 1 {

		# Test the center.
		k = index[j]
		x = Memr[DP_APXCEN(apsel)+k-1]
		y = Memr[DP_APYCEN(apsel)+j-1]
		if (IS_INDEFR(x) || IS_INDEFR(y))
		    break
		call dp_wout (dao, im, x, y, x, y, 1)

		# Get the rest of the numbers.
		id = Memi[DP_APID(apsel)+k-1]
		mag = Memr[DP_APMAG(apsel)+k-1]
		sky = Memr[DP_APMSKY(apsel)+k-1]

		# Write the results.
		call fprintf (grp, GR_DATASTR)
		    call pargi (ngroup)
		    call pargi (id)
		    call pargr (x)
		    call pargr (y)
		    call pargr (mag)
		    call pargr (sky)
	    }

	    ngroup = ngroup + 1
	    first_ingrp = first_ingrp + group_size[first_ingrp]
	}

	# Compute the number of groups and the number of stars.
	ngroup = 0
	nstars = 0
	do i = 1, maxgroup {
	    if (number[i] <= 0)
		next
	    nstars = nstars + i * number[i]
	    ngroup = ngroup + number[i]
	}

	if (DP_VERBOSE(dao) == YES) {
	    call printf ("\nTotal of %d stars in %d groups\n")
	         call pargi (nstars)
	         call pargi (ngroup)
	}
end


# DP_TWRTGROUP -- Write each group into the GROUP output text file.

procedure dp_twrtgroup (dao, im, grp, number, index, group_size, maxgroup)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
pointer	grp			# pointer to group output file
int	number[ARB]		# number in group of each size
int	index[ARB]		# index to results
int	group_size[ARB]		# size of groups
int	maxgroup		# maximum group size

int	row, first_ingrp, ngroup, nstars, i, j, k, id
pointer	apsel, sp, colpoint
real	x, y, mag, sky

begin
	# Get the daophot pointer.
	apsel = DP_APSEL(dao)

	# Allocate space for the column pointers.
	call smark( sp)
	call salloc (colpoint, NCOLUMN, TY_INT)

	# Get the necessary info to create the ST table.
	call dp_tgnewgrp  (grp, Memi[colpoint])

	# Add header parameters to the ST table.
	call dp_tggrppars (dao, grp)

	# Optionally print results to the standard output.
	if (DP_VERBOSE(dao) == YES) {
	    call printf (" Size of     Number of\n")
	    call printf ("  group        groups\n\n")
	    do i = 1, maxgroup {
		if (number[i] <= 0)
		    next
	        call printf ("  %d        %d\n")
	    	    call pargi (i)
	    	    call pargi (number[i])
	    }
	}

	# Initialize for writing.
	ngroup = 1
	row = 0

	# Initialize for reading.
	first_ingrp = 1

	# Write out the data for all the groups.
	while (first_ingrp <= DP_APNUM(apsel)) {

	    do j = first_ingrp, first_ingrp + group_size[first_ingrp] - 1 {

		# Test the center.
		k = index[j]
		x = Memr[DP_APXCEN(apsel)+k-1]
		y = Memr[DP_APYCEN(apsel)+j-1]
		if (IS_INDEFR(x) || IS_INDEFR(y))
		    break
		call dp_wout (dao, im, x, y, x, y, 1)

		# Get the rest of the values.
		id = Memi[DP_APID(apsel)+k-1]
		mag = Memr[DP_APMAG(apsel)+k-1]
		sky = Memr[DP_APMSKY(apsel)+k-1]

		# Copy the values to the correct table row.
		row = row + 1
		call tbrpti (grp, Memi[colpoint], ngroup, 1, row)
		call tbrpti (grp, Memi[colpoint+1], id, 1, row)
	    	call tbrptr (grp, Memi[colpoint+2], x, 1, row)
	    	call tbrptr (grp, Memi[colpoint+3], y, 1, row)
	    	call tbrptr (grp, Memi[colpoint+4], mag, 1, row)
	    	call tbrptr (grp, Memi[colpoint+5], sky, 1, row)
	    }

	    ngroup = ngroup + 1
	    first_ingrp = first_ingrp + group_size[first_ingrp]
	}

	# Compute the number of groups and the number of stars.
	ngroup = 0
	nstars = 0
	do i = 1, maxgroup {
	    if (number[i] <= 0)
		next
	    nstars = nstars + i * number[i]
	    ngroup = ngroup + number[i]
	}

	# Optionally print out a summary of the results.
	if (DP_VERBOSE(dao) == YES) {
	    call printf ("\nTotal of %d stars in %d groups\n")
	         call pargi (nstars)
	         call pargi (ngroup)
	}

	call sfree (sp)
end
