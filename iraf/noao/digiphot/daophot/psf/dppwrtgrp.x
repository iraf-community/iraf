include <time.h>
include	<tbset.h>
include	"../lib/daophotdef.h"
include	"../lib/apseldef.h"
include	"../lib/psfdef.h"


# DP_WNEISTARS -- Identify the neighbour stars of the psf stars and write them
# out in groups.

procedure dp_wneistars (dao, im, psfgr)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
int	psfgr			# the output group file descriptor

bool	newfile
int	top_star, psf_star, nei1, nei2
pointer	psf
int	dp_neistars()

begin
	psf = DP_PSF(dao)
	newfile = true
	top_star = DP_PNUM(psf) + 1
	do psf_star = 1, DP_PNUM(psf) {
	    if (dp_neistars (dao, psf_star, top_star, nei1, nei2) <= 0)
	        ;
	    call dp_pwrtgrp (dao, im, psfgr, psf_star, nei1, nei2, newfile)
	    if (newfile)
	        newfile = false
	}
end


# DP_NEISTARS -- Identify the neighbours and friends of the neighbours for
# an individual PSF star.

int procedure dp_neistars (dao, psf_star, top_star, nei1, nei2)

pointer	dao			# pointer to the daophot structure
int	psf_star		# the psf star in question
int	top_star		# pointer to the current top_star
int	nei1, nei2		# pointer to to the list of neighbours

int	j, nei_star
pointer	apsel
real	rsq1, rsq2, rsq

begin
	# Define some pointers
	apsel = DP_APSEL(dao)

	# These are thhe values used by daophot ii. I have decided to keep
	# the old values because I have kept the old grouping algorithm.
	#rsq1 = (1.5 * DP_PSFRAD(dao) + 2.0 * DP_FITRAD(dao) + 1.0) ** 2
	#rsq2 = (2.0 * DP_FITRAD(dao) + 1.0) ** 2

	rsq1 = (DP_PSFRAD(dao) + 2.0 * DP_FITRAD(dao) + 1.0) ** 2
	rsq2 = (2.0 * DP_FITRAD(dao)) ** 2

	# Find the neighbour stars for a given psf star. This step is the
	# same as the daophot ii step although I am using a smaller critical
	# radius.

	nei1 = top_star
	for (j = top_star; j <= DP_APNUM(apsel); j = j + 1) {
	    rsq = (Memr[DP_APXCEN(apsel)+j-1] -
		Memr[DP_APSEL(apsel)+psf_star-1]) ** 2 +
		(Memr[DP_APYCEN(apsel)+j-1] -
		Memr[DP_APYCEN(apsel)+psf_star-1]) ** 2
	    if (rsq > rsq1)
		next
	    call dp_5swap (j, top_star, Memi[DP_APID(apsel)],
		Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)])
	    top_star = top_star +1
	}
	nei2 = top_star - 1

	# Find the friends of the neighbor stars. I do this on a per psf
	# star basis. I do not find friends of all the neighbor stars
	# only the neighbour stars for a particular psf star. This is
	# because I found the daophot ii algorithm could produce too 
	# may odd stars.

	do nei_star = nei1, nei2 { 
	    for (j = top_star; j <= DP_APNUM(apsel); j = j + 1) {
	        rsq = (Memr[DP_APXCEN(apsel)+j-1] -
		    Memr[DP_APSEL(apsel)+nei_star-1]) ** 2 +
		    (Memr[DP_APYCEN(apsel)+j-1] -
		    Memr[DP_APYCEN(apsel)+nei_star-1]) ** 2
		if (rsq > rsq2)
		    next
		call dp_5swap (j, top_star, Memi[DP_APID(apsel)],
		    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)])
		top_star = top_star +1
	    }
	}
	nei2 = top_star - 1

	return (nei2 - nei1 + 1)
end


# DP_PWRTGRP --  Add a group to the PSF output group file.

procedure dp_pwrtgrp (dao, im, psfgr, psf_star, nei1_star, nei2_star, new_table)

pointer	dao			# the pointer to the daophot structure
pointer	im			# the input image descriptor
int	psfgr			# the group file descriptor
int	psf_star		# the psf star index
int	nei1_star, nei2_star	# the first and last neighbour star indices
bool	new_table		# should a new table be created

int	group

begin
	# Chexk to see if the PSF group file is open.
	if (psfgr == NULL)
	    return

	# Create the table.
	if (new_table) {

	    # Initialize.
	    group = 1

	    # Make a new group.
	    if (DP_TEXT(dao) == YES)
		call dp_pxnewgrp (dao, im, psfgr, psf_star, nei1_star,
		    nei2_star, group)
	    else
	        call dp_ptnewgrp  (dao, im, psfgr, psf_star, nei1_star,
		    nei2_star, group) 

	} else  {

	    # Increment.
	    group = group + 1

	    # Add to the file.
	    if (DP_TEXT(dao) == YES)
	        call dp_pxaddgrp (dao, im, psfgr, psf_star, nei1_star,
		    nei2_star, group)
	    else
	        call dp_ptaddgrp (dao, im, psfgr, psf_star, nei1_star,
		    nei2_star, group)
	}

end


define	NCOLUMN	5

# DP_WPLIST -- Write the list of psf stars to the output psf star list.

procedure dp_wplist (dao, im, opst)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor 
int	opst			# the output psf star list descriptor

pointer	apsel, psf, sp, ocolpoint, tx, ty
int	dp_stati()
bool	itob()

begin
	# Get some daophot pointers.
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)

	# Allocate some working memory.
	call smark (sp)
	call salloc (ocolpoint, NCOLUMN, TY_POINTER)
	call salloc (tx, DP_PNUM(psf), TY_REAL)
	call salloc (ty, DP_PNUM(psf), TY_REAL)

	# Initialize the output file.
	if (dp_stati (dao, TEXT) == YES) {
	    call dp_pxgrppars (dao, opst)
	    call dp_xpbanner (opst)
	} else {
	    call dp_tpdefcol (opst, Memi[ocolpoint])
	    call dp_ptgrppars (dao, opst)
	}

	# Write out the stars.
	call dp_wout (dao, im, Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
	    Memr[tx], Memr[ty], DP_PNUM(psf))
	call dp_wpstars (opst, Memi[ocolpoint], itob (dp_stati (dao, TEXT)),
	    Memi[DP_APID(apsel)], Memr[tx], Memr[ty], Memr[DP_APMAG(apsel)],
	    Memr[DP_APMSKY(apsel)], DP_PNUM(psf))

	# Free memory.
	call sfree (sp)
end


define PGR_NAMESTR "#N%4tID%10tGROUP%16tXCENTER%26tYCENTER%36tMAG%48t\
MSKY%80t\\\n"
define PGR_UNITSTR "#U%4t##%10t##%16tpixels%26tpixels%36tmagnitudes%48t\
counts%80t\\\n"
define PGR_FORMATSTR "#F%4t%%-9d%10t%%-6d%16t%%-10.3f%26t%%-10.3f%36t\
%%-12.3f%48t%%-15.7g%80t \n"
define	PGR_DATASTR "%-9d%10t%-6d%16t%-10.3f%26t%-10.3f%36t%-12.3f%48t\
%-15.7g%80t \n"

# DP_PXNEWGRP -- Create a new PSF output group text file and write the
# first group to it.

procedure dp_pxnewgrp (dao, im, tp, psf_star, nei1_star, nei2_star, group)

pointer	dao			# pointer to the daophot structure.
pointer	im			# pointer to the input image
int	tp			# the output file descriptor
int	psf_star		# the psf star index
int	nei1_star, nei2_star	# the first and last neighbour star indices
int	group			# current group 

real	tx, ty
pointer	apsel
int	i

begin
	# Check to see if the PSF group file is open.
	if (tp == NULL)
	    return

	# Define some pointers.
	apsel = DP_APSEL(dao)

	# Write out the header parameters.
	call dp_pxgrppars (dao, tp)

	# Set up the column definitions.
	call fprintf (tp, "#\n")
	call fprintf (tp, PGR_NAMESTR)
	call fprintf (tp, PGR_UNITSTR)
	call fprintf (tp, PGR_FORMATSTR)
	call fprintf (tp, "#\n")

	# Write out the psf star.
	call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+psf_star-1],
	    Memr[DP_APYCEN(apsel)+psf_star-1], tx, ty, 1)
	call fprintf (tp, PGR_DATASTR)
	    call pargi (Memi[DP_APID(apsel)+psf_star-1])
	    call pargi (group)
	    call pargr (tx)
	    call pargr (ty)
	    call pargr (Memr[DP_APMAG(apsel)+psf_star-1])
	    call pargr (Memr[DP_APMSKY(apsel)+psf_star-1])

	# Write out the neighbour stars.
	do i = nei1_star, nei2_star {
	    call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+i-1],
	        Memr[DP_APYCEN(apsel)+i-1], tx, ty, 1)
	    call fprintf (tp, PGR_DATASTR)
		call pargi (Memi[DP_APID(apsel)+i-1])
		call pargi (group)
	        call pargr (tx)
	        call pargr (ty)
	        call pargr (Memr[DP_APMAG(apsel)+i-1])
	        call pargr (Memr[DP_APMSKY(apsel)+i-1])
	}
end


# DP_PTNEWGRP -- Create a new PSF output group ST table and add to a new
# group to it.

procedure dp_ptnewgrp (dao, im, tp, psf_star, nei1_star, nei2_star, group)

pointer	dao			# pointer to the daophot structure.
pointer	im			# the input image descriptor
pointer	tp			# pointer to table
int	psf_star		# the psf star index
int	nei1_star, nei2_star	# the first and last psf star indices
int	group			# current group 

real	tx, ty
pointer	sp, colnames, colunits, colformat, coldtype, collen, colpoint
pointer	apsel
int	i, j, row

begin
	# Check to see if the PSF group file is open.
	if (tp == NULL)
	    return

	# Define some pointers.
	apsel = DP_APSEL(dao)

	# Allocate space for table definition.
	call smark (sp)
	call salloc (colpoint, PSF_NOUTCOLS, TY_INT)
	call salloc (colnames, PSF_NOUTCOLS * (SZ_COLNAME + 1), TY_CHAR)
	call salloc (colunits, PSF_NOUTCOLS * (SZ_COLUNITS + 1), TY_CHAR)
	call salloc (colformat, PSF_NOUTCOLS * (SZ_COLFMT + 1), TY_CHAR)
	call salloc (coldtype, PSF_NOUTCOLS, TY_INT)
	call salloc (collen, PSF_NOUTCOLS, TY_INT)

	# Set up the column definitions.
	call strcpy (ID, Memc[colnames], SZ_COLNAME)
	call strcpy (GROUP, Memc[colnames+SZ_COLNAME+1], SZ_COLNAME)
	call strcpy (XCENTER, Memc[colnames+2*SZ_COLNAME+2], SZ_COLNAME)
	call strcpy (YCENTER, Memc[colnames+3*SZ_COLNAME+3], SZ_COLNAME)
	call strcpy (MAG, Memc[colnames+4*SZ_COLNAME+4], SZ_COLNAME)
	call strcpy (SKY, Memc[colnames+5*SZ_COLNAME+5], SZ_COLNAME)

	# Set up the format definitions.
	call strcpy ("%6d", Memc[colformat], SZ_COLFMT)
	call strcpy ("%6d", Memc[colformat+SZ_COLFMT+1], SZ_COLFMT)
	call strcpy ("%10.2f", Memc[colformat+2*SZ_COLFMT+2], SZ_COLFMT)
	call strcpy ("%10.2f", Memc[colformat+3*SZ_COLFMT+3], SZ_COLFMT)
	call strcpy ("%12.3f", Memc[colformat+4*SZ_COLFMT+4], SZ_COLFMT)
	call strcpy ("%15.7g", Memc[colformat+5*SZ_COLFMT+5], SZ_COLFMT)

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
	do i = 1, PSF_NOUTCOLS {
	    j = i - 1
	    Memi[collen+j] = 1
	}
	
	# Define the table.
	call tbcdef (tp, Memi[colpoint], Memc[colnames], Memc[colunits],
	    Memc[colformat], Memi[coldtype], Memi[collen], PSF_NOUTCOLS)

	# Create the table.
	call tbtcre (tp)

	# Write out the psf star.
	call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+psf_star-1],
	    Memr[DP_APYCEN(apsel)+psf_star-1], tx, ty, 1)
	call tbrpti (tp, Memi[colpoint], Memi[DP_APID(apsel)+psf_star-1], 1, 1)
	call tbrpti (tp, Memi[colpoint+1], group, 1, 1)
	call tbrptr (tp, Memi[colpoint+2], tx, 1, 1)
	call tbrptr (tp, Memi[colpoint+3], ty, 1, 1)
	call tbrptr (tp, Memi[colpoint+4], Memr[DP_APMAG(apsel)+psf_star-1],
	    1, 1)
	call tbrptr (tp, Memi[colpoint+5], Memr[DP_APMSKY(apsel)+psf_star-1],
	    1, 1)

	# Now write out the group.
	row = 2
	do i = nei1_star, nei2_star {
	    call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+i-1],
	        Memr[DP_APYCEN(apsel)+i-1], tx, ty, 1)
	    call tbrpti (tp, Memi[colpoint], Memi[DP_APID(apsel)+i-1], 1, row)
	    call tbrpti (tp, Memi[colpoint+1], group, 1, row)
	    call tbrptr (tp, Memi[colpoint+2], tx, 1, row)
	    call tbrptr (tp, Memi[colpoint+3], ty, 1, row)
	    call tbrptr (tp, Memi[colpoint+4], Memr[DP_APMAG(apsel)+i-1],
	        1, row)
	    call tbrptr (tp, Memi[colpoint+5], Memr[DP_APMSKY(apsel)+i-1],
	        1, row)
	    row = row + 1
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
	# Check to see if the PSF group file is open.
	if (tp == NULL)
	    return

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
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "IMAGE", Memc[outstr])

	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PHOTFILE", Memc[outstr])
	if (DP_COORDS(dao) == EOS)
	    call tbhadt (tp, "PSTFILE", "\"\"")
	else {
	    call dp_froot (DP_COORDS(dao), Memc[outstr], SZ_LINE)
	    call tbhadt (tp, "PSTFILE", Memc[outstr])
	}
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "PSFIMAGE", DP_PSFIMAGE(dao))
	call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "OPSTFILE", Memc[outstr])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call tbhadt (tp, "GRPSFILE", Memc[outstr])

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
int	tp			# the output file descriptor

pointer	sp, outstr, date, time
int	envfind()

begin
	# Check to see if the PSF group file is open.
	if (tp == NULL)
	    return

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
	call dp_sparam (tp, "DATE", Memc[date], "yyyy-mm-dd", "")
	call dp_sparam (tp, "TIME", Memc[time], "hh:mm:ss", "") 

	# Define the package and task.
	call dp_sparam (tp, "PACKAGE", "daophot", "name", "")
	call dp_sparam (tp, "TASK", "psf", "name", "") 

	# Define the input and output files.
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "IMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PHOTFILE", Memc[outstr], "filename", "")
	if (DP_COORDS(dao) == EOS)
	    call dp_sparam (tp, "PSTFILE", "\"\"", "filename", "")
	else {
	    call dp_froot (DP_COORDS(dao), Memc[outstr], SZ_LINE)
	    call dp_sparam (tp, "PSTFILE", Memc[outstr], "filename", "")
	}
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "PSFIMAGE", Memc[outstr], "imagename", "")
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "GRPSFILE", Memc[outstr], "filename", "")
	call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_LINE)
	call dp_sparam (tp, "OPSTFILE", Memc[outstr], "filename", "")

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

procedure dp_pxaddgrp (dao, im, tp, psf_star, nei1_star, nei2_star, group)

pointer	dao			# pointer to daophot structure
pointer	im			# the input image descriptor
int	tp			# the output file descriptor
int	psf_star		# the psf star index
int	nei1_star, nei2_star	# the first and last neighbour star indices
int	group			# current group

real	tx, ty
pointer	apsel
int	i

begin
	# Check to see if the PSF group file is open.
	if (tp == NULL)
	    return

	# Get some daophot pointers.
	apsel = DP_APSEL(dao)

	# Write out the psf star.
	call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+psf_star-1],
	    Memr[DP_APYCEN(apsel)+psf_star-1], tx, ty, 1)
	call fprintf (tp, PGR_DATASTR)
	    call pargi (Memi[DP_APID(apsel)+psf_star-1])
	    call pargi (group)
	    call pargr (tx)
	    call pargr (ty)
	    call pargr (Memr[DP_APMAG(apsel)+psf_star-1])
	    call pargr (Memr[DP_APMSKY(apsel)+psf_star-1])

	# Now write out the group.
	do i = nei1_star, nei2_star {
	    call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+i-1],
	        Memr[DP_APYCEN(apsel)+i-1], tx, ty, 1)
	    call fprintf (tp, PGR_DATASTR)
		call pargi (Memi[DP_APID(apsel)+i-1])
		call pargi (group)
	        call pargr (tx)
	        call pargr (ty)
	        call pargr (Memr[DP_APMAG(apsel)+i-1])
	        call pargr (Memr[DP_APMSKY(apsel)+i-1])
	}
end


# DP_PTADDGRP -- Add a new group to the existing PSF output group ST table.

procedure dp_ptaddgrp (dao, im, tp, psf_star, nei1_star, nei2_star, group)

pointer	dao			# pointer to daophot structure
pointer	im			# the input image descriptor
pointer	tp			# pointer to output table
int	psf_star		# the psf star index
int	nei1_star, nei2_star	# the first and last neighbor star indices
int	group			# current group

real	tx, ty
pointer	apsel, sp, colpoint
int	i, nrows
int	tbpsta()

begin
	# Check to see if the PSF group file is open.
	if (tp == NULL)
	    return

	# Allocate space for the column pointers.
	call smark (sp)
	call salloc (colpoint, PSF_NOUTCOLS, TY_INT)

	# Get some daophot pointers.
	apsel = DP_APSEL(dao)

	# Find the number of rows in the table and add on at the end.
	nrows = tbpsta (tp, TBL_NROWS)

	# Write out the psf star
	call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+psf_star-1],
	    Memr[DP_APYCEN(apsel)+psf_star-1], tx, ty, 1)
	nrows = nrows + 1
	call tbrpti (tp, Memi[colpoint], Memi[DP_APID(apsel)+psf_star-1],
	    1, nrows)
	call tbrpti (tp, Memi[colpoint+1], group, 1, nrows)
	call tbrptr (tp, Memi[colpoint+2], tx, 1, nrows)
	call tbrptr (tp, Memi[colpoint+3], ty, 1, nrows)
	call tbrptr (tp, Memi[colpoint+4], Memr[DP_APMAG(apsel)+psf_star-1],
	    1, nrows)
	call tbrptr (tp, Memi[colpoint+5], Memr[DP_APMSKY(apsel)+psf_star-1],
	    1, nrows)

	# Now write out the group.
	do i = nei1_star, nei2_star {
	    nrows = nrows + 1
	    call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+i-1],
	        Memr[DP_APYCEN(apsel)+i-1], tx, ty, 1)
	    call tbrpti (tp, Memi[colpoint], Memi[DP_APID(apsel)+i-1], 1, nrows)
	    call tbrpti (tp, Memi[colpoint+1], group, 1, nrows)
	    call tbrptr (tp, Memi[colpoint+2], tx, 1, nrows)
	    call tbrptr (tp, Memi[colpoint+3], ty, 1, nrows)
	    call tbrptr (tp, Memi[colpoint+4], Memr[DP_APMAG(apsel)+i-1], 1,
	        nrows)
	    call tbrptr (tp, Memi[colpoint+5], Memr[DP_APMSKY(apsel)+i-1], 1,
	        nrows)
	}

	call sfree (sp)
end
