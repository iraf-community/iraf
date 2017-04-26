include "../lib/daophotdef.h"
include "../lib/apseldef.h"

define	NCOLUMN 	6

define GR_DATASTR "%-9d%10t%-6d%16t%-10.3f%26t%-10.3f%36t%-12.3f%48t%-15.7g%80t \n"

# DP_XWRTSELECT -- Write out the groups into an ST Table.

procedure dp_xwrtselect (dao, grp, ngroup, group_id)

pointer	dao			# pointer to the daophot structure
pointer	grp			# pointer to group output file
int	ngroup			# number in the group
int	group_id		# the id of the group

int	i
pointer	apsel

begin
	# Get the daophot pointer.
	apsel = DP_APSEL(dao)

	# Write out the data.
	do i = 1, ngroup {
	    call fprintf (grp, GR_DATASTR)
		call pargi (group_id)
		call pargi (Memi[DP_APID(apsel)+i-1])
		call pargr (Memr[DP_APXCEN(apsel)+i-1])
		call pargr (Memr[DP_APYCEN(apsel)+i-1])
		call pargr (Memr[DP_APMAG(apsel)+i-1])
		call pargr (Memr[DP_APMSKY(apsel)+i-1])
	}
end


# DP_TWRTSELECT -- Write out the groups into an ST Table.

procedure dp_twrtselect (dao, grp, colpoint, ngroup, cur_group, row)

pointer	dao			# pointer to the daophot structure
pointer	grp			# pointer to group output file
pointer	colpoint[ARB]		# column pointers
int	ngroup			# number in group
int	cur_group		# current group

int	i, row
pointer	apsel

begin
	# Get the daophot pointer.
	apsel = DP_APSEL(dao)

	# Write out the data.
	do i = 1, ngroup {
	    row = row + 1
	    call tbrpti (grp, colpoint[1], Memi[DP_APID(apsel)+i-1], 1, row)
	    call tbrpti (grp, colpoint[2], cur_group, 1, row)
	    call tbrptr (grp, colpoint[3], Memr[DP_APXCEN(apsel)+i-1], 1, row)
	    call tbrptr (grp, colpoint[4], Memr[DP_APYCEN(apsel)+i-1], 1, row)
	    call tbrptr (grp, colpoint[5], Memr[DP_APMAG(apsel)+i-1], 1, row)
	    call tbrptr (grp, colpoint[6], Memr[DP_APMSKY(apsel)+i-1], 1, row)

	}
end


# DP_XGSELPARS -- Add various parameters to the header of the group table.

procedure dp_xgselpars (tp, min_group, max_group)

pointer	tp			# pointer to the table
int	min_group		# minimum group size
int	max_group		# maximum group size

begin
	# Add the min_group and max_group parameters.
	call dp_iparam (tp, "MINSZGROUP", min_group, "number", "")
	call dp_iparam (tp, "MAXSZGROUP", max_group, "number", "")
end


# DP_TGSELCOL -- Set the column pointers for the output file.

procedure dp_tgselcol (tp, colpoints)

pointer	tp		# table pointer
pointer	colpoints[ARB]	# column pointers

begin
	call tbcfnd (tp, ID, colpoints[1], 1)
	if (colpoints[1] == NULL)
	    call tbcfnd (tp, "ID", colpoints[1], 1)
	if (colpoints[1] == NULL)
	    call printf ("Error reading ID.\n")

	call tbcfnd (tp, GROUP, colpoints[2], 1)
	if (colpoints[2] == NULL)
	    call tbcfnd (tp, "GROUP", colpoints[2], 1)
	if (colpoints[2] == NULL)
	    call printf ("Error reading GROUP.\n")

	call tbcfnd (tp, XCENTER, colpoints[3], 1)
	if (colpoints[3] == NULL)
	    call tbcfnd (tp, "XCENTER", colpoints[3], 1)
	if (colpoints[3] == NULL)
	    call printf ("Error reading XCENTER.\n")

	call tbcfnd (tp, YCENTER, colpoints[4], 1)
	if (colpoints[4] == NULL)
	    call tbcfnd (tp, "YCENTER", colpoints[4], 1)
	if (colpoints[4] == NULL)
	    call printf ("Error reading YCENTER.\n")

	call tbcfnd (tp, MAG, colpoints[5], 1)
	if (colpoints[5] == NULL)
	    call tbcfnd (tp, APMAG, colpoints[5], 1)
	if (colpoints[5] == NULL)
	    call printf ("Error reading MAG.\n")

	call tbcfnd (tp, SKY, colpoints[6], 1)
	if (colpoints[6] == NULL)
	    call tbcfnd (tp, SKY, colpoints[6], 1)
	if (colpoints[6] == NULL)
	    call printf ("Error reading SKY.\n")
end


# DP_TGSELPARS -- Add various parameters to the header of the group table.

procedure dp_tgselpars (tp, min_group, max_group)

pointer	tp			# pointer to the table
int	min_group		# minimum group size
int	max_group		# maximum group size

begin
	# Add the min_group and max_group parameters.
	call tbhadi (tp, "MINSZGROUP", min_group)
	call tbhadi (tp, "MAXSZGROUP", max_group)
end
