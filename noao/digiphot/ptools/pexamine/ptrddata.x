include	"../../lib/ptkeysdef.h"
include	"pexamine.h"

# PT_RXYDATA -- Load the data for the input columns from the structure.

int procedure pt_rxydata (px, xptr, yptr)

pointer	px			# pointer to the pexamine structure
pointer	xptr			# pointer to the X coordinate array
pointer	yptr			# pointer to the Y coordinate array

int	data_invalid, field
pointer	sp, str
int	strdic()

begin
	data_invalid = NO

	# Allocate some temporary memory
	call smark (sp)
	call salloc (str, PX_SZCOLNAME, TY_CHAR)

	# Load the x column.
	field = strdic (PX_XCOLNAME(px), Memc[str], PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)])
	if (field > 0)
	    xptr = Memi[PX_COLPTRS(px)+field-1]
	else {
	    xptr = NULL
	    data_invalid = YES
	}

	# Load the y column.
	field = strdic (PX_YCOLNAME(px), Memc[str], PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)])
	if (field > 0)
	    yptr = Memi[PX_COLPTRS(px)+field-1]
	else {
	    yptr = NULL
	    data_invalid = YES
	}

	call sfree (sp)

	return (data_invalid)
end


# PT_RHDATA -- Load the data for the histogram column from the structure.

int procedure pt_rhdata (px, xptr)

pointer	px			# pointer to the pexamine structure
pointer	xptr			# array containing the x points

int	data_invalid, field
pointer	sp, str
int	strdic()

begin
	data_invalid = NO

	# Allocate some temporary memory
	call smark (sp)
	call salloc (str, PX_SZCOLNAME, TY_CHAR)

	# Load the x column.
	field = strdic (PX_HCOLNAME(px), Memc[str], PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)])
	if (field > 0)
	    xptr = Memi[PX_COLPTRS(px)+field-1]
	else {
	    xptr = NULL
	    data_invalid = YES
	}

	call sfree (sp)

	return (data_invalid)
end


# PT_RCOODATA -- Load the coordinate data from the structure.

int procedure pt_rcoodata (px, xptr, yptr)

pointer	px			# pointer to the pexamine structure
pointer	xptr			# pointer to x coordinates array
pointer	yptr			# pointer to y coordinates array

int	data_invalid, field
pointer	sp, str
int	strdic()

begin
	data_invalid = NO

	# Allocate some temporary memory
	call smark (sp)
	call salloc (str, PX_SZCOLNAME, TY_CHAR)

	# Load the x coordinate.
	field = strdic (PX_XPOSNAME(px), Memc[str], PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)])
	if (field > 0)
	    xptr = Memi[PX_COLPTRS(px)+field-1]
	else {
	    data_invalid = YES
	    xptr = NULL
	}

	# Load the y coordinate.
	field = strdic (PX_YPOSNAME(px), Memc[str], PX_SZCOLNAME,
	    Memc[PX_COLNAMES(px)])
	if (field > 0)
	    yptr = Memi[PX_COLPTRS(px)+field-1]
	else {
	    data_invalid = YES
	    yptr = NULL
	}

	call sfree (sp)

	return (data_invalid)
end
