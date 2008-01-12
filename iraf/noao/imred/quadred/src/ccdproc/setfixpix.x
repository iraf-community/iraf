include	<imhdr.h>
include	"ccdred.h"

# SET_FIXPIX -- Setup for fixing bad pixels.
#
#   1.  Return immediately if the bad pixel correction is not requested or
#	if the image has been previously corrected.
#   2.  Determine the bad pixel correction file.  This may be specified
#	directly or indirectly through the image header or symbol table.
#	Return warning if not found.
#   3.  Read through the file collecting the bad pixel regions into a
#       bad column array (regions to be interpolated across columns) and
#	a bad line array (regions to be interpolated across lines).
#   4.  Set the processing flag.
#   5.  Log the operation (to user, logfile, and output image header).

procedure set_fixpix (ccd)

pointer	ccd			# CCD structure

int	fd, nc, nl, c1, c2, l1, l2, dc, dl, nbadcols, nbadlines
pointer	sp, image, str, badcols, badlines

int	open(), fscan(), nscan(), strmatch()
bool	clgetb(), streq(), ccdflag()
errchk	open

begin
	# Check if the user wants this operation or it has been done.
	if (!clgetb ("fixpix") || ccdflag (IN_IM(ccd), "fixpix"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the bad pixel file.  If the name is "image" then get the file
	# name from the image header or symbol table.

	call clgstr ("fixfile", Memc[image], SZ_FNAME)
	if (streq (Memc[image], "image"))
	    call hdmgstr (IN_IM(ccd), "fixfile", Memc[image], SZ_FNAME)

	# If no processing is desired print message and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Bad pixel file is %s\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Open the file and read the bad pixel regions.  Use dynamic memory.
	# Set the bad pixel coordinates.  By default the bad pixel coordinates
	# refer to the image directly but if the word "untrimmed" appears
	# in a comment then the coordinates refer to the CCD coordinates.

	fd = open (Memc[image], READ_ONLY, TEXT_FILE)
	dc = 0
	dl = 0
	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)
	nbadcols = 0
	nbadlines = 0
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    if (Memc[str] == '#') {
		call gargstr (Memc[str], SZ_LINE)
		if (strmatch (Memc[str], "{untrimmed}") != 0) {
		    dc = IN_C1(ccd) - CCD_C1(ccd)
		    dl = IN_L1(ccd) - CCD_L1(ccd)
		}
	        next
	    }
		    
	    call reset_scan()
	    call gargi (c1)
	    call gargi (c2)
	    call gargi (l1)
	    call gargi (l2)

	    # Ignore badly specified lines.
	    if (nscan() != 4) {
		if (nscan() == 2) {
		    l1 = c2
		    c2 = c1
		    l2 = l1
		} else
		    next
	    }

	    # Do the coordinate conversion.
	    c1 = max (IN_C1(ccd), c1 + dc)
	    c2 = min (IN_C2(ccd), c2 + dc)
	    l1 = max (IN_L1(ccd), l1 + dl)
	    l2 = min (IN_L2(ccd), l2 + dl)

	    # Ignore an inproperly specified region.
	    if ((c1 > c2) || (l1 > l2))
		next

	    # Interpolate across the shortest direction.
	    if ((l2 - l1) < (c2 - c1)) {
		nbadlines = nbadlines + 1
		if (nbadlines == 1)
		    call calloc (badlines, 2*nl*nbadlines, TY_SHORT)
		else {
		    call realloc (badlines, 2*nl*nbadlines, TY_SHORT)
		    call aclrs (Mems[badlines+2*nl*(nbadlines-1)], 2*nl)
		}
		call set_badcols (c1, c2, l1, l2, Mems[badlines],
		    nl, nbadlines)

	    } else {
		nbadcols = nbadcols + 1
		if (nbadcols == 1)
		    call calloc (badcols, 2*nl*nbadcols, TY_SHORT)
		else {
		    call realloc (badcols, 2*nl*nbadcols, TY_SHORT)
		    call aclrs (Mems[badcols+2*nl*(nbadcols-1)], 2*nl)
		}
		call set_badcols (c1, c2, l1, l2, Mems[badcols],
		    nl, nbadcols)
	    }
	}
	call close (fd)

	# Set structure parameters and the correction flags.
	if (nbadcols != 0) {
	    NBADCOLS(ccd) = nbadcols
	    BADCOLS(ccd) = badcols
	    CORS(ccd, FIXPIX) = YES
	    COR(ccd) = YES
	}
	if (nbadlines != 0) {
	    NBADLINES(ccd) = nbadlines
	    BADLINES(ccd) = badlines
	    CORS(ccd, FIXPIX) = YES
	    COR(ccd) = YES
	}

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE, "Bad pixel file is %s")
	    call pargstr (Memc[image])
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "fixpix", Memc[str])

	call sfree (sp)
end


# SET_BADCOLS -- Enter bad columns in a bad column array.
# This procedure is used both for the line and column interpolation arrays.
# The bad column array contains the starting and ending bad columns for
# each line.  This allows quick look up when processing the image at the
# expense of memory.  A column index of zero indicates no further bad columns
# in the line.

procedure set_badcols (c1, c2, l1, l2, array, nl, nbadcols)

int	c1, c2, l1, l2			# Bad column
short	array[2,nl,nbadcols]		# Bad column array
int	nl				# Number of image lines
int	nbadcols			# Number of bad column areas

int	i, j

begin
	# For each line in the bad columns set the columns
	# in the first unused entry in the array.

	do i = l1, l2 {
	    do j = 1, nbadcols {
		if (array[1,i,j] == 0) {
		    array[1,i,j] = c1
		    array[2,i,j] = c2
		    break
		}
	    }
	}
end
