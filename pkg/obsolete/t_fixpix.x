include	<imhdr.h>
include	<fset.h>

# FIXPIX -- Interpolate over bad columns and lines.

procedure t_fixpix ()

char	images[SZ_LINE]			# Image template
char	badpixels[SZ_FNAME]		# File containing the badpixel regions
bool	verbose				# Print image names and regions?

char	imname[SZ_FNAME]		# Image name
pointer	image				# Image pointer

int	list, badpix

int	open(), imtopen(), imtgetim()
bool	clgetb()
pointer	immap()

begin
	# Get the image template and expand.

	call clgstr ("images", images, SZ_LINE)
	call clgstr ("badpixels", badpixels, SZ_FNAME)
	verbose = clgetb ("verbose")
	if (verbose)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	list = imtopen (images)

	while (imtgetim (list, imname, SZ_FNAME) != EOF) {

	    if (verbose) {
		call printf ("fixpix: %s\n")
		    call pargstr (imname)
	    }
	    image = immap (imname, READ_WRITE, 0)
	    badpix = open (badpixels, READ_ONLY, TEXT_FILE)
	    call fixpix (image, badpix, verbose)
	    call imunmap (image)
	    call close (badpix)
	}

	call imtclose (list)
end

procedure fixpix (image, badpix, verbose)

pointer	image				# Image pointer
int	badpix				# File pointer
bool	verbose				# Print regions fixed?

int	x1, x2, y1, y2			# Bad region

int	temp, fscan(), nscan()

begin
	# Scan the bad pixel regions from the file.

	while (fscan (badpix) != EOF) {
	    call gargi (x1)
	    call gargi (x2)
	    call gargi (y1)
	    call gargi (y2)
	    if (nscan () != 4)
		next

	    if (x1 > x2) {
		temp = x1; x1 = x2; x2 = temp
	    }
	    if (y1 > y2) {
		temp = y1; y1 = y2; y2 = temp
	    }

	    # Check that the region is not the entire image.

	    if ((x1 == 1) && (x2 == IM_LEN (image, 1)) &&
		(y1 == 1) && (y2 == IM_LEN (image, 2))) {
		call eprintf ("Cannot fix an entire image")
		next
	    }

	    # If the bad region spans entire lines interpolate lines.
	    if ((x1 == 1) && (x2 == IM_LEN (image, 1)))
		call fixline (image, x1, x2, y1, y2, verbose)

	    # If the bad region spans entire columns interpolate columns.
	    else if ((y1 == 1) && (y2 == IM_LEN (image, 2)))
		call fixcolumn (image, x1, x2, y1, y2, verbose)

	    # If the bad region is longer in the columns interpolate columns.
	    else if ((x2 - x1) < (y2 - y1))
		call fixcolumn (image, x1, x2, y1, y2, verbose)

	    # If the bad region is longer in the lines interpolate lines.
	    else 
		call fixline (image, x1, x2, y1, y2, verbose)
	}
end

# FIXLINE -- Switch to the appropriate generic procedure to optimize the
# image I/O.

procedure fixline (image, x1, x2, y1, y2, verbose)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed
bool	verbose				# Print regions fixed?

begin
	if (verbose) {
	    call printf ("  Interpolate lines for region %d %d %d %d\n")
		call pargi (x1)
		call pargi (x2)
		call pargi (y1)
		call pargi (y2)
	}

	switch (IM_PIXTYPE (image)) {
	case TY_SHORT:
	    call fixlines (image, x1, x2, y1, y2)
	case TY_INT:
	    call fixlinei (image, x1, x2, y1, y2)
	case TY_USHORT, TY_LONG:
	    call fixlinel (image, x1, x2, y1, y2)
	case TY_REAL:
	    call fixliner (image, x1, x2, y1, y2)
	case TY_DOUBLE:
	    call fixlined (image, x1, x2, y1, y2)
	case TY_COMPLEX:
	    call fixlinex (image, x1, x2, y1, y2)
	default:
	    call eprintf ("Unknown pixel type")
	}
end

# FIXCOLUMN -- Switch to the appropriate generic procedure to optimize the
# image I/O.

procedure fixcolumn (image, x1, x2, y1, y2, verbose)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed
bool	verbose				# Print regions fixed?

begin
	if (verbose) {
	    call printf ("  Interpolate columns for region %d %d %d %d\n")
		call pargi (x1)
		call pargi (x2)
		call pargi (y1)
		call pargi (y2)
	}

	switch (IM_PIXTYPE (image)) {
	case TY_SHORT:
	    call fixcols (image, x1, x2, y1, y2)
	case TY_INT:
	    call fixcoli (image, x1, x2, y1, y2)
	case TY_USHORT, TY_LONG:
	    call fixcoll (image, x1, x2, y1, y2)
	case TY_REAL:
	    call fixcolr (image, x1, x2, y1, y2)
	case TY_DOUBLE:
	    call fixcold (image, x1, x2, y1, y2)
	case TY_COMPLEX:
	    call fixcolx (image, x1, x2, y1, y2)
	default:
	    call eprintf ("Unknown pixel type")
	}
end
