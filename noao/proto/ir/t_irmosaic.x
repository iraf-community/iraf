include <imhdr.h>
include <fset.h>
include "iralign.h"


# T_IRMOSAIC -- Procedure to combine a list of subrasters into a single large
# image.

procedure t_irmosaic ()

int	nxsub, nysub, nxoverlap, nyoverlap, raster, corner, order
int	ncols, nrows, nimages, nmissing, verbose, median, subtract
pointer	sp, outimage, database, section, nobs, ranges
pointer	str, imlist, outim, dt
real	oval

bool	clgetb()
int	btoi(), clgwrd(), imtlen()
int	decode_ranges()
pointer	imtopenp(), ir_setim(), dtmap()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary working space.
	call smark (sp)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (nobs, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (ranges, 3 * MAX_NRANGES + 1, TY_INT)

	# Get the image list, output image name and database file name.
	imlist = imtopenp ("input")
	call clgstr ("section", Memc[section], SZ_FNAME)
	call clgstr ("unobserved", Memc[nobs], SZ_FNAME)
	call clgstr ("output", Memc[outimage], SZ_FNAME)
	call clgstr ("database", Memc[database], SZ_FNAME)

	# Get the mosaicing parameters.
	corner = clgwrd ("corner", Memc[str], SZ_FNAME, ",ll,lr,ul,ur,")
	order = clgwrd ("direction", Memc[str], SZ_FNAME, ",row,column,")
	raster = btoi (clgetb ("raster"))

	# Check that the number of observed and missing images matches
	# the number of specified subrasters.
	if (Memc[nobs] == EOS) {
	    nmissing = 0
	    Memi[ranges] = 0
	    Memi[ranges+1] = 0
	    Memi[ranges+2] = 0
	    Memi[ranges+3] = NULL
	} else {
	    if (decode_ranges (Memc[nobs], Memi[ranges], MAX_NRANGES,
	        nmissing) == ERR)
	        call error (0, "Error decoding list of unobserved rasters.")
	}

	# Compute the output image characteristics.
	outim = ir_setim (imlist, Memc[section], Memc[outimage], nxsub, nysub,
	    nxoverlap, nyoverlap, ncols, nrows, oval)
	nimages = imtlen (imlist) + nmissing
	if (nimages != (nxsub * nysub))
	    call error (0,
	        "The number of images does not match the number of subrasters.")
	median = btoi (clgetb ("median"))
	subtract = btoi (clgetb ("subtract"))
	verbose = btoi (clgetb ("verbose"))

	# Write the parameters to the database file.
	dt = dtmap (Memc[database], APPEND)
	call ir_dtwparams (dt, Memc[outimage], Memc[section], ncols, nrows,
	    nxsub, nysub, nxoverlap, nyoverlap, corner, order, raster, oval)

	# Fill the output image with the blank value.
	call ir_imzero (outim, int (IM_LEN(outim,1)), int (IM_LEN(outim, 2)),
	    oval)

	# Make the output image.
	call ir_mkmosaic (imlist, Memc[section], Memi[ranges], outim, dt,
	    ncols, nrows, corner, order, raster, nxsub, nysub, nxoverlap,
	    nyoverlap, oval, median, subtract, verbose)

	# Close up files
	call dtunmap (dt)
	call imunmap (outim)
	call clpcls (imlist)
	call sfree (sp)
end


define	NTYPES	7

# IR_GET_IMTYPE -- Procedure to get the image type.

int procedure ir_get_imtype (c)

char	c	# character denoting the image type

int	i, typecodes[NTYPES]
int	stridx()
string	types "usilrdx"
data	typecodes /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE,
		   TY_COMPLEX/

begin
	i = stridx (c, types)
	if (i == 0)
	    return (ERR)
	else
	    return (typecodes[i])
end


# IR_SETIM -- Procedure to set up the output image characteristics.

pointer procedure ir_setim (list, section, outimage, nxsub, nysub, nxoverlap,
    nyoverlap, ncols, nrows, oval)

pointer	list		# pointer to list of input images
char	section[ARB]	# input image section
char	outimage[ARB]	# name of the output image
int	nxsub		# number of subrasters in the x direction
int	nysub		# number of subrasters in the y direction
int	nxoverlap	# number of columns of overlap
int	nyoverlap	# number of rows of overlap
int	ncols		# number of columns per subraster
int	nrows		# number of rows per subraster
real	oval		# pixel value of undefined regions

int	ijunk, opixtype, nimcols, nimrows
pointer	sp, image, im, outim
char	clgetc()
int	clgeti(), ir_get_imtype()
pointer	imtgetim(), immap()
real	clgetr()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Get the number of subrasters and the overlap parameters.
	nxsub = clgeti ("nxsub")
	nysub = clgeti ("nysub")
	nxoverlap = clgeti ("nxoverlap")
	nyoverlap = clgeti ("nyoverlap")

	# Get the individual subraster size.
	if (imtgetim (list, Memc[image], SZ_FNAME) == EOF)
	    call error (0, "Null length image list.")
	call strcat (section, Memc[image], SZ_FNAME)
	im = immap (Memc[image], READ_ONLY, 0)
	ncols = IM_LEN(im,1)
	nrows = IM_LEN(im,2)

	# Compute the size of the output image.
	nimcols = nxsub * ncols - (nxsub - 1) * nxoverlap
	nimrows = nysub * nrows - (nysub - 1) * nyoverlap
	ijunk = clgeti ("nimcols")
	if (! IS_INDEFI(ijunk))
	    nimcols = max (nimcols, ijunk)
	ijunk = clgeti ("nimrows")
	if (! IS_INDEFI(ijunk))
	    nimrows = max (nimrows, ijunk)

	# Set the output pixel type.
	opixtype = ir_get_imtype (clgetc ("opixtype"))
	if (opixtype == ERR)
	    opixtype = IM_PIXTYPE(im)

	# Set the output pixel value.
	oval = clgetr ("oval")

	# Open output image and set the parameters.
	outim = immap (outimage, NEW_IMAGE, 0)
	IM_NDIM(outim) = 2
	IM_LEN(outim,1) = nimcols
	IM_LEN(outim,2) = nimrows
	IM_PIXTYPE(outim) = opixtype

	call imunmap (im)
	call imtrew (list)
	call sfree (sp)
	return (outim)
end


# IR_LOOP -- Procedure to compute the ordering parameters for inserting the
# subrasters into the output image.

procedure ir_loop (corner, nxsub, nysub, order, kbegin, kend, kincr, jbegin,
    jend, jincr)

int	corner	# beginning corner for adding subrasters to the image
int	nxsub	# number of subrasters in the x direction
int	nysub	# number of subrasters in the y direction
int	order	# row or column order
int	kbegin	# beginning parameter for the inner loop
int	kend	# ending parameter for the inner loop
int	kincr	# inner loop increment
int	jbegin	# beginning parameter for the inner loop
int	jend	# ending parameter for the inner loop
int	jincr	# outer loop increment

begin
	switch (corner) {
	case IR_LL:
	    if (order == IR_ROW) {
	        kbegin = 1
	        kend = nxsub
	        kincr = 1
	        jbegin = 1
	        jend = nysub
	        jincr = 1
	    } else {
		kbegin = 1
		kend = nysub
		kincr = 1
		jbegin = 1
		jend = nxsub
		jincr = 1
	    }
	case IR_LR:
	    if (order == IR_ROW) {
	        kbegin = nxsub
	        kend = 1
	        kincr = -1
	        jbegin = 1
	        jend = nysub
	        jincr = 1
	    } else {
		kbegin = 1
		kend = nysub
		kincr = 1
		jbegin = nxsub
		jend = 1
		jincr = -1
	    }
	case IR_UL:
	    if (order == IR_ROW) {
	        kbegin = 1
	        kend = nxsub
	        kincr = 1
	        jbegin = nysub
	        jend = 1
	        jincr = -1
	    } else {
		kbegin = nysub
		kend = 1
		kincr = -1
		jbegin = 1
		jend = nxsub
		jincr = 1
	    }
	case IR_UR:
	    if (order == IR_ROW) {
	        kbegin = nxsub
	        kend = 1
	        kincr = -1
	        jbegin = nysub
	        jend = 1
	        jincr = -1
	    } else {
		kbegin = nysub
		kend = 1
		kincr = -1
		jbegin = nxsub
		jend = 1
		jincr = -1
	    }
	}
end


# IR_MKMOSAIC -- Procedure to make the mosaiced image.

procedure ir_mkmosaic (imlist, section, ranges, outim, dt, ncols, nrows, corner,
    order, raster, nxsub, nysub, nxoverlap, nyoverlap, oval, median,
    subtract, verbose)

pointer	imlist		# pointer to input image list
char	section[ARB]	# input image section
int	ranges[ARB]	# list of missing subrasters
pointer	outim		# pointer to the output image
pointer	dt		# pointer to the database file
int	ncols		# maximum number of columns in input image
int	nrows		# maximum number of rows in input image
int	corner		# which corner to begin mosaic
int	order		# row or column order
int	raster		# raster scan pattern
int	nxsub		# number of subrasters per output image column
int	nysub		# number of subrasters per output image row
int	nxoverlap	# number of columns of overlap
int	nyoverlap	# number of rows of overlap
real	oval		# pixel value of undefined output image regions
int	median	 	# compute the median of each subraster
int	subtract	# subtract the median off each subraster
int	verbose		# print output messages

int	k, j, kbegin, kend, kincr, jbegin, jend, jincr, ncount, next_missing
int	itemp
pointer	sp, inimage, subim
int	get_next_number()
pointer	imtgetim(), immap()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (inimage, SZ_FNAME, TY_CHAR)

	# Compute the looping parameters.
	call ir_loop (corner, nxsub, nysub, order, kbegin, kend, kincr, jbegin,
	    jend, jincr)

	# Initialize the counters
	ncount = 1
	next_missing = 0
	if (get_next_number (ranges, next_missing) == EOF)
	    next_missing = nxsub * nysub + 1

	# Write out the number of subraster
	call dtput (dt, "\tnsubrasters\t%d\n")
	    call pargi (nxsub * nysub)

	# Loop over the list of subrasters.
	do j = jbegin, jend, jincr {

	    do k = kbegin, kend, kincr {
		if (ncount < next_missing) {
		    if (imtgetim (imlist, Memc[inimage], SZ_FNAME) == EOF)
			call error (0, "Error reading subraster.")
		    call strcat (section, Memc[inimage], SZ_FNAME)
		    subim = immap (Memc[inimage], READ_ONLY, 0)
		    call ir_put_subraster (dt, subim, outim, ncols, nrows, oval,
			k, j, nxoverlap, nyoverlap, order, median, subtract,
			verbose)
		    call imunmap (subim)
		} else {
		    call ir_put_null (dt, outim, ncols, nrows, oval, k, j,
			nxoverlap, nyoverlap, order, median, subtract, verbose)
		    if (get_next_number (ranges, next_missing) == EOF)
			next_missing = nxsub * nysub + 1
		}
		ncount = ncount + 1
	    }

	    if (raster == YES) {
		itemp = kbegin
		kbegin = kend
		kend = itemp
		kincr = - kincr
	    }
	}

	call sfree (sp)
end


# IR_PUT_NULL -- Procedure to store a dummy subraster.

procedure ir_put_null (dt, outim, ncols, nrows, oval, k, j, nxoverlap,
    nyoverlap, order, median, subtract, verbose)

pointer	dt		# pointer to the database file
pointer	outim		# pointer to the output image
int	ncols		# number of columns in the subraster
int	nrows		# number of rows in the subraster
real	oval		# value of the output pixel
int	k		# index of the inner loop
int	j		# index of the outer loop
int	nxoverlap	# the amount of column overlap
int	nyoverlap	# the amount of row overlap
int	order		# row or column order
int	median		# compute the median value
int	subtract	# subtract the median from the image
int	verbose		# print verbose messages

int	nimcols, nimrows, c1, c2, l1, l2
pointer	buf
pointer	imps2r()

begin
	# Compute the column and row limits.
	nimcols = IM_LEN(outim,1)
	nimrows = IM_LEN(outim,2)

	# Compute the section limits.
	if (order == IR_ROW) {
	    c1 = max (1, min (1 + (k - 1) * (ncols - nxoverlap), nimcols))
	    c2 = min (nimcols, max (1, c1 + ncols - 1))
	    l1 = max (1, min (1 + (j - 1) * (nrows - nyoverlap), nimrows))
	    l2 = min (nimrows, max (1, l1 + nrows - 1))
	} else {
	    c1 = max (1, min (1 + (j - 1) * (ncols - nxoverlap), nimcols))
	    c2 = min (nimcols, max (1, c1 + ncols - 1))
	    l1 = max (1, min (1 + (k - 1) * (nrows - nyoverlap), nimrows))
	    l2 = min (nimrows, max (1, l1 + nrows - 1))
	}

	# Move the constant into buffer.
	buf = imps2r (outim, c1, c2, l1, l2)
	if (buf == NULL)
	    call error (0, "Error writing the image.\n")
	if (median == YES && subtract == YES)
	    call amovkr (0.0, Memr[buf], (c2 - c1 + 1) * (l2 - l1 + 1)) 
	else
	    call amovkr (oval, Memr[buf], (c2 - c1 + 1) * (l2 - l1 + 1)) 

	# Write the record to the data base.
	call dtput (dt,"\tnullraster\t%s[%d:%d,%d:%d]  %g  %s\n")
	    call pargstr (IM_HDRFILE(outim))
	    call pargi (c1)
	    call pargi (c2)
	    call pargi (l1)
	    call pargi (l2)
	if (median == YES)
	    call pargr (oval)
	else
	    call pargr (INDEFR)
	if (median == YES && subtract == YES)
	    call pargstr ("subtract")
	else
	    call pargstr ("")

	# Print messages on the standard output.
	if (verbose == YES) {
	    call printf ("imcopy  nullimage  %s[%d:%d,%d:%d]  %g  %s\n") 
		call pargstr (IM_HDRFILE(outim))
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    if (median == YES)
		call pargr (oval)
	    else
		call pargr (INDEFR)
	    if (median == YES && subtract == YES)
	        call pargstr ("subtract")
	    else
	        call pargstr ("")
	}
end


# IR_PUT_SUBRASTER -- Procedure to  output a subraster

procedure ir_put_subraster (dt, subim, outim, ncols, nrows, oval, k, j,
    nxoverlap, nyoverlap, order, median, subtract, verbose)

pointer	dt		# pointer to the database file
pointer	subim		# pointer to input subraster
pointer	outim		# pointer to the output image
int	ncols		# number of columns in the subraster
int	nrows		# number of rows in the subraster
real	oval		# value of the output pixel
int	k		# index of the inner loop
int	j		# index of the outer loop
int	nxoverlap	# the amount of column overlap
int	nyoverlap	# the amount of row overlap
int	order		# row or column order
int	median		# compute the median of the subraster
int	subtract	# subtract the median from the subraster
int	verbose		# print verbose messages

int	nimcols, nimrows, c1, c2, l1, l2
pointer	inbuf, outbuf
real	medval
pointer	imgs2r(), imps2r()
real	amedr()

begin
	# Compute the column and row limits.
	nimcols = IM_LEN(outim,1)
	nimrows = IM_LEN(outim,2)

	# Compute the output section limits.
	if (order == IR_ROW) {
	    c1 = max (1, min (1 + (k - 1) * (ncols - nxoverlap), nimcols))
	    c2 = min (nimcols, max (1, c1 + ncols - 1))
	    l1 = max (1, min (1 + (j - 1) * (nrows - nyoverlap), nimrows))
	    l2 = min (nimrows, max (1, l1 + nrows - 1))
	} else {
	    c1 = max (1, min (1 + (j - 1) * (ncols - nxoverlap), nimcols))
	    c2 = min (nimcols, max (1, c1 + ncols - 1))
	    l1 = max (1, min (1 + (k - 1) * (nrows - nyoverlap), nimrows))
	    l2 = min (nimrows, max (1, l1 + nrows - 1))
	}

	# Fetch the input pixels.
	inbuf = imgs2r (subim, 1, (c2 - c1 + 1), 1, (l2 - l1 + 1))
	if (inbuf == NULL)
	    call error (0, "Error reading image.")

	# Move the constant into buffer.
	outbuf = imps2r (outim, c1, c2, l1, l2)
	if (outbuf == NULL)
	    call error (0, "Error writing the image.")
	call amovr (Memr[inbuf], Memr[outbuf], (c2 - c1 + 1) * (l2 - l1 + 1)) 
	if (median == YES) {
	    medval = amedr (Memr[inbuf], (c2 - c1 + 1) * (l2 - l1 + 1))
	    if (subtract == YES)
		call asubkr (Memr[outbuf], medval, Memr[outbuf],
		    (c2 - c1 + 1) * (l2 - l1 + 1)) 
	} else
	    medval = INDEFR

	call dtput (dt,"\t%s\t%s[%d:%d,%d:%d]\t%g\t%s\n")
	    call pargstr (IM_HDRFILE(subim))
	    call pargstr (IM_HDRFILE(outim))
	    call pargi (c1)
	    call pargi (c2)
	    call pargi (l1)
	    call pargi (l2)
	    call pargr (medval)
	if (median == YES && subtract == YES)
	    call pargstr ("subtract")
	 else
	    call pargstr ("")

	if (verbose == YES) {
	    call printf ("imcopy  %s  %s[%d:%d,%d:%d]  %g  %s\n") 
		call pargstr (IM_HDRFILE(subim))
		call pargstr (IM_HDRFILE(outim))
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
		call pargr (medval)
	    if (median == YES && subtract == YES)
		call pargstr ("subtract")
	    else
	        call pargstr ("")
	}
end
