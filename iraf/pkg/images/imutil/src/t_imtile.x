include <imhdr.h>
include <fset.h>
include "imtile.h"


# T_IMTILE -- Combine a list of same-size subrasters into a single large
# mosaiced image.

procedure t_imtile ()

int	nimages, subtract, verbose
long	nmissing
pointer	it, sp, outimage, trimsection, medsection, nullinput, ranges
pointer	str, index, c1, c2, l1, l2, isnull, median, imlist, outim
size_t	sz_val

bool	clgetb()
char	clgetc()
int	btoi(), clgwrd(), imtlen(), decode_ranges(), it_get_imtype()
long	clgetl()
pointer	imtopenp(), it_setim()
real	clgetr()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)
	sz_val = LEN_IRSTRUCT
	call malloc (it, sz_val, TY_STRUCT)

	# Allocate temporary working space.
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (outimage, sz_val, TY_CHAR)
	call salloc (trimsection, sz_val, TY_CHAR)
	call salloc (medsection, sz_val, TY_CHAR)
	call salloc (nullinput, sz_val, TY_CHAR)
	sz_val = 3 * MAX_NRANGES + 1
	call salloc (ranges, sz_val, TY_LONG)
	sz_val = SZ_FNAME
	call salloc (str, sz_val, TY_CHAR)

	# Get the input image list and the output image name.
	imlist = imtopenp ("input")
	call clgstr ("output", Memc[outimage], SZ_FNAME)
	call clgstr ("trim_section", Memc[trimsection], SZ_FNAME)
	call clgstr ("missing_input", Memc[nullinput], SZ_FNAME)
	call clgstr ("median_section", Memc[medsection], SZ_FNAME)
	if (Memc[medsection] == EOS)
	    subtract = NO
	else
	    subtract = btoi (clgetb ("subtract"))
	verbose = btoi (clgetb ("verbose"))

	# Get the mosaicing parameters.
	IT_NXSUB(it) = clgetl ("nctile")
	IT_NYSUB(it) = clgetl ("nltile")
	IT_CORNER(it) = clgwrd ("start_tile", Memc[str], SZ_FNAME,
	    ",ll,lr,ul,ur,")
	if (clgetb ("row_order"))
	    IT_ORDER(it) = IT_ROW
	else
	    IT_ORDER(it) = IT_COLUMN
	IT_RASTER(it) = btoi (clgetb ("raster_order"))
	IT_NXOVERLAP(it) = clgetl ("ncoverlap")
	IT_NYOVERLAP(it) = clgetl ("nloverlap")
	IT_OVAL(it) = clgetr ("ovalue")

	# Check that the number of observed and missing images matches
	# the number of specified subrasters.
	if (Memc[nullinput] == EOS) {
	    nmissing = 0
	    Meml[ranges] = 0
	    Meml[ranges+1] = 0
	    Meml[ranges+2] = 1
	    Meml[ranges+3] = NULL
	} else {
	    if (decode_ranges (Memc[nullinput], Meml[ranges], MAX_NRANGES,
	        nmissing) == ERR)
	        call error (0, "Error decoding list of unobserved rasters.")
	}
	nimages = imtlen (imlist) + nmissing
	if (nimages != (IT_NXSUB(it) * IT_NYSUB(it)))
	    call error (0,
	        "The number of input images is not equal to nxsub * nysub.")

	# Compute the output image characteristics and open the output image.
	outim = it_setim (it, imlist, Memc[trimsection], Memc[outimage],
	    clgetl ("ncols"), clgetl ("nlines"), it_get_imtype (clgetc (
	    "opixtype")))

	# Allocate space for and setup the section descriptors.
	sz_val = nimages
	call salloc (index, sz_val, TY_LONG)
	call salloc (c1, sz_val, TY_LONG)
	call salloc (c2, sz_val, TY_LONG)
	call salloc (l1, sz_val, TY_LONG)
	call salloc (l2, sz_val, TY_LONG)
	call salloc (isnull, sz_val, TY_INT)
	call salloc (median, sz_val, TY_REAL)

	call it_setup (it, imlist, Meml[ranges], Memc[trimsection],
	    Memc[medsection], outim, Meml[index], Meml[c1], Meml[c2],
	    Meml[l1], Meml[l2], Memi[isnull], Memr[median])

	# Make the output image.
	call it_mkmosaic (imlist, Memc[trimsection], outim, Meml[index],
	    Meml[c1], Meml[c2], Meml[l1], Meml[l2], Memi[isnull],
	    Memr[median], IT_NXSUB(it), IT_NYSUB(it), IT_OVAL(it), subtract)

        # Printe the results.
	if (verbose == YES) {
            call it_show (imlist, Memc[trimsection], Memc[outimage],
		Meml[index], Meml[c1], Meml[c2], Meml[l1], Meml[l2],
		Memi[isnull], Memr[median], IT_NXSUB(it)*IT_NYSUB(it), subtract)
	}

	# Close up files and free space.
	call imunmap (outim)
	call clpcls (imlist)
	call sfree (sp)
	call mfree (it, TY_STRUCT)
end


define	NTYPES	7

# IT_GET_IMTYPE -- Procedure to get the image type.

int procedure it_get_imtype (c)

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


# IT_SETUP -- Setup the data base parameters for the images.

procedure it_setup (it, imlist, ranges, trimsection, medsection, outim,
	index, c1, c2, l1, l2, isnull, median)

pointer	it			# pointer to the imtil structure
pointer	imlist			# pointer to the list of input images
long	ranges[ARB]		# list of missing subrasters
char	trimsection[ARB]	# input image section for output
char	medsection[ARB]		# input image section for median computation
pointer	outim			# pointer to the output image
long	index[ARB]		# index array
long	c1[ARB]			# array of beginning column limits
long	c2[ARB]			# array of ending column limits
long	l1[ARB]			# array of beginning line limits
long	l2[ARB]			# array of ending line limits
int	isnull[ARB]		# output input image order number
real	median[ARB]		# output median of input image

size_t	sz_val
pointer	p_val
long	l_val
int	imcount
long	i, j, k, nimrows, nimcols, next_null
pointer	sp, imname, im, buf
long	get_next_number()
int	imtgetim()
pointer	immap(), imgs2r()
real	amedr()

begin
	nimcols = IM_LEN(outim,1)
	nimrows = IM_LEN(outim,2)

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (imname, sz_val, TY_CHAR)

	imcount = 1
	next_null = 0
	if (get_next_number (ranges, next_null) == EOF)
	    next_null = IT_NXSUB(it) * IT_NYSUB(it) + 1

	# Loop over the input images.
	do i = 1, IT_NXSUB(it) * IT_NYSUB(it) {

	    # Set the indices array.
	    call it_indices (i, j, k, IT_NXSUB(it), IT_NYSUB(it),
	        IT_CORNER(it), IT_RASTER(it), IT_ORDER(it))
	    index[i] = i
	    c1[i] = max (1, min (1 + (j - 1) * (IT_NCOLS(it) -
	        IT_NXOVERLAP(it)), nimcols))
	    c2[i] = min (nimcols, max (1, c1[i] + IT_NCOLS(it) - 1))
	    l1[i] = max (1, min (1 + (k - 1) * (IT_NROWS(it) -
	        IT_NYOVERLAP(it)), nimrows))
	    l2[i] = min (nimrows, max (1, l1[i] + IT_NROWS(it) - 1))

	    # Set the index of each image in the image template
	    # and compute the median of the subraster.
	    if (i < next_null) {
		isnull[i] = imcount
		if (medsection[1] != EOS) {
		    if (imtgetim (imlist, Memc[imname], SZ_FNAME) == EOF)
			call error (0, "Error reading input image list.")
		    call strcat (medsection, Memc[imname], SZ_FNAME)
		    p_val = TY_CHAR
		    im = immap (Memc[imname], READ_ONLY, p_val)
		    l_val = 1
		    buf = imgs2r (im, l_val, IM_LEN(im,1), l_val, IM_LEN(im,2))
		    sz_val = IM_LEN(im,1) * IM_LEN(im,2)
		    median[i] = amedr (Memr[buf], sz_val)
		    call imunmap (im)
		} else
		    median[i] = INDEFR
		imcount = imcount + 1
	    } else {
		isnull[i] = 0
		if (medsection[1] == EOS)
		    median[i] = INDEFR
		else
		    median[i] = IT_OVAL(it)
		if (get_next_number (ranges, next_null) == EOF)
	    	    next_null = IT_NXSUB(it) * IT_NYSUB(it) + 1
	    }

	}

	call imtrew (imlist)
	call sfree (sp)
end


# IT_SETIM -- Procedure to set up the output image characteristics.

pointer procedure it_setim (it, list, trimsection, outimage, nimcols, nimrows,
	opixtype)

pointer	it		# pointer to the imtile structure
pointer	list		# pointer to list of input images
char	trimsection[ARB]# input image section
char	outimage[ARB]	# name of the output image
long	nimcols		# number of output image columns
long	nimrows		# number of output image rows
int	opixtype	# output image pixel type

size_t	sz_val
long	ijunk, nc, nr
pointer	sp, imname, im, outim
int	imtgetim()
pointer	immap()
include	<nullptr.inc>

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (imname, sz_val, TY_CHAR)

	# Get the size of the first subraster.
	if (imtgetim (list, Memc[imname], SZ_FNAME) != EOF) {
	    call strcat (trimsection, Memc[imname], SZ_FNAME)
	    im = immap (Memc[imname], READ_ONLY, NULLPTR)
	    IT_NCOLS(it) = IM_LEN(im,1)
	    IT_NROWS(it) = IM_LEN(im,2)
	    call imunmap (im)
	    call imtrew (list)
	} else
	    call error (0, "Error reading first input image.\n")

	# Compute the size of the output image.
	ijunk = IT_NXSUB(it) * IT_NCOLS(it) - (IT_NXSUB(it) - 1) *
	    IT_NXOVERLAP(it)
	if (IS_INDEFL(nimcols))
	    nc = ijunk
	else
	    nc = max (nimcols, ijunk)
	ijunk = IT_NYSUB(it) * IT_NROWS(it) - (IT_NYSUB(it) - 1) *
	    IT_NYOVERLAP(it)
	if (IS_INDEFL(ijunk))
	    nr = ijunk
	else
	    nr = max (nimrows, ijunk)

	# Set the output pixel type.
	if (opixtype == ERR)
	    opixtype = TY_REAL

	# Open output image and set the parameters.
	outim = immap (outimage, NEW_IMAGE, NULLPTR)
	IM_NDIM(outim) = 2
	IM_LEN(outim,1) = nc
	IM_LEN(outim,2) = nr
	IM_PIXTYPE(outim) = opixtype

	call sfree (sp)

	return (outim)
end


# IT_MKMOSAIC -- Procedure to make the mosaiced image.

procedure it_mkmosaic (imlist, trimsection, outim, index, c1, c2, l1, l2,
	isnull, median, nxsub, nysub, oval, subtract)

pointer	imlist		# pointer to input image list
char	trimsection[ARB]# input image section
pointer	outim		# pointer to the output image
long	index[ARB]	# index array for sorting the images
long	c1[ARB]		# array of column beginnings
long	c2[ARB]		# array of column endings
long	l1[ARB]		# array of line beginnings
long	l2[ARB]		# array of line endings
int	isnull[ARB]	# index of input image in the template
real	median[ARB]	# array of input image median values
long	nxsub		# number of subrasters per output image column
long	nysub		# number of subrasters per output image row
real	oval		# pixel value of undefined output image regions
int	subtract	# subtract the median off each subraster

size_t	sz_val
long	i, j, olineptr, ll1, ll2
size_t	noutcols, noutlines
pointer	sp, inimage, imptrs, buf
int	imtrgetim()
pointer	immap(), impl2r()
include	<nullptr.inc>

begin
	# Allocate temporary space.
	call smark (sp)
	sz_val = nxsub
	call salloc (imptrs, sz_val, TY_POINTER)
	sz_val = SZ_FNAME
	call salloc (inimage, sz_val, TY_CHAR)

	# Sort the subrasters on the yindex.
	do i = 1, nxsub * nysub
	    index[i] = i
	call rg_qsortl (l1, index, index, nxsub * nysub)

	noutcols = IM_LEN(outim,1)
	noutlines = IM_LEN(outim,2)

	# Loop over the input images.
	olineptr = 1
	do i = 1, nxsub * nysub, nxsub {

	    # Compute the line and column limits.
	    ll1 = l1[index[i]]
	    ll2 = l2[index[i]]

	    # Open the nxsub input images.
	    do j = i, i + nxsub - 1 {
		if (isnull[index[j]] <= 0) {
		    Memc[inimage] = EOS
		    Memp[imptrs+j-i] = NULL
		} else {
		    if (imtrgetim (imlist, isnull[index[j]], Memc[inimage],
		        SZ_FNAME) == EOF)
			Memp[imptrs+j-i] = NULL
		    else {
			call strcat (trimsection, Memc[inimage], SZ_FNAME)
			Memp[imptrs+j-i] = immap (Memc[inimage], READ_ONLY, NULLPTR)
		    }
		}
	    }

	    # Write out the undefined lines.
	    while (olineptr < ll1) {
		buf = impl2r (outim, olineptr)
		call amovkr (oval, Memr[buf], noutcols)
		olineptr = olineptr + 1
	    }

	    # Write the output lines.
	    call it_mklines (Memp[imptrs], outim, index, c1, c2, ll1, ll2,
	        median, i, nxsub, oval, subtract)
	    olineptr = ll2 + 1

	    # Close up the images.
	    # Open the nxsub input images.
	    do j = i, i + nxsub - 1 {
		if (Memp[imptrs+j-i] != NULL)
		    call imunmap (Memp[imptrs+j-i])
	    }

	}

	# Write out the remaining undefined lines.
	while (olineptr < noutlines) {
	    buf = impl2r (outim, olineptr)
	    call amovkr (oval, Memr[buf], noutcols)
	    olineptr = olineptr + 1
	}

	call sfree (sp)
end


# IT_MKLINES -- Construct and output image lines.

procedure it_mklines (imptrs, outim, index, c1, c2, l1, l2, meds, init, nsub, 
	oval, subtract)

pointer	imptrs[ARB]		# array of input image pointers
pointer	outim			# output imnage pointer
long	index[ARB]		# array of indices
long	c1[ARB]			# array of beginning columns
long	c2[ARB]			# array of ending columns
long	l1			# beginning line
long	l2			# ending line
real	meds[ARB]		# array of median values
long	init			# first index
long	nsub			# number of subrasters
real	oval			# output value
int	subtract		# subtract the median value

long	i, j, jj
size_t	noutcols
pointer	obuf, ibuf
pointer	impl2r(), imgl2r()

begin
	noutcols = IM_LEN(outim, 1)
	do i = l1, l2 {
	    obuf = impl2r (outim, i)
	    call amovkr (oval, Memr[obuf],  noutcols)
	    do j = 1, nsub {
		jj = index[j+init-1]
		if (imptrs[j] != NULL) {
		    ibuf = imgl2r (imptrs[j], i - l1 + 1)
		    if (subtract == YES)
		        call asubkr (Memr[ibuf], meds[jj], Memr[obuf+c1[jj]-1],
			    c2[jj] - c1[jj] + 1)
		    else
		        call amovr (Memr[ibuf], Memr[obuf+c1[jj]-1], c2[jj] -
			    c1[jj] + 1)
		}
	    }
	}
end


# IT_INDICES -- Given the number in the list for a missing subraster and
# information about how the subrasters were written return the i and j
# indices of the specified subrasters.

procedure it_indices (num, i, j, nxsub, nysub, corner, raster, order)

long	num		# number of the subraster
long	i,j		# indices of the subraster
long	nxsub,nysub	# number of subrasters in x and y
int	corner		# starting corner
int	raster		# raster order
int	order		# column or row order

long	c_2
long	lmod()

begin
	c_2 = 2

	switch (corner) {
	case IT_LL:
	    if (order == IT_ROW) {
		if (lmod(num, nxsub) == 0) {
		    j = num / nxsub
		    if (raster == YES && lmod(j,c_2) == 0)
			i = 1
		    else
		        i = nxsub
		} else {
		    j = num / nxsub + 1
		    if (raster == YES && lmod(j,c_2) == 0)
			i = nxsub - lmod(num, nxsub) + 1
		    else
		        i = lmod(num, nxsub)
		}
	    } else if (order == IT_COLUMN) {
		if (lmod(num, nysub) == 0) {
		    i = num / nysub
		    if (raster == YES && lmod(i,c_2) == 0)
			j = 1
		    else
		        j = nysub
		} else {
		    i = num / nysub + 1
		    if (raster == YES && lmod(i,c_2) == 0)
			j = nysub - lmod(num, nysub) + 1
		    else
		        j = lmod(num, nysub)
		}
	    }
	case IT_LR:
	    if (order == IT_ROW) {
		if (lmod(num, nxsub) == 0) {
		    j = num / nxsub
		    if (raster == YES && lmod(j,c_2) == 0)
			i = nxsub
		    else
			i = 1
		} else {
		    j = num / nxsub + 1
		    if (raster == YES && lmod(j,c_2) == 0)
			i = lmod(num, nxsub)
		    else
			i = nxsub - lmod(num, nxsub) + 1
		}
	    } else if (order == IT_COLUMN) {
		if (lmod(num, nysub) == 0) {
		    i = nxsub - num / nysub + 1
		    if (raster == YES && lmod(i,c_2) != 0)
			j = 1
		    else
		        j = nysub
		} else {
		    i = nxsub - num / nysub
		    if (raster == YES && lmod(i,c_2) != 0)
			j = nysub - lmod(num, nysub) + 1
		    else
		        j = lmod(num, nysub)
		}
	    }
	case IT_UL:
	    if (order == IT_ROW) {
		if (lmod(num, nxsub) == 0) {
		    j = nysub - num / nxsub + 1
		    if (raster == YES && lmod(j,c_2) != 0)
			i = 1
		    else
		        i = nxsub
		} else {
		    j = nysub - num / nxsub
		    if (raster == YES && lmod(j,c_2) != 0)
			i = nxsub - lmod(num, nxsub) + 1
		    else
		        i = lmod(num, nxsub)
		}
	    } else if (order == IT_COLUMN) {
		if (lmod(num, nysub) == 0) {
		    i = num / nysub
		    if (raster == YES && lmod(i,c_2) == 0)
			j = nysub
		    else
			j = 1
		} else {
		    i = num / nysub + 1
		    if (raster == YES && lmod(i,c_2) == 0)
			j = lmod(num, nysub)
		    else
			j = nysub - lmod(num, nysub) + 1
		}
	    }
	case IT_UR:
	    if (order == IT_ROW) {
		if (lmod(num, nxsub) == 0) {
		    j = nysub - num / nxsub + 1
		    if (raster == YES && lmod(j,c_2) != 0)
			i = nxsub
		    else
			i = 1
		} else {
		    j = nysub - num / nxsub
		    if (raster == YES && lmod(j,c_2) != 0)
			i = lmod(num, nxsub)
		    else
			i = nxsub - lmod(num, nxsub) + 1
		}
	    } else if (order == IT_COLUMN) {
		if (lmod(num, nysub) == 0) {
		    i = nxsub - num / nysub + 1
		    if (raster == YES && lmod(i,c_2) != 0)
			j = nysub
		    else
			j = 1
		} else {
		    i = nxsub - num / nysub
		    if (raster == YES && lmod(i,c_2) != 0)
			j = lmod(num, nysub)
		    else
			j = nysub - lmod(num, nysub) + 1
		}
	    }
	}
end


# IT_SHOW -- List the results.

procedure it_show (imlist, trimsection, outimage, index, c1, c2, l1,
        l2, isnull, median, nsub, subtract)

pointer	imlist          # input image list
char    trimsection[ARB]# trim section of input image
char    outimage[ARB]   # output image
long	index[ARB]      # array of sorted indices (not used at present)
long	c1[ARB]         # array of beginning column limits
long	c2[ARB]         # array of ending column limits
long	l1[ARB]         # array of beginning line limits
long	l2[ARB]         # array of ending line limits
int	isnull[ARB]     # image name index
real    median[ARB]     # array of medians
long	nsub            # number of subrasters
int	subtract        # subtract the median from the subraster

size_t	sz_val
long	i
pointer sp, imname
int	imtrgetim()

begin
        call smark (sp)
        sz_val = SZ_FNAME
        call salloc (imname, sz_val, TY_CHAR)

        do i = 1, nsub {

            if (isnull[i] <= 0)
                call strcpy ("nullimage", Memc[imname], SZ_FNAME)
            else if (imtrgetim (imlist, isnull[i], Memc[imname],
                SZ_FNAME) != EOF)
                call strcat (trimsection, Memc[imname], SZ_FNAME)
            else
                Memc[imname] = EOS

            call printf ("imcopy  %s  %s[%d:%d,%d:%d]  %g  %g\n")
                call pargstr (Memc[imname])
                call pargstr (outimage)
                call pargl (c1[i])
                call pargl (c2[i])
                call pargl (l1[i])
                call pargl (l2[i])
                call pargr (median[i])
            if (subtract == YES)
                call pargr (-median[i])
            else
                call pargr (0.0)
        }

        call sfree (sp)
end



