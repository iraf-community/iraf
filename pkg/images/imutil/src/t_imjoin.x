include	<imhdr.h>
include	<error.h>
include	<syserr.h>

define	DEFBUFSIZE		65536		# default IMIO buffer size
define	FUDGE			0.8		# fudge factor


# T_IMJOIN -- Produce a single output image from a list of input images
# by joining the images in the input image list along a single dimension.
# The set of input images need have the same number of dimensions and
# elements per dimension ONLY along the axes not being joined.
# The output pixel type will be converted to the highest precedence pixel
# type if not all the images do not have the same pixel type.

procedure t_imjoin()

int	i, j, joindim, list, nimages, inpixtype, ndim, nelems[IM_MAXDIM]
int	bufsize, maxsize, memory, oldsize, outpixtype, verbose
pointer	sp, in, out, im, im1, input, output

bool	clgetb()
#char	clgetc()
int	imtopenp(), imtlen(), imtgetim(), clgeti(), btoi()
int	getdatatype(), ij_tymax(), sizeof(), begmem(), errcode()
pointer	immap()
errchk	immap

define	retry_	99

begin
	# Allocate working space.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# Get the parameters. Note that clgetc no longer accepts a blank
	# string as input so clgstr is used to fetch the pixtype parameter
	# and input is used as the temporary holding variable.
	list = imtopenp ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	joindim = clgeti ("join_dimension")
	#outpixtype = getdatatype (clgetc ("pixtype"))
	call clgstr ("pixtype", Memc[input], SZ_FNAME)
	outpixtype = getdatatype (Memc[input])
	verbose = btoi (clgetb ("verbose"))

	# Check to make sure that the input image list is not empty.
	nimages = imtlen (list)
	if (nimages == 0) {
	    call imtclose (list)
	    call sfree (sp)
	    call error (0, "The input image list is empty")
	} else
	    call salloc (in, nimages, TY_POINTER)

	# Check the the join dimension is not too large.
	if (joindim > IM_MAXDIM)
	    call error (0,
	    "The join dimension cannot be greater then the current IM_MAXDIM")

	bufsize = 0

retry_

	# Map the input images.
	nimages = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    nimages = nimages + 1
	    Memi[in+nimages-1] = immap (Memc[input], READ_ONLY, 0)
	}

	# Determine the dimensionality, size, and pixel type of the output
	# image. Force the output image to have the same number of dimensions
	# as the input images, with the following check even though the
	# remainder of the code permits stacking the images into a higher
	# dimension.

	im = Memi[in]
	inpixtype = IM_PIXTYPE(im)
	if (joindim > IM_NDIM(im)) {
	    call eprintf (
	        "ERROR: For image %s ndim is %d  max join dimension is %d\n")
		call pargstr (IM_HDRFILE(im))
		call pargi (IM_NDIM(im))
		call pargi (IM_NDIM(im))
	    call error (0, "The user-specified join dimension is too large")
	}
	ndim = max (IM_NDIM(im), joindim)
	do j = 1, ndim {
	    if (j <= IM_NDIM(im))
	        nelems[j] = IM_LEN(im,j)
	    else
		nelems[j] = 1
	}

	# Make sure that all the input images have the same dimensionality,
	# and that the length of each dimension is the same for all dimensions
	# but the one being joined.

	do i = 2, nimages {
	    im1 = Memi[in+i-1]
	    if (IM_NDIM(im1) != IM_NDIM(im))
		call error (0, "The input images have different dimensions")
	    ndim = max (ndim, IM_NDIM(im1))
	    do j = 1, ndim {
		if (j > IM_NDIM(im1))
		    nelems[j] = nelems[j] + 1
		else if (j == joindim)
		    nelems[j] = nelems[j] + IM_LEN(im1,j)
		else if (IM_LEN(im1,j) != nelems[j])
		    call error (0,
                "The input images have unequal sizes in the non-join dimension")
	    }
	    inpixtype = ij_tymax (inpixtype, IM_PIXTYPE(im1))
	}

	# Open the output image and set its pixel data type, number of
	# dimensions, and length of each of the dimensions.

	out = immap (Memc[output], NEW_COPY, Memi[in])
	if (outpixtype == ERR || outpixtype == TY_BOOL)
	    IM_PIXTYPE(out) = inpixtype
	else
	    IM_PIXTYPE(out) = outpixtype
	IM_NDIM(out) = ndim
	do j = 1, ndim
	    IM_LEN(out,j) = nelems[j]

	if (bufsize == 0) {

	    # Set initial IMIO buffer size based on the number of images
	    # and maximum amount of working memory available.  The buffer
	    # size may be adjusted later if the task runs out of memory.
	    # The FUDGE factor is used to allow for the size of the
	    # program, memory allocator inefficiencies, and any other
	    # memory requirements besides IMIO.

	    bufsize = 1
	    do i = 1, IM_NDIM(out)
		bufsize = bufsize * IM_LEN(out,i)
	    bufsize = bufsize * sizeof (inpixtype)
	    bufsize = min (bufsize, DEFBUFSIZE)
	    memory = begmem ((nimages + 1) * bufsize, oldsize, maxsize)
	    memory = min (memory, int (FUDGE * maxsize))
	    bufsize = memory / (nimages + 1)
	}

	# Join the images along the join dimension. If an out of memory error
	# occurs close all images and files, divide the IMIO buffer size in
	# half and try again.

	iferr {
	    switch (inpixtype) {
	    case TY_SHORT:
		call imjoins (Memi[in], nimages, out, joindim, outpixtype)
	    case TY_INT:
		call imjoini (Memi[in], nimages, out, joindim, outpixtype)
	    case TY_USHORT, TY_LONG:
		call imjoinl (Memi[in], nimages, out, joindim, outpixtype)
	    case TY_REAL:
		call imjoinr (Memi[in], nimages, out, joindim, outpixtype)
	    case TY_DOUBLE:
		call imjoind (Memi[in], nimages, out, joindim, outpixtype)
	    case TY_COMPLEX:
		call imjoinx (Memi[in], nimages, out, joindim, outpixtype)
	    }
	} then {
	    switch (errcode()) {
	    case SYS_MFULL:
		do j = 1, nimages
		    call imunmap (Memi[in+j-1])
		call imunmap (out)
		call imdelete (Memc[output])
		call imtrew (list)
		bufsize = bufsize / 2
		goto retry_
	    default:
		call erract (EA_ERROR)
	    }
	}

	if (verbose == YES)
	    call ij_verbose (Memi[in], nimages, out, joindim)

	# Unmap all the images.
	call imunmap (out)
	do i = 1, nimages
	    call imunmap (Memi[in+i-1])

	# Restore memory.
	call sfree (sp)
	call fixmem (oldsize)
end


define	MAX_NTYPES	8
define	MAX_NPIXTYPES	7

# IJ_TYMAX -- Return the data type of highest precedence.

int procedure ij_tymax (type1, type2)

int	type1, type2		# Input data types

int	i, j, order[MAX_NTYPES]
data	order/TY_SHORT,TY_USHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,
	     TY_REAL/
begin
	for (i=1; (i<=MAX_NPIXTYPES) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=MAX_NPIXTYPES) && (type2!=order[j]); j=j+1)
	    ;
	return (order[max(i,j)])
end


# IJ_VERBOSE -- Print messages about the actions taken by IMJOIN.

procedure ij_verbose (imptrs, nimages, outptr, joindim)

pointer	imptrs[ARB]		# array of input image pointers
int	nimages			# the number of input images
pointer	outptr			# the output image pointer
int	joindim			# the join dimension

int	i, j, nindim, noutdim
long	offset

begin
	noutdim = IM_NDIM(outptr)
	offset = 1

	do i = 1, nimages {

	    nindim = IM_NDIM(imptrs[i])
	    call printf ("Join: %s size: ") 
		call pargstr (IM_HDRFILE(imptrs[i]))
	    do j = 1, nindim {
		if (j == nindim)
		    call printf ("%d  ->  ")
		else
		    call printf ("%d X ")
		call pargl (IM_LEN(imptrs[i],j))
	    }

	    call printf ("%s[") 
		call pargstr (IM_HDRFILE(outptr))
	    do j = 1, noutdim {
		if (j > nindim) {
		    call printf ("%d:%d")
			call pargi (i)
			call pargi (i)
		} else if (j == joindim) {
		    call printf ("%d:%d")
			call pargl (offset)
			call pargl (offset + IM_LEN(imptrs[i],j)-1)
		    offset = offset + IM_LEN(imptrs[i],j)
		} else {
		    call printf ("1:%d")
			call pargl (IM_LEN(outptr,j))
		}
		if (j != noutdim)
		    call printf (",")
		else
		    call printf ("]")
	    }

	    call printf ("\n")

	}
end
