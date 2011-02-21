include	<imhdr.h>
include	<error.h>
include	<syserr.h>

define	DEFBUFSIZE		65536		# default IMIO buffer size
define	FUDGE			0.8		# fudge factor


# T_IMJOIN -- Task driver for imjoin:  up to IM_MAXDIM image join, along
# any one specified axis, from multiple input images.  The set of input
# images need have the same number of dimensions and elements per dimension
# ONLY along the axes not being joined.  Datatype will be converted to 
# highest precedence type if not all the same.

procedure t_imjoin()

int	list			# List of input images
pointer	output			# Output image
char	outtype			# Output datatype

int	i, j, nimages, intype, ndim, joindim, outdtype, nelems[IM_MAXDIM]
int	bufsize, maxsize, memory, oldsize
pointer	sp, in, out, im, im1, input

int	imtopenp(), imtlen(), imtgetim(), clgeti()
int	ty_max(), sizeof(), begmem(), errcode()
char	clgetc()
pointer	immap()
errchk	immap
define	retry_	99

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# Get the parameters.  Some parameters are obtained later.
	list = imtopenp ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	joindim = clgeti ("joindim")
	outtype = clgetc ("outtype")

	# Check if there are no images.
	nimages = imtlen (list)
	if (nimages == 0) {
	    call imtclose (list)
	    call sfree (sp)
	    call error (0, "No input images to join")
	}
	call salloc (in, nimages, TY_POINTER)

	# Map the input images.
	bufsize = 0
retry_
	nimages = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    nimages = nimages + 1
	    Memi[in+nimages-1] = immap (Memc[input], READ_ONLY, 0)
	}

	# Determine the dimensionality, size, and datatype of the output image.
	im = Memi[in]
	intype = IM_PIXTYPE(im)
	ndim = max (IM_NDIM(im), joindim)
	do j = 1, ndim
	    nelems[j] = IM_LEN(im,j)

	do i = 2, nimages {
	    im1 = Memi[in+i-1]
	    ndim = max (ndim, IM_NDIM(im1))
	    do j = 1, ndim {
		if (j == joindim)
		    nelems[j] = nelems[j] + IM_LEN(im1,j)
		else {
		    if (IM_LEN(im1,j) != nelems[j]) {
			call eprintf ("Image %d different size in dimen %d\n")
			    call pargi (i)
			    call pargi (IM_LEN(im1,j))
			call error (1, "Non-joindim image sizes must match")
		    }
		}
	    }
	    intype = ty_max (intype, IM_PIXTYPE(im1))
	}

	# Open the output image and set its pixel datatype.
	# If outtype was not specified (the default), set it to intype.

	out = immap (Memc[output], NEW_COPY, Memi[in])
	switch (outtype) {
	case 's':
	    outdtype = TY_SHORT
	case 'i':
	    outdtype = TY_INT
	case 'l':
	    outdtype = TY_LONG
	case 'r':
	    outdtype = TY_REAL
	case 'd':
	    outdtype = TY_DOUBLE
	case 'x':
	    outdtype = TY_COMPLEX
	default:
	    outdtype = intype
	}
	IM_PIXTYPE(out) = outdtype

	# Set output image dimensionality and axis lengths.
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
	    bufsize = bufsize * sizeof (intype)
	    bufsize = min (bufsize, DEFBUFSIZE)
	    memory = begmem ((nimages + 1) * bufsize, oldsize, maxsize)
	    memory = min (memory, int (FUDGE * maxsize))
	    bufsize = memory / (nimages + 1)
	}

	# Join the images along joindim.  If an out of memory error occurs
	# close all images and files, divide the IMIO buffer size in half
	# and try again.
	iferr {
	    switch (intype) {
	    case TY_SHORT:
		call imjoins (Memi[in], nimages, out, joindim, outdtype)
	    case TY_INT:
		call imjoini (Memi[in], nimages, out, joindim, outdtype)
	    case TY_LONG:
		call imjoinl (Memi[in], nimages, out, joindim, outdtype)
	    case TY_REAL:
		call imjoinr (Memi[in], nimages, out, joindim, outdtype)
	    case TY_DOUBLE:
		call imjoind (Memi[in], nimages, out, joindim, outdtype)
	    case TY_COMPLEX:
		call imjoinx (Memi[in], nimages, out, joindim, outdtype)
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

	# Unmap all the images and restore memory.
	call imunmap (out)
	do i = 1, nimages
	    if (joindim < 3)
		call imunmap (Memi[in+i-1])

	call sfree (sp)
	call fixmem (oldsize)
end


# TY_MAX -- Return the datatype of highest precedence.

int procedure ty_max (type1, type2)

int	type1, type2		# Datatypes

int	i, j, order[7]
data	order/TY_SHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,TY_REAL/

begin
	for (i=1; (i<=6) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=6) && (type2!=order[j]); j=j+1)
	    ;
	return (order[max(i,j)])
end
