include <imhdr.h>
include <fset.h>		# to check whether input is redirected
include <tbset.h>

# tabim -- create an image from one column of a table
# This task copies a column of a table into an image.  If the image already
# exists, it will be overwritten; otherwise, a new image will be created.
# For a new image, if the 'ndim' parameter is greater than zero, the size of
# the image will be taken from the parameters 'n1', 'n2', etc.  It is the
# user's responsibility to ensure that the product of these values equals
# the number of rows in the table.
#
# Phil Hodge, 12-Oct-1989  Task created
# Phil Hodge, 11-Jan-1991  Allow multi-dimensional output.
# Phil Hodge, 15-May-1998  Check null flag, and replace INDEF with -999.
# Phil Hodge,  8-Jun-1999  Set input to STDIN if redirected.
# Phil Hodge, 30-Mar-2000  Allow lists of names for input and output.

procedure tabim()

pointer inlist, outlist		# for input and output names
char	intable[SZ_FNAME]	# name of an input table
char	output[SZ_FNAME]	# name of an output image
char	colname[SZ_COLNAME]	# column name
int	ndim			# dimension of output image
int	axlen[IM_MAXDIM]	# length of each axis of output image
#--
pointer sp		# stack pointer for scratch space
pointer tp		# pointer to descriptor for input table
pointer cp		# column descriptor
pointer im		# pointer to image descriptor
pointer xp		# pointer to output data for image
pointer temp		# scratch for parameter name
pointer nullflag	# scratch for null flags (ignored)
long	v[IM_MAXDIM]	# for call to impnld()
int	dtype		# data type
int	npix		# number of pixels, accumulated one axis at a time
int	nrows		# number of rows in table
int	nlines		# number of lines in image
int	frow, lrow	# row number limits for tbcgtd
int	i, k
int	junk
bool	new_image	# true if the image does not already exist
pointer immap(), tbtopn()
int	clgeti(), impnld(), imaccess()
int	fstati()
int	tbpsta(), tbcigi()

pointer imt, tnt	# pointers for filename templates
int	nin, nout	# numbers of names in lists
pointer imtopen(), tbnopen()
int	imtlen(), imtgetim(), tbnlen(), tbnget()

begin
	call smark (sp)
	call salloc (inlist, SZ_LINE, TY_CHAR)
	call salloc (outlist, SZ_LINE, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	# Get the names of the input tables.
	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", Memc[inlist], SZ_FNAME)
	else
	    call clgstr ("intable", Memc[inlist], SZ_FNAME)
	tnt = tbnopen (Memc[inlist])
	nin = tbnlen (tnt)

	# Get the names of the output images.
	call clgstr ("output", Memc[outlist], SZ_FNAME)
	imt = imtopen (Memc[outlist])
	nout = imtlen (imt)

	if (nin == 0)
	    call error (1, "no input table specified")
	if (nout == 0)
	    call error (1, "no output image specified")
	if (nin != nout)
	    call error (1, "input and output lists must have the same length")

	call clgstr ("colname", colname, SZ_COLNAME)

	# ndim is either zero or the dimension for new output images.
	ndim = clgeti ("ndim")
	if (ndim < 1)
	    ndim = 1
	do k = 1, IM_MAXDIM		# initial values
	    axlen[k] = 1
	# Get the length of all but the last axis.
	do k = 1, ndim-1 {
	    call sprintf (Memc[temp], SZ_FNAME, "n%d")
		call pargi (k)
	    axlen[k] = clgeti (Memc[temp])
	}

	# Loop over the list of input tables.
	while (tbnget (tnt, intable, SZ_FNAME) != EOF) {

	    junk = imtgetim (imt, output, SZ_FNAME)

	    tp = tbtopn (intable, READ_ONLY, NULL)
	    call tbcfnd (tp, colname, cp, 1)	# only one column name
	    if (cp == NULL) {
		call tbtclo (tp)
		call error (1, "column not found")
	    }
	    nrows = tbpsta (tp, TBL_NROWS)

	    # Open the output image.
	    if (imaccess (output, READ_WRITE) == YES) {
		new_image = false
		im = immap (output, READ_WRITE, NULL)
	    } else {
		new_image = true
		im = immap (output, NEW_IMAGE, NULL)
	    }

	    if (new_image) {
		# Set the size of the new image.
		IM_NDIM(im) = ndim
		npix = 1		# initial value
		do k = 1, ndim-1 {
		    IM_LEN(im,k) = axlen[k]
		    npix = npix * axlen[k]
		}
		axlen[ndim] = nrows / npix
		IM_LEN(im,ndim) = axlen[ndim]

		# The image data type is the same as that of the column.
		dtype = tbcigi (cp, TBL_COL_DATATYPE)
		if (dtype == TY_BOOL)
		    dtype = TY_SHORT
		IM_PIXTYPE(im) = dtype
	    }

	    nlines = 1			# initial value
	    do k = 2, IM_NDIM(im)
		nlines = nlines * IM_LEN(im,k)
	    if (IM_LEN(im,1) * nlines != nrows) {
		call tbtclo (tp)
		call imunmap (im)
		if (new_image) {
		    call imdelete (output)
		    call error (1,
		"specified axis lengths are not consistent with size of table")
		} else {
		    call error (1,
		"size of existing image is not consistent with size of table")
		}
	    }

	    # Allocate space for the array of null flags (which we ignore),
	    # one element for each pixel in a line.
	    call salloc (nullflag, IM_LEN(im,1), TY_BOOL)

	    # Copy the column into the image, one line at a time.
	    do k = 1, IM_MAXDIM
		v[k] = 1
	    frow = 1
	    lrow = IM_LEN(im,1)
	    do k = 1, nlines {
		junk = impnld (im, xp, v)
		call tbcgtd (tp, cp, Memd[xp], Memb[nullflag], frow, lrow)
		do i = 0, lrow-frow {
		    if (Memb[nullflag+i])
			Memd[xp+i] = -999.d0
		}
		frow = frow + IM_LEN(im,1)
		lrow = lrow + IM_LEN(im,1)
	    }

	    call imunmap (im)
	    call tbtclo (tp)
	}

	call sfree (sp)
end
