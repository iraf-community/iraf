include	<error.h>
include	<imhdr.h>
include	"exio.h"

# EX_MAP   -- Map an image and allocate a data structure.
# EX_UNMAP -- Unmap the extraction image and free the data structure.
# EX_G2R   -- Get a real vector (line or column) from a 2D image.
#
# These procedures provide for getting lines or colums from input images
# perpendicular to the dispersion axis.  Thus, the calling programs can
# be largely ignorant of whether the dispersion is along lines or columns.
# The data structure also contains the dispersion and aperture axis and
# their lengths.  Column access uses image sections whose size is determined
# by MAXBUF.  This makes column access nearly as efficent as line access
# at the cost of more memory.

# EX_MAP -- Map an image and allocate a data structure.

pointer procedure ex_map (image)

char	image[ARB]	# Image to map

int	dispaxis, apaxis, imgeti(), sizeof()
pointer	im, ex, immap()
errchk	immap, imgeti, malloc

begin
	im = immap (image, READ_ONLY, 0)
	iferr {
	    if (IM_NDIM(im) != 2)
	        call error (0, "Image must be two dimensional")
	    dispaxis = imgeti (im, "dispaxis")
	    apaxis = mod (dispaxis, 2) + 1
	} then {
	    call imunmap (im)
	    call erract (EA_ERROR)
	}

	call malloc (ex, LEN_EX, TY_STRUCT)
	EX_IM(ex) = im
	EX_DAXIS(ex) = dispaxis
	EX_AAXIS(ex) = apaxis
	EX_DLEN(ex)  = IM_LEN(im,dispaxis)
	EX_ALEN(ex)  = IM_LEN(im,apaxis)
	EX_MAXBUF(ex) = MAXBUF / sizeof (IM_PIXTYPE(im))
	EX_DATA(ex) = NULL
	EX_BUF(ex) = NULL

	return (ex)
end


# EX_UNMAP -- Unmap the extraction image and free the data structure.

procedure ex_unmap (ex)

pointer	ex			# EXIO pointer

begin
	if (EX_DATA(ex) != NULL)
	    call mfree (EX_DATA(ex), TY_REAL)
	call imunmap (EX_IM(ex))
	call mfree (ex, TY_STRUCT)
end


# EX_G2R -- Get a real vector (line or column) from a 2D image.

pointer procedure ex_g2r (ex, vec)

pointer	ex		# EXIO pointer
int	vec		# Image vector (line or column)

int	ncols, nlines, col1, col2, line1, line2
int	i, imlen1, imlen2, nc
pointer	im, data, buf, imgl2r(), imgs2r()
errchk	malloc, imgl2r, imgs2r

begin
	if (EX_AAXIS(ex) == 1)
	    return (imgl2r (EX_IM(ex), vec))

	# Dereference the structure elements to improve the readability of
	# the code and reduce the Mem index arithmetic.

	im     = EX_IM(ex)
	data   = EX_DATA(ex)
	buf    = EX_BUF(ex)
	ncols  = EX_NCOLS(ex)
	nlines = EX_NLINES(ex)
	col1   = EX_COL1(ex)
	col2   = EX_COL2(ex)
	line1  = EX_LINE1(ex)
	line2  = EX_LINE2(ex)
	imlen1 = IM_LEN(im,1)
	imlen2 = IM_LEN(im,2)

	if (data == NULL) {
	    EX_NCOLS(ex) = max (1, min (imlen1, EX_MAXBUF(ex) / imlen2))
	    EX_LINE1(ex) = 1
	    EX_LINE2(ex) = imlen2
	    EX_NLINES(ex) = imlen2
	    EX_COL1(ex) = 0
	    call malloc (EX_DATA(ex), imlen2, TY_REAL)

	    ncols = EX_NCOLS(ex)
	    line1 = EX_LINE1(ex)
	    line2 = EX_LINE2(ex)
	    nlines = EX_NLINES(ex)
	    data = EX_DATA(ex)
	}

	# Determine the starting column of the current section.
	col1 = ((vec - 1) / ncols) * ncols + 1
	if (col1 != EX_COL1(ex)) {
	    nc = min (ncols, imlen1 - col1 + 1)
	    col2 = col1 + nc - 1
	    EX_COL1(ex) = col1
	    EX_COL2(ex) = col2
	    EX_BUF(ex) = imgs2r (im, col1, col2, line1, line2)
	}

	nc = col2 - col1 + 1
	buf = EX_BUF(ex) + vec - col1
	do i = 1, nlines {
	    Memr[data+i-1] = Memr[buf]
	    buf = buf + nc
	}

	EX_COL(ex) = vec
	return (data)
end
