# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>

# MKTEST -- Make a test image.

procedure t_mktest()

char	imname[SZ_FNAME]
int	ndim, dim[IM_MAXDIM]
int	i, j, k, scalar
long	offset
int	clgeti(), nscan(), clscan(), stridx()
pointer	buf, im, immap(), impl3l()

int	dtype
string	types "usilrdx"			# Supported pixfile datatypes
char	ty_code[7], clgetc()
data	ty_code /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
	TY_DOUBLE, TY_COMPLEX, EOS/

begin
	call clgstr ("image_name", imname, SZ_FNAME)
	dtype = ty_code[stridx (clgetc ("datatype"), types)]
	ndim = clgeti ("ndim")

	call amovki (1, dim, 3)
	if (clscan ("axis_lengths") != EOF) {
	    do i = 1, ndim
		call gargi (dim[i])
	    if (nscan() < ndim)
		call error (1, "Insufficient dimensions")
	}

	im = immap (imname, NEW_IMAGE, 0)

	IM_PIXTYPE(im) = dtype
	do i = 1, ndim
	    IM_LEN(im,i) = dim[i]

	do k = 1, dim[3]
	    do j = 1, dim[2] {
		buf = impl3l (im, j, k)

		# Pixel value eq pixel coords.
		offset = 1
		if (ndim > 1) {
		    if (dim[1] < 100)
			scalar = 100
		    else
			scalar = 1000
		    offset = offset + j * scalar
		}

		if (ndim > 2)
		    offset = offset + k * (scalar ** 2)

		# Avoid integer overflow if large type short image.
		if (IM_PIXTYPE(im) == TY_SHORT)
		    offset = min (MAX_SHORT, offset - dim[1])

		# Initialize line of pixels.
		do i = 0, dim[1]-1
		    Meml[buf+i] = offset + i
	    }

	call imunmap (im)
end
