# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

define	NTYPES		7


# MKIMAGE -- Make a new two dimensional image of a specified size
# and datatype.  The image pixels are all set to zero.

procedure t_mkimage()

int	dtype
real	pixval
int	ncols, nlines
char	imname[SZ_FNAME]
char	title[SZ_LINE]
short	ty_code[NTYPES]

real	clgetr()
char	clgetc(), ch
int	clgeti(), stridx()

string	types "usilrdx"			# Supported pixfile datatypes
data	ty_code /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
	TY_DOUBLE, TY_COMPLEX/
begin
	call clgstr ("image", imname, SZ_FNAME)
	ncols  = clgeti ("ncols")
	nlines = clgeti ("nlines")
	ch = clgetc ("datatype")
	dtype  = ty_code[stridx(ch,types)]
	pixval = clgetr ("pixval")
	call clgstr ("title", title, SZ_LINE)

	call immake2 (imname, ncols, nlines, dtype, pixval, title)
end


# IMMAKE2 -- Make a two dimensional image of datatype [usilr] with all pixels
# set to the given value.

procedure immake2 (imname, ncols, nlines, dtype, pixval, title)

char	imname[ARB]		# name of new image
int	ncols, nlines		# image size
int	dtype			# datatype
real	pixval			# constant pixel value
char	title[ARB]		# image title

int	i
pointer	im, buf
pointer	immap(), impl2r()

begin
	im = immap (imname, NEW_IMAGE, 0)

	IM_PIXTYPE(im) = dtype
	IM_LEN(im,1)   = ncols
	IM_LEN(im,2)   = nlines
	call strcpy (title, IM_TITLE(im), SZ_IMTITLE)

	# Write out the lines.

	do i = 1, nlines {
	    buf = impl2r (im, i)
	    call amovkr (pixval, Memr[buf], ncols)
	}

	call imunmap (im)
end
