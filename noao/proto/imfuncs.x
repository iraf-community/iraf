include <imhdr.h>



# IMABS -- Take the absolute value of pixels in image1 and write to image2.

procedure imabsr (im1, im2)

pointer	im1				# Input IMIO pointer
pointer	im2				# Output IMIO pointer

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)

	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call aabsr (Memr[buf1], Memr[buf2], npix)
end


# IMLOG -- Take log of pixels in image1 and write to image2

procedure imlogr (im1, im2)

pointer	im1				# Input IMIO pointer
pointer	im2				# Output IMIO pointer

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]

real	errlogr()
extern	errlogr()
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)

	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call alogr (Memr[buf1], Memr[buf2], npix, errlogr)
end

real procedure errlogr (x)

real	x

begin
	return (0.0)
end


# IMSQR -- Take the square root of pixels in image1 and write to image2.

procedure imsqrr (im1, im2)

pointer	im1				# Input IMIO pointer
pointer	im2				# Output IMIO pointer

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]

real	errsqrr()
extern	errsqrr()
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)

	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call asqrr (Memr[buf1], Memr[buf2], npix, errsqrr)
end


real procedure errsqrr (x)

real	x

begin
	return (0.0)
end


# IMDEX -- Take DEX of pixels in image1 and write to image2

procedure imdexr (im1, im2)

pointer	im1				# Input IMIO pointer
pointer	im2				# Output IMIO pointer

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)

	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call adexr (Memr[buf1], Memr[buf2], npix)
end


# ADEX -- Take the DEX of a vector.

procedure adexr (a, b, n)

real	a[n]
real	b[n]
int	n

int	i

begin
	do i = 1, n
	    b[i] = 10. ** a[i]
end

