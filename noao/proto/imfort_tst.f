C IMFORT -- Fortran interface to IRAF images.  This interface permits a
C Fortran program to read or write an existing IRAF image.  There is currently
C no provision for creating new images or deleting old images from Fortran.
C The interface routines are as follows:
C
C	im =	   imopen (image, mode, ndim, len_axes)
C		   imclos (im)
C
C		imget[sr] (im, buf, x1, x2, linenum)
C		imput[sr] (im, buf, x1, x2, linenum)
C
C Legal access modes are "r", "w", and "rw".  All coordinates are 1-indexed.
C The integer array "len_axes" should be dimensioned at least 7.  The "im"

	integer	im
	integer axlen(7), ndim
	integer imopen
	integer*2 pix(1024)

	im = imopen ('/tmp2/iraf/images/canon', 'r', ndim, axlen)
	write (*,*) ndim, axlen
	call imgets (im, pix, 10,15, 5)
	write (*,*) pix(1), pix(5)
	stop
	end
