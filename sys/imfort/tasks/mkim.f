c MKIM -- Make a two dimensional test image of type short or real.  The pixel
c values go 1, 2, 3, etc. in storage order.
c
c 	usage:  mkim image ncols nlines [dtype] [pixdir]
c
c The data type defaults to type short if not specified on the command line.
c ---------------------------------------------------------------------------- 

	program mkim

	character*80	image, errmsg, pixdir
	integer		im, ier, axlen(7), naxis, dtype
	integer		nlines, ncols, i, j
	real		pix(8192)

c --- Get image name.
	call clargc (1, image, ier)
	if (ier .ne. 0) then
	    write (*, '('' enter image name: '',$)')
	    read (*,*) image
	endif

c --- Get image size.
	call clargi (2, ncols, ier)
	if (ier .ne. 0) then
	    write (*, '('' ncols: '',$)')
	    read (*,*) ncols
	endif
	call clargi (3, nlines, ier)
	if (ier .ne. 0) then
	    write (*, '('' nlines: '',$)')
	    read (*,*) nlines
	endif

c --- Get pixel datatype (optional).
	call clargi (4, dtype, ier)
	if (ier .ne. 0) dtype = 3

c --- Get pixel directory (optional).
	call clargc (5, pixdir, ier)
	if (ier .eq. 0) then
	    call imsdir (pixdir)
	endif

	axlen(1) = ncols
	axlen(2) = nlines
	naxis    = 2

c --- Create the image.
	call imcrea (image, axlen, naxis, dtype, ier)
	if (ier .ne. 0) goto 91

c --- Open the image for writing, and write the data.
	call imopen (image, 3, im, ier)
	if (ier .ne. 0) goto 91

	do 20 j = 1, nlines
	    do 10 i = 1, ncols
		pix(i) = (j-1) * ncols + i
 10	    continue
	    call impl2r (im, pix, j, ier)
	    if (ier .ne. 0) goto 91
 20	continue

c --- Close the image and quit.
	call imclos (im, ier)
	if (ier .ne. 0) goto 91

	stop

c --- Error exit.
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg
	stop
	end
