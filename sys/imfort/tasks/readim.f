c READIM -- Read through an image and count the lines (used for timing tests).
c Tests line sequential i/o.
c
c	usage:  readim image
c ----------------------------------------------------------------------------

	program readim

	character*80	image, errmsg
	integer		ncols, nlines, nbands, j, k
	integer		im, ier, axlen(7), naxis, dtype
	integer*2	pix(8192)

c --- Get image name.
	call clargc (1, image, ier)
	if (ier .ne. 0) then
	    write (*, '('' enter image name: '',$)')
	    read (*,*) image
	endif

c --- Open the image.
	call imopen (image, 1, im, ier)
	if (ier .ne. 0) goto 91
	call imgsiz (im, axlen, naxis, dtype, ier)
	if (ier .ne. 0) goto 91

	ncols  = axlen(1)
	nlines = axlen(2)
	nbands = axlen(3)

c --- Read through the image.
	do 20 k = 1, nbands
	    do 10 j = 1, nlines
		call imgl3s (im, pix, j, k, ier)
		if (ier .ne. 0) goto 91
 10	    continue
 20	continue

c --- Clean up.
	call imclos (im, ier)
	if (ier .ne. 0) goto 91

	print 81, nlines, image
 81	format (' read ', i4, ' lines from image ', a64)

	stop

c --- Error exit.
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg

	stop
	end
