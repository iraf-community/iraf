c MINMAX -- Compute the minimum and maximum pixel values in an image.
c The new values are printed as well as updated in the image header.
c
c	usage:  minmax image
c ----------------------------------------------------------------------

	program minmax

	character*80	image, errmsg
	real		pix(8192), dmin, dmax, vmin, vmax
	integer		im, axlen(7), naxis, dtype, ier, j

c --- Get image name.
	call clargc (1, image, ier)
	if (ier .ne. 0) then
	    write (*, '('' enter image name: '',$)')
	    read (*,*) image
	endif

c --- Open the image for readwrite access (we need to update the header).
	call imopen (image, 3, im, ier)
	if (ier .ne. 0) goto 91
	call imgsiz (im, axlen, naxis, dtype, ier)
	if (ier .ne. 0) goto 91

c --- Read through the image and compute the limiting pixel values.
	do 10 j = 1, axlen(2)
	    call imgl2r (im, pix, j, ier)
	    if (ier .ne. 0) goto 91
	    call alimr (pix, axlen(1), vmin, vmax)
	    if (j .eq. 1) then
		dmin = vmin
		dmax = vmax
	    else
		dmin = min (dmin, vmin)
		dmax = max (dmax, vmax)
	    endif
 10	continue

c --- Update the image header.
	call impkwr (im, 'datamin', dmin, ier)
	if (ier .ne. 0) goto 91
	call impkwr (im, 'datamax', dmax, ier)
	if (ier .ne. 0) goto 91

c --- Clean up.
	call imclos (im, ier)
	if (ier .ne. 0) goto 91
	write (*, '(1x, a20, 2 g12.5)') image, dmin, dmax
	stop

c --- Error exit.
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg
	stop
	end
