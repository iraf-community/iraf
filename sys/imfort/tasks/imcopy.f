c IMCOPY -- Copy an image of up to 2048 pixels per line.  Works for images of
c up to three dimensions with a pixel type of either short or real.
c
c	usage:  imcopy oldimage newimage
c ----------------------------------------------------------------------------

	program imcopy

	real		rpix(2048)
	integer*2	spix(4096)
	equivalence	(rpix, spix)
	character*80	oimage, nimage, errmsg
	integer		ncols, nlines, nbands, j, k, oim, nim
	integer		ier, axlen(7), naxis, pixtype, nargs

c --- Get command line arguments.
	call clnarg (nargs)
	if (nargs .eq. 2) then
	    call clargc (1, oimage, ier)
	    if (ier .ne. 0) goto 91
	    call clargc (2, nimage, ier)
	    if (ier .ne. 0) goto 91
	else
	    write (*, '('' input image: '',$)')
	    read (*,*) oimage
	    write (*, '('' output image: '',$)')
	    read (*,*) nimage
	endif

c --- Open the input image.
	call imopen (oimage, 1, oim, ier)
	if (ier .ne. 0) goto 91

c --- Create a new output image with the same header and size as the
c	input image.

	call imopnc (nimage, oim, nim, ier)
	if (ier .ne. 0) goto 91

c --- Determine the size and pixel type of the image being copied.
	call imgsiz (oim, axlen, naxis, pixtype, ier)
	if (ier .ne. 0) goto 91
	ncols  = axlen(1)
	nlines = axlen(2)
	nbands = axlen(3)

c --- Copy the image.
	if (pixtype .eq. 3) then
	    do 15 k = 1, nbands
		do 10 j = 1, nlines
		    call imgl3s (oim, spix, j, k, ier)
		    if (ier .ne. 0) goto 91
		    call impl3s (nim, spix, j, k, ier)
		    if (ier .ne. 0) goto 91
 10	        continue
 15	    continue
	else
	    do 25 k = 1, nbands
		do 20 j = 1, nlines
		    call imgl3r (oim, rpix, j, k, ier)
		    if (ier .ne. 0) goto 91
		    call impl3r (nim, rpix, j, k, ier)
		    if (ier .ne. 0) goto 91
 20	        continue
 25	    continue
	endif

c --- Clean up.
	call imclos (oim, ier)
	if (ier .ne. 0) goto 91
	call imclos (nim, ier)
	if (ier .ne. 0) goto 91

	stop

c -- Error actions.
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg

	stop
	end
