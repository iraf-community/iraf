c PCUBE -- Extract a subraster (image cube) from an image and print
c the values on the standard output.  This is used with a standard
c test image to verify that the IMFORT interface is working correctly.
c
c	usage:  pcube image i1 i2 [j1 j2 [k1 k2]]
c ---------------------------------------------------------------------

	program pcube

	character*80	image, errmsg
	integer		i1, i2, j1, j2, k1, k2
	integer		im, ier, axlen(7), naxis, dtype, nargs
	real		pix(8192)

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

c --- Get subraster coordinates.
	call clnarg (nargs)
	if (nargs .lt. 3) then
	    write (*, '('' enter subraster coordinates (i1 i2 j1 j2): '',$)')
	    read (*,*) i1, i2, j1, j2
	    k1 = 1
	    k2 = 1
	else
	    call clargi (2, i1, ier)
	    if (ier .ne. 0) goto 91
	    call clargi (3, i2, ier)
	    if (ier .ne. 0) goto 91

	    if (nargs .ge. 5) then
		call clargi (4, j1, ier)
		if (ier .ne. 0) goto 91
		call clargi (5, j2, ier)
		if (ier .ne. 0) goto 91
	    else
		j1 = 1
		j2 = 1
	    endif

	    if (nargs .ge. 7) then
		call clargi (6, k1, ier)
		if (ier .ne. 0) goto 91
		call clargi (7, k2, ier)
		if (ier .ne. 0) goto 91
	    else
		k1 = 1
		k2 = 1
	    endif
	endif

c --- Extract the subraster.
	call imgs3r (im, pix, i1, i2, j1, j2, k1, k2, ier)
	if (ier .ne. 0) goto 91

c --- Print the pixel values.
	call pcuber (pix, i2-i1+1, j2-j1+1, k2-k1+1, i1,i2, j1,j2, k1,k2)

c --- Close the input image and quit.
	call imclos (im, ier)
	if (ier .ne. 0) goto 91

	stop

c --- Error handler.
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg
	stop
	end


c PCUBER -- Print pixel values, 3d subraster, type real.
c ----------------------------------------------------------------

	subroutine pcuber (pix, nx,ny,nz, i1,i2, j1,j2, k1,k2)

	integer		nx, ny, nz
	real		pix(nx,ny,nz)
	integer		i1, i2, j1, j2, k1, k2
	integer		i, j, k

	nx = i2 - i1 + 1
	ny = j2 - j1 + 1
	nz = k2 - k1 + 1

	do 20 k = k1, k2
	    write (*, '('' band '', i3)') k

	    print 81, i1, i2, j1, j2
	    do 10 j = 1, ny
		print 82, j-1+j1, (pix(i,j,k), i = 1, nx)
 10	    continue
 20	continue

 81	format (' subraster at ', 4 i4)
 82	format (' line ', i4, 8 (1x, f7.0))

	end
