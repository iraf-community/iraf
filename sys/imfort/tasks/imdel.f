c IMDEL -- Delete an image.
c
c	usage:  imdel imagename
c ----------------------------------------------------------------------

	program imdel

	integer		ier
	character*80	image, errmsg

c --- Get the name of the image to be deleted.
	call clargc (1, image, ier)
	if (ier .ne. 0) then
	    write (*, '('' enter image name: '',$)')
	    read (*,*) image
	endif

c --- Delete the image.
	call imdele (image, ier)
	if (ier .ne. 0) goto 91

	stop

c --- Error exit.
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg

	stop
	end
