c IMREN -- Rename an image.
c
c	usage:  imren oldname newname
c ----------------------------------------------------------------------

	program imren

	integer		nargs, ier
	character*80	oname, nname, errmsg

c --- Get the old and new names of the image to be renamed.
	call clnarg (nargs)
	if (nargs .ge. 2) then
	    call clargc (1, oname, ier)
	    if (ier .ne. 0) goto 91
	    call clargc (2, nname, ier)
	    if (ier .ne. 0) goto 91
	else
	    write (*, '('' enter old image name: '',$)')
	    read (*,*) oname
	    write (*, '('' enter new image name: '',$)')
	    read (*,*) nname
	endif

c --- Rename the image.
	call imrnam (oname, nname, ier)
	if (ier .ne. 0) goto 91

	stop

c --- Error exit.
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg

	stop
	end
