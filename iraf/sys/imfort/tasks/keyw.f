c KEYW -- Test the image header get/put interface routines.
c
c	usage:  keyw imagename
c ----------------------------------------------------------------------------

	program keyw

	character*80	image, errmsg
	character*8	keywrd, option
	character*80	valstr, commnt
	integer		ncols, nlines, dtype
	integer		im, ier, axlen(7), naxis

c --- Get image name.
	call clargc (1, image, ier)
	if (ier .ne. 0) then
	    write (*, '('' enter image name: '',$)')
	    read (*,*) image
	endif

c --- Open the image.
	call imopen (image, 3, im, ier)
	if (ier .ne. 0) goto 91
	call imgsiz (im, axlen, naxis, dtype, ier)
	if (ier .ne. 0) goto 91

	ncols  = axlen(1)
	nlines = axlen(2)

c --- Interpreter loop.
 10	continue
	write (*, '('' enter command (quit,gkw[cir],pkw[cir],addk,delk): '',$)')
	read (*,*) option

	if (option .eq. 'pkwc') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    write (*, '('' value: '',$)')
	    read (*,*) valstr
	    call impkwc (im, keywrd, valstr, ier)
	    if (ier .ne. 0) goto 91
	    goto 10

	else if (option .eq. 'pkwi') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    write (*, '('' value: '',$)')
	    read (*,*) ival
	    call impkwi (im, keywrd, ival, ier)
	    if (ier .ne. 0) goto 91
	    goto 10

	else if (option .eq. 'pkwr') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    write (*, '('' value: '',$)')
	    read (*,*) rval
	    call impkwr (im, keywrd, rval, ier)
	    if (ier .ne. 0) goto 91
	    goto 10

	else if (option .eq. 'gkwc') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    call imgkwc (im, keywrd, valstr, ier)
	    if (ier .ne. 0) goto 91
	    write (*,*) 'value ', valstr
	    goto 10

	else if (option .eq. 'gkwi') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    call imgkwi (im, keywrd, ival, ier)
	    if (ier .ne. 0) goto 91
	    write (*,*) 'value ', ival
	    goto 10

	else if (option .eq. 'gkwr') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    call imgkwr (im, keywrd, rval, ier)
	    if (ier .ne. 0) goto 91
	    write (*,*) 'value ', rval
	    goto 10

	else if (option .eq. 'addk') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    write (*, '('' keyword datatype: '',$)')
	    read (*,*) dtype
	    write (*, '('' comment field: '',$)')
	    read (*,*) commnt
	    call imaddk (im, keywrd, dtype, commnt, ier)
	    if (ier .ne. 0) goto 91
	    write (*,*) 'value ', rval
	    goto 10

	else if (option .eq. 'delk') then
	    write (*, '('' keyword name: '',$)')
	    read (*,*) keywrd
	    call imdelk (im, keywrd, ier)
	    if (ier .ne. 0) goto 91
	    goto 10

	endif

c --- Clean up.
	call imclos (im, ier)
	if (ier .ne. 0) goto 91

	stop
 91	call imemsg (ier, errmsg)
	write (*, '('' Error: '', a80)') errmsg

	stop
	end
