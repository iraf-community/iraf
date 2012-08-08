c PHEAD -- Print the header of the named image in FITS format, one keyword
c per line.  A pattern may optionally be specified to list some subset of the
c header keywords.
c
c	usage:  phead image [pattern]
c ----------------------------------------------------------------------------

	program phead

	character*20	kwname
	character*80	image, patstr, errmsg
	integer		im, kwl, ier
	logical		sortit

c --- Get image name.
	call clargc (1, image, ier)
	if (ier .ne. 0) then
	    write (*, '('' enter image name: '',$)')
	    read (*,*) image
	endif

c --- Get pattern string (list everything if no pattern given).
	call clargc (2, patstr, ier)
	if (ier .ne. 0) then
	    patstr = '*'
	endif

c --- Open the image.
	call imopen (image, 1, im, ier)
	if (ier .ne. 0) goto 91

c --- Open the keyword list and print each keyword in FITS format on the
c     standard output device.

	sortit = .false.
	call imokwl (im, patstr, sortit, kwl, ier)

 10	continue
	call imgnkw (kwl, kwname, ier)
	if (ier .ne. 0) goto 20
	    call putkey (im, kwname, ier)
	    if (ier .ne. 0) goto 91
	    goto 10
 20	continue

	call imckwl (kwl, ier)
	if (ier .ne. 0) goto 91

c --- Clean up.
	call imclos (im, ier)
	if (ier .ne. 0) goto 91

	stop

c --- Error exit.
 91	call imemsg (ier, errmsg)
	write (*, '(1x, '' Error: '', a80)') errmsg

	stop
	end


c PUTKEY -- Read the value and comment fields of the named image header
c keyword, and print the value of the keyword in FITS format on the
c standard output device.
c
c     000000000111111111122222222223333333333444444444455555555556
c     123456789012345678901234567890123456789012345678901234567890
c     keyword =                  xxx / comment
c     keyword = 'sval    '           / comment
c
c Datatype codes: 1=bool, 2=char, 3,4,5=int, 6,7=real/double, 8=complex
c Only codes 1, 2, 4, and 6 (bool,char,int,real) are returned by IMTYPK.
c ------------------------------------------------------------------------

	subroutine putkey (im, kwname, ier)

	integer		im
	character*(*)	kwname

	logical		bval
	character*68	sval
	integer		ival
	doubleprecision dval

	character*18	valstr
	character*47	comstr
	character*70	lngstr
	integer		nchars, dtype, ier, i

c --- Get the keyword data type and comment information.
	call imtypk (im, kwname, dtype, comstr, ier)
	if (ier .ne. 0) return

c --- Print the value of the keyword in FITS format.  The format depends
c     upon the datatype of the parameter.

	if (dtype .eq. 1) then
	    call imgkwb (im, kwname, bval, ier)
	    if (ier .ne. 0) return
	    write (*, 10) kwname, bval, comstr
 10	    format (1x, a8, '= ', l20, ' / ', a47)

	else if (dtype .ge. 3 .and. dtype .le. 5) then
	    call imgkwi (im, kwname, ival, ier)
	    if (ier .ne. 0) return
	    write (*, 20) kwname, ival, comstr
 20	    format (1x, a8, '= ', i20, ' / ', a47)

	else if (dtype .eq. 6 .or. dtype .eq. 7) then
	    call imgkwd (im, kwname, dval, ier)
	    if (ier .ne. 0) return
	    if (abs(dval) .lt. 1.0E6 .and. abs(dval) .ge. 1.0E-1) then
	        write (*, 30) kwname, dval, comstr
 30	        format (1x, a8, '= ', f20.2, ' / ', a47)
	    else
	        write (*, 31) kwname, dval, comstr
 31	        format (1x, a8, '= ', e20.12, ' / ', a47)
	    endif

	else
	    call imgkwc (im, kwname, sval, ier)
	    if (ier .ne. 0) return

	    nchars = len(sval) - 1
	    do 40 i = nchars, 9, -1
		if (sval(i:i) .ne. ' ') goto 41
		nchars = i - 1
 40	    continue
 41	    continue

	    if (nchars .le. 8) then
		write (*, 45) kwname, sval, comstr
 45	    	format (1x, a8, '= ''', a8, '''', 10x, ' / ', a47)
	    else if (nchars .le. 18) then
		valstr = sval
		write (*, 46) kwname, valstr, comstr
 46	    	format (1x, a8, '= ''', a18, '''', ' / ', a47)
	    else
		nchars = min (nchars, len(lngstr) - 2)
		lngstr(1:1) = ''''
		do 47 i = 1, nchars
		    lngstr(i+1:i+1) = sval(i:i)
 47		continue
		lngstr(nchars+2:nchars+2) = ''''
		do 48 i = nchars + 3, len(lngstr)
		    lngstr(i:i) = ' '
 48		continue
		write (*, 49) kwname, lngstr
 49	    	format (1x, a8, '= ', a69)
	    endif
	endif

	ier = 0
	end
