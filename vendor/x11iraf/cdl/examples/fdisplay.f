C ========================================================================
C                                                                        =
C  FDISPLAY -- Example fortran program showing the use of the Client     =
C  Display Library (CDL) Fortran interface for displaying images.  In    =
C  this simple program all input is prompted for on the command line.    =
C                                                                        =
C ========================================================================


	program fdisplay
	character*132  	imname
        character*132	imtdev

C 	--------------------------
C 	Initialize the CDL package
C 	--------------------------
        call getenv('IMTDEV',imtdev)
        call cfopen(imtdev, ier)
	if (ier .gt. 0) then
	    write (*,*) 'open: Error return from CDL'
	    goto 999
	endif

        write (*, "('Image Name: ', $)")
        read (5, *) imname
        write (*, "('Frame Number: ', $)")
        read (5, *) iframe
        write (*, "('Frame buffer configuration number: ', $)")
        read (5, *) ifb

C 	----------------------------------------------------------
C	If we've got a FITS format image, go ahead and display it.
C 	----------------------------------------------------------
	call cfisfits (imname, isfits)
	if (isfits .gt. 0) then
	    call cfdisplayfits (imname, iframe, ifb, 1, ier)
	    if (ier .gt. 0) then
	        write (*,*) 'displayFITS: Error return from CDL'
	        goto 999
	    endif
	else 
C 	    --------------------------------------------------------
C	    We've got an IRAF format image, go ahead and display it.
C 	    --------------------------------------------------------
	    call cfisiraf (imname, isiraf)
	    if (isiraf .gt. 0) then
	        call cfdisplayiraf (imname, 1, iframe, ifb, 1, ier)
	        if (ier .gt. 0) then
	            write (*,*) 'displayIRAF: Error return from CDL'
	            goto 999
	        endif
	    else 
C 	    	----------------------------------
C	    	Unrecognized image, punt and exit.
C 	    	----------------------------------
	        write (*,*) 'Unrecognized image format'
	    endif
	endif

C 	------------------
C 	Clean up and exit.
C 	------------------
999	continue
	call cfclose (ier)
	end
