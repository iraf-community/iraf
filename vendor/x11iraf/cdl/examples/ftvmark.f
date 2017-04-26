C ==========================================================================
C                                                                          =
C  FTVMARK --  Example fortran program showing the use of the Client       =
C  Display Library (CDL) Fortran interface for doing graphics overlay. In  =
C  this simple program all input is prompted for on the command line.      =
C                                                                          =
C ==========================================================================


	program ftvmark
	include 	"../cdlftn.inc"
	character*64   	imname
        character*132   imtdev

C       --------------------------
C       Initialize the CDL package
C       --------------------------
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
C 	        -------------------------------------------------------------
C	        No valid image given, so map the current display for marking.
C 	        -------------------------------------------------------------
	        call cfmapframe (iframe)
	    endif
	endif

C 	---------------------------------------------------------------
C 	Now that we've got an image displayed or mapped, enter a cursor
C	loop to mark the image.  We do this in a subroutine so all the
C	parameters needed are together.
C 	---------------------------------------------------------------
	call markInteractive ()

C 	-----------------
C 	Clean up and exit
C 	-----------------
999	continue
	call cfclose (ier)
	end


C =======================================================================
C                                                                       =
C   MARKINTERACTIVE -- Subroutine for processing the cursor loop.       =
C                                                                       =
C =======================================================================

	subroutine markInteractive ()
	include 	"../cdlftn.inc"
	real		angle, rx, ry, txsize
	integer		nx, ny, x, y, x2, y2, fill, size, color
	integer		number, radius, xrad, yrad, nannuli, sep
	character	key
	character*64	cmd, str

C 	-----------------------------------------------------------------
C  	Allocate a 1024x1024 array for pixels.  This is the largest frame
C	buffer we support in this example task
C 	-----------------------------------------------------------------
	character	pix(1048576)

C 	--------------------------------
C	Initialize the parameters to use
C 	--------------------------------
	color	= 205
	size	= 10
	fill	= 0
	angle	= 0.0
	txsize	= 1.0
	number	= 1
	radius	= 11
	xrad 	= 11
	yrad	= 6
	nannuli	= 3
	sep	= 5

C       ----------------------------------------------
C	Read a cursor keystroke telling us what to do.
C       ----------------------------------------------
10 	call cfreadcursor (0, rx, ry, key, ier)
	if (ier .gt. 0) then
	    write (*,*) 'cfreadCursor: Error return from CDL'
	    goto 998
	endif

C       ----------------------------------------------------------
C	Round the real cursor position to integer pixel positions.
C       ----------------------------------------------------------
	    x = nint (rx + 0.5)
	    y = nint (ry + 0.5)

C       --------------------------------------------------------------
C	Check the keystroke and take the appropriate action.  Don't go
C	looking for an error condition.
C       --------------------------------------------------------------

C           --------------
C	    Colon Commands
C           --------------
	    if (key .eq. ':') then
C		----------------------------------------------
C		Read a three character command and value field
C		----------------------------------------------
	        read (*,'(A3, i4)') cmd, ival

C		-------------------------
C		Process the colon command
C		-------------------------
		if (cmd(1:3) .eq. 'ang') then
		    angle = real (ival)
		else if (cmd(1:3) .eq. 'col') then
		    color = ival
		else if (cmd(1:3) .eq. 'fil') then
		    fill = ival
		else if (cmd(1:3) .eq. 'num') then
		    number = ival
		else if (cmd(1:3) .eq. 'nan') then
		    nannuli = ival
		else if (cmd(1:3) .eq. 'lab') then
		    label = ival
		else if (cmd(1:3) .eq. 'sep') then
		    sep = ival
		else if (cmd(1:3) .eq. 'siz') then
		    size = ival
		else if (cmd(1:3) .eq. 'txs') then
		    txsize = ival
		else if (cmd(1:3) .eq. 'xra') then
		    xrad = ival
		else if (cmd(1:3) .eq. 'yra') then
		    yraf = ival
		else if (cmd(1:3) .eq. 'pri') then
		    call cfreadframebuffer (pix, nx, ny, ier)
		    call cfprintpix ("lpr", pix, nx, ny, 1, ier)
		else if (cmd(1:3) .eq. 'sta') then
     		    print 201, angle, color
     		    print 202, fill, number
     		    print 203, nannuli, sep
     		    print 204, size, txsize
     		    print 205, xrad, yrad
     		    print 206, label
201     	    format ('angle   = ',F5.3, t25, 'color  = ',I5)
202     	    format ('fill    = ',I5, t25, 'number  = ',I5)
203     	    format ('nannuli = ',I5, t25, 'sep     = ',I5)
204     	    format ('size    = ',I5, t25, 'txsize  = ',F5.3)
205     	    format ('xrad    = ',I5, t25, 'yrad    = ',I5)
206     	    format ('fill    = ',I5)
	 	endif

C           -------------
C	    Point Markers
C           -------------
	    else if (key .eq. 'p') then
		call cfmarkpoint (x, y, 1, size, M_PLUS, color, ier)
	    else if (key .eq. 'x') then
		call cfmarkpoint (x, y, 1, size, M_CROSS, color, ier)
	    else if (key .eq. '.') then
		call cfmarkpoint (x, y, 1, size, M_POINT, color, ier)
	    else if (key .eq. '*') then
		call cfmarkpoint (x, y, 1, size, M_STAR, color, ier)
	    else if (key .eq. '_') then
		call cfmarkpoint (x, y, 1, size, M_HBLINE, color, ier)
	    else if (key .eq. '|') then
		call cfmarkpoint (x, y, 1, size, M_VBLINE, color, ier)
	    else if (key .eq. 'o') then
		call cfmarkpoint (x, y, 1, size, ior(M_CIRCLE,fill),
     &		    color, ier)
	    else if (key .eq. 's') then
		call cfmarkpoint (x, y, 1, size, ior(M_BOX,fill), color,
     &		    ier)
	    else if (key .eq. 'v') then
		call cfmarkpoint (x, y, 1, size, ior(M_DIAMOND,fill),
     &		    color, ier)

C           -------------
C	    Other Markers
C           -------------

	    else if (key .eq. 'b') then
		print '("Hit another key to define the box ....")'
 		call cfreadcursor (0, rx, ry, key, ier)
	    	x2 = nint (rx + 0.5)
	    	y2 = nint (ry + 0.5)
		call cfmarkbox (x, y, x2, y2, fill, color, ier)
	    else if (key .eq. 'c') then
		print '("Hit another key to set the radius ....")'
 		call cfreadcursor (0, rx, ry, key, ier)
	    	x2 = nint (rx + 0.5)
	    	y2 = nint (ry + 0.5)
		radius = nint (sqrt (real((x2-x)**2 + (y2-y)**2)))
		call cfmarkcircle (x, y, radius, fill, color, ier)
	    else if (key .eq. 'd') then
		call cfdeletemark (x, y, ier)
	    else if (key .eq. 'e') then
		call cfmarkellipse (x, y, xrad, yrad, angle, fill, color, ier)
	    else if (key .eq. 'l') then
		print '("Hit another key to set line endpoint ....")'
 		call cfreadcursor (0, rx, ry, key, ier)
	    	x2 = nint (rx + 0.5)
	    	y2 = nint (ry + 0.5)
		call cfmarkline (x, y, x2, y2, color, ier)
	    else if (key .eq. 't') then
		print '("Test string:  ", $)'
		read (*,'(A64)') str
		call cfmarktext (x, y, str, txsize, angle, color, ier)
	    else if (key .eq. 'C') then
		call cfmarkcircannuli (x, y, radius, nannuli, sep, ier)
	    else if (key .eq. 'D') then
		call cfclearoverlay (ier)
	    else if (key .eq. 'E') then
		call cfmarkellipannuli (x, y, xrad, yrad, angle, nannuli,
     &		    sep, ier)

C           -------------
C	    Misc Commands
C           -------------
	    else if (key .eq. '?') then
		call printHelp ()
	    else if (key .eq. 'q') then
		goto 998
	    endif

C	Loop back until we want to quit
	goto 10

998	continue
	end


C =======================================================================
C                                                                       =
C PRINTHELP -- Utility subroutine to print a help summary for the task. =
C                                                                       =
C =======================================================================

      subroutine printHelp ()
      print '("                        Command Summary")'
      print '(" ")'
      print '("    :angle <real>   - set ellipse or text  angle")'
      print '("    :color <int>    - set marker color")'
      print '("    :fill  <0|1>    - set fill option (zero or one)")'
      print '("    :number <int>   - set point number")'
      print '("    :nannuli <int>  - set number of annuli")'
      print '("    :label <0|1>    - set point label option")'
      print '("    :sep <int>      - set annuli separation (pixels)")'
      print '("    :size <int>     - set point marker size")'
      print '("    :txsize <real>  - set relative text size")'
      print '("    :xrad <int>     - set ellipse x radius")'
      print '("    :yrad <int>     - set ellipse y radius")'
      print '("    :status         - print current settings")'
      print '("    :snap <file>    - snap frame buffer as EPS to file")'
      print '("    :print          - print FB to default printer")'
      print '(" ")'
      print '("Point Markers:")'
      print '("    v - diamond mark p - plus mark    x - cross mark")'
      print '("    . - point mark   * - star mark    _ - horiz dash")'
      print '("    | - vert dash    o - circle mark  s - square mark")'
      print '(" ")'
      print '("Misc. Commands")'
      print '("    ?  - Print Help     q  - Quit")'
      print '("    b  - Box            c  - Circle")'
      print '("    d  - Delete marker  e  - Ellipse marker")'
      print '("    l  - Line           t  - Text string")'
      print '("    C  - Circular annuli D  - Delete all markers")'
      print '("    E  - Elliptical annuli")'
      print '(" ")'
      end
