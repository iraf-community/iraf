C
C  F77CONE -- Fortran example task of VOClient interface
C
C  M.Fitzpatrick, NOAO, Jul 2006

	program f77cone

	double precision ra, dec, sr
	character service*80

C  Initialize values.  Note the VOClient library assumes all floating
C  point values are double precision.
	ra  = 12.0d0
	dec = 12.0d0
	sr  = 0.1d0
	service = "http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=USNO-B1&"

	call coneservice (service, ra, dec, sr)

	stop
	end


C  Simple test routine to call a Cone search service and summarize results.

	subroutine coneservice (service, ra, dec, sr)

	character*80 	 service, fmt, sid, sra, sdec
	character*160    attrlist, qstring
	integer		 nrec, nattr, len, ier, index
	integer		 cone, query, qr, rec
	double precision ra, dec, sr


C   Initialize the VOClient
	call vfinitvoclient ("", ier)

C   Get a new connection to the named service and form a query.
	call vfopenconeconnection (service, cone, ier)
	call vfgetconequery (cone, ra, dec, sr, query)

C  Print the query string we're about to execute.
        index = 1
        call vfgetquerystring (query, 3, index, qstring, len)
        print *, "Executing Query: "
        print *, qstring
        print *, ""


C  Execute the query, get back a response handle 'qr'.
	call vfexecquery (query, qr)

C  Now summarize the response.
	call vfgetrecordcount (qr, nrec)
	if (nrec <= 0) then
	  print *, "No records matched"
	  return
	endif

C  Get a sample record so we can summarize columns.
	call vfgetrecord (qr, 1, rec)
	if (rec > 0) then
	    call vfgetattrcount (rec, nattr)
	endif

	print *, "# returns ", nrec, " records of ", nattr, " attrs"
	print *, "#"
	print *, "# --- Summary output ---"
	print *, "#"

C  Get the list of attributes.  Note that when receiving a string value
C  the interface requires you specify the max length of the string and
C  will return the actual length of the string.
	call vfgetattrlist (rec, attrlist, len)
	print *, "# ", attrlist
	print *, "#"


C  Summarize and print selected query results.  Note the results table
C  is one-indexed in this interface.  We also show how to catch an invalid 
C  record descriptor.  To try this, change the starting index to 0 instead
C  of 1.

	do i = 1, nrec, 1

C	  Get the row in the table and then select values to print
	  call vfgetrecord (qr, i, rec)
	  if (rec > 0) then
	      call vfgetstringattr (rec, "ID_MAIN", sid, len)
	      call vfgetstringattr (rec, "POS_EQ_RA_MAIN", sra, len)
	      call vfgetstringattr (rec, "POS_EQ_DEC_MAIN", sdec, len)
	  else
	      print *, "Error getting record number: ", i
	      goto 100
	  endif

	  fmt = '(i3,a6,a18,a6,a18,a6,a18)'
	  write (*, fmt)  i, "  id= ", sid, "  ra= ", sra, "  dec= ", sdec
	enddo
100	continue


C  Close the connection and shut down the VOClient server.
	call vfcloseconnection (cone)
	call vfclosevoclient (1, ier)

	return
	end
