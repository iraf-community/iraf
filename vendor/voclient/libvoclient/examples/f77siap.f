C
C  F77SIAP -- Fortran example task of VOClient interface
C
C  M.Fitzpatrick, NOAO, Jul 2006

	program f77siap

	double precision ra, dec, size
	character 	service*80, ffmt*80
	integer  	maxim

C  Initialize values.  Note the VOClient library assumes all floating
C  point values are double precision.
	ra      = 12.0d0
	dec     = 0.0d0
	size    = 0.5d0
C	service = "http://envoy.cacr.caltech.edu:8080/questsiap/siap?band=JU&"
C	service = "http://www-gsss.stsci.edu/gscvo/DSS2.jsp"
 	service = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?"

	ffmt    = "image/fits"
	maxim   = 5

	call siapservice (service, ra, dec, size, size, ffmt, maxim)

	stop
	end


C  Simple test routine to call a Cone search service and summarize results.

	subroutine siapservice (service, ra, dec, rasiz, decsiz, ffmt, maxim)

	character*80 	  service, ffmt, fname
	character*160     qstring, acref
	integer		  nrec, nattr, len, ier, maxim, index, nloop
	integer		  siap, query, qr, rec, v
	double precision  ra, dec, rasiz, decsiz

C  Define VOClient interface constants we'll use.

C  Initialize the VOClient
	call vfinitvoclient ("", ier)

C  Get a new connection to the named service and form a query.
	call vfopensiapconnection (service, siap, ier)
	call vfgetsiapquery (siap, ra, dec, rasiz, decsiz, ffmt, query)

C  Print the query string we're about to execute.
	index = 1
	call vfgetquerystring (query, "siap", index, qstring, len)
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



C  Download the first 'maxim' images in the list.   Note the results table
C  is one-indexed in this interface.  We also show how to catch an invalid 
C  record descriptor.  To try this, change the starting index to 0 instead
C  of 1.

	nloop = min (maxim, nrec)
	do i = 1, nloop, 1

C  Get the row in the table and then select values to print.
	  call vfgetrecord (qr, i, rec)
	  if (rec < 0) then
	      write (*,'(a,i4)'), "Error getting record number: ", i
	      goto 100
	  endif

C  Get the image access reference and download the file.
	  call vfgetattribute (rec, "AccessReference", v)
	  call vfstringvalue (v, acref, len)
	  write (*,'(a,a)'), "Downloading: ", acref
 
C  Create a filename for the download.
	  call vfmkfname ("dataset%04d.fits", i, fname, len)

	  call vfgetdataset (rec, acref, fname, ier)
	  if (ier .eq. 0) then
	      write (*,'(a,a)'), "    Saved to file: ",fname
	  else
	      write (*, '(a)'), "    ERROR: Download failed...."
	  endif

	enddo
100	continue


C  Shut down the VOClient server.
	call vfcloseconnection (siap)
	call vfclosevoclient (1, ier)

	return
	end
