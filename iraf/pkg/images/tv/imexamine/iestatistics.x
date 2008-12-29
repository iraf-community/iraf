# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"imexam.h"

 
# IE_STATISTICS -- Compute and print statistics.
 
procedure ie_statistics (ie, x, y)
 
pointer	ie			# IMEXAM structure
real	x, y			# Aperture coordinates
 
size_t	sz_val
long	l_val, c_2
double	mean, median, std
long	ncstat, nlstat, x1, x2, y1, y2
size_t	npts
pointer	sp, imname, im, data, sortdata
long	clgetl(), lmod()
pointer	ie_gimage(), ie_gdata()
string	label "\
#            SECTION     NPIX     MEAN   MEDIAN   STDDEV      MIN      MAX\n"
 
begin
	c_2 = 2

	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	ncstat = clgetl ("ncstat")
	nlstat = clgetl ("nlstat")
	x1 = x - (ncstat-1) / 2 + 0.5
	x2 = x + ncstat / 2 + 0.5
	y1 = y - (nlstat-1) / 2 + 0.5
	y2 = y + nlstat / 2 + 0.5
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}
	npts = (x2-x1+1) * (y2-y1+1)

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (imname, sz_val, TY_CHAR)
	call salloc (sortdata, npts, TY_DOUBLE)

	call achtrd (Memr[data], Memd[sortdata], npts)
	call asrtd (Memd[sortdata], Memd[sortdata], npts)
	call aavgd (Memd[sortdata], npts, mean, std)
	l_val = npts
	if (lmod(l_val, c_2) == 0)
	    median = (Memd[sortdata+npts/2-1] + Memd[sortdata+npts/2]) / 2
	else
	    median = Memd[sortdata+npts/2]

	call sprintf (Memc[imname], SZ_FNAME, "[%d:%d,%d:%d]")
	    call pargl (x1)
	    call pargl (x2)
	    call pargl (y1)
	    call pargl (y2)

	if (IE_LASTKEY(ie) != 'm')
	    call printf (label)

	call printf ("%20s %8d %8.4g %8.4g %8.4g %8.4g %8.4g\n")
	    call pargstr (Memc[imname])
	    call pargz (npts)
	    call pargd (mean)
	    call pargd (median)
	    call pargd (std)
	    call pargd (Memd[sortdata])
	    call pargd (Memd[sortdata+npts-1])

	if (IE_LOGFD(ie) != NULL) {
	    if (IE_LASTKEY(ie) != 'm')
		call fprintf (IE_LOGFD(ie), label)

	    call fprintf (IE_LOGFD(ie),
		"%20s %8d %8.4g %8.4g %8.4g %8.4g %8.4g\n")
	        call pargstr (Memc[imname])
	        call pargz (npts)
	        call pargd (mean)
	        call pargd (median)
	        call pargd (std)
	        call pargd (Memd[sortdata])
	        call pargd (Memd[sortdata+npts-1])
	}

	call sfree (sp)
end
