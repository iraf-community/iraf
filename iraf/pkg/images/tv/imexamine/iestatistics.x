# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"imexam.h"

 
# IE_STATISTICS -- Compute and print statistics.
 
procedure ie_statistics (ie, x, y)
 
pointer	ie			# IMEXAM structure
real	x, y			# Aperture coordinates
 
double	mean, median, std
int	ncstat, nlstat, x1, x2,y1, y2, npts, clgeti()
pointer	sp, imname, im, data, sortdata, ie_gimage(), ie_gdata()
string	label "\
#            SECTION     NPIX     MEAN   MEDIAN   STDDEV      MIN      MAX\n"
 
begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	ncstat = clgeti ("ncstat")
	nlstat = clgeti ("nlstat")
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
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (sortdata, npts, TY_DOUBLE)

	call achtrd (Memr[data], Memd[sortdata], npts)
	call asrtd (Memd[sortdata], Memd[sortdata], npts)
	call aavgd (Memd[sortdata], npts, mean, std)
	if (mod (npts, 2) == 0)
	    median = (Memd[sortdata+npts/2-1] + Memd[sortdata+npts/2]) / 2
	else
	    median = Memd[sortdata+npts/2]

	call sprintf (Memc[imname], SZ_FNAME, "[%d:%d,%d:%d]")
	    call pargi (x1)
	    call pargi (x2)
	    call pargi (y1)
	    call pargi (y2)

	if (IE_LASTKEY(ie) != 'm')
	    call printf (label)

	call printf ("%20s %8d %8.4g %8.4g %8.4g %8.4g %8.4g\n")
	    call pargstr (Memc[imname])
	    call pargi (npts)
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
	        call pargi (npts)
	        call pargd (mean)
	        call pargd (median)
	        call pargd (std)
	        call pargd (Memd[sortdata])
	        call pargd (Memd[sortdata+npts-1])
	}

	call sfree (sp)
end
