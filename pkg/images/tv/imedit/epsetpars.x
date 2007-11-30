include	<error.h>
include	"epix.h"
 
# EP_SETPARS -- Set the parameter values in the EPIX structure.
# If a logfile is given record selected parameters.
 
procedure ep_setpars (ep)
 
pointer	ep		# EPIX structure
 
int	fd, clgeti(), btoi(), clgwrd(), nowhite(), open()
char	clgetc()
bool	clgetb()
real	clgetr()
pointer	sp, aperture, logfile
errchk	open
 
begin
	call smark (sp)
	call salloc (aperture, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
 
	EP_ANGH(ep) = clgetr ("angh")
	EP_ANGV(ep) = clgetr ("angv")
	EP_APERTURE(ep) = clgwrd ("aperture", Memc[aperture], SZ_FNAME, APTYPES)
	EP_AUTODISPLAY(ep) = btoi (clgetb ("autodisplay"))
	EP_AUTOSURFACE(ep) = btoi (clgetb ("autosurface"))
	EP_BUFFER(ep) = clgetr ("buffer")
	EP_DEFAULT(ep) = clgetc ("default")
	EP_DISPLAY(ep) = btoi (clgetb ("display"))
	EP_FIXPIX(ep) = btoi (clgetb ("fixpix"))
	EP_RADIUS(ep) = clgetr ("radius")
	EP_SEARCH(ep) = clgetr ("search")
	EP_SIGMA(ep) = clgetr ("sigma")
	EP_VALUE(ep) = clgetr ("value")
	EP_MINVALUE(ep) = clgetr ("minvalue")
	EP_MAXVALUE(ep) = clgetr ("maxvalue")
	EP_WIDTH(ep) = clgetr ("width")
	EP_XORDER(ep) = clgeti ("xorder")
	EP_YORDER(ep) = clgeti ("yorder")
	call clgstr ("command", EP_COMMAND(ep), EP_SZLINE)
	call clgstr ("graphics", EP_GRAPHICS(ep), EP_SZFNAME)
 
	if (EP_LOGFD(ep) != NULL)
	    call close (EP_LOGFD(ep))
	EP_LOGFD(ep) = NULL
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	if (nowhite (Memc[logfile], Memc[logfile], SZ_FNAME) > 0) {
	    iferr {
		EP_LOGFD(ep) = open (Memc[logfile], APPEND, TEXT_FILE)
		fd = EP_LOGFD(ep)
		call fprintf (fd, ":aperture %s\n")
		    call pargstr (Memc[aperture])
		call fprintf (fd, ":search %g\n")
		    call pargr (EP_SEARCH(ep))
		call fprintf (fd, ":radius %g\n")
		    call pargr (EP_RADIUS(ep))
		call fprintf (fd, ":buffer %g\n")
		    call pargr (EP_BUFFER(ep))
		call fprintf (fd, ":width %g\n")
		    call pargr (EP_WIDTH(ep))
		call fprintf (fd, ":value %g\n")
		    call pargr (EP_VALUE(ep))
		call fprintf (fd, ":sigma %g\n")
		    call pargr (EP_SIGMA(ep))
		call fprintf (fd, ":xorder %d\n")
		    call pargi (EP_XORDER(ep))
		call fprintf (fd, ":yorder %d\n")
		    call pargi (EP_YORDER(ep))
	    } then
		call erract (EA_WARN)
	}
 
	call sfree (sp)
end
