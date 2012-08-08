include	"epix.h"
 
# EP_DOSURFACE -- Display surface plots.
# There are two modes.  If there is no output subraster then just
# display the input subraster otherwise display both.  The orientation
# is given by the user.
 
procedure ep_dosurface (ep)
 
pointer	ep			# EPIX structure
pointer	gp, gopen()
 
begin
	if (EP_INDATA(ep) == NULL && EP_OUTDATA(ep) == NULL) {
	    call eprintf ("No region defined\n")
	    return
	}
	
	gp = gopen (EP_GRAPHICS(ep), NEW_FILE, STDGRAPH)
 
	if (EP_OUTDATA(ep) == NULL) {
	    call gsview (gp, 0.03, 0.98, 0.03, 0.98)
	    call ep_surface (gp, Memr[EP_INDATA(ep)], EP_NX(ep), EP_NY(ep),
		EP_ANGH(ep), EP_ANGV(ep))
	} else {
	    call gsview (gp, 0.03, 0.48, 0.03, 0.98)
	    call ep_surface (gp, Memr[EP_INDATA(ep)], EP_NX(ep), EP_NY(ep),
		EP_ANGH(ep), EP_ANGV(ep))
	    call gsview (gp, 0.53, 0.98, 0.03, 0.98)
	    call ep_surface (gp, Memr[EP_OUTDATA(ep)], EP_NX(ep),EP_NY(ep),
		EP_ANGH(ep), EP_ANGV(ep))
	}
 
	call gclose (gp)
end
