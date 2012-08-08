include <gset.h>
include "../lib/apphot.h"
include "../lib/fitpsf.h"


# AP_PFMARK -- Procedure to mark the psf fitting box on the display.

procedure appfmark (ap, id, mkbox)

pointer	ap		# pointer to the apphot procedure
pointer	id		# pointer to the display stream
int	mkbox		# mark the psf fitting box

int	marktype
real	radius, xc, yc
int	gstati()
real	apstatr()
errchk	greactivate, gdeactivate, gamove, gadraw

begin
	if (id == NULL)
	    return
	if (mkbox == NO)
	    return

	iferr (call greactivate (id, 0))
	    return

	marktype = gstati (id,G_PMLTYPE)
	iferr {
            call gseti (id, G_PMLTYPE, GL_DASHED)
            xc = apstatr (ap, PFXCUR)
	    yc = apstatr (ap, PFYCUR)
	    radius = apstatr (ap, SCALE) * apstatr (ap, PSFAPERT)
	    call gamove (id, xc - radius, yc - radius)
	    call gadraw (id, xc + radius, yc - radius)
	    call gadraw (id, xc + radius, yc + radius)
	    call gadraw (id, xc - radius, yc + radius)
	    call gadraw (id, xc - radius, yc - radius)
	} then
	    ;
        call gseti (id, G_PMLTYPE, marktype)

	iferr (call gdeactivate (id, 0))
	    return
end
