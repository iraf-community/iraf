include	<mach.h>
include	"apertures.h"

# AP_ICSET -- Set the background fitting ICFIT structure for an aperture.
# If the input template aperture is NULL then the output background fitting
# ICFIT pointer is initialized otherwise a copy from the input template
# aperture is made.

procedure ap_icset (apin, xmin, xmax, apout)

pointer	apin		# Input template aperture pointer
real	xmin		# Minimum for background subtraction
real	xmax		# Maximum for background subtraction
pointer	apout		# Output aperture pointer

int	i
real	x, x1, x2
pointer	ic, sp, str

int	clgeti(), ctor()
real	clgetr(), ic_getr()

begin
	if (AP_IC(apout) == NULL)
	    call ic_open (AP_IC(apout))
	ic = AP_IC(apout)

	if (apin == NULL) {
	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call clgstr ("apdefault.function", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "function", Memc[str])
	    call ic_puti (ic, "order", clgeti ("apdefault.order"))
	    call clgstr ("apdefault.sample", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "sample", Memc[str])
	    call ic_puti (ic, "naverage", clgeti ("apdefault.naverage"))
	    call ic_puti (ic, "niterate", clgeti ("apdefault.niterate"))
	    call ic_putr (ic, "low", clgetr ("apdefault.low_reject"))
	    call ic_putr (ic, "high", clgetr ("apdefault.high_reject"))
	    call ic_putr (ic, "grow", clgetr ("apdefault.grow"))
	    if (AP_AXIS(apout) == 1)
		    call ic_pstr (ic, "xlabel", "Column")
		else
		    call ic_pstr (ic, "xlabel", "Line")

	    # Set background min and max based on sample regions.
	    x1 = xmin
	    x2 = xmax
	    for (i=str; Memc[i]!=EOS; i=i+1)
		if (Memc[i] == ':')
		    Memc[i] = ','
	    for (i=1; Memc[str+i-1]!=EOS; i=i+1)
	        if (ctor (Memc[str], i, x) > 0) {
		    x1 = min (x1, x)
		    x2 = max (x2, x)
		    i = i - 1
	        }
	    if (x1 > x2) {
		x1 = xmin
		x2 = xmax
	    }
	    call ic_putr (ic, "xmin", x1)
	    call ic_putr (ic, "xmax", x2)

	    call sfree (sp)
	} else {
	    if (AP_IC(apin) == NULL) {
		call ic_closer (AP_IC(apout))
		AP_IC(apout) = NULL
	    } else
	        call ic_copy (AP_IC(apin), AP_IC(apout))
	}

	# Insure the background region passes under the aperture.
	x1 = AP_LOW(apout, AP_AXIS(apout))
	x2 = AP_HIGH(apout, AP_AXIS(apout))
	call ic_putr (AP_IC(apout), "xmin",
	    min (x1, x2, ic_getr (AP_IC(apout), "xmin")))
	call ic_putr (AP_IC(apout), "xmax",
	    max (x1, x2, ic_getr (AP_IC(apout), "xmax")))
end
