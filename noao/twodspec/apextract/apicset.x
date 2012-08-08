include	<imhdr.h>
include	"apertures.h"

# AP_ICSET -- Set the background fitting ICFIT structure for an aperture.
# If the input template aperture is NULL then the output background fitting
# ICFIT pointer is initialized otherwise a copy from the input template
# aperture is made.

procedure ap_icset (apin, apout, imlen)

pointer	apin		# Input template aperture pointer
pointer	apout		# Output aperture pointer
int	imlen		# Image length along aperture axis

int	i
real	x, x1, x2
pointer	ic, sp, str

int	apgeti(), ctor()
real	apgetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	if (AP_IC(apout) == NULL)
	    call ic_open (AP_IC(apout))
	ic = AP_IC(apout)

	if (apin == NULL) {
	    call apgstr ("b_function", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "function", Memc[str])
	    call ic_puti (ic, "order", apgeti ("b_order"))
	    call apgstr ("b_sample", Memc[str], SZ_LINE)
	    for (i=str; Memc[i]==' '; i=i+1)
		;
	    if (Memc[i] == EOS)
		call strcpy ("*", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "sample", Memc[str])
	    call ic_puti (ic, "naverage", apgeti ("b_naverage"))
	    call ic_puti (ic, "niterate", apgeti ("b_niterate"))
	    call ic_putr (ic, "low", apgetr ("b_low_reject"))
	    call ic_putr (ic, "high", apgetr ("b_high_reject"))
	    call ic_putr (ic, "grow", apgetr ("b_grow"))
	    if (AP_AXIS(apout) == 1)
		    call ic_pstr (ic, "xlabel", "Column")
		else
		    call ic_pstr (ic, "xlabel", "Line")
	} else {
	    if (AP_IC(apin) == NULL) {
		call ic_closer (AP_IC(apout))
		AP_IC(apout) = NULL
		ic = NULL
	    } else if (AP_IC(apin) != ic)
	        call ic_copy (AP_IC(apin), ic)
	}

	# Set the background limits
	if (ic != NULL) {
	    i = AP_AXIS(apout)
	    x1 = AP_LOW(apout, i)
	    x2 = AP_HIGH(apout, i)

	    call ic_gstr (ic, "sample", Memc[str], SZ_LINE)
	    for (i=str; Memc[i]!=EOS; i=i+1)
		if (Memc[i] == ':')
		    Memc[i] = ','
	    for (i=1; Memc[str+i-1]!=EOS; i=i+1) {
		if (Memc[str+i-1] == '*') {
		    x1 = min (x1, real(-imlen))
		    x2 = max (x2, real(imlen))
		} else if (ctor (Memc[str], i, x) > 0) {
		    x1 = min (x1, x)
		    x2 = max (x2, x)
		    i = i - 1
	        }
	    }

	    call ic_putr (ic, "xmin", x1)
	    call ic_putr (ic, "xmax", x2)
	}

	call sfree (sp)
end
