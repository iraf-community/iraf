include	"apertures.h"

# AP_ALLOC -- Allocate and initialize an aperture structure.

procedure ap_alloc (ap)

pointer	ap		# Aperture

begin
	call calloc (ap, AP_LEN, TY_STRUCT)
	AP_TITLE(ap) = NULL
	AP_CV(ap) = NULL
	AP_IC(ap) = NULL
	AP_SELECT(ap) = YES
end


# AP_FREE -- Free an aperture structure and related CURFIT structures.

procedure ap_free (ap)

pointer	ap		# Aperture

begin
	if (ap != NULL) {
	    if (AP_TITLE(ap) != NULL)
		call mfree (AP_TITLE(ap), TY_CHAR)
	    if (AP_CV(ap) != NULL)
	        call cvfree (AP_CV(ap))
	    if (AP_IC(ap) != NULL)
	        call ic_closer (AP_IC(ap))
	    call mfree (ap, TY_STRUCT)
	}
end
